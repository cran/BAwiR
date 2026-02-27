#' Compute lineups with their statistics
#' 
#' @aliases do_study_lineup
#'
#' @description 
#' This is an improvement of \code{\link{do_lineup}} to obtain all the 
#' information related to the lineups that a given team shows during a game.
#' 
#' @usage 
#' do_study_lineup(data, day_num, game_code_num, team_sel, season = "2025-2026", verbose)
#' 
#' @param data Play-by-play prepared data from a given game.
#' @param day_num Day number.
#' @param game_code_num Game code.
#' @param team_sel One of the teams involved in the game.
#' @param season Season string.
#' @param verbose Logical to indicate if the information of the 
#' computations must be provided.
#' 
#' @return 
#' A list with four data frames:
#' \itemize{
#'  \strong{data_lin}: Statistics obtained by every lineup.
#'  \strong{data_pos}: Start of each possession.
#'  \strong{data_usg}: Possessions ended by each player.  
#'  \strong{data_usg_act}: Actions that ended each possession per player.   
#' }
#' 
#' @note 
#' A possession lasts 24 seconds in the ACB league.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_lineup}}, \code{\link{do_possession}}, 
#' \code{\link{do_lineup_metrics}}, \code{\link{do_usage}}
#' 
#' @examples 
#' \dontrun{
#' library(dplyr)
#' df0 <- acb_vbc_cz_pbp_2223
#' 
#' day_num <- unique(acb_vbc_cz_pbp_2223$day)
#' game_code <- unique(acb_vbc_cz_pbp_2223$game_code)
#' 
#' # Starting players:
#' acb_games_2223_sl <- acb_vbc_cz_sl_2223 %>%
#'   dplyr::filter(period == "1C")
#' 
#' # Prepare data:
#' df1 <- do_prepare_data(df0, day_num, 
#'                        acb_games_2223_sl, acb_games_2223_info,
#'                        game_code)
#' 
#' teams_game <- sort(unique(df1$team))
#' 
#' # Study the lineups:
#' data_res <- do_study_lineup(df1, day_num, game_code, teams_game[1], "2022-2023", FALSE) 
#' }
#'
#' @importFrom dplyr between relocate
#' @importFrom stats na.omit
#'
#' @export

do_study_lineup <- function(data, day_num, game_code_num, team_sel, season = "2025-2026", verbose) {
  time_point <- team <- action <- player <- row_num <- day <- game_code <- NULL
  opponent <- period <- num_players <- block <- usage_perc <- poss_end <- last_value <- NULL
  
  # Create an ancillary matrix with a label for the last actions of a period different from in or out of the court:
  data_aux <- data %>%
    mutate(row_num = row_number()) %>%
    group_by(period) %>%
    mutate(last_value = ifelse(row_number() == max(which(!(action %in% c("Sale de la pista", "Entra a pista")))), "yes", NA)) %>%
    ungroup()
  
  # Add row numbers for an easy identification of rows:
  data <- data %>%
    mutate(row_num = row_number()) %>%
    mutate(time_point = ifelse(nchar(time_point) < 5, paste0("0", time_point), time_point))
  
  # Filter by team:
  data1 <- data %>%
    filter(team == team_sel)
  
  # Set also the opponent team:
  team_opp <- setdiff(unique(data$team), team_sel)
  
  # Add the last row of games' data to have the real final 
  # game score in case it is not available:
  last_row_game <- data[nrow(data),]
  
  last_row_game$time_point <- "00:00"
  last_row_game$player <- NA
  last_row_game$action <- NA
  last_row_game$team <- team_sel
  
  data1 <- bind_rows(data1, last_row_game)

  # Get players out:
  pl_out <- c(1, which(data1$action == "Sale de la pista"), nrow(data1))
  
  data_res <- data.frame()
  data_res_pos <- data.frame()
  data_res_usg <- data.frame()
  data_res_usg_act <- data.frame()
  for (i in 1:(length(pl_out) - 1)) {
    if (verbose) {
      cat("ITERATION:", i, "\n") 
      cat("VALUES:", pl_out, "\n") 
    }
    
    if (i == 1) {
      data2 <- data1 %>%
        slice(pl_out[i]:pl_out[i + 1]) 
      
      nr <- nrow(data2)
      
      # Lineup:
      lineup <- data2 %>%
        filter(action != "Sale de la pista") %>%
        # Avoid actions that are assigned to teams: 
        filter(player != team_sel) %>%
        distinct(player) %>%
        pull()
      
      # WARNING: CONSIDER THE SITUATION WHEN THE PLAYER WHO GOT THE FOUL CANNOT SHOOT THE FREE THROWS.
      # SEE FOR EXAMPLE 104557 1C 06:47 HOWARD SHOOTS THE FREE THROWS THAT GOT VILLAR.
      # SEE do_fix_pbp.R
      if (length(lineup) > 5) {
        lineup <- data2[data2$action == "Quinteto inicial", "player"] 
      }
      
      # Time playing:
      time_in <- data2$time_point[1]
      time_out <- data2$time_point[nr]
      time_seconds <- period_to_seconds(ms(time_in)) - period_to_seconds(ms(time_out))
      
      # Identify when the possessions start:
      data2_rival <- data %>%
        filter(team == team_opp) %>%
        filter(between(row_num, data2$row_num[1], data2$row_num[nr]))
      
      data3 <- rbind(data2, data2_rival) %>%
        arrange(row_num) %>%
        na.omit()
      
      # This is needed for situations where the first row related to the 
      # starting lineup was for a player of the opponent team. See game 104459.
      if (data3$row_num[1] != 1) {
        data3 <- rbind(data3, data[1, ]) %>%
          arrange(row_num)
      }
      
      # This is needed when there was a replacement just after the starting lineups.
      # See for example 1046371C 1C 09:56
      if (all(grepl("Quinteto|Salto|Sale de la pista|Entra a pista", data3$action))) {
        data_next_iter <- NULL
        data_res_pos <- NULL
        next
      }
      
      data4 <- do_possession(data3, NULL, "10:00") 
      # Sanity check: if (!all(unique(data4$poss_time) <= 24)) stop()
      # Some possessions last more than 24 seconds.
      # See for example 104475 1C 04:05 starts possession and 1C 03:38 ends possession.
      
      # Avoid starting a possession when it was already started before the replacements.
      # See for example 104459 1C 06:31: Frey started the possession by stealing the ball,
      # then there were several replacements and then Jaworski shoots.
      blocks <- unique(data4$block)
      if (grepl("Falta|Recuperaci\u00f3n|Rebote Defensivo", data4$action[nrow(data4)])) {
        data_next_iter <- data4 %>% 
          filter(block == blocks[length(blocks)]) %>% # Get the latest block.
          mutate(block = 1)
        
        data4 <- data4 %>% 
          filter(block != blocks[length(blocks)])
      }else{
        data_next_iter <- NULL
      }
      
      # Turn dunks into common two-point field goals:
      data4[data4$action == "Mate", "action"] <- "Tiro de 2 anotado"
      
      # Obtain usage values for the lineup:
      data4_usg <- do_usage(data4, team_sel, lineup, season)
      
      if (any(is.na(data4_usg$data_all$day))) {
        stop()
      }
      
      if (!is.null(data4_usg$data_all)) {
        data_save_usg <- data4_usg$data_all %>% 
          mutate(time_in = time_in, .after = usage_perc) %>% 
          mutate(time_out = time_out, .after = time_in) %>%
          mutate(time_seconds = time_seconds, .after = time_out)
      }else{
        data_save_usg <- NULL
      }
      
      if (!is.null(data4_usg$data_all_act)) {
        data_save_usg_act <- data4_usg$data_all_act %>% 
          mutate(time_in = time_in, .after = poss_end) %>% 
          mutate(time_out = time_out, .after = time_in) %>%
          mutate(time_seconds = time_seconds, .after = time_out) 
      }else{
        data_save_usg_act <- NULL
      }
      
      # Obtain metrics for the lineup:
      data5 <- do_lineup_metrics(data4, team_sel, team_opp)
      
      data_save <- data5 %>%
        mutate(day = day_num, .before = 1) %>%
        mutate(game_code = game_code_num, .after = day) %>%
        mutate(team = team_sel, .after = game_code) %>%
        mutate(opponent = team_opp, .after = team) %>%
        mutate(period = data4$period[1], .after = opponent) %>%
        mutate(lineup = paste(sort(lineup), collapse = ", "), .after = period) %>%
        mutate(num_players = str_count(lineup, ",") + 1, .after = lineup) %>% 
        mutate(time_in = time_in, .after = num_players) %>% 
        mutate(time_out = time_out, .after = time_in) %>%
        mutate(time_seconds = time_seconds, .after = time_out) %>%
        mutate(lineup_type = "quintet")
    }else{
      if ((pl_out[i] - 1) == pl_out[i - 1]) {
        next
      }
      
      out_index <- pl_out[i]
      
      if (verbose) {
        cat("OUT INDEX:", out_index, "\n") 
      }
      
      next_index <- NA
      
      # Case of just one replacement:
      if ((pl_out[i] + 1) == pl_out[i + 1]) {
        out_index <- pl_out[i] + 0:1
        next_index <- pl_out[i + 2]
      }
      
      # Case of two replacements at the same time:
      if (!is.na(pl_out[i + 2]) & (pl_out[i] + 2) == pl_out[i + 2]) {
        out_index <- pl_out[i] + 0:2
        next_index <- pl_out[i + 3]
      }
      
      # Case of three replacements at the same time:
      if (!is.na(pl_out[i + 3]) & (pl_out[i] + 3) == pl_out[i + 3]) {
        out_index <- pl_out[i] + 0:3
        next_index <- pl_out[i + 4]
      }
      
      # Case of four replacements at the same time:
      if (!is.na(pl_out[i + 4]) & (pl_out[i] + 4) == pl_out[i + 4]) {
        out_index <- pl_out[i] + 0:4
        next_index <- pl_out[i + 5]
      }
      
      # Case of five replacements at the same time:
      if (!is.na(pl_out[i + 5]) & (pl_out[i] + 5) == pl_out[i + 5]) {
        out_index <- pl_out[i] + 0:5
        next_index <- pl_out[i + 6]
      }
      
      # No replacement:
      if (is.na(next_index)) { 
        next_index <- pl_out[i + 1]
      }
      
      player_out <- data1 %>%
        slice(out_index) %>%
        pull(player)
      
      data2 <- data1 %>%
        slice(out_index[1]:next_index) 
      
      nr <- nrow(data2)
      nper <- unique(data2$period)
      
      player_in <- data2 %>%
        filter(action == "Entra a pista") %>%
        pull(player)
      
      # Lineup:
      lineup <- c(setdiff(lineup, player_out), player_in)
      
      # Time playing:
      # In the season 2024-2025 overtimes were labelled with PR1, PR2, etc, but in 
      # the season 2025-206, they are labelled with 5C, 6C, etc.
      if (grepl("PR|5|6", data2$period[nr]) & data2$period[nr] != data2$period[nr - 1]) {
        data2$time_point[nr] <- "00:00"
      }
      
      if (length(nper) > 2) { # This means the lineup was on the court more than two different periods,
                              # which would be very rare.
       stop() 
      }else if (length(nper) == 1) {
        time_in <- data2$time_point[1]
        time_out <- data2$time_point[nr]
        
        time_seconds <- period_to_seconds(ms(time_in)) - period_to_seconds(ms(time_out))
        
        data_time <- data.frame(period = nper, time_in = time_in, time_out = time_out, time_seconds = time_seconds)
      }else{
        data_time <- data.frame()
        for (i in 1:2) {
          aux_nper <- data2 %>%
            filter(period == nper[i])
        
          if (i == 1) {
            time_in <- aux_nper$time_point[1]
            time_out <- "00:00"
            
            time_seconds <- period_to_seconds(ms(time_in)) - period_to_seconds(ms(time_out))
          }else if (i == 2) {
            if (grepl("PR|5|6", nper[i])) {
              time_in <- "05:00"
            }else{
              time_in <- "10:00"
            }
            time_out <- aux_nper$time_point[nrow(aux_nper)]
            
            time_seconds <- period_to_seconds(ms(time_in)) - period_to_seconds(ms(time_out))
          }
          
          data_time_aux <- data.frame(period = nper[i], time_in = time_in, time_out = time_out, time_seconds = time_seconds)
          data_time <- rbind(data_time, data_time_aux)
        }
      }
      
      # Identify when the possessions start:
      data2_rival <- data %>%
        filter(team == team_opp) %>%
        filter(between(row_num, data2$row_num[1], data2$row_num[nr]))
      
      data3 <- rbind(data2, data2_rival) %>%
        arrange(row_num) %>%
        na.omit()
      
      # This is needed when there was a replacement after the free throws.
      # See for example 104475 1C 07:07 Hilliard and Lazarevic.
      if (all(grepl("Sale de la pista|Entra a pista", data3$action))) {
        next
      }

      data4 <- do_possession(data3, NULL, data3$time_point[1]) 
      # Sanity check: if (!all(unique(data4$poss_time) <= 24)) stop()
      # Some possessions last more than 24 seconds.
      # See for example 104475 1C 04:05 starts possession and 1C 03:38 ends possession.
      
      if (nrow(data4) == 0) {
        next
      }
      
      # Avoid starting a possession when it was already started before the replacements.
      # See for example 104459 1C 06:31: Frey started the possession by stealing the ball,
      # then there were several replacements and then Jaworski shoots.
      if (!is.null(data_res_pos)) {
        if (!is.null(data_next_iter)) {
          # Check if team coincides, if there is a new "inicio" and period coincides.
          if (data_next_iter$team[nrow(data_next_iter)] == data4$team[1] & !is.na(data4$possession[1]) & 
              data_next_iter$period[nrow(data_next_iter)] == data4$period[1]) {
            data4$possession[1] <- NA
            data4 <- rbind(data_next_iter, data4)
          }
        }else{
          if (data_res_pos$team[nrow(data_res_pos)] == data4$team[1] & !is.na(data4$possession[1]) & 
              data_res_pos$period[nrow(data_res_pos)] == data4$period[1]) {
            data4$possession[1] <- NA
          }
        } 
      }
      
      # The following is to avoid creating data_next_iter for the last actions of a period.
      check_last <- left_join(data4, data_aux %>% select(row_num, last_value), by = "row_num")
      # Create data_next_iter if all the last values are NA.
      # Example: ex <- c(NA, NA, "yes"). For this vector !any(!is.na(ex)) is FALSE, so we do not create data_next_iter.
      # Example: ex <- c(NA, NA, NA). For this vector !any(!is.na(ex)) is TRUE, so we create data_next_iter.
      if (!any(!is.na(check_last$last_value))) {
        # Get the latest block:
        blocks <- unique(data4$block)
        if (grepl("Falta|Recuperaci\u00f3n|Rebote Defensivo", data4$action[nrow(data4)])) {
          data_next_iter <- data4 %>% 
            filter(block == blocks[length(blocks)]) %>%
            mutate(block = 1)
          
          data4 <- data4 %>% 
            filter(block != blocks[length(blocks)])
        }else{
          data_next_iter <- NULL
        } 
      }else{
        data_next_iter <- NULL
      } 
      
      if (nrow(data4) == 0) {
        next
      }

      # There are some ending blocks that are not well collected:
      data4 <- data4 %>% filter(!is.na(block))
      
      # Turn dunks into common two-point field goals:
      data4[data4$action == "Mate", "action"] <- "Tiro de 2 anotado"
      
      # Obtain usage values for the lineup:
      data4_usg <- do_usage(data4, team_sel, lineup, season)
      
      if (any(is.na(data4_usg$data_all$day))) {
        stop()
      }
      
      if (!is.null(data4_usg$data_all)) {
        data_save_usg <- left_join(data4_usg$data_all, data_time, by = "period")
      }else{
        data_save_usg <- NULL
      }

      if (!is.null(data4_usg$data_all_act)) {
        data_save_usg_act <- left_join(data4_usg$data_all_act, data_time, by = "period")
      }else{
        data_save_usg_act <- NULL
      }
      
      # Obtain metrics for the lineup:
      # For the case that there is more than one period for the same lineup.
      # See for example 104459 3C 01:35 -> 4C 09:14 Bilbao
      check_per <- unique(data4$period)
      
      if (length(check_per) > 1) {
        data5 <- data.frame()
        for (i in 1:length(check_per)) {
          data4_aux <- data4 %>%
            filter(period == check_per[i]) 
          
          # Remove the defensive rebounds in the last seconds of a period that do not start a possession:
          # See for example 104459 3C 00:01 Hlinason
          if (data4_aux$action[nrow(data4_aux)] == "Rebote Defensivo") {
            data4_aux <- data4_aux[-nrow(data4_aux), ]
          }
          
          data5_per <- do_lineup_metrics(data4_aux, team_sel, team_opp) %>%
            mutate(day = day_num, .before = 1) %>%
            mutate(game_code = game_code_num, .after = day) %>%
            mutate(team = team_sel, .after = game_code) %>%
            mutate(opponent = team_opp, .after = team) %>%
            mutate(period = check_per[i], .after = opponent) %>%
            mutate(lineup = paste(sort(lineup), collapse = ", "), .after = period) %>%
            mutate(num_players = str_count(lineup, ",") + 1, .after = lineup) 
          
          data5_per <- left_join(data5_per, data_time, by = "period") %>%
            relocate(time_in, time_out, time_seconds, .after = num_players) %>%
            mutate(lineup_type = "quintet")
          
          data5 <- rbind(data5, data5_per) 
        }
        
        data_save <- data5 
      }else{
        data5 <- do_lineup_metrics(data4, team_sel, team_opp) 
        
        data_save <- data5 %>%
          mutate(day = day_num, .before = 1) %>%
          mutate(game_code = game_code_num, .after = day) %>%
          mutate(team = team_sel, .after = game_code) %>%
          mutate(opponent = team_opp, .after = team) %>%
          mutate(period = data4$period[1], .after = opponent) %>%
          mutate(lineup = paste(sort(lineup), collapse = ", "), .after = period) %>%
          mutate(num_players = str_count(lineup, ",") + 1, .after = lineup) 
        
        data_save <- left_join(data_save, data_time, by = "period") %>%
          relocate(time_in, time_out, time_seconds, .after = num_players) %>%
          mutate(lineup_type = "quintet")
      }
    } # End of i>1 iteration.
  
    if (verbose) {
      cat("LINEUP:", data_save$lineup, "\n") 
    }
    
    data_res <- bind_rows(data_res, data_save)
    
    data_res_pos <- bind_rows(data_res_pos, data4)
    
    data_res_usg <- bind_rows(data_res_usg, data_save_usg)
    data_res_usg_act <- bind_rows(data_res_usg_act, data_save_usg_act)
  } # End of i iteration.
  
  cols_eng <- c("turnover", "ft_made", "ft_missed", "two_made", "two_missed", "three_made", 
                "three_missed", "assist", "fg_assisted") # , "foul_ag", "block_ag"
  colnames(data_res)[c(13:20, 23, 26:33, 36)] <- c(cols_eng, paste0(cols_eng, "_opp")) # c(13:22, 25, 28:37, 40)
  
  return(list(data_lin = data_res, data_pos = data_res_pos, data_usg = data_res_usg, data_usg_act = data_res_usg_act))
}

