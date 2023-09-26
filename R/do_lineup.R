#' Compute ACB lineups
#' 
#' @aliases do_lineup
#'
#' @description 
#' Compute all the lineups that a given team shows during a game.
#' 
#' @usage 
#' do_lineup(data, day_num, game_code, team_sel, verbose)
#' 
#' @param data Play-by-play prepared data from a given game.
#' @param day_num Day number.
#' @param game_code Game code.
#' @param team_sel One of the teams' names involved in the game.
#' @param verbose Logical. Decide if information of the computations
#' must be provided or not.
#' 
#' @return 
#' Data frame. Each row is a different lineup. This is the meaning of the 
#' columns that might not be explanatory by themselves:
#' \itemize{
#'   \strong{team_in}: Time point when that lineup starts playing together.
#'   \strong{team_out}: Time point when that lineup stops playing together 
#'   (because there is a substitution).
#'   \strong{num_players}: Number of players forming the lineup (must be 5 in this case).
#'   \strong{time_seconds}: Total of seconds that the lineup played.
#'   \strong{diff_points}: Game score in the time that the lineup played.
#'   \strong{plus_minus}: Plus/minus achieved by the lineup. This is the difference
#'   between the game score of the previous lineup and of the current one.
#'   \strong{plus_minus_poss}: Plus/minus per possession.
#' }
#' 
#' @note 
#' A possession lasts 24 seconds in the ACB league.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' library(dplyr)
#' df0 <- acb_vbc_cz_pbp_2223
#' 
#' day_num <- unique(acb_vbc_cz_pbp_2223$day)
#' game_code <- unique(acb_vbc_cz_pbp_2223$game_code)
#' 
#' acb_games_2223_sl <- acb_vbc_cz_sl_2223 %>%
#'   filter(period == "1C")
#' 
#' df1 <- do_prepare_data(df0, day_num, 
#'                        acb_games_2223_sl, acb_games_2223_info,
#'                        game_code)
#'                 
#' df2 <- do_lineup(df1, day_num, game_code, "Valencia Basket", FALSE)    
#' #df2
#' 
#' @importFrom lubridate period_to_seconds   
#' @importFrom stringr str_count   
#'
#' @export

do_lineup <- function(data, day_num, game_code, team_sel, verbose) {
  team <- action <- player <- period <- time_in <- time_out <- plus_minus <- NULL
  
  local_team <- unique(data$local)
  
  data1 <- data %>%
    filter(team == team_sel)

  # Add the last row of games' data to have the real final game score 
  # in case it is not available:
  last_row_game <- data[nrow(data),]
  last_row_game$time_point <- "00:00"
  last_row_game$player <- NA
  last_row_game$action <- NA
  last_row_game$team <- team_sel
  data1 <- bind_rows(data1, last_row_game)
  
  # Get players out:
  pl_out <- c(1, which(data1$action == "Sale de la pista"), nrow(data1))
  
  data_res <- data.frame()
  for (i in 1:(length(pl_out) - 1)) {
    if (verbose) {
      cat("ITERATION:", i, "\n") 
      cat("VALUES:", pl_out, "\n") 
    }
    
    if (i == 1) {
      data2 <- data1 %>%
        slice(pl_out[i]:pl_out[i + 1]) 
      
      lineup <- data2 %>%
        filter(action != "Sale de la pista") %>%
        filter(player != team_sel) %>%
        distinct(player) %>%
        pull()
      
      # For cases where brothers receive the same label.
      if (length(lineup) == 4) {
        if (team_sel == "Monbus Obradoiro") {
          lineup <- c(lineup, "Scrubb")
        }else if (team_sel == "R\\u00edo Breog\\u00e1n") {
          lineup <- c(lineup, "Quintela")
        }
      }
      
      nr <- nrow(data2)
      time_seconds <- period_to_seconds(ms(data2$time_point[1])) - period_to_seconds(ms(data2$time_point[nr]))
      
      diff_points <- ifelse(local_team == team_sel, 
                            data2$local_score[nr] - data2$visitor_score[nr],
                            data2$visitor_score[nr] - data2$local_score[nr])
      
      data_save <- data.frame(period = data2$period[1],
                              time_in = data2$time_point[1],
                              time_out = data2$time_point[nr],
                              lineup = paste(sort(lineup), collapse = ", "),
                              time_seconds = time_seconds,
                              diff_points = diff_points) %>%
        mutate(lineup = as.character(lineup))
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
      
      player_in <- data2 %>%
        filter(action == "Entra a pista") %>%
        pull(player)
      
      lineup <- c(setdiff(lineup, player_out), player_in)
      
      # For cases where brothers receive the same label.
      if (length(lineup) == 4) {
        if (team_sel == "Monbus Obradoiro") {
          lineup <- c(lineup, "Scrubb")
        }else if (team_sel == "R\\u00edo Breog\\u00e1n") {
          lineup <- c(lineup, "Quintela")
        }
      }
      
      nr <- nrow(data2)
      nper <- unique(data2$period)
      
      if (grepl("PR", data2$period[nr]) & data2$period[nr] != data2$period[nr - 1]) {
        data2$time_point[nr] <- "00:00"
      }
      
      if (length(nper) == 1) {
        time_seconds <- period_to_seconds(ms(data2$time_point[1])) - period_to_seconds(ms(data2$time_point[nr]))
      }else{
        # For the case when the replacement has been done in the next period. 
        # See for example i=13 of Real Madrid in 103350. 
        # The same lineup is between "04:25" of 3C and "08:05" of 4C.
        if (grepl("PR", data2$period[nr]) & data2$period[nr] != data2$period[nr - 1]) {
          time_seconds <- period_to_seconds(ms(data2$time_point[1])) - period_to_seconds(ms(data2$time_point[nr]))
        }else{
          aux <- period_to_seconds(ms("10:00") - ms(data2$time_point[nr]))
          time_seconds <- period_to_seconds(ms(data2$time_point[1])) + aux 
        }
      }
      
      diff_points <- ifelse(local_team == team_sel, 
                            data2$local_score[nr] - data2$visitor_score[nr],
                            data2$visitor_score[nr] - data2$local_score[nr])
      
      data_save <- data.frame(period = data2$period[1],
                              time_in = data2$time_point[1],
                              time_out = data2$time_point[nr],
                              lineup = paste(sort(lineup), collapse = ", "),
                              time_seconds = time_seconds,
                              diff_points = diff_points) %>%
        mutate(lineup = as.character(lineup))
    } # End of i>1 iteration.
    
    if (verbose) {
      cat("LINEUP:", data_save$lineup, "\n") 
    }
    
    data_res <- bind_rows(data_res, data_save)
  } # End of i iteration.
  
  data_res <- data_res %>%
    mutate(team = team_sel, .before = period) %>%
    mutate(game_code = game_code, .before = team) %>%
    mutate(day = day_num, .before = game_code) %>%
    mutate(num_players = str_count(lineup, ",") + 1, .after = lineup) %>%
    mutate(plus_minus = diff_points - lag(diff_points)) %>%
    mutate(lineup_type = "quintet")

  data_res$plus_minus[1] <- data_res$diff_points[1]

  data_res$time_out[which(data_res$time_out == "10:00")] <- "00:00"
  
  data_res <- data_res %>%
    filter(!(time_in == "00:00" & time_out == "00:00")) %>%
    filter(!(time_in == "00:00" & time_out == "05:00"))
  
  # Plus/Minus per possession.
  data_res1 <- data_res %>% 
    mutate(plus_minus_poss = ifelse(time_seconds == 0, 
                                    plus_minus, 
                                    round((24 * plus_minus) / time_seconds, 2)), 
           .after = plus_minus)
    
  return(data_res1)
}
