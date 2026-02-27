#' Players' usage
#' 
#' @aliases do_usage
#'
#' @description 
#' This function computes the players' usage to indicate how many possessions 
#' each player ended. A possession ends with a field-goal or free-throw attempt, 
#' or with a turnover.
#' 
#' @usage 
#' do_usage(data_all_posse, team_name, lineup_curr, season = "2025-2026")
#' 
#' @param data_all_posse Data frame with the start of each possession.
#' @param team_name Name of the team.
#' @param lineup_curr Lineup currently playing in the game interval under analysis.
#' @param season Season string.
#' 
#' @return 
#' A list with two data frames:
#' \itemize{
#'  \strong{data_all}: Possessions ended by each player.
#'  \strong{data_all_act}: Actions that ended each possession per player.
#' }
#' 
#' @author 
#' Guillermo Vinue
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
#' team_sel <- teams_game[1]
#' 
#' data <- df1
#' data <- data %>%
#'   mutate(row_num = row_number()) %>%
#'   mutate(time_point = ifelse(nchar(time_point) < 5, paste0("0", time_point), time_point))
#' 
#' # Filter by team:
#' data1 <- data %>%
#'   filter(team == team_sel)
#' 
#' # Set also the opponent team:
#' team_opp <- setdiff(unique(data$team), team_sel)
#' 
#' # Add the last row of games' data to have the real final 
#' # game score in case it is not available:
#' last_row_game <- data[nrow(data),]
#' 
#' last_row_game$time_point <- "00:00"
#' last_row_game$player <- NA
#' last_row_game$action <- NA
#' last_row_game$team <- team_sel
#' 
#' data1 <- bind_rows(data1, last_row_game)
#' 
#' # Get players out:
#' pl_out <- c(1, which(data1$action == "Sale de la pista"), nrow(data1))
#' 
#' i <- 1
#' data2 <- data1 %>%
#'   slice(pl_out[i]:pl_out[i + 1]) 
#' 
#' nr <- nrow(data2)
#' 
#' # Lineup:
#' lineup <- data2 %>%
#'   filter(action != "Sale de la pista") %>%
#'   # Avoid actions that are assigned to teams: 
#'   filter(player != team_sel) %>%
#'   distinct(player) %>%
#'   pull()
#' 
#' # Identify when the possessions start:
#' data2_rival <- data %>%
#'   filter(team == team_opp) %>%
#'   filter(between(row_num, data2$row_num[1], data2$row_num[nr]))
#' 
#' data3 <- rbind(data2, data2_rival) %>%
#'   arrange(row_num) %>%
#'   na.omit()
#' 
#' data4 <- do_possession(data3, NULL, "10:00") 
#' 
#' data4[data4$action == "Mate", "action"] <- "Tiro de 2 anotado"
#' 
#' data4_usg <- do_usage(data4, team_sel, lineup, "2022-2023")
#' }
#'
#' @export

do_usage <- function(data_all_posse, team_name, lineup_curr, season = "2025-2026") {
  team <- period <- block <- time_start <- time_end <- action <- day <- game_code <- NULL
  player <- poss_end <- poss_num <- poss_time.x <- poss_time.y <- poss_time <- usage_perc <- NULL
  
  # Filter non-informative actions:
  data_all_posse <- data_all_posse %>% 
    filter(!action %in% c("Asistencia", "Tap\u00f3n", "Tap\u00f3n Recibido")) %>%
    # When scraping the actions assigned to teams they do not appear, for example 
    # a turnover because of 24 seconds, so the previous action becomes the last action
    # and this does not correspond to real end possession. Related to this, the fact 
    # that team actions do not appear also forced me to extract the timeouts manually.
    filter(!grepl("Falta T\u00e9cnica|Falta Personal|Falta Antideportiva|Rebote Ofensivo|Descalificado del partido", action))
  
  # Remove drawn personal fouls that are misleading:
  # Case 1:
  misl_pf <- which(data_all_posse$action == "Falta Recibida" & is.na(data_all_posse$possession))
  
  if (length(misl_pf) != 0) {
    data_all_posse <- data_all_posse[-misl_pf, ] 
  }
  
  # Case 2:
  misl_pf1 <- which(data_all_posse$action == "Falta Recibida" & 
                     !is.na(data_all_posse$possession) & 
                     data_all_posse$team != lead(data_all_posse$team))
  
  if (length(misl_pf1) != 0) {
    data_all_posse <- data_all_posse[-misl_pf1, ] 
  }
  
  # Filter by team:
  data_possess <- data_all_posse %>%
    filter(team == team_name)
  
  if ("Mate" %in% unique(data_possess$action)) {
    data_possess <- data_possess %>%
      mutate(action = plyr::mapvalues(action, from = "Mate", to = "Tiro de 2 anotado")) 
  }
  
  if (nrow(data_possess) == 0) {
    data_all <- NULL
    data_all_act <- NULL
  }else{
    # Check if the last action was not a real end of possession:
    if (!grepl("Tiro|Triple|P\u00e9rdida", data_possess$action[nrow(data_possess)])) {
      data_possess <- data_possess[-nrow(data_possess), ] 
    }
    
    if (nrow(data_possess) == 0) {
      data_all <- NULL
      data_all_act <- NULL
    }else{
      data_usg <- data_possess %>% 
        group_by(block) %>% 
        slice(n()) %>% # Take the last row of each block, i.e., the end of possessions.
        ungroup() %>%
        select(day, game_code, period, team, player, action)
      
      # Remove misleading steals or rebounds:
      data_usg <- data_usg %>%
        filter(action != "Recuperaci\u00f3n")
      # There are very few defensive rebounds that ended in a player's turnover,
      # that turned into a team turnover, such as Willy 104473 2C 03:27,
      # that if removed, other results are ruined.
      data_usg <- data_usg %>%
        filter(action != "Rebote Defensivo")  
      
      data_player_poss <- data_usg %>% 
        count(day, game_code, period, team, player, name = "poss_end")
      
      # For the case that there is more than one period for the same lineup.
      # See for example 104459 3C 01:35 -> 4C 09:14 Bilbao
      for (i in unique(data_player_poss$period)) {
        aux <- data_player_poss %>%
          filter(period == i)
        
        # Add the players of the lineup who did not end any possession:
        pl_no <- setdiff(lineup_curr, aux$player)
        
        if (length(pl_no) != 0) {
          data_other <- data.frame(day = aux$day[1],
                                   game_code = aux$game_code[1],
                                   period = i,
                                   team = aux$team[1],
                                   player = pl_no,
                                   poss_end = 0)
          
          data_player_poss <- rbind(data_player_poss, data_other) 
        }
      }
      
      data_player_poss <- data_player_poss %>%
        arrange(period)
      
      data_team_poss <- data_player_poss %>% 
        group_by(day, game_code, period, team) %>% 
        summarise(poss_num = sum(poss_end)) %>% 
        ungroup()
      
      data_all <- left_join(data_team_poss, data_player_poss, by = c("day", "game_code", "period", "team")) %>%
        mutate(season = season) %>%
        select(season, day, game_code, period, team, player, poss_end, poss_num) %>%
        mutate(usage_perc = ifelse(poss_num == 0, 0, (poss_end / poss_num) * 100)) %>%
        mutate(usage_perc = round(usage_perc, 2))
      
      # Sanity check: only 100 for both teams.
      #data_all %>% group_by(team) %>% summarise(sum(usage_perc)) %>% ungroup()
      
      # Adding actions:
      data_all_act <- data_usg %>% 
        count(day, game_code, period, team, player, action, name = "poss_end") %>%
        mutate(season = season, .before = 1) 
      
      # Sanity check: only field goals, free throws and turnovers.
      #sort(table(data_all_act$action))   
    }
  }
  
  return(list(data_all = data_all, data_all_act = data_all_act))
}
