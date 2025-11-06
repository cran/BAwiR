#' Players' usage
#' 
#' @aliases do_usage
#'
#' @description 
#' For each period of a game, this function computes the players' usage, which indicates how 
#' many possessions each player ended. A possession ends with a field-goal or free-throw attempt, 
#' or with a turnover.
#' 
#' @usage 
#' do_usage(data_possess, season = "2025-2026")
#' 
#' @param data_possess Data frame with the beginning of each possession 
#' obtained with \code{\link{do_possession}}.
#' @param season Season string.
#' 
#' @return 
#' A data frame with the players' usage.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_possession}}
#' 
#' @examples 
#' \dontrun{
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
#' df2 <- do_possession(df1, "1C")    
#' 
#' do_usage(df2, "2022-2023")
#' }
#'
#' @export

do_usage <- function(data_possess, season = "2025-2026") {
  poss_time <- action <- day <- game_code <- player <- NULL
  block <- team <- period <- possession <- poss_end <- poss_num <- NULL
  
  data_possess <- data_possess %>% 
    filter(!action %in% c("Asistencia", "Tap\u00f3n", "Tap\u00f3n Recibido")) %>%
    # When scraping the actions assigned to teams do not appear, for example 
    # a turnover because of 24 seconds, so the previous action becomes the last action
    # and this does not correspond to real end possession. Related to this, the fact 
    # that team actions do not appear also forced me to extract the timeouts manually.
    filter(!grepl("Falta Personal|Falta Antideportiva|Rebote Ofensivo|Descalificado del partido", action)) %>%
    mutate(action = plyr::mapvalues(action, from = "Mate", to = "Tiro de 2 anotado")) 
  
  # Remove drawn personal fouls that are misleading:
  misl_pf <- which(data_possess$action == "Falta Recibida" & 
                     is.na(data_possess$possession) & 
                     lag(data_possess$possession) == "inicio" & 
                     lead(data_possess$possession) == "inicio")
  
  if (length(misl_pf) != 0) {
    data_possess <- data_possess[-misl_pf, ] 
  }
  
  # Locate more drawn personal fouls that are misleading:
  misl_pf_more <- which(data_possess$action == "Falta Recibida" & data_possess$possession == "inicio")
  misl_pf_more <- misl_pf_more[misl_pf_more != nrow(data_possess)]
  
  if (length(misl_pf_more) != 0) {
    for (i in 1:length(misl_pf_more)) {
      if (data_possess[misl_pf_more[i] + 1, "poss_time"]  == 0) {
        data_possess[misl_pf_more[i], "block"] <- data_possess[misl_pf_more[i] + 1, "block"]
      }
    }
  }
  
  # Check if the last action was not a real end of possession:
  if (!grepl("Tiro|Triple|P\u00e9rdida", data_possess$action[nrow(data_possess)])) {
    data_possess <- data_possess[-nrow(data_possess), ] 
  }
  
  # There are very few defensive rebounds that ended in a player's turnover,
  # that turned into a team turnover, such as Willy 104473 2C 03:27,
  # that if removed, other results are ruined.
  data_possess <- data_possess %>%
    filter(action != "Rebote Defensivo")
  
  data_usg <- data_possess %>% 
    group_by(block) %>% 
    slice(n()) %>% # Take the last row of each block, i.e., the end of possessions.
    ungroup() %>%
    select(day, game_code, period, team, player, action)

  data_player_poss <- data_usg %>% 
    count(day, game_code, period, team, player, name = "poss_end")
  
  data_team_poss <- data_player_poss %>% 
    group_by(team) %>% 
    summarise(poss_num = sum(poss_end)) %>% 
    ungroup()
  
  data_all <- left_join(data_team_poss, data_player_poss, by = "team") %>%
    mutate(season = season) %>%
    select(season, day, game_code, period, team, player, poss_end, poss_num) %>%
    mutate(usage_perc = round((poss_end / poss_num) *100, 2))
  
  # Sanity check:
  #data_all %>% group_by(team) %>% summarise(sum(usage_perc)) %>% ungroup() # Only 100 for both teams.
  
  # Adding actions:
  data_all_act <- data_usg %>% 
    count(day, game_code, period, team, player, action, name = "poss_end") %>%
    mutate(season = season, .before = 1) 
  
  # Sanity check: only field goals, free throws and turnovers.
  #sort(table(data_all_act$action))

  return(list(data_all = data_all, data_all_act = data_all_act))
}
