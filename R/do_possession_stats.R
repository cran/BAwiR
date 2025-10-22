#' Possessions-related statistics
#' 
#' @aliases do_possession_stats
#'
#' @description 
#' Compute the possessions-related statistics, namely, offensive rating, defensive rating, 
#' net rating, pace and number of possessions.
#' 
#' @usage 
#' do_possession_stats(data_possess, season = "2025-2026")
#' 
#' @param data_possess Data frame with the beginning of each possession 
#' obtained with \code{\link{do_possession}}.
#' @param season Season string.
#' 
#' @details
#' See \url{https://www.basketball-reference.com/about/glossary.html}
#' for formulas and explanations.
#' 
#' Both teams in the same game share the same pace. Pace reflects the tempo of the game itself, 
#' not just one team's style.Over many games, a team's average pace reflects how fast they 
#' usually play, but any individual game's pace is shared with their opponent.
#' 
#' @return 
#' A data frame with the possessions statistics for each team.
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
#' do_possession_stats(df2, "2022-2023")
#' }
#' 
#' @importFrom dplyr mutate_if
#'
#' @export

do_possession_stats <- function(data_possess, season = "2025-2026") {
  day <- game_code <- team <- period <- possession <- NULL
  ortg <- drtg <- pace <- possessions <- points <- poss_num <- NULL 
  
  # Count possessions:
  res_posses <- data_possess %>% 
    mutate(day = as.numeric(day)) %>%
    count(day, game_code, team, period, possession, name = "poss_num") %>% 
    filter(!is.na(possession)) %>% 
    select(-possession) 
  
  # Count points:
  res_points <- data_possess %>% 
    mutate(day = as.numeric(day)) %>%
    group_by(day, game_code, team, period) %>%
    summarise(points = sum(points, na.rm = TRUE)) %>%
    ungroup()
  
  df0 <- left_join(res_posses, res_points, by = c("day", "game_code", "team", "period"))
  
  # Get ratings:
  df1 <- df0 %>%
    group_by(day, game_code, team) %>%
    summarise(possessions = sum(poss_num), 
              points = sum(points)) %>%
    ungroup() %>%
    mutate(ortg = (points / possessions) * 100) %>%
    group_by(day, game_code) %>%
    mutate(drtg = rev(ortg)) %>%
    mutate(pace = 40 * ((sum(possessions)) / (2 * (200 / 5)))) %>% # Same as sum(possessions) / 2
    ungroup()
  
  df2 <- df1 %>%
    group_by(team) %>%
    summarise(ortg = mean(ortg),
              drtg = mean(drtg),
              pace = mean(pace),
              possessions = sum(possessions)) %>%
    ungroup() %>%
    mutate(netrtg = ortg - drtg, .after = drtg) %>%
    mutate_if(is.numeric, round, 2)
  
  data_possess_def <- df2 %>% mutate(season = season)
  
  return(data_possess_def)
}
