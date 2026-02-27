#' Possessions-related statistics for lineups
#' 
#' @aliases do_possession_stats_lineup
#'
#' @description 
#' Compute the possessions-related statistics for lineups. These statistics are
#' offensive rating, defensive rating, net rating, pace and number of possessions.
#' 
#' @usage 
#' do_possession_stats_lineup(data_combs, team_name, type_lineup, type_period, type_opponent, 
#'                            cols_group = c("team", "lineup"))
#' 
#' @param data_combs Data frame with all the combinations of lineups.
#' @param team_name Name of the team.
#' @param type_lineup Type of lineups to analyze. Options are 'quintet', 'quartet', 'trio',
#' 'duo' and 'single'. 
#' @param type_period Period of interest. Options are xC, where x starts from 1. 
#' Common periods are from 1 to 4. Overtimes are labeled with the next numbers, such as 5C 
#' for the first overtime and 6C for the second one. Nothing to do if NULL.
#' @param type_opponent Name of the opponent teams. Nothing to do if NULL.
#' @param cols_group Group of columns to apply the computations. Default is c("team", "lineup")
#' to compute the metrics just for the players on court. To compute them for the players
#' both on and off court, use c("team", "lineup", "status").
#' 
#' @details
#' See \url{https://www.basketball-reference.com/about/glossary.html}
#' for formulas and explanations.
#' 
#' @return 
#' A data frame with the possessions statistics for each lineup.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_possession_stats}}
#' 
#' @examples 
#' \dontrun{
#' do_possession_stats_lineup(acb_combs, "Unicaja", "quintet", NULL, NULL)
#' }
#'
#' @importFrom dplyr all_of
#'
#' @export

do_possession_stats_lineup <- function(data_combs, team_name, type_lineup, type_period, type_opponent, 
                                       cols_group = c("team", "lineup")) {
  team <- lineup_type <- period <- opponent <- lineup <- time_seconds <- time_minutes <- status <- NULL
  pts_num <- pts_num_opp <- poss_num <- poss_num_opp <- ortg <- drtg <- netrtg <- pace <- pace_opp <- NULL
  
  # Filter by team and lineup type:
  data_combs_filt <- data_combs %>%
    filter(team == team_name) %>%
    filter(lineup_type == type_lineup)
  
  # Filter by period:
  if (!is.null(type_period)) {
   data_combs_filt <- data_combs_filt %>% 
     filter(period %in% type_period) 
  }
  
  # Filter by opponent:
  if (!is.null(type_opponent)) {
   data_combs_filt <- data_combs_filt %>% 
     filter(opponent %in% type_opponent) 
  }
  
  # Minutes played:
  data_mp <- data_combs_filt %>% 
    group_by(across(all_of(cols_group))) %>%
    summarise(time_seconds = sum(time_seconds)) %>%
    ungroup() %>%
    mutate(total_time = sprintf("%dH %dM %02dS", 
                                hour(seconds_to_period(time_seconds)), 
                                minute(seconds_to_period(time_seconds)), 
                                second(seconds_to_period(time_seconds))), .after = time_seconds) 
  
  data_poss <- data_combs_filt %>% 
    group_by(across(all_of(cols_group))) %>%
    summarise(pts_num = sum(pts_num), 
              pts_num_opp = sum(pts_num_opp), 
              poss_num = sum(poss_num),
              poss_num_opp = sum(poss_num_opp)) %>%
    ungroup() %>%
    mutate(ortg = ifelse(poss_num == 0, 0, (pts_num / poss_num) * 100)) %>%
    mutate(drtg = ifelse(poss_num_opp == 0, 0, (pts_num_opp / poss_num_opp) * 100)) %>%
    mutate(netrtg = ortg - drtg) %>%
    mutate_if(is.numeric, round, 2) %>%
    select(all_of(cols_group), contains("rtg"), contains("pts"), contains("poss")) 
 
  data_mp_poss <- left_join(data_mp, data_poss, by = cols_group) %>%
    mutate(pace = (poss_num * 2400) / time_seconds, .after = netrtg) %>% # 40 minutes are 2400 seconds.
    mutate(pace_opp = (poss_num_opp * 2400) / time_seconds, .after = pace) %>%
    mutate(pace = round(pace, 2)) %>%
    mutate(pace_opp = round(pace_opp, 2))
    
  data_mp_poss <- data_mp_poss %>%
    filter(time_seconds > 0) %>%
    mutate(time_seconds = time_seconds / 60) %>%
    rename(time_minutes = time_seconds)
  
  if ("status" %in% cols_group) {
    data_mp_poss <- data_mp_poss %>%
      arrange(lineup, desc(status))
  }else{
    data_mp_poss <- data_mp_poss %>%
      arrange(desc(time_seconds))
  }
  
  return(data_mp_poss) 
}
