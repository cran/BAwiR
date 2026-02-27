#' Four factors for lineups
#' 
#' @aliases do_four_factors_df_lineup
#'
#' @description 
#' This function computes offense and defense four factors for a given lineup.

#' The four factors are the effective field goal percentage (EFGP), 
#' the turnover percentage (TOVP), the offensive rebound percentage (ORBP) 
#' and the free throws rate (FTRate). They are well defined at 
#' \url{http://www.rawbw.com/~deano/articles/20040601_roboscout.htm} and
#' \url{https://www.basketball-reference.com/about/factors.html}.
#' 
#' As a summary, EFGP is a measure of shooting efficiency; TOVP is
#' the percentage of possessions where the team missed the ball; ORBP measures 
#' how many rebounds were offensive from the total of available rebounds, and 
#' FTRate is a measure of how often a team gets to the line.
#' 
#' @usage 
#' do_four_factors_df_lineup(data_combs, team_name, type_lineup, type_period, type_opponent)
#' 
#' @param data_combs Data frame with all the combinations of lineups.
#' @param team_name Name of the team.
#' @param type_lineup Type of lineups to analyze. Options are 'quintet', 'quartet', 'trio',
#' 'duo' and 'single'. 
#' @param type_period Period of interest. Options are xC, where x starts from 1. 
#' Common periods are from 1 to 4. Overtimes are labeled with the next numbers, such as 5C 
#' for the first overtime and 6C for the second one.
#' @param type_opponent Name of the opponent teams.
#' 
#' @details 
#' Instead of defining the Offensive and Defensive Rebound Percentage
#' as mentioned in the previous links, I have computed just the Offensive
#' Rebound Percentage for the team and for its rivals. This makes easier
#' to have four facets, one per factor, in the ggplot.
#' 
#' @return 
#' A data frame with the four factors in defense and in offense for a given lineup.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_four_factors_df}} 
#' 
#' @examples 
#' \dontrun{
#' do_four_factors_df_lineup(acb_combs, "Unicaja", "quintet", NULL, NULL)
#' }
#' 
#' @importFrom lubridate hour minute second
#'
#' @export

do_four_factors_df_lineup <- function(data_combs, team_name, type_lineup, type_period, type_opponent) {
  team <- lineup_type <- period <- opponent <- lineup <- time_seconds <- fta <- fta_opp <- NULL
  ft_made <- ft_missed <- two_made <- two_missed <- three_made <- three_missed <- fg <- fga <- NULL
  ft_made_opp <- ft_missed_opp <- two_made_opp <- two_missed_opp <- three_made_opp <- three_missed_opp <- fg_opp <- fga_opp <- NULL
  poss_num <- turnover <- total_reb <- off_reb <- efgp <- tovp <- orbp <- ftrate <- time_minutes <- NULL
  poss_num_opp <- turnover_opp <- total_reb_opp <- off_reb_opp <- efgp_opp <- tovp_opp <- orbp_opp <- ftrate_opp <- NULL
  
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
    group_by(team, lineup) %>%
    summarise(time_seconds = sum(time_seconds)) %>%
    ungroup() %>%
    mutate(total_time = sprintf("%dH %dM %02dS", 
                                hour(seconds_to_period(time_seconds)), 
                                minute(seconds_to_period(time_seconds)), 
                                second(seconds_to_period(time_seconds))), .after = time_seconds) 
  
  # Create columns of total attempts:
  data_combs1 <- data_combs_filt %>%
    # Team:
    mutate(fta = ft_made + ft_missed, .after = ft_missed) %>%
    mutate(fg = two_made + three_made, .after = three_missed) %>%
    mutate(fga = two_made + two_missed + three_made + three_missed, .after = fg) %>%
    # Opponent:
    mutate(fta_opp = ft_made_opp + ft_missed_opp, .after = ft_missed_opp) %>%
    mutate(fg_opp = two_made_opp + three_made_opp, .after = three_missed_opp) %>%
    mutate(fga_opp = two_made_opp + two_missed_opp + three_made_opp + three_missed_opp, .after = fg_opp)
  
  ## OFFENSE:  
  data_combs2_off <- data_combs1 %>%
    group_by(team, lineup) %>%
    summarise(efgp = ifelse(sum(fga) == 0, 0, (sum(fg) + 0.5 * sum(three_made)) / sum(fga)),
              tovp = ifelse(sum(poss_num) == 0, 0, sum(turnover) / sum(poss_num)),
              orbp = ifelse(sum(total_reb) == 0, 0, sum(off_reb) / sum(total_reb)),
              ftrate = ifelse(sum(fga) == 0, 0, sum(fta) / sum(fga))
    ) %>%
    ungroup() %>%
    mutate(efgp = round(efgp * 100, 2),
           tovp = round(tovp * 100, 2),
           orbp = round(orbp * 100, 2),
           ftrate = round(ftrate * 100, 2)) %>%
    mutate(type = "Offense", .after = lineup)
  
  ## DEFENSE:
  data_combs2_def <- data_combs1 %>%
    group_by(team, lineup) %>%
    summarise(efgp = ifelse(sum(fga_opp) == 0, 0, (sum(fg_opp) + 0.5 * sum(three_made_opp)) / sum(fga_opp)),
              tovp = ifelse(sum(poss_num_opp) == 0, 0, sum(turnover_opp) / sum(poss_num_opp)),
              orbp = ifelse(sum(total_reb_opp) == 0, 0, sum(off_reb_opp) / sum(total_reb_opp)),
              ftrate = ifelse(sum(fga_opp) == 0, 0, sum(fta_opp) / sum(fga_opp))
    ) %>%
    ungroup() %>%
    mutate(efgp = round(efgp * 100, 2),
           tovp = round(tovp * 100, 2),
           orbp = round(orbp * 100, 2),
           ftrate = round(ftrate * 100, 2)) %>%
    mutate(type = "Defense", .after = lineup)
  
  data_four_fact <- rbind(data_combs2_def, data_combs2_off) 
  
  data_mp_four_fact <- left_join(data_mp, data_four_fact, by = c("team", "lineup")) %>%
    arrange(desc(time_seconds)) %>%
    filter(time_seconds > 0) %>%
    mutate(time_seconds = time_seconds / 60) %>%
    rename(time_minutes = time_seconds)
  
  return(data_mp_four_fact)
}
