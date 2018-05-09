#' Four factors data frame
#' 
#' @aliases do_four_factors_df
#'
#' @description 
#' This function computes team's offense and defense four factors.
#' The four factors are Effective Field Goal Percentage (EFGP), 
#' Turnover Percentage (TOVP), Offensive Rebound Percentage (ORBP) and
#' Free Throws Rate (FTRate). They are well defined at 
#' \url{http://www.rawbw.com/~deano/articles/20040601_roboscout.htm} and 
#' \url{https://www.basketball-reference.com/about/factors.html}.
#' 
#' As a summary, EFGP is a measure of shooting efficiency; TOVP is
#' the percentage of possessions where the team missed the ball, see
#' \url{http://www.nba.com/thunder/news/stats101.html} to read about 
#' the 0.44 coefficient; ORBP measures how many rebounds were offensive 
#' from the total of available rebounds; Finally, FTRate is a measure of both 
#' how often a team gets to the line and how often they make them.
#' 
#' @usage do_four_factors_df(df_games, teams)
#' 
#' @param df_games Data frame with the games, players info, advanced stats and
#' eventually recoded teams names.
#' @param teams Teams names.
#' 
#' @details 
#' Instead of defining the Offensive and Defensive Rebound Percentage
#' as mentioned in the previous links, I have computed just the Offensive
#' Rebound Percentage for the team and for its rivals. This makes easier
#' to have four facets, one per factor, in the ggplot.
#' 
#' In order to establish the team rankings, we have to consider these facts:
#' In defense (accumulated statistics of the opponent teams to the team of interest), 
#' the best team in each factor is the one that allows the smallest EFGP, the biggest TOVP, 
#' the smallest ORBP and the smallest FTRate, respectively.
#' 
#' In offense (accumulated statistics of the team of interest), the best team in each factor 
#' is the one that has the biggest EFGP, the smallest TOVP, 
#' the biggest ORBP and the biggest FTRate, respectively. 
#' 
#' @return 
#' A list with two data frames, \code{df_rank} and \code{df_no_rank}. 
#' Both have the same columns:
#' \itemize{
#' \item Team: Team name.
#' \item Type: Either Defense or Offense.
#' \item EFGP, ORBP, TOVP and FTRate.
#' }
#' 
#' The \code{df_rank} data frame contains the team ranking label for 
#' each statistic between parentheses. Therefore, \code{df_no_rank} is used
#' to create the ggplot with the numerical values and \code{df_rank} is
#' used to add the ranking labels.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{get_four_factors_plot}} 
#' 
#' @examples 
#' df <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718)
#' df1 <- do_add_adv_stats(df)
#' df_four_factors <- do_four_factors_df(df1, "Valencia")
#' 
#' @importFrom dplyr summarise bind_rows
#'
#' @export

do_four_factors_df <- function(df_games, teams) {
  GameID <- Day <- Game <- Team <- Player.x <- FG <- FGA <- ThreeP <- FT <- FTA <- NULL 
  DRB <- ORB <- TOV <- Type <- EFGP <- TOVP <- ORBP <- FTRate <- NULL

  df5 <- data.frame()
  for (i in teams) {
    #team_GameID <- unique(df_games$GameID[df_games$Team == i])
    team_Game <- unique(df_games$Game[df_games$Team == i])
    df2 <- df_games %>%
      #ungroup() %>%
      #filter(grepl(i, Game)) %>%
      #filter(GameID %in% team_GameID) %>%
      filter(Game %in% team_Game) %>%
      select(Day, Game, Team, Player.x, FG, FGA, ThreeP, FT, FTA, DRB, ORB, TOV) %>%
      group_by(Team) %>%
      mutate(Type = ifelse(Team == i, "Offense", "Defense")) 
    
    df3 <- df2 %>%
      group_by(Type) %>%
      summarise(EFGP = (sum(FG) + 0.5 * sum(ThreeP)) / sum(FGA),
                TOVP = sum(TOV) / (sum(FGA) + 0.44 * sum(FTA) + sum(TOV)),
                ORB = sum(ORB),
                DRB = sum(DRB), 
                ORBP = NA,
                FTRate = sum(FT) / sum(FGA)) 
    df3$ORBP[1] <- df3$ORB[1] / (df3$ORB[1] + df3$DRB[2])
    df3$ORBP[2] <- df3$ORB[2] / (df3$ORB[2] + df3$DRB[1])
    
    df4 <- df3 %>%
      select(-ORB, -DRB) %>%
      mutate(EFGP = round(EFGP * 100, 2),
             TOVP = round(TOVP * 100, 2),
             ORBP = round(ORBP * 100, 2),
             FTRate = round(FTRate, 2)) %>%
      mutate(Team = i) %>%
      select(Team, everything())
    
    # Data frame with the four factors for each team, both defense and offense:
    df5 <- bind_rows(df5, df4) 
  }  
  
  # The next steps are to add the ranking label for each team in the corresponding factor.
  df6 <- df5 %>%
    filter(Type == "Defense") %>%
    # The best team is the one that allows the worst (smallest) field percentage:
    mutate(order_EFGP = Team[order(EFGP)]) %>%
    # The best team is the one that allows the biggest turnover percentage:
    mutate(order_TOVP = Team[order(TOVP, decreasing = TRUE)]) %>%
    # The best team is the one that allows the worst (smallest) offensive rebounding percentage:
    mutate(order_ORBP = Team[order(ORBP)]) %>%
    # The best team is the one that allows the worst (smallest) free throw rate:
    mutate(order_FTRate = Team[order(FTRate)])
  
  for (i in teams) {
    # Find the position of the team in each of the order columns.
    orders_cols <- apply(df6[,7:10], 2, function(x){grep(i, x)})
    df6[df6$Team == i, 3:6] <- paste(df6[df6$Team == i, 3:6], 
                                        " (", orders_cols, ")", sep = "")
  }
  
  df7 <- df5 %>%
    filter(Type == "Offense") %>%
    # The best team is the one that has the best (biggest) field percentage:
    mutate(order_EFGP = Team[order(EFGP, decreasing = TRUE)]) %>%
    # The best team is the one that has the smallest turnover percentage:
    mutate(order_TOVP = Team[order(TOVP)]) %>%
    # The best team is the one that has the best (biggest) offensive rebounding percentage:
    mutate(order_ORBP = Team[order(ORBP, decreasing = TRUE)]) %>%
    # The best team is the one that has the best (biggest) free throw rate:
    mutate(order_FTRate = Team[order(FTRate, decreasing = TRUE)])
  
  for (i in teams) {
    orders_cols <- apply(df7[,7:10], 2, function(x){grep(i, x)})
    df7[df7$Team == i, 3:6] <- paste(df7[df7$Team == i, 3:6], 
                                        " (", orders_cols, ")", sep = "")
  }
  
  # Data frame with the four factors for each team, both defense and offense and the ranking label:
  df8 <- bind_rows(df6, df7) %>%
    select(-contains("order")) %>%
    arrange(rev(Team)) 
  
  return(list(df_rank = df8, df_no_rank = df5)) 
}
