#' Accumulated and average statistics for teams
#' 
#' @aliases do_stats_teams
#'
#' @description 
#' This function computes the total and average statistics for every team.
#' 
#' @usage 
#' do_stats_teams(df_games, season, competition, type_season)
#' 
#' @param df_games Data frame with the games, players info, advanced stats and
#' eventually recoded teams names.
#' @param season String indicating the season, for example, 2017-2018.
#' @param competition String. Options are "ACB", "Euroleague" and "Eurocup".
#' @param type_season String with the round of competition, for example regular season
#' or playoffs and so on.
#' 
#' @return 
#' A list with two elements:
#' \itemize{
#' \item df_team_total: Data frame with the total statistics for every team.
#' \item df_team_mean: Data frame with the average statistics for every team.
#' }
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' compet <- "ACB"
#' df <- do_join_games_bio(compet, acb_games_1718, acb_players_1718)
#' df$Compet <- compet
#' df_teams <- do_stats_teams(df, "2017-2018", "ACB", "Regular Season")
#' # Total statistics:
#' #df_teams$df_team_total
#' # Average statistics:
#' #df_teams$df_team_mean
#' 
#' @importFrom dplyr funs
#'                  
#' @export

do_stats_teams <- function(df_games, season, competition, type_season){
  Compet <- Season <- Type_season <- Name <- CombinID <- NULL
  Position <- Nationality <- Type_season <- Type_stats <- NULL
  GP <- GS <- MP <- Team <-   Game <- Type <- PTSrv <- PTSrv_mean <- NULL
  FGPerc <- FGA <- FG <- TwoPPerc <- TwoPA <- TwoP <- NULL
  ThreePPerc <- ThreePA <- ThreeP <- FTPerc <- FTA <- NULL
  FT <- EFGPerc <- PTS <- NULL
  
  # Get the total statistics for every player:
  if (type_season == "All") {
    df_games1 <- df_games %>%
      filter(Compet == competition, 
             Season == season)
  }else{
    df_games1 <- df_games %>%
      filter(Compet == competition, 
             Season == season, 
             Type_season == type_season)
  }  
  
  df_games1 <- do_add_adv_stats(df_games1)
  
  # Games played by every team:
  games_played <- df_games1 %>%
    group_by(Team) %>%
    distinct(Game) %>%
    count()
  
  if (type_season == "All") {
    df_games2 <- do_stats(df_games1, 
                          "Total", 
                          unique(df_games1$Season), 
                          unique(df_games1$Compet), 
                          "All")
  }else{
    df_games2 <- do_stats(df_games1, 
                          "Total", 
                          unique(df_games1$Season), 
                          unique(df_games1$Compet), 
                          unique(df_games1$Type_season))
  } 
  
  # Once we have the total statistics for every player, we can get
  # the teams' statistics by summing their players' statistics:
  df_team <- df_games2 %>% 
    ungroup() %>%
    select(-c(Name, CombinID, Position, Nationality, 
              Season, Compet, Type_season, Type_stats)) %>%
    select(-GP, -GS, -MP, -contains("Perc")) %>%
    group_by(Team) %>%
    summarise_all(sum) #%>%
  
  df_team1 <- left_join(df_team, games_played) %>%
    rename(GP = n)
  
  df_team2 <- df_team1 %>%
    select(Team, GP, everything()) %>%
    mutate(FGPerc = ifelse(FGA == 0, 0, round((FG / FGA) * 100))) %>%
    mutate(TwoPPerc = ifelse(TwoPA == 0, 0, round((TwoP / TwoPA) * 100))) %>%
    mutate(ThreePPerc = ifelse(ThreePA == 0, 0, round((ThreeP / ThreePA) * 100))) %>%
    mutate(FTPerc = ifelse(FTA == 0, 0, round((FT / FTA) * 100))) %>%
    mutate(EFGPerc = ifelse(FGA == 0, 0, (FG + 0.5 * ThreeP) / FGA)) %>% # Effective Field Goal Percentage.
    mutate(EFGPerc = round(EFGPerc * 100)) %>%
    mutate(EFGPerc = ifelse(EFGPerc > 100, 100, EFGPerc)) %>% # FG = 1 ; Three = 1 --> (1 + 0.5 * 1) / 1 > 1
    select(1:5, FGPerc, 6:7, TwoPPerc, 8:9, ThreePPerc, 10:11, FTPerc, 12:27, EFGPerc, everything())
  

  # In order to get the average statistics, we have just to divide by the
  # number of games played so far:
  df_team_mean <- df_team2 %>%
    select(-contains("Perc")) 
  # -c(1,2) is to discard the team's name and games played:
  df_team_mean_aux <- apply(df_team_mean[,-c(1,2)], 2, "/", df_team_mean$GP)
  df_team_mean_aux1 <- as.data.frame(df_team_mean_aux)
  df_team_mean1 <- cbind(df_team_mean[,1:2], df_team_mean_aux1)

  df_team_mean2 <- df_team_mean1 %>%
    mutate(FGPerc = ifelse(FGA == 0, 0, round((FG / FGA) * 100))) %>%
    mutate(TwoPPerc = ifelse(TwoPA == 0, 0, round((TwoP / TwoPA) * 100))) %>%
    mutate(ThreePPerc = ifelse(ThreePA == 0, 0, round((ThreeP / ThreePA) * 100))) %>%
    mutate(FTPerc = ifelse(FTA == 0, 0, round((FT / FTA) * 100))) %>%
    mutate(EFGPerc = ifelse(FGA == 0, 0, (FG + 0.5 * ThreeP) / FGA)) %>% # Effective Field Goal Percentage.
    mutate(EFGPerc = round(EFGPerc * 100)) %>%
    mutate(EFGPerc = ifelse(EFGPerc > 100, 100, EFGPerc)) %>% # FG = 1 ; Three = 1 --> (1 + 0.5 * 1) / 1 > 1
    select(1:5, FGPerc, 6:7, TwoPPerc, 8:9, ThreePPerc, 10:11, FTPerc, 12:27, EFGPerc, everything())
  
  df_team_mean2[, 3:ncol(df_team_mean2)] <- round(df_team_mean2[, 3:ncol(df_team_mean2)], 1)

  # Finally, this is to compute the total and average points received by every team:
  teams <- df_team_mean2$Team
  df_defense <- data.frame()
  for (i in teams) {
    team_Game <- unique(df_games1$Game[df_games1$Team == i])
    df_defense_team <- df_games1 %>%
      filter(Game %in% team_Game) %>%
      group_by(Team) %>%
      mutate(Type = ifelse(Team == i, "Offense", "Defense")) #%>%
  
    df_defense_team1 <- df_defense_team %>%
      filter(Type == "Defense") %>%
      ungroup() %>%
      summarise(PTSrv = sum(PTS)) %>%
      mutate(Team = i) %>%
      mutate(PTSrv_mean = round(PTSrv / games_played$n[games_played$Team == i], 1))
      
    df_defense <- bind_rows(df_defense, df_defense_team1)
  }  

  df_team3 <- left_join(df_team2, df_defense) %>%
    select(-PTSrv_mean) %>%
    select(Team, GP, PTS, PTSrv, everything())

  df_team_mean3 <- left_join(df_team_mean2, df_defense) %>%
    select(-PTSrv) %>%
    select(Team, GP, PTS, PTSrv_mean, everything()) %>%
    rename(PTSrv = PTSrv_mean)
      
  return(list(df_team_total = df_team3, df_team_mean = df_team_mean3))
}