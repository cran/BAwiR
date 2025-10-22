#' Accumulated or average statistics
#' 
#' @aliases do_stats
#'
#' @description 
#' This function computes either the total or the average statistics.
#' 
#' @usage 
#' do_stats(df_games, type_stats = "Total", season, competition, type_season)
#' 
#' @param df_games Data frame with the games, players info, advanced stats and
#' eventually recoded teams names.
#' @param type_stats String. In English, the options are "Total" and "Average" and in
#' Spanish, the options are "Totales" and "Promedio".
#' @param season String indicating the season, for example, 2017-2018.
#' @param competition String. Options are "ACB", "Euroleague" and "Eurocup".
#' @param type_season String with the round of competition, for example regular season
#' or playoffs and so on.
#' 
#' @return 
#' Data frame.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' compet <- "ACB"
#' df <- do_join_games_bio(compet, acb_games_1718, acb_players_1718)
#' 
#' df1 <- do_add_adv_stats(df)
#' 
#' df2 <- do_stats(df1, "Total", "2017-2018", compet, "Regular Season")
#'         
#' @importFrom dplyr contains rename n everything summarise_all distinct            
#' @importFrom lubridate as.period ms  
#'                  
#' @export

do_stats <- function(df_games, type_stats = "Total", season, competition, type_season){
 Number <- TwoPPerc <- ThreePPerc <- FTPerc <- Day <- Date <- NULL
 Game <- GameRes <- GameID <- Website <- Player.y <- Height <- NULL
 Date_birth <- Licence <- Website_player <- Age <- Month <- Compet <- NULL
 FGPerc <- EFGPerc <- ThreeRate <- FRate <- STL_TOV <- AST_TOV <- NULL
 PPS <- OE <- EPS <- Season <- Type_season <- MP <- Player.x <- NULL
 Name <-Team <- GS <- CombinID <- Nationality <- GP <- NULL
 MP_oper <- MP_oper_def <- TwoPA <- TwoP <- ThreePA <- NULL
 ThreeP <- FTA <- FT <- FGA <- FG <- TOV <- STL <- AST <- PTS <- NULL
  
 # Remove columns related to strings and also related to percentages:
 df2 <- df_games %>%
    select(-c(Number, TwoPPerc, ThreePPerc, FTPerc, Season, Type_season, 
              Day, Date, Game, GameRes, GameID, 
              Website, Player.y, Height, Date_birth, Website_player, Age, Month, Compet,
              contains("Gm"), FGPerc, EFGPerc, ThreeRate, FRate, STL_TOV, AST_TOV, PPS, OE, EPS))
 
  
 df2_1 <- df2 %>% 
  filter(MP != 0) %>% # If I don't filter by MP != 0, for example for Josep Puerto there were four games 
   # where he was on the game roster but only two where he played. So the average must be computed 
   # regarding two games.
  rename(Name = Player.x) %>%
  group_by(Name, Team) %>% # Group by Name and Team because for example Llompart played 3 games for 
   # Valencia and 2 for Tenerife. If I don't group by Team, his total games played for both teams is 5.
  mutate(GP = n()) %>%
  mutate(GS = sum(GS))
 
 df2_2 <- df2_1 %>%
  select(Name, Team, CombinID, Position, Nationality, GP, GS, everything()) 
 
 df3 <- df2_2 %>% 
  select(-MP) %>%
  group_by(Name, Team, CombinID, Position, Nationality, GP, GS)
    
 # Sum or average all numeric variables:
 if (type_stats == "Total" | type_stats == "Totales") {
   df3 <- df3 %>%
    summarise_all(sum, na.rm = TRUE) # gv <- c(NA, NA) ; sum(gv) is NA but sum(gv, na.rm = TRUE) is 0.
   
   # Sum minutes:
   # See do_sum_MP.R to sum the MP:
   df3_mp <- df2_2 %>% 
     group_by(Name, Team, CombinID, Position, Nationality, GP, GS) %>%
     mutate(MP_oper = ifelse(all(MP == "0"), 
                             0,
                             sum(as.numeric(as.period(ms(MP), unit = "sec")), na.rm = TRUE))) %>%
     mutate(MP_oper_def = sprintf("%02d:%02d", MP_oper %/% 60, MP_oper %% 60)) %>%
     distinct(Name, Team, CombinID, Position, Nationality, GP, GS, MP_oper_def)
  }else if (type_stats == "Average" | type_stats == "Promedio") {
    df3 <- df3 %>%
     summarise_all(mean, na.rm = TRUE) 
    df3[, 8:ncol(df3)] <- round(df3[, 8:ncol(df3)], 1)
    
    # Average minutes:
    # See do_sum_MP.R to sum the MP:
    df3_mp <- df2_2 %>% 
      group_by(Name, Team, CombinID, Position, Nationality, GP, GS) %>%
      mutate(MP_oper = ifelse(all(MP == "0"), 
                              0,
                              floor(mean(as.numeric(as.period(ms(MP), unit = "sec")), na.rm = TRUE)))) %>%
      mutate(MP_oper_def = sprintf("%02d:%02d", MP_oper %/% 60, MP_oper %% 60)) %>%
      distinct(Name, Team, CombinID, Position, Nationality, GP, GS, MP_oper_def)
  }else{
    stop("Wrong option.")
  }
    
  df3_def <- left_join(df3, df3_mp) %>%
   rename(MP = MP_oper_def) %>%
   select(Name, Team, CombinID, Position, Nationality, GP, GS, MP, everything())
    
  # Add now the percentages and other variables related to accumulated statistics:
  df4 <- df3_def %>% 
    mutate(TwoPPerc = ifelse(TwoPA == 0, 0, round((TwoP / TwoPA) * 100))) %>%
    mutate(ThreePPerc = ifelse(ThreePA == 0, 0, round((ThreeP / ThreePA) * 100))) %>%
    mutate(FTPerc = ifelse(FTA == 0, 0, round((FT / FTA) * 100))) %>%
    mutate(FGPerc = ifelse(FGA == 0, 0, round((FG / FGA) * 100))) %>% # Field Goal Percentage.
    mutate(EFGPerc = ifelse(FGA == 0, 0, (FG + 0.5 * ThreeP) / FGA)) %>% # Effective Field Goal Percentage.
    mutate(EFGPerc = round(EFGPerc * 100)) %>%
    mutate(EFGPerc = ifelse(EFGPerc > 100, 100, EFGPerc)) %>% # FG = 1 ; Three = 1 --> (1 + 0.5 * 1) / 1 > 1
    mutate(ThreeRate = ifelse(FGA == 0, 0, round((ThreePA / FGA) * 100, 1))) %>% # 3-Point Attempt Rate.
    mutate(FRate = ifelse(FGA == 0, 0, round(FT / FGA, 1))) %>% # Free Throw Attempt Rate.
    mutate(STL_TOV = ifelse(TOV == 0, STL, round(STL / TOV, 1))) %>% # Steal to Turnover Ratio.
    mutate(AST_TOV = ifelse(TOV == 0, AST, round(AST / TOV, 1))) %>% # Assist to Turnover Ratio.
    mutate(PPS = ifelse(FGA == 0, 0, round(PTS / FGA, 1))) # Points per Shot.

  df4$OE <- do_OE(df4) # Offensive Efficiency.
  df4$EPS <- do_EPS(df4) # Efficient Points Scored.
  
  # Matrix with all the accumulated statistics in the suitable order:
  df5 <- df4 %>%
    select(1:9, FG, FGA, FGPerc, 10:11, TwoPPerc, 12:13, ThreePPerc, 14:15, FTPerc, everything()) %>%
    # To convert the minutes into numeric:
    mutate(MP = round(as.numeric(ms(MP), unit = "mins")))

  df5$Season <- season
  df5$Compet <- competition
  if (length(type_season) > 1) { # In some months, more than one round is played.
    # In February, there are games from Copa del Rey and ACB regular season.
    df5$Type_season <- "All"
  }else{
    df5$Type_season <- type_season 
  }
  df5$Type_stats <- type_stats

  return(df5)
}  