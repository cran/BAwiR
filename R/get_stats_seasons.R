#' Season-by-season stats
#' 
#' @aliases get_stats_seasons
#'
#' @description 
#' This function represents the average values of a set of statistics 
#' for a certain player in every season where the player played. It gives
#' an idea of the season-by-season performance. 
#' 
#' @usage get_stats_seasons(df, competition, player, variabs, type_season)
#' 
#' @param df Data frame with the games and the players info.
#' @param competition Competition.
#' @param player Player.
#' @param variabs Vector with the statistics to plot.
#' @param type_season String with the round of competition, for example regular season
#' or playoffs and so on.
#' 
#' @return 
#' List with two elements:
#' \itemize{
#' \item gg Graphical device.
#' \item df_gg Data frame associated with the plot.
#' }
#' 
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' competition <- "ACB"
#' df <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718)
#' df$Compet <- competition
#' player <- "Carroll, Jaycee"
#' variabs <- c("GP", "MP", "PTS", "EFGPerc", "TRB", "AST", "TOV", "PIR")
#' plot_yearly <- get_stats_seasons(df, competition, player, variabs, "All")
#' plot_yearly$gg
#' # There are only games from the regular season in this demo data frame.
#' plot_yearly1 <- get_stats_seasons(df, competition, player, variabs, "Regular Season")
#' plot_yearly1$gg
#' }
#'
#' @importFrom ggplot2 geom_point geom_line
#'
#' @export

get_stats_seasons <- function(df, competition, player, variabs, type_season){
  Compet <- Player.x <- Season <- Name <- Team <- NULL
  Type_season <- Age <- value <- variable <- NULL
  
  
  if (type_season == "All") {
    df1 <- df %>%
      filter(Compet == competition,
             Player.x == player)
  }else{
    df1 <- df %>%
      filter(Compet == competition,
             Player.x == player,
             Type_season == type_season)    
  }  

  # It might happen that the player hasn't played the competition.
  if (nrow(df1) == 0) { 
    return(NA)
  }
  
  df2 <- do_add_adv_stats(df1)
  
  df3 <- data.frame()
  for (i in rev(unique(df2$Season))) {
    df2_loop <- df2 %>%
      filter(Season == i)
    
    if (type_season == "All") {
      df3_loop <- do_stats(df2_loop, 
                           "Average", 
                           unique(df2_loop$Season), 
                           unique(df2_loop$Compet), 
                           "All")     
    }else{
      df3_loop <- do_stats(df2_loop, 
                           "Average", 
                           unique(df2_loop$Season), 
                           unique(df2_loop$Compet), 
                           unique(df2_loop$Type_season)) 
    }
    
    df3 <- bind_rows(df3, df3_loop)
  }
  
  df4 <- df3 %>%
    select(Name, Team, Season, variabs)
  
  Date_birth <- unique(df2$Date_birth)
  # Age of Player at the start of October 1st of that season. 
  # For instance, season 2013-2014, age at October 1st 2013.
  #df4$Age <- round((as.Date(paste("1/10/", substr(df4$Season, 1, 4), sep = ""), "%d/%m/%Y") - 
  #                    as.Date(Date_birth, "%d/%m/%Y")) / 365.25, 1)
  # Age of Player at the start of February 1st of that season.
  # For instance, season 2013-2014, age at February 1st 2014.
  df4$Age <- trunc((as.Date(paste("1/02/", sapply(strsplit(df4$Season, "-"), `[`, 2), sep = ""), "%d/%m/%Y") - 
                      as.Date(Date_birth, "%d/%m/%Y")) / 365.25)
  
  df5 <- melt(df4 %>% select(-Age))
  
  if (length(unique(df5$Season)) == 1) {
    gg <- ggplot(data = df5, aes(x = Season, y = value, group = 1, color = variable)) +
      geom_point() 
  }else{
    gg <- ggplot(data = df5, aes(x = Season, y = value, group = variable, color = variable)) +
      geom_point() +
      geom_line() 
  }
  
  gg1 <- gg +
    labs(x = "", y = "", title = player) +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 30, size = 7)) 
  
  return(list(gg = gg1, df_gg = df4))
}
