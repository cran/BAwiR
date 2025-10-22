#' Season-by-season stats
#' 
#' @aliases get_stats_seasons
#'
#' @description 
#' This function represents the average values of a set of statistics 
#' for certain players in every season where the players played. It gives
#' an idea of the season-by-season performance. 
#' 
#' @usage get_stats_seasons(df, competition, player, variabs, type_season, add_text, show_x_axis)
#' 
#' @param df Data frame with the games and the players info.
#' @param competition Competition.
#' @param player Players's names.
#' @param variabs Vector with the statistics to plot.
#' @param type_season String with the round of competition, for example regular season
#' or playoffs and so on.
#' @param add_text Boolean. Should text be added to the plot points?
#' @param show_x_axis Boolean. Should x-axis labels be shown in the plot? 
#' 
#' @return 
#' List with two elements:
#' \itemize{
#' \item gg Graphical device.
#' \item df_gg Data frame associated with the plot.
#' }
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' competition <- "ACB"
#' 
#' df <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718)
#' df$Compet <- competition
#' 
#' player <- "Carroll, Jaycee"
#' 
#' variabs <- c("GP", "MP", "PTS", "EFGPerc", "TRB", "AST", "TOV", "PIR")
#' 
#' plot_yearly <- get_stats_seasons(df, competition, player, variabs, "All", TRUE, TRUE)
#' plot_yearly$gg
#' 
#' # There are only games from the regular season in this demo data frame.
#' plot_yearly1 <- get_stats_seasons(df, competition, player, variabs, "Regular Season", 
#'                                   TRUE, TRUE)
#' plot_yearly1$gg
#' }
#'
#' @importFrom ggplot2 geom_point geom_line
#' @importFrom dplyr do
#'
#' @export

get_stats_seasons <- function(df, competition, player, variabs, type_season, 
                              add_text, show_x_axis){
  Compet <- Player.x <- Season <- Name <- Team <- NULL
  Type_season <- Age <- value <- variable <- NULL
  
  
  if (type_season == "All") {
    df1 <- df %>%
      filter(Compet == competition,
             Player.x %in% player)
  }else{
    df1 <- df %>%
      filter(Compet == competition,
             Player.x %in% player,
             Type_season == type_season)    
  }  

  # It might happen that the player hasn't played the competition.
  if (nrow(df1) == 0) { 
    return(NA)
  }
  
  df2 <- df1 %>%
    group_by(Player.x) %>%
    do(do_add_adv_stats(.))
  
  
  df_gg <- data.frame()
  df_all <- data.frame()
  for (i in 1:length(player)) {
    df3 <- data.frame()
    df21 <- df2 %>%
      filter(Player.x == player[i])
    
    for (j in rev(unique(df21$Season))) {
      df2_loop <- df21 %>%
        filter(Season == j)
      
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
    
    Date_birth <- unique(df21$Date_birth)
    # Age of Player at the start of October 1st of that season. 
    # For instance, season 2013-2014, age at October 1st 2013.
    #df4$Age <- round((as.Date(paste("1/10/", substr(df4$Season, 1, 4), sep = ""), "%d/%m/%Y") - 
    #                    as.Date(Date_birth, "%d/%m/%Y")) / 365.25, 1)
    # Age of Player at the start of February 1st of that season.
    # For instance, season 2013-2014, age at February 1st 2014.
    if (competition == "ACB") {
      year_season <- sapply(strsplit(df4$Season, "-"), `[`, 2) 
    }else{
      year_season <- sapply(strsplit(df4$Season, "-"), `[`, 2)
      year_season <- as.numeric(paste(20, year_season, sep = ""))
    }
    df4$Age <- trunc((as.Date(paste("1/02/", year_season, sep = ""), "%d/%m/%Y") - 
                        as.Date(Date_birth, "%d/%m/%Y")) / 365.25)
    
    df_gg <- rbind(df_gg, df4)
    
    df5 <- melt(df4 %>% select(-Age))     
    
    df_all <- rbind(df_all, df5)
  }  
  
  if (length(player) == 1){
    size_x_axis <- 13
    size_x_strip <- 17
  }else{
    size_x_axis <- 5
    size_x_strip <- 7
  }
  
  gg1 <- ggplot(df_all, aes(x = Season, y = value, group = variable, color = variable)) +
    geom_point() +
    geom_line() +
    facet_wrap(~Name, scales = "free") +
    labs(x = "", y = "") +
    theme(legend.title = element_blank(),
          strip.text.x = element_text(size = size_x_strip),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 15))
  
  if (add_text) {
    gg1 <- gg1 + geom_text(aes(label = value), hjust = -0.5, color = "black")
  }
  
  if (show_x_axis) {
    gg1 <- gg1 + theme(axis.text.x = element_text(angle = 30, size = size_x_axis))
  }
  
  return(list(gg = gg1, df_gg = df_gg))
}
