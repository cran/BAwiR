#' League cross table
#' 
#' @aliases get_table_results
#'
#' @description 
#' The league results are represented with a cross table. 
#' 
#' @usage get_table_results(df, competition, season)
#' 
#' @param df Data frame with the games and the players info.
#' @param competition Competition.
#' @param season Season.
#' 
#' @return 
#' List with these two elements:
#' \itemize{
#' \item plot_teams Graphical device with the cross table.
#' \item wins_teams Vector with the team wins.
#' }
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' df <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718)
#' df$Compet <- "ACB"
#' 
#' gg <- get_table_results(df, "ACB", "2017-2018")
#' 
#' gg$wins_teams
#' gg$plot_teams
#' }
#'
#' @importFrom ggplot2 geom_tile
#' @importFrom tidyr separate
#'
#' @export

get_table_results <- function(df, competition, season){
  Compet <- Type_season <- Season <- Game <- GameRes <- Win_num <- NULL
  Team <- Local <- Visitor <- Local_points <- Visitor_points <- Win <- NULL

  df1 <- df %>%
    filter(Compet == competition,
           Type_season == "Regular Season",
           Season == season) %>%
    distinct(Game, GameRes, Team)
  
  df11 <- df1 %>%
    group_by(Game) %>%
    mutate(Local = Team[1], Visitor = Team[2]) %>%
    distinct(Game, GameRes, Local, Visitor)
  
  if (competition == "ACB") {
    df2 <- df11 %>%
      ungroup() %>%
      separate(GameRes, c("Local_points", "Visitor_points"), " - ") %>%
      select(-Game)
  }else{
    df2 <- df11 %>%
      ungroup() %>%
      separate(GameRes, c("Local_points", "Visitor_points"), "-") %>%
      select(-Game)
  }
  df2$GameRes <- df11$GameRes
  
  df3 <- df2 %>%
    mutate(Local_points = as.numeric(Local_points)) %>%
    mutate(Visitor_points = as.numeric(Visitor_points)) %>%
    mutate(Win = ifelse(Local_points > Visitor_points, Local, Visitor))
  
  wins_teams <- sort(table(df3$Win), decreasing = TRUE)
  
  df4 <- df3 %>%
    select(-Local_points, -Visitor_points) %>%
    arrange(Local, Visitor) %>%
    mutate(Win_num = ifelse(Local == Win, 1, 2)) %>%
    select(-Win)
  
  df41 <- df4 %>%
    mutate(Win_num = as.character(Win_num))
  
  gg <- ggplot(data = df41, aes(x = Visitor, y = Local, fill = Win_num)) + 
    geom_tile(colour = "white",size = 0.2) +
    labs(title = paste("Regular Season", competition, season, sep = " ")) +
    theme(axis.ticks = element_blank(),
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 30)) +
    geom_text(aes(label = GameRes), size = 4)
  
  return(list(plot_teams = gg, wins_teams = wins_teams))
}