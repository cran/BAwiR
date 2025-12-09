#' Monthly stats
#' 
#' @aliases get_plot_monthly_stats
#'
#' @description 
#' In all the available basketball websites, the stats are presented for the whole
#' number of games played. This function represents the players' stats 
#' for each month, which is very useful to analyze the players' evolution. The plot
#' can be either a bar plot or a line plot.
#' 
#' @usage 
#' get_plot_monthly_stats(df_stats, title, size_text = 2.5, type_plot, language, 
#'                        same_team = FALSE, hjust_val = 2, vjust_val = 0.5)
#' 
#' @param df_stats Data frame with the statistics.
#' @param title Plot title.
#' @param size_text Label size for each bar. Default 2.5.
#' @param type_plot String, either 'bar_plot' or 'line_plot'.
#' @param language String, either 'English' or 'Spanish'. Needed for the line plot.
#' @param same_team Logical to specify if players selected belong to the same team.
#' If so, the facet labels in the line plot can be abbreviated.
#' @param hjust_val Adjust horizontally the text in the line plot.
#' @param vjust_val Adjust vertically the text in the line plot.
#' 
#' @return 
#' Graphical device.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{capit_two_words}}
#' 
#' @examples 
#' \dontrun{
#' library(dplyr)
#' 
#' compet <- "ACB"
#' 
#' df <- do_join_games_bio(compet, acb_games_1718, acb_players_1718)
#' df1 <- do_add_adv_stats(df)
#' 
#' months <- c(df %>% distinct(Month))$Month
#' months_order <- c("septiembre", "octubre", "noviembre", "diciembre", "enero")
#' months_plot <- match(months_order, months)
#' months_plot1 <- months_plot[!is.na(months_plot)]
#' months_plot2 <- months[months_plot1]
#'
#' df3_m <- df1 %>%
#' filter(Team == "Real_Madrid", 
#'       Player.x == "Doncic, Luka") %>%
#'  group_by(Month) %>%
#'  do(do_stats(., "Average", "2017-2018", "ACB", "Regular Season")) %>%
#'  ungroup() %>%
#'  mutate(Month = factor(Month, levels = months_plot2)) %>%
#'  arrange(Month)
#' 
#' stats <- c("GP", "MP", "PTS", "FGA", "FGPerc", "ThreePA", 
#'            "ThreePPerc", "FTA", "FTPerc",
#'            "TRB", "ORB", "AST", "TOV", "STL")
#'            
#' df3_m1 <- df3_m %>%
#'   select(1:5, all_of(stats), 46:50) %>%
#'   mutate(Month = plyr::mapvalues(Month, 
#'                                  from = c("octubre", "noviembre", "diciembre", "enero"),
#'                                  to = c("October", "November", "December", "January")))
#'   
#' get_plot_monthly_stats(df3_m1, paste("ACB", "2017-2018", "Average", sep = " ; "), 
#'                        2.5, "bar_plot", "English")
#'                        
#' get_plot_monthly_stats(df3_m1, paste("ACB", "2017-2018", "Average", sep = " ; "), 
#'                        2.5, "line_plot", "English")
#' }
#' 
#' @importFrom ggplot2 geom_bar facet_grid label_wrap_gen ylim coord_flip
#' @importFrom stats reformulate
#'
#' @export

get_plot_monthly_stats <- function(df_stats, title, size_text = 2.5, type_plot, language, 
                                   same_team = FALSE, hjust_val = 2, vjust_val = 0.5){
  Team <- Name <- CombinID <- Season <- Compet <- Month <- NULL
  Type_season <- Type_stats <- variable <- value <- NULL
  
  df_stats1 <- df_stats %>%
    select(-c(CombinID, Position, Season, Compet, Type_season, Type_stats))

  df_stats2 <- melt(df_stats1)
  # Order the stats:
  df_stats2$variable <- factor(df_stats2$variable, 
                               levels = rev(levels(df_stats2$variable)))
  
  df_stats2 <- df_stats2 %>%
    mutate(Month = tolower(Month))

  if (language == "English") {
    df_stats3 <- df_stats2 %>%
      mutate(Month = plyr::mapvalues(Month, 
                               from = c("september", "october", "november", "december", 
                                        "january", "february", "march", "april", "may", "june"),
                               to = as.character(c(9:12, 1:6))))
  }else if (language == "Spanish") {
    df_stats3 <- df_stats2 %>%
      mutate(Month = plyr::mapvalues(Month, 
                               from = c("septiembre", "octubre", "noviembre", "diciembre", 
                                        "enero", "febrero", "marzo", "abril", "mayo", "junio"),
                               to = as.character(c(9:12, 1:6))))
  }else{
    stop("This language is not available.")
  }
  
  df_stats3$Month <- factor(df_stats3$Month, levels = c(9:12, 1:6))
  
  if (type_plot == "bar_plot") {
    gg <- ggplot(df_stats3, aes(variable, value)) + 
      geom_bar(stat = "identity", color = "black", fill = "white") +
      facet_grid(Month ~ Name + Team, labeller = label_wrap_gen(width = 1)) + 
      geom_text(aes(label = value), hjust = -0.2, size = size_text, color = "red") +
      coord_flip() +
      ggtitle(title) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 7),
            strip.text = element_text(size = 20)) 
  }else if (type_plot == "line_plot") {
    gg_aux <- ggplot(df_stats3, aes(x = Month, y = value, group = Name)) + 
      geom_point() +
      geom_line() 
    
    if (same_team) {
      gg_aux1 <- gg_aux +
        facet_grid(variable ~ Name, labeller = label_wrap_gen(width = 1), scales = "free_y") 
    }else{
      gg_aux1 <- gg_aux +
        facet_grid(variable ~ Name + Team, labeller = label_wrap_gen(width = 1), scales = "free_y") 
    }
    
    gg <- gg_aux1 + 
      scale_y_continuous(breaks = function(x) floor(seq(min(x), max(x), length.out = 5))) + 
      geom_text(aes(label = value), hjust = hjust_val, vjust = vjust_val, size = size_text, color = "red") +
      ggtitle(title)
  }else{
    stop("This type of plot is not available.")
  }
  
  return(gg)
}
