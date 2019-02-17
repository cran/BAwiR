#' Barplots with monthly stats
#' 
#' @aliases get_barplot_monthly_stats
#'
#' @description 
#' In all the available basketball websites, the stats are presented for the whole
#' number of games played. This function represents a barplot with the players' stats 
#' for each month, which is very useful to analyse the players' evolution.
#' 
#' @usage 
#' get_barplot_monthly_stats(df_stats, title, size_text = 2.5)
#' 
#' @param df_stats Data frame with the statistics.
#' @param title Plot title.
#' @param size_text Label size for each bar. Default 2.5.
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
#' compet <- "ACB"
#' df <- do_join_games_bio(compet, acb_games_1718, acb_players_1718)
#' df1 <- do_add_adv_stats(df)
#' 
#' months <- c(df %>% distinct(Month))$Month
#' months_order <- c("September", "October", "November", "December", 
#'                   "January", "February", "March", "April", "May", "June")
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
#'   select(1:5, stats, 46:50)
#' get_barplot_monthly_stats(df3_m1, paste("; ACB", "2017-2018", "Average", sep = " ; "), 
#'                           2.5)
#' 
#' # For all teams and players:
#' teams <- as.character(sort(unique(df1$Team)))
#' df3_m <- df1 %>%
#' filter(Team == teams[13]) %>%
#'  group_by(Month) %>%
#'  do(do_stats(., "Average", "2017-2018", "ACB", "Regular Season")) %>%
#'  ungroup() %>%
#'  mutate(Month = factor(Month, levels = months_plot2)) %>%
#'  arrange(Month)
#' 
#' df3_m1 <- df3_m %>%
#'   select(1:5, stats, 46:50)
#' 
#' for (i in unique(df3_m1$Name)) {
#'   print(i)
#'   print(get_barplot_monthly_stats(df3_m1 %>% filter(Name == i), 
#'                                   paste(" ; ACB", "2017-2018", "Average", sep = " ; "), 
#'                                   2.5))
#' }
#' }
#' 
#' @importFrom ggplot2 geom_bar facet_grid label_wrap_gen ylim coord_flip
#' @importFrom stats reformulate
#'
#' @export

get_barplot_monthly_stats <- function(df_stats, title, size_text = 2.5){
  Team <- Name <- CombinID <- Season <- Compet <- NULL
  Type_season <- Type_stats <- variable <- value <- NULL
  
  df_stats1 <- df_stats %>%
    #filter(Team %in% team, Name == player) %>% # Team %in% team instead Team == team because some players
    # have played for different teams in the same season, e.g., Pedro Llompart played for Valencia and Tenerife
    # in season 2017-2018.
    select(-c(CombinID, Position, Season, Compet, Type_season, Type_stats))
  # Order the months:
  #df_stats1$Month <- factor(df_stats1$Month, 
  #                          levels = c("September", "October", "November", "December", 
  #                                    "January", "February", "March", "April", "May"))
  
  df_stats2 <- melt(df_stats1)
  # Order the stats:
  df_stats2$variable <- factor(df_stats2$variable, 
                               levels = rev(levels(df_stats2$variable)))
  
  #gg <- ggplot(df_stats2, aes(variable, value)) + 
  #  geom_bar(stat = "identity", color = "black", fill = "white") +
  #  # reformulate works fine to pass string to facet_grid.
  #  # label_wrap_gen to split facet title in several lines.
  #  facet_wrap(reformulate("Month"), labeller = label_wrap_gen(width = 1), nrow = nrow_facet) + 
  #  #ylim(min(df_stats2$value), max(df_stats2$value)) + 
  #  geom_text(aes(label = value), hjust = -0.2, size = size_text, color = "red") +
  #  coord_flip() +
  #  #ggtitle(paste(paste(team, collapse = " and "), player, title, sep = "; ")) +
  #  ggtitle(paste(unique(df_stats2$Team), ";", unique(df_stats2$Name), title, sep = " ")) +
  #  theme(axis.title.x = element_blank(),
  #        axis.title.y = element_blank(),
  #        axis.text.x = element_text(size = 7),
  #        strip.text = element_text(size = 20)) 
  
  gg <- ggplot(df_stats2, aes(variable, value)) + 
    geom_bar(stat = "identity", color = "black", fill = "white") +
    facet_grid(Month~Name+Team, labeller = label_wrap_gen(width = 1)) + 
    geom_text(aes(label = value), hjust = -0.2, size = size_text, color = "red") +
    coord_flip() +
    ggtitle(title) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 7),
          strip.text = element_text(size = 20)) 
  
  return(gg)
}

