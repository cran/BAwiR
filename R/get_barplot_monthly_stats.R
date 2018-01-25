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
#' get_barplot_monthly_stats(df_stats, team, player, title, nrow_facet)
#' 
#' @param df_stats Data frame with the statistics.
#' @param team Team.
#' @param player Player. 
#' @param title Plot title.
#' @param nrow_facet Number of facet rows.
#' 
#' @return 
#' Graphical device
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
#' df2 <- do_stats(df1, "Total", "2017-2018", compet, "Regular Season")
#' 
#' months <- c(df %>% distinct(Month))$Month
#' months_order <- c("September", "October", "November", "December", 
#'                   "January", "February", "March", "April", "May", "June")
#' months_plot <- match(months_order, months)
#' months_plot1 <- months_plot[!is.na(months_plot)]
#' months_plot2 <- months[months_plot1]
#'
#' df3 <- data.frame()
#' for (i in months_plot2) {
#'  df1_month <- df1 %>%
#'    filter(Month == i)
#'  df2_month <- do_stats(df1_month, "Total", "2017-2018", compet, "Regular Season") %>%
#'    mutate(Month = i)
#'  df3 <- bind_rows(df3, df2_month)
#'}
#' 
#' # For just one player:
#' player <- "Abalde, Alberto"
#' get_barplot_monthly_stats(df3, "Valencia", player, 
#'                           paste(compet, "2017-2018", unique(df3$Type_stats), sep = " "), 2)
#' 
#' # For all teams and players:
#' teams <- as.character(rev(sort(unique(df2$Team))))
#' for (i in teams[1:2]) {
#'   players <- sort(unique(df3[df3$Team == i, "Name"]))
#'   for (j in players[1:2]) {
#'     print(get_barplot_monthly_stats(df3, i, j, "ACB 2017-2018", 2))
#'   }
#' }
#' }
#' 
#' @importFrom ggplot2 geom_bar facet_grid label_wrap_gen ylim coord_flip
#' @importFrom stats reformulate
#'
#' @export

get_barplot_monthly_stats <- function(df_stats, team, player, title, nrow_facet){
  Team <- Name <- CombinID <- Nationality <- Season <- Compet <- NULL
  Type_season <- Type_stats <- variable <- value <- NULL
  
  df_stats1 <- df_stats %>%
    filter(Team %in% team, Name == player) %>% # Team %in% team instead Team == team because some players
    # have played for different teams in the same season, e.g., Pedro Llompart played for Valencia and Tenerife
    # in season 2017-2018.
    select(-c(Team, CombinID, Position, Nationality, Season, Compet, Type_season, Type_stats))
  # Order the months:
  df_stats1$Month <- factor(df_stats1$Month, 
                            levels = c("September", "October", "November", "December", 
                                      "January", "February", "March", "April", "May"))
  
  df_stats2 <- melt(df_stats1)
  # Order the stats:
  df_stats2$variable <- factor(df_stats2$variable, 
                               levels = rev(levels(df_stats2$variable)))
  
  gg <- ggplot(df_stats2, aes(variable, value)) + 
    geom_bar(stat = "identity", color = "black", fill = "white") +
    # reformulate works fine to pass string to facet_grid.
    # label_wrap_gen to split facet title in several lines.
    facet_wrap(reformulate("Month"), labeller = label_wrap_gen(width = 1), nrow = nrow_facet) + 
    #ylim(min(df_stats2$value), max(df_stats2$value)) + 
    geom_text(aes(label = value), hjust = -0.2, size = 2.5, color = "red") +
    coord_flip() +
    ggtitle(paste(paste(team, collapse = " and "), player, title, sep = "; ")) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 7),
          strip.text = element_text(size = 20)) 
  
  return(gg)
}
