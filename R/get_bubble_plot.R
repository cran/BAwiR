#' Basketball bubble plot
#' 
#' @aliases get_bubble_plot
#'
#' @description 
#' This plot is a representation of the percentiles of all statistics 
#' for a particular player. The figure shows four cells. The first box 
#' contains the percentiles between 0 and 24. The second, between 25 and 49. 
#' The third, between 50 and 74 and the fourth, between 75 and 100. The
#' percentiles are computed with the function 
#' \code{\link[Anthropometry]{percentilsArchetypoid}}.
#' Boxes of the same percentile category are in the same color in the interests
#' of easy understanding.
#'
#' This type of visualization allows the user to analyze each player in a very 
#' simple way, since a general idea of those aspects of the game in which the 
#' player excels can be obtained.
#' 
#' @usage get_bubble_plot(df_stats, player, descr_stats, size_text, size_text_x, size_legend)
#' 
#' @param df_stats Data frame with the statistics.
#' @param player Player. 
#' @param descr_stats Description of the statistics for the legend.
#' @param size_text Text size inside each box.
#' @param size_text_x Stats labels size.
#' @param size_legend Legend size.
#' 
#' @return 
#' Graphical device.
#' 
#' @author 
#' This function has been created using the code from this website:
#' \url{https://www.r-bloggers.com/2017/01/visualizing-the-best/}.
#' 
#' @details 
#' In the example shown below, it can be seen that Alberto Abalde has a percentile of 
#' x in free throws percentage. This means that the x percent of league players has a fewer 
#' percentage than him, while there is a (100-x) percent who has a bigger percentage.
#' 
#' @seealso 
#' \code{\link[Anthropometry]{percentilsArchetypoid}}
#' 
#' @examples 
#' \dontrun{
#' compet <- "ACB"
#' df <- do_join_games_bio(compet, acb_games_1718, acb_players_1718)
#' df1 <- do_add_adv_stats(df)
#' df2 <- do_stats(df1, "Total", "2017-2018", compet, "Regular Season")
#' # When choosing a subset of stats, follow the order in which they appear
#' # in the data frame.
#' stats <- c("GP", "MP", "PTS", "FGA", "FGPerc", "ThreePA", "ThreePPerc", 
#'            "FTA", "FTPerc", "TRB", "ORB", "AST", "STL", "TOV")
#' df2_1 <- df2[, c(1:5, which(colnames(df2) %in% stats), 46:49)]
#' descr_stats <- c("Games played", "Minutes played", "Points", 
#'                 "Field goals attempted", "Field goals percentage", 
#'                 "3-point field goals attempted", "3-point percentage", 
#'                 "FTA: Free throws attempted", "Free throws percentage", 
#'                 "Total rebounds", "Offensive rebounds", 
#'                 "Assists", "Steals", "Turnovers")
#' get_bubble_plot(df2_1, "Abalde, Alberto", descr_stats, 6, 10, 12)
#' }
#' 
#' @importFrom Anthropometry percentilsArchetypoid
#' @importFrom ggplot2 geom_col geom_vline geom_hline coord_polar scale_color_grey
#' @importFrom ggplot2 scale_y_continuous element_rect scale_fill_manual guides
#' @importFrom grid unit
#'
#' @export

get_bubble_plot <- function(df_stats, player, descr_stats, size_text, size_text_x, size_legend){
  Name <- Team <- CombinID <- Position <- Nationality <- NULL 
  Season <- Compet <- Type_season <- Type_stats <- NULL
  stat <- outof4 <- percentile <- descr <- outof4_f <- NULL
  
  df_stats1 <- df_stats %>% 
    ungroup() %>%
    select(-c(Name, Team, CombinID, Position, Nationality, 
              Season, Compet, Type_season, Type_stats)) 
  
  percs <- sapply(1:dim(df_stats1)[2], percentilsArchetypoid, 
                  which(df_stats$Name == player), df_stats1, 0)
  
  df_cp <- data.frame(player = player,
                      percentile = percs,
                      stat = colnames(df_stats1),
                      outof4 = ifelse(percs == 100, 4, percs %/% 25 + 1))

  df_cp$descr <- descr_stats
  # Order is to get the stats legend in the same order as they are displayed in the circular plot.
  #df_cp <- df_cp[order(df_cp$stat),]
  df_cp <- df_cp[do.call("order", c(df_cp["stat"], list(decreasing = FALSE))),]
  labs <- with(df_cp, paste(stat, descr, sep = ": "))
  
  percs_player <- sort(unique(df_cp$outof4))
  labels_plot <- c("0-24", "25-49", "50-74", "75-100")
  df_cp$outof4_f <- factor(df_cp$outof4, labels = labels_plot[percs_player])

  cols_ggplot <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
  
  gg <- ggplot(df_cp, aes(x = stat, y = outof4, col = descr, fill = outof4_f)) + 
    geom_col(alpha = 0.5, width = 1, color = "white") + 
    scale_color_grey(labels = labs, end = 0) +
    scale_fill_manual(values = cols_ggplot[percs_player]) +
    geom_hline(yintercept = seq(0, 4, by = 1), 
               colour = "#949494", size = 0.5, lty = 3) + #949494 is dark grey.
    geom_vline(xintercept = seq(0.5, nrow(df_cp), 1), 
               colour = "#949494", size = 0.4, lty = 1) +
    facet_wrap(~player) +
    coord_polar() + 
    scale_y_continuous(limits = c(0, 4), breaks = c(1, 2, 3, 4)) + 
    labs(x = "", y = "", fill = "Percentiles", col = "") +#, col = "Statistics") + 
    geom_text(aes(label = percentile), nudge_y = -0.2, size = size_text) +
    geom_point(size = 0) +
    guides(colour = guide_legend(override.aes = list(size = 5))) +
    theme(panel.background = element_rect(fill = "#FFFFFF"), # plot with white (#FFFFFF) background.
          strip.background = element_rect(fill = "#FFFFFF"), # title with white background.
          #strip.text = element_text(size = 18), # text label
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = size_text_x),
          panel.spacing = unit(20, "lines"),
          legend.text = element_text(size = size_legend))
  
  return(gg)
}
