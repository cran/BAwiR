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
#'
#' This type of visualization allows the user to analyze each player in a very 
#' simple way, since a general idea of those aspects of the game in which the 
#' player excels can be obtained.
#' 
#' @usage get_bubble_plot(df_stats, player)
#' 
#' @param df_stats Data frame with the statistics.
#' @param player Player. 
#' 
#' @return 
#' Graphical device
#' 
#' @author 
#' This function has been created using the code of this website:
#' \url{https://www.r-bloggers.com/visualizing-the-best/}.
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
#' get_bubble_plot(df2, "Abalde, Alberto")
#' }
#' 
#' @importFrom Anthropometry percentilsArchetypoid
#' @importFrom ggplot2 geom_col geom_vline geom_hline coord_polar
#' @importFrom ggplot2 scale_y_continuous element_rect
#' @importFrom grid unit
#'
#' @export

get_bubble_plot <- function(df_stats, player){
  Name <- Team <- CombinID <- Position <- Nationality <- NULL 
  Season <- Compet <- Type_season <- Type_stats <- NULL
  stat <- outof4 <- percentile <- NULL
  
  
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
  
  gg <- ggplot(df_cp, aes(x = stat, y = outof4)) + 
    geom_col(alpha = 0.1, width = 1, show.legend = FALSE, color = "white") +
    geom_hline(yintercept = seq(0, 4, by = 1), 
               colour = "#949494", size = 0.5, lty = 3) + #949494 is dark grey.
    geom_vline(xintercept = seq(0.5, nrow(df_cp), 1), 
               colour = "#949494", size = 0.4, lty = 1) +
    facet_wrap(~player) +
    coord_polar() + 
    scale_y_continuous(limits = c(0, 4), breaks = c(1, 2, 3, 4)) + 
    labs(x = "", y = "") + 
    geom_text(aes(label = percentile)) +
    theme(panel.background = element_rect(fill = "#FFFFFF"), #FFFFFF is white.
          plot.background = element_rect(fill = "#FFFFFF"),
          strip.background = element_rect(fill = "#FFFFFF"),
          strip.text = element_text(size = 18), # text label
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10),
          panel.spacing = unit(20, "lines"))
  
  return(gg)
}