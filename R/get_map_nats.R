#' Nationalities map
#' 
#' @aliases get_map_nats
#'
#' @description 
#' A world map is represented. The countries from where there are players 
#' in the competition selected are in green color. 
#' 
#' @usage 
#' get_map_nats(df_stats)
#' 
#' @param df_stats Data frame with the statistics and the corrected nationalities.
#' 
#' @return 
#' Graphical device.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_map_nats}}  
#' 
#' @examples 
#' \dontrun{
#' compet <- "ACB"
#' df <- do_join_games_bio(compet, acb_games_1718, acb_players_1718)
#' df1 <- do_add_adv_stats(df)
#' df2 <- do_stats(df1, "Total", "2017-2018", compet, "Regular Season")
#' get_map_nats(df2)
#' }
#'         
#' @importFrom ggplot2 geom_polygon
#'                  
#' @export

get_map_nats <- function(df_stats){
  long <- lat <- region <- color_region <- NULL
  
  df_all <- do_map_nats(df_stats)$df_all

  P <- ggplot() + 
    geom_polygon(data = df_all, aes(x = long, y = lat, group = region, 
                                    fill = color_region), colour = "white") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(), 
          axis.title = element_blank(),
          legend.position = "none") 

  return(P)
}
