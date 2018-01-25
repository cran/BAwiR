#' Nationalities map
#' 
#' @aliases get_map_nats
#'
#' @description 
#' A world map is represented. The countries from where there are players 
#' in the competition selected are in green color. By hovering over each 
#' country, the user gets the exact number of players and their names.
#' 
#' @usage 
#' get_map_nats(competition, season, df_stats)
#' 
#' @param competition String. Options are "ACB", "Euroleague" and "Eurocup".
#' @param season Season, e.g. 2017-2018.
#' @param df_stats Data frame with the statistics and the
#' corrected nationalities.
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
#' get_map_nats("ACB", "2017-2018", df2)
#' }
#'         
#' @importFrom ggplot2 geom_polygon
#' @importFrom plotly ggplotly layout
#'                  
#' @export

get_map_nats <- function(competition, season, df_stats){
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

  
  if (competition == "ACB") {
    if (season %in% paste(1985:2010, 1986:2011, sep = "-")) {
      num_split <- 8
      size_plotly <- 8
    }else{
      num_split <- 4
      size_plotly <- 10
    }
  }else{
    num_split <- 4
    size_plotly <- 10
  }  
  
  P <- ggplotly(P, width = 750, height = 500) %>% 
    layout(autosize = FALSE, hoverlabel = list(font = list(size = size_plotly)))

  countr_num <- do_map_nats(df_stats)$countr_num
  leng <- do_map_nats(df_stats)$leng
  for (i in 1:leng) {
    P$x$data[[i]]$text <- strsplit(P$x$data[[i]]$text, "<")[[1]][1] # i = 230 is Mexico.
    P$x$data[[i]]$text <- trimws(gsub("region:", "",  P$x$data[[i]]$text))
    if (P$x$data[[i]]$text %in% names(countr_num)) {
      players <- df_stats$Name[df_stats$Nationality == P$x$data[[i]]$text]
      
      if (any(duplicated(players))) {
        players <- unique(players) # For examples, Llompart is counted twice because he played
        # in 2017-2018 for Valencia and Tenerife. 
        countr_num[which(names(countr_num) == P$x$data[[i]]$text)] <- length(players)
      }

      if (length(players) > 12) {
        players <- split(players, ceiling(seq_along(players) / num_split))
        players1 <- paste(players, collapse = "\n") 
        players1 <- gsub("\"", "", players1)
        players1 <- gsub("c\\(", "", players1)
        players1 <- gsub("\\)", "", players1)
      }else{
        players1 <- paste(players, collapse = "\n") 
      }
      P$x$data[[i]]$text <- paste(P$x$data[[i]]$text, ": ",
                                  countr_num[which(names(countr_num) == P$x$data[[i]]$text)],
                                  "\n",
                                  players1,
                                  sep = "")
    }
  }
  
  return(P)
}
