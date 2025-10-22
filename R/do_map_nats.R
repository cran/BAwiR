#' Data frame for the nationalities map
#' 
#' @aliases do_map_nats
#'
#' @description 
#' This function prepares the data frame with the nationalities
#' to be mapped with \code{\link{get_map_nats}}. It is used inside it.
#
#' 
#' @usage 
#' do_map_nats(df_stats)
#' 
#' @param df_stats Data frame with the statistics and the
#' corrected nationalities.
#' 
#' @return 
#' List with the following elements:
#' \itemize{
#' \item df_all: Data frame with each country, its latitudes and
#' longitudes and whether it must be coloured or not (depending on
#' if there are players from that country).
#' \item countr_num: Vector with the countries from where there are
#' players and the number of them.
#' \item leng: Number of countries in the world.
#' }
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{get_map_nats}}  
#'                  
#' @export

do_map_nats <- function(df_stats){
  
  countr_num <- table(df_stats$Nationality)
  countr_num <- countr_num[countr_num != 0]
  worldMap <- rworldmap::getMap() 
  leng <- length(worldMap$NAME)
  
  df_all <- data.frame()
  for (i in 1:leng) {
    df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
    name_reg <- as.character(worldMap$NAME[i])
    df$region <- name_reg
    if (name_reg %in% unique(df_stats$Nationality)) {
      df$color_region <- "Yes"
    }else{
      df$color_region <- "No"
    }
    
    colnames(df) <- list("long", "lat", "region", "color_region")
    
    df_all <- rbind(df_all, df)
  }
  
  return(list(df_all = df_all, countr_num = countr_num, leng = leng))
}