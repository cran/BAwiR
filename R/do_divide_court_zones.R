#' Zones of the basketball court
#' 
#' @aliases do_divide_court_zones
#'
#' @description 
#' Divide the basketball court into 10 zones.
#' 
#' @usage 
#' do_divide_court_zones(data_shots)
#' 
#' @param data_shots Shooting data frame.
#' 
#' @return 
#' The shooting data frame with a new column called "location" indicating 
#' the zone from which each shot was taken.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' do_divide_court_zones(acb_shooting_data_2425)
#' }
#' 
#' @export

do_divide_court_zones <- function(data_shots){
  shot <- pos_x <- pos_y <- NULL
  
  data_zones <- data_shots %>%
    mutate(location = ifelse(shot == "3pt" & pos_x <= 1000 & pos_y >= 6600, "3pt_right_corner", 
                             ifelse(shot == "3pt" & pos_x <= 1000 & pos_y <= -6600, "3pt_left_corner", 
                                    ifelse(shot == "3pt" & pos_x > 1000 & pos_x <= 10000 & pos_y > 2000, "3pt_right", 
                                           ifelse(shot == "3pt" & pos_x > 1000 & pos_x <= 10000 & pos_y < -2000, "3pt_left", 
                                                  ifelse(shot == "3pt" & pos_x > 1000 & pos_x <= 10000 & pos_y >= -2000 & pos_y <= 2000, "3pt_center", 
                                                         ifelse(shot == "2pt" & pos_x <= 4000 & pos_y >= -2000 & pos_y <= 2000, "paint", 
                                                                ifelse(shot == "2pt" & pos_x > 4000 & pos_y >= -2000 & pos_y <= 2000, "2pt_center", 
                                                                       ifelse(shot == "2pt" & pos_y > 2000, "2pt_right", 
                                                                              ifelse(shot == "2pt" & pos_y < -2000, "2pt_left", 
                                                                                     ifelse(shot == "3pt" & pos_x > 10000, "3pt_long", 
                                                                                            "other")))))))))), .after = pos_y)
                                                                                     
  return(data_zones)
}
