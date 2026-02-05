#' Get games with clutch time
#' 
#' @aliases do_clutch_time
#'
#' @description 
#' Obtain the games that have clutch time. The clutch time is generally defined as 
#' the game situation when the scoring margin is within 5 points with five or fewer 
#' minutes remaining in a game. In order to allow more clutch situations, the user
#' will be able to set the minutes left and the scoring difference.
#' 
#' @usage 
#' do_clutch_time(data, min_left = "05:00", score_diff = 5)
#' 
#' @param data Source play-by-play data.
#' @param min_left Minutes left to finish the game. Default 5 minutes.
#' @param score_diff Scoring difference. Default 5 points.
#' 
#' @return 
#' Data frame of the game that has clutch time.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' df0 <- do_clutch_time(acb_vbc_cz_pbp_2223)
#' #df0 # If no rows, that means that the game did not have clutch time.
#'
#' @export

do_clutch_time <- function(data, min_left = "05:00", score_diff = 5) {
  period <- time_point <- local_score <- visitor_score <- NULL
  
  # Check if there were overtimes after the fourth period:
  per_type <- unique(data$period)
  per_sel <- per_type[grep("4C|5C|6C|PR", per_type)]
  
  data_cl <- data.frame()
  for (k in per_sel) {
    # Clutch time:
    df1 <- data %>%
      filter(grepl(k, period), time_point < min_left) %>%
      mutate(local_score = as.numeric(local_score), visitor_score = as.numeric(visitor_score))
    
    diff_score <- abs(df1$local_score[1] - df1$visitor_score[1])
    
    if (diff_score > score_diff) {
      next
    }else{
      data_cl <- bind_rows(data_cl, df1)
    }
  }
    
  return(data_cl)
}
