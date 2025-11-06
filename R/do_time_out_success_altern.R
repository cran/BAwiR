#' Alternative timeouts
#' 
#' @aliases do_time_out_success_altern
#'
#' @description 
#' This is an alternative to \code{\link{do_time_out_success}} from season 2025-2026
#' because to my best of knowledge the timeouts are not directly collected from
#' web scraping and I have to check manually when they were called.
#' 
#' @usage 
#' do_time_out_success_altern(data, day_num, game_code, team_sel, data_to_list, verbose)
#' 
#' @param data Prepared data from a given game.
#' @param day_num Day number.
#' @param game_code Game code.
#' @param team_sel One of the teams' names involved in the game.
#' @param data_to_list List with all the timeouts called from all coaches.
#' @param verbose Logical. Decide if information of the computations
#' must be provided or not.
#' 
#' @return 
#' Data frame.  This is the meaning of the columns:
#' \itemize{
#'   \strong{day}: Day number.
#'   \strong{game_code}: Game code.
#'   \strong{team}: Name of the corresponding team and coach.
#'   \strong{times_out_requested}: Number of timeouts requested in the game.
#'   \strong{times_out_successful}: Number of timeouts that resulted in scoring.
#'   \strong{times_out_successful_perc}: Percentage of successful timeouts.
#'   \strong{points_scored}: Total of points achieved after the timeouts.
#' }
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_time_out_success}}
#'
#' @export

do_time_out_success_altern <- function(data, day_num, game_code, team_sel, data_to_list, verbose) {
  period <- time_point <- row_number <- team <- points <- NULL
  
  data1 <- data %>%
    group_by(period) %>%
    mutate(row_number = match(time_point, unique(time_point)), .after = time_point) %>%
    ungroup()
  
  # Locate the position of time outs:
  to_pos_aux <- which(data$player == team_sel & data$action == "Tiempo Muerto")
  
  # Case when the time out is at the last row of the data, for example Joventut in 8/103315:
  lr <- which(to_pos_aux == nrow(data))
  if (length(lr) != 0) {
    to_pos_aux <- to_pos_aux[-lr]
  }
  
  # Case of no time out requested, for example Unicaja in 6/103297:
  if (length(to_pos_aux) == 0) {
    data_res <- tibble(day = day_num,
                       game_code = game_code,
                       team = team_sel, 
                       times_out_requested = 0,
                       times_out_successful = NA, 
                       times_out_successful_perc = NA, 
                       points_scored = NA)
  }else{
    # To delete duplicated time outs, see for example the fourth period 00:08 from 103346. 
    to_pos_dup <- data[to_pos_aux,] %>%
      mutate(to_pos_aux = to_pos_aux) %>%
      arrange(desc(to_pos_aux)) %>%
      distinct(period, time_point, .keep_all = TRUE) %>%
      pull(to_pos_aux)
    
    if (length(to_pos_dup) < length(to_pos_aux)) {
      to_pos <- rev(to_pos_dup)
    }else{
      to_pos <- to_pos_aux
    }
    
    points_scored <- 0 # To accumulate the total points scored after asking for a time out.
    times_succ <- 0 # To accumulate the times that a time out generated some points, i.e., was successful.
    
    for (i in 1:length(to_pos)) {
      bl_pos <- data1[to_pos[i], c("period", "row_number")]

      to_pos_after_points <- data1 %>%
        filter(period == bl_pos$period, row_number == bl_pos$row_number + 1, team == team_sel) %>%
        select(points) %>%
        pull()
      
      to_pos_after_points_nona <- to_pos_after_points[!is.na(to_pos_after_points)]
      
      if (length(to_pos_after_points_nona) != 0) {
        # sum() just in case there were more than one action with points, such as foul plus free throw.
        points_scored <- points_scored + sum(to_pos_after_points_nona)
        times_succ <- times_succ + 1
        
        data_to_list$points[i] <- sum(to_pos_after_points_nona)
      }else{
        data_to_list$points[i] <- 0
      }
    }
    
    times_succ_perc <- round((times_succ / length(to_pos)) * 100, 2) 
    
    data_res <- tibble(day = day_num,
                       game_code = game_code,
                       team = team_sel, 
                       times_out_requested = length(to_pos),
                       times_out_successful = times_succ, 
                       times_out_successful_perc = times_succ_perc, 
                       points_scored = points_scored) 
  }
  
  return(list(to_summ = data_res, to_list = data_to_list))
}
