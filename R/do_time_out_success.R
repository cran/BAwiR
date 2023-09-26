#' Check if timeouts resulted in scoring
#' 
#' @aliases do_time_out_success
#'
#' @description 
#' For each team, locate the position of timeouts and check if they 
#' resulted in scoring points.
#' 
#' @usage 
#' do_time_out_success(data, day_num, game_code, team_sel, verbose)
#' 
#' @param data Prepared data from a given game.
#' @param day_num Day number.
#' @param game_code Game code.
#' @param team_sel One of the teams' names involved in the game.
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
#' \code{\link{do_prepare_data_to}}
#' 
#' @examples 
#' df0 <- acb_vbc_cz_pbp_2223
#' 
#' day_num <- unique(acb_vbc_cz_pbp_2223$day)
#' game_code <- unique(acb_vbc_cz_pbp_2223$game_code)
#' 
#' df1 <- do_prepare_data_to(df0, TRUE, acb_games_2223_info, acb_games_2223_coach)
#' 
#' # sort(unique(df1$team))
#' # "Casademont Zaragoza_Porfirio Fisac" "Valencia Basket_Alex Mumbru"
#' 
#' df2 <- do_time_out_success(df1, day_num, game_code, 
#'                            "Casademont Zaragoza_Porfirio Fisac", FALSE)
#' #df2
#'
#' @importFrom dplyr pull
#'
#' @export

do_time_out_success <- function(data, day_num, game_code, team_sel, verbose) {
  period <- time_point <- NULL
  
  # Locate the position of time outs:
  to_pos_aux <- which(data$player == team_sel & data$action == "Tiempo Muerto")
  
  # Case when the time out is at the last row of the data, for example Joventut in 8/103315:
  lr <- which(to_pos_aux == nrow(data))
  if (length(lr) != 0) {
    to_pos_aux <- to_pos_aux[-which(to_pos_aux == nrow(data))]
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
      to_pos_after <- c()
      # In the following loop, :nrow(data) is just for closing the set of rows to consider.
      # This especially works for the case when the time out is close to the end, see for
      # example row 527 of 14/103384.
      for (j in (to_pos[i] + 1):nrow(data)) {
        if (verbose) cat("ROW: ", j, "\n")
        
        # This first "if" is to avoid cases where actions are from different periods
        if (data$period[j] == data$period[j - 1]) {
          if (data$team[j] == team_sel) {
            to_pos_after <- c(to_pos_after, j)
          }else if (data$team[j] != team_sel & grepl("Falta Personal|T\\u00e9cnica", 
                                                     data$action[j])) {
            to_pos_after <- c(to_pos_after, j)
          }else{
            break()
          } 
        }else{
          break()
        } 
      }
      
      to_pos_after_points <- data[to_pos_after, "points"]$points
      to_pos_after_points_nona <- to_pos_after_points[!is.na(to_pos_after_points)]
      
      if (length(to_pos_after_points_nona) != 0) {
        # sum() just in case there were more than action with points, such as foul plus free throw.
        points_scored <- points_scored + sum(to_pos_after_points_nona)
        times_succ <- times_succ + 1
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
  
  return(data_res)
}
