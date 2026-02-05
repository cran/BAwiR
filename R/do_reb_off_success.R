#' Check if scoring after offensive rebounds
#' 
#' @aliases do_reb_off_success
#'
#' @description 
#' For each team and player, locate the position of offensive rebounds 
#' and check if they resulted in scoring points.
#' 
#' @usage 
#' do_reb_off_success(data, day_num, game_code, team_sel, verbose)
#' 
#' @param data Play-by-play prepared data from a given game.
#' @param day_num Day number.
#' @param game_code Game code.
#' @param team_sel One of the teams' names involved in the game.
#' @param verbose Logical. Decide if information of the computations
#' must be provided or not.
#' 
#' @return 
#' List with two data frames, one for the results for the team (\strong{stats_team}) 
#' and the other for the players (\strong{stats_player}).
#' The team data frame shows the number of offensive rebounds, the number of those
#' that finished in scoring (and the percentage associated) and the total of points 
#' scored.
#' The player data frame shows the player who grabbed the offensive rebound, the 
#' player who scored and how many points.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_prepare_data_or}}
#' 
#' @examples 
#' df0 <- acb_vbc_cz_pbp_2223
#' 
#' day_num <- unique(acb_vbc_cz_pbp_2223$day)
#' game_code <- unique(acb_vbc_cz_pbp_2223$game_code)
#' 
#' df1 <- do_prepare_data_or(df0, TRUE, acb_games_2223_info)
#' 
#' df2 <- do_reb_off_success(df1, day_num, game_code, "Valencia Basket", FALSE)
#' #df2                        
#'
#' @importFrom tibble tibble
#'
#' @export

do_reb_off_success <- function(data, day_num, game_code, team_sel, verbose) {
  # Locate the position of offensive rebounds:
  to_pos <- which(data$team == team_sel & data$action == "Rebote Ofensivo")
  
  # For teams:
  # To accumulate the total points scored after an offensive rebound:
  points_scored <- 0 
  # To accumulate the times that an offensive rebound generated some points, i.e., was successful:
  times_succ <- 0 
  
  # For players:
  # To accumulate the total points scored after an offensive rebound:
  points_scored_pl <- 0 
  
  stats_player <- data.frame()
  for (i in 1:length(to_pos)) {
    to_pos_after <- c()
    
    # For the case when Rebote Ofensivo is in the last row, for example 8/103319 Girona
    if ((to_pos[i] + 1) > nrow(data)) {
      next
    }else{
      # In the following loop, :nrow(data) is just for closing the set of rows to consider.
      for (j in (to_pos[i] + 1):nrow(data)) {
        if (verbose) cat("ROW: ", j, "\n")
        
        # This first "if" is to avoid cases where actions are from different periods.
        if (data$period[j] == data$period[j - 1]) {
          if (data$team[j] == team_sel) {
            to_pos_after <- c(to_pos_after, j)
          }else if (data$team[j] != team_sel & grepl("Falta Personal|T\u00e9cnica", 
                                                     data$action[j])) {
            to_pos_after <- c(to_pos_after, j)
          }else{
            break()
          } 
        }else{
          break()
        } 
      }
      
      # TEAM STATS:
      to_pos_after_points <- data[to_pos_after, "points"]$points
      to_pos_after_points_nona <- to_pos_after_points[!is.na(to_pos_after_points)]
      
      if (length(to_pos_after_points_nona) != 0) {
        # sum() just in case there were more than action with points, such as foul plus free throw.
        points_scored <- points_scored + sum(to_pos_after_points_nona)
        times_succ <- times_succ + 1
      }
      
      # PLAYER STATS:
      name_player_reb_off <- data$player[to_pos[i]]
      
      to_pos_after_points_pl <- data[to_pos_after, "points"]$points
      to_pos_after_points_pl_nona <- to_pos_after_points[!is.na(to_pos_after_points_pl)]
      
      if (length(to_pos_after_points_pl_nona) != 0) {
        points_scored_pl <- sum(to_pos_after_points_pl_nona)
      }
      
      name_player_reb_off_succ <- unique(data$player[to_pos_after[!is.na(to_pos_after_points_pl)]])
      
      stats_player_i <- tibble(day = day_num,
                               game_code = game_code,
                               team = team_sel, 
                               player_reb_off = name_player_reb_off,
                               player_reb_off_succ = name_player_reb_off_succ, 
                               points_scored = points_scored_pl)
      
      stats_player <- bind_rows(stats_player, stats_player_i) 
    }
  }
  
  # In 'stats_player' when 'player_reb_off' coincides with 'player_reb_reb_off_succ',
  # this means that the player who grabbed the rebound, scored.
  
  times_succ_perc <- round((times_succ / length(to_pos)) * 100, 2) 
  
  stats_team <- tibble(day = day_num,
                       game_code = game_code,
                       team = team_sel, 
                       times_reb_off = length(to_pos),
                       times_reb_off_succ = times_succ, 
                       times_reb_off_succ_perc = times_succ_perc, 
                       points_scored = points_scored)
  
  return(list(stats_team = stats_team, stats_player = stats_player))
}
