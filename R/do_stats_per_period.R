#' Compute stats per period
#' 
#' @aliases do_stats_per_period
#'
#' @description 
#' Compute time played and points scored for a player of interest in any period
#' of the game.
#' 
#' @usage 
#' do_stats_per_period(data, day_num, game_code, team_sel, period_sel, player_sel)
#' 
#' @param data Prepared data from a given game.
#' @param day_num Day number.
#' @param game_code Game code.
#' @param team_sel One of the teams' names involved in the game.
#' @param period_sel Period of interest. Options can be "xC", where x=1,2,3,4.
#' @param player_sel Player of interest.
#' 
#' @return 
#' Data frame with one row and mainly time played (seconds and minutes) and points
#' scored by the player of interest in the period of interest.
#' 
#' @note 
#' The \strong{game_code} column allows us to detect the source website, for example,
#' \url{https://jv.acb.com/es/103389/jugadas}.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' library(dplyr)
#' df0 <- acb_vbc_cz_pbp_2223
#' 
#' day_num <- unique(acb_vbc_cz_pbp_2223$day)
#' game_code <- unique(acb_vbc_cz_pbp_2223$game_code)
#' 
#' # Remove overtimes:
#' rm_overtime <- TRUE
#' if (rm_overtime) {
#'  df0 <- df0 %>%
#'    filter(!grepl("PR", period)) %>%
#'    mutate(period = as.character(period))
#' }
#'  
#' team_sel <- "Valencia Basket" # "Casademont Zaragoza"
#' period_sel <- "1C"            # "4C"
#' player_sel <- "Webb"          # "Mara"
#'  
#' df1 <- df0 %>%
#'   filter(team == team_sel) %>%
#'   filter(!action %in% c("D - Descalificante - No TL", "Altercado no TL")) 
#'    
#' df2 <- df1 %>%
#'   filter(period == period_sel)
#'    
#' df0_inli_team <- acb_vbc_cz_sl_2223 %>% 
#'    filter(team == team_sel, period == period_sel)
#'  
#' df3 <- do_prepare_data(df2, day_num, 
#'                        df0_inli_team, acb_games_2223_info,
#'                        game_code)
#'                         
#' df4 <- do_stats_per_period(df3, day_num, game_code, team_sel, period_sel, player_sel)
#' #df4
#'
#' @importFrom tibble is_tibble
#' @importFrom lubridate seconds_to_period
#'
#' @export

do_stats_per_period <- function(data, day_num, game_code, team_sel, period_sel, player_sel) {
  action <- player <- points <- time_point <- NULL
  
  data0 <- data %>%
    filter(!(time_point == "10:00" & action %in% c("Sale de la pista", "Entra a pista"))) %>%
    mutate(action = ifelse(action == "Quinteto inicial", "Entra a pista", action)) %>%
    mutate(points = case_when(
      action == "Tiro Libre anotado" ~ 1,
      action == "Mate" ~ 2,
      action == "Tiro de 2 anotado" ~ 2,
      action == "Triple anotado" ~ 3))
  
  data1 <- data0 %>%
    filter(player == player_sel)
  
  if (nrow(data1) == 0) {
    data_res <- NULL
  }else{
    # Case when there are two consecutive "Entra a pista", for example, 
    # Hollatz 3C 9/103312
    if (nrow(data1) > 1 & all(data1$action[1:2] == "Entra a pista")) {
      data1 <- data1[-2,]
    }
    
    # Add the last row of games' data to have the real final game score 
    # in case it is not available:
    last_row_game <- data.frame(matrix(NA, nrow = 1, ncol = ncol(data1)))
    colnames(last_row_game) <- colnames(data1)
    last_row_game$time_point <- "00:00"
    
    data2 <- bind_rows(data1, last_row_game) %>%
      mutate(time_point = period_to_seconds(ms(time_point)))
    
    row_in <- which(data2$action == "Entra a pista")
    row_out <- which(data2$action == "Sale de la pista")
    
    # For the case when the period finishes with the player: 
    if (length(row_out) < length(row_in)) {
      row_out <- c(row_out, nrow(data2))
    }
    
    # For the case when there is neither "Entra a pista" nor "Sale de la pista",
    # for example Bamforth 3C 29/103282
    if (length(row_in) == 0 & length(row_out) == 0) {
      row_in <- 1
      row_out <- nrow(data2)
    }
    
    total_min <- c()
    total_pts <- c()
    for (i in 1:length(row_in)) {
      tp_time <- data2[c(row_in[i], row_out[i]), "time_point"]
      if (is_tibble(tp_time)) {
        tp_time <- tp_time %>% pull() 
      }
      
      total_min <- c(total_min, diff(rev(tp_time)))
      
      tp_pt <- data2[row_in[i]:row_out[i], ] %>%
        filter(player == player_sel) %>%
        pull(points)
      
      total_pts <- c(total_pts, tp_pt)
    }
    
    data_res <- data.frame(day = day_num,
                           game_code = game_code,
                           team = team_sel,
                           player = player_sel,
                           period = period_sel,
                           seconds = sum(total_min),
                           minutes = seconds_to_period(sum(total_min)),
                           points = sum(total_pts, na.rm = TRUE))
  }
  
  return(data_res)
}
