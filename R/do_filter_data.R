#' Filter shooting data
#' 
#' @aliases do_filter_data
#'
#' @description 
#' Filter the shooting data with the team or player of interest, and also by periods, minutes and game place.
#' If neither team nor player is given, the data from the whole league is used.
#' 
#' @usage 
#' do_filter_data(data_shots_zones, season_str, team, period, minute_vect, place, player)
#' 
#' @param data_shots_zones Shooting data with the court zones.
#' @param season_str String with the season.
#' @param team String with the team's full name. Nothing to filter if "".
#' @param period Number with the periods (1, 2, 3 and 4 for the common 
#' four quarters, 5 for the first overtime and 6 for the second overtime).
#' Nothing to filter if "".
#' @param minute_vect Vector with the minutes to filter by. 
#' Nothing to filter if "".
#' @param place String. If "Home" or "Casa", the local games are filtered.
#' Nothing to filter if "".
#' @param player String with the player's name. Nothing to filter if "".
#' 
#' @return 
#' A data frame with the filters applied.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_divide_court_zones}}
#' 
#' @examples 
#' \dontrun{
#' df0 <- do_divide_court_zones(acb_shooting_data_2425)
#' 
#' # Data for the whole league:
#' df1 <- do_filter_data(df0, "2024-2025", "", "", "", "", "")
#' 
#' # Data for a team:
#' df1 <- do_filter_data(df0, "2024-2025", "UCAM Murcia", "", "", "", "")
#' 
#' # Data for a player:
#' df1 <- do_filter_data(df0, "2024-2025", "", "", "", "", "D. Ennis")
#' 
#' # Other filters:
#' # By minutes:
#' df1 <- do_filter_data(df0, "2024-2025", "", "", c(8,10), "", "D. Ennis")
#' }
#'
#' @export

do_filter_data <- function(data_shots_zones, season_str, team, period, minute_vect, place, player) {
  season <- full_name <- quarter <- minute <- local <- player_name <- NULL
  
  df0 <- data_shots_zones %>%
    filter(season == season_str)
  
  if (team != "") {
    df1 <- df0 %>%
      filter(full_name == team)
  }else{
    df1 <- df0
  }
  
  if (period != "") {
    df2 <- df1 %>%
      filter(quarter %in% period)
  }else{
    df2 <- df1
  }
  
  if (!is.character(minute_vect)) {
    df3 <- df2 %>%
      filter(minute >= minute_vect[1] & minute <= minute_vect[2]) 
  }else{
    df3 <- df2
  }
  
  if (place != "") {
    if (place %in% c("Home", "Casa")) {
      df4 <- df3 %>%
        filter(local == TRUE)
    }else{
      df4 <- df3 %>%
        filter(local == FALSE)
    }
  }else{
    df4 <- df3
  }
  
  if (player != "") {
    df5 <- df4 %>%
      filter(player_name == player) 
  }else{
    df5 <- df4
  }
  
  return(df5)
}
