#' Join games and players' info
#' 
#' @aliases do_join_games_bio
#'
#' @description 
#' This function calls the needed ancillary functions to join the games played 
#' by all the players in the desired competition (currently ACB, Euroleague
#' and Eurocup) with their personal details. 
#' 
#' @usage 
#' do_join_games_bio(competition, df_games, df_rosters)
#' 
#' @param competition String. Options are "ACB", "Euroleague" and "Eurocup".
#' @param df_games Data frame with the games.
#' @param df_rosters Data frame with the biography of the roster players.
#' 
#' @return 
#' Data frame.
#' 
#' @author 
#' Guillermo Vinue
#'
#' @seealso 
#' \code{\link{join_players_bio_age_acb}}, \code{\link{join_players_bio_age_euro}}
#' 
#' @examples 
#' df <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718)
#'           
#' @importFrom dplyr filter as_data_frame     
#' @importFrom purrr map_if  
#'                  
#' @export

do_join_games_bio <- function(competition, df_games, df_rosters) {
  CombinID <- NULL # This is needed to avoid the issue:
  # 'no visible binding for global variable CombinID' when R CMD check
  # Pre-processing:
  # Filter by CombinID != 0 and CombinID != "NA": 
  # CombinID == 0 refers to the players who didn't play any minute in the corresponding game. 
  # CombinID == NA refers to the rows of "Equipo".
  df2 <- df_games %>% 
    filter(CombinID != 0, CombinID != "NA") %>% 
    droplevels() %>% # To drop unused levels after filtering by factor.
    map_if(is.factor, as.character) %>% 
    as_data_frame()
  
  if (competition == "ACB") {
    # Add players bio and age:
    df2_1 <- join_players_bio_age_acb(df2, df_rosters)
  }  
  
  if (competition %in% c("Euroleague", "Eurocup") ) {
    # Add players bio and age:
    df2_1 <- join_players_bio_age_euro(df2, df_rosters)
  }

  return(df_all = df2_1)
}