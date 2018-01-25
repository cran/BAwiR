#' Join Euroleague and Eurocup games and players' info
#' 
#' @aliases join_players_bio_age_euro
#'
#' @description 
#' This function joins the Euroleague/Eurocup games with the players' bio 
#' and computes the players' age at each game.
#' 
#' @usage 
#' join_players_bio_age_euro(df_games, df_rosters)
#' 
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
#' \code{\link{do_join_games_bio}}
#' 
#' @examples 
#' df <- join_players_bio_age_euro(euroleague_games_1718, euroleague_players_1718)
#'           
#' @importFrom dplyr filter as_data_frame left_join mutate   
#' @importFrom purrr map_if 
#' @importFrom lubridate month
#'                  
#' @export

join_players_bio_age_euro <- function(df_games, df_rosters){
  Nationality <- Date_birth <- Date <- Age <- NULL
  df_bio_updated1 <- df_rosters %>% 
    filter(Nationality != "</div>") %>%
    droplevels() %>% # To drop unused levels after filtering by factor
    map_if(is.factor, as.character) %>%  
    as_data_frame()
  
  # Merge both data frames using column CombinID
  df_merg <- left_join(df_games, df_bio_updated1, by = "CombinID") 
  
  df_merg$Position[which(is.na(df_merg$Position))] <- "Unknown"
  
  df_merg1 <- df_merg %>% 
    filter(!is.na(Date_birth)) %>%
    filter(Date_birth != "01/01/1753")
  
  # Compute player's age the day of the game:
  df_merg_ages <- df_merg1 %>% 
    # Dividing by 365.25 is accurate enough.
    mutate(Age = (as.Date(Date, "%d/%m/%Y") - as.Date(Date_birth, "%d/%m/%Y")) / 365.25) %>%
    mutate(Age = round(Age, 5))
  
  # Add month:
  df_merg_ages$Month <- month(df_merg_ages$Date, label = TRUE, abbr = FALSE)
  
  return(df_merg_ages)
}  
