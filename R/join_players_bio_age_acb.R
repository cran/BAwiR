#' Join ACB games and players' info
#' 
#' @aliases join_players_bio_age_acb
#'
#' @description 
#' This function joins the ACB games with the players' bio 
#' and computes the players' age at each game.
#' 
#' @usage 
#' join_players_bio_age_acb(df_games, df_rosters)
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
#' df <- join_players_bio_age_acb(acb_games_1718, acb_players_1718)
#'           
#' @importFrom dplyr filter left_join mutate    
#' @importFrom purrr map_if 
#' @importFrom tidyr drop_na
#' @importFrom lubridate month
#'                  
#' @export

join_players_bio_age_acb <- function(df_games, df_rosters){
  Date_birth <- Player.x <- CombinID <- Date <- Age <- NULL
  df_bio_updated1 <- df_rosters %>% 
    filter(Date_birth != "-</td>") %>%
    filter(Position != "|") %>%
    droplevels() %>% # To drop unused levels after filtering by factor
    map_if(is.factor, as.character) %>%  
    #as_data_frame()
    as_tibble()

  # Merge both data frames using column CombinID:
  df_merg <- left_join(df_games, df_bio_updated1, by = "CombinID") 

  # Remove rows with NA (Some players such as Tavares had two CombinID in the games scraping
  # but only one exists as a website, so there will be no biography data for these players
  # in the games with the wrong duplicated CombinID):
  df_merg1 <- df_merg %>% drop_na(Position)
  # Compute player's age the day of the game:
  df_merg_ages <- df_merg1 %>% 
    # Dividing by 365.25 is accurate enough.
    mutate(Age = (as.Date(Date, "%d/%m/%Y") - as.Date(Date_birth, "%d/%m/%Y")) / 365.25) %>%
    mutate(Age = round(Age, 5))
  
  # Add month:
  df_merg_ages$Month <- month(df_merg_ages$Date, label = TRUE, abbr = FALSE)
  
  return(df_merg_ages)
}  