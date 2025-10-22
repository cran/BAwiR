#' ACB shooting data
#' 
#' @aliases do_scrape_shots_acb
#' 
#' @description 
#' Obtain the shooting data from the ACB website and creates a common R data structure.
#' Each shot is described with its (x, y) coordinates and other additional information, 
#' such as the outcome of the shot (made or missed) or the player who took that shot.
#' 
#' @usage 
#' do_scrape_shots_acb(data_days, verbose, user_agent_def, x_apikey)
#' 
#' @param data_days Data frame with the game codes of each day. 
#' It is obtained with \code{\link{do_scrape_days_acb}}.
#' @param verbose Should R report information on progress? TRUE or FALSE.
#' @param user_agent_def String with the user agent.
#' @param x_apikey String with the X-APIKEY.
#' 
#' @note
#' The original codes of the playType column have the following meaning:
#' 92: ft made;    93: 2pt made;   94: 3pt made; 96: ft missed.
#' 97: 2pt missed; 98: 3pt missed; 100: dunk.
#' 
#' @return 
#' A data frame with the shooting data.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_scrape_days_acb}}
#' 
#' @examples 
#' \dontrun{
#' data_days <- do_scrape_days_acb("2024", "analyst_name", TRUE, 2, 975)
#' 
#' data_shots <- do_scrape_shots_acb(data_days[1:2, ], TRUE, "user_agent_def", "x_apikey")
#' }
#' 
#' @importFrom httr stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom janitor clean_names
#' 
#' @export

do_scrape_shots_acb <- function(data_days, verbose, user_agent_def, x_apikey){
  logo <- pos_x <- local <- play_type <- day <- id_game <- NULL
  
  url <- "https://api2.acb.com/api/matchdata/MatchShots/match-shots"
  
  data_shots <- data.frame()
  for (i in 1:nrow(data_days)) {
    if (verbose) {
      cat("DAY:", data_days$day[i], "\n")
      cat("GAME:", data_days$id_game[i], "\n")
    }
    
    res <- GET(
      url,
      query = list(matchId = data_days$id_game[i]), # "104052" "104053"
      add_headers(
        `Accept` = "/*/",
        `Referer` = "https://live.acb.com/",
        `User-Agent` = user_agent_def,
        `X-APIKEY` = x_apikey
      )
    )
    
    stop_for_status(res) # If request was successful, the response is invisible. 
    
    json <- content(res, as = "text", encoding = "UTF-8")
    shots <- fromJSON(json, flatten = TRUE)
    
    home_team <- as.data.frame(shots$homeTeam) %>%
      clean_names() %>%
      select(contains("name"), logo) %>%
      mutate(local = TRUE)
    
    away_team <- as.data.frame(shots$awayTeam) %>%
      clean_names() %>%
      select(contains("name"), logo) %>%
      mutate(local = FALSE)
    
    home_players <- shots$homePlayerStats %>%
      clean_names()
    
    away_players <- shots$awayPlayerStats %>%
      clean_names()
    
    shots_data <- shots$shotPoints
    
    shots_data_clean <- shots_data %>% 
      clean_names() %>%
      filter(!play_type %in% c(92, 96, 533)) %>%
      mutate(play_type = plyr::mapvalues(play_type, 
                                         from = c(93, 94, 97, 98, 100), 
                                         to = c("2pt_made", "3pt_made", "2pt_missed", "3pt_missed", "2pt_made")))
    
    df0_home <- shots_data_clean %>%
      filter(local == TRUE) %>%
      left_join(home_team, by = "local") %>%
      left_join(home_players, by = "player_license_id")
    
    df0_away <- shots_data_clean %>%
      filter(local == FALSE) %>%
      left_join(away_team, by = "local") %>%
      left_join(away_players, by = "player_license_id")
    
    df0 <- bind_rows(df0_home, df0_away) %>%
      separate(play_type, c("shot", "outcome"), sep = "_", remove = FALSE)
    
    data_shots_iter <- df0 %>%
      mutate(day = data_days$day[i], .before = pos_x) %>%
      mutate(id_game = data_days$id_game[i], .before = pos_x)
    
    data_shots <- rbind(data_shots, data_shots_iter)
    
    # Take slowly:
    Sys.sleep(5)
  }
  
  data_shots$play_type <- factor(data_shots$play_type, levels = c("2pt_made", "2pt_missed", "3pt_made", "3pt_missed")) 
  
  return(data_shots)
}
