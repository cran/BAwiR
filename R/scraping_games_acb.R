#' ACB player game finder data 
#' 
#' @aliases scraping_games_acb
#'
#' @description 
#' This is the new function to obtain the ACB box score data.
#' 
#' @usage 
#' scraping_games_acb(code, game_id, season = "2020-2021", 
#'                    type_season = "Regular Season",
#'                    user_email, user_agent_goo)
#' 
#' @param code Game code.
#' @param game_id Game id.
#' @param season Season, e.g. 2022-2023.
#' @param type_season Type of season, e.g. 'Regular season'.
#' @param user_email Email's user to identify the user when doing web scraping. 
#' This is a polite way to do web scraping and to certify that the user 
#' is working as transparently as possible with a research purpose.
#' @param user_agent_goo User-agent to identify the user when doing web scraping. 
#' This is a polite way to do web scraping and to certify that the user 
#' is working as transparently as possible with a research purpose.
#' 
#' @return 
#' A data frame with the player game finder data (box score data).
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{scraping_games_acb_old}}
#' 
#' @examples 
#' \dontrun{
#' # Not needed to scrape every time the package is checked, built and installed.
#' user_email <- "yours"       
#' user_agent_goo <- "yours" 
#' df1 <- scraping_games_acb("103350", 1, "2022_2023", "Regular Season",
#'                           user_email, user_agent_goo)
#' }                           
#'
#' @importFrom rvest html_nodes html_text
#' @importFrom stringi stri_replace_all_charclass
#' @importFrom dplyr across mutate_at vars relocate
#'
#' @export

scraping_games_acb <- function(code, game_id, season = "2020-2021", type_season = "Regular Season", 
                               user_email, user_agent_goo) {
  Number <- Player <- TwoP <- ThreeP <- FT <- RB <- Team <- GameRes <- NULL
  Coach <- Website <- NULL
  
  col_stats <- c("Number", "Player", "MP", "PTS", "TwoP", "TwoPPerc", 
                 "ThreeP", "ThreePPerc", "FT", "FTPerc", "TRB", "RB", 
                 "AST", "STL", "TOV", "Counteratt", "BLKfv", "BLKag",
                 "Dunks", "PF", "PFrv", "PlusMinus", "PIR")
  
  url_base <- "http://www.acb.com/partido/estadisticas/id/"
  url_link <- paste(url_base, code, sep = "")
  
  link_content <- GET(url_link, 
                      user_agent(str_c(user_agent_goo, R.version$version.string, sep = ", ")),
                      add_headers(from = user_email))
  
  if (link_content$status == 404) {
    stop("URL not found. Please check if it exists.")
  }else{
    url_html <- read_html(url_link)
    
    # Basic games' data:
    url_data <- url_html %>%
      html_nodes(xpath = './/div[@class="datos_fecha roboto_bold colorweb_4 float-left bg_principal"]') %>%
      html_text()
    url_data1 <- trimws(strsplit(url_data, "\\|")[[1]])
    
    # Referees:
    url_refs <- url_html %>%
      html_nodes(xpath = './/div[@class="datos_arbitros bg_gris_claro colorweb_2 float-left roboto_light"]') %>%
      html_text() 
    url_refs1 <- gsub(".*: ", "", url_refs)
    
    # Result and teams involved:
    url_res <- url_html %>%
      html_nodes(xpath = './/h6[@class="colorweb_4 bg_azul_oscuro roboto_bold"]') %>%
      html_text()
    res <- paste(readr::parse_number(url_res), collapse = " - ")
    teams <- paste(gsub(paste(readr::parse_number(url_res), collapse = "|"), "", url_res), collapse = "-")
    teams1 <- stri_replace_all_charclass(teams, "\\p{WHITE_SPACE}", "")
    teams_sep <- strsplit(teams1, "-")[[1]]
    
    # GAME:
    url_tab <- url_html %>%
      html_table(fill = TRUE)
    
    score <- url_tab[[1]] 
    score1 <- score[,-c(2,ncol(score))]
    score2 <- sapply(score1, function(x) paste(x[1], x[2], sep = "-"))[-1]
    
    # HOME:
    stats_home <- url_tab[[2]]
    stats_home1 <- stats_home[-1,]
    colnames(stats_home1) <- stats_home[1,]
    colnames(stats_home1) <- col_stats
    
    coach_home <- stats_home1$Player[stats_home1$Number == "E"]
    
    stats_home2 <- stats_home1 %>%
      filter(!Number %in% c("E", "5f")) %>% # "", 
      filter(Player != "Total")
    # Change the blank cells to 0:
    stats_home3 <- stats_home2 %>% 
      mutate(across(everything(), ~ifelse(.== "", 0, as.character(.)))) %>%
      mutate(Team = teams_sep[1],
             Coach = coach_home)
    
    # AWAY:
    stats_away <- url_tab[[3]]
    stats_away1 <- stats_away[-1,]
    colnames(stats_away1) <- stats_away[1,]
    colnames(stats_away1) <- col_stats
    
    coach_away <- stats_away1$Player[stats_away1$Number == "E"]
    
    stats_away2 <- stats_away1 %>%
      filter(!Number %in% c("E", "5f")) %>% # "", 
      filter(Player != "Total")
    # Change the blank cells to 0:
    stats_away3 <- stats_away2 %>% 
      mutate(across(everything(), ~ifelse(.== "", 0, as.character(.)))) %>%
      mutate(Team = teams_sep[2],
             Coach = coach_away)
    
    # Join home and away
    stats_game <- rbind(stats_home3, stats_away3) %>%
      mutate(GS = ifelse(grepl("\\*", Number), 1, 0), .after = Number) %>%
      separate(TwoP, c("TwoP", "TwoPA"), sep = "/", fill = "left") %>%
      separate(ThreeP, c("ThreeP", "ThreePA"), sep = "/", fill = "left") %>%
      separate(FT, c("FT", "FTA"), sep = "/", fill = "left") %>%
      separate(RB, c("DRB", "ORB"), sep = "\\+", fill = "left") %>%
      mutate_at(vars(contains("Perc")), ~gsub("%", "", .)) %>%
      mutate(Season = season, 
             Type_season = type_season,
             Day = gsub("JORNADA ", "", url_data1[1]),
             Date = url_data1[2],
             Game = tolower(gsub("-", " - ", teams1)),
             GameRes = res,
             GameID = game_id,
             Website = url_link) %>%
      relocate(Team, .after = GameRes) %>%
      mutate(Periods = paste(score2, collapse = " ; "),
             Time = url_data1[3],
             Place = url_data1[4],
             Audience = url_data1[5],
             Referees = url_refs1) %>%
      relocate(Coach, .after = Website)
    stats_game[is.na(stats_game)] <- "0"
    
    # PLAYERS:
    url_player <- read_html(url_link) %>%
      html_nodes(xpath = './/td[@class="nombre jugador ellipsis"]') %>%
      as.character() 
    url_player1 <- sapply(strsplit(url_player, '\\/ver*.'), `[`, 2)
    url_player2 <- gsub("-.*", "", url_player1)
    #url_player3 <- url_player2[!is.na(url_player2)]
    
    stats_game1 <- stats_game %>%
      mutate(CombinID = url_player2, .after = Website)
    
    return(data_game = stats_game1)
  }
}
