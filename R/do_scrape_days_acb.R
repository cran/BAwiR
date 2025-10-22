#' ACB day game codes
#' 
#' @aliases do_scrape_days_acb
#'
#' @description 
#' Obtain the game codes of any regular season day from any ACB season. 
#' These game codes will be used to define the target url from which collecting 
#' the shooting data of every game.
#' 
#' @usage 
#' do_scrape_days_acb(season, analyst_name, verbose, num_days, edition_id)
#' 
#' @param season String with the starting year of the season. For example, "2024"
#' refers to the 2024-2025 season.
#' @param analyst_name Name to identify the user when doing web scraping. 
#' This is a polite way to do web scraping and certify that the user 
#' is working as transparently as possible with a research purpose.
#' @param verbose Should R report information on progress? TRUE or FALSE.
#' @param num_days Number of days to obtain.
#' @param edition_id Identifier of the league edition. For 2024 is 975
#' and for 2025 is 979. For coming seasons, check it at the ACB website, such as
#' \url{https://acb.com/calendario/index/temporada_id/2025} and click on any of
#' the days to see which url appears.
#' 
#' @note
#' Before starting the web scraping, we must visit 
#' \url{https://www.acb.com/robots.txt} to check for permissions.
#' 
#' @return 
#' A data frame with two columns, one with the days and the other with the game codes.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_scrape_shots_acb}}
#' 
#' @examples 
#' \dontrun{
#' data_days <- do_scrape_days_acb("2024", "analyst_name", TRUE, 2, 975)
#' }
#' 
#' @importFrom robotstxt paths_allowed
#' @importFrom polite bow scrape
#' @importFrom rvest html_attr
#'
#' @export

do_scrape_days_acb <- function(season, analyst_name, verbose, num_days, edition_id){
  url_acb <- paste0("https://acb.com/calendario/index/temporada_id/", season)
  
  if (paths_allowed(url_acb)) {
    session <- bow(url_acb, user_agent = paste0(analyst_name, ", polite R bot ", getOption("HTTPUserAgent")))
    
    if (verbose) {
      print(session)
    }
    
    id_days_all <- scrape(session) %>%
      html_nodes(xpath = './/div[@class="desplegable_personalizado desplegable_jornada roboto_bold"]') %>%
      html_nodes(xpath = './/div[@class="elemento colorweb_7 mayusculas"]') %>%
      html_attr("data-t2v-id") 
    
    id_days <- id_days_all[1:num_days]
    
    data_days <- data.frame()
    for (i in 1:length(id_days)) {
      if (verbose) {
        cat("DAY:", id_days[i], "\n")
      }
      
      url_acb_day <- paste0("https://acb.com/calendario/index/temporada_id/", 
                            season, "/edicion_id/", edition_id, "/jornada_id/", id_days[i])
      
      if (paths_allowed(url_acb_day)) {
        session1 <- bow(url_acb_day, user_agent = paste0(analyst_name, ", polite R bot ", getOption("HTTPUserAgent")))
        
        if (verbose) {
          print(session1)
        }
        
        id_games_all <- scrape(session1) %>%
          html_nodes(xpath = './/article[@class="partido"]') %>%
          html_nodes(xpath = './/article[@class="varios"]') %>%
          html_nodes("a") %>%
          html_attr("href") 
        
        id_games <- unique(gsub(".*\\/id\\/", "", id_games_all))
        
        data_iter <- data.frame(day = i, id_game = id_games)
        data_days <- rbind(data_days, data_iter)
      }
      
      # Take slowly:
      Sys.sleep(5)
    }
    
    return(data_days)
  }else{
    stop("No permission to access page")
  }
}
