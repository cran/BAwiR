#' ACB day game codes
#' 
#' @aliases do_scrape_days_acb
#'
#' @description 
#' Obtain the game codes of any regular season day from any ACB season. 
#' These game codes can be used for example to define the target url 
#' from which collecting the shooting data of every game. 
#' 
#' @usage 
#' do_scrape_days_acb(edition_id)
#' 
#' @param edition_id Identifier of the league edition. For 2026-2027 is 91. 
#' For coming seasons, check it at the ACB website.
#' 
#' @note
#' Before starting the web scraping, we must visit 
#' \url{https://acb.com/robots.txt} to check for permissions.
#' 
#' @return 
#' A data frame with two columns, one with the day and the other with 
#' the game code. For 34 days and 9 games per day, there must be 306 rows.
#' 
#' @author 
#' Guillermo Vinue with help from ChatGPT.
#' 
#' @seealso 
#' \code{\link{do_scrape_shots_acb}}
#' 
#' @examples 
#' \dontrun{
#' data_days <- do_scrape_days_acb(91)
#' }
#' 
#' @importFrom rvest read_html
#' @importFrom stringr str_match str_extract_all
#'
#' @export

do_scrape_days_acb <- function(edition_id){
  url_acb <- paste0("https://acb.com/es/liga/calendario?temporada=", edition_id)
  
  x <- read_html(url_acb) %>%
    html_text()
  
  # Split at each roundNumber:
  blocks <- str_split(x, '(?=\\\\?"roundNumber\\\\?":)', simplify = FALSE)[[1]]
  
  result <- lapply(blocks, function(block) {
    rN <- str_match(block, 'roundNumber\\\\?":(\\d+)')[, 2]
    
    # Everything after "matches":[
    after_matches <- str_split(block, 'matches\\\\?":\\[', n = 2)[[1]]
    
    if (length(after_matches) < 2) return(NULL)
    
    matches_text <- after_matches[2]
    
    # ALL six-digit IDs after matches:
    ids <- str_extract_all(matches_text, 'id\\D*(\\d{6})(?!\\d)')[[1]]
    
    # Extract only the numbers:
    ids <- str_extract(ids, '\\d{6}')
    
    data.frame(rN = rN, id = ids)
  }) %>%
    bind_rows()
  
  result_def <- result[-which(duplicated(result$id)), ]
  
  return(result_def)
}
