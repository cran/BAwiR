#' Players profile data
#' 
#' @aliases do_scraping_rosters
#'
#' @description 
#' This function calls the needed ancillary functions to scrape the players' profile 
#' data for the desired competition (currently, ACB, Euroleague and Eurocup).
#' 
#' @usage 
#' do_scraping_rosters(competition, pcode, verbose, accents, year, r_user)
#' 
#' @param competition String. Options are "ACB", "Euroleague" and "Eurocup".
#' @param pcode Code corresponding to the player's website to scrape.
#' @param verbose Should R report information on progress? Default TRUE.
#' @param accents If \code{competition} is ACB, should we keep the Spanish accents? 
#' The recommended option is to remove them, so default FALSE.
#' @param year If \code{competition} is either Euroleague or Eurocup, the year
#' when the season starts is needed. 2017 refers to 2017-2018 and so on.
#' @param r_user Email to identify the user when doing web scraping. 
#' This is a polite way to do web scraping and to certify that the user 
#' is working as transparently as possible with a research purpose.
#' 
#' @return 
#' A data frame with the players' information.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{scraping_games_acb}}, \code{\link{scraping_rosters_euro}}
#' 
#' @examples 
#' \dontrun{
#' # Not needed to scrape every time the package is checked, built and installed.
#' df_bio <- do_scraping_rosters(competition = "ACB", pcode = "56C", 
#'                               verbose = TRUE, accents = FALSE, 
#'                               r_user = "guillermo.vinue@uv.es")
#'                               
#' df_bio_eur <- do_scraping_rosters(competition = "Euroleague", pcode = "007969", 
#'                               year = "2017", verbose = TRUE, 
#'                               r_user = "guillermo.vinue@uv.es")                               
#' }                               
#'
#' @export

do_scraping_rosters <- function(competition, pcode, verbose, accents, year, r_user) {
  if (competition == "ACB") {
    df <- scraping_rosters_acb(pcode, verbose, accents, r_user)
  }
  
  if (competition %in% c("Euroleague", "Eurocup") ) {
    df <- scraping_rosters_euro(competition, pcode, year, verbose, r_user)
  }
    
  return(df)  
}