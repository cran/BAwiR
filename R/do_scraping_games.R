#' Player game finder data 
#' 
#' @aliases do_scraping_games
#'
#' @description 
#' This function calls the needed ancillary functions to scrape the player game 
#' finder data for the desired competition (currently, ACB, Euroleague and Eurocup).
#' 
#' @usage 
#' do_scraping_games(competition, type_league, nums, year, verbose, accents, r_user)
#' 
#' @param competition String. Options are "ACB", "Euroleague" and "Eurocup".
#' @param type_league String. If \code{competition} is ACB, to scrape 
#' ACB league games ("ACB"), Copa del Rey games ("CREY") or Supercopa games ("SCOPA").
#' @param nums Numbers corresponding to the website from which scraping.
#' @param year If \code{competition} is either Euroleague or Eurocup, the year
#' when the season starts is needed. 2017 refers to 2017-2018 and so on.
#' @param verbose Should R report information on progress? Default TRUE.
#' @param accents If \code{competition} is ACB, should we keep the Spanish accents? 
#' The recommended option is to remove them, so default FALSE.
#' @param r_user Email to identify the user when doing web scraping. 
#' This is a polite way to do web scraping and to certify that the user 
#' is working as transparently as possible with a research purpose.
#' 
#' @return 
#' A data frame with the player game finder data for the competition selected.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{scraping_games_acb}}, \code{\link{scraping_games_euro}}
#' 
#' @examples 
#' \dontrun{
#' # Not needed to scrape every time the package is checked, built and installed.
#' df1 <- do_scraping_games(competition = "ACB", type_league = "ACB", nums = 62001,
#'                          year = "2017-2018", verbose = TRUE, accents = FALSE, 
#'                          r_user = "guillermo.vinue@uv.es")
#'                          
#' df1_eur <- do_scraping_games(competition = "Euroleague", nums = 1,
#'                          year = "2017", verbose = TRUE,
#'                          r_user = "guillermo.vinue@uv.es")                          
#' }                          
#'
#' @export

do_scraping_games <- function(competition, type_league, nums, year, verbose, accents, r_user) {
  if (competition == "ACB") {
    df <- scraping_games_acb(type_league, nums, year, verbose, accents, r_user)
  }
  
  if (competition %in% c("Euroleague", "Eurocup") ) {
    df <- scraping_games_euro(competition, nums, year, verbose, r_user)
  }
  
  if (!competition %in% c("ACB", "Euroleague", "Eurocup")) {
    print("This competition is not available.")
  }

  return(df)
}