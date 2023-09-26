#' Euroleague and Eurocup players' profile
#'
#' @aliases
#' scraping_rosters_euro
#' 
#' @description
#' This function should allow us to obtain the basic information of each 
#' Euroleague/Eurocup player, including his birth date. Then, we will 
#' be able to compute the age that each player had in the date that he 
#' played each game. The websites used to collect information are
#' \url{https://www.euroleaguebasketball.net/euroleague/} and 
#' \url{https://www.euroleaguebasketball.net/eurocup/}.
#' 
#' @usage
#' scraping_rosters_euro(competition, pcode, year, verbose = TRUE, 
#'                       r_user = "guillermo.vinue@uv.es")
#'                     
#' @param competition String. Options are "Euroleague" and "Eurocup".                     
#' @param pcode Code corresponding to the player's website to scrape.
#' @param year Year when the season starts. 2017 refers to 2017-2018 and so on.
#' @param verbose Should R report information on progress? Default TRUE.
#' @param r_user Email user to identify the user when doing web scraping. 
#' This is a polite way to do web scraping and to certify that the user 
#' is working as transparently as possible with a research purpose.
#'
#' @return
#' Data frame with seven columns: 
#' \itemize{
#' \item CombinID: Unique ID to identify the players.
#' \item Player: Player's name.
#' \item Position: Player's position on the court.
#' \item Height: Player's height.
#' \item Date_birth: Player's birth date.
#' \item Nationality Player's nationality.
#' \item Website_player: Website.
#' }
#' 
#' @author
#' Guillermo Vinue
#'
#' @note 
#' In addition to use the email address to stay identifiable, the function also 
#' contains two headers regarding the R platform and version used.
#' 
#' \url{https://www.euroleaguebasketball.net/robots.txt}
#' there is no Crawl-delay field. However, we assume crawlers to pause between 
#' requests for 15 seconds. This is done by adding to the function the command
#' \code{Sys.sleep(15)}.
#'
#' @seealso 
#' \code{\link{do_scraping_rosters}}
#'
#' @examples
#' \dontrun{
#' # Not needed to scrape every time the package is checked, built and installed.
#' # It takes 15 seconds.
#' df_bio <- scraping_rosters_euro("Euroleague", "005791", "2017", verbose = TRUE,
#'                                  r_user = "guillermo.vinue@uv.es")
#' }
#'
#' @importFrom httr GET user_agent add_headers
#' @importFrom stringr str_extract str_replace str_c
#'
#' @export

scraping_rosters_euro <- function(competition, pcode, year, verbose = TRUE, 
                                  r_user = "guillermo.vinue@uv.es"){
  df <- NULL
  #for (i in 1:length(pcode)) 
  len_pcode <- length(pcode)
  # Instead of using 1:len_pcode, we can use seq_len(len_pcode) to avoid the backwards sequence bug.
  for (i in seq_len(len_pcode)) {
    if (verbose) {
      print(i)
      print(pcode[i]) 
    }
    
    if (competition == "Euroleague") {
      website <- paste("http://www.euroleague.net/competition/players/showplayer?pcode=", 
                       pcode[i], "&seasoncode=E", year, sep = "")      
    }else if (competition == "Eurocup") {
      website <- paste("http://www.eurocupbasketball.com/eurocup/competition/players/showplayer?pcode=", 
                       pcode[i], "&seasoncode=U", year, sep = "")  
    }
    
    if (verbose) {
      print(website)
    } 
    
    # This is just to check that the website exists, because with readLines the website
    # can be directly scraped.
    #get_website <- GET(website)
    get_website <- GET(website, 
                       user_agent(str_c(R.version$platform, R.version$version.string, sep = ", ")), 
                       add_headers(from = r_user))
    if (get_website$status_code == 404) { # The status code 404 is for 
      # the websites that cannot be found, i.e., the websites that 
      # don't exist.
      print("Web doesn't exist")
      next
    }
    
    if (verbose) {
      print("Ready to scrape")  
    }
    
    html_pl <- readLines(website, warn = FALSE) # There are some minor unimportant warnings.
    
    # Name:
    playerdata <- grep("player-data", html_pl)
    name1 <- html_pl[playerdata + 2]
    name2 <- gsub("<div class=\"name\">", "", name1)
    name3 <- gsub("</div>", "", name2)
    name4 <- gsub(" ", "", name3)
    name5 <- tools::toTitleCase(tolower(gsub(",", ", ", name4)))
    
    # Date of birth:
    born <- grep("summary-second", html_pl)
    born1 <- html_pl[born + 2]
    # There are players with no height such as:
    # http://www.eurocupbasketball.com/eurocup/competition/players/showplayer?pcode=BMO&seasoncode=U2002
    if (unique(grepl("Nationality", born1))) {
      born1 <- html_pl[born + 1]
      heig4 <- NA
    }else{
      # Height:
      heig1 <- html_pl[born + 1]
      heig2 <- gsub("<span>Height:", "", heig1)
      heig3 <- gsub("</span>", "", heig2)
      heig4 <- gsub(" ", "", heig3)  
    }
    
    born2 <- gsub("<span>Born:", "", born1)
    born3 <- gsub("</span>", "", born2)
    if (length(born3) > 1) {
      born3 <- born3[1]
      born4 <- gsub(",", "",gsub("                                     ", "", born3)) 
    }else{
      born4 <- gsub(",", "", born3)
    }
    born5 <- as.Date(born4, format = "%d %B %Y")
    born6 <- gsub("-", "/", born5)
    born7 <- strsplit(born6, "/")[[1]]
    born8 <- paste(born7[3], born7[2], born7[1], sep = "/")
    
    # Position:
    pos <- grep("summary-first", html_pl)
    pos1 <- html_pl[pos + 4]
    pos2 <- gsub("<span>", "", pos1)
    pos3 <- gsub("</span>", "", pos2)
    pos4 <- gsub(" ", "", pos3)    
    # Some players don't have the t-shirt number, e.g.
    # http://www.euroleague.net/competition/players/showplayer?pcode=ABN&seasoncode=E2000
    if (pos4 == "") {
      pos1 <- html_pl[pos + 3]
      pos2 <- gsub("<span>", "", pos1)
      pos3 <- gsub("</span>", "", pos2)
      pos4 <- gsub(" ", "", pos3) 
    }
    # There are many websites where the position is not available, e.g.,
    # http://www.euroleague.net/competition/players/showplayer?pcode=BXN&seasoncode=E2003
    if (!(pos4 %in% c("Guard", "Forward", "Center"))) {
      # "Guard", "Forward", "Center" are the three positions available from the euroleague.
      pos4 <- NA
    }  
    # There are some players who played the eurocup who don't have the position, but others do!
    # These are some scraping issues because of the websites structure.
    # Sergio Rodriguez:
    # http://www.eurocupbasketball.com/eurocup/competition/players/showplayer?pcode=CVM&seasoncode=U2005
    # http://www.euroleague.net/competition/players/showplayer?pcode=CVM&seasoncode=E2004
    # http://www.euroleague.net/competition/players/showplayer?pcode=CVM&seasoncode=E2017
    # Felipe Reyes:
    # http://www.eurocupbasketball.com/eurocup/competition/players/showplayer?pcode=AAX&seasoncode=U2002
    # http://www.euroleague.net/competition/players/showplayer?pcode=AAX&seasoncode=E2017
    
    # Nationality:
    nat <- grep("summary-second", html_pl)
    nat1 <- html_pl[nat + 3]
    # There are players with no height such as:
    # http://www.eurocupbasketball.com/eurocup/competition/players/showplayer?pcode=BMO
    if (nat1 == "                                    </div>") {
      nat1 <- html_pl[born + 2]
    }
    nat2 <- gsub("<span>Nationality:", "", nat1)
    nat3 <- gsub("</span>", "", nat2)
    nat4 <- trimws(nat3)
    
    df1 <- data.frame(pcode[i], name5, pos4, heig4, born8, nat4, website)
    if (verbose) {
      print(df1) 
    }
    df <- rbind.data.frame(df, df1)

    # Crawl-delay asks to pause between requests for 15 seconds.
    Sys.sleep(15)  
  } 
  if (!is.null(df)) {
    colnames(df) <- c("CombinID", "Player", "Position", "Height", 
                      "Date_birth", "Nationality", "Website_player")
  }  
  
  return(df)
}