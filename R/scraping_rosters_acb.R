#' ACB players' profile
#'
#' @aliases
#' scraping_rosters_acb
#' 
#' @description
#' This function allows us to obtain the basic information of each player, 
#' including his birth date. Then, we will be able to compute the age that 
#' each player had in the date that he played each game. 
#' The website used to collect information is \url{www.acb.com}.
#' 
#' @usage
#' scraping_rosters_acb(pcode, verbose = TRUE, accents = FALSE, 
#'                      r_user = "guillermo.vinue@uv.es")
#'                     
#' @param pcode Code corresponding to the player's website to scrape.
#' @param verbose Should R report information on progress? Default TRUE.
#' @param accents Should we keep the Spanish accents? The recommended 
#' option is to remove them, so default FALSE.
#' @param r_user Email user to identify the user when doing web scraping. 
#' This is a polite way to do web scraping and to certify that the user 
#' is working as transparently as possible with a research purpose.
#'
#' @return
#' Data frame with eight columns: 
#' \itemize{
#' \item CombinID: Unique ID to identify the players.
#' \item Player: Player's name.
#' \item Position: Player's position on the court.
#' \item Height: Player's height.
#' \item Date_birth: Player's birth date.
#' \item Nationality: Player's nationality.
#' \item Licence: Player's licence.
#' \item Website_player: Website.
#' }
#' 
#' @details 
#' Some players have a particular licence, which does not necessarily match with their
#' nationality, in order not to be considered as a foreign player, according to the 
#' current ACB rules.
#' 
#' @note 
#' In addition to use the email address to stay identifiable, the function also 
#' contains two headers regarding the R platform and version used.
#' 
#' Furthermore, even though in the robots.txt file at 
#' \url{http://www.acb.com/robots.txt}, there is no information about scraping
#' limitations and all robots are allowed to have complete access,
#' the function also includes the command \code{Sys.sleep(2)}
#' to pause between requests for 2 seconds. In this way, we don't bother the server 
#' with multiple requests and we do carry out a friendly scraping.
#' 
#' @author
#' Guillermo Vinue
#'
#' @seealso 
#' \code{\link{do_scraping_rosters}}
#'
#' @examples
#' \dontrun{
#' # Not needed to scrape every time the package is checked, built and installed.
#' df_bio <- scraping_rosters_acb("56C", verbose = TRUE, accents = FALSE, 
#'                                r_user = "guillermo.vinue@uv.es")
#' }
#' 
#' @importFrom stringr str_extract str_replace str_c
#' @importFrom httr GET user_agent add_headers
#' @importFrom xml2 read_html
#' @importFrom rvest html_table 
#' @importFrom plyr .
#'
#' @export

scraping_rosters_acb <- function(pcode, verbose = TRUE, 
                                 accents = FALSE, r_user = "guillermo.vinue@uv.es"){
  df <- NULL
  #for (i in 1:length(pcode)) 
  len_pcode <- length(pcode)
  # Instead of using 1:len_pcode, we can use seq_len(len_pcode) to avoid the backwards sequence bug.
  for (i in seq_len(len_pcode)) {
    if (verbose) {
      print(pcode[i])
    } 
    
    website <- paste("http://www.acb.com/jugador.php?id=", pcode[i], sep = "")
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
    
    # Date of birth:
    born <- grep("fecha de nac", html_pl)
    if (length(born) == 0) { # This means that web doesn't exist.
      print("Web doesn't exist")
      next 
    }
    born1 <- html_pl[born + 1]
    born2 <- gsub("            <td class=\"datojug\">", "", born1)
    born3 <- sub(".*,", "", born2)
    born4 <- substr(gsub(" ", "", born3), 1, 10)
    
    # Position:
    posit <- grep("posic", html_pl)
    posit1 <- html_pl[posit + 1]
    posit2 <- gsub("            <td class=\"datojug\">", "", posit1)
    if (length(posit2) == 1) {
      posit3 <- gsub(" ", "", substr(posit2, 1, 2))[1]   
    }else if (length(posit2) == 2) { # This is because for some players, there are
      # two elements in posit2, but in some cases the player's position is in 
      # the second element and in other cases it is in the first element.
      posit3 <- gsub(" ", "", substr(posit2[2], 1, 2))[1]
      if (posit3 == "" | posit3 == "19") { # See for example Bodiroga AYG for "" and 
        # Keith Robinson AS4 for 19.
        posit3 <- gsub(" ", "", substr(posit2[1], 1, 2))[1] 
      }
    }
    
    # Name:
    name <- grep('"portadader"', html_pl)
    name1 <- html_pl[name + 1]
    name2 <- gsub("            <div id=\"portadadertop\">", "", name1)
    name3 <- gsub("</div><br>", "", name2)
    
    # Right Spanish accents in R:
    # Scrape lookup table of accented char html codes, from the 4th table on this page:
    ref_url <- 'http://www.w3schools.com/charsets/ref_html_8859.asp'
    #html is deprecated. 
    char_table <- read_html(ref_url) %>% html_table %>% `[[`(4) 
    # 4 means that the table of interest in this website is the fourth.
    # Fix names:
    names(char_table) <- names(char_table) %>% tolower %>% gsub(' ', '_', .)
    # Names with the rights accents
    #name3 <- mgsub(char_table$entity_name, char_table$character, name3) 
    # WARNING: SEE THIS BEHAVIOUR BOTH IN WINDOWS AND LINUX:
    #stats1[,3] <- gsub("", "u", stats1[,3]) # This is because the accented u is 
    # not replaced rightly with mgsub.
    
    enti <- str_extract(pattern = char_table$entity_name, name3)
    repl <- char_table$character[!is.na(enti)]
    if (length(repl) != 0) {
      if (length(repl) > 1) { # The player's name may contain several special accents, such as Alex Mumbru.
        aux_pl <- name3
        aux1_pl <- c()
        for (re in 1:length(repl)) {
          aux1_pl <- str_replace(aux_pl, enti[!is.na(enti)][re], repl[re]) 
          aux_pl <- aux1_pl
        }
        rm(aux_pl)
        name3 <- aux1_pl
      }else{
        name3 <- str_replace(name3, enti[!is.na(enti)], repl) 
      }
      
      enti <- str_extract(pattern = char_table$entity_name, name3)
      repl <- char_table$character[!is.na(enti)]
      if (length(repl) != 0) { # It can happen that the name and the surname has the same special character,
        # such as Andres Jimenez, so we have to repeat the procedure.
        name3 <- str_replace(name3, enti[!is.na(enti)], repl) 
      }  
    }
    
    if (!accents) {
      name3 <- stri_trans_general(name3, "Latin-ASCII")
    }
    
    # Nationality and licence:
    nac_lic <- grep("nacionalidad | licencia", html_pl)
    nac_lic1 <- html_pl[nac_lic + 1]
    nac_lic2 <- gsub("            <td class=\"datojug\">", "", nac_lic1)
    nac_lic3 <- sub("</td>", "", nac_lic2)
    aux_nac_lic <- strsplit(nac_lic3, " ")[[1]]
    nac <- aux_nac_lic[1]
    if (length(aux_nac_lic) == 3) {
      lic <- aux_nac_lic[3]
    }else{
      lic <- "NA" 
    }
    
    # Height:
    heig <- gsub(" ", "", substr(posit2, 4, 8))[1]
    
    df1 <- data.frame(pcode[i], name3, posit3, heig, born4, nac, lic, website)
    df <- rbind.data.frame(df, df1)
    
    Sys.sleep(2)  
  }
  if (!is.null(df)) {
    colnames(df) <- c("CombinID", "Player", "Position", "Height", 
                      "Date_birth", "Nationality", "Licence", "Website_player")
  }  
  
  return(df)
}  