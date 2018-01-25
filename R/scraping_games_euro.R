#' Euroleague and Eurocup player game finder data 
#' 
#' @aliases scraping_games_euro
#'
#' @description 
#' This function allows us to get all the player game finder data for 
#' all the desired Euroleague and Eurocup seasons available from
#' \url{http://www.euroleague.net/main/results/} and
#' \url{http://www.eurocupbasketball.com/eurocup/games/results}, respectively. 
#' 
#' @usage 
#' scraping_games_euro(competition, nums, year, verbose = TRUE,
#'                     r_user = "guillermo.vinue@uv.es")
#' 
#' @param competition String. Options are "Euroleague" and "Eurocup".
#' @param nums Numbers corresponding to the website from which scraping.
#' @param year Year when the season starts. 2017 refers to 2017-2018 and so on.
#' @param verbose Should R report information on progress? Default TRUE.
#' @param r_user Email to identify the user when doing web scraping. 
#' This is a polite way to do web scraping and to certify that the user 
#' is working as transparently as possible with a research purpose.
#' 
#' @details 
#' See the examples in \code{\link{get_games_rosters}} to see the game numbers
#' to scrape in each season.
#' 
#' 
#' @return 
#' A data frame with the player game finder data.
#' 
#' @note 
#' In addition to use the email address to stay identifiable, the function also 
#' contains two headers regarding the R platform and version used.
#' 
#' Furthermore, in the robots.txt file located at
#' \url{http://www.euroleague.net/robots.txt} and 
#' \url{https://www.eurocupbasketball.com/robots.txt}
#' there is the Crawl-delay field which asks crawlers to pause between 
#' requests for 15 seconds. This is done by adding to the function the command
#' \code{Sys.sleep(15)}.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_scraping_games}}
#' 
#' @examples 
#' \dontrun{
#' # Not needed to scrape every time the package is checked, built and installed.
#' # It takes 15 seconds as it is required in http://www.euroleague.net/robots.txt
#' df1 <- do_scraping_games(competition = "Euroleague", nums = 1:2, 
#'                          year = "2017", verbose = TRUE, r_user = 
#'                          "guillermo.vinue@uv.es")
#' }
#' 
#' @importFrom stringr word str_sub str_extract str_replace str_c
#' @importFrom httr GET user_agent add_headers
#' @importFrom xml2 read_html
#' @importFrom stringi stri_trans_general stri_extract_all_regex
#'
#' @export

scraping_games_euro <- function(competition, nums, year, verbose = TRUE,
                                r_user = "guillermo.vinue@uv.es"){
  #Auxiliar matrix to save the statistics in the same file.
  stats1 <- c()
  
  for (jorn in 1:length(nums)) {
    if (verbose) {
      print(jorn)
      print(nums[jorn]) 
    }  
    
    #To go through all the websites:  
    if (competition == "Euroleague") {
      website <- paste("http://www.euroleague.net/main/results/showgame?gamecode=", 
                       nums[jorn], "&seasoncode=E", year, sep = "") 
    }else if (competition == "Eurocup") {
      website <- paste("http://www.eurocupbasketball.com/eurocup/games/results/showgame?gamecode=", 
                       nums[jorn], "&seasoncode=U", year, sep = "")      
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
    if (get_website$status_code == 404 | get_website$status_code == 500) { 
      # The status code 404 is for the websites that cannot be found, i.e., the websites that don't exist.
      # The status code 500 is for the websites under maintenance, so the websites don't exist either.
      next
    }
    
    if (verbose) {
      print("Ready to scrape")  
    }
    
    # https://stat.ethz.ch/pipermail/r-help/2006-July/108654.html
    pl_page <- readLines(website, warn = FALSE)
        
    # We identify "PlayerContainer" because is the word which is only once 
    # for all the player's statistics. Then, from this line, we can get them.
    playCont <- grep("PlayerContainer", pl_page)
    numCols <- 38
      
    # Matrix with the data of each website:
    stats <- matrix(0, nrow = length(playCont), ncol = numCols)
    colnames(stats) <- c("Number", "GS", "Player", "MP", "PTS", "TwoP", "TwoPA", "TwoPPerc", "ThreeP", 
                         "ThreePA", "ThreePPerc", "FT", "FTA", "FTPerc", "TRB", "DRB", "ORB", "AST", 
                         "STL", "TOV", "Counteratt", "BLKfv", "BLKag", "Dunks", "PF", "PFrv", 
                         "PlusMinus", "PIR", "Season", "Type_season", "Day", "Date", "Game", "GameRes", 
                         "Team", "GameID", "Website", "CombinID") 
   
    # CombinID is the unique ID of the players and allows us to univocally identify each player.

    players <- list()
    equip <- c()
    for (i in seq_along(playCont)) {
      players[[i]] <- pl_page[c(playCont[i] - 1, playCont[i]:(playCont[i] + 16))] 
      # + 16 because there are 16 rows after PlayerContainer with values to fill.
      
      # CombinID
      pcode <- strsplit(strsplit(players[[i]][2], "pcode=")[[1]][2], "&seasoncode")[[1]][1]
      stats[i, numCols] <- pcode
      
      # To put the player's name (and for "Equipo"):
      aux_n <- strsplit(players[[i]][2], ">")[[1]][3]
      aux_n_1 <- unlist(strsplit(aux_n, "</a"))
      
      if (aux_n_1 == "Team") {
        stats[i,3] <- "Team"
        # This is to know the number of players of each team to put in the corresponding rows their team name.
        equip[i] <- i # If the non-NA values are 13 and 26, this means that there are 12 players in each team
      }else{   
        # https://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
        stats[i,3] <- tools::toTitleCase(tolower(aux_n_1))
      }
      
      # This is to put the number of each player's T-Shirt. 
      # For "Equipo" we put a 0.
      aux1 <- gsub("\t<td>", "", players[[i]][1])
      aux2 <- gsub("</td>", "", aux1)
      if (aux2 == "") {
        stats[i,1] <- 0 # For the row Team.  
      }else{
        stats[i,1] <- as.numeric(aux2)
      } 
      
      # This is to say if the player started the game or not.
      if (grepl("PlayerStartFive", players[[i]][2])) {
        stats[i,2] <- 1 # started the game.
      }else{
        stats[i,2] <- 0 # Didn't start the game. 
      }
      
      # Minutes played:
      aux7 <- gsub("\t<td>", "", players[[i]][3])
      aux8 <- gsub("</td>", "", aux7)
      if (aux8 == "DNP") {
        next
      }else if (aux8 == "&nbsp;") {
        stats[i,4] <- 0 # For the row Team.  
      }else{  
        stats[i,4] <- aux8
      }
      # Points:
      aux9 <- gsub("\t<td>", "", players[[i]][4])
      aux10 <- gsub("</td>", "", aux9)
      if (aux10 == "&nbsp;") { 
        stats[i,5] <- 0
      }else{
        stats[i,5] <- aux10 
      }  
      
      
      for (j in c(5,6,7)) { # This is to divide the 2 and 3-field 
        # shots scored/ attempted and free throws.
        # 5,6,7 are the columns that correspond with '/'.  
        if (j == 5) {
          index1 <- j + 1
          index2 <- j + 2
          index3 <- j + 3
        }else if (j == 6) {
          index1 <- j + 3
          index2 <- j + 4
          index3 <- j + 5
        }else if (j == 7) {
          index1 <- j + 5
          index2 <- j + 6
          index3 <- j + 7
        } 
        
        aux11 <- gsub("\t<td>", "", players[[i]][j])
        aux12 <- gsub("</td>", "", aux11)
        if (aux12 == "&nbsp;") { # This means that the player didn't shot from this distance, 
          # so their values remain zero.
          stats[i,index1] <- 0
          stats[i,index2] <- 0
          stats[i,index3] <- 0 # percentage 
        }else{
          aux13 <- as.numeric(strsplit(aux12, "/")[[1]])  
          stats[i,index1] <- aux13[1]
          stats[i,index2] <- aux13[2]
          stats[i,index3] <- round(aux13[1] / aux13[2], 2) # percentage   
        } 

      } # End of loop j in c(5,6,7)
      
      # This is for the rest of statistics:
      rest <- 8:18
      indexes <- c(17, 16, 15, 18:20, 22, 23, 25, 26, 28)
      for (j in 1:length(rest)) {
        aux14 <- gsub("\t<td>", "", players[[i]][rest[j]])
        aux15 <- gsub("</td>", "", aux14)
        if (aux15 == "&nbsp;") {
         stats[i, indexes[j]] <- 0
        }else{
          stats[i, indexes[j]] <- as.numeric(aux15)      
         }  
      }
      
    } # End of loop i.
    
    # For "Season", "Type_season", "Day", "Date", "Game", "GameRes", "Tem", "GameID":
    # Season, Type_season and Day:
    ref <- grep("gc-title", pl_page)
    ref1 <- pl_page[ref + 1]
    ref2 <- strsplit(ref1, "span")[[1]]
    stats[,numCols - 9] <- gsub("</", "", gsub(">", "", ref2[2])) # Season.
    stats[,numCols - 8] <- gsub("</", "", gsub(">", "", ref2[4])) # Type_season.
    stats[,numCols - 7] <- gsub("</", "", gsub(">", "", ref2[6])) # Day.
    
    
    # Date: All the following is needed to put the date in the same format as ACB tables.
    date_ref <- grep("date", pl_page)
    date_ref1 <- pl_page[date_ref[2]]
    
    date_ref2 <- gsub("      <div class=\"date cet\">|      </div>", "", date_ref1)
    date_ref3 <- strsplit(date_ref2, "CET")[[1]][1]
    date_ref4 <- gsub("  ", "", date_ref3)
    date_ref5 <- gsub(",", "", date_ref4)
    # https://www.r-bloggers.com/date-formats-in-r/
    date_ref6 <- as.Date(date_ref5, format = "%B %d %Y")
    date_ref7 <- gsub("-", "/", date_ref6)
    date_ref8 <- strsplit(date_ref7, "/")[[1]]
    date_ref9 <- paste(date_ref8[3], date_ref8[2], date_ref8[1], sep = "/")
    
    stats[,numCols - 6] <- date_ref9  # Date.

    
    # Game,  GameRes, Team,  GameID: 
    # Game:
    game_ref <- grep("game-score", pl_page)
    game_ref1_local <- pl_page[game_ref + 5]
    game_ref2_local <- strsplit(game_ref1_local, "<span class=\"name\">")[[1]]
    
    game_ref1_road <- pl_page[game_ref + 12]
    game_ref2_road <- strsplit(game_ref1_road, "<span class=\"name\">")[[1]]
    
    game_local <- tolower(gsub("</", "", gsub("</span>", "", game_ref2_local[2]))) # Local team of the game.
    game_road <- tolower(gsub("</", "", gsub("</span>", "", game_ref2_road[2]))) # Road team of the game
    stats[,numCols - 5] <- paste(game_local, game_road, sep = "-") # Game
    
    # GameRes:
    res_ref1_local <- pl_page[game_ref + 7]
    res_ref2_local <- strsplit(res_ref1_local, "<span class=\"score\">")[[1]]
    
    res_ref1_road <- pl_page[game_ref + 14]
    res_ref2_road <- strsplit(res_ref1_road, "<span class=\"score\">")[[1]]
    
    res_local <- gsub("</", "", gsub("</span>", "", res_ref2_local[2]))
    res_road <- gsub("</", "", gsub("</span>", "", res_ref2_road[2]))
    stats[,numCols - 4] <- paste(res_local, res_road, sep = "-") # GameRes
    
    # Team: To put the teams' names:
    equip1 <- which(!is.na(equip))
    if (length(equip1) > 1) {
      stats[1:(equip1[1] - 1), numCols - 3] <- game_local
      stats[(equip1[1] + 1):equip1[2], numCols - 3] <- game_road 
      # It can happen like in http://www.euroleague.net/main/results/showgame?gamecode=14&seasoncode=E2006
      # that there is no row Team for one team:
    }else{
      stats[1:(equip1[1] - 1), numCols - 3] <- game_local
      stats[(equip1[1] + 1):nrow(stats), numCols - 3] <- game_road 
    }
    
    
    # GameID:
    stats[,numCols - 2] <- rep(jorn, length(playCont)) 
    stats[,numCols - 1] <- website 
    
    stats1 <- rbind(stats1, stats)
    
    if (verbose) {
      print("Done")  
    }
  
    # http://www.euroleague.net/robots.txt  
    # https://www.eurocupbasketball.com/robots.txt
    # Crawl-delay asks to pause between requests for 15 seconds.
    Sys.sleep(15)  
  } # End loop jorn.

  #stats1 <- as.data.frame(stats1)
  #stats1$GS <- as.numeric(stats1$GS)
  return(stats1)
}