#' ACB player game finder data 
#' 
#' @aliases scraping_games_acb
#'
#' @description 
#' This function allows us to get all the player game finder data for 
#' all the desired ACB seasons available from: 
#' \url{http://www.acb.com}. 
#' 
#' @usage 
#' scraping_games_acb(type_league, nums, year, verbose = TRUE, 
#'                    accents = FALSE, r_user = "guillermo.vinue@uv.es")
#' 
#' @param type_league String. If \code{competition} is ACB, to scrape 
#' ACB league games ("ACB"), Copa del Rey games ("CREY") or Supercopa games ("SCOPA").
#' @param nums Numbers corresponding to the website to scrape.
#' @param year Season, e.g. 2017-2018.
#' @param verbose Should R report information on progress? Default TRUE.
#' @param accents Should we keep the Spanish accents? The recommended 
#' option is to remove them, so default FALSE.
#' @param r_user Email to identify the user when doing web scraping. 
#' This is a polite way to do web scraping and to certify that the user 
#' is working as transparently as possible with a research purpose.
#' 
#' @details 
#' The official website of the Spanish basketball league ACB presents the
#' statistics of each game in a php website, such as:
#' \url{http://www.acb.com/fichas/LACB62090.php}.
#' 
#' Note that for example http://www.acb.com/fichas/LACB60315.php
#' doesn't exist, so for these cases is where we can use the 
#'  \code{httr} package.
#' 
#' In \url{www.uv.es/vivigui/docs/acb_scraping.pdf} a document 
#' is available with the exact numbers xxxxx related to
#' http://www.acb.com/fichas/LACBxxxxx.php for some seasons.
#' 
#' @return 
#' A data frame with the player game finder data.
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
#' \code{\link{do_scraping_games}}
#' 
#' @examples 
#' \dontrun{
#' # Not needed to scrape every time the package is checked, built and installed.
#' df1 <- scraping_games_acb(type_league = "ACB", nums = 62001:62002, year = "2017-2018",
#'                           verbose = TRUE, accents = FALSE, 
#'                           r_user = "guillermo.vinue@uv.es")
#' }                           
#' 
#' @importFrom stringr word str_sub str_extract str_replace str_c
#' @importFrom httr GET user_agent add_headers
#' @importFrom xml2 read_html
#' @importFrom rvest html_table 
#' @importFrom plyr .
#' @importFrom stringi stri_trans_general stri_extract_all_regex
#'
#' @export

scraping_games_acb <- function(type_league, nums, year, verbose = TRUE, accents = FALSE, 
                               r_user = "guillermo.vinue@uv.es"){
  #Auxiliar matrix to save the statistics in the same file.
  #stats1 <- matrix(0, nrow = 1, ncol = 34)
  stats1 <- c()
  
  for (jorn in 1:length(nums)) {
    if (verbose) {
      print(paste("Day", jorn))
    }  
    #To go through all the websites:  
    if (type_league == "CREY") {
      website <- paste("http://www.acb.com/fichas/CREY", nums[jorn], ".php", sep = "")
    }else if (type_league == "SCOPA"){
      website <- paste("http://www.acb.com/fichas/SCOPA", nums[jorn], ".php", sep = "")
    }else if (type_league == "ACB"){
      website <- paste("http://www.acb.com/fichas/LACB", nums[jorn], ".php", sep = "")
    }else{
      print("Valid options are ACB, CREY or SCOPA")
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
    
    #pl_page <- readLines(website, encoding = "utf8")
    pl_page <- readLines(website)
        
    # We identify 'naranjaclaro' because is the color which is only once 
    # for all the player's statistics. Then, from this line, we can get them.
    orange <- grep('"naranjaclaro"', pl_page)
    
    if (type_league == "CREY") {
      # These are the seasons where there is not column Plus/Minus, so they must have 35 columns:
      if (nums[jorn] %in% c(50001:50004, 51001:51007, 52001:52007, 53033:53039, 
                            54033:54039, 55033:55040, 56033:56040, 57029:57036, 
                            58025:58032, 59038:59045, 60001:60008, 61001:61007, 
                            62001:62007, 63001:63007, 64001:64007, 65001:65007, 
                            66001:66007, 67001:67007, 68001:68007, 69001:69007, 
                            70001:70007, 71001:71007, 72001:72007, 73001:73007, 
                            74001:74007, 75001:75007)) {
        numCols <- 35
      }else{
        numCols <- 36
      }
    }else if (type_league == "SCOPA"){
      # These are the seasons where there is not column Plus/Minus, so they must have 35 columns::
      if (nums[jorn] %in% c(3001, 4001, 5001:5004, 6001:6004, 7001:7003, 
                            9001:9003, 10001:10003, 11001:11003, 12001, 12003)) {
        # http://www.acb.com/fichas/SCOPA12002.php # There is Plus/Minus.
        # http://www.acb.com/fichas/SCOPA12003.php # There is not Plus/Minus.
        numCols <- 35
      }else{
        numCols <- 36
      }
    }else{
      # These are the seasons where there is not column Plus/Minus:
      # Warning: in the season 1999-2000, there is only column Plus/Minus in the regular season, 
      # but not in the playoffs, thus added 44307:44341.
      if (nums[jorn] %in% c(35001:35494, 36001:36498, 37001:37401, 38001:38347, 39001:39417,
                            40001:40415, 41001:41351, 42001:42350, 43001:43339, 44307:44341,
                            45001:45339, 46001:46339, 47001:47339, 48001:48341, 49001:49341, 
                            50001:50339, 51001:51340, 52001:52327, 53001:53294, 54001:54331, 
                            55001:55331)) {
        numCols <- 35
      }else{
        numCols <- 36
      }
    }  
    # Matrix with the data of each website:
    stats <- matrix(0, nrow = length(orange), ncol = numCols)
    if (numCols == 35) {
      colnames(stats) <- c("Number", "GS", "Player", "MP", "PTS", "TwoP", "TwoPA", "TwoPPerc", "ThreeP", 
                           "ThreePA", "ThreePPerc", "FT", "FTA", "FTPerc", "TRB", "DRB", "ORB", "AST", 
                           "STL", "TOV", "Counteratt", "BLKfv", "BLKag", "Dunks", "PF", "PFrv", 
                           "PIR", "Day", "Date", "Game", "GameRes", "Team", "GameID", "Website", 
                           "CombinID")
      last_cols <- 23
    }else{
      colnames(stats) <- c("Number", "GS", "Player", "MP", "PTS", "TwoP", "TwoPA", "TwoPPerc", "ThreeP", 
                           "ThreePA", "ThreePPerc", "FT", "FTA", "FTPerc", "TRB", "DRB", "ORB", "AST", 
                           "STL", "TOV", "Counteratt", "BLKfv", "BLKag", "Dunks", "PF", "PFrv", 
                           "PlusMinus", "PIR", "Day", "Date", "Game", "GameRes", 
                           "Team", "GameID", "Website", "CombinID") 
      last_cols <- 24
    }
    # BLKfv are blocks in favor ; BLKag are blocks against.
    # PF are personal fouls commited ; PFrv are personal fouls received.
    # PIR is Performace Index Rating.
    # CombinID is the unique ID of the players and allows us to univocally identify each player.
    # This is especially very useful to distinguish the players with the same name, see
    # scraping_acb_rosters_from_acb.R
    
    equip <- -1 # This is an auxiliary value to put the corresponding row 
    # of "Equipo" in the right place. I have to do this because I cannot be
    # sure that "Equipo" goes in the rows 13 and 26 (that would happen if each
    # team played with 12 players, but this doesn't always happen. It may 
    # happen that a team has only 10 ready players for a game). 
    players <- list()
    for (i in seq_along(orange)) {
      players[[i]] <- pl_page[c(orange[i] - 1, orange[i]:(orange[i] + 21))] # + 21 because there are 21 rows
      # after naranjaclaro with values to fill.
      
      # To put the player's name (and for "Equipo"):
      aux5 <- strsplit(players[[i]][2], ">")[[1]][3]
      aux5_1 <- unlist(strsplit(aux5, "</a"))
      
      if (is.na(aux5_1)) {
        stats[i,3] <- "Equipo"
        if (i != length(orange)) {
          equip <- i
        }  
      }else{   
        stats[i,3] <- aux5_1
      }
      
      # This is for the players who didn't play and all 
      # their statistics are empty:
      auxNA <- gsub("      <td class=\"grisclaro\" width=\"|      <td class=\"blanco\" width=\"|</td>", "", players[[i]][4])
      auxNA1 <- gsub("\">", "", auxNA)
      auxNA2 <- str_sub(auxNA1, 3)
      if (auxNA2 == "&nbsp;") {
        auxNA3 <- strsplit(players[[i]][2], ">")[[1]][3]
        stats[i,3] <- unlist(strsplit(auxNA3, "</a")) # Playe's name who didn't play.
        stats[i,seq(1,length(orange))[-3]] <- 0 # We put a 0 for all those empty statistics.
        next
      } 
      
      # This is to put the number of each player's T-Shirt. 
      # For "Equipo" we put a 0.
      aux1 <- strsplit(players[[i]][1], "<td class=\"")
      aux2 <- word(aux1[[1]], 2)
      aux3 <- gsub("width=\"|\">|</td>", "", aux2[2])
      if (i == equip || i == length(orange)) {
        stats[i,1] <- 0  
      }else{
        stats[i,1] <- as.numeric(str_sub(aux3, 3))
      } 
      
      # This is to say if the player started the game or not.
      aux4 <- word(aux1[[1]], 1)
      if (strsplit(aux4, "\"")[[2]] == "gristit") {
        stats[i,2] <- 1 # started the game.
      }else{
        stats[i,2] <- 0 # Didn't start the game. 
      }
      
      for (j in c(5,7,9)) { # This is to divide the 2 and 3-field 
        # shots scored/ attempted and free throws.
        # 5,7,9 are the columns that correspond with '/'.  
        if (j == 5) {
          index1 <- j + 1
          index2 <- j + 2
          index3 <- j + 3
        }else if (j == 7) {
          index1 <- j + 2
          index2 <- j + 3
          index3 <- j + 4
        }else if (j == 9) {
          index1 <- j + 3
          index2 <- j + 4
          index3 <- j + 5
        } 
        
        aux7 <- gsub("      <td class=\"grisclaro\" width=\"|      <td class=\"blanco\" width=\"|</td>", "", players[[i]][j])
        aux8 <- gsub("\">", "", aux7)
        aux9 <- str_sub(aux8, 3)
        aux10 <- as.numeric(strsplit(aux9, "/")[[1]])  
        stats[i,index1] <- aux10[1]
        stats[i,index2] <- aux10[2]
        
        aux11 <- gsub("      <td class=\"grisclaro\" width=\"|      <td class=\"blanco\" width=\"|</td>", "", players[[i]][j+1])
        aux12 <- gsub("\">", "", aux11)
        aux13 <- str_sub(aux12, 3)
        aux14 <- as.numeric(strsplit(aux13, "%")[[1]])
        stats[i,index3] <- aux14
      } # End of loop j in c(5,7,9)
      
      
      # This is for the total rebounds:
      aux15 <- gsub("      <td class=\"grisclaro\" width=\"|      <td class=\"blanco\" width=\"|</td>", "", players[[i]][11])
      aux16 <- gsub("\">", "", aux15)
      aux17 <- str_sub(aux16, 3)
      stats[i,15] <- as.numeric(aux17)
      
      # This is to divide the offensive and defensive rebounds.
      aux18 <- gsub("      <td class=\"grisclaro\" width=\"|      <td class=\"blanco\" width=\"|</td>", "", players[[i]][12])
      aux19 <- gsub("\">", "", aux18)
      aux20 <- str_sub(aux19, 3)
      aux21 <- strsplit(aux20, "+")[[1]]
      
      if (length(aux21) == 4) { # This is needed if the number of offensive or defensive rebounds (and on consequence the 
                                # total rebounds) is 10 or more than 10.
        nums_rebs <-  as.numeric(stri_extract_all_regex(aux20, "[0-9]+")[[1]])
        if (nums_rebs[1] > nums_rebs[2]) { # For example 10+2
          stats[i,16] <- as.numeric(paste(aux21[1], aux21[2], sep = "")) # 10 or more defensive rebounds.
          stats[i,17] <- as.numeric(aux21[4])                            # Less than 10 offensive rebounds.
        }else{# For example 4+13, see Hopkins in 45274.php
          stats[i,16] <- as.numeric(aux21[1])                            # Less than 10 defensive rebounds.
          stats[i,17] <- as.numeric(paste(aux21[3], aux21[4], sep = "")) # 10 or more offensive rebounds.
        }
      }else if (length(aux21) == 5) { # It can be the case, see 30031.php player King, Winfred where 
                                    # we have 10+10 rebounds, so aux21 is  "1" "0" "+" "1" "0"
                                    # so the length in this case is 5, not 4.
        stats[i,16] <- as.numeric(paste(aux21[1], aux21[2], sep = "")) # 10 or more defensive rebounds.
        stats[i,17] <- as.numeric(paste(aux21[4], aux21[5], sep = "")) # 10 or more offensive rebounds. 
      }else{# Less than 10 total rebounds (for example 4+3)
        stats[i,16] <- as.numeric(aux21[1])
        stats[i,17] <- as.numeric(aux21[3]) 
      }  
      
      # This is to remove the two values that appear before 
      # the minutes and points of each player.
      for (k in c(4:5)) {
        aux22 <- gsub("      <td class=\"grisclaro\" width=\"|      <td class=\"blanco\" width=\"|</td>", "", players[[i]][k - 1])
        aux23 <- gsub("\">", "", aux22)
        if (k == 4 & (i == equip || i == length(orange))) {
          stats[i,k] <- 0  # For "Equipo".
        }else{
          stats[i,k] <- substring(aux23, 3, nchar(aux23))
        }  
      }
      
      # For the statistics from assists until rating (PIR).
      for (l in c(14:last_cols)) {
        aux24 <- gsub("      <td class=\"grisclaro\" width=\"|      <td class=\"blanco\" width=\"|</td>", "", players[[i]][l - 1])
        aux25 <- gsub("\">", "", aux24)
        stats[i,l + 4] <- substring(aux25, 3, nchar(aux25))
      }
      
      aux_comb <- strsplit(players[[i]][2], ">")[[1]][2]
      aux_comb1 <- strsplit(aux_comb , "id=")[[1]][2]
      aux_comb2 <- gsub("\"", "", aux_comb1) 
      stats[i,numCols] <- aux_comb2
      
     } # End of loop i.
    
    # For "Day", "Date", "Game", "GameRes", "Tem", "GameID"
    date_ref <- grep('class="estnegro"', pl_page)
    
    gv <- gsub("      <td width=\"|      |</td>", "", pl_page[date_ref + 1])
    stats[,numCols - 7] <- as.numeric(strsplit(gv, " ")[[1]][3])   # Day.
    stats[,numCols - 6] <- as.character(strsplit(gv, " ")[[1]][5]) # Date.
    
    date_ref <- grep('class="estverdel"', pl_page)
    
    gv1 <- gsub("<td colspan=\"10\" class=\"estverdel\">" , "", pl_page[date_ref])
    gv2 <- strsplit(gv1, "</td>")
    gv3 <- gsub("   ", "", gv2[[1]])
    len_gv3 <- sapply(strsplit(gv3, " "), length)
    gv4 <- word(gv3, 1, len_gv3 - 1)
    gv5 <- gsub("   ", "", gv2[[2]])
    len_gv5 <- sapply(strsplit(gv5, " "), length)
    gv6 <- word(gv5, 1, len_gv5 - 1)
    stats[,numCols - 5] <- paste(gv4, gv6, sep = " - ") # Game.
    stats[,numCols - 5] <- tolower(stats[,numCols - 5])
    
    gv7 <- word(gv3, -1)
    gv8 <- word(gv5, -1)
    stats[,numCols - 4] <- paste(gv7, gv8, sep = " - ") # GameRes.
    
    # To put the teams' names:
    stats[1:(equip - 1),numCols - 3] <- gv4
    stats[(equip + 1):(length(orange) - 1),numCols - 3] <- gv6
    stats[,numCols - 3] <- tolower(stats[,numCols - 3])
    
    stats[,numCols - 2] <- rep(jorn, length(orange)) # GameID
    stats[,numCols - 1] <- website 
    
    # Right Spanish accents in R:
    # Scrape lookup table of accented char html codes, 
    # from the 4th table on this page:
    ref_url <- 'http://www.w3schools.com/charsets/ref_html_8859.asp'
    #html is deprecated. 
    char_table <- read_html(ref_url) %>% html_table %>% `[[`(4) 
    # 4 means that the table of interest in this website is the fourth.
    # Fix names:
    names(char_table) <- names(char_table) %>% tolower %>% gsub(' ', '_', .)
    # Names with the rights accents
    #stats1[,3] <- mgsub(char_table$entity_name, char_table$character, stats1[,3]) 
    # WARNING: SEE THIS BEHAVIOUR BOTH IN WINDOWS AND LINUX:
    ##stats1[,3] <- gsub("", "u", stats1[,3]) # This is because the accented u is 
    # not replaced rightly with mgsub.
    #stats1[,31] <- mgsub(char_table$entity_name, char_table$character, stats1[,31]) 
    #stats1[,33] <- mgsub(char_table$entity_name, char_table$character, stats1[,33]) 
    
    for (row in 1:nrow(stats)) {
      # Players' names:
      enti <- str_extract(pattern = char_table$entity_name, stats[row,3])
      repl <- char_table$character[!is.na(enti)]
      if (length(repl) != 0) {
        if (length(repl) > 1) { # The player's name may contain several special accents, such as Mumbru, Alex.
          aux_pl <- stats[row,3]
          aux1_pl <- c()
          for (re in 1:length(repl)) {
            aux1_pl <- str_replace(aux_pl, enti[!is.na(enti)][re], repl[re]) 
            aux_pl <- aux1_pl
          }
          rm(aux_pl)
          stats[row,3] <- aux1_pl
         }else{
           #stats[row,3] <- str_replace(stats[row,3], enti[!is.na(enti)], repl) # The problem 
           # with this sentence is that if there is the same accent in both the name and surname, 
           # for example, Nogues, Jose I. (accented e in Nogues and Jose) in 
           # http://www.acb.com/fichas/LACB60017.php, only the first accent is rightly replaced, 
           # so we have to use:
           stats[row,3] <- as.character(gsub(enti[!is.na(enti)], repl, stats[row,3]))
           
        }
      }
      # Teams that played the game:
      enti <- str_extract(pattern = char_table$entity_name, stats[row,numCols - 5])
      repl <- char_table$character[!is.na(enti)]
      if (length(repl) != 0) {
        if (length(repl) > 1) { 
          aux_pl <- stats[row,numCols - 5]
          aux1_pl <- c()
          for (re in 1:length(repl)) {
            aux1_pl <- str_replace(aux_pl, enti[!is.na(enti)][re], repl[re]) 
            aux_pl <- aux1_pl
          }
          rm(aux_pl)
          stats[row,numCols - 5] <- aux1_pl
        }else{
          stats[row,numCols - 5] <- str_replace(stats[row,numCols - 5], enti[!is.na(enti)], repl) 
        }
      }
      
      # Teams that played the game separately:
      enti <- str_extract(pattern = char_table$entity_name, stats[row,numCols - 3])
      repl <- char_table$character[!is.na(enti)]
      if (length(repl) != 0) {
        if (length(repl) > 1) { 
          aux_pl <- stats[row,numCols - 3]
          aux1_pl <- c()
          for (re in 1:length(repl)) {
            aux1_pl <- str_replace(aux_pl, enti[!is.na(enti)][re], repl[re]) 
            aux_pl <- aux1_pl
          }
          rm(aux_pl)
          stats[row,numCols - 3] <- aux1_pl
        }else{
          stats[row,numCols - 3] <- str_replace(stats[row,numCols - 3], enti[!is.na(enti)], repl) 
        }
      }
    } # End of loop row.  
              
    if (!accents) {
      stats[,3] <- stri_trans_general(stats[,3], "Latin-ASCII")
      stats[,numCols - 5] <- stri_trans_general(stats[,numCols - 5], "Latin-ASCII")
      stats[,numCols - 3] <- stri_trans_general(stats[,numCols - 3], "Latin-ASCII")
    }    
  
    if (numCols == 35) { # This is because in order to merge all the data frames for all the seasons, 
      # the data frames must have the same number of columns, so I have to add the Plus/Minus column:
      stats <- cbind(stats, PlusMinus = NA) # Add in the new column.
      stats <- stats[,c(1:26, 36, 27:35)] # Reorder columns.
    }
      
    if (verbose) {
      print("Done")  
    }
    
    if (type_league == "CREY") {
      type_season <- "Copa del Rey"
    }else if (type_league == "SCOPA"){
      type_season <- "Supercopa"
    }else{
      if (nums[jorn] %in% c(30225:30257, 31225:31262, 32225:32264, 33433:33492, 34433:34487, 
                            35409:35494, 36409:36498, 37342:37401, 38281:38347, 39381:39417,
                            40381:40415, 41307:41351, 42307:42350, 43307:43339, 44307:44341, 
                            45307:45339, 46307:46339, 47307:47339, 48307:48341, 49307:49341, 
                            50307:50339, 51307:51340, 52307:52337, 53273:53294, 54307:54331,
                            55307:55331, 56307:56333, 57307:57333, 58307:58333, 59307:59331,
                            60307:60332, 61273:61298, 62307:62332)) {
        type_season <- "Playoffs"
      }else{
        type_season <- "Regular Season"
      }      
    }  

    stats <- cbind(stats[, 1:28], Type_season = type_season, stats[, 29:36]) # Add the season.
    
    stats1 <- rbind(stats1, stats)
    
    Sys.sleep(2)  
  } # End loop jorn.
  
  #stats1 <- stats1[-1,]
  stats1 <- cbind(stats1[, 1:28], Season = year, stats1[, 29:37]) # Add the season.
  
  #stats1 <- as.data.frame(stats1)
  return(stats1)
}