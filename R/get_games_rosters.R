#' Get all games and rosters
#' 
#' @aliases get_games_rosters
#'
#' @description 
#' This function is to get all the games and rosters of the 
#' competition selected. 
#' 
#' @usage get_games_rosters(competition, type_league, nums, verbose = TRUE, 
#'                          accents = FALSE, r_user, df0, df_bio0)
#' 
#' @param competition String. Options are "ACB", "Euroleague" and "Eurocup".
#' @param type_league String. If \code{competition} is ACB, to scrape 
#' ACB league games ("ACB"), Copa del Rey games ("CREY") or Supercopa games ("SCOPA").
#' @param nums Numbers corresponding to the website from which scraping.
#' @param verbose Should R report information on progress? Default TRUE.
#' @param accents If \code{competition} is ACB, should we keep the Spanish accents? 
#' The recommended option is to remove them, so default FALSE.
#' @param r_user Email to identify the user when doing web scraping. 
#' This is a polite way to do web scraping and to certify that the user 
#' is working as transparently as possible with a research purpose.
#' @param df0 Data frame to save the games data.
#' @param df_bio0 Data frame to save the rosters data.
#' 
#' @return 
#' Data frame.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' library(readr)
#' # 1. The first time, all the historical data until the last games played can be 
#' # directly scraped. 
#' 
#' # ACB seasons available and corresponding games numbers:
#' acb_nums <- list(30001:30257, 31001:31262, 32001:32264, 33001:33492, 34001:34487,
#'                  35001:35494, 36001:36498, 37001:37401, 38001:38347, 39001:39417,
#'                  40001:40415, 41001:41351, 42001:42350, 43001:43339, 44001:44341,
#'                  45001:45339, 46001:46339, 47001:47339, 48001:48341, 49001:49341,
#'                  50001:50339, 51001:51340, 52001:52327, 53001:53294, 54001:54331,
#'                  55001:55331, 56001:56333, 57001:57333, 58001:58332, 59001:59331,
#'                  60001:60332, 61001:61298, 
#'                  62001:62135)
#' names(acb_nums) <- paste(as.character(1985:2017), as.character(1986:2018), sep = "-")
#' 
#' df0 <- data.frame()
#' df_bio0 <- data.frame(CombinID = NA, Player = NA, Position = NA, 
#'                       Height = NA, Date_birth = NA, 
#'                       Nationality = NA, Licence = NA, Website_player = NA)
#'                       
#' # All the games and players:
#' get_data <- get_games_rosters(competition = "ACB", type_league = "ACB", 
#'                               nums = acb_nums, verbose = TRUE, accents = FALSE, 
#'                               r_user = "guillermo.vinue@uv.es", 
#'                               df0 = df0, df_bio0 = df_bio0)
#' acb_games <- get_data$df0
#' acb_players <- get_data$df_bio0
#' write_csv(acb_games, path = "acb_games.csv")
#' write_csv(acb_players, path = "acb_players.csv")        
#' 
#' # 2. Then, in order to scrape new games as they are played, the df0 and df_bio0 objects are
#' # the historical games and rosters:
#' acb_nums <- list(62136:62153)
#' names(acb_nums) <- "2017-2018"
#' df0 <- read_csv("acb_games.csv", guess_max = 1e5)
#' df_bio0 <- read_csv("acb_players.csv", guess_max = 1e3)
#' get_data <- get_games_rosters(competition = "ACB", type_league = "ACB", 
#'                               nums = acb_nums, verbose = TRUE, accents = FALSE, 
#'                               r_user = "guillermo.vinue@uv.es", 
#'                               df0 = df0, df_bio0 = df_bio0)
#' 
#' # -----                               
#' 
#' # ACB Copa del Rey seasons available and corresponding games numbers (rosters were 
#' already downloaded with the ACB league):
#' acb_crey_nums <- list(50001:50004, 51001:51007, 52001:52007, 53033:53039, 
#'                       54033:54039, 55033:55040, 56033:56040, 57029:57036, 
#'                       58025:58032, 59038:59045, 60001:60008, 61001:61007, 
#'                       62001:62007, 63001:63007, 64001:64007, 65001:65007, 
#'                       66001:66007, 67001:67007, 68001:68007, 69001:69007, 
#'                       70001:70007, 71001:71007, 72001:72007, 73001:73007, 
#'                       74001:74007, 75001:75007, 76001:76007, 77001:77007, 
#'                       78001:78007, 79001:79007, 80001:80007, 81001:81007)
#' names(acb_crey_nums) <- paste(as.character(1985:2016), as.character(1986:2017), sep = "-")
#' 
#' df0 <- data.frame()
#' get_data <- get_games_rosters(competition = "ACB", type_league = "CREY", 
#'                               nums = acb_crey_nums, verbose = TRUE, accents = FALSE, 
#'                               r_user = "guillermo.vinue@uv.es", 
#'                               df0 = df0, df_bio0 = NULL)
#' acb_crey_games <- get_data$df0
#' write_csv(acb_crey_games, path = "acb_crey_games.csv")
#' 
#' # -----  
#' 
#' # ACB Supercopa seasons available and corresponding games numbers (rosters were 
#' already downloaded with the ACB league):
#' acb_scopa_nums <- list(1001, 2001, 3001, 4001, 5001:5004, 6001:6004, 
#'                        7001:7003, 9001:9003, 10001:10003, 11001:11003,
#'                        12001:12003, 13001:13003, 14001:14003, 15001:15003, 
#'                        16001:16003, 17001:17003, 18001:18003, 19001:19003)
#' # I haven't found the data for the supercopa in Bilbao 2007 ; 8001:8003
#' # http://www.acb.com/fichas/SCOPA8001.php
#' names(acb_scopa_nums) <- c(paste(as.character(1984:1987), as.character(1985:1988), sep = "-"),
#'                            paste(as.character(2004:2006), as.character(2005:2007), sep = "-"),
#'                            paste(as.character(2008:2018), as.character(2009:2019), sep = "-"))
#'                            
#' df0 <- data.frame()
#' get_data <- get_games_rosters(competition = "ACB", type_league = "SCOPA", 
#'                               nums = acb_scopa_nums, verbose = TRUE, accents = FALSE, 
#'                               r_user = "guillermo.vinue@uv.es", 
#'                               df0 = df0, df_bio0 = NULL)
#' acb_scopa_games <- get_data$df0
#' write_csv(acb_scopa_games, path = "acb_scopa_games.csv")
#' 
#' # -----  
#' 
#' # Euroleague seasons available and corresponding games numbers:
#' euroleague_nums <- list(1:128,
#'                         1:263, 1:250, 1:251, 1:253, 1:253, 1:188, 1:189, 
#'                         1:188, 1:188, 1:231, 1:231, 1:231, 1:229, 1:220, 
#'                         1:220, 1:275, 1:169)
#' names(euroleague_nums) <- 2017:2000
#'
#' df0 <- data.frame()
#' df_bio0 <- data.frame(CombinID = NA, Player = NA, Position = NA, 
#'                      Height = NA, Date_birth = NA, 
#'                      Nationality = NA, Website_player = NA)
#' get_data <- get_games_rosters(competition = "Euroleague", nums = euroleague_nums, 
#'                               verbose = TRUE, r_user = "guillermo.vinue@uv.es", 
#'                               df0 = df0, df_bio0 = df_bio0)
#' euroleague_games <- get_data$df0
#' euroleague_players <- get_data$df_bio0
#' write_csv(euroleague_games, path = "euroleague_games.csv")
#' write_csv(euroleague_players, path = "euroleague_players.csv")                         
#' 
#' # ----- 
#' 
#' # Eurocup seasons available and corresponding games numbers:
#' eurocup_nums <- list(1:128,
#'                      2:186, 1:306, 1:306, 1:366, 1:157, 1:156, 1:156, 1:156, 
#'                      1:151, 1:326, 1:149, 1:149, 1:239, 1:209, 1:150)
#' names(eurocup_nums) <- 2017:2002
#' 
#' df0 <- data.frame()
#' df_bio0 <- data.frame(CombinID = NA, Player = NA, Position = NA, 
#'                      Height = NA, Date_birth = NA, 
#'                      Nationality = NA, Website_player = NA)
#' get_data <- get_games_rosters(competition = "Eurocup", nums = eurocup_nums, 
#'                               verbose = TRUE, r_user = "guillermo.vinue@uv.es", 
#'                               df0 = df0, df_bio0 = df_bio0)
#' eurocup_games <- get_data$df0
#' eurocup_players <- get_data$df_bio0
#' write_csv(eurocup_games, path = "eurocup_games.csv")
#' write_csv(eurocup_players, path = "eurocup_players.csv") 
#' 
#' }
#'
#' @export

get_games_rosters <- function(competition, type_league, nums, verbose = TRUE, 
                              accents = FALSE, r_user, df0, df_bio0){
  if (competition == "ACB") {
    for (i in 1:length(nums)) {
      if (verbose){
        print(names(nums)[i]) 
      }
      df1 <- do_scraping_games(competition = competition, type_league = type_league, 
                               nums = nums[[i]], year = names(nums)[i],
                               verbose = verbose, accents = accents, r_user = r_user)
      df0 <- rbind(df0, df1)
      
      #if (type_league == "ACB") {
        pcode <- setdiff(df0$CombinID, df_bio0$CombinID)
        pcode1 <- pcode[pcode != 0 & !is.na(pcode)]
        if (verbose) {
          print(length(pcode1))
        }  
      
        df_bio1 <- do_scraping_rosters(competition = competition, pcode = pcode1, 
                                       verbose = verbose, accents = accents, r_user = r_user)
      
        df_bio0 <- rbind(df_bio0, df_bio1)
      #}  
      if (verbose) {
        print("Done!")
      }
    }
  }
  
  if (competition %in% c("Euroleague", "Eurocup") ) {
    for (i in 1:length(nums)) {
      if (verbose){
        print(names(nums)[i]) 
      }
      df1 <- do_scraping_games(competition = competition, nums = nums[[i]], 
                               year = names(nums)[i],
                               verbose = verbose, r_user = r_user)
      df0 <- rbind(df0, df1)
      
      pcode <- setdiff(df0$CombinID, df_bio0$CombinID)
      pcode1 <- pcode[pcode != 0 & !is.na(pcode)]
      print(length(pcode1))
      
      df_bio1 <- do_scraping_rosters(competition = competition, pcode = pcode1, 
                                     year = names(nums)[i],
                                     verbose = verbose, r_user = r_user)
      
      df_bio0 <- rbind(df_bio0, df_bio1)
      if (verbose) {
        print("Done!")
      }
    }
  }
  
  if (!competition %in% c("ACB", "Euroleague", "Eurocup")) {
    print("This competition is not available.")
  }

  return(list(df0 = df0, df_bio0 = df_bio0))
}

