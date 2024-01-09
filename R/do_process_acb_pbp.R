#' Processing of the ACB website play-by-play data 
#'
#' @aliases
#' do_process_acb_pbp
#' 
#' @description
#' This function disentangles the play-by-play data coming from the ACB website and 
#' creates a common data structure in R.
#' 
#' @usage
#' do_process_acb_pbp(game_elem, day, game_code, period, acb_shields, verbose)
#'                     
#' @param game_elem Character with the tangled play-by-play data.
#' @param day Day of the game.
#' @param game_code Game code.
#' @param period Period of the game.
#' @param acb_shields Data frame with the links to the shields of the ACB teams.
#' @param verbose Logical to display processing information.
#'
#' @return
#' Data frame with eight columns: 
#' \itemize{
#' \item period: Period of the game.
#' \item time_point: Time point when the basketball action happens.
#' \item player: Player who performs the action.
#' \item action: Basketball action.
#' \item local_score: Local score at that time point.
#' \item visitor_score: Visitor score at that time point.
#' \item day: Day of the game.
#' \item game_code: Game code.
#' }
#' 
#' @note 
#' 1. Actions are given in Spanish. A bilingual basketball vocabulary (Spanish/English)
#' is provided in \url{https://www.uv.es/vivigui/docs/basketball_dictionary.xlsx}.
#' 
#' 2. The \strong{game_code} column allows us to detect the source website, for example,
#' \url{https://jv.acb.com/es/103389/jugadas}.
#' 
#' @author
#' Guillermo Vinue
#'
#' @examples
#' \dontrun{
#' # Load packages required:
#' library(RSelenium)
#' 
#' # Provide the day and game code:
#' day <- "24"
#' game_code <- "103170"
#' 
#' # Open an Internet server:
#' rD <- rsDriver(browser = "firefox", chromever = NULL)
#' 
#' # Follow this procedure on the server:
#' # 1. Copy and paste the game link https://jv.acb.com/es/103170/jugadas
#' # 2. Click on each period, starting with 1C.
#' # 3. Scroll down to the first row of data.
#' # 4. Go back to R and run the following code:
#'  
#' # Set the remote driver:
#' remDr <- rD$client
#' 
#' # Get the play-by-play data:
#' game_elem <- remDr$getPageSource()[[1]]
#' 
#' # Close the client and the server:
#' remDr$close()
#' rD$server$stop()
#' 
#' period <- "1C"
#' data_game <- do_process_acb_pbp(game_elem, day, game_code, 
#'                                 period, acb_shields, FALSE)
#' }
#' 
#' @importFrom qdapRegex ex_between 
#' @importFrom stringr str_match_all    
#'
#' @export

do_process_acb_pbp <- function(game_elem, day, game_code, period, acb_shields, verbose) {
  # "Final del Partido", "Inicio del Partido"
  game_data <- ex_between(game_elem, "Final de Periodo", "Inicio de Periodo")[[1]]
  
  remove_obs <- ifelse(period != "4C", 1, c(1, 2))
  game_data_per <- str_split(game_data, period)[[1]][-remove_obs]
  
  # -1 because the last observation is the one indicating the start of the period.
  df0 <- data.frame(matrix(NA, nrow = length(game_data_per) - 1, ncol = 8))
  colnames(df0) <- c("period", "time_point", "player", "action", "local_score", "visitor_score", "day", "game_code")
  
  # Get the data for each action:
  for (i in 1:nrow(df0)) {
    if (verbose) {
      cat("ITERATION:", i, "\n") 
    }
    
    game_data_per_i <- str_match_all(game_data_per[i], ">\\s*(.*?)\\s*</span>")[[1]][,1]
    
    game_data_per_ij <- c()
    for (j in 1:length(game_data_per_i)) {
      game_data_per_ij <- c(game_data_per_ij, 
                            gsub(">", "", gsub("<.*", "", gsub(".*\\\">", "", game_data_per_i[j]))))
    }
    
    if (grepl("\\(", game_data_per_ij[4])) {
      game_data_per_ij <- game_data_per_ij[-4] 
    }
    
    if (length(game_data_per_ij) < 5) {
      if (game_data_per_ij[2] %in% c("Instant Replay", "Tiempo Muerto de TV", 
                                     "IR - Challenge entrenador local", "IR - Challenge entrenador visitante",
                                     "IR - Revisi\\u00f3n del tipo de falta", "IR - Revisi\\u00f3n reloj de posesi\\u00f3n",
                                     "IR - Revisi\\u00f3n acci\\u00f3n jugador", 
                                     "IR - Revisi\\u00f3n \\u00faltimo jugador en tocar bal\\u00f3n",
                                     "IR - Revisi\\u00f3n por enfrentamiento", "IR - Revisi\\u00f3n de una violaci\\u00f3n", 
                                     "IR - Revisi\\u00f3n del reloj de partido", "IR - Revisi\\u00f3n de la validez de una canasta", 
                                     "IR - Comprobaci\\u00f3n del tipo de tiro convertido")) {
        if (i == 1) {
          # For the case when the first play of the quarter is an instant replay, see for example the second
          # quarter of http://jv.acb.com/es/103363/jugadas
          game_data_per_ij <- c(game_data_per_ij[1], NA, game_data_per_ij[2], NA, NA)
        }else{
          game_data_per_ij <- c(game_data_per_ij[1], NA, game_data_per_ij[2], 
                                df0[i - 1, "local_score"], df0[i - 1, "visitor_score"]) 
        }
      }else{
        link_team <- which(sapply(acb_shields$team_link, grepl, game_data_per_i[2]))
        game_data_per_ij <- c(game_data_per_ij[1], acb_shields$team[link_team], game_data_per_ij[2:4]) 
      }
    }
    
    df0[i,] <- c(period, game_data_per_ij, day, game_code)
  }
  
  df1 <- df0[nrow(df0):1, ]
  
  df1$action <- gsub("1TL", "(1TL)", df1$action)
  df1$action <- gsub("2TL", "(2TL)", df1$action)
  df1$action <- gsub("3TL", "(3TL)", df1$action)
  
  return(df1)
}
