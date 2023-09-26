#' Prepare ACB play-by-play data
#' 
#' @aliases do_prepare_data
#'
#' @description 
#' Prepare the ACB play-by-play data to be analyzed in further steps. 
#' It involves correcting some inconsistencies and filtering some
#' unnecessary information.
#' 
#' @usage 
#' do_prepare_data(data, day_num, data_gsl, data_ginfo, game_code_excel)
#' 
#' @param data Source play-by-play data from a given game.
#' @param day_num Day number.
#' @param data_gsl Games' starting lineups.
#' @param data_ginfo Games' basic information.
#' @param game_code_excel Game code.
#' 
#' @return 
#' Data frame. Each row represents the action happened in the game. It has
#' associated a player, a time point and the game score. The \strong{team} 
#' column refers to the team to which the player belongs.
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
#' library(dplyr)
#' df0 <- acb_vbc_cz_pbp_2223
#' 
#' day_num <- unique(acb_vbc_cz_pbp_2223$day)
#' game_code <- unique(acb_vbc_cz_pbp_2223$game_code)
#' 
#' acb_games_2223_sl <- acb_vbc_cz_sl_2223 %>%
#'   filter(period == "1C")
#' 
#' df1 <- do_prepare_data(df0, day_num, 
#'                        acb_games_2223_sl, acb_games_2223_info,
#'                        game_code)
#' #df1                        
#'
#' @export

do_prepare_data <- function(data, day_num, data_gsl, data_ginfo, game_code_excel) {
  local_score <- visitor_score <- day <- game_code <- game <- action <- NULL
  
  # Correct names:
  data$player[which(data$player == "Fern\\u00e1ndez_Juan")] <- "Fern\\u00e1ndez"
  data$player[which(data$player == "Fern\\u00e1ndez_J")] <- "Fern\\u00e1ndez"
  data$player[which(data$player == "Garc\\u00eda_J")] <- "Garc\\u00eda"
  data$player[which(data$player == "Rodr\\u00edguez_S")] <- "Rodr\\u00edguez"
  data$player[which(data$player == "Garc\\u00eda_S")] <- "Garc\\u00eda"
  data$player[which(data$player == "D\\u00edaz_A")] <- "D\\u00edaz"
  data$player[which(data$player == "Diop_I")] <- "Diop"
  data$player[which(data$player == "Diop_K")] <- "Diop"

  # ----
  
  data1 <- data %>%
    mutate(local_score = as.numeric(local_score),
           visitor_score = as.numeric(visitor_score))

  data1_gsl <- data_gsl %>%
    filter(day == day_num, game_code == game_code_excel) 
  
  data2 <- bind_rows(data1_gsl, data1)
  
  # Join game information:
  data3 <- left_join(data2, data_ginfo, by = c("game_code", "day"))
  
  # Process data:
  data4 <- data3 %>%
    mutate(local = gsub("-.*", "", game)) %>%
    mutate(visitor = gsub(".*-", "", game)) %>%
    select(-game) %>%
    filter(!action %in% c("Instant Replay", "Tiempo Muerto de TV", 
                          "IR - Challenge entrenador local", "IR - Challenge entrenador visitante",
                          "IR - Revisi\\u00f3n del tipo de falta", "IR - Revisi\\u00f3n reloj de posesi\\u00f3n",
                          "IR - Revisi\\u00f3n acci\\u00f3n jugador", 
                          "IR - Revisi\\u00f3n \\u00faltimo jugador en tocar bal\\u00f3n",
                          "IR - Revisi\\u00f3n por enfrentamiento", "IR - Revisi\\u00f3n de una violaci\\u00f3n", 
                          "IR - Revisi\\u00f3n del reloj de partido", "IR - Revisi\\u00f3n de la validez de una canasta", 
                          "IR - Comprobaci\\u00f3n del tipo de tiro convertido"))
  
  return(data4)
}
