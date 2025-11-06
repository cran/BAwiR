#' Prepare data for the offensive rebounds computation
#' 
#' @aliases do_prepare_data_or
#'
#' @description 
#' The computation of the scoring after offensive rebounds requires a 
#' specifical data preparation. This function does this data processing.
#' 
#' @usage 
#' do_prepare_data_or(data, rm_overtime, data_ginfo)
#' 
#' @param data Source play-by-play data from a given game.
#' @param rm_overtime Logical. Decide to remove overtimes or not.
#' @param data_ginfo Games' basic information. If NULL, nothing to add.
#' 
#' @return 
#' Data frame. Each row represents the action happened in the game. 
#' The \strong{points} column is added to transform the action
#' that finished in scoring into numbers.
#' 
#' @note 
#' 1. Actions are given in Spanish. A bilingual basketball vocabulary (Spanish/English)
#' is provided in \url{https://www.uv.es/vivigui/docs/basketball_dictionary.xlsx}.
#' 
#' 2. The \strong{game_code} column allows us to detect the source website, for example,
#' \url{https://live.acb.com/es/partidos/103389/jugadas}.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_reb_off_success}}
#' 
#' @examples 
#' df0 <- acb_vbc_cz_pbp_2223
#' 
#' df1 <- do_prepare_data_or(df0, TRUE, acb_games_2223_info)
#' #df1                     
#'
#' @importFrom dplyr case_when
#'
#' @export

do_prepare_data_or <- function(data, rm_overtime, data_ginfo) {
  period <- game <- action <- NULL
  
  # Correct names:
  data$player[which(data$player == "Fern\u00e1ndez_Juan")] <- "Fern\u00e1ndez"
  data$player[which(data$player == "Fern\u00e1ndez_J")] <- "Fern\u00e1ndez"
  data$player[which(data$player == "Garc\u00eda_J")] <- "Garc\u00eda"
  data$player[which(data$player == "Rodr\u00edguez_S")] <- "Rodr\u00edguez"
  data$player[which(data$player == "Garc\u00eda_S")] <- "Garc\u00eda"
  data$player[which(data$player == "D\u00edaz_A")] <- "D\u00edaz"
  data$player[which(data$player == "Diop_I")] <- "Diop"
  data$player[which(data$player == "Diop_K")] <- "Diop"
  
  # ----
  
  if (rm_overtime) {
    data <- data %>%
      filter(!grepl("PR|5|6", period)) %>%
      mutate(period = as.character(period))
  }
  
  if (!is.null(data_ginfo)) {
    # Join with games' information to find out later on which team was local 
    # and which team was visitor:
    data1 <- left_join(data, data_ginfo, by = c("game_code", "day"))
    
    # Process data:
    data1 <- data1 %>%
      mutate(local = gsub("-.*", "", game)) %>%
      mutate(visitor = gsub(".*-", "", game)) %>%
      select(-game)  
  }else{
    data1 <- data
  }
  
  data2 <- data1 %>%
    filter(!action %in% c("Entra a pista", "Sale de la pista", "Instant Replay", 
                          "Tiempo Muerto", "Tiempo Muerto de TV", 
                          "IR - Challenge entrenador local", "IR - Challenge entrenador visitante",
                          "IR - Revisi\u00f3n del tipo de falta", "IR - Revisi\u00f3n reloj de posesi\u00f3n",
                          "IR - Revisi\u00f3n acci\u00f3n jugador", 
                          "IR - Revisi\u00f3n \u00faltimo jugador en tocar bal\u00f3n",
                          "IR - Revisi\u00f3n por enfrentamiento", "IR - Revisi\u00f3n de una violaci\u00f3n", 
                          "IR - Revisi\u00f3n del reloj de partido", "IR - Revisi\u00f3n de la validez de una canasta", 
                          "IR - Comprobaci\u00f3n del tipo de tiro convertido")) %>%
    mutate(points = case_when(
      action == "Tiro Libre anotado" ~ 1,
      action == "Mate" ~ 2,
      action == "Tiro de 2 anotado" ~ 2,
      action == "Triple anotado" ~ 3))
  
  return(data2)
}
