#' Compute when possessions start
#' 
#' @aliases do_possession
#'
#' @description 
#' Compute when the possession starts for each team during each period of a game.
#' 
#' @usage 
#' do_possession(data, period_sel)
#' 
#' @param data Play-by-play prepared data from a given game.
#' @param period_sel Period of interest. Options can be "xC", where x=1,2,3,4.
#' 
#' @return 
#' Data frame.  This is the meaning of the columns that might not be 
#' explanatory by themselves:
#' \itemize{
#'   \strong{time_start}: Time point when the action starts.
#'   \strong{time_end}: Time point when the action ends.
#'   \strong{poss_time}: Duration of the possession.
#'   \strong{possession}: Indicates when the possession starts. This is encoded
#'   with the Spanish word \emph{inicio} (\emph{start}, in English).
#'   \strong{points}: Number of points scored from a given action.
#' }
#' 
#' @note 
#' 1. A possession lasts 24 seconds in the ACB league.
#' 
#' 2. Actions are given in Spanish. A bilingual basketball vocabulary (Spanish/English)
#' is provided in \url{https://www.uv.es/vivigui/docs/basketball_dictionary.xlsx}.
#' 
#' 3. The \strong{game_code} column allows us to detect the source website, for example,
#' \url{https://live.acb.com/es/partidos/103389/jugadas}.
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
#'   dplyr::filter(period == "1C")
#' 
#' df1 <- do_prepare_data(df0, day_num, 
#'                        acb_games_2223_sl, acb_games_2223_info,
#'                        game_code)
#'                        
#' df2 <- do_possession(df1, "1C")                         
#' #df2
#'
#' @importFrom dplyr lag lead summarize
#'
#' @export

do_possession <- function(data, period_sel) {
  team <- action <- player <- period <- time_point <- block <- NULL
  time_start <- time_end <- poss_time <- possession <- NULL
  
  # Two main situations with start possession:
  # First one:
  data1 <- data %>%
    filter(!action %in% c("Quinteto inicial", "Salto perdido", "Tiempo Muerto",
                          "Sale de la pista", "Entra a pista")) %>%
    filter(period == period_sel) %>%
    filter(!(player == team & action != "Rebote Defensivo")) %>%
    mutate(possession = ifelse(action %in% c("Salto ganado", "Rebote Defensivo", "Recuperaci\u00f3n"), 
                              "inicio", NA), 
           .after = action)
  
  if (period_sel != "1C") {
    data1$possession[1] <- "inicio"
  }

  # Reverse situations where some type of Falta Personal is before Falta Recibida. 
  # This causes errors in the computation of the time possession.
  wh_fo <- which(grepl("Falta Personal", data1$action) & data1$team == lag(data1$team))
  while (length(wh_fo) != 0) {
    for (i in 1:length(wh_fo)) {
      # 24 103158 1C wh_fo[4] is 97 (the penultimate row), so wh_fo[i] + 2 does not exist:
      if ((wh_fo[i] + 2) > nrow(data1)) {
        data1 <- data1[c(1:(wh_fo[i] - 1), wh_fo[i] + 1, wh_fo[i]), ]
      }else{
        data1 <- data1[c(1:(wh_fo[i] - 1), wh_fo[i] + 1, wh_fo[i], (wh_fo[i] + 2):nrow(data1)), ] 
      }
    } 
    wh_fo <- which(grepl("Falta Personal", data1$action) & data1$team == lag(data1$team))
  } 
  
  # Second one:
  wh <- which(data1$action == "Asistencia" & data1$team != lead(data1$team))
  data1$possession[wh + 1] <- "inicio"
  
  # Other situations with start possession:
  # si1 : Check if any dunk came after no assist, for example,
  # when finishing an offensive rebound with a direct dunk.
  si1 <- which(data1$action == "Mate" & lead(data1$action) != "Asistencia") + 1
  si2 <- which(grepl("Tiro de", data1$action) & data1$team != lag(data1$team) & lag(data1$action) != "Falta Personal")
  si3 <- which(grepl("Triple", data1$action) & data1$team != lag(data1$team) & lag(data1$action) != "Falta Personal")
  si4 <- which(grepl("Tiro Libre", data1$action) & data1$team != lead(data1$team)) + 1
  si5 <- which(data1$action == "P\u00e9rdida" & lead(data1$action) != "Recuperaci\u00f3n") + 1
  si6 <- which(data1$action == "P\u00e9rdida" & data1$team != lag(data1$team))
  si7 <- which(data1$action == "Falta Personal (2TL)" & data1$team == lag(data1$team)) + 1
  si8 <- which(data1$action == "Falta en Ataque" & data1$team != lag(data1$team))
  si9 <- which(data1$action == "Falta Recibida" & !grepl("Falta Personal", lag(data1$action)))
  
  data1$possession[c(si1, si2, si3, si4, si5, si6, si7, si8, si9)] <- "inicio"   
  
  # Correct some inaccuracies, when they are not in the first row:
  data1$possession[which(data1$action == "Tiro Libre fallado" & data1$possession == "inicio")] <- NA
  data1$possession[which(data1$action == "Falta Personal (1TL)" & data1$possession == "inicio")] <- NA

  if (is.na(data1$possession[1])) {
    data1$possession[1] <- "inicio"
  }
  
  # Create time end and start to compute the possession time:
  data2 <- data1 %>%
    mutate(time_start = lag(time_point), .before = time_point) %>%
    rename(time_end = time_point)
  data2$time_start[1] <- "10:00"
  data2$time_end[nrow(data2)] <- "00:00"
  
  # Add for each possession a label block to be able to compute the possession time:
  ini <- which(data2$possession == "inicio")
  block_v <- c()
  for (i in 1:(length(ini) - 1)) {
    block_v <- c(block_v, rep(ini[i], ini[i + 1] - ini[i]))
  }
  
  # If 'inicio' is in the last row of data2:
  if (ini[length(ini)] == nrow(data2)) {
    block_v <- c(block_v, ini[length(ini)])
  }else{
    # If not, repeat the needed value as many times as needed. For example, if 
    # 'inicio' is in the row 84 and data2 has 85 rows, we need to create two 84,
    # as 85 - 84 + 1:
    reps_need <- nrow(data2) - ini[length(ini)] + 1
    block_v <- c(block_v, rep(ini[length(ini)], reps_need))
  }
  
  # Note: The block numbers refer to the rows where 'inicio' were located. 
  # For example, if the second 'inicio' label was in the fourth row, 
  # the second block will be labelled with a 4.
  
  data3 <- data2 %>%
    mutate(block = block_v, .after = period) 
  
  # Compute the possession times:
  data3_time <- data3 %>%
    group_by(block) %>%
    summarize(poss_time = period_to_seconds(ms(time_start[1])) - 
                period_to_seconds(ms(time_end[n()]))) %>%
    ungroup()
    
  data4 <- left_join(data3, data3_time, by = "block")  %>%
    select(period, block, time_start, time_end, poss_time, everything())
  
  # In data4, poss_time goes beyond 24 either because offensive rebounds 
  # or because fouls received or because transcription typos.
  # 103389 1C: In data1 between data1$time_point[84] and data1$time_point[85] 
  # goes 38 seconds! --> "02:14" and "01:36" ; Also 2C: 08:00 and 07:33

  # Add points:
  data5 <- data4 %>%
    mutate(points = case_when(
      action == "Tiro Libre anotado" ~ 1,
      action == "Mate" ~ 2,
      action == "Tiro de 2 anotado" ~ 2,
      action == "Triple anotado" ~ 3),
      .after = possession)
  
  return(data5)
}
