#' Players' usage
#' 
#' @aliases do_usage
#'
#' @description 
#' For each period of a game, this function computes the players' usage, which indicates how 
#' many possessions each player ended. A possession ends with a field-goal or free-throw attempt, 
#' or with a turnover.
#' 
#' @usage 
#' do_usage(data_possess, season = "2025-2026")
#' 
#' @param data_possess Data frame with the beginning of each possession 
#' obtained with \code{\link{do_possession}}.
#' @param season Season string.
#' 
#' @return 
#' A data frame with the players' usage.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_possession}}
#' 
#' @examples 
#' \dontrun{
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
#'                        
#' df2 <- do_possession(df1, "1C")    
#' 
#' do_usage(df2, "2022-2023")
#' }
#'
#' @export

do_usage <- function(data_possess, season = "2025-2026") {
  poss_time <- action <- day <- game_code <- player <- NULL
  team <- period <- possession <- poss_end <- poss_num <- NULL
  
  data_possess <- data_possess %>% 
    filter(poss_time != 0) %>% 
    filter(!action %in% c("Asistencia", "Tap\u00f3n", "Tap\u00f3n Recibido")) 
  # sort(unique(data_possess$action))
  
  # Sometimes there are two 'inicios' as the last two rows, 
  # so I have to check twice:
  if (!is.na(data_possess$possession[nrow(data_possess)])) {
    data_possess <- data_possess[-nrow(data_possess), ]
  }
  
  if (!is.na(data_possess$possession[nrow(data_possess)])) {
    data_possess <- data_possess[-nrow(data_possess), ]
  }
  
  init_poss <- c(which(data_possess$possession == "inicio"), nrow(data_possess))
  
  data_usg <- data.frame()
  for (i in 1:(length(init_poss) - 1)) {
    ini_seq <- init_poss[i]
    
    if ((ini_seq + 1) == nrow(data_possess)) {
      end_seq <- nrow(data_possess)
    }else{
      end_seq <- init_poss[i + 1] 
    }
    
    if ((ini_seq + 1) == end_seq) {
      data_usg_iter <- data_possess %>% 
        slice(ini_seq) 
    }else{
      data_usg_iter <- data_possess %>% 
        slice(end_seq - 1) 
    }
    
    data_usg_iter <- data_usg_iter %>% 
      select(day, game_code, player, team)
    
    data_usg <- rbind(data_usg, data_usg_iter)
  }
  
  data_player_poss <- data_usg %>% 
    count(day, game_code, team, player, name = "poss_end")
  
  data_team_poss <- data_possess %>% 
    count(day, game_code, team, period, possession, name = "poss_num") %>% 
    filter(!is.na(possession)) %>%
    select(-possession)
  
  data_all <- left_join(data_team_poss, data_player_poss, by = c("day", "game_code", "team")) %>%
    mutate(season = season) %>%
    select(season, day, game_code, period, team, player, poss_end, poss_num) %>%
    mutate(usage_perc = round((poss_end / poss_num) *100, 2))
  
  return(data_all)
}
