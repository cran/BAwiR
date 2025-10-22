#' Prepare data for the timeouts computation
#' 
#' @aliases do_prepare_data_to
#'
#' @description 
#' The computation of the successful timeouts requires a specific data
#' preparation. This function does this data processing.
#' 
#' @usage 
#' do_prepare_data_to(data, rm_overtime, data_ginfo, data_gcoach)
#' 
#' @param data Source play-by-play data from a given game.
#' @param rm_overtime Logical. Decide to remove overtimes or not.
#' @param data_ginfo Games' basic information.
#' @param data_gcoach Coach of each team in each day.
#' 
#' @return 
#' Data frame. Each row represents the action happened in the game. 
#' The \strong{team} column refers in this case both to the team to 
#' which the player belongs and the coach of that team. In addition,
#' a \strong{points} column is added to transform the action
#' that finished in scoring into numbers .
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
#' \code{\link{do_time_out_success}}
#' 
#' @examples 
#' df0 <- acb_vbc_cz_pbp_2223
#' 
#' df1 <- do_prepare_data_to(df0, TRUE, acb_games_2223_info, acb_games_2223_coach)
#' #df1                        
#'
#' @importFrom tidyr unite
#'
#' @export

do_prepare_data_to <- function(data, rm_overtime, data_ginfo, data_gcoach) {
  period <- game <- action <- team <- player <- coach <- NULL
  
  if (rm_overtime) {
    data <- data %>%
      filter(!grepl("PR", period)) %>%
      mutate(period = as.character(period))
  }
  
  # Join with games' information to find out later on which team was local 
  # and which team was visitor:
  data1 <- left_join(data, data_ginfo, by = c("game_code", "day"))
  
  # Extract all unique actions:
  #sort(unique(data1$action))
  #data1 %>% 
  #  distinct(action) %>% 
  #  arrange(action) %>%
  #  pull()
  # "Mate" indicates the same as "Tiro de 2 anotado", i.e., two points scored.
  
  # Process data:
  data2 <- data1 %>%
    mutate(local = gsub("-.*", "", game)) %>%
    mutate(visitor = gsub(".*-", "", game)) %>%
    select(-game) %>%
    filter(!action %in% c("Entra a pista", "Sale de la pista", "Instant Replay", "Tiempo Muerto de TV", 
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
  
  # Check that points indeed sum the final scores.
  #data2 %>%
  #  group_by(game_code, team) %>%
  #  summarise(team_points = sum(points, na.rm = TRUE)) %>%
  #  ungroup()
  
  # Join the coach's name for each team:
  data3 <- left_join(data2, data_gcoach, by = c("game_code", "day", "team"))
  
  data4 <- data3 %>%
    unite(team, c("team", "coach"), sep = "_", remove = FALSE) %>%
    mutate(player = ifelse(action == "Tiempo Muerto",
                           paste0(player, "_", coach),
                           player)) %>%
    select(-coach)
  
  return(data4)
}
