#' Lineups-related information
#' 
#' @aliases do_lineup_metrics
#'
#' @description 
#' For every lineup, compute the number of possessions, points, assists, 
#' turnovers, field goals, rebounds and assisted field goals, both in defense 
#' and offense. 
#' 
#' @usage 
#' do_lineup_metrics(data_possess, team_sel, team_opp)
#' 
#' @param data_possess Play-by-play data with the start of possessions.
#' @param team_sel One of the teams involved in the game.
#' @param team_opp Opponent team.
#' 
#' @return 
#' Data frame. Each row is a different lineup.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' library(dplyr)
#' df0 <- acb_vbc_cz_pbp_2223
#' 
#' day_num <- unique(acb_vbc_cz_pbp_2223$day)
#' game_code <- unique(acb_vbc_cz_pbp_2223$game_code)
#' 
#' # Starting players:
#' acb_games_2223_sl <- acb_vbc_cz_sl_2223 %>%
#'   dplyr::filter(period == "1C")
#' 
#' # Prepare data:
#' df1 <- do_prepare_data(df0, day_num, 
#'                        acb_games_2223_sl, acb_games_2223_info,
#'                        game_code)
#' 
#' teams_game <- sort(unique(df1$team))
#' team_sel <- teams_game[1]
#' 
#' data <- df1
#' data <- data %>%
#'   mutate(row_num = row_number()) %>%
#'   mutate(time_point = ifelse(nchar(time_point) < 5, paste0("0", time_point), time_point))
#' 
#' # Filter by team:
#' data1 <- data %>%
#'   filter(team == team_sel)
#' 
#' # Set also the opponent team:
#' team_opp <- setdiff(unique(data$team), team_sel)
#' 
#' # Add the last row of games' data to have the real final 
#' # game score in case it is not available:
#' last_row_game <- data[nrow(data),]
#' 
#' last_row_game$time_point <- "00:00"
#' last_row_game$player <- NA
#' last_row_game$action <- NA
#' last_row_game$team <- team_sel
#' 
#' data1 <- bind_rows(data1, last_row_game)
#' 
#' # Get players out:
#' pl_out <- c(1, which(data1$action == "Sale de la pista"), nrow(data1))
#' 
#' i <- 1
#' data2 <- data1 %>%
#'   slice(pl_out[i]:pl_out[i + 1]) 
#' 
#' nr <- nrow(data2)
#' 
#' # Lineup:
#' lineup <- data2 %>%
#'   filter(action != "Sale de la pista") %>%
#'   # Avoid actions that are assigned to teams: 
#'   filter(player != team_sel) %>%
#'   distinct(player) %>%
#'   pull()
#' 
#' # Identify when the possessions start:
#' data2_rival <- data %>%
#'   filter(team == team_opp) %>%
#'   filter(between(row_num, data2$row_num[1], data2$row_num[nr]))
#' 
#' data3 <- rbind(data2, data2_rival) %>%
#'   arrange(row_num) %>%
#'   na.omit()
#' 
#' data4 <- do_possession(data3, NULL, "10:00") 
#' 
#' data4[data4$action == "Mate", "action"] <- "Tiro de 2 anotado"
#' 
#' # Obtain metrics for the lineup:
#' data5 <- do_lineup_metrics(data4, team_sel, team_opp)
#' }
#'
#' @importFrom dplyr n_distinct
#' @importFrom tidyr pivot_wider
#'
#' @export

do_lineup_metrics <- function(data_possess, team_sel, team_opp) {
  team <- possession <- points <- action <- NULL
  perdida <- tiro_libre_anotado <- tiro_libre_fallado <- tiro_de_2_anotado <- tiro_de_2_fallado <- NULL
  triple_anotado <- triple_fallado <- asistencia <- block <- NULL
  # <- falta_recibida <- tapon_recibido 

  # Filter non-informative actions:
  data_possess <- data_possess %>% 
    filter(!action %in% c("Tap\u00f3n", "Tap\u00f3n Recibido")) %>%
    filter(!grepl("Falta T\u00e9cnica|Falta Personal|Falta Antideportiva|Descalificado del partido", action))
  
  # Remove drawn personal fouls that are misleading:
  # Case 1:
  misl_pf <- which(data_possess$action == "Falta Recibida" & is.na(data_possess$possession))
  
  if (length(misl_pf) != 0) {
    data_possess <- data_possess[-misl_pf, ] 
  }
  
  # Case 2:
  misl_pf1 <- which(data_possess$action == "Falta Recibida" & 
                      !is.na(data_possess$possession) & 
                      data_possess$team != lead(data_possess$team))
  
  if (length(misl_pf1) != 0) {
    data_possess <- data_possess[-misl_pf1, ] 
  }
  
  # SELECTED TEAM:
  # 1. Number of possessions:
  num_poss_team <- data_possess %>%
    count(team, possession) %>%
    filter(possession == "inicio") %>%
    select(-possession) 
  
  poss_num <- num_poss_team$n[num_poss_team$team == team_sel]
  if (length(poss_num) == 0) {
    poss_num <- 0
  }

  # 2. Number of points:
  num_pts_team <- data_possess %>%
    group_by(team) %>%
    summarise(n = sum(points, na.rm = TRUE)) %>%
    ungroup()
  
  pts_num <- num_pts_team$n[num_pts_team$team == team_sel]
  if (length(pts_num) == 0) {
    pts_num <- 0
  }

  # 3. Number of different actions:
  #, "Falta Recibida", "Tap\u00f3n Recibido"
  template_actions <- data.frame(action = c("Asistencia", "P\u00e9rdida",
                                            "Tiro de 2 fallado", "Tiro de 2 anotado",
                                            "Tiro Libre fallado", "Tiro Libre anotado",
                                            "Triple fallado", "Triple anotado"),
                                 n = 0)
  
  data_possess_act <- data_possess %>%
    filter(team == team_sel) %>%
    count(action) %>%
    filter(!grepl("Rebote|Salto", action))
  
  data_possess_act_all <- rbind(data_possess_act, anti_join(template_actions, data_possess_act, by = "action")) %>% 
    pivot_wider(names_from = action, values_from = n) %>% 
    clean_names() %>%
    select(perdida, tiro_libre_anotado, tiro_libre_fallado, tiro_de_2_anotado, 
           tiro_de_2_fallado, triple_anotado, triple_fallado, asistencia) # , falta_recibida, tapon_recibido
  
  # 4. Number of offensive rebounds:
  data_possess_reb <- data_possess %>%
    filter((team == team_sel & action == "Rebote Ofensivo") | (team == team_opp & action == "Rebote Defensivo"))
  
  ## Number of rebounds grabbed:
  total_reb_gr <- data_possess_reb %>%
    filter(team == team_sel) %>%
    summarise(n()) %>%
    pull()
  
  ## Number of total available rebounds:
  total_reb_av <- data_possess_reb %>%
    summarise(n()) %>%
    pull()
  
  data_reb <- data.frame(off_reb = total_reb_gr, total_reb = total_reb_av)
  
  # 5. Assisted field goals:
  data_fg_ast <- data_possess %>%
    filter(team == team_sel) %>%
    group_by(block) %>%
    filter(any(action == "Asistencia") & (any(action == "Tiro de 2 anotado") | any(action == "Triple anotado"))) %>%
    ungroup() %>%
    summarise(canastas_asistidas = n_distinct(block))
  
  data_metrics_sel <- data.frame(poss_num = poss_num, pts_num = pts_num) %>%
    cbind(data_possess_act_all, data_reb, data_fg_ast)
    
  # ---
  
  # OPPONENT TEAM:
  # 1. Number of possessions:
  poss_num <- num_poss_team$n[num_poss_team$team == team_opp]
  if (length(poss_num) == 0) {
    poss_num <- 0
  }
  
  # 2. Number of points:
  pts_num <- num_pts_team$n[num_pts_team$team == team_opp]
  if (length(pts_num) == 0) {
    pts_num <- 0
  }
  
  # 3. Number of different actions:
  data_possess_act <- data_possess %>%
    filter(team == team_opp) %>%
    count(action) %>%
    filter(!grepl("Rebote|Salto", action))
  
  data_possess_act_all <- rbind(data_possess_act, anti_join(template_actions, data_possess_act, by = "action")) %>% 
    pivot_wider(names_from = action, values_from = n) %>% 
    clean_names() %>%
    select(perdida, tiro_libre_anotado, tiro_libre_fallado, tiro_de_2_anotado, 
           tiro_de_2_fallado, triple_anotado, triple_fallado, asistencia) # , falta_recibida, tapon_recibido
  
  # 4. Number of offensive rebounds:
  data_possess_reb <- data_possess %>%
    filter((team == team_opp & action == "Rebote Ofensivo") | (team == team_sel & action == "Rebote Defensivo"))
  
  ## Number of total available rebounds:
  total_reb_av <- data_possess_reb %>%
    summarise(n()) %>%
    pull()
  
  ## Number of rebounds grabbed:
  total_reb_gr <- data_possess_reb %>%
    filter(team == team_opp) %>%
    summarise(n()) %>%
    pull()
  
  data_reb <- data.frame(off_reb = total_reb_gr, total_reb = total_reb_av)
  
  # 5. Assisted field goals:
  data_fg_ast <- data_possess %>%
    filter(team == team_opp) %>%
    group_by(block) %>%
    filter(any(action == "Asistencia") & (any(action == "Tiro de 2 anotado") | any(action == "Triple anotado"))) %>%
    ungroup() %>%
    summarise(canastas_asistidas = n_distinct(block))
  
  data_metrics_opp <- data.frame(poss_num = poss_num, pts_num = pts_num) %>%
    cbind(data_possess_act_all, data_reb, data_fg_ast)
  colnames(data_metrics_opp) <- paste0(colnames(data_metrics_opp), "_opp")
  
  data_metrics <- cbind(data_metrics_sel, data_metrics_opp)
  
  return(data_metrics)
}
