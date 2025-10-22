#' Volume of three-point shots
#' 
#' @aliases do_volume_threes
#'
#' @description 
#' This function computes the three-point shots volume, both in offense in defense.
#' This volume is defined as the percentage of three-point shots attempted with respect 
#' to the total field-goal attempts.
#' 
#' @usage 
#' do_volume_threes(df)
#' 
#' @param df Data frame with the games and the players info.
#' 
#' @return 
#' A data frame with the volume statistics.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' library(dplyr)
#' 
#' df0 <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718) 
#' 
#' df1 <- df0 %>% rename(game_code = Game)
#' 
#' data_volume <- do_volume_threes(df1)
#' 
#' data_volume$data_volume_threes
#' data_volume$data_volume_threes_comp
#'                  
#' @export

do_volume_threes <- function(df) {
  game_code <- Team <- ThreePA <- TwoPA <- team <- NULL
  threes_off <- fg_off <- threes_def <- fg_def <- NULL
  
  data_volume_threes <- df %>%
    group_by(game_code, Team) %>%
    summarise(threes_off = sum(ThreePA),
              fg_off = sum(TwoPA) + sum(ThreePA)) %>%
    ungroup() %>%
    group_by(game_code) %>%
    mutate(threes_def= rev(threes_off),
           fg_def = rev(fg_off)) %>%
    ungroup() %>%
    group_by(Team) %>%
    summarise(threes_off = sum(threes_off),
              fg_off = sum(fg_off),
              threes_def = sum(threes_def),
              fg_def = sum(fg_def)) %>%
    ungroup() %>%
    mutate(volume_off = round((threes_off / fg_off) * 100, 2)) %>%
    mutate(volume_def = round((threes_def / fg_def) * 100, 2)) %>%
    rename(team = Team)
  
  data_volume_threes_comp <- data_volume_threes %>%
    select(-contains("threes"), -contains("fg")) %>%
    rename(offense = 2, defense = 3) %>%
    pivot_longer(!team, names_to = "area", values_to = "volume")
  
  return(list(data_volume_threes = data_volume_threes, data_volume_threes_comp = data_volume_threes_comp))
}
