#' Compute ACB sub-lineups
#' 
#' @aliases do_sub_lineup
#'
#' @description 
#' Compute all the sub-lineups that a given team shows during a game. They can
#' be made up of four, three or two players.
#' 
#' @usage 
#' do_sub_lineup(data, elem_choose)
#' 
#' @param data Data frame with the lineups (quintets).
#' @param elem_choose Numeric: 4, 3 or 2.
#' 
#' @return 
#' Data frame. Each row is a different sub-lineup. This is the meaning of the 
#' columns that might not be explanatory by themselves:
#' \itemize{
#'   \strong{team_in}: Time point when that sub-lineup starts playing together.
#'   \strong{team_out}: Time point when that sub-lineup stops playing together 
#'   (because there is a substitution).
#'   \strong{time_seconds}: Total of seconds that the sub-lineup played.
#'   \strong{plus_minus}: Plus/minus achieved by the sub-lineup. This is the difference
#'   between the game score of the previous lineup and of the current one.
#'   \strong{plus_minus_poss}: Plus/minus per possession.
#' }
#' 
#' @note 
#' A possession lasts 24 seconds in the ACB league.
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
#'                 
#' df2 <- do_lineup(df1, day_num, game_code, "Valencia Basket", FALSE)    
#' 
#' df3 <- do_sub_lineup(df2, 4)
#' #df3
#' 
#' @importFrom stringr str_split 
#' @importFrom utils combn
#' 
#' @export

do_sub_lineup <- function(data, elem_choose) {
  num_players <- diff_points <- lineup_type <- NULL
  
  if (elem_choose == 4) {
    tsl <- "quartet"
  }else if (elem_choose == 3) {
    tsl <- "trio"
  }else if (elem_choose == 2) {
    tsl <- "duo"
  }
  
  data_res <- data.frame()
  for (i in 1:nrow(data)) {
    lineup_i <- unlist(str_split(data$lineup[i], ", "))
    lineup_sub <- combn(lineup_i, elem_choose, simplify = FALSE)
    
    for (j in 1:length(lineup_sub)) {
      data_save <-  data[i,]
      data_save$lineup <- paste(lineup_sub[[j]], collapse = ", ")
      data_save$lineup_type <- tsl
      
      data_save <- data_save %>%
        select(-num_players, -diff_points) %>%
        mutate(lineup_type = as.character(lineup_type))
      
      data_res <- bind_rows(data_res, data_save)
    }
  }
  
  return(data_res)
}
