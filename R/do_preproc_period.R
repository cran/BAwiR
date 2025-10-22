#' Data preprocessing for periods
#' 
#' @aliases do_preproc_period
#'
#' @description 
#' Preprocess the data that will be needed for computing statistics per period.
#' 
#' @usage 
#' do_preproc_period(data, team_sel, period_sel, data_sl)
#' 
#' @param data Prepared data from a given game.
#' @param team_sel One of the teams' names involved in the game.
#' @param period_sel Period of interest. Options can be "xC", where x=1,2,3,4.
#' @param data_sl Data with the starting lineups.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' team_sel <- "Valencia Basket"
#' period_sel <- "1C"
#'
#' pre_per <- do_preproc_period(acb_vbc_cz_pbp_2223, team_sel, period_sel, acb_vbc_cz_sl_2223)
#' 
#' df2 <- pre_per$df2
#' df0_inli_team <- pre_per$df0_inli_team
#'
#' @export

do_preproc_period <- function(data, team_sel, period_sel, data_sl) {
  period <- team <- action <- NULL
  
  df0 <- data
   
  # Remove overtimes:
  rm_overtime <- TRUE
  if (rm_overtime) {
    df0 <- df0 %>%
      filter(!grepl("PR", period)) %>%
      mutate(period = as.character(period))
  }
    
  df1 <- df0 %>%
    filter(team == team_sel) %>%
    filter(!action %in% c("D - Descalificante - No TL", "Altercado no TL")) 
      
  df2 <- df1 %>%
    filter(period == period_sel)
      
  df0_inli_team <- data_sl %>% 
    filter(team == team_sel, period == period_sel)
  
  return(list(df2 = df2, df0_inli_team = df0_inli_team))
}
