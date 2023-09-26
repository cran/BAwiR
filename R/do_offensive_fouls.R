#' Compute offensive fouls
#' 
#' @aliases do_offensive_fouls
#'
#' @description 
#' Compute how many offensive fouls has committed or received every player.
#' 
#' @usage 
#' do_offensive_fouls(data, type)
#' 
#' @param data Play-by-play data.
#' @param type Either 'comm' (for committed) or 'rec' (for received).
#' 
#' @return 
#' Data frame with the following columns:
#' \itemize{
#'   \strong{team}: Name of the team.
#'   \strong{player}: Name of the player.
#'   \strong{n_offensive_fouls_x}: Number of offensive fouls.
#' }
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' df01 <- do_offensive_fouls(acb_vbc_cz_pbp_2223, "comm")
#' #df01
#' 
#' df02 <- do_offensive_fouls(acb_vbc_cz_pbp_2223, "rec")
#' #df02
#'
#' @export

do_offensive_fouls <- function(data, type) {
  action <- team <- player <- n_offensive_fouls_given <- NULL
  
  if (type == "comm") {
    df1 <- data %>%
      filter(action == "Falta en Ataque") %>%
      group_by(team, player) %>%
      summarise(n_offensive_fouls_given = n()) %>%
      ungroup() %>%
      arrange(desc(n_offensive_fouls_given))
  }
  
  if (type == "rec") {
    pos_att <- grep("Falta en Ataque", data$action)
    
    #df0 <- data %>% 
    #  slice(pos_att + 1) 
    #unique(df0$action) # It must only appear 'Pérdida' 
    
    df0 <- data %>% 
      slice(pos_att + 2) 
    #unique(df0$action) # It must only appear 'Falta Recibida'
    
    df1 <- df0 %>%
      group_by(team, player) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      arrange(desc(n)) %>%
      rename(n_offensive_fouls_got = n)  
  }
  
  return(df1)
}
