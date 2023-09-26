#' Compute free throw fouls
#' 
#' @aliases do_ft_fouls
#'
#' @description 
#' Compute how many 1-,2- and 3-free throw fouls has committed or 
#' received every player.
#' 
#' @usage 
#' do_ft_fouls(data, type)
#' 
#' @param data Play-by-play data.
#' @param type Either 'comm' (for committed) or 'rec' (for received).
#' 
#' @return 
#' Data frame with the following columns:
#' \itemize{
#'   \strong{team}: Name of the team.
#'   \strong{player}: Name of the player.
#'   \strong{n_ft_fouls_x}: Number of free throw fouls committed or received.
#'   \strong{n_ft_x}: Number of free throws given or got.
#'   \strong{n_ft_char}: Type of free throw. Options can be 1TL, 2TL and 3TL.
#'   \strong{n}: Number of free throws of each type.
#' }
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' df01 <- do_ft_fouls(acb_vbc_cz_pbp_2223, "comm")
#' #df01  
#'  
#' df02 <- do_ft_fouls(acb_vbc_cz_pbp_2223, "rec")
#' #df02
#' 
#' @importFrom dplyr slice
#'
#' @export

do_ft_fouls <- function(data, type) {
  action <- n_ft_char <- team <- player <- n_ft_given <- n_ft_fouls_given <- NULL
  n_ft_got <- n_ft_fouls_got <- NULL
  
  data <- data %>%
    # I have to remove both the Scrubb brothers and the
    # Quintela brothers, because they are not differentiated,
    # so we would be summing statistics for two different players.
    filter(!player %in% c("Scrubb", "Quintela"))
  
  if (type == "comm") {
    df0 <- data %>%
      filter(grepl("TL)", action)) %>%
      filter(!grepl("T\\u00e9cnica", action)) %>%
      mutate(n_ft_char = gsub("\\)", "", gsub(".*\\(", "", action)), .after = action) %>%
      mutate(n_ft_given = case_when(
        grepl("1TL", action) ~ 1,
        grepl("2TL", action) ~ 2,
        grepl("3TL", action) ~ 3), .after = n_ft_char)
    
    df1_points_given <- df0 %>%
      group_by(team, player) %>%
      summarise(n_ft_fouls_given = n(),
                n_ft_given = sum(n_ft_given)) %>%
      ungroup() %>%
      arrange(desc(n_ft_fouls_given))
    
    df1_type_given <- df0 %>%
      group_by(team, player, n_ft_char) %>%
      summarise(n = n()) %>%
      ungroup()
    
    data_res <- left_join(df1_points_given, df1_type_given, by = c("team", "player"))
  }
  
  if (type == "rec") {
    data <- data %>%
      filter(!grepl("T\\u00e9cnica", action))
    
    df2 <- data.frame()
    for (i in paste0("(", 1:3, "TL)")) {
      pos_ft <- grep(i, data$action)
      
      df0 <- data %>% 
        slice(pos_ft + 1) 
      #unique(df0$action)  #It must only appear 'Falta Recibida'
      #table(df0$action)
      
      df1 <- df0 %>%
        group_by(team, player) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        mutate(n_ft_char = i, .before = n) 
      
      df2 <- bind_rows(df2, df1)
    }
    
    df1_type_got <- df2 %>%
      arrange(team, player)  
    
    df1_points_got <- df1_type_got %>%
      mutate(n_ft_char = gsub("\\(|\\)", "", n_ft_char)) %>%
      mutate(n_ft_got = case_when(
        grepl("1TL", n_ft_char) ~ 1,
        grepl("2TL", n_ft_char) ~ 2,
        grepl("3TL", n_ft_char) ~ 3), .after = n_ft_char) %>%
      group_by(team, player) %>%
      summarise(n_ft_fouls_got = sum(n),
                n_ft_got = sum(n_ft_got * n)) %>%
      ungroup() 
    
    data_res <- left_join(df1_points_got, df1_type_got, by = c("team", "player")) %>%
      mutate(n_ft_char = gsub("\\(|\\)", "", n_ft_char)) %>%
      arrange(desc(n_ft_fouls_got))
  }
  
  return(data_res)
}
