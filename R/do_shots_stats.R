#' Shots statistics
#' 
#' @aliases do_shots_stats
#'
#' @description 
#' Compute both the total and by zone two-point and threes statistics. 
#' 
#' @usage 
#' do_shots_stats(data_filter, data_shots_zones)
#' 
#' @param data_filter Shooting filtered data obtained with \code{\link{do_filter_data}}.
#' @param data_shots_zones Shooting data with the court zones.
#' 
#' @return 
#' A list with the following three elements:
#' \itemize{
#' \item all_shots: Shooting data frame associated with the filters given to the function.
#' \item summary_shots: Summary of the shots as a whole.
#' \item summary_shots_zone: Summary of the shots by zone.
#' }
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_divide_court_zones}}, \code{\link{do_filter_data}}
#' 
#' @examples 
#' \dontrun{
#' df0 <- do_divide_court_zones(acb_shooting_data_2425)
#' 
#' df1 <- do_filter_data(df0, "2024-2025", "", "", "", "", "")
#' 
#' shots_stats <- do_shots_stats(df1, df0) 
#' 
#' all_shots <- shots_stats$all_shots
#' 
#' summary_shots <- shots_stats$summary_shots
#' 
#' summary_shots_zone <- shots_stats$summary_shots_zone
#' }
#' 
#' @importFrom dplyr anti_join full_join
#'
#' @export

do_shots_stats <- function(data_filter, data_shots_zones) {
  perc <- summary <- short_name <- player_license_id <- NULL
  shot <- outcome <- location <- total <- NULL
  
  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)
  
  df2_aux <- data_filter %>% 
    group_by(shot, outcome) %>% 
    summarise(count = n()) %>% 
    ungroup() %>%
    group_by(shot) %>% 
    mutate(perc = round(count / sum(count) * 100, 2)) %>%
    ungroup() 
  
  # Needed when the player did not made any 2pt or 3pt shot:
  if (nrow(df2_aux) != 4) {
    data_check <- data.frame(shot = c("2pt", "2pt", "3pt", "3pt"), outcome = c("made", "missed", "made", "missed"))
    
    no_row <- anti_join(data_check, df2_aux, by = c("shot", "outcome"))
    no_row$count <- 0
    no_row$perc <- 0
    
    df2_aux <- rbind(df2_aux, no_row) %>%
      arrange(shot, outcome)
  }
  
  df2 <- df2_aux %>%
    filter(outcome == "made")
  
  df3 <- data_filter %>% 
    group_by(shot) %>% 
    summarise(total = n()) %>%
    ungroup()
  
  # Needed when the player only shot 2pt or 3pt.
  if (nrow(df2) == 1) {
    no_shot <- setdiff(c("2pt", "3pt"), df2$shot)
    
    no_shot_df <- data.frame(shot = no_shot, outcome = "made", count = 0, perc = 0)
    df2 <- rbind(df2, no_shot_df)
    
    no_shot_df1 <- data.frame(shot = no_shot, total = 0)
    df3 <- rbind(df3, no_shot_df1)
  }
  
  df4 <- full_join(df2, df3, by = "shot") %>%
    replace(is.na(.), 0)
  
  df4$perc[1] <- paste("2pt: ", df4$perc[1], "% (", df4$count[1], "/", df4$total[1], ")", sep = "")
  df4$perc[2] <- paste("3pt: ", df4$perc[2], "% (", df4$count[2], "/", df4$total[2], ")", sep = "")
  
  df5 <- df4 %>%
    mutate(pos_x = c(9700, 9700), pos_y = c(7200, 6200)) %>% # to locate the text in the plot.
    select(-outcome)  
    
  # STATS PER ZONE:
  df2_loc <- data_filter %>% 
    group_by(location, outcome) %>% 
    summarise(count = n()) %>% 
    ungroup() %>%
    group_by(location) %>% 
    mutate(perc = round(count / sum(count) * 100, 2)) %>%
    ungroup() %>%
    filter(outcome == "made")
  
  df3_loc <- data_filter %>% 
    group_by(location) %>% 
    summarise(total = n()) %>%
    ungroup()
  
  df4_loc <- full_join(df2_loc, df3_loc, by = "location") %>%
    full_join(data_shots_zones %>% distinct(location), by = "location") %>% 
    select(-outcome) %>%
    replace(is.na(.), 0) %>%
    mutate(summary = paste(perc, "% (", count, "/", total, ")", sep = "")) %>%
    arrange(location)
  
  df4_loc$location <- factor(df4_loc$location, levels = c("2pt_center", "2pt_left", "2pt_right", "3pt_center", "3pt_left", 
                                                          "3pt_left_corner", "3pt_long", "3pt_right", "3pt_right_corner", "paint"))
  
  df5_loc <- df4_loc %>%
    mutate(pos_x = c(5300, 3000,  3000, 9000, 9000,   1000,   12400, 9000, 1000,  1200), # to locate the text in the plot.
           pos_y = c(0,    -4800, 4800, 0,    -4800, -6300,  0,     4800, 6300, 0)) %>%
    mutate(location_color = c(rep("2pt", 3), rep("3pt", 6), "2pt"))
  
  # RETURN:
  return(list(all_shots = data_filter, summary_shots = df5, summary_shots_zone = df5_loc))
}
