#' Prepare the data for the gradient shooting plots
#' 
#' @aliases do_prepare_data_gradient
#'
#' @description 
#' Prepare the data for the gradient shooting visualizations at a player level.
#' 
#' @usage 
#' do_prepare_data_gradient(all_shots_pl, summary_shots_zone_pl, summary_shots_zone_league) 
#' 
#' @param all_shots_pl Shooting data frame associated with the filters given 
#' to \code{\link{do_shots_stats}} for the player of interest.
#' @param summary_shots_zone_pl Summary of the player's shots by zone.
#' @param summary_shots_zone_league Summary of the league's shots by zone.
#' 
#' @return 
#' A list with the following three elements:
#' \itemize{
#' \item all_shots_comp_data: Summary of the shooting data of the player and of the league.
#' \item all_shots_comp_viz: Summary of the shooting data prepared for the visualization.
#' \item player: Player's name.
#' }
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_divide_court_zones}}, \code{\link{do_shots_stats}}
#' 
#' @examples 
#' \dontrun{
#' library(dplyr)
#' 
#' df0 <- do_divide_court_zones(acb_shooting_data_2425)
#' 
#' df1 <- do_filter_data(df0, "2024-2025", "", "", "", "", "")
#' 
#' # LEAGUE METRICS:
#' shots_stats <- do_shots_stats(df1, df0) 
#' 
#' summary_shots_zone_lg <- shots_stats$summary_shots_zone
#' 
#' summary_shots_zone_league <- summary_shots_zone_lg %>%
#'   mutate(pps_league = ifelse(location_color == "2pt", 
#'                              (2 * count) / total,  
#'                              (3 * count) / total)) %>%
#'   select(location, perc_league = perc, pps_league)
#' 
#' # PLAYER METRICS:
#' df1 <- do_filter_data(df0, "2024-2025", "", "", "", "", "D. Ennis")
#' 
#' shots_stats <- do_shots_stats(df1, df0) 
#' 
#' all_shots_pl <- shots_stats$all_shots
#' 
#' summary_shots_zone_pl <- shots_stats$summary_shots_zone
#' 
#' res_grad <- do_prepare_data_gradient(all_shots_pl, summary_shots_zone_pl, summary_shots_zone_league)
#' }
#'
#' @export

do_prepare_data_gradient <- function(all_shots_pl, summary_shots_zone_pl, summary_shots_zone_league) {
  location <- pos_x <- pos_y <- player_name <- location_color <- total <- perc <- NULL
  pps_player <- fga <- fgm <- perc_player <- perc_league <- pps_league <- NULL
  
  all_shots_player <- all_shots_pl %>%
    select(location, pos_x, pos_y, player_name)
  
  summary_shots_zone_player <- summary_shots_zone_pl %>%
    mutate(pps_player = ifelse(location_color == "2pt", (2 * count) / total,  (3 * count) / total)) %>%
    select(location, fgm = count, fga = total, perc_player = perc, pps_player) %>%
    replace(is.na(.), 0)
  
  all_shots_comp <- left_join(summary_shots_zone_player, summary_shots_zone_league, by = "location") 
  
  all_shots_comp_data <- all_shots_comp %>%
    select(zone = location, fgm, fga, contains("perc"), contains("pps"))

  all_shots_comp_viz <- all_shots_comp %>%
    mutate(perc_diff = perc_player - perc_league) %>%
    mutate(pps_diff = pps_player - pps_league) %>%
    left_join(all_shots_player, by = "location") %>%
    mutate(pos_x = ifelse(pos_x > 13000, 12990, pos_x)) %>%
    filter(fga != 0) # Remove zones where the player did not take any shot.
  
  player <- unique(all_shots_comp_viz$player_name)
  
  return(list(all_shots_comp_data = all_shots_comp_data, all_shots_comp_viz = all_shots_comp_viz, player = player))
}
