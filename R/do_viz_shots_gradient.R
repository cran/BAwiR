#' Visualization of the shots statistics with advanced features
#' 
#' @aliases do_viz_shots_gradient
#'
#' @description 
#' Create a visualization of the left half of the court and compare either the field goal
#' percentage or the points per shot of a given player with respect to the league. 
#' In addition, it can also show a heatmap with the zones where the player takes the shots.
#' 
#' @usage 
#' do_viz_shots_gradient(data_filter, type, metric, data_shots_zones, language = "English")
#' 
#' @param data_filter Shooting filtered data obtained with \code{\link{do_filter_data}}.
#' @param type Options are 'team' for team statistics, 'player' for player statistics and 
#' 'all' for the whole league.
#' @param metric Options are 'fg' for the field goal percentage, 'pps' for the points per shot
#' and 'none' if plotting a heatmap is preferred.
#' @param data_shots_zones Shooting data with the court zones.
#' @param language Language of the titles. Valid options are 'English' and 'Spanish' so far.
#' 
#' @return 
#' A plot.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_divide_court_zones}}, \code{\link{do_filter_data}}, 
#' \code{\link{do_shots_stats}}, \code{\link{do_prepare_data_gradient}}
#' 
#' @examples 
#' \dontrun{
#' df0 <- do_divide_court_zones(acb_shooting_data_2425)
#' 
#' df1 <- do_filter_data(df0, "2024-2025", "", "", "", "", "")
#' 
#' do_viz_shots_gradient(df1, "all", "none", df0)
#'
#' df1 <- do_filter_data(df0, "2024-2025", "", "", "", "", "D. Ennis")
#' 
#' do_viz_shots_gradient(df1, "player", "none", df0)
#' do_viz_shots_gradient(df1, "player", "fg", df0)
#' 
#' df1 <- do_filter_data(df0, "2024-2025", "Valencia Basket", "", "", "", "")
#' 
#' do_viz_shots_gradient(df1, "team", "none", df0)
#' }
#' 
#' @importFrom dplyr anti_join full_join
#' @importFrom ggplot2 after_stat stat_density_2d scale_fill_viridis_c guide_colorbar scale_color_viridis_c scale_color_gradient2
#'
#' @export

do_viz_shots_gradient <- function(data_filter, type, metric, data_shots_zones, language = "English") {
  player_license_id <- player_name <- perc_diff <- pps_diff <- pos_x <- pos_y <- NULL
  diff_val <- location_color <- total <- location <- perc <- pps_league <- NULL
  
  img_path <- system.file("parquet.png", package = "BAwiR")
  parquet <- png::readPNG(img_path)
  
  if (metric %in% c("fg", "pps")) {
    # LEAGUE METRICS:
    df_aux <- data_shots_zones
    shots_stats <- do_shots_stats(df_aux, data_shots_zones) 
    
    summary_shots_zone_lg <- shots_stats$summary_shots_zone
    
    summary_shots_zone_league <- summary_shots_zone_lg %>%
      mutate(pps_league = ifelse(location_color == "2pt", (2 * count) / total,  (3 * count) / total)) %>%
      select(location, perc_league = perc, pps_league)
    
    # PLAYER METRICS:
    shots_stats <- do_shots_stats(data_filter, data_shots_zones) 
    
    all_shots_pl <- shots_stats$all_shots
    
    summary_shots_zone_pl <- shots_stats$summary_shots_zone
    
    res_grad <- do_prepare_data_gradient(all_shots_pl, summary_shots_zone_pl, summary_shots_zone_league)
    
    all_shots <- res_grad$all_shots_comp_viz
  }else{
    shots_stats <- do_shots_stats(data_filter, data_shots_zones) 
    
    all_shots <- shots_stats$all_shots
  }
  
  if (type == "team") {
    title_plot_aux <- unique(all_shots$full_name)
  }else if (type == "player") {
    title_plot_aux <- unique(all_shots$player_name)
  }else if (type == "all") {
    if (language == "English") {
      title_plot_aux <- "ALL LEAGUE" 
    }else{
      title_plot_aux <- "TODA LA LIGA"
    }
  }else{
    stop("valid options are 'team', 'player' or 'all'.")
  }
  
  vect_local <- unique(data_filter$local)
  if (length(vect_local) == 2) {
    if (language == "English") {
      title_plot_aux1 <- paste0(title_plot_aux, ", HOME AND AWAY") 
    }else{
      title_plot_aux1 <- paste0(title_plot_aux, ", CASA Y FUERA")
    }
  }else if (vect_local == TRUE) {
    if (language == "English") {
      title_plot_aux1 <- paste0(title_plot_aux, ", HOME") 
    }else{
      title_plot_aux1 <- paste0(title_plot_aux, ", CASA")
    }
  }else{
    if (language == "English") {
      title_plot_aux1 <- paste0(title_plot_aux, ", AWAY") 
    }else{
      title_plot_aux1 <- paste0(title_plot_aux, ", FUERA")
    }
  }
  
  if (metric == "fg") {
    all_shots <- all_shots %>%
      rename(diff_val = perc_diff)
    
    if (language == "English") {
      title_plot <- paste0(title_plot_aux1, ", Field Goal %")  
    }else{
      title_plot <- paste0(title_plot_aux1, ", % TC") 
    }
  }else if (metric == "pps") {
    all_shots <- all_shots %>%
      rename(diff_val = pps_diff)
    
    if (language == "English") {
      title_plot <- paste0(title_plot_aux1, ", Points per shot") 
    }else{
      title_plot <- paste0(title_plot_aux1, ", Puntos por tiro")
    }
  }else if (metric == "none") {
    title_plot <- title_plot_aux1
  }else{
    stop("valid options are 'fg', 'pps' or 'none'.")
  }
  
  gg <- ggplot(data = all_shots, aes(x = pos_x, y = pos_y)) + # all_shots %>% filter(location != "paint")
    background_image(parquet) 
  
  if (metric == "none") {
    if (language == "English") {
      leg_tit <- "Shot Frequency \n"
      leg_lab <- c("lower", "higher")
    }else{
      leg_tit <- "Frecuencia de tiro \n"
      leg_lab <- c("menor", "mayor")
    }
    
    gg <- gg +
      stat_density_2d(geom = "raster", aes(fill = after_stat(density / max(density))), contour = FALSE) + 
      scale_fill_viridis_c(leg_tit, limits = c(0, 1), breaks = c(0, 1), labels = leg_lab,
                           option = "inferno", guide = guide_colorbar(barwidth = 2)) #+
      #theme(legend.position = "bottom") 
  }else{
    gg <- gg +
      geom_point(aes(color = diff_val)) +
      xlim(0, 13000) +
      ylim(-7602, 7602)
    
    range_diff <- range(all_shots$diff_val)
    range_legend <- c(floor(range_diff[1]), ceiling(range_diff[2]))
    
    if (diff(range_legend) > 30) {
      gg <- gg + scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
    }else{
      gg <- gg +
        scale_color_viridis_c(limits = range_legend, breaks = ceiling(seq(range_legend[1], range_legend[2], length.out = 5)),
                              option = "viridis")
    }
  }
  
  if (language == "English") {
    leg_col <- "Difference between \n player and league \n"
  }else{
    leg_col <- "Diferencia entre \n el jugador y la liga \n"
  }

  gg <- gg +
    # 3PT SHOT LINE:
    # Left white line:
    annotate("segment", x = 0, y = -7602, xend = 2300, yend = -7602, color = "white", linewidth = 1.2, alpha = 0.4) +
    # Right white line:
    annotate("segment", x = 0, y = 7602, xend = 2300, yend = 7602, color = "white", linewidth = 1.2, alpha = 0.4) +
    # Curve that joins the left and white lines:
    annotate("curve", x = 2300, y = -7602, xend = 2300, yend = 7602, curvature = 0.8, color = "white", linewidth = 1.2, alpha = 0.4) +
    # ---
    # PAINT:
    annotate("segment", x = 0, y = -2000, xend = 4000, yend = -2000, color = "black", linewidth = 1.2) +
    annotate("segment", x = 0, y = 2000, xend = 4000, yend = 2000, color = "black", linewidth = 1.2) +
    annotate("segment", x = 4000, y = -2000, xend = 4000, yend = 2000, color = "black", linewidth = 1.2) +
    # Arc paint:
    annotate("curve", x = 4000, y = -2000, xend = 4000, yend = 2000, curvature = 1, color = "white", linewidth = 1.2, alpha = 0.4) +
    # Arc paint dashed:
    annotate("curve", x = 4000, y = -2000, xend = 4000, yend = 2000, curvature = -1, linetype = 2, color = "white", linewidth = 1.2, alpha = 0.4) +
    # ---
    # BASKET:
    annotate("segment", x = 10, y = -1000, xend = 700, yend = -1000, color = "white", linewidth = 1.2, alpha = 0.4) +
    annotate("segment", x = 10, y = 1000, xend = 700, yend = 1000, color = "white", linewidth = 1.2, alpha = 0.4) +
    annotate("curve", x = 700, y = -1000, xend = 700, yend = 1000, curvature = 1, color = "white", linewidth = 1.2, alpha = 0.4) +
    annotate("segment", x = 0, y = -500, xend = 0, yend = 500, color = "white", linewidth = 1.2, alpha = 0.4) +
    geom_point(size = 4, pch = 1, data = data.frame(pos_x = 200, pos_y = 0), color = "white", stroke = 2, alpha = 0.4) +
    # ---
    # MID-LINE COURT:
    annotate("segment", x = 13000, y = -7602, xend = 13000, yend = 7602, color = "white", linewidth = 1.2, alpha = 0.4) +
    annotate("curve", x = 13000, y = 2000, xend = 13000, yend = -2000, curvature = 1, color = "white", linewidth = 1.2, alpha = 0.4) +
    # ---
    # 3PT RIGHT CORNER:
    annotate("segment", x = 0, y = 6700, xend = 0, yend = 7602, color = "black", linewidth = 1.2) +
    annotate("segment", x = 1000, y = 6700, xend = 1000, yend = 7602, color = "black", linewidth = 1.2) +
    annotate("segment", x = 0, y = 6700, xend = 1000, yend = 6700, color = "black", linewidth = 1.2) +
    # ---
    # 3PT LEFT CORNER:
    annotate("segment", x = 0, y = -6700, xend = 0, yend = -7602, color = "black", linewidth = 1.2) +
    annotate("segment", x = 1000, y = -6700, xend = 1000, yend = -7602, color = "black", linewidth = 1.2) +
    annotate("segment", x = 0, y = -6700, xend = 1000, yend = -6700, color = "black", linewidth = 1.2) +
    # ---
    # 3PT RIGHT:
    annotate("segment", x = 7000, y = 2000, xend = 7700, yend = 2000, color = "black", linewidth = 1.2) +
    # ---
    # 3PT LEFT: 
    annotate("segment", x = 7000, y = -2000, xend = 7700, yend = -2000, color = "black", linewidth = 1.2) +
    labs(x = "", y = "", title = title_plot, color = leg_col) +
    theme(legend.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  return(gg)
}
