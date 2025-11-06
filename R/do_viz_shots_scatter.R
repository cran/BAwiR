#' Visualization of the shots statistics
#' 
#' @aliases do_viz_shots_scatter
#'
#' @description 
#' Create a visualization of the left half of the court and annotates both the total and 
#' by zone shooting statistics. It can also show the location of each individual shot, 
#' with color-coding for makes and misses.
#' 
#' @usage 
#' do_viz_shots_scatter(shots_stats, type, draw, size_lab_box = 2.8, size_lab_court = 3, 
#'                      size_point = 3, language = "English")
#' 
#' @param shots_stats Shooting data associated with the filters given to \code{\link{do_shots_stats}}.
#' @param type Options are 'team' for team statistics, 'player' for player statistics and 
#' 'all' for the whole league.
#' @param draw Logical. TRUE to add the shots in their coordinates. FALSE to add just the
#' number of mades and attempted field goals.
#' @param size_lab_box Size of the text indicating the overall percentages (they are inside a box).
#' @param size_lab_court Size of the text indicating the percentages by zone.
#' @param size_point Size of the points.
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
#' \code{\link{do_shots_stats}}
#' 
#' @examples 
#' \dontrun{
#' df0 <- do_divide_court_zones(acb_shooting_data_2425)
#' 
#' df1 <- do_filter_data(df0, "2024-2025", "", "", "", "", "")
#' 
#' shots_stats <- do_shots_stats(df1, df0) 
#' 
#' do_viz_shots_scatter(shots_stats, "all", FALSE) 
#' do_viz_shots_scatter(shots_stats, "all", TRUE)
#' 
#' df1 <- do_filter_data(df0, "2024-2025", "", "", "", "", "D. Ennis")
#' 
#' shots_stats <- do_shots_stats(df1, df0) 
#' 
#' do_viz_shots_scatter(shots_stats, "player", FALSE) 
#' do_viz_shots_scatter(shots_stats, "player", TRUE)
#' do_viz_shots_scatter(shots_stats, "player", TRUE, language = "Spanish")
#' }
#' 
#' @importFrom ggpubr background_image
#' @importFrom dplyr anti_join full_join
#' @importFrom ggplot2 scale_color_manual scale_shape_manual xlim
#'
#' @export

do_viz_shots_scatter <- function(shots_stats, type, draw, size_lab_box = 2.8, size_lab_court = 3, 
                                 size_point = 3, language = "English") {
  outcome <- NULL
  
  all_shots <- shots_stats$all_shots
   
  summary_shots <- shots_stats$summary_shots
  
  summary_shots_zone <- shots_stats$summary_shots_zone
  
  pos_x <- pos_y <- perc <- play_type <- location_color <- NULL
  
  img_path <- system.file("parquet.png", package = "BAwiR")
  parquet <- png::readPNG(img_path)
  
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
  
  vect_local <- unique(all_shots$local)
  if (length(vect_local) == 2) {
    if (language == "English") {
      title_plot <- paste0(title_plot_aux, ", HOME AND AWAY.") 
    }else{
      title_plot <- paste0(title_plot_aux, ", CASA Y FUERA.")
    }
  }else if (vect_local == TRUE) {
    if (language == "English") {
      title_plot <- paste0(title_plot_aux, ", HOME.")
    }else{
      title_plot <- paste0(title_plot_aux, ", CASA.")
    }
  }else{
    if (language == "English") {
      title_plot <- paste0(title_plot_aux, ", AWAY.") 
    }else{
      title_plot <- paste0(title_plot_aux, ", FUERA.")
    }
  }
  
  all_shots <- all_shots %>%
    mutate(pos_x = ifelse(pos_x > 13000, 12990, pos_x))
  
  if (language == "English") {
    leg_col_manual <- c("2pt_made" = "darkgreen", "2pt_missed" = "red", "3pt_made" = "darkgreen", "3pt_missed" = "red")
    leg_sha_manual <- c("2pt_made" = 8, "2pt_missed" = 8, "3pt_made" = 17, "3pt_missed" = 17)
  }else{
    all_shots <- all_shots %>%
      mutate(play_type = plyr::mapvalues(play_type, 
                                         from = c("2pt_made", "2pt_missed", "3pt_made", "3pt_missed"),
                                         to = c("2pt_anotado", "2pt_fallado", "3pt_anotado", "3pt_fallado"))) %>%
      mutate(outcome = plyr::mapvalues(outcome, 
                                       from = c("made", "missed"),
                                       to = c("anotado", "fallado")))
    
    leg_col_manual <- c("2pt_anotado" = "darkgreen", "2pt_fallado" = "red", "3pt_anotado" = "darkgreen", "3pt_fallado" = "red")
    leg_sha_manual <- c("2pt_anotado" = 8, "2pt_fallado" = 8, "3pt_anotado" = 17, "3pt_fallado" = 17)
  }
  
  gg <- ggplot(data = all_shots, aes(x = pos_x, y = pos_y)) +
    background_image(parquet) +
    geom_text(data = summary_shots, aes(x = pos_x, y = pos_y, label = perc), size = size_lab_box) 
  
  if (draw) {
    gg <- gg +
      #geom_text(data = summary_shots_zone, aes(x = pos_x, y = pos_y, label = summary), size = 3) +
      geom_point(aes(colour = play_type, shape = play_type), size = size_point, alpha = 0.4) +
      scale_color_manual(values = leg_col_manual) +
      scale_shape_manual(values = leg_sha_manual) 
  }else{
    gg <- gg +
      geom_text(data = summary_shots_zone, aes(x = pos_x, y = pos_y, label = summary, color = location_color), size = size_lab_court) +
      scale_color_manual(values = c("2pt" = "blue", "3pt" = "darkgreen")) +
      guides(color = guide_legend(override.aes = list(label = "-", size = 12)))
  }
    
  gg <- gg +
    xlim(0, 13000) +
    ylim(-7602, 7602) +
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
    #labs(x = "", y = "", title = paste0(all_shots$player_name, 
    #                                    " <img src='", all_shots$logo, "' width='60'/>",
    #                                    " <img src='", all_shots$player_image, "' width='60'/>")) +
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
    # ---
    # BOX SUMMARY:
    #annotate("segment", x = 7800, y = 5800, xend = 7800, yend = 7600, color = "black") +
    #annotate("segment", x = 7800, y = 7600, xend = 11600, yend = 7600, color = "black") +
    #annotate("segment", x = 11600, y = 7600, xend = 11600, yend = 5800, color = "black") +
    annotate("segment", x = 11600, y = 5800, xend = 7800, yend = 5800, color = "black", linetype = "twodash") +
    # Available linetype options include: "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", or 0–6.
    # ---
    labs(x = "", y = "", title = title_plot) +
    #theme_minimal(base_size = 14) +
    #theme_void() +
    theme(legend.key.spacing.y = unit(0.4, "cm"),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          plot.title = element_text(size = 16),
          axis.text = element_blank(),
          axis.ticks = element_blank())#, plot.title = element_markdown()) 
  
  return(gg)
}
