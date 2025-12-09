#' Player's sticker
#' 
#' @aliases get_sticker
#'
#' @description 
#' This function creates players' cards (a kind of sticker) that bring together 
#' for each player three of the concepts considered most important in basketball 
#' analytics. These are efficiency (how many points the team scores and receives 
#' per 100 possessions with the player on the floor), the shooting context (from 
#' where and with what percentage they shoot) and the use of possessions (how they 
#' end the possessions they execute).
#' 
#' @usage 
#' get_sticker(data_player_eff, data_team, player_sel, language = "English",
#'             change_hjust_perc = FALSE, size_head = c(0.4, 0.3, 5.6, 3.5), 
#'             size_eff = 2, size_cont_us = c(2.3, 1.7), size_plot_tit = 8,
#'             hjust_title = 0.1, xjust_vt = c(0.392, 0.3), size_netrtg = 20)
#' 
#' @param data_player_eff Data frame with the efficiency statistics.
#' @param data_team Data frame with the context and usage statistics.
#' @param player_sel Player of interest.
#' @param language Language of the titles. Valid options are 'English' and 'Spanish' so far.
#' @param change_hjust_perc Logical to change the position of the win percentage sentence.
#' @param size_head Vector with the sizes of headers text.
#' @param size_eff Size of the text related to the winning percentage and player's efficiencies.
#' @param size_cont_us Vector with the sizes of context and usage text.
#' @param size_plot_tit Size of the plot titles.
#' @param hjust_title Adjust the title of the net efficiency.
#' @param xjust_vt Adjust the text of the team's winning percentage and efficiencies.
#' @param size_netrtg Size of the colored net efficiency value.
#' 
#' @return 
#' A plot.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' # The efficiency data frame must have this type of structure:
#' data_player_eff <- data.frame(team = "Real Madrid", 
#'                               win_perc = "83.3% (5/6)",
#'                               pts_poss = 114,
#'                               pts_poss_opp = 104,
#'                               net_rtg = 10)
#' 
#' get_sticker(data_player_eff, acb_sticker_data_2526, "A. Abalde", language = "English")
#' get_sticker(data_player_eff, acb_sticker_data_2526, "A. Abalde", language = "Spanish", TRUE)
#' }
#'
#' @importFrom ggplot2 margin
#'
#' @export

get_sticker <- function(data_player_eff, data_team, player_sel, language = "English",
                        change_hjust_perc = FALSE, size_head = c(0.4, 0.3, 5.6, 3.5), 
                        size_eff = 2, size_cont_us = c(2.3, 1.7), size_plot_tit = 8,
                        hjust_title = 0.1, xjust_vt = c(0.392, 0.3), size_netrtg = 20) {
  player <- team <- picture <- shield <- type <- value <- category <- text_plot <- NULL
  
  if (language == "English") {
    data_team <- data_team %>%
      mutate(category = plyr::mapvalues(category,
                                        from = c("3pt_anotado", "3pt_fallado",
                                                 "2pt_anotado", "2pt_fallado",
                                                 "1pt_anotado", "1pt_fallado",
                                                 "p\u00e9rdida",
                                                 "2pt_zona", "2pt_no_zona", 
                                                 "3pt_esquina", "3pt_no_esquina"),
                                        to = c("3pt_scored", "3pt_missed",
                                               "2pt_scored", "2pt_missed",
                                               "1pt_scored", "1pt_missed",
                                               "turnover",
                                               "2pt_paint", "2pt_no_paint", 
                                               "3pt_corner", "3pt_no_corner")))
    
    eff_lab1 <- "Team's winning percentage: "
    eff_lab2 <- "Offensive efficiency: "
    eff_lab3 <- " points scored per 100 possessions"
    eff_lab4 <- "Defensive efficiency: "
    eff_lab5 <- " points received per 100 possessions"
    eff_lab6 <- " NET EFFICIENCY:"
    con_lab1 <- " SHOOTING CONTEXT: \n"
    usa_lab1 <- " USE OF POSSESSIONS: \n"
  }else{
    eff_lab1 <- "Porcentaje de victorias del equipo: "
    eff_lab2 <- "Eficiencia ofensiva: "
    eff_lab3 <- " puntos anotados por 100 posesiones"
    eff_lab4 <- "Eficiencia defensiva: "
    eff_lab5 <- " puntos recibidos por 100 posesiones"
    eff_lab6 <- " EFICIENCIA NETA:"
    con_lab1 <- " CONTEXTO DE LANZAMIENTO: \n"
    usa_lab1 <- " USO DE LAS POSESIONES: \n"
  }
  
  # Select player:
  data_pl <- data_team %>%
    filter(player == player_sel)
  
  # HEADERS:
  data_sticker <- data_pl %>%
    distinct(team, player, picture, shield)
  
  header_plot <- ggplot(data_sticker) +
    ggimage::geom_image(aes(x = 1, y = 0.7, image = picture), size = size_head[1], by = "width") +
    ggimage::geom_image(aes(x = 3, y = 0.7, image = shield), size = size_head[2], by = "width") +
    annotate("text", x = 1.9, y = 0.8, label = unique(data_sticker$player),
             size = size_head[3], fontface = "bold", hjust = 0.3) +
    annotate("text", x = 2.1, y = 0.6, label = unique(data_sticker$team),
             size = size_head[4], color = "gray30") +
    xlim(0.5, 3.5) + 
    ylim(0, 1.2) +
    theme_void() +
    theme(plot.margin = margin(0, 0, -30, 0))
  
  # EFFICIENCY:
  if (language == "English") {
    if (substring(data_player_eff$win_perc, 3, 3) !=  "%") {
      hjust_perc <- 1.05
    }else{
      hjust_perc <- 0.893
    }
    hjust_eff_lab4 <- 0.535
  }else{
    if (substring(data_player_eff$win_perc, 3, 3) !=  "%") {
      if (change_hjust_perc) {
        hjust_perc <- 0.862
      }else{
        hjust_perc <- 0.845  
      }
    }else{
      hjust_perc <- 0.893
    } 
    hjust_eff_lab4 <- 0.54
  }
  
  # Determine color based on net rating:
  net_rtg <- data_player_eff$net_rtg
  
  color_net <- if (net_rtg > 3) {
    "darkgreen"
  } else if (net_rtg < -3) {
    "red"
  } else {
    "gold"
  }
  
  # Build title dynamically from external prefix
  tit_pl <- sprintf("%s <span style='font-size:%spx;color:%s;'>%s</span>", eff_lab6, size_netrtg, color_net, net_rtg)
  
  eff_plot <- ggplot(data_player_eff) +
    annotate("text", x = xjust_vt[1], y = 0.72, size = size_eff, hjust = hjust_perc,
             label = paste0(eff_lab1, data_player_eff$win_perc)) +
    annotate("text", x = xjust_vt[2], y = 0.6, size = size_eff, hjust = 0.549,
             label = paste0(eff_lab2, data_player_eff$pts_poss, eff_lab3)) +
    annotate("text", x = xjust_vt[2], y = 0.48, size = size_eff, hjust = hjust_eff_lab4,
             label = paste0(eff_lab4, data_player_eff$pts_poss_opp, eff_lab5)) +
    xlim(0, 1) + 
    ylim(0.4, 0.8) +
    labs(title = tit_pl) +
    theme_void() +
    theme(
      plot.title = ggtext::element_markdown(face = "bold", size = size_plot_tit, hjust = hjust_title),
      plot.margin = margin(-1, 0, 0, 0)
    )
  
  # CONTEXT:
  data_pl_co <- data_pl %>%
    filter(type == "context") %>%
    arrange(desc(value)) %>%
    mutate(category = factor(category, levels = category))
  
  table_plot_context <- ggplot(data_pl_co, aes(x = category, y = 1)) +
    geom_tile(aes(fill = value), color = "white", width = 0.9, height = 0.9) +
    geom_text(aes(label = text_plot), size = size_cont_us[1]) +
    geom_text(aes(y = 1.58, label = category), size = size_cont_us[2], fontface = "bold") +
    scale_fill_gradientn(colors = c("red", "yellow", "green")) +
    labs(title = con_lab1) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = size_plot_tit),
      plot.margin = margin(-1, 0, 0, 0)
    )
  
  # USAGE:
  data_pl_us <- data_pl %>%
    filter(type == "usage") %>%
    arrange(desc(value)) %>%
    mutate(category = factor(category, levels = category))
  
  table_plot_usage <- ggplot(data_pl_us, aes(x = category, y = 1)) +
    geom_tile(aes(fill = value), color = "white", width = 0.9, height = 0.9) +
    geom_text(aes(label = paste(value, "%")), size = size_cont_us[1]) +
    geom_text(aes(y = 1.58, label = category), size = size_cont_us[2], fontface = "bold") +
    scale_fill_gradientn(colors = c("red", "yellow", "green")) +
    labs(title = usa_lab1) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = size_plot_tit),
          plot.margin = margin(-1, 0, 0, 0)
    )
  
  # Final plot:
  final_plot <- cowplot::plot_grid(header_plot, eff_plot, table_plot_context, table_plot_usage, 
                                   ncol = 1, 
                                   rel_heights = c(0.35, 0.3, 0.35, 0.5), scale = 0.9)

  return(final_plot)
}
