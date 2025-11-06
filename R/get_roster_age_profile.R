#' Roster age profile
#' 
#' @aliases get_roster_age_profile
#'
#' @description 
#' For the players of the same team, show their age at time of joining the team,
#' their current year and how many years they have spent in the team.
#' 
#' @usage get_roster_age_profile(data_age_team, team_sel, language)
#' 
#' @param data_age_team Data frame with the team's age profile.
#' @param team_sel Team.
#' @param language Language labels. Current options are 'en' for English
#' and 'es' for Spanish.
#' 
#' @return 
#' Graphical device.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' get_roster_age_profile(acb_age_profile_data_2526, "Valencia Basket", "es")
#' }
#' 
#' @importFrom ggplot2 annotation_custom arrow
#' @importFrom grid rasterGrob
#' @importFrom ggpubr ggarrange
#'
#' @export

get_roster_age_profile <- function(data_age_team, team_sel, language) {
  age_current <- age_join <- games <- minutes <- NULL
  player <- team_name <- team_shield <- value <- NULL
  
  
  data_team <- data_age_team %>%
    filter(team_name == team_sel) %>%
    filter(!is.na(age_join))
  
  data_team1 <- data_team %>%
    select(-minutes, -team_name, -team_shield) %>%
    pivot_longer(!c(player, games), names_to = "variable", values_to = "value") 
  
  xlims <- c(floor(range(data_team$age_join)[1]), ceiling(range(data_team$age_join)[2]) + 1)
  
  if (language == "en") {
    ann_lab1 <- "Age at time of joining club"
    ann_lab2 <- "Current age"
    x_lab <- "Current age"
    y_lab <- "Games"
    title_lab <- paste0(team_sel, ". Roster age profile")
    label_info1 <- "Number of players:"
    label_info2 <- "Average age:"
    label_info3 <- "Average duration of stay"
    lab_yr <- "years"
  }else if (language == "es") {
    ann_lab1 <- "edad de incorporaci\u00f3n"
    ann_lab2 <- "edad actual"
    x_lab <- "Edad actual"
    y_lab <- "Partidos"
    title_lab <- paste0(team_sel, ". Perfil de edades en la plantilla")
    label_info1 <- "N\u00famero de jugadores:"
    label_info2 <- "Edad media:"
    label_info3 <- "Permanencia media:"
    lab_yr <- "a\u00f1os"
  }
  
  max_y <- max(data_team$games)
  upper_limit <- ceiling(max_y / 50) * 50  # next multiple of 50
  
  max_game <- which.max(data_team$games)
  
  gg1 <- ggplot(data_team, aes(x = age_current, y = games, color = "red")) +
    geom_point(aes(x = age_current), size = 3) +
    geom_line(data_team1, mapping = aes(x = value, y = games, group = player), linewidth = 1.2) +
    ggrepel::geom_label_repel(aes(label = player), color = "black", max.overlaps = 20, label.size = 0.15) +
    scale_x_continuous(breaks = seq(xlims[1], xlims[2], 2)) +
    scale_y_continuous(limits = c(0, upper_limit), breaks = seq(0, upper_limit, by = 50)) +
    annotate("segment", 
             x = data_team[max_game, "age_join"]$age_join, 
             y = data_team[max_game, "games"]$games, 
             xend = data_team[max_game, "age_join"]$age_join, 
             yend = ceiling(data_team[max_game, "games"]$games / 10) * 10 + 7,
             arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
    annotate("segment", 
             x = data_team[max_game, "age_current"]$age_current, 
             y = data_team[max_game, "games"]$games, 
             xend = data_team[max_game, "age_current"]$age_current, 
             yend = ceiling(data_team[max_game, "games"]$games / 10) * 10 + 7,
             arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
    annotate("text", 
             x = data_team[max_game, "age_join"]$age_join, 
             y = ceiling(data_team[max_game, "games"]$games / 10) * 10 + 10,
             label = ann_lab1, size = 4, color = "red") +
    annotate("text", 
             x = data_team[max_game, "age_current"]$age_current, 
             y = ceiling(data_team[max_game, "games"]$games / 10) * 10 + 10, 
             label = ann_lab2, size = 4, color = "red") +
    labs(x = x_lab, 
         y = y_lab,
         title = title_lab) + 
    theme_minimal() + 
    theme(legend.position = "none",
          plot.title = ggtext::element_markdown(face = "bold", size = 17),
          plot.subtitle = element_text(face = "bold", size = 15),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 16))
  
  label_info <- paste0(paste(label_info1, nrow(data_team)), "\n",
                       paste(label_info2, round(mean(data_team$age_current), 2), lab_yr), "\n",
                       paste(label_info3, round(mean(data_team$age_current - data_team$age_join), 2), lab_yr))
  
  
  shield_png <- data_age_team %>%
    filter(team_name == team_sel) %>%
    pull(team_shield) %>%
    unique()
  
  img <- png::readPNG(RCurl::getURLContent(shield_png))
  g_img <- rasterGrob(img, interpolate = TRUE)
  
  gg2 <- ggplot() +
    annotate("text", x = 4.003, y = 25.01, size = 5, label = label_info) + 
    annotation_custom(grob = g_img, xmin = 3.99, xmax = 4.02, ymin = 25, ymax = 25.05) +
    theme_void()
  
  gg_def <- ggarrange(gg1, gg2, widths = c(1, 0.3), nrow = 1)
  
  return(gg_def)
}
