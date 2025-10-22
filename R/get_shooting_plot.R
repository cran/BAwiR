#' Shooting plot
#' 
#' @aliases get_shooting_plot
#'
#' @description 
#' This plot represents the number of shots attempted and scored by every 
#' player of the same team, together with the scoring percentage. 
#' The players are sortered by percentage.
#' 
#' @usage get_shooting_plot(df_stats, team, type_shot, min_att, title, language, 
#'                          size_summ = 5, size_add = 16)
#' 
#' @param df_stats Data frame with the statistics.
#' @param team Team.
#' @param type_shot Numeric with values 1-2-3: 1 refers to free throws,
#' 2 refers to two point shots and 3 refers to three points shots. 
#' @param min_att Minimum number of attempts by the player to
#' be represented in the plot.
#' @param title Plot title.
#' @param language Language labels. Current options are 'en' for English
#' and 'es' for Spanish.
#' @param size_summ Size of the text summarizing the total shots and the percentage.
#' @param size_add Size of the additional axis and legends.
#' 
#' @return 
#' Graphical device.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' compet <- "ACB"
#' df <- do_join_games_bio(compet, acb_games_1718, acb_players_1718)
#' df1 <- do_add_adv_stats(df)
#' df2 <- do_stats(df1, "Total", "2017-2018", compet, "Regular Season")
#' get_shooting_plot(df2, "Valencia", 3, 1, paste("Valencia", compet, "2017-2018", sep = " "), "en")
#' }
#' 
#' @importFrom ggplot2 geom_segment theme_minimal scale_x_continuous
#' @importFrom stats setNames
#'
#' @export

get_shooting_plot <- function(df_stats, team, type_shot, min_att, title, language, size_summ = 5, size_add = 16){
  Team <- Name <- FT <- FTA <- TwoP <- TwoPA <- ThreeP <- ThreePA <- NULL
  total_att <- total_sco <- perc_sco <- perc_no_sco <- total_no_sco <- NULL
  
  if (type_shot == 1) {
    df1 <- df_stats %>% 
      ungroup() %>%
      filter(Team == team) %>%
      select(Name, FT, FTA) %>%
      group_by(Name) %>%
      summarise(total_att = sum(FTA), total_sco = sum(FT)) 
    if (language == "en") {
      color1 <- "Free throws scored"
      color2 <- "Free throws missed"
    }else if (language == "es") {
      color1 <- "Tiros libres anotados"
      color2 <- "Tiros libres fallados"
     }  
  }else if (type_shot == 2) {
    df1 <- df_stats %>% 
      ungroup() %>%
      filter(Team == team) %>%
      select(Name, TwoP, TwoPA) %>%
      group_by(Name) %>%
      summarise(total_att = sum(TwoPA), total_sco = sum(TwoP)) 
    if (language == "en") {
      color1 <- "Two-points scored"
      color2 <- "Two-points missed"
    }else if (language == "es") {
      color1 <- "Tiros de dos anotados"
      color2 <- "Tiros de dos fallados"
    }  
  }else if (type_shot == 3) {
    df1 <- df_stats %>% 
      ungroup() %>%
      filter(Team == team) %>%
      select(Name, ThreeP, ThreePA) %>%
      group_by(Name) %>%
      summarise(total_att = sum(ThreePA), total_sco = sum(ThreeP)) 
    if (language == "en") {
      color1 <- "Three-points scored"
      color2 <- "Three-points missed"
    }else if (language == "es") {
      color1 <- "Tiros de tres anotados"
      color2 <- "Tiros de tres fallados"
    } 
  }
  
  df_tm <- df1 %>%
    mutate(total_no_sco = total_att - total_sco) %>%
    mutate(perc_sco = round(ifelse(total_att == 0, 0, (total_sco / total_att) * 100), 1)) %>%
    mutate(perc_no_sco = round(100 - perc_sco, 1)) %>%
    filter(total_att >= min_att) %>%
    arrange(perc_sco)
  
  df_tm$Name <- factor(df_tm$Name, levels = df_tm$Name)
  
  gg <- ggplot(df_tm) +
    geom_segment(aes(0, Name, xend = perc_sco, yend = Name, color = color1), linewidth = 13) +
    geom_segment(aes(perc_sco, Name, xend = perc_sco + perc_no_sco, yend = Name, color = color2), linewidth = 13) +
    geom_text(aes(x = 1, y = Name, label = total_sco), hjust = 0, nudge_x = 0.01, size = 7) +
    geom_text(aes(x = 99, y = Name, label = total_no_sco), hjust = 1, nudge_x = -0.01, size = 7) +
    geom_text(aes(x = 104, y = Name, label = total_att), hjust = 1, nudge_x = -0.01, nudge_y = 0.14, size = size_summ) +
    geom_text(aes(x = 110, y = Name, label = perc_sco), hjust = 1, nudge_x = -0.01, nudge_y = 0.14, size = size_summ) +
    labs(x = NULL, y = NULL, color = "") +
    scale_x_continuous(breaks = seq(0, 100, 25), labels = c("0%", "25%", "50%", "75%", "100%")) +
    scale_color_manual(breaks = c(color1, color2), values = setNames(c("#00BFC4", "#F8766D"), c(color1, color2))) +
    #@importFrom hrbrthemes scale_x_percent scale_color_ipsum
    #scale_x_percent(breaks = seq(0,100,25), labels = c("0%", "25%", "50%", "75%", "100%")) +
    #scale_color_ipsum(name = NULL) + 
    theme_minimal(base_size = 17) +
    theme(#axis.text.x = element_text(hjust = c(0, 0.5, 0.5, 0.5, 1)),
          legend.position = c(0.7, 1.025),
          legend.direction = "horizontal",
          #legend.title = element_blank(),
          axis.text = element_text(size = size_add),
          legend.text = element_text(size = size_add)) +
    ggtitle(paste(capit_two_words(team), title, sep = " "))
  
  return(gg)  
}  