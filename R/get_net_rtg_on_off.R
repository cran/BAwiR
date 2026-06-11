#' Net Rating On/Off
#' 
#' @aliases get_net_rtg_on_off
#'
#' @description 
#' The Net Rating On/Off measures a team's net rating when a specific player is on the court versus 
#' when they are on the bench. It highlights the player's impact by comparing the team's efficiency 
#' both in offense and defense. A positive differential indicates a positive, higher-impact player.
#' 
#' @usage 
#' get_net_rtg_on_off(data_combs, team_name, type_lineup, type_period, type_opponent, 
#'                    filter_players, language, asp_ratio = 1)
#' 
#' @param data_combs Data frame with all the combinations of lineups.
#' @param team_name Name of the team.
#' @param type_lineup Type of lineups to analyze. Options are 'quintet', 'quartet', 'trio',
#' 'duo' and 'single'. 
#' @param type_period Period of interest. Options are xC, where x starts from 1. 
#' Common periods are from 1 to 4. Overtimes are labeled with the next numbers, such as 5C 
#' for the first overtime and 6C for the second one. Nothing to do if NULL.
#' @param type_opponent Name of the opponent teams. Nothing to do if NULL.
#' @param filter_players String with the players' names to filter. Nothing to do if NULL.
#' @param language Language of the legends and titles.
#' @param asp_ratio Aspect ratio of the plot. Default 1.
#' 
#' @details
#' \itemize{
#' \item Net Rating: difference between Offensive Rating and Defensive Rating.
#' \item On-Court Rating: team's net rating while the player is in the game.
#' \item Off-Court Rating: team's net rating while the player is on the bench.
#' \item On/Off Differential: difference between the on-court and off-court net ratings, 
#' which gives a first idea of how much better/worse the team performs with that player.
#' }
#' 
#' @return 
#' A plot with the players' net ratings and differentials.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_possession_stats_lineup}}
#' 
#' @examples 
#' \dontrun{
#' get_net_rtg_on_off(acb_combs, "Unicaja", "single", NULL, NULL, NULL, "Spanish")
#' get_net_rtg_on_off(acb_combs, "Unicaja", "single", NULL, NULL, NULL, "English")
#' }
#'
#' @importFrom dplyr if_else
#' @importFrom tidyr complete fill
#' @importFrom ggplot2 position_dodge
#'
#' @export

get_net_rtg_on_off <- function(data_combs, team_name, type_lineup, type_period, type_opponent, 
                               filter_players, language, asp_ratio = 1) {
  team <- lineup_type <- period <- opponent <- season <- day <- game_code <- time_in <- time_out <- NULL
  status <- lineup <- netrtg <- on <- off <- diff <- diff1 <- NULL
  
  # Filter by team and lineup type:
  df0 <- data_combs %>%
    filter(team == team_name) %>%
    filter(lineup_type == type_lineup)
  
  # Filter by period:
  if (!is.null(type_period)) {
    df0 <- df0 %>% 
      filter(period %in% type_period) 
  }
  
  # Filter by opponent:
  if (!is.null(type_opponent)) {
    df0 <- df0 %>% 
      filter(opponent %in% type_opponent) 
  }
  
  team_players <- sort(unique(df0$lineup))
  
  df1 <- df0 %>%
    mutate(status = "on") %>%
    group_by(season, day, game_code, team, opponent, period, time_in, time_out) %>%
    # Complete with all the roster players:
    complete(lineup = team_players) %>%
    mutate(status = if_else(is.na(status), "off", status)) %>%
    fill(-c(season, day, game_code, team, opponent, period, time_in, time_out, lineup, status), .direction = "downup") %>%
    ungroup() %>%
    relocate(status, .after = lineup) %>%
    arrange(season, day, game_code, team, opponent, period, desc(time_in), desc(status))
  
  df2 <- do_possession_stats_lineup(df1, team_name, type_lineup, type_period, type_opponent, c("team", "lineup", "status"))
  
  if (!is.null(filter_players)) {
    df3 <- df2 %>%
      filter(!lineup %in% filter_players) %>%
      select(lineup, status, netrtg)
  }else{
    df3 <- df2 %>%
      select(lineup, status, netrtg)
  }

  # To add the value of net rating:
  df3_diff <- df3 %>%
    pivot_wider(names_from = status, values_from = netrtg) %>%
    mutate(diff = round(on - off, 2)) %>%
    select(lineup, diff)
  
  df4 <- left_join(df3, df3_diff, by = "lineup") 
  
  df4$diff1 <- ifelse(df4$diff >= 3, 
                      paste0("<span style='color:darkgreen;'>", df4$diff, "</span>"), 
                      ifelse(df4$diff < 3 & df4$diff >= -3, 
                             paste0("<span style='color:orange;'>", df4$diff, "</span>"), 
                             paste0("<span style='color:red;'>", df4$diff, "</span>")))
  
  
  df4 <- df4 %>%
    mutate(lineup = paste0(lineup, " (", diff1, ")")) %>%
    arrange(desc(diff), lineup)
  
  df4$lineup <- factor(df4$lineup, levels = rev(unique(df4$lineup)))
  
  if (language == "English") {
    legend_status <- c("On the court", "On the bench")
    plot_tit <- paste0(team_name, "'s net rating when a specific player \n is on the court versus when he is on the bench.")
    plot_subtit <- "The difference between the two values is shown in parentheses for each player."
  }else{
    legend_status <- c("En pista", "En el banquillo")
    plot_tit <- paste0("\u00cdndice neto obtenido por el ", team_name , "\n con cada jugador en pista y en el banquillo.")
    plot_subtit <- "La diferencia entre ambos valores se muestra entre par\u00e9ntesis para cada jugador."
  }
  
  df4 <- df4 %>%
    mutate(status = plyr::mapvalues(status, from = c("on", "off"), to = legend_status))
  
  gg <- ggplot(df4, aes(x = netrtg, y = lineup, fill = status)) +
    geom_col(position = position_dodge(width = 0.5), width = 0.3) +
    geom_text(aes(label = netrtg), position = position_dodge(width = 0.5), vjust = 0.8, hjust = -0.1, size = 3.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_x_continuous(limits = c(-max(abs(df4$netrtg)), max(abs(df4$netrtg)))) 
  
  if (language == "English") {
    gg <- gg +
      scale_fill_manual(values = c("On the court" = "purple",
                                   "On the bench" = "lightblue"), 
                        breaks = legend_status) 
  }else{
    gg <- gg +
      scale_fill_manual(values = c("En pista" = "purple",
                                   "En el banquillo" = "lightblue"), 
                        breaks = legend_status) 
  }
  
  gg <- gg +
    labs(x = "", y = NULL, 
         title = plot_tit,
         subtitle = plot_subtit,
         fill = NULL) +
    theme(axis.text.x = element_text(size = 16), 
          axis.text.y = ggtext::element_markdown(size = 14), 
          aspect.ratio = asp_ratio,
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 15),
          legend.text = element_text(size = 16),
          legend.key.size = unit(2, "cm")) + 
    geom_rect(aes(xmin = -max(abs(netrtg)), xmax = 0, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.002) + 
    geom_rect(aes(xmin = 0, xmax = max(abs(netrtg)), ymin = -Inf, ymax = Inf), fill = "green", alpha = 0.002)
  
  return(gg)
}
