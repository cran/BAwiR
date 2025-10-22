#' Plots of data distributions
#' 
#' @aliases do_violin_box_plots
#'
#' @description 
#' Create violin plots and boxplots to analyze the distribution of the two-point, 
#' three-point and total shots. Violin plots show the distribution shape, while
#' boxplots give a compact statistical summary.
#' 
#' @usage 
#' do_violin_box_plots(data_shots, data_players)
#' 
#' @param data_shots Shooting data frame.
#' @param data_players Players' identifiers data frame.
#' 
#' @return 
#' A plot.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' df0 <- do_divide_court_zones(acb_shooting_data_2425)
#' 
#' df1 <- do_filter_data(df0, "2024-2025", "", "", "", "", "")
#' 
#' do_violin_box_plots(df1, acb_players_2425)
#' }
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom stats quantile median density
#' @importFrom ggplot2 geom_violin stat_boxplot geom_boxplot
#' 
#' @export

do_violin_box_plots <- function(data_shots, data_players) {
  ymin <- lower <- middle <- upper <- ymax <- player_license_id <- shot <- NULL
  
  shots_player <- data_shots %>%
    count(player_license_id, name = "total") %>%
    pivot_longer(!player_license_id, names_to = "shot", values_to = "count") %>%
    bind_rows(data_shots %>% count(player_license_id, shot, name = "count")) %>%
    left_join(data_players, by = "player_license_id") #%>% arrange(player_name)
  
  #shots_player %>%
  #  select(shot, count) %>%
  #  group_by(shot) %>%
  #  summarise(sum(count)) %>%
  #  ungroup()
  
  #pt <- shots_player %>%
  #  select(shot, count) %>%
  #  filter(shot == "2pt") 
  #View(pt %>% arrange(-count))
  #summary(pt$count)
  
  summary_df <- shots_player %>%
    group_by(shot) %>%
    summarise(
      ymin = min(count),
      lower = quantile(count, 0.25),
      middle = median(count),
      upper = quantile(count, 0.75),
      ymax = max(count)
    )
  
  gg <- shots_player %>%
    select(shot, count) %>%
    #ggplot(aes(x = count, color = shot)) +
    #geom_histogram(fill = "white", alpha = 0.5, position = "identity")
    #group_by(shot) %>%
    #top_n(n = 5) %>%
    ggplot(aes(x = count, y = shot, fill = shot)) +
    geom_violin(trim = FALSE, alpha = 0.5) +
    stat_boxplot(geom = "errorbar", width = 0.15) +
    geom_boxplot(width = 0.1, outlier.size = 3) + 
    #geom_text(data = summary_df, aes(x = ymin, y = shot, label = round(ymin, 1)), vjust = 1.5) +
    geom_text(data = summary_df, aes(x = lower, y = shot, label = round(lower, 1)), vjust = -0.7) +
    geom_text(data = summary_df, aes(x = middle, y = shot, label = round(middle, 1)), vjust = -0.7) +
    geom_text(data = summary_df, aes(x = upper, y = shot, label = round(upper, 1)), vjust = -0.7) +
    #geom_text(data = summary_df, aes(x = ymax, y = shot, label = round(ymax, 1)), vjust = -0.7) +
    labs(x = "", y = "") +
    theme(axis.text = element_text(size = 16), 
          legend.position = "none")
  
  return(gg)
}
