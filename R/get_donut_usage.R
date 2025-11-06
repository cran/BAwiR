#' Donut chart usage possessions
#' 
#' @aliases get_donut_usage
#'
#' @description 
#' This function creates a donut chart that displays the percentage of 
#' possessions that each player of a team ends while being on the court.
#' 
#' @usage 
#' get_donut_usage(data_usage, team_sel, size_play, size_perc)
#' 
#' @param data_usage Data frame with the number of possessions that each 
#' player played and the number that he ended.
#' @param team_sel String with the team's full name.
#' @param size_play Size of the players' labels.
#' @param size_perc Size of the percentages labels.
#' 
#' @return 
#' A donut chart.
#' 
#' @details
#' Donut charts are an alternative for pie charts, which have a hole 
#' in the middle, making them cleaner to read than pie charts. 
#'
#' @author 
#' Guillermo Vinue with the help of ChatGPT.
#' 
#' @seealso 
#' \code{\link{get_donut_usage_action}}
#' 
#' @examples 
#' \dontrun{
#' get_donut_usage(acb_usage_data_2526, "Valencia Basket", 3, 4)
#' }
#' 
#' @importFrom ggplot2 theme_void scale_fill_gradient
#'
#' @export

get_donut_usage <- function(data_usage, team_sel, size_play, size_perc) {
  season <- team <- player <- poss_end <- poss_num <- usage_perc <- NULL
  cumulative <- label_pos <- usage_perc_label <- NULL

  df0 <- data_usage %>%
    filter(team == team_sel) %>%
    group_by(player) %>%
    summarise(poss_end = sum(poss_end), poss_num = sum(poss_num)) %>%
    ungroup() %>%
    mutate(usage_perc = round((poss_end / poss_num) *100, 2))
  
  df1 <- df0 %>%
    arrange(desc(usage_perc)) %>%
    mutate(
      cumulative = cumsum(usage_perc),
      label_pos = cumulative - usage_perc / 2
    ) %>%
    mutate(usage_perc_label = paste0(usage_perc, "%\n(", poss_end, "/", poss_num, ")")) %>%
    mutate(player = gsub(".*\\. ", "", player))
  
  gg <- ggplot(df1, aes(x = 2, y = usage_perc, fill = usage_perc)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    theme_void() +
    scale_fill_gradient(low = "red", high = "green") +
    # Category (smaller)
    geom_text(aes(y = label_pos, label = player), color = "black", size = size_play, vjust = -3) +
    # Percentage (larger)
    geom_text(aes(y = label_pos, label = usage_perc_label), color = "black", size = size_perc) +
    theme(legend.position = "none")
  
  return(gg)
}
