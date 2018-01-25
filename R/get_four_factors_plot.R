#' Four factors plot
#' 
#' @aliases get_four_factors_plot
#'
#' @description 
#' Once computed the team's factors and its rankings with 
#' \code{\link{do_four_factors_df}}, this function represents them.
#' 
#' @usage get_four_factors_plot(df_rank, df_no_rank, team)
#' 
#' @param df_rank Data frame with the team's offense and 
#' defense four factors and its ranking labels.
#' @param df_no_rank Data frame with the team's offense and 
#' defense four factors.
#' @param team Team name.
#' 
#' @return 
#' Graphical device
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_four_factors_df}} 
#' 
#' @examples 
#' \dontrun{
#' df <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718)
#' df1 <- do_add_adv_stats(df)
#' team <- "Valencia"
#' df_four_factors <- do_four_factors_df(df1, team)
#' # If only one team is represented the ranking between parentheses is just one.
#' get_four_factors_plot(df_four_factors$df_rank, df_four_factors$df_no_rank, team)
#' }
#' 
#' @importFrom ggplot2 facet_wrap labs
#'
#' @export

get_four_factors_plot <- function(df_rank, df_no_rank, team) {
  Team <- Type <- value <- NULL
  
  # Data frame with ranking:
  df_rank1 <- df_rank %>%
    filter(Team == team) %>%
    select(-Team)
  
  df_rank2 <- melt(df_rank1, id = "Type")
  levels(df_rank2$variable) <- c("EFG%", "TOV%", "ORB%", "FTR")
  
  # Data frame without ranking:
  df_no_rank1 <- df_no_rank %>%
    filter(Team == team) %>%
    select(-Team)
  
  df_no_rank2 <- melt(df_no_rank1, id = "Type")
  levels(df_no_rank2$variable) <- c("EFG%", "TOV%", "ORB%", "FTR")
  
  gg <- ggplot(df_no_rank2, aes(x = Type, y = value, fill = Type)) +
    facet_wrap(~variable, scales = "free_y", ncol = 2) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "", title = team, 
         subtitle = "Team ranking for each factor between parentheses") +
    geom_text(aes(label = df_rank2$value), vjust = 5, size = 4) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12))
  
  return(gg)
}
