#' Four factors plot
#' 
#' @aliases get_four_factors_plot
#'
#' @description 
#' Once computed the team's factors and its rankings with 
#' \code{\link{do_four_factors_df}}, this function represents them.
#' 
#' @usage get_four_factors_plot(df_rank, df_no_rank, team, language)
#' 
#' @param df_rank Data frame with the team's offense and 
#' defense four factors and its ranking labels.
#' @param df_no_rank Data frame with the team's offense and 
#' defense four factors.
#' @param team Team name.
#' @param language Language labels. Current options are 'en' for english
#' and 'es' for spanish.
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
#' get_four_factors_plot(df_four_factors$df_rank, 
#'                       df_four_factors$df_no_rank, team, "en")
#' }
#' 
#' @importFrom ggplot2 facet_wrap labs scale_color_grey guides
#'
#' @export

get_four_factors_plot <- function(df_rank, df_no_rank, team, language) {
  Team <- Type <- value <- variable <- descr <- NULL
  
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
  
  df_no_rank21 <- df_no_rank2[order(df_no_rank2$Type),]
  df_rank21 <- df_rank2[order(df_rank2$Type),]
  
  if (language == "en") {
    descr_stats <- rep(c("Effective field goal percentage", "Turnover percentage", 
                         "Offensive rebound percentage", "Free throws per field goal attempted"), 2)
    subtitle_plot <- "Team ranking for each factor between parentheses"  
  }else if (language == "es") {
    descr_stats <- rep(c("Porcentaje efectivo en tiros de campo", "Porcentaje de balones perdidos", 
                         "Porcentaje de rebotes ofensivos", 
                         "Tiros libres anotados por cada tiro de campo intentado"), 2)
    subtitle_plot <- "Ranking del equipo en cada factor entre parentesis" 
    df_no_rank21$Type <- factor(df_no_rank21$Type)
    levels(df_no_rank21$Type) <- c("Defensa", "Ataque")
   }  
  
  
  df_no_rank21$descr <- descr_stats
  labs <- with(df_no_rank21, paste(variable, descr, sep = ": "))
  
  gg <- ggplot(df_no_rank21, aes(x = variable, y = value, col = descr, fill = Type)) +
    facet_wrap(~Type, scales = "free_y", ncol = 2) +
    scale_color_grey(labels = labs, end = 0) +
    geom_bar(stat = "identity") +
    geom_point(size = 0) +
    guides(colour = guide_legend(override.aes = list(size = 10))) +
    labs(x = "", y = "", title = team, fill = "", col = "",
         subtitle = subtitle_plot) +
    ylim(c(0, max(df_no_rank21$value) + 10)) +
    geom_text(aes(label = df_rank21$value), vjust = -1, size = 7) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 12))
  
  return(gg)
}
