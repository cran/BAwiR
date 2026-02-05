#' Four factors plot
#' 
#' @aliases get_four_factors_plot
#'
#' @description 
#' Once computed the team's factors and its rankings with 
#' \code{\link{do_four_factors_df}}, this function represents them.
#' 
#' @usage get_four_factors_plot(df_rank, df_no_rank, team, language, scope = "def_off")
#' 
#' @param df_rank Data frame with the team's offense and 
#' defense four factors and its ranking labels.
#' @param df_no_rank Data frame with the team's offense and 
#' defense four factors.
#' @param team Team name. Multiple teams can be chosen.
#' @param language Language labels. Current options are 'en' for English
#' and 'es' for Spanish.
#' @param scope Plot both defense and offense or just one of them. 
#' Options are "def_off", "def", "off".
#' 
#' @return 
#' Graphical device.
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
#' 
#' df1 <- do_add_adv_stats(df)
#' 
#' team <- "Valencia"
#' 
#' df_four_factors <- do_four_factors_df(df1, team, NULL)
#' 
#' # If only one team is represented the ranking between parentheses is just one.
#' get_four_factors_plot(df_four_factors$df_rank, df_four_factors$df_no_rank, team, "en")
#' 
#' # Example with only defense:
#' get_four_factors_plot(df_four_factors$df_rank, df_four_factors$df_no_rank, team, 
#'                       "en", "def") + 
#'  ggplot2::theme(legend.position = "none")
#' }
#' 
#' @importFrom ggplot2 facet_wrap labs scale_color_grey guides
#'
#' @export

get_four_factors_plot <- function(df_rank, df_no_rank, team, language, scope = "def_off") {
  Team <- Type <- value <- variable <- descr <- NULL
  
  if (scope == "def") {
    df_rank <- df_rank %>%
      filter(Type == "Defense")
    
    df_no_rank <- df_no_rank %>%
      filter(Type == "Defense")
  }else if (scope == "off") {
    df_rank <- df_rank %>%
      filter(Type == "Offense")
    
    df_no_rank <- df_no_rank %>%
      filter(Type == "Offense")
  }
  
  # Set the same order by teams' name:
  df_rank <- df_rank %>% 
    arrange(Team)
  df_no_rank <- df_no_rank %>% 
    arrange(Team)
  
  # Data frame with ranking:
  df_rank1 <- df_rank %>%
    filter(Team %in% team) 
  
  df_rank2 <- melt(df_rank1, id = c("Team", "Type"))
  levels(df_rank2$variable) <- c("EFG%", "TOV%", "ORB%", "FTR%")
  
  # Data frame without ranking:
  df_no_rank1 <- df_no_rank %>%
    filter(Team %in% team)
  
  df_no_rank2 <- melt(df_no_rank1, id = c("Team", "Type"))
  levels(df_no_rank2$variable) <- c("EFG%", "TOV%", "ORB%", "FTR%")
  
  df_no_rank21 <- df_no_rank2[do.call("order", c(df_no_rank2["Type"], list(decreasing = FALSE))),]
  df_rank21 <- df_rank2[do.call("order", c(df_rank2["Type"], list(decreasing = FALSE))),]
  
  if (scope == "def_off") {
    rep_text <- 2
  }else if (scope == "def" | scope == "off") {
    rep_text <- 1
  }
  
  if (language == "en") {
    descr_stats <- rep(c(rep("Effective field goal percentage", length(team)), 
                         rep("Turnover percentage", length(team)),
                         rep("Offensive rebound percentage", length(team)), 
                         rep("Free throws per field goal attempted", length(team))), rep_text)
    
    subtitle_plot <- "Team ranking for each factor between parentheses"  
    
    df_no_rank21$Type <- factor(df_no_rank21$Type)
    
    if (scope == "def_off") {
      levels(df_no_rank21$Type) <- c("Defense", "Offense")
    }else if (scope == "def") {
      levels(df_no_rank21$Type) <- "Defense"
    }else if (scope == "off") {
      levels(df_no_rank21$Type) <- "Offense"
    }
  }else if (language == "es") {
    descr_stats <- rep(c(rep("Porcentaje efectivo en tiros de campo", length(team)), 
                         rep("Porcentaje de balones perdidos", length(team)),
                         rep("Porcentaje de rebotes ofensivos", length(team)), 
                         rep("Tiros libres anotados por cada tiro de campo intentado", length(team))), rep_text)
    
    subtitle_plot <- "Ranking del equipo en cada factor entre parentesis" 
    
    df_no_rank21$Type <- factor(df_no_rank21$Type)
    
    if (scope == "def_off") {
      levels(df_no_rank21$Type) <- c("Defensa", "Ataque")
    }else if (scope == "def") {
      levels(df_no_rank21$Type) <- "Defensa"
    }else if (scope == "off") {
      levels(df_no_rank21$Type) <- "Ataque"
    }
   }  
  
  df_no_rank21$descr <- descr_stats
  labs <- with(df_no_rank21, paste(variable, descr, sep = ": "))
  
  if (length(team) <= 2) {
    size_text <- 7
    axis_text <- 15
  }else if (length(team) == 3){
    size_text <- 4
    axis_text <- 15
  }else if (length(team) == 4){
    size_text <- 3
    axis_text <- 12
  }else{
    size_text <- 3
    axis_text <- 10
  }
  
  gg <- ggplot(df_no_rank21, aes(x = variable, y = value, col = descr, fill = Type)) +
    facet_grid(Type~Team, scales = "free_y") +
    scale_color_grey(labels = unique(labs), end = 0) 
  
  if (scope == "def_off") {
    gg <- gg + 
      geom_bar(stat = "identity")  
  }else if (scope == "def") {
    gg <- gg + 
      geom_bar(stat = "identity", fill = "#F8766D")  
  }else if (scope == "off") {
    gg <- gg + 
      geom_bar(stat = "identity", fill = "#00BFC4")  
  }
  
  gg <- gg +
    geom_point(size = 0) +
    guides(colour = guide_legend(override.aes = list(size = 10))) +
    labs(x = "", y = "", fill = "", col = "", subtitle = subtitle_plot) +
    ylim(c(0, max(df_no_rank21$value) + 10)) +
    geom_text(aes(label = df_rank21$value), vjust = -1, size = size_text) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 18),
          axis.text = element_text(size = axis_text))
  
  return(gg)
}
