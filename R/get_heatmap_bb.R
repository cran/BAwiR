#' Basketball heatmap
#' 
#' @aliases get_heatmap_bb
#'
#' @description 
#' The heatmap created with this function allows the user to easily represent
#' the stats for each player. The more intense the color, the more the player
#' highlights in the statistic considered. The plot can be ordered by any
#' statistic. The offensive statistics are grouped in red, the defensive in green, 
#' the rest in purple and the advanced in pink.
#' 
#' @usage 
#' get_heatmap_bb(df_stats, team, levels_stats = NULL, stat_ord, base_size = 9, title) 
#' 
#' @param df_stats Data frame with the statistics.
#' @param team Team.
#' @param levels_stats Statistics classified in several categories to plot.
#' If this is NULL, all the statistics are included in the data frame. Otherwise,
#' the user can define a vector with the variables to represent.
#' @param stat_ord To sort the heatmap on one particular statistic.
#' @param base_size Sets the font size in the theme used. Default 9.
#' @param title Plot title.
#' 
#' @return 
#' Graphical device
#' 
#' @author 
#' This function has been created using the code of these websites:
#' \url{https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/} and 
#' \url{http://stackoverflow.com/questions/13016022/ggplot2-heatmaps-using-different-gradients-for-categories/13016912}
#' 
#' @examples 
#' \dontrun{
#' compet <- "ACB"
#' df <- do_join_games_bio(compet, acb_games_1718, acb_players_1718)
#' df1 <- do_add_adv_stats(df)
#' df2 <- do_stats(df1, "Total", "2017-2018", compet, "Regular Season")
#' teams <- as.character(rev(sort(unique(df2$Team))))
#' get_heatmap_bb(df2, teams[6], NULL, "MP", 9, paste(compet, "2017-2018", "Total", sep = " "))
#' }
#' 
#' @importFrom plyr ddply
#' @importFrom dplyr select_if ungroup arrange desc
#' @importFrom stats reorder
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradientn scale_x_discrete scale_y_discrete
#' @importFrom ggplot2 element_blank element_text theme_grey theme geom_text ggtitle
#' @importFrom scales rescale
#'
#' @export

get_heatmap_bb <- function(df_stats, team, levels_stats = NULL, stat_ord, base_size = 9, title){ 
  Team <- CombinID <- Nationality <- Season <- Compet <- NULL
  Type_season <- Type_stats <- Month <- MP <- NULL
  value <- variable2 <- Name <- rescaleoffset <- NULL

  df <- df_stats %>% 
    filter(Team == team) %>%
    ungroup(CombinID) %>%
    select(-c(Team, CombinID, Position, Nationality, Season, Compet, Type_season, Type_stats)) #%>%
    #arrange(desc(MP))
  df_order <- data.frame(df)
  df_order1 <- df_order[order(df_order[, stat_ord], decreasing = TRUE), ]
  
  if (is.null(levels_stats)) {
    levels_stats <- list("Offensive" = c("PTS", "FG", "FGA", "FGPerc", 
                                         "TwoP", "TwoPA", "TwoPPerc", 
                                         "ThreeP", "ThreePA", "ThreePPerc", 
                                         "FT", "FTA", "FTPerc", 
                                         "ORB", "AST", "TOV", "Counteratt", 
                                         "BLKag", "Dunks", "PFrv"),
                         "Defensive" = c("DRB", "STL", "BLKfv", "PF"),
                         "Other" = c("GP", "GS", "MP", "TRB", "PlusMinus", "PIR"),
                         "Advanced" = c("GameSc", "PIE", "EFGPerc", "ThreeRate", "FRate", 
                                        "STL_TOV", "AST_TOV", "PPS", "OE", "EPS"))
  }else{
    levels_stats <- levels_stats
    df_order1 <- df_order1[, c("Name", unlist(levels_stats))]
  }
  
  is_zero <- function(x) !all(x == 0) # To remove the columns that only contain zeros.
  
  df1 <- df_order1 %>% 
    select_if(is_zero) #%>%
  #select(Name, unlist(levels_stats))
  df.m <- melt(df1)
  df.s <- ddply(df.m, ~variable, transform, rescale = scale(value))  
  # This is needed when some column has the same value for all the players, 
  # so the column rescale has NaN values.
  nas_rescale <- which(is.na(df.s$rescale))
  if (length(nas_rescale ) != 0) {
    df.s$rescale[nas_rescale] <- 0 
  }
  
  df.s$Category <- df.s$variable
  levels(df.s$Category) <- levels_stats
  
  df.s$rescaleoffset <- df.s$rescale + 100 * (as.numeric(df.s$Category) - 1)
  scalerange <- range(df.s$rescale)
  gradientends <- scalerange + rep(c(0, 100, 200, 300), each = 2)
  colorends <- c("white", "red", "white", "green", "white", "blue", "white", "pink")
    
  df.s$variable2 <- reorder(df.s$variable, as.numeric(df.s$Category))
  df.s$Name <- factor(df.s$Name, levels = rev(unique(df.s$Name)))
  
  gg <- ggplot(df.s, aes(variable2, Name)) + 
    geom_tile(aes(fill = rescaleoffset), colour = "white") + 
    scale_fill_gradientn(colours = colorends, values = rescale(gradientends)) + 
    scale_x_discrete("", expand = c(0, 0)) + 
    scale_y_discrete("", expand = c(0, 0)) + 
    theme_grey(base_size = base_size) + 
    theme(legend.position = "none",
          axis.ticks = element_blank(), 
          axis.text.x = element_text(angle = 300, hjust = 0, size = 6)) +
    # To add the values to each tile:
    geom_text(aes(label = value), size = 2.8) +
    ggtitle(paste(capit_two_words(team), title, sep = " "))
  
  return(gg)
}