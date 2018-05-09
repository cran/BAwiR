#' ACB population pyramid
#' 
#' @aliases get_pop_pyramid_acb
#'
#' @description 
#' This is the code to get a population pyramid with the number of both Spanish 
#' and foreigner players along the ACB seasons. This aids in discussion of nationality 
#' imbalance.
#' 
#' @usage get_pop_pyramid_acb(df)
#' 
#' @param df Data frame that contains the ACB players' nationality.
#' 
#' @return 
#' Graphical device
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#'  # Load the data_app_acb file with the ACB games 
#'  # from seasons 1985-1986 to 2017-2018:
#'  load(url("http://www.uv.es/vivigui/softw/data_app_acb.RData"))
#'  get_pop_pyramid_acb(data_app_acb)
#' }
#' 
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 guide_legend
#' @importFrom dplyr count
#'
#' @export

get_pop_pyramid_acb <- function(df) {
  Nationality <- Nationality_1 <- NULL
  Number <- Player.x <- Season <- NULL
  
  df1 <- df %>%
    mutate(Nationality_1 = ifelse(Nationality == "Spain", "Spanish", "Foreigner")) %>%
    distinct(Player.x, Season, Nationality_1) %>%
    group_by(Season) %>%
    count(Nationality_1) %>%
    mutate(Number = ifelse(Nationality_1 == "Foreigner", -n, n)) %>%
    #rename(Number = n) %>%
    select(Season, Nationality_1, Number)
  df1$Season <- as.factor(df1$Season)
  
  tit <- " Number of Spanish and foreign players along the ACB seasons \n Data from www.acb.com"
  g1 <- ggplot(data = df1, aes(x = Season, fill = Nationality_1, group = Nationality_1)) +
    geom_bar(aes(y = Number), stat = "identity") + #, subset(df1, df1$Nationality_1 == "Spanish")) +
    #geom_bar(aes(y = Number), stat = "identity", subset(df1, df1$Nationality_1 == "Foreigner")) +
    geom_text(aes(y = Number, label = Number), 
              subset(df1, df1$Nationality_1 == "Spanish"), 
              size = 4, hjust = -0.1) +
    geom_text(aes(y = Number, label = Number * (-1)), 
              subset(df1, df1$Nationality_1 == "Foreigner"), 
              size = 4, hjust = -0.1) +
    coord_flip() +
    labs(x = "Season\n", y = "Number of players", 
         title = tit, 
         fill = guide_legend(title = "")) + 
    scale_y_continuous(breaks = seq(-270, 270, 30), labels = abs(seq(-270, 270, 30))) +
    scale_x_discrete(limits = rev(levels(df1$Season))) + 
    theme_few(base_size = 14)
  
  return(g1)
}