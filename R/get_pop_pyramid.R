#' Population pyramid
#' 
#' @aliases get_pop_pyramid
#'
#' @description 
#' This is the code to get a population pyramid with the number of both Spanish 
#' and foreigner players along the seasons for the ACB league. 
#' This aids in discussion of nationality imbalance.
#' 
#' @usage get_pop_pyramid(df, title, language)
#' 
#' @param df Data frame that contains the ACB players' nationality.
#' @param title Title of the plot
#' @param language String, "eng" for English labels; "esp" for Spanish labels.
#' 
#' @return 
#' Graphical device.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#'  # Load the data_app_acb file with the ACB games 
#'  # from seasons 1985-1986 to 2017-2018:
#'  load(url("http://www.uv.es/vivigui/softw/data_app_acb.RData"))
#'  title <- " Number of Spanish and foreign players along the ACB seasons \n Data from www.acb.com"
#'  get_pop_pyramid(data_app_acb, title, "eng")
#' }
#' 
#' @importFrom ggthemes theme_few
#' @importFrom ggplot2 guide_legend
#' @importFrom dplyr count
#'
#' @export

get_pop_pyramid <- function(df, title, language) {
  Nationality <- Nationality_1 <- NULL
  Number <- Player.x <- Season <- NULL
  
  if (language == "eng") {
    cat1 <- "Spanish"
    cat2 <- "Foreigner"
    xlab <- "Season\n"
    ylab <- "Number of players"
  }else if (language == "esp") {
    cat1 <- "Espana"
    cat2 <- "Extranjero"
    xlab <- "Temporada\n"
    ylab <- "Numero de jugadores"
  }else{
    stop("Language must be 'eng' for English and 'esp' for Spanish.")
  }
  
  df1 <- df %>%
    mutate(Nationality_1 = ifelse(Nationality == "Spain", cat1, cat2)) %>%
    distinct(Player.x, Season, Nationality_1) %>%
    group_by(Season) %>%
    count(Nationality_1) %>%
    mutate(Number = ifelse(Nationality_1 == cat2, -n, n)) %>%
    #rename(Number = n) %>%
    select(Season, Nationality_1, Number)
  df1$Season <- as.factor(df1$Season)
  
  g1 <- ggplot(data = df1, aes(x = Season, fill = Nationality_1, group = Nationality_1)) +
    geom_bar(aes(y = Number), stat = "identity") + #, subset(df1, df1$Nationality_1 == "Spanish")) +
    #geom_bar(aes(y = Number), stat = "identity", subset(df1, df1$Nationality_1 == "Foreigner")) +
    geom_text(aes(y = Number, label = Number), 
              subset(df1, df1$Nationality_1 == cat1), 
              size = 4, hjust = -0.1) +
    geom_text(aes(y = Number, label = Number * (-1)), 
              subset(df1, df1$Nationality_1 == cat2), 
              size = 4, hjust = -0.1) +
    coord_flip() +
    labs(x = xlab, y = ylab, 
         title = title, 
         fill = guide_legend(title = "")) + 
    scale_y_continuous(breaks = seq(-270, 270, 30), labels = abs(seq(-270, 270, 30))) +
    scale_x_discrete(limits = rev(levels(df1$Season))) + 
    theme_few(base_size = 14)
  
  return(g1)
}