## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----packages, message=FALSE, eval=FALSE---------------------------------
#  # Firstly, load BAwiR and other packages that will be used in the paper:
#  library(BAwiR)
#  library(tidyverse)
#  library(Anthropometry)

## ----figure 1, eval=FALSE------------------------------------------------
#  # Code for Figure 1:
#  # Load the data_app_acb file with the ACB games from seasons 1985-1986 to 2017-2018:
#  load(url("http://www.uv.es/vivigui/softw/data_app_acb.RData"))
#  get_pop_pyramid_acb(data_app_acb)

## ----data, message=FALSE, eval=FALSE-------------------------------------
#  # Create the data with games and players' info, add the advanced stats
#  # and compute the total numbers:
#  df0 <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718)
#  df1 <- do_add_adv_stats(df0)
#  df2 <- do_stats(df1, "Total", "2017-2018", "ACB", "Regular Season")

## ----table 2, eval=FALSE-------------------------------------------------
#  # Code for Table 2:
#  df3 <- df2[which(df2$Position == "Center"), c("MP", "PTS", "Name")]
#  preproc <- preprocessing(df3[,1:2], stand = TRUE, percAccomm = 1)
#  set.seed(4321)
#  lass <- stepArchetypesRawData(preproc$data, 1:2, numRep = 20, verbose = FALSE)
#  res <- archetypoids(2, preproc$data, huge = 200, step = FALSE, ArchObj = lass,
#                      nearest = "cand_ns", sequ = TRUE)
#  cases <- anthrCases(res)
#  alphas <- round(res$alphas, 4)
#  df3[cases,]

## ----figure 2, eval=FALSE------------------------------------------------
#  # Code for Figure 2:
#  sel1 <- df3 %>%
#     mutate(Archetypoid = ifelse(as.numeric(rownames(df3)) %in% cases, "Yes", "No"))
#  ggplot(sel1, aes(x = c(sel1[,1])[[1]], y = c(sel1[,2])[[1]], color = Archetypoid)) +
#    geom_point() +
#    labs(x = colnames(sel1)[1], y = colnames(sel1)[2]) +
#    guides(color = guide_legend(override.aes = list(size = 5)))

## ----table 3, eval=FALSE-------------------------------------------------
#  # Code for Table 3:
#  df3_aux <- df2[which(df2$Position == "Center"), ]
#  get_similar_players(2, 0.95, alphas, cases, df3_aux, c("MP", "PTS"),
#                      unique(df3_aux$Compet), unique(df3_aux$Season))

## ----figure 3, eval=FALSE------------------------------------------------
#  # Code for Figure 3:
#  stats <- c("GP", "MP", "PTS", "FGPerc", "FTPerc", "TRB", "AST", "TOV", "PlusMinus", "PIR")
#  descr_stats <- c("Games played", "Minutes played", "Points", "Field goals percentage",
#                   "Free throws percentage", "Total rebounds", "Assists", "Turnovers",
#                   "Plus/minus", "Performance index rating")
#  df2_1 <- df2 %>%
#    select(1:5, stats, 46:49)
#  get_bubble_plot(df2_1, "Doncic, Luka", descr_stats)

## ----figure 4, message=FALSE, eval=FALSE---------------------------------
#  # Code for Figure 4:
#  months <- c(df0 %>% distinct(Month))$Month
#  months_order <- c("September", "October", "November", "December",  "January",
#                    "February", "March", "April", "May", "June")
#  months_plot <- match(months_order, months)
#  months_plot1 <- months_plot[!is.na(months_plot)]
#  months_plot2 <- months[months_plot1]
#  
#  df1_m <- df1 %>%
#                filter(Team == "Real_Madrid",
#                       Player.x == "Doncic, Luka") %>%
#                group_by(Month) %>%
#                do(do_stats(., "Average", "2017-2018", "ACB", "Regular Season")) %>%
#                ungroup() %>%
#                mutate(Month = factor(Month, levels = months_plot2)) %>%
#                arrange(Month)
#  
#  
#  df1_m1 <- df1_m %>%
#    select(1:5, stats, 46:50) %>%
#    select(-EPS)
#  title <- paste(paste(";", "ACB"), "2017-2018", "Regular Season",
#                           "Average", sep = " ; ")
#  get_barplot_monthly_stats(df1_m1, title, 2, 4)

## ----figure 5, message=FALSE, eval=FALSE---------------------------------
#  # Code for Figure 5:
#  df0$Compet <- "ACB"
#  plot_yearly <- get_stats_seasons(df0, "ACB", "Doncic, Luka", stats, "Regular Season")
#  plot_yearly$gg + labs(title = "Doncic, Luka ; Yearly statistics")

## ----figure 6, message=FALSE, eval=FALSE---------------------------------
#  # Code for Figure 6:
#  levels_stats <- list("Offensive" = c("PTS", "FG", "FGA", "FGPerc",
#                                       "TwoP", "TwoPA", "TwoPPerc",
#                                       "ThreeP", "ThreePA", "ThreePPerc",
#                                       "FT", "FTA", "FTPerc", "ORB", "AST"),
#                       "Defensive" = c("DRB", "STL", "PF"),
#                       "Other" = c("GP", "MP", "TRB", "PlusMinus", "PIR"),
#                       "Advanced" = c("EFGPerc", "PPS"))
#  get_heatmap_bb(df2, "Real_Madrid", levels_stats, "PlusMinus", 9,
#                 paste("ACB", "2017-2018", "Total", sep = " "))

## ----figure 7, eval=FALSE------------------------------------------------
#  # Code for Figure 7:
#  get_shooting_plot(df2, "Real_Madrid", 3, 1, paste("ACB", "17-18", sep = " "), "en")

## ----figure 8, eval=FALSE------------------------------------------------
#  # Code for Figure 8:
#  df1_10 <- df1 %>%
#    filter(Day <= 10)
#  teams <- as.character(rev(sort(unique(df2$Team))))
#  df_four_factors <- do_four_factors_df(df1_10, teams)
#  get_four_factors_plot(df_four_factors$df_rank, df_four_factors$df_no_rank, "Real_Madrid", "en")

## ----figure 9, eval=FALSE------------------------------------------------
#  # Code for Figure 9:
#  df0$Compet <- "ACB"
#  gg <- get_table_results(df0, "ACB", "2017-2018")
#  gg$plot_teams

## ----figure 10, eval=FALSE-----------------------------------------------
#  # Code for Figure 10:
#  get_map_nats(df2)

## ----session info--------------------------------------------------------
sessionInfo()
