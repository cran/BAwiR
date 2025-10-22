## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----packages, message=FALSE, eval=FALSE--------------------------------------
# # Firstly, load BAwiR and other packages that will be used here:
# library(BAwiR)
# library(dplyr)
# library(Anthropometry)

## ----data, eval=FALSE---------------------------------------------------------
# data_days <- do_scrape_days_acb("2024", "analyst_name", TRUE, 2)
# 
# data_shots <- do_scrape_shots_acb(data_days[1:2, ], TRUE, "user_agent_def", "x_apikey")

## ----eval=FALSE---------------------------------------------------------------
# ?acb_shooting_data_2425
# head(acb_shooting_data_2425)

## ----eval=FALSE---------------------------------------------------------------
# sort(unique(acb_shooting_data_2425$player_name))
# sort(unique(acb_shooting_data_2425$full_name))

## ----eval=FALSE---------------------------------------------------------------
# df0 <- do_divide_court_zones(acb_shooting_data_2425)

## ----eval=FALSE---------------------------------------------------------------
# df1 <- do_filter_data(df0, "2024-2025", "", "", "", "", "")

## ----eval=FALSE---------------------------------------------------------------
# do_violin_box_plots(df1, acb_players_2425)

## ----eval=FALSE---------------------------------------------------------------
# shots_stats <- do_shots_stats(df1, df0)
# 
# do_viz_shots_scatter(shots_stats, "all", FALSE)

## ----eval=FALSE---------------------------------------------------------------
# df1 <- do_filter_data(df0, "2024-2025", "", "", "", "", "D. Ennis")
# 
# shots_stats <- do_shots_stats(df1, df0)
# 
# do_viz_shots_scatter(shots_stats, "player", TRUE)
# do_viz_shots_scatter(shots_stats, "player", FALSE)

## ----eval=FALSE---------------------------------------------------------------
# do_viz_shots_gradient(df1, "player", "none", df0)
# do_viz_shots_gradient(df1, "player", "fg", df0)

## ----eval=FALSE---------------------------------------------------------------
# zones_court <- metrics_player_zone %>%
#   distinct(location) %>%
#   pull()
# 
# numArch <- 10
# numRep <- 20
# numArchoid <- 2 # Number of archetypoids.
# 
# data_arch <- data.frame()
# 
# # Run the algorithm for each zone one by one and save the archetypoid
# # with least shots and highest percentage.
# i <- 1
# 
# zone <- metrics_player_zone %>%
#   filter(location == zones_court[i]) %>%
#   select(-pps_player)
# 
# zone_num <- zone %>%
#   select(total, perc_player)
# 
# lass <- stepArchetypesRawData(data = zone_num, numArch = 1:numArch,
#                               numRep = numRep, verbose = FALSE)
# 
# res_ns <- archetypoids(numArchoid, zone_num, huge = 200, step = FALSE,
#                        ArchObj = lass, nearest = "cand_ns",sequ = TRUE)
# zone[res_ns$cases, ]
# 
# # Here [1, ] indicates the archetypoid of interest. Change it accordingly.
# # Here 4 indicates the number of similar players to the archetypoid. Change it accordingly.
# arch_targ <- zone[order(res_ns$alphas[1, ], decreasing = TRUE)[1:4], ]
# data_arch <- rbind(data_arch, arch_targ)
# 
# i <- 2
# 
# zone <- metrics_player_zone %>%
#   filter(location == zones_court[i]) %>%
#   select(-pps_player)
# 
# zone_num <- zone %>%
#   select(total, perc_player)
# 
# lass <- stepArchetypesRawData(data = zone_num, numArch = 1:numArch,
#                               numRep = numRep, verbose = FALSE)
# 
# res_ns <- archetypoids(numArchoid, zone_num, huge = 200, step = FALSE,
#                        ArchObj = lass, nearest = "cand_ns",sequ = TRUE)
# zone[res_ns$cases, ]
# 
# arch_targ <- zone[order(res_ns$alphas[2, ], decreasing = TRUE)[1:4], ]
# data_arch <- rbind(data_arch, arch_targ)
# 
# do_best_zones(data_arch)

## ----session info-------------------------------------------------------------
sessionInfo()

