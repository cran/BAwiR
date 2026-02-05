## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----packages, message=FALSE, eval=FALSE--------------------------------------
# # Firstly, load BAwiR:
# library(BAwiR)

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
# data_player_eff <- data.frame(team = "Real Madrid",
#                               win_perc = "83.3% (5/6)",
#                               pts_poss = 114,
#                               pts_poss_opp = 104,
#                               net_rtg = 10)
# 
# get_sticker(data_player_eff, acb_sticker_data_2526, "A. Abalde", "English")

## ----session info-------------------------------------------------------------
sessionInfo()

