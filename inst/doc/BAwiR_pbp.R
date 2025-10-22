## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----packages, message=FALSE, eval=FALSE--------------------------------------
# # Firstly, load BAwiR and other packages that will be used in the paper:
# library(BAwiR)
# library(tidyverse)

## ----data, eval=FALSE---------------------------------------------------------
# df0 <- acb_vbc_cz_pbp_2223
# 
# day_num <- unique(df0$day)
# game_code <- unique(df0$game_code)

## ----processing, eval=FALSE---------------------------------------------------
# acb_games_2223_sl <- acb_vbc_cz_sl_2223 %>%
#   filter(period == "1C")
# 
# df1 <- do_prepare_data(df0, day_num,
#                       acb_games_2223_sl, acb_games_2223_info,
#                       game_code)

## ----lineups, eval=FALSE------------------------------------------------------
# # Lineups and sub-lineups:
# data_li <- do_lineup(df1, day_num, game_code, "Valencia Basket", FALSE)
# data_subli <- do_sub_lineup(data_li, 4)

## ----possessions, eval=FALSE--------------------------------------------------
# # Possessions:
# data_poss <- do_possession(df1, "1C")

## ----timeouts, eval=FALSE-----------------------------------------------------
# # Timeouts:
# df1_to <- do_prepare_data_to(df0, TRUE, acb_games_2223_info, acb_games_2223_coach)
# data_to <- do_time_out_success(df1_to, day_num, game_code,
#                                "Casademont Zaragoza_Porfirio Fisac", FALSE)

## ----periods, eval=FALSE------------------------------------------------------
# # Periods:
# team_sel <- "Valencia Basket" # "Casademont Zaragoza"
# period_sel <- "1C"            # "4C"
# player_sel <- "Webb"          # "Mara"
# 
# pre_per <- do_preproc_period(acb_vbc_cz_pbp_2223, team_sel, period_sel, acb_vbc_cz_sl_2223)
# 
# df2 <- pre_per$df2
# df0_inli_team <- pre_per$df0_inli_team
# 
# df3 <- do_prepare_data(df2, day_num, df0_inli_team, acb_games_2223_info, game_code)
# 
# data_per <- do_stats_per_period(df3, day_num, game_code, team_sel, period_sel, player_sel)
# 
# # Clutch time:
# data_clutch <- do_clutch_time(df0)
# # If no rows, that means that the game did not have clutch time.

## ----fouls, eval=FALSE--------------------------------------------------------
# # Free throw fouls:
# data_ft_comm <- do_ft_fouls(df0, "comm")
# data_ft_rec <- do_ft_fouls(df0, "rec")
# 
# # Offensive fouls:
# data_off_comm <- do_offensive_fouls(df0, "comm")
# data_off_rec <- do_offensive_fouls(df0, "rec")

## ----rebounds, eval=FALSE-----------------------------------------------------
# # Offensive rebounds:
# df1_or <- do_prepare_data_or(df0, TRUE, acb_games_2223_info)
# data_or <- do_reb_off_success(df1_or, day_num, game_code, "Valencia Basket", FALSE)

## ----session info-------------------------------------------------------------
sessionInfo()

