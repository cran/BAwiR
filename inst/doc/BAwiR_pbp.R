## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----packages, message=FALSE, eval=FALSE--------------------------------------
# # Firstly, load BAwiR and other packages that will be used in the paper:
# library(BAwiR) # 1.3
# library(tidyverse) # 1.3.2

## ----data, eval=FALSE---------------------------------------------------------
# df0 <- acb_vbc_cz_pbp_2223
# 
# day_num <- unique(acb_vbc_cz_pbp_2223$day)
# game_code <- unique(acb_vbc_cz_pbp_2223$game_code)

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
# df0_per <- df0
# 
# rm_overtime <- TRUE # Decide if remove overtimes.
# if (rm_overtime) {
#   df0 <- df0 %>%
#     filter(!grepl("PR", period)) %>%
#     mutate(period = as.character(period))
# }
# 
# team_sel <- "Valencia Basket" # "Casademont Zaragoza"
# period_sel <- "1C"            # "4C"
# player_sel <- "Webb"          # "Mara"
# 
# df1 <- df0 %>%
#   filter(team == team_sel) %>%
#   filter(!action %in% c("D - Descalificante - No TL", "Altercado no TL"))
# 
# df2 <- df1 %>%
#   filter(period == period_sel)
# 
# df0_inli_team <- acb_vbc_cz_sl_2223 %>%
#   filter(team == team_sel, period == period_sel)
# 
# df3 <- do_prepare_data(df2, day_num,
#                        df0_inli_team, acb_games_2223_info,
#                        game_code)
# 
# data_per <- do_stats_per_period(df3, day_num, game_code, team_sel, period_sel, player_sel)
# 
# # Clutch time:
# data_clutch <- do_clutch_time(acb_vbc_cz_pbp_2223)

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

