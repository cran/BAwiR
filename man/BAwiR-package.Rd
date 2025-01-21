\name{BAwiR-package}
\alias{BAwiR-package}
\docType{package}
\title{
Analysis of Basketball Data
}
\description{
Collection of tools to work with European basketball data. Functions available are related to friendly 
web scraping, data management and visualization. Data were obtained from 
<https://www.euroleaguebasketball.net/euroleague/>, 
<https://www.euroleaguebasketball.net/eurocup/> and <https://www.acb.com/>, following the instructions 
of their respectives robots.txt files, when available. Box score data are available for the three leagues. 
Play-by-play data are also available for the Spanish league. Methods for analysis include a population pyramid, 
2D plots, circular plots of players' percentiles, plots of players' monthly/yearly stats, team heatmaps, 
team shooting plots, team four factors plots, cross-tables with the results of regular season games,
maps of nationalities, combinations of lineups, possessions-related variables, timeouts, performance by periods, personal fouls and offensive rebounds. 
Please see Vinue (2020) <doi:10.1089/big.2018.0124> and Vinue (2024) <doi:10.1089/big.2023.0177>.    
}
\details{
\tabular{ll}{
Package: BAwiR\cr
Type: Package\cr
Version: 1.3.3\cr
Date: 2024-04-16\cr
License: GPL-2\cr
LazyLoad: yes\cr
LazyData: yes\cr
}
acb_games_1718: ACB games 2017-2018.\cr
acb_games_2223_coach: ACB coaches in the 2022-2023 season.\cr
acb_games_2223_info: ACB games 2022-2023, days and codes.\cr
acb_players_1718: ACB players 2017-2018.\cr
acb_shields: Shields of the ACB teams.\cr
acb_vbc_cz_pbp_2223: ACB play-by-play data, 2022-2023, Valencia Basket-Casademont Zaragoza.\cr
acb_vbc_cz_sl_2223: ACB starting lineups, 2022-2023, Valencia Basket-Casademont Zaragoza.\cr
capit_two_words: Capitalize two-word strings.\cr
do_add_adv_stats:	Advanced statistics.\cr
do_clutch_time: Get games with clutch time.\cr
do_EPS: Efficient Points Scored (EPS). \cr
do_four_factors_df:	Four factors data frame.\cr
do_ft_fouls: Compute free throw fouls.\cr
do_join_games_bio: Join games and players' info.\cr
do_lineup: Compute ACB lineups.\cr
do_map_nats: Data frame for the nationalities map.\cr
do_OE: Offensive Efficiency (OE). \cr
do_offensive_fouls: Compute offensive fouls.\cr
do_possession: Compute when possessions start.\cr
do_prepare_data: Prepare ACB play-by-play data.\cr
do_prepare_data_or: Prepare data for the offensive rebounds computation.\cr
do_prepare_data_to: Prepare data for the timeouts computation.\cr
do_process_acb_pbp: Processing of the ACB website play-by-play data.\cr
do_reb_off_success: Check if scoring after offensive rebounds.\cr
do_scraping_games: Player game finder data.\cr
do_scraping_rosters: Players profile data.\cr
do_stats: Accumulated or average statistics.\cr
do_stats_per_period: Compute stats per period.\cr
do_stats_teams: Accumulated and average statistics for teams.\cr
do_sub_lineup: Compute ACB sub-lineups.\cr
do_time_out_success: Check if timeouts resulted in scoring.\cr
eurocup_games_1718: Eurocup games 2017-2018.\cr
eurocup_players_1718:	Eurocup players 2017-2018.\cr
euroleague_games_1718: Euroleague games 2017-2018.\cr
euroleague_players_1718:	Euroleague players 2017-2018.\cr
get_barplot_monthly_stats: Barplots with monthly stats.\cr
get_bubble_plot: Basketball bubble plot.\cr
get_four_factors_plot: Four factors plot.\cr
get_games_rosters: Get all games and rosters.\cr
get_heatmap_bb: Basketball heatmap.\cr  
get_map_nats: Nationalities map.\cr
get_pop_pyramid: ACB population pyramid.\cr
get_shooting_plot: Shooting plot.\cr
get_similar_players: Similar players to archetypoids.\cr
get_similar_teams: Similar teams to archetypoids.\cr
get_stats_seasons: Season-by-season stats.\cr
get_table_results: League cross table.\cr
join_players_bio_age_acb: Join ACB games and players' info.\cr
join_players_bio_age_euro: Join Euroleague and Eurocup games and players' info.\cr
scraping_games_acb: ACB player game finder data.\cr
scraping_games_acb_old: Old ACB player game finder data.\cr
scraping_games_euro: Euroleague and Eurocup player game finder data.\cr
scraping_rosters_acb: ACB players' profile.\cr
scraping_rosters_euro: Euroleague and Eurocup players' profile.\cr
}
\references{
Vinue, G., (2020). A Web Application for Interactive Visualization of European Basketball Data, Big Data 8(1), 70-86. https://doi.org/10.1089/big.2018.0124, https://www.uv.es/vivigui/AppEuroACB.html

Vinue, G., (2024). A Basketball Big Data Platform for Box Score and Play-by-Play Data, Big Data (), -. https://doi.org/10.1089/big.2018.0124, https://www.uv.es/vivigui/basketball_platform.html
}
\author{
Guillermo Vinue <Guillermo.Vinue@uv.es>, <guillermovinue@gmail.com>
}
\keyword{BAwiR}