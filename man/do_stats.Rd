% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_stats.R
\name{do_stats}
\alias{do_stats}
\title{Accumulated or average statistics}
\usage{
do_stats(df_games, type_stats = "Total", season, competition, type_season)
}
\arguments{
\item{df_games}{Data frame with the games, players info, advanced stats and
eventually recoded teams names.}

\item{type_stats}{String. In English, the options are "Total" and "Average" and in
Spanish, the options are "Totales" and "Promedio".}

\item{season}{String indicating the season, for example, 2017-2018.}

\item{competition}{String. Options are "ACB", "Euroleague" and "Eurocup".}

\item{type_season}{String with the round of competition, for example regular season
or playoffs and so on.}
}
\value{
Data frame.
}
\description{
This function computes either the total or the average statistics.
}
\examples{
compet <- "ACB"
df <- do_join_games_bio(compet, acb_games_1718, acb_players_1718)
df1 <- do_add_adv_stats(df)
df2 <- do_stats(df1, "Total", "2017-2018", compet, "Regular Season")
        
}
\author{
Guillermo Vinue
}
