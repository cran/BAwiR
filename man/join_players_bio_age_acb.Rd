% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join_players_bio_age_acb.R
\name{join_players_bio_age_acb}
\alias{join_players_bio_age_acb}
\title{Join ACB games and players' info}
\usage{
join_players_bio_age_acb(df_games, df_rosters)
}
\arguments{
\item{df_games}{Data frame with the games.}

\item{df_rosters}{Data frame with the biography of the roster players.}
}
\value{
Data frame.
}
\description{
This function joins the ACB games with the players' bio 
and computes the players' age at each game.
}
\examples{
df <- join_players_bio_age_acb(acb_games_1718, acb_players_1718)
          
}
\seealso{
\code{\link{do_join_games_bio}}
}
\author{
Guillermo Vinue
}
