% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_EPS.R
\name{do_EPS}
\alias{do_EPS}
\title{Efficient Points Scored (EPS)}
\usage{
do_EPS(df)
}
\arguments{
\item{df}{Data frame with the games and the players info.}
}
\value{
EPS values.
}
\description{
A limitation of \code{\link{do_OE}} is that it doesn't rely on the quantity
of the player's offense production, that's to say, whether the player 
provides a lot of offense or not. In addition, it does not give credit 
for free-throws. An extension of \code{\link{do_OE}} has been defined: 
the Efficient Points Scored (EPS), which is the result of the product of 
OE and points scored. Points scored counts free-throws, two-point and 
three-point field goals. A factor \emph{F} is also added to put the adjusted 
total points on a points scored scale. With the factor \emph{F}, the sum of the 
EPS scores for all players in a given season is equal to the sum of the 
league total points scored in that season.
}
\examples{
df <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718)
df1 <- do_add_adv_stats(df)
do_EPS(df1)[1]

}
\references{
Shea, S., Baker, C., (2013). Basketball Analytics: 
Objective and Efficient Strategies for Understanding 
How Teams Win. Lake St. Louis, MO: Advanced Metrics, LLC.
}
\seealso{
\code{\link{do_OE}}, \code{\link{do_add_adv_stats}}
}
\author{
Guillermo Vinue
}
