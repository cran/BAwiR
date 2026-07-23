\name{acb_shooting_data_2425}
\alias{acb_shooting_data_2425}
\docType{data}
\title{
ACB shooting data, 2024-2025
}
\description{
Spatial shooting data from a sample of 30 players from the ACB 2024-2025 season.
See also \code{acb_players_2425}. 

This data set uses a standard 2D Cartesian coordinate system (x,y) to map the exact 
location of a field goal attempt on the court. The meanings of its variables are provided below:

\itemize{
\item season: Season in which the game was played.
\item day: Matchday on which the game was played.
\item id_game: Game identification label.
\item pos_x: X-axis representing the horizontal position on the court. An x-coordinate of 0 
is on the baseline or endline (where the backboard sits). The larger the value of x, the 
farther from the backboard the shot is taken. The current range of values is 0 to 24832.
\item pos_y: Y-axis representing the vertical position on the court. The limits are set by 
the sidelines, that is to say, the two boundary lines running the length of the court.
The current range of values is -7473 to 7450.
\item play_type: Four play types, namely, missed and made two-point field goal, and missed and made 
three-point field goal.
\item shot: Two- or three-point field goal.
\item outcome: Missed or made shot.
\item quarter: Quarter of the game in which the shot was taken.
\item minute: Minute of the game in which the shot was taken.           
\item second: Second of the game in which the shot was taken.       
\item local: Home (TRUE) or away (FALSE) player.
\item score_home: Home team's points at the moment the shot was taken.
\item score_away: Away team's points at the moment the shot was taken.
\item player_license_id: Player identification label.
\item full_name: Full name of the team to which the player taking the shot belongs.
\item short_name: Short name of the team to which the player taking the shot belongs.
\item abbreviated_name: Abbreviated name of the team to which the player taking the shot belongs. 
\item logo: Logo of the team to which the player taking the shot belongs.
\item points: Total points scored by the player taking the shot at the end of the game.          
\item player_name: Name of the player taking the shot.
\item player_number: Jersey number of the player taking the shot.
\item player_image: Image of the player taking the shot.
}
}
\usage{acb_shooting_data_2425}
\format{Data frame with 4277 rows and 23 columns.}
\source{\url{https://www.acb.com/}}
\keyword{datasets}
