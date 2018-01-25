#' Offensive Efficiency (OE)
#' 
#' @aliases do_OE
#'
#' @description 
#' Offensive Efficiency (OE) is a measure to evaluate the quality of 
#' offense produced. OE counts the total number of successful offensive 
#' possessions the player was involved in, regarding the player's total 
#' number of potential ends of possession. 
#' 
#' This measure is used in the definition of \code{\link{do_EPS}}.
#' 
#' @usage 
#' do_OE(df)
#' 
#' @param df Data frame with the games and the players info.
#' 
#' @note 
#' When either both the numerator and denominator of the OE expression 
#' are 0 or just the denominator is 0, the function returns a 0.
#' 
#' @return 
#' OE values.
#' 
#' @references 
#' Shea, S., Baker, C., (2013). Basketball Analytics: 
#' Objective and Efficient Strategies for Understanding 
#' How Teams Win. Lake St. Louis, MO: Advanced Metrics, LLC.
#' 
#' @author 
#' Guillermo Vinue
#'
#' @seealso 
#' \code{\link{do_EPS}}, \code{\link{do_add_adv_stats}}
#' 
#' @examples 
#' df <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718)
#' df1 <- do_add_adv_stats(df)
#' # Players with OE = 0:
#' # df1[55, c("Player.x", "FG", "AST", "FGA", "ORB", "TOV")]
#' # Player.x     FG  AST  FGA  ORB  TOV
#' # Triguero, J.  0    0    0    0    0
#' # OE can be greater than 1, for example:
#' # df1[17, c("Player.x", "FG", "AST", "FGA", "ORB", "TOV")]
#' # Player.x        FG  AST  FGA  ORB  TOV
#' # Diagne, Moussa   3    0    3    1    0
#' do_OE(df1[1,])
#'
#' @export

do_OE <- function(df){
  numer <- df$FG + df$AST
  denom <- df$FGA - df$ORB + df$AST + df$TOV
  
  oe_num <- c()
  for (i in 1:nrow(df)) {
    oe_num[i] <- ifelse(any(c(numer[i], denom[i]) == 0), 0, round(numer[i] / denom[i], 1))
  }
  
  return(oe_num)
}



