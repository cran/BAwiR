#' Similar teams to archetypoids
#' 
#' @aliases get_similar_teams
#'
#' @description 
#' Similar teams to the archetypoids computed with 
#' \code{\link[Anthropometry]{archetypoids}} according to a similarity threshold.
#' 
#' @usage get_similar_teams(atype, threshold, alphas, cases, data, variables)
#' 
#' @param atype Number assigned to the archetypoid (1:length(\code{cases})) from which
#' searching the players who most resemble to it.
#' @param threshold Similarity threshold.
#' @param alphas Alpha values of all the players.
#' @param cases Archetypoids.
#' @param data Data frame with the statistics.
#' @param variables Statistics used to compute the archetypoids.
#' 
#' @return 
#' Data frame with the features of the similar teams.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link[Anthropometry]{archetypoids}}
#' 
#' @examples 
#' \dontrun{
#' (s0 <- Sys.time())
#' library(Anthropometry)
#' df <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718)
#' df$Compet <- "ACB"
#' df_teams <- do_stats_teams(df, "2017-2018", "ACB", "Regular Season")
#' df_team_total <- df_teams$df_team_total
#' 
#' df3 <- df_team_total[, c("PTS", "PTSrv", "Team")]
#' preproc <- preprocessing(df3[,1:2], stand = TRUE, percAccomm = 1)
#' set.seed(4321)
#' lass <- stepArchetypesRawData(preproc$data, 1:2, numRep = 20, verbose = FALSE)
#' res <- archetypoids(2, preproc$data, huge = 200, step = FALSE, ArchObj = lass,
#'                     nearest = "cand_ns", sequ = TRUE)
#' cases <- anthrCases(res)
#' df3[cases,]
#' alphas <- round(res$alphas, 4)
#' 
#' get_similar_teams(1, 0.95, alphas, cases, df_team_total, c("PTS", "PTSrv"))
#' s1 <- Sys.time() - s0
#' s1
#' }                     
#'
#' @export

get_similar_teams <- function(atype, threshold, alphas, cases, data, variables) {
  vec <- which(alphas[atype,] > threshold) 
  vec <- vec[!vec == cases[atype]] 
  
  good_coef <- t(alphas[,vec])
  
  good_dat <- data[vec, c("Team", variables)]
  
  #good_dat1 <- cbind(good_dat, good_coef)
  good_dat1 <- cbind(as.data.frame(good_dat), good_coef)
  
  #good_dat2 <- good_dat1[order(good_dat1[, colnames(good_dat1) == atype], decreasing = TRUE),]
  cols <- as.character(atype)
  good_dat2 <- good_dat1[do.call("order", c(good_dat1[cols], list(decreasing = TRUE))),]
  
  good_dat3 <- rbind(as.data.frame(data[cases[atype], c("Team", variables)]), 
                     good_dat2[, c("Team", variables)])
 
  return(good_dat3)
}