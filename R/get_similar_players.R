#' Similar players to archetypoids
#' 
#' @aliases get_similar_players
#'
#' @description 
#' Similar players to the archetypoids computed with 
#' \code{\link[Anthropometry]{archetypoids}} according to a similarity threshold.
#' 
#' @usage get_similar_players(atype, threshold, alphas, cases, data, variables, compet, season)
#' 
#' @param atype Number assigned to the archetypoid (1:length(\code{cases})) from which
#' searching the players who most resemble to it.
#' @param threshold Similarity threshold.
#' @param alphas Alpha values of all the players.
#' @param cases Archetypoids.
#' @param data Data frame with the statistics.
#' @param variables Statistics used to compute the archetypoids.
#' @param compet Competition.
#' @param season Season.
#' 
#' @return 
#' Data frame with the features of the similar players.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link[Anthropometry]{archetypoids}}
#' 
#' @examples 
#' (s0 <- Sys.time())
#' library(Anthropometry)
#' df <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718)
#' df1 <- do_add_adv_stats(df)
#' df2 <- do_stats(df1, "Total", "2017-2018", "ACB", "Regular Season")
#' df3 <- df2[which(df2$Position == "Guard")[1:31], c("MP", "PTS", "Name")]
#' preproc <- preprocessing(df3[,1:2], stand = TRUE, percAccomm = 1)
#' set.seed(4321)
#' lass <- stepArchetypesRawData(preproc$data, 1:2, numRep = 20, verbose = FALSE)
#' res <- archetypoids(2, preproc$data, huge = 200, step = FALSE, ArchObj = lass,
#'                     nearest = "cand_ns", sequ = TRUE)
#' cases <- anthrCases(res)
#' df3[cases,]
#' alphas <- round(res$alphas, 4)
#' df3_aux <- df2[which(df2$Position == "Guard")[1:31], ]
#' get_similar_players(1, 0.99, alphas, cases, df3_aux, c("MP", "PTS"), 
#'                     unique(df3_aux$Compet), unique(df3_aux$Season))
#' s1 <- Sys.time() - s0
#' s1                    
#'
#' @export

get_similar_players <- function(atype, threshold, alphas, cases, data, variables, compet, season) {
  vec <- which(alphas[atype,] > threshold) 
  vec <- vec[!vec == cases[atype]] 
  
  good_coef <- t(alphas[,vec])
  
  good_dat <- data[vec, c("Name", "Position", "Team", variables, "CombinID")]
  
  good_dat1 <- cbind(good_dat, good_coef)
  
  good_dat2 <- good_dat1[order(good_dat1[, colnames(good_dat1) == atype], decreasing = TRUE),]
  
  good_dat3 <- rbind(data[cases[atype], c("Name", "Position", "Team", variables, "CombinID")], 
                     good_dat2[, c("Name", "Position", "Team", variables, "CombinID")])
  
  if (compet == "ACB") {
    good_dat3$CombinID <- paste("http://www.acb.com/jugador.php?id=", good_dat3$CombinID, sep = "")
  }else if (compet == "Euroleague") {
    good_dat3$CombinID <- paste("http://www.euroleague.net/competition/players/showplayer?pcode=", 
                                good_dat3$CombinID, "&seasoncode=E", substr(season, 1, 4), sep = "")
  }else if (compet == "Eurocup") {
    good_dat3$CombinID <- paste("http://www.eurocupbasketball.com/eurocup/competition/players/showplayer?pcode=", 
                                good_dat3$CombinID, "&seasoncode=U", substr(season, 1, 4), sep = "") 
  }
  
  colnames(good_dat3)[6] <- "Web_info"
  return(good_dat3)
}