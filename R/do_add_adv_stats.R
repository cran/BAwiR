#' Advanced statistics
#' 
#' @aliases do_add_adv_stats
#'
#' @description 
#' This function adds to the whole data frame the advanced statistics 
#' for every player in every game.
#' 
#' @usage 
#' do_add_adv_stats(df)
#' 
#' @param df Data frame with the games and the players info.
#' 
#' @return 
#' Data frame.
#' 
#' @details 
#' The advanced statistics computed are as follows:
#' \itemize{
#' \item GameSc: Game Score.
#' \item PIE: Player Impact Estimate.
#' \item EFGPerc: Effective Field Goal Percentage.
#' \item ThreeRate: Three points attempted regarding the total field goals attempted.
#' \item FRate: Free Throws made regarding the total field goals attempted.
#' \item STL_TOV: Steal to Turnover Ratio.
#' \item AST_TOV: Assist to Turnover Ratio.
#' \item PPS: Points Per Shot.
#' \item OE: Offensive Efficiency.
#' \item EPS: Efficient Points Scored. 
#' }
#' 
#' The detailed definition of some of these stats can be found at
#' \url{https://www.basketball-reference.com/about/glossary.html} and
#' \url{https://www.nba.com/stats/help/glossary/}.
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @seealso 
#' \code{\link{do_OE}}, \code{\link{do_EPS}}
#' 
#' @examples 
#' df <- do_join_games_bio("ACB", acb_games_1718, acb_players_1718)
#' 
#' df1 <- do_add_adv_stats(df)
#'           
#' @importFrom magrittr %>%            
#' @importFrom dplyr group_by mutate select    
#'                  
#' @export

do_add_adv_stats <- function(df) {
  AST <- BLKfv <- DRB <- EFGPerc <- FG <- FGA <- FT <- FTA <- Game <- NULL 
  GameRes <- GameSc <- GmAST <- GmBLKfv <- GmDRB <- GmFG <- GmFGA <- NULL
  GmFT <- GmFTA <- GmORB <- GmPF <- GmPTS <- GmSTL <- GmTOV <- ORB <- NULL 
  PF <- PTS <- Player.x <- STL <- TOV <- ThreeP <- ThreePA <- TwoP <- NULL
  Day <- MP <- TwoPA <- denom_pie <- numer_pie <- NULL
  
  # Total game stats:
  df1 <- df %>%
    filter(MP != 0) %>%
    filter(!is.na(MP)) %>%
    filter(GameRes != "NA-NA") %>%
    group_by(Game) %>%
    mutate(GmPTS = sum(as.numeric(strsplit(as.character(unique(GameRes)), "-")[[1]]))) %>%
    mutate(GmFG = sum(TwoP) + sum(ThreeP)) %>%
    mutate(GmFGA = sum(TwoPA) + sum(ThreePA)) %>%
    mutate(GmFT = sum(FT)) %>%
    mutate(GmFTA = sum(FTA)) %>%
    mutate(GmDRB = sum(DRB)) %>%
    mutate(GmORB = sum(ORB)) %>%
    mutate(GmAST = sum(AST)) %>%
    mutate(GmSTL = sum(STL)) %>%
    mutate(GmBLKfv = sum(BLKfv)) %>%
    mutate(GmPF = sum(PF)) %>%
    mutate(GmTOV = sum(TOV))
  
  # Game Score and PIE:
  df2 <- df1 %>%
    group_by(Player.x) %>%
    mutate(FG = TwoP + ThreeP) %>%
    mutate(FGA = TwoPA + ThreePA) %>%
    mutate(FGPerc = ifelse(FGA == 0, 0, round((FG / FGA) * 100))) %>%
    mutate(GameSc =  PTS + 0.4 * FG - 0.7 * FGA - 0.4 * (FTA - FT) + 0.7 * ORB + 0.3 * DRB + 
                     STL + 0.7 * AST + 0.7 * BLKfv - 0.4 * PF - TOV) %>%
    mutate(GameSc = round(GameSc)) %>%
    mutate(numer_pie = PTS + FG + FT - FGA - FTA + DRB + (0.5 * ORB) + AST + STL + (0.5 * BLKfv) - PF - TOV) %>%
    mutate(denom_pie = GmPTS + GmFG + GmFT - GmFGA - GmFTA + GmDRB + (0.5 * GmORB) + 
                       GmAST + GmSTL + (0.5 * GmBLKfv) - GmPF - GmTOV) %>%
    mutate(PIE = round((numer_pie / denom_pie) * 100)) %>%
    select(-numer_pie, -denom_pie) %>%
    # More stats:
    #mutate(TSP = PTS / (2 * (FGA + 0.44 * FTA))) %>% # True Shooting Percentage.
    #mutate(TSP = round(TSP * 100)) %>% # For Causeur (first row), this percentage is greater than 100.
    mutate(EFGPerc = ifelse(FGA == 0, 0, (FG + 0.5 * ThreeP) / FGA)) %>% # Effective Field Goal Percentage.
    mutate(EFGPerc = round(EFGPerc * 100)) %>%
    mutate(EFGPerc = ifelse(EFGPerc > 100, 100, EFGPerc)) %>% # FG = 1 ; Three = 1 --> (1 + 0.5 * 1) / 1 > 1
    mutate(ThreeRate = ifelse(FGA == 0, 0, round((ThreePA / FGA) * 100, 1))) %>% # 3-Point Attempt Rate.
    mutate(FRate = ifelse(FGA == 0, 0, round(FT / FGA, 1))) %>% # Free Throw Attempt Rate.
    mutate(STL_TOV = ifelse(TOV == 0, STL, round(STL / TOV, 1))) %>% # Steal to Turnover Ratio.
    mutate(AST_TOV = ifelse(TOV == 0, AST, round(AST / TOV, 1))) %>% # Assist to Turnover Ratio.
    mutate(PPS = ifelse(FGA == 0, 0, round(PTS / FGA, 1))) # Points per Shot.
    
  df2$OE <- do_OE(df2) # Offensive Efficiency.
  df2$EPS <- do_EPS(df2) # Efficient Points Scored.
    
  return(df2)
}