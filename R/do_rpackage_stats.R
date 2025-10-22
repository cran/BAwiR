#' R package downloads
#' 
#' @aliases do_rpackage_stats
#'
#' @description 
#' Counts the number of times that a given R package was downloaded in a given year.
#' 
#' @usage 
#' do_rpackage_stats(r_packages, year, verbose)
#' 
#' @param r_packages Vector with the names of the R packages.
#' @param year String with the year.
#' @param verbose Should R report information on progress? TRUE or FALSE.
#' 
#' @return 
#' A data frame.
#' 
#' @seealso 
#' \link[packageRank]{cranDownloads}
#' 
#' @author 
#' Guillermo Vinue
#' 
#' @examples 
#' \dontrun{
#' do_rpackage_stats(c("BAwiR", "BasketballAnalyzeR"), 2025, TRUE)
#' }
#' 
#' @importFrom dplyr last
#' 
#' @export

do_rpackage_stats <- function(r_packages, year, verbose) {
  package <- cumulative <- NULL
  
  pkg_data <- data.frame()
  for (i in 1:length(r_packages)) {
    if (verbose) {
      cat("ITERATION", i, "\n")
      cat("PACKAGE", r_packages[i], "\n") 
    }
    
    pkg_downl <- packageRank::cranDownloads(packages = r_packages[i], from = year)
    
    pkg_iter <- pkg_downl$cranlogs.data %>% last() %>% select(package, cumulative)
    
    pkg_data <- rbind(pkg_iter, pkg_data)
  }
  
  return(pkg_data)
}
