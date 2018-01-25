#' Capitalize two-word strings
#' 
#' @aliases capit_two_words
#'
#' @description 
#' Ancillary function to capitalize the first letter of both words in a 
#' two-word string. This can be used for example to capitalize the teams 
#' names for the plots title.
#' 
#' @usage 
#' capit_two_words(two_word_string)
#' 
#' @param two_word_string Two-word string.
#' 
#' @return 
#' Vector with the two words capitalized.
#' 
#' @examples 
#' capit_two_words("valencia basket")
#'
#' @export

capit_two_words <- function(two_word_string){
    two_words <- strsplit(two_word_string, " ")[[1]]
    two_words_cap <-paste(toupper(substring(two_words, 1,1)), 
                                  substring(two_words, 2), 
                          sep = "", collapse = " ")
    return(two_words_cap)
}
