#' substrBoth get a sub-string based on the number of characters and the side to start
#'
#' @param strng String from which to grab a subset
#' @param howManyCharacters numeric. How many characters should be returned in the sub-string?
#' @param fromEnd logical. Default is TRUE. Should te subset start in the end of the string?
#'
#' @return character string of the subset.
#'
#' @author Tati Micheletti
#' @export
#'
#' @rdname substrBoth
substrBoth <- function(strng, howManyCharacters, fromEnd = TRUE) {
  if (fromEnd) return(substr(x = strng, start = nchar(strng) - howManyCharacters+1, nchar(strng))) else
    return(substr(x = strng, start = 1, stop = howManyCharacters))
}
