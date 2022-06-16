#' grepMulti works similarly to \code{grepl}, but for multiple patterns and returning the object.
#'
#' @param x object where to look for patterns.
#'
#' @param patterns Character vector of patterns to look for objects.
#'
#' @param unwanted Character vector of patterns to exclude from search.
#'
#' @return The objects with specified patterns combined
#'
#' @author Tati Micheletti
#' @export
#' @rdname grepMulti
grepMulti <- function(x, patterns, unwanted = NULL) {
  rescued <- sapply(x, function(fun) all(sapply(X = patterns, FUN = grepl, fun)))
  recovered <- x[rescued]
  if (!is.null(unwanted)){
    discard <- sapply(recovered, function(fun) all(sapply(X = unwanted, FUN = grepl, fun)))
    afterFiltering <- recovered[!discard]
    return(afterFiltering)
  } else {
    return(recovered)
  }
}
