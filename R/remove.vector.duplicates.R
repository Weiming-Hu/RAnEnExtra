#' removes the duplicated elements in a vector
#'
#' remove.vector.duplicates removes the
#' duplicated elements from the original vector
#' and return the new vector with no duplicates
#'
#' @param v a vector to check and remove the
#' duplicates
#'
#' @return a vector with no duplicates
#'
#' @export
remove.vector.duplicates <- function(v) {
  if (is.vector(v)) {
    index.duplicated <- which(duplicated(v))
    return(v[-index.duplicated])
  } else {
    warning(paste("Not implemented for",
                  class(v)))
    return(NULL)
  }
}
