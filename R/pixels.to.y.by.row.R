#' convert pixel index to y coordinate
#'
#' convert the pixel index to its y coordinate counting
#' row-wise. The start matters because it sets the offset.
#' The returned numbers are counted from 1.
#'
#' @param pixels a vector of indices of pixels that
#' will get divided
#' @param xgrids.total total number of x.
#' @param start the counting start of pixels.
#'
#' @return y coordinate(s)
#' @export
pixels.to.y.by.row <- function(
  pixels, xgrids.total, start) {
  return (floor((pixels - start) / xgrids.total) + 1)
}
