#' RAnEnExtra::pixelsToX
#'
#' RAnEnExtra::pixelsToX convert the pixel index
#' to its x coordinate counting row-wise. The start matters
#' because it sets the offset. The returned numbers are
#' counted from 1.
#'
#' @param pixels a vector of indices of pixels that
#' will get divided
#' @param xgrids.total total number of x.
#' @param start the counting start of pixels.
#'
#' @return x coordinate(s)
#'
#' @export
pixelsToX <- function(
  pixels, xgrids.total, start) {
  return((pixels %% xgrids.total) + 1 - start)
}
