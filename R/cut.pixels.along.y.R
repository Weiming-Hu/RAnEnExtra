#' divide pixels according to cuts on y
#'
#' cut.pixels.along.y cuts the pixels into separate
#' sets based on the cutting points specified in
#' ycuts and the y coordinates of each pixel.
#' The start counting index of pixels should
#' be specified in the argument start. Note that
#' ycuts should be counted from 1
#'
#' @param pixels a vector of indices of pixels that
#' will get divided
#' @param ycuts a vector of integers specifing the y
#' locations that will be cut. The range of the cut will
#' be defined as [ycuts[i], ycuts[i+1]).
#' @param xgrids.total total number of x.
#' @param ygrids.total total number of y.
#' @param start the counting start of pixels.
#' @param flag.sort a bool specifing whether to sort
#' the cut pixels or not
#'
#' @return a list of vectors with the separate pixels.
#'
#' @export
cut.pixels.along.y <- function(
  pixels, ycuts, xgrids.total,
  ygrids.total, start, flag.sort = T) {

  res <- list()

  ys.to.compute <-
    pixels.to.y.by.row(pixels, xgrids.total, start)

  for (i in 1:length(ycuts)) {
    min = ycuts[i]
    max = ycuts[i+1] - 1
    if (i == length(ycuts)) {
      max = xgrids.total*ygrids.total
    }

    flags <- ys.to.compute<=max &
      min<=ys.to.compute

    if(flag.sort) {
      res <- c(res, list(sort(pixels[flags])))
    } else {
      res <- c(res, list(pixels[flags]))
    }
  }

  return(res)
}
