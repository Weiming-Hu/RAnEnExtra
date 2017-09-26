#' convert x and y to pixel index
#'
#' convert x and y coordinates to pixels index. Assume
#' that x and y coordinates are counting from 1. Pixels
#' start counting from the argument start, and they are
#' counted row-wise.
#'
#' @param x a vector of x coordinates of the pixels
#' starting from 1
#' @param y a vector of y coordinates of the pixels
#' starting from 1
#' @param xgrids.total total number of x.
#' @param start the counting start of pixels.
#'
#' @return the index of the pixel
#'
#' @export
xy.to.pixels <- function(x, y, xgrids.total, start) {
  return((y-1) * xgrids.total + (x-1) + start)
}
