#' include.boundaries select the points on the four
#' edges randomly and replace the original points
#' in pixels.
#'
#' @param pixels a vector of indices of pixels that
#' will get divided
#' @param num.edge.points an integer specifies the
#' extra points to select on each edge
#' @param xgrids.total total number of x.
#' @param ygrids.total total number of y.
#' @param start the counting start of pixels.
#'
#' @return same object as the argument pixels but
#' with the pixels on the boundaries
#'
#' @export
include.boundaries <- function (
  pixels, num.edge.points, xgrids.total, ygrids.total, start) {

  if (length(pixels) < (4 + 4 * num.edge.points)) {
    warning("Not enough points avaiable in the input pixels. Nothing is done.",
            call. = TRUE)
    return(pixels)
  }

  grids <- expand.grid(1:xgrids.total, 1:ygrids.total)

  # bottom
  head <- 4+1
  pixels[head:(head+num.edge.points-1)] <- sample((which(grids[, 2] == 1)-1),
                                                  num.edge.points)

  # left
  head <- head + num.edge.points
  pixels[head:(head+num.edge.points-1)] <- sample((which(grids[, 1] == 1)-1),
                                                  num.edge.points)

  # top
  head <- head + num.edge.points
  pixels[head:(head+num.edge.points-1)] <- sample((which(grids[, 2] == ygrids.total)-1),
                                                  num.edge.points)

  # right
  head <- head + num.edge.points
  pixels[head:(head+num.edge.points-1)] <- sample((which(grids[, 1] == xgrids.total)-1),
                                                  num.edge.points)

  return(pixels)
}
