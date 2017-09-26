#' check and include the pixels at the 4 corners
#'
#' include.corners will return the same vector if the corner
#' pixels are already included. Otherwise, it will replace
#'     first element   -->   left top pixel
#'     second element  -->   right top pxel
#'     third element   -->   left bottom pixel
#'     forth element   -->   right bottom pixel
#'
#' Pixels are counted row-wise. There should at least be
#' 4 pixels; otherwise nothing will be done. The start
#' counting index of pixels should be specified in the
#' argument start.
#'
#' @param pixels a vector of indices of pixels that
#' will get divided
#' @param xgrids.total total number of x.
#' @param ygrids.total total number of y.
#' @param start the counting start of pixels.
#'
#' @return same object as the argument pixels but
#' being sure the 4 corner pixels are included
#'
#' @export
include.corners <- function(
  pixels, xgrids.total, ygrids.total, start) {

  if (length(pixels) < 4) {
    warning("Length of pixels is less than 4. Nothing is done.",
            call. = TRUE)
    return(pixels)
  }

  corners <- c(0+start, (xgrids.total-1)+start,
               xgrids.total*ygrids.total - xgrids.total + start,
               xgrids.total*ygrids.total - 1 + start)
  for (i in 1:length(corners)) {
    if (!(corners[i] %in% pixels)) {
      pixels[i] <- corners[i]
    }
  }

  return(pixels)
}
