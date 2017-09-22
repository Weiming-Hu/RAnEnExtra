# get.pixels.within.ycut selects the pixels that lie
# within a certain cut interval along the y axis.
# The index of pixels should be counted row-wise,
# which means that their x coordinates vary the fastest.
#
get.pixels.within.ycut <- function(
  cut.start, cut.end, pixels, xgrids.total) {

  # compute the y coordinates of the pixels
  ys.to.compute <- floor(pixels / xgrids.total)

  # which pixels lie within the specified cut interval
  flags <- ys.to.compute<=cut.end &
    cut.start<=ys.to.compute

  # return the pixels
  return(pixels.to.compute[flags])
}
