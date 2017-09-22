# convert the pixel index to its x coordinate counting
# row-wise. The start matters because it sets the offset.
# The returned numbers are counted from 1.
#
pixels.to.x.by.row <- function(
  pixels, xgrids.total, start = 0) {
  res <- (pixels %% xgrids.total) + 1 - start
  return(res)
}
