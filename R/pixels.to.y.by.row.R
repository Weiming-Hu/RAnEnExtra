# convert the pixel index to its y coordinate counting
# row-wise. The start matters because it sets the offset.
# The returned numbers are counted from 1.
#
pixels.to.y.by.row <- function(
  pixels, xgrids.total, start = 0) {
  res <- floor((pixels - start) / xgrids.total) + 1
  return (res)
}
