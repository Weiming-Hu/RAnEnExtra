# nni interpolate a raster based on the given control
# points using nearest neighbor interpolation.
#
nni <- function(x, y, z, rast, n = 3) {
  require(raster)
  require(RANN)

  # get the query points that we want to find the
  # nearest neighbors for
  #
  cp <- rasterToPoints(rast)

  # search for the nearest neighbors for query points
  nn <- nn2(cbind(x, y), cp, k = n)

  # compute the mean for each query point across the
  # n nearest neighbors
  #
  values <- apply(nn$nn.idx, 1, function(x) {return(mean(z[x]))})

  # assign values to the raster
  values(rast) <- values

  return(rast)
}
