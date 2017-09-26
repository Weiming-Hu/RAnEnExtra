#' nearest neighbor interpolation
#'
#' nni interpolate a raster based on the given control
#' points using nearest neighbor interpolation.
#'
#' @param x a vector of x coordinates
#' @param y a vector of y coordinates
#' @param z a vector of values of each point
#' @param rast a base raster that specifies the outline
#' of the raster to be interpolated
#' @param n number of nearest neighbors
#'
#' @return an interpolated raster object
#'
#' @export
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
