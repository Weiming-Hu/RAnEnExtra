#' interpolate values using inverse Euclidean distance weighting
#'
#' @param pts.interpolate coordinates for points that will be
#' interpolated
#'
#' @param pts.known coordinates for known points
#'
#' @param pts.values values for known points
#'
#' @return a vector with values for the points to be
#' interpolated
#'
#' @export
inverse.distance.interpolation <- function (
  pts.interpolate, pts.known, pts.values) {

  # parameter check
  if (class(pts.interpolate) != 'matrix') {
    stop(paste('pts.interpolate is expected to be a matrix. Found',
               class(pts.interpolate)))
  } else {
    pts.interpolate <- apply(pts.interpolate, 2, as.numeric)
  }
  if (class(pts.known) != 'matrix') {
    stop(paste('pts.known is expected to be a matrix. Found',
               class(pts.interpolate)))
  } else {
    pts.known <- apply(pts.known, 2, as.numeric)
  }
  if (!is.vector(pts.values)) {
    stop(paste('pts.values is expected to be a vector.'))
  } else {
    pts.values <- as.numeric(pts.values)
  }

  if (dim(pts.known)[1] != length(pts.values)) {
    stop(paste("The number of points (", dim(pts.known)[1], ')',
               ' is not the same with the number of values (',
               length(pts.values), ')', sep = ''))
  }
  if (dim(pts.known)[2] != 2) {
    stop("There are more than 2 columns in the known points")
  }
  if (dim(pts.interpolate)[2] != 2) {
    stop("There are more than 2 columns in the interpolation points")
  }

  # calculate interpolated values
  weights <- apply(pts.interpolate, 1, function(row, pts.known) {
    dists <- apply((t(pts.known)- row)^2, 2, sum, na.rm = T)
    if (0 %in% dists) {
      index <- which(dists == 0)

      if (length(index) > 1) {
        print("pts.known:")
        print(pts.known)
        stop("There are duplicated points in pts.known")
      }

      dists <- rep(0, length(dists))
      dists[index] <- 1
      print(dists)
    }

    sum <- sum(dists)
    weights <- dists / sum
    return(weights)
  }, pts.known = pts.known)

  interpolation.values <- apply(weights, 2, function(col, pts.values) {
    return(sqrt(sum((col*pts.values)^2)))}, pts.values = pts.values)

  return(interpolation.values)
}
