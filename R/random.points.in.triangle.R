#' randomly select points within the triangle
#'
#' random.points.in.trianglegon randomly select num.points
#' points in the triangular polygon, and return the points
#'
#' @param triangle a spatialpolygon object of a triangle
#' @param num.points an integer specifying the amount of
#' random points to select
#' @param bound If points fall exactly on polygon boundaries,
#' the default NULL gives arbitrary assignments. If TRUE,
#' then all points "on" boundaries are set as within the
#' polygon, if FALSE, outside.
#' @param verbose a bool controls the standard output level
#'
#' @return a matrix with x and y coordinates of the random
#' points
#'
#' @export
random.points.in.triangle <- function (
  triangle, num.points, bound = T, verbose = F) {
  require(splancs)
  require(raster)

  # create lattice points over the triangle extent
  ext <- extent(triangle)
  xs <- extent(triangle)[1]: extent(triangle)[2]
  ys <- extent(triangle)[3]: extent(triangle)[4]
  pts.lattice <- expand.grid(x=xs, y=ys)
  colnames(pts.lattice) <- c('x', 'y')

  # find the points within the triangle
  outline <- triangle@polygons[[1]]@Polygons[[1]]@coords
  pts.triangle <- pts.lattice[inout(pts.lattice, outline,
                                    bound = bound), ]

  # randomly select from the points in the triangle
  pts.triangle.total <- nrow(pts.triangle)
  if (pts.triangle.total > num.points) {
    selected <- sample.int(pts.triangle.total, size = num.points)
  } else {
    if (verbose) {
      print(paste("Only ", pts.triangle.total, " points available.",
                  "But you asked for ", num.points, ". All points are selected."))
    }
    selected <- sample.int(pts.triangle.total, size = pts.triangle.total)
  }

  return(pts.triangle[selected, ])
}
