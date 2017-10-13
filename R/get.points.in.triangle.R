#' get the x and y of points within the triangle
#'
#' @param triangle a spatialpolygon object of a triangle
#' @param bound If points fall exactly on polygon boundaries,
#' the default NULL gives arbitrary assignments. If TRUE,
#' then all points "on" boundaries are set as within the
#' polygon, if FALSE, outside.
#'
#' @return a matrix with x and y of points in the triangle
#'
#' @export
get.points.in.triangle <- function (triangle, bound = T) {
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

  return (pts.triangle)
}
