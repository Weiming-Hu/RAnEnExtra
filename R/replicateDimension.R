# "`-''-/").___..--''"`-._
#  (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
#  (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#    _ ..`--'_..-_/  /--'_.' ,'
#  (il),-''  (li),'  ((!.-'
#
# Author: Weiming Hu <weiming@psu.edu>
#         Martina Calovi <mcalovi@psu.edu>
#
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University
#

#' RAnEnExtra::replicateDimension
#'
#' RAnEnExtra::replicateDimension replicates a particular dimensions of the arrray.
#'
#' @author Weiming Hu \email{weiming@@psu.edu}
#' @author Martina Calovi \email{mxc895@@psu.edu}
#'
#' @md
#' @export
replicateDimension <- function(arr, times, dimension) {

  if (!requireNamespace('abind', quietly = T)) {
    stop('abind package is missing')
  }

  stopifnot(is.array(arr))
  stopifnot(dim(forecasts)[dimension] == 1)

  # Create a list of replicated arrays
  l <- list()
  for (i in 1:times) {
    l[[i]] <- arr
  }

  # Concatenate them
  arr.rep <- abind::abind(l, along = dimension)
  rm(l)

  # Garbage collection
  garbage <- gc(reset = T)
  rm(garbage)

  return(arr.rep)
}
