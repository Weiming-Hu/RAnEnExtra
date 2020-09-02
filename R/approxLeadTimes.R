#  "`-''-/").___..--''"`-._
# (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
# (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#   _ ..`--'_..-_/  /--'_.' ,'
# (il),-''  (li),'  ((!.-'
#
#
# Author: Weiming Hu (weiming@psu.edu)
#         Martina Calovi (mxc895@psu.edu)
#
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University
#

#' RAnEnExtra::approxLeadTimes
#'
#' RAnEnExtra::approxLeadTimes interpolate forecasts across the lead time dimension.
#'
#' @author Weiming Hu \email{weiming@@psu.edu}
#' @author Martina Calovi \email{mxc895@@psu.edu}
#'
#' @param forecasts A 4-dimensional array
#' @param flt_dimension Which dimension is the lead time dimension.
#' @param old_flts Original forecast lead times in seconds
#' @param new_flts New forecast lead times in seconds
#' @param method The `method` argument passed to `approx`.
#' @param parallel Whether to use `future_apply` for parallelization
#'
#' @md
#' @export
approxLeadTimes <- function(forecasts, flt_dimension, old_flts, new_flts, method = 'linear', parallel = FALSE) {

	check.package("R.utils")
	check.package("abind")
	check.package("progress")

	if (parallel) {
	  check.package('future.apply')
	}

	# Sanity check
	stopifnot(!is.null(dim(forecasts)))

	# Figure out the dimensions that I need to loop through
	apply_dimensions <- 1:length(dim(forecasts))
	apply_dimensions <- apply_dimensions[apply_dimensions != flt_dimension]

	# Define a function to downscale along a slice of forecast lead times
	func <- function(y, x, xout, method) {
	  approx(x = x, y = y, xout = xout, method = method)$y
	}

	# Carry out downscaling along the dimensions
	if (parallel) {
	  forecasts_ds <- future.apply::future_apply(forecasts, apply_dimensions, func, x = old_flts, xout = new_flts, method = method)
	} else {
	  forecasts_ds <- apply(forecasts, apply_dimensions, func, x = old_flts, xout = new_flts, method = method)
	}

	# Fix the dimensions of downscaled forecasts
	perm <- seq_len(length(apply_dimensions)) + 1
	perm <- R.utils::insert(perm, flt_dimension, 1)
	forecasts_ds <- aperm(forecasts_ds, perm)

	return(forecasts_ds)
}
