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
#' @param progress Whether to plot a progress bar.
#' 
#' @md
#' @export
approxLeadTimes <- function(forecasts, flt_dimension, old_flts, new_flts,
														 method = 'linear', progress = FALSE) {
	
	check.package("R.utils")
	check.package("abind")
	check.package("progress")
	
	# Sanity check
	stopifnot(length(dim(forecasts)) == 4)
	
	# Figure out the dimensions that I need to loop through
	apply_dimensions <- 1:length(dim(forecasts))
	apply_dimensions <- apply_dimensions[apply_dimensions != flt_dimension]
	
	# Define new dimensions
	new_dims <- c(dim(forecasts)[apply_dimensions], length(new_flts))
	
	# Allocation
	forecasts_ds <- array(NA, new_dims)
	
	if (progress) {
		pb <- progress::progress_bar$new(total = prod(dim(forecasts_ds)[1:3]))
	}
	
	for (i1 in seq_len(new_dims[1])) {
		for (i2 in seq_len(new_dims[2])) {
			for (i3 in seq_len(new_dims[3])) {
				
				y <- abind::asub(forecasts, list(i1, i2, i3), apply_dimensions, drop = T)
				y_ds <- approx(x = old_flts, y = y, xout = new_flts, method = method)
				forecasts_ds[i1, i2, i3, ] <- y_ds$y
				
				if (progress) {
					pb$tick()
				}
			}
		}
	}
	
	if (progress) {
		pb$terminate()
	}
	
	# Fix the dimensions of downscaled forecasts
	perm <- seq_len(length(apply_dimensions))
	perm <- R.utils::insert(perm, flt_dimension, length(apply_dimensions) + 1)
	forecasts_ds <- aperm(forecasts_ds, perm)
	
	return(forecasts_ds)
}
