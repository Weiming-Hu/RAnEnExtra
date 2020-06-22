
# "`-''-/").___..--''"`-._
#  (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
#  (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#    _ ..`--'_..-_/  /--'_.' ,'
#  (il),-''  (li),'  ((!.-'
# 
# Author: Weiming Hu <weiming@psu.edu>
#         Martina Calovi <mcalovi@psu.edu>
#         Guido Cervone <cervone@@psu.edu>
#         
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University
#

#' RAnEnExtra::queryObservationsTime
#' 
#' RAnEnExtra::queryObservationsTime carries out the temporal downscaling by
#' reusing the indices from AnEn at forecast lead times. The downscaled time
#' series are defined with the original forecast lead time being at the center.
#' 
#' @author Weiming Hu \email{weiming@@psu.edu}
#' @author Martina Calovi \email{mcalovi@@psu.edu}
#' @author Guido Cervone \email{cervone@@psu.edu}
#' 
#' @param analogs A 4-dimensional array for analogs time index generated from
#' [RAnEn::generateAnalogs](https://weiming-hu.github.io/AnalogsEnsemble/R/reference/generateAnalogs.default.html).
#' @param analogs.flt The forecast lead times for analog stations in number of seconds.
#' @param obs A 3-dimensional array for observations used to generate
#' analogs. It is usually the member `search_observations` from `config` that 
#' is used by [RAnEn::generateAnalogs](https://weiming-hu.github.io/AnalogsEnsemble/R/reference/generateAnalogs.html).
#' @param obs.id The observation variable ID that will be used to downscale.
#' @param obs.time The obseravtion times. This is usually the member `observation_times`
#' from `config` that is used by
#' [RAnEn::generateAnalogs](https://weiming-hu.github.io/AnalogsEnsemble/R/reference/generateAnalogs.html).
#' @param radius The number of seconds that each forecast lead time will be extended to.
#' @param res The number of seconds for the downscaled tempoeral resolution.
#' @param verbose Verbose level for output messages.
#' @param show.progress Whether to show progress information.
#' @param keep.downscaled.FLTs Whether to return the downscaled forecast lead times.
#' 
#' @return An `AnEn` list.
#' 
#' @md
#' @export
queryObservationsTime <- function(
	analogs.index, analogs.flt, obs, obs.id, obs.time,
	radius = 1.5 * 3600, res = 15 * 60, verbose = 3,
	show.progress = F, keep.downscaled.FLTs = F) {
	
	# Sanity check
	stopifnot(length(dim(analogs.index)) == 4)
	stopifnot(length(dim(obs)) == 3)
	stopifnot(dim(analogs.index)[3] == length(analogs.flt))
	stopifnot(dim(obs)[1] >= obs.id)
	stopifnot(dim(obs)[3] == length(obs.time))
	
	# Convert radius and resolution to an integer. This integer sets where we starts
	# assigning values into our downscaled array.
	# 
	left_offset <- floor(radius / res)
	right_offset <- ceiling(radius / res) - 1
	
	# Initialize analogs for downscaling
	if (verbose >= 3) {
		cat('Temporally downscaling ...\n')
	}
	
	# Create a time mask for each original forecast lead time
	time.mask <- seq(from = -left_offset, to = right_offset, by = 1) * res
	
	# Create the downscaled lead time sequence
	analogs.flt.downscaled <- time.mask + rep(analogs.flt, each = length(time.mask))
	num.downscaled.flts <- length(analogs.flt.downscaled)
	
	analogs.downscaled.values <- array(NA, dim = c(dim(analogs.index)[1:2], num.downscaled.flts, dim(analogs.index)[4]))
	analogs.downscaled.time.index <- array(NA, dim = c(dim(analogs.index)[1:2], num.downscaled.flts, dim(analogs.index)[4]))
	
	if (show.progress) {
		pb <- txtProgressBar(min = 0, max = prod(dim(analogs.index)[1:2]), style = 3)
		counter <- 1
	}
	
	for (i.station in 1:dim(analogs.index)[1]) {
		for (i.test in 1:dim(analogs.index)[2]) {
			for (i.flt in 1:dim(analogs.index)[3]) {
				for (i.member in 1:dim(analogs.index)[4]) {
					
					# Get the selected observation time for the current analog member
					member.time <- obs.time[analogs.index[i.station, i.test, i.flt, i.member]]
					
					if (!is.na(member.time)) {
						
						# From the selected observation time, we downscale it to a higher 
						# resolution with the observation time centered in the time period.
						# 
						masked.time <- member.time + time.mask
						
						downscaled.times.index <- unlist(lapply(masked.time, function(x) {
							which(x == obs.time)}))
						
						# The center position in the downscaled time series
						i.flt.downscaled <- which(analogs.flt.downscaled == analogs.flt[i.flt])
						
						# The positions in the downscaled time searies that will
						# be assigned with downscaled observations
						# 
						downscaled.pos <- (i.flt.downscaled - left_offset) : (i.flt.downscaled + right_offset)
						
						analogs.downscaled.values[i.station, i.test, downscaled.pos, i.member] <- obs[obs.id, i.station, downscaled.times.index]
						analogs.downscaled.time.index[i.station, i.test, downscaled.pos, i.member] <- downscaled.times.index
					}
					
				}
			}
			
			if (show.progress) {
				setTxtProgressBar(pb, counter)
				counter <- counter + 1
			}
		}
	}
	
	if (show.progress) {
		close(pb)
	}
	
	if (verbose >= 3) {
		cat('Done (queryObservations)!\n')
	}
	
	ret <- list(analogs = analogs.downscaled.values,
							analogs_time_index = analogs.downscaled.time.index)
	
	if (keep.downscaled.FLTs) {
		ret$flts = analogs.flt.downscaled
	}
	
	class(ret) = 'AnEn'
	
	return(ret)
}
