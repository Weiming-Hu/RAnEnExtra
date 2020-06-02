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

#' RAnEnExtra::queryObservationsSpace
#' 
#' RAnEnExtra::queryObservationsSpace carries out the spatial downscaling
#' by reusing the indices from AnEn at different locations.
#' 
#' @author Weiming Hu \email{weiming@@psu.edu}
#' @author Martina Calovi \email{mcalovi@@psu.edu}
#' @author Guido Cervone \email{cervone@@psu.edu}
#' 
#' @param analogs.index A 4-dimensional array for analogs time index generated from
#' [RAnEn::generateAnalogs](https://weiming-hu.github.io/AnalogsEnsemble/R/reference/generateAnalogs.html).
#' @param analogs.x The x coordinates for analog stations.
#' @param analogs.y The y coordinates for analog stations.
#' @param analogs.flt The forecast lead times for analog stations in number of seconds.
#' @param obs A 3-dimensional array for observations used to generate
#' analogs. It is usually the member `search_observations` from `config` that 
#' is used by [RAnEn::generateAnalogs](https://weiming-hu.github.io/AnalogsEnsemble/R/reference/generateAnalogs.html).
#' @param obs.x The x coordinates for observation stations.
#' These will be the target stations that analogs will be downscaled to.
#' @param obs.y The y coordinates for observation stations.
#' These will be the target stations that analogs will be downscaled to.
#' @param obs.id The observation variable ID that will be used to downscale.
#' @param verbose Verbose level for output messages.
#' @param show.progress Whether to show progress information.
#' @param keep.station.table Whether to keep the lookup table for matching
#' AnEn stations and the target observation stations in the return.
#' 
#' @return An `AnEn` list
#' 
#' @md
#' @export
queryObservationsSpace <- function(
	analogs.index, analogs.x, analogs.y, analogs.flt,
	obs, obs.x, obs.y, obs.id, verbose = 3,
	show.progress = F, keep.station.table = F) {
	
	# Additional check to make sure analogs are not created from search space extension
	
	# Sanity check
	stopifnot(length(dim(analogs.index)) == 4)
	stopifnot(length(dim(obs)) == 3)
	stopifnot(dim(analogs.index)[1] == length(analogs.x))
	stopifnot(dim(analogs.index)[1] == length(analogs.y))
	stopifnot(dim(analogs.index)[3] == length(analogs.flt))
	stopifnot(dim(obs)[2] == length(obs.x))
	stopifnot(dim(obs)[2] == length(obs.y))
	
	# Find matching observation stations to each analog station
	lookup.station <- nearestNeighbor(obs.x, obs.y, analogs.x, analogs.y)
	
	# Initialize analogs for spatial downscaling
	if (verbose >= 3) {
		cat('Spatially downscaling ...\n')
	}
	
	analogs.downscaled <- array(NA, dim = c(length(lookup.station), dim(analogs.index)[-1]))
	
	if (show.progress) {
		pb <- txtProgressBar(min = 0, max = prod(dim(analogs.downscaled)[1:2]), style = 3)
		counter <- 1
	}
	
	for (i.station in 1:dim(analogs.downscaled)[1]) {
		for (i.test in 1:dim(analogs.downscaled)[2]) {
			for (i.flt in 1:dim(analogs.downscaled)[3]) {
				for (i.member in 1:dim(analogs.downscaled)[4]) {
					
					# Get the selected observation index for the current analog member
					time.index <- analogs.index[lookup.station[i.station], i.test, i.flt, i.member]
					
					# Assign values
					analogs.downscaled[i.station, i.test, i.flt, i.member] <- obs[obs.id, i.station, time.index]
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
		cat('Done (queryObservationsSpace)!\n')
	}
	
	ret <- list(analogs_downscaled = analogs.downscaled)
	
	if (keep.station.table) {
		ret$downscaled_stations = lookup.station
	}
	
	class(ret) = 'AnEn'
	
	return(ret)
}

nearestNeighbor <- function(target.x, target.y, pool.x, pool.y) {
	
	stopifnot(length(target.x) == length(target.y))
	stopifnot(length(pool.x) == length(pool.y))
	
	nearest <- sapply(1:length(target.x), function(i) {
		
		distances <- sapply(1:length(pool.x), function(j) {
			dist(target.x[i], target.y[i], pool.x[j], pool.y[j])
		})
		
		which.min(distances)
	})
	
	return(nearest)
}

dist <- function(x1, y1, x2, y2) {
	sqrt((x1 - x2) ^ 2 + (y1 - y2)^2)
}
