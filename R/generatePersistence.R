# "`-''-/").___..--''"`-._
#  (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
#  (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#    _ ..`--'_..-_/  /--'_.' ,'
#  (il),-''  (li),'  ((!.-'
#
# Author: Weiming Hu <weiming@psu.edu>
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University
#

#' RAnEnExtra::generatePersistence
#'
#' RAnEnExtra::generatePersistence generates persistent analog ensemble forecasts.
#' For each test forecast, it simply takes the historical observation values
#' that are associated with the historical forecasts that are directly prior
#' to the current forecasts. The number of values taken would be the number of
#' ensemble members desired.
#'
#' @author Weiming Hu \email{weiming@@psu.edu}
#'
#' @details
#' For example, to generate a 3-member persistent analog ensemble for a particular
#' forecast at a certain grid point, a certain lead time, and on July 25, 2019,
#' the **observations** associated with the historical forecasts on July 22,
#' 24, and 24 will be included in the persistent analogs.
#'
#' @param forecasts Forecast data array
#' @param flts Forecast lead times
#' @param observations Observation data array
#' @param obs.id Observation vairable index
#' @param obs.times Observation times
#' @param test.times Test times of analog ensemble
#' @param num.analogs The number of analogs
#' @param forecast.time.interval The forecast time interval in seconds. This is
#' usually `24 * 60 * 60` because the third dimension of forecasts is usually day.
#' If it is half day, change the value accordingly.
#' @param show.progress Whether to show a progress bar.
#' @param silent Whether to be silent.
#'
#' @return An 5-dimension array
#' `[stations, times, flts, members, index for (values, stations, times)]`.
#'
#' @md
#' @export
generatePersistence <- function(

  forecasts,
  flts,
  observations,
  obs.id,
  obs.times,
  test.times,
  num.analogs,

  forecast.time.interval = 86400, show.progress = F, silent = F) {

  check.package('RAnEn')

  if (length(unique(obs.times)) != length(obs.times)) {
    stop('Observation times have duplicates!')
  }

  if (max(obs.times) < max(test.times) + max(flts)) {
    stop('The observation times do not cover all test times!')
  }

  # Read meta info
  num.grids <- dim(forecasts)[2]
  num.test.times <- length(test.times)
  num.flts <- dim(forecasts)[4]

  # Generate a historical time sequence for each test time. Observations for this historical
  # time sequence will be collected for persistence.
  #
  time.prev <- sapply(test.times, function(x) {
    return(x - (0:num.analogs) * forecast.time.interval)})

  # Generate the times that will be extracted for persistence
  times.to.extract <- sort(as.POSIXct(unique(as.vector(time.prev)), origin = '1970-01-01', tz = 'UTC'))

  # Align observations
  if (!silent) cat('Aligning observations ...\n')
  obs.align <- alignObservations(
    observations[obs.id, , , drop = F],
    obs.times, times.to.extract, flts,
    silent = silent, show.progress = show.progress)

  # Generate mapping from the forecast times to be extracted to observation times
  mapping <- RAnEn::generateTimeMapping(times.to.extract, flts, obs.times)

  # Transpose this mapping so that it has flts by rows and times by columns
  mapping <- t(mapping)

  # Change the dimensions of observations to make it easier for value extraction
  dim(obs.align) <- dim(obs.align)[-1]

  # Dimensions become [stations, FLTs, forecast times]
  obs.align <- aperm(obs.align, c(1, 3, 2))

  # Initialize memory for persistent analogs
  persistent <- array(NA, dim = c(
    num.grids, num.test.times, num.flts, num.analogs, 3))

  if (!silent) cat('Generating persistence ...\n')
  if (show.progress) {
    pb <- txtProgressBar(max = num.test.times, style = 3)
    counter <- 0
  }

  for (i.test.time in 1:num.test.times) {
    extract.times <- which(test.times[i.test.time] == times.to.extract) - 1:num.analogs
    persistent[, i.test.time, , , 1] <- obs.align[, , extract.times]
    persistent[, i.test.time, , , 2] <- rep(1:num.grids, times = num.flts*num_members)
    persistent[, i.test.time, , , 3] <- rep(mapping[as.matrix(expand.grid(1:num.flts, extract.times))], each = num.grids)

    if (show.progress) {
      counter <- counter + 1
      setTxtProgressBar(pb, counter)
    }
  }

  if (show.progress) {
    close(pb)
  }

  if (!silent) cat('Done (generatePersistence)!\n')
  return(persistent)
}
