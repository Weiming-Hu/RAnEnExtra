# "`-''-/").___..--''"`-._
#  (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
#  (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#    _ ..`--'_..-_/  /--'_.' ,'
#  (il),-''  (li),'  ((!.-'
#
# Author: Weiming Hu <wuh20@psu.edu>
#         Martina Calovi <mxc895@psu.edu>
#
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University

#' RAnEnExtra::biasCorrection
#'
#' RAnEnExtra::biasCorrection carries out a bias correction routine on the analog ensembles
#' using a linear regression method. This correction method is useful for predicting extreme event.
#'
#' @author Weiming Hu \email{weiming@@psu.edu}
#' @author Martina Calovi \email{mxc895@@psu.edu}
#'
#' @param analogs A four-dimensional array for analog values with the dimensions `[stations, test times, lead times, members]`.
#' This is usually generated from the `RAnEn::generateAnalogs`.
#' @param target.forecasts A three- or four- dimensional array for test forecasts that analogs are generated for. The dimensions
#' can either be `[stations, test times, lead times]` or `[stations, test times, lead times, 1]`. The forecasts should be aligned
#' with the anlaogs for the first three dimensions.
#' @param historical.forecasts The historical forecast search repository with the dimensions `[parameters, stations, times, lead times]`.
#' This is usually the `Forecasts` used in `RAnEn::generateAnalogs`.
#' @param similarity.time.index Similarity time index for each analog members. To have this for your analogs, you need to
#' set `config$save_similarity_time_index = T` before you run `RAnEn::generateAnalogs`.
#' @param similarity.station.index Similarity station index for each analog members. To have this for your analogs, you need to
#' set `config$save_similarity_station_index = T` and use `SSE` for analog generation.
#' @param regression.forecasts The forecast values used to calculate the slope of a linear regression line. These forecasts must
#' correspond to observations for regression.
#' @param regression.observations The observation values used to calculate the slope of a linear regression line These
#' observations must correspond to forecasts for regression.
#' @param forecast.id A forecasts parameter index used by the `historical.forecasts` to specify which forecast parameter to use.
#' @param activation.func An activation function to signify whether a particular ensemble should be bias corrected. This function
#' should takes a single argument and return a TRUE or FALSE. The single argument of the function will be an analog ensemble,
#' or a numeric vector. For example, `activation.func = function(members) {if (mean(members) > 15) {return(T)} else {return(F)}}`
#' will only correct ensembles that have an average over 15.
#' @param show.progress Whether to show a progress bar
#' @param return.more Whether to return more information
#' @param group.func How to collapse the analog ensemble to a single value to calculate the amount of correction. In the paper and
#' by default, this is `mean`.
#' @param ... Additional variables passed to `group.func`
#'
#' @references
#' Alessandrini, Stefano, Simone Sperati, and Luca Delle Monache. "Improving the analog ensemble
#' wind speed forecasts for rare events." Monthly Weather Review 147.7 (2019): 2677-2692.
#'
#' @md
#' @export
biasCorrection <- function(analogs, target.forecasts, historical.forecasts,
                           similarity.time.index, similarity.station.index = NULL,
                           regression.forecasts = NULL, regression.observations = NULL,
                           forecast.id = NULL, activation.func = NULL, show.progress = T,
                           return.more = F, group.func = mean, ...) {

  # Sanity check

  if (!requireNamespace('progress', quietly = T)) {
    stop('abind package is missing')
  }

  stopifnot(length(dim(analogs)) == 4)
  stopifnot(length(regression.forecasts) == length(regression.observations))
  stopifnot(length(dim(target.forecasts)) >= 3)
  stopifnot(identical(dim(analogs)[1:3], dim(target.forecasts)[1:3]))
  stopifnot(length(dim(historical.forecasts)) == 4)
  stopifnot(identical(dim(similarity.time.index), dim(analogs)))

  # Check for the same number of stations
  stopifnot(dim(analogs)[1] == dim(historical.forecasts)[2])

  # Check for the same number of lead times
  stopifnot(dim(analogs)[3] == dim(historical.forecasts)[4])

  if (!is.null(similarity.station.index)) {
    stopifnot(identical(dim(similarity.station.index), dim(analogs)))
  }

  # If there are multiple variables in the historical forecasts, I need to know
  # which variables to use.
  #
  if (dim(historical.forecasts)[1] != 1) {
    if (is.null(forecast.id)) {
      stop('Please specify forecast.id in the historical forecasts for which variable to use')
    }

    stopifnot(length(forecast.id) == 1)
  }

  # The dimension information is not important for regression forecasts and
  # observations. I'm going to flatten regression forecasts and observations
  # into vectors.
  #
  skip.regression <- is.null(regression.forecasts) | is.null(regression.observations)

  if (!skip.regression) {
    stopifnot(is.null(regression.forecasts))
    stopifnot(is.null(regression.observations))

    if (!is.null(dim(regression.forecasts))) {
      regression.forecasts <- as.numeric(regression.forecasts)
    }

    if (!is.null(dim(regression.observations))) {
      regression.observations <- as.numeric(regression.observations)
    }
  }

  # If target forecasts have more than 3 dimensions, only a single-length
  # forth dimension is allowed!
  #
  if (length(dim(target.forecasts)) > 3) {
    stopifnot(length(dim(target.forecasts)) == 4)
    stopifnot(dim(target.forecasts)[4] == 1)
    target.forecasts <- abind::adrop(target.forecasts, 4)
  }

  ################################################################
  # Step 1: Estimate the slope (beta_0) of the linear regression #
  ################################################################

  if (skip.regression) {
    slope <- 1
  } else {
    mod <- lm(regression.observations ~ regression.forecasts)
    slope <- mod$coefficients[2]
  }

  ###########################
  # Step 2: Bias correction #
  ###########################

  num.stations <- dim(analogs)[1]
  num.times <- dim(analogs)[2]
  num.flts <- dim(analogs)[3]

  if (show.progress) {
    pb <- progress::progress_bar$new(total = num.stations)
  }

  # Initialize a bias
  bias <- array(NA, dim = dim(analogs)[1:3])

  for (station.index in 1:num.stations) {
    for (time.index in 1:num.times) {
      for (flt.index in 1:num.flts) {

        # Analog ensemble members
        O.a <- analogs[station.index, time.index, flt.index, ]

        # Check if an activation function is provided
        if (is.function(activation.func)) {

          # Check whether the activation function returns false, if false is returned,
          # the later bias correction routine is skipped for this particular analog ensemble.
          #
          if (!activation.func(O.a)) {
            next
          }
        }

        # The target forecast (or the current forecast)
        P.t <- target.forecasts[station.index, time.index, flt.index]

        # The analog historical forecasts
        historical.forecast.times <- similarity.time.index[station.index, time.index, flt.index, ]

        if (is.null(similarity.station.index)) {

          # If station indices are not provided (usually in the case of independent search)
          P.a <- historical.forecasts[forecast.id, station.index, historical.forecast.times, flt.index]

        } else {

          # If station indices are provided (usually in the case of search space extension)
          historical.forecast.stations <- similarity.station.index[station.index, time.index, flt.index, ]
          matrix.index <- cbind(forecast.id, historical.forecast.stations, historical.forecast.times, flt.index)
          P.a <- historical.forecasts[matrix.index]
        }

        # Calculate the bias
        bias[station.index, time.index, flt.index] <- - slope * (P.t - group.func(P.a, ...))

        # Correction (Analogs - Bias)
        analogs[station.index, time.index, flt.index, ] <- O.a - bias[station.index, time.index, flt.index]
      }
    }

    if (show.progress) {
      pb$tick()
    }
  }

  if (show.progress) {
    pb$terminate()
  }

  if (return.more) {
    return(list(analogs = analogs, bias = bias, slope = slope))
  } else {
    return(analogs)
  }
}
