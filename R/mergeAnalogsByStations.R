#  "`-''-/").___..--''"`-._
# (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
# (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#   _ ..`--'_..-_/  /--'_.' ,'
# (il),-''  (li),'  ((!.-'
#
#
# Author: Weiming Hu (weiming@psu.edu)
#
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University
#

#' RAnEnExtra::mergeAnalogsByStations
#'
#' RAnEnExtra::mergeAnalogsByStations reads a vector of analog files, usually generated from `anen_netcdf` or `anen_grib`,
#' and then combines analogs by stations. It is assumed that `analogs`, `Xs`, and `Ys` exist in all files.
#'
#' @param files A vector of AnEn files
#' @param verbose Whether to be verbose
#' @param merge_forecasts Whether to merge `Forecasts/Data` if it exists
#' @param cores The number of cores to use while reading files
#' @param copy_vars The variable names to simply copy from the first file in the (sorted) list into the final AnEn list
#' @return An AnEn.
#' @md
#' @export
mergeAnalogsByStations <- function(files, verbose = T, merge_forecasts = T,
                                   cores = 1, copy_vars = NULL) {

  # Sanity check
  file_exists <- file.exists(files)
  stopifnot(all(file_exists))

  files <- sort(files)

  #################################
  # Read data from separate files #
  #################################

  if (verbose) {
    cat('Reading analogs from separate files ...\n')
  }

  analogs <- parallel::mclapply(files, function(file) {
    RAnEn::readNc(file, var_names = 'analogs')$analogs
  }, mc.cores = cores)

  if (verbose) {
    cat('Reading Xs from separate files ...\n')
  }

  xs <- parallel::mclapply(files, function(file) {
    RAnEn::readNc(file, var_names = 'Xs')$Xs
  }, mc.cores = cores)

  if (verbose) {
    cat('Reading Ys from separate files ...\n')
  }

  ys <- parallel::mclapply(files, function(file) {
    RAnEn::readNc(file, var_names = 'Ys')$Ys
  }, mc.cores = cores)

  if (merge_forecasts) {

    if (verbose) {
      cat('Reading forecast data from separate files ...\n')
    }

    forecasts <- parallel::mclapply(files, function(file) {
      RAnEn::readNc(file, var_names = 'Forecasts/Data')[['Forecasts/Data']]
    }, mc.cores = cores)
  }

  ################
  # Bind analogs #
  ################

  if (verbose) {
    cat('Binding a list of data ...\n')
  }

  AnEn <- list(
    analogs = abind::abind(analogs, along = 1),
    Xs = as.numeric(abind::abind(xs, along = 1)),
    Ys = as.numeric(abind::abind(ys, along = 1))
  )

  if (merge_forecasts) {
    AnEn[['Forecasts/Data']] <- abind::abind(forecasts, along = 2)
  }

  class(AnEn) <- c('AnEn', class(AnEn))

  ############################
  # Variables to simply copy #
  ############################

  if (!is.null(copy_vars)) {
    copy_vars <- RAnEn::readNc(files[1], root_group_only = F, var_names = copy_vars)

    for (copy_var in names(copy_vars)) {
      stopifnot(!copy_var %in% names(AnEn))
      AnEn[[copy_var]] <- copy_vars[[copy_var]]
    }
  }

  return(AnEn)
}
