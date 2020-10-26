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
#' @return An AnEn.
#' @md
#' @export
mergeAnalogsByStations <- function(files, verbose = T) {

  # Sanity check
  file_exists <- file.exists(files)
  stopifnot(all(file_exists))

  files <- sort(files)

  #################################
  # Read data from separate files #
  #################################

  if (verbose) {
    cat('Reading data from separate files ...\n')
  }

  analogs <- lapply(files, function(file) {
    RAnEn::readNc(file, var_names = 'analogs')$analogs
  })

  xs <- lapply(files, function(file) {
    RAnEn::readNc(file, var_names = 'Xs')$Xs
  })

  ys <- lapply(files, function(file) {
    RAnEn::readNc(file, var_names = 'Ys')$Ys
  })

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

  class(AnEn) <- c('AnEn', class(AnEn))

  return(AnEn)
}
