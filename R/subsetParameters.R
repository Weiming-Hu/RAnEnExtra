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

#' RAnEnExtra::subsetParameters
#'
#' RAnEnExtra::subsetParameters is a convevient function to subset parameters
#' from forecast and observation lists.
#'
#' @details
#' RAnEnExtra::subsetParameters will select the parameters based on the input
#' index from the following members of the input list (if they exist):
#'
#' - ParameterNames
#' - ParameterCirculars
#' - Data
#'
#' @param index An index vector for parameters to extract
#' @param l A forecast or observation list. For how to create such a
#' list, please see
#' [this tutorial](https://weiming-hu.github.io/AnalogsEnsemble/2019/11/18/format-obs.html).
#' For what members to include in the list, see
#' [this doc](https://weiming-hu.github.io/AnalogsEnsemble/2019/01/16/NetCDF-File-Types.html).
#' @param verbose Whether to print progress information.
#'
#' @return A forecast or observation list depending on your input
#' list type with the subset parameters.
#'
#' @seealso [RAnEnExtra::subsetStations] which shares a similar interface and has an usage example.
#'
#' @md
#' @export
subsetParameters <- function(index, l, verbose = T) {

  # Sanity check
  stopifnot(is.list(l))
  stopifnot('Data' %in% names(l))

  num_parameters <- dim(l$Data)[1]

  if (max(index) > num_parameters) {
    stop('Some indices are larger than the number of parameters.')
  }

  # These are the members to subset
  names <- c('ParameterNames', 'Data')

  for (name in names) {
    if (name %in% names(l) & !is.null(l[[name]])) {

      if (verbose) {
        cat('Subset the list member', name, '...\n')
      }

      if (name == 'Data') {

        if (length(dim(l[[name]])) == 3) {
          l[[name]] <- l[[name]][index, , , drop = F]
        } else if (length(dim(l[[name]])) == 4) {
          l[[name]] <- l[[name]][index, , , , drop = F]
        } else {
          stop('The member Data should have either 3 or 4 dimensions.')
        }

      } else {

        if (length(l[[name]]) != num_parameters) {
          stop(paste('The member', name, 'has a different number of parameters.'))
        }

        l[[name]] <- as.vector(l[[name]])[index]
      }

    }
  }

  if ('ParameterCirculars' %in% names(l)) {
    if (length(l$parameterCirculars) > 0) {

      l$ParameterCirculars <- l$ParameterCirculars[l$ParameterCirculars %in% l$ParameterNames]

      if (verbose) {
        cat('Circular parameters have been subset\n')
      }
    }
  }

  if (verbose) {
    cat('Done (subsetParameters)\n')
  }

  return(l)
}
