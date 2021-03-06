#  "`-''-/").___..--''"`-._
# (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
# (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#   _ ..`--'_..-_/  /--'_.' ,'
# (il),-''  (li),'  ((!.-'
#
#
# Author: Weiming Hu (weiming@psu.edu)
#         Guido Cervone (cervone@psu.edu)
#
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University
#

#' RAnEnExtra::verifySpreadSkill
#'
#' RAnEnExtra::verifySpreadSkill calculates the spread skill correlation between ensemble
#' variance and squared error. **Read details for caveats**.
#'
#' Spread skill correlation without normalization or binning has several caveats during
#' its interpretation. Please make sure you have read through [several materials](https://www.ecmwf.int/sites/default/files/elibrary/2007/15443-verifying-relationship-between-ensemble-forecast-spread-and-skill.pdf) before
#' applying this verification technique. Alternatively, `RAnEn::verifyBinnedSpreadSkill` is
#' an improved version of this function.
#'
#' @details Bootstrap confidence interval is defaulted to 0.95.
#' To change this, use `options(RAnEnExtra_boot_conf = 0.9)`.
#'
#' @author Weiming Hu \email{weiming@@psu.edu}
#'
#' @param anen.ver A 4-dimensional array. This array is usually created from the `value` column of
#' the `analogs` member in the results of `RAnEn::generateAnalogs`. The dimensions should be
#' `[stations, times, lead times, members]`.
#' @param obs.ver A 3-dimensional array. The dimensions should be `[stations, times, lead times]`.
#' You can generate the array using `RAnEn::alignObservations`.
#' @param boot Whether to use bootstrap.
#' @param R The number of bootstrap replicates. Used by the function `boot::boot`.
#' @param na.rm Whether to remove NA values.
#' @param keep.cor Whether to keep all spread and errors.
#'
#' @md
#' @export
verifySpreadSkill <- function(anen.ver, obs.ver, boot = F, R = 1000, na.rm = T, keep.cor = F) {

  stopifnot(length(dim(anen.ver)) == 4)
  stopifnot(length(dim(obs.ver)) == 3)

  if ( !identical(dim(anen.ver)[1:3], dim(obs.ver)[1:3]) ) {
    cat("Error: Observations and Forecasts have incompatible dimensions.\n")
    return(NULL)
  }

  # Convert to [stations x days x flt]
  obs       <-  as.vector(obs.ver)
  anen      <-  matrix(anen.ver,ncol=dim(anen.ver)[4])

  m     <- ncol(anen) # Number of members
  mdegf <- m / (m+1)

  # To calculate the correlation between spread and error,
  # here I'm using squared differences as errors and
  # ensemble variances as spread, as denoted by the L2 measure by Hopson.
  #
  # Reference:
  # https://www.ecmwf.int/sites/default/files/elibrary/2007/15443-verifying-relationship-between-ensemble-forecast-spread-and-skill.pdf
  #
  anen.mean <-  rowMeans(anen ,na.rm=na.rm)
  error     <-  ( anen.mean-obs ) ^ 2

  # Compute variance. I didn't use apply and var functions.
  # I vectorize the implementation for better performance.
  #
  spread    <- rowSums((anen - rowMeans(anen, na.rm = na.rm))^2,
                       na.rm = na.rm) / (dim(anen)[2] - 1)

  # Combine the indeces and remove the NAs
  mat <- data.frame(mean = anen.mean, spread = spread, error = error)

  num.entries <- prod(dim(anen.ver)[1:2])
  num.flts <- dim(anen.ver)[3]

  # Store the results for each lead time
  flt <- rep(NA, times = num.flts)

  # Store the results of the bootstrap
  flt.boot <-  matrix(NA, ncol = num.flts, nrow=3)

  # group the rows into FLTs
  cuts <- seq(from = 1, by = num.entries, length.out = num.flts)

  for (i.cut in 1:length(cuts)) {

    # Define the subset of rows which belong to the specific FLT
    start <- cuts[i.cut]
    end <- start + num.entries - 1

    # Extract the rows for a specific FLT
    mat.sub <- mat[start:end, ]

    # Remove NAs
    if ( na.rm == T ) mat.sub <- na.omit( mat.sub )

    if (nrow(mat.sub) == 0) next

    if(boot == T){
      #bootstrap confidence intervals only if required

      temp <- mat.sub[, 3]

      err.boot <- boot.fun.ver(temp, R)

      # Normalize the results
      flt.boot[, i] <- sqrt( err.boot * mdegf )
    }

    flt[i.cut] <-  cor(mat.sub[, 2], mat.sub[, 3] * mdegf,
                       use="pairwise.complete.obs")
  }

  ret <- list(mean = mean(flt), flt = flt, flt.boot = flt.boot)
  if (keep.cor) {
    ret$cor <- mat
  }

  return(ret)
}
