#  "`-''-/").___..--''"`-._
# (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
# (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#   _ ..`--'_..-_/  /--'_.' ,'
# (il),-''  (li),'  ((!.-'
#
#
# Author: Weiming Hu (weiming@psu.edu)
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University
#

#' RAnEnExtra::verifyBrier
#' 
#' RAnEnExtra::verifyBrier computes the Brier score and its decomposition for each 
#' all lead times available. This function used the NCAR verification package.
#' 
#' @author Weiming Hu \email{weiming@@psu.edu}
#' 
#' @param anen.ver A 4-dimensional array for analogs.
#' @param obs.ver A 3-dimensional array for observations.
#' @param threshold The numeric threshold for computing the brier score.
#' Observation larger than or equal to the threshold
#' will be converted to 1. Analog values will *not* be processed with this
#' threshold value because it is assumed that the ensemble function `ensemble.func`
#' will convert analog ensemble to probability values. Please see examples.
#' @param ensemble.func A function that takes a vector as input and then
#' converts the ensemble members (the 4th dimension of analogs) into a scalar.
#' This scalar is usually a probability within `[0, 1]`. Please see examples.
#' @param baseline TRUE if the anen.ver is an one-member forecast array. This is
#' usually used for deterministic baseline model.
#' @param ... Extra parameters for the ensemble.func.
#' 
#' @examples 
#' 
#' # Create synthetic datasets from different distributions
#' # with 10 stations, 2 test days, 5 lead times, and
#' # 50 ensemble members.
#' # 
#' 
#' anen.ver <- array(rnorm(5000), dim = c(10, 2, 5, 50))
#' obs.ver <- array(runif(100, min = -5, max = 5), dim = c(10, 2, 5))
#' 
#' # Set the threshold for observation. Values larger
#' # than or equal to this value will be converted to 1, otherwise 
#' # to 0.
#' #
#' threshold <- 0.5
#' 
#' # This is the function that takes the ensemble value vector 
#' # as input and output a probability. For example, here I count
#' # how many values in the member are larger than or equal to
#' # the split value. You can define your own function to compute
#' # a probability from the ensemble.
#' # 
#' ensemble.func <- function(v, split) {return(
#'   length(which(v >= split)) / length(v)
#' )}
#' 
#' # Calculate the Brier score.
#' # Don't forget to pass the additional threshold to your ensemble
#' # function. Otherwise you will receive an error complaining 
#' # about a missing argument.
#' # 
#' score <- verifyBrier(anen.ver, obs.ver, threshold, ensemble.func, split = threshold)
#' 
#' print(score)
#' 
#' if (all(c('ggplot2', 'reshape2') %in% installed.packages())) {
#'   # Use ggplot2
#'   library(reshape2)
#'   library(ggplot2)
#'   
#'   # Unpivot the score table
#'   colnames(score) <- c('Brier', 'Skill', "Reliability", 'Resolution')
#'   melted <- melt(data = score)
#'   colnames(melted) <- c('FLT', 'Type', 'Value')
#'   
#'   # Generate ggplot
#'   ggplot(data = melted) +
#'     theme_bw() + 
#'     geom_bar(mapping = aes(x = FLT, y = Value, group = Type, fill = Type),
#'              stat = 'identity', position = 'dodge') +
#'     scale_fill_brewer(palette = 'Paired', direction = -1) +
#'     geom_text(mapping = aes(x = FLT, y = Value, label = round(Value, digits = 3),
#'                             group = Type), position = position_dodge(0.9), vjust = -0.2) +
#'     labs(y = 'Score') +
#'     theme(legend.position = 'top')
#'   
#' } else {
#'   # Use the base graphics
#'   barplot(score[, 'bs'], ylab = 'Brier')
#' }
#' 
#' 
#' @md
#' @export
verifyBrier <- function(
  anen.ver, obs.ver, threshold,
  ensemble.func = stop('Ensemble function missing. If it is not an ensemble, set baseline = T'),
  baseline = F, ...) {
  
  check.package('verification')
  check.package('stringr')
  
  stopifnot(length(dim(anen.ver)) == 4)
  stopifnot(length(dim(obs.ver)) == 3)
  stopifnot(is.logical(baseline))
  
  if ( !identical(dim(anen.ver)[1:3], dim(obs.ver)[1:3]) ) {
    cat("Error: Observations and analogs have incompatible dimensions.\n")
    return(NULL)
  }
  
  # Convert each AnEn ensemble to a probability using the ensemble.func
  if (!baseline) {
    stopifnot(is.function(ensemble.func))
    stopifnot(dim(anen.ver)[4] > 1)
    
    # The anen.ver is an ensemble, apply ensemble function to it
    anen.ver <- apply(anen.ver, 1:3, ensemble.func, ...)
    
  } else {
    # The anen.ver is a deterministic baseline model, apply threshold to it
    stopifnot(dim(anen.ver)[4] == 1)
    anen.ver <- anen.ver >= threshold
    dim(anen.ver) <- dim(anen.ver)[-4]
  }
  
  # Convert observations to a binary variable using the threshold
  obs.ver <- obs.ver >= threshold
  
  num.flts <- dim(obs.ver)[3]
  ret.flts <- matrix(nrow = 0, ncol = 4)
  
  for (i.flt in 1:num.flts) {
    
    # Check for invalid values
    obs.ver.flt <- obs.ver[, , i.flt]
    anen.ver.flt <- anen.ver[, , i.flt]
    
    id <- is.finite(obs.ver.flt) & is.finite(anen.ver.flt)
    if (any(id)) {
      ret.flt <- verification::brier(obs = obs.ver.flt, pred = anen.ver.flt)
      stopifnot(all(ret.flt$check - ret.flt$bs < 1e-6))
      
      ret.flts <- rbind(ret.flts, c(ret.flt$bs, ret.flt$ss, ret.flt$bs.reliability, ret.flt$bs.resol))
    } else {
      ret.flts <- rbind(ret.flts, rep(NA, 4))
    }
  }
  
  # Compute the overall brier score for all lead times
  ret.flt <- verification::brier(
    obs = obs.ver, pred = anen.ver)
  stopifnot(all(ret.flt$check - ret.flt$bs < 1e-6))
  
  ret.flts <- rbind(ret.flts, c(
    ret.flt$bs, ret.flt$ss, ret.flt$bs.reliability, ret.flt$bs.resol))
  
  colnames(ret.flts) <- c('bs', 'ss', 'reliability', 'resol')
  rownames(ret.flts) <- c(paste('FLT', stringr::str_pad(1:num.flts, width = nchar(num.flts), pad = 0), sep = ''), 'All')
  
  return(ret.flts)
}
