#  "`-''-/").___..--''"`-._
# (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
# (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#   _ ..`--'_..-_/  /--'_.' ,'
# (il),-''  (li),'  ((!.-'
#
#
# Author: Guido Cervone (cervone@psu.edu), Martina Calovi (mxc895@psu.edu), Laura Clemente-Harding (laura@psu.edu)
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University
#

#' RAnEnExtra::verifyDispersion
#' 
#' RAnEnExtra::verifyDispersion calculates dispersion.
#' 
#' @author Guido Cervone \email{cervone@@psu.edu}
#' @author Martina Calovi \email{mxc895@@psu.edu}
#' @author Laura Clemente-Harding \email{laura@@psu.edu}
#' 
#' @param anen.ver A 4-dimensional array. This array is usually created from the `value` column of
#' the `analogs` member in the results of `RAnEn::generateAnalogs`. The dimensions should be
#' `[stations, times, lead times, members]`.
#' @param obs.ver A 3-dimensional array. The dimensions should be `[stations, times, lead times]`.
#' You can generate the array using `RAnEn::alignObservations`.
#' @param boot Whether to use bootstrap.
#' @param R The number of bootstrap replicates. Used by the function `boot::boot`.
#' @param na.rm Whether to remove NA values.
#' 
#' @export
verifyDispersion <- function(anen.ver, obs.ver, boot=F, R=1000, na.rm=T) {
  
  stopifnot(length(dim(anen.ver)) == 4)
  stopifnot(length(dim(obs.ver)) == 3)
  
  rmse <- rmse.ver(anen.ver, obs.ver, boot, R, na.rm)
  spread <- spread.ver( anen.ver, na.rm)
  
  return(list(mean=rmse$mean, flt=rbind( rmse$flt, spread$flt), mat=rmse$mat) )
}
