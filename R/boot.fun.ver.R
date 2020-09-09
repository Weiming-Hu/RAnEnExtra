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

#' RAnEnExtra::boot.fun.ver
#'
#' @keywords internal
boot.fun.ver <- function( temp, R, fun = function(d,i){ mean( d[i,1] ) }) {
  temp <- data.frame( val = na.omit( temp ) )
  boot <- rep(NA,3)

  check.package('boot')

  if( nrow(temp) > 1 ) {

    err.boot   <- boot::boot(temp, fun, R=R)

    if (length(unique(temp[, 1])) == 1) {
      # If all values equal
      boot[1] <- boot[2] <- boot[3] <- err.boot$t0

    } else {
      # If values do not equal
      conf <- getOption('RAnEnExtra_boot_conf')
      if (is.null(conf)) conf <- 0.95

      bootresult <- boot::boot.ci(err.boot, type="basic", conf = conf)

      if ( !is.na(err.boot[[1]])) {
        boot[1] <- bootresult$t0
        boot[2] <- bootresult$basic[4]
        boot[3] <- bootresult$basic[5]
      }
    }
  }

  return(boot)
}
