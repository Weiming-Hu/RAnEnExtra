#  "`-''-/").___..--''"`-._
# (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
# (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#   _ ..`--'_..-_/  /--'_.' ,'
# (il),-''  (li),'  ((!.-'
#
#
# Author: Weiming Hu <weiming@psu.edu>
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University
#

#' RAnEnExtra::organizeFLTs
#'
#' RAnEnExtra::organizeFLTs organize the verification results into a flat table format so that
#' it is easy to plot with ggplot. Currently, this function only knows how to organize a portion
#' of the verification metrics.
#'
#' @author Weiming Hu \email{weiming@@psu.edu}
#'
#'
#' @param results The results from RAnEnExtra::verify. It should be a list.
#' @param flts The forecast lead times.
#' @param boot Whether `boot` is used during the verification process
#'
#' @return a data frame or a data table
#'
#' @md
#' @export
organizeFLTs <- function(results, flts, boot = FALSE) {

  if (boot) {
    stop('Organizing results generated with bootstrap is currently not implemented.')
  }

  # I know how to organize the following verification metrics
  get_flts <- c('Bias', 'Correlation', 'Dispersion', 'MAE', 'RMSE', 'Spread', 'SpreadSkill', 'CRMSE')

  if (requireNamespace('data.table', quietly = T)) {
    use_data_table <- TRUE
  } else {
    use_data_table <- FALSE
  }

  if (use_data_table) {
    df <- data.table::data.table()
  } else {
    df <- data.frame()
  }

  unknown_metrics <- c()

  for (method in names(results)) {
    for (metric in names(results[[method]])) {

      if (metric %in% get_flts) {
        if (use_data_table) {
          df_single <- data.table::data.table(x = flts, Method = method, Metric = metric,
                                              y = verification[[method]][[metric]]$flt)

        } else {
          df_single <- data.frame(x = flts, Method = method, Metric = metric,
                                  y = verification[[method]][[metric]]$flt)

        }

        df <- rbind(df, df_single)
      } else {
        unknown_metrics <- c(unknown_metrics, metric)
      }

    }
  }

  if (length(unknown_metrics) > 0) {
    msg <- paste("These metrics are ignored:", paste(unknown_metrics, collapse = ', '))
    warning(msg)
  }

  return(df)
}
