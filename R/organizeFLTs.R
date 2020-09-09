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
#' @param flts The forecast lead times to be copied to the data frame.
#' @param parse_metrics The metrics to organize into a data frame. Default to all supported metrics.
#'
#' @return a data frame or a data table
#'
#' @md
#' @export
organizeFLTs <- function(results, flts, parse_metrics = NULL) {

  # I know how to organize the following verification metrics
  known_metrics <- c('Bias', 'Correlation', 'Dispersion', 'MAE',
                     'RMSE', 'Spread', 'SpreadSkill', 'CRMSE',
                     'CRPS', 'Brier')

  if (is.null(parse_metrics)) {
    parse_metrics <- known_metrics
  } else {
    if (!all(parse_metrics %in% known_metrics)) {
      msg <- paste0('Unknown metrics specified! I only know how to parse these metrics:\n', paste(known_metrics, collapse = ', '))
      stop(msg)
    }
  }

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
      if (metric %in% parse_metrics) {
        if (metric %in% known_metrics) {

          if (metric == 'Brier') {

            ##############
            # Parse Brier #
            ##############
            #
            # Brier has different member names.
            # It does not have an option for bootstrap.
            #

            values <- results[[method]][[metric]]$bs[1, ]

            # Remove the last row because that is the brier score for all lead times
            values <- values[-length(values)]
            stopifnot(length(values) == length(flts))

            if (use_data_table) {
              df_single <- data.table::data.table(x = flts, Method = method,
                                                  Metric = metric, y = values,
                                                  floor = NA, ceiling = NA)
            } else {
              df_single <- data.frame(x = flts, Method = method,
                                      Metric = metric, y = values,
                                      floor = NA, ceiling = NA)
            }

          } else if (metric == 'CRPS') {

            ##############
            # Parse CRPS #
            ##############
            #
            # CRPS has different member names
            #

            if ('crps.flt' %in% names(results[[method]][[metric]])) {
              variable_has_boot <- F
              crps_name <- 'crps.flt'
            } else if ('crps.boot.flt' %in% names(results[[method]][[metric]])) {
              variable_has_boot <- T
              crps_name <- 'crps.boot.flt'
            } else {
              stop('Required names in CRPS results are not found. This is fatal!')
            }

            if (use_data_table) {
              df_single <- data.table::data.table(x = flts, Method = method, Metric = metric,
                                                  y = results[[method]][[metric]][[crps_name]][1, ])

            } else {
              df_single <- data.frame(x = flts, Method = method, Metric = metric,
                                      y = results[[method]][[metric]][[crps_name]][1, ])
            }

            if (variable_has_boot) {
              df_single$floor <- results[[method]][[metric]][[crps_name]][2, ]
              df_single$ceiling <- results[[method]][[metric]][[crps_name]][3, ]
            } else {
              df_single$floor <- rep(NA, ncol(results[[method]][[metric]][[crps_name]]))
              df_single$ceiling <- rep(NA, ncol(results[[method]][[metric]][[crps_name]]))
            }

          } else {

            #########################
            # Parse other variables #
            #########################
            #
            # All other variables share the same member names
            #

            if (nrow(results[[method]][[metric]]$mat) == 3 &&
                ncol(results[[method]][[metric]]$mat) == length(results[[method]][[metric]]$flt)) {
              variable_has_boot <- T
            } else {
              variable_has_boot <- F
            }

            if (use_data_table) {
              df_single <- data.table::data.table(x = flts, Method = method, Metric = metric,
                                                  y = results[[method]][[metric]]$flt)

            } else {
              df_single <- data.frame(x = flts, Method = method, Metric = metric,
                                      y = results[[method]][[metric]]$flt)
            }

            if (variable_has_boot) {
              df_single$floor <- results[[method]][[metric]]$mat[2, ]
              df_single$ceiling <- results[[method]][[metric]]$mat[3, ]
            } else {
              df_single$floor <- rep(NA, length(results[[method]][[metric]]$flt))
              df_single$ceiling <- rep(NA, length(results[[method]][[metric]]$flt))
            }
          }

          df <- rbind(df, df_single)

        } else {
          unknown_metrics <- c(unknown_metrics, metric)
        }
      }
    }
  }

  # Remove columns that only contain NA values
  if (all(is.na(df$ceiling))) df$ceiling <- NULL
  if (all(is.na(df$floor))) df$floor <- NULL

  if (length(unknown_metrics) > 0) {
    msg <- paste("These metrics are ignored:", paste(unknown_metrics, collapse = ', '))
    warning(msg)
  }

  return(df)
}
