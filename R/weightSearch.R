#' RAnEnExtra::weightSearch
#'
#' RAnEnExtra::weightSearch is a grid search function for weight optimization.
#'
#' @author Alon Sidel \email{ays10@@psu.edu}
#' @author Weiming Hu \email{weiming@@psu.edu}
#'
#' @param weights Either a numeric matrix or a single numeral. If it is a matrix, each row is a certain weight combination and the number
#' of rows equal to the number of iterations; if it is a single numeral, it is the number of iterations and the weights will be randomly generated.
#' @param forecasts Forecasts for `RAnEn::generateAnalogs.Forecasts`
#' @param observations Observations for `RAnEn::generateAnalogs.Forecasts`
#' @param test.times Test times for `RAnEn::generateAnalogs.Forecasts`
#' @param search.times Search times for `RAnEn::generateAnalogs.Forecasts`
#' @param config Config for `RAnEn::generateAnalogs.Forecasts`
#' @param metric The metric for `RAnEnExtra::verify`. Please note two things: (1) please make sure the member `mean` exists in the verification
#' results because it is used to evaluate different combinations of weights; (2) it is assumed that the metric is an error measurement so that
#' the lowest metric is considered the best.
#' @param return.best.only Whether to only return the best combination of weights
#'
#' @return Either a vector or a matrix. If a vector is returned, it is the best combination of weights; if a matrix is returned, it is the
#' combinations of weights that have been searched and the last column is the verification metric.
#'
#' @md
#' @export
weightSearch <- function(weights, forecasts, observations, test.times, search.times, config, metric = 'RMSE', return.best.only = F) {

  ##########
  # Set up #
  ##########

  # By default, the observation ID is 0. Change this to 1.
  if (config$observation_id == 0) {
    config$observation_id = config$observation_id + 1
  }

  # Avoid too many output
  config$verbose <- 1

  # Align observations for verification
  cat('Aligning observations for verification ...\n')
  obs.aligned <- RAnEn::alignObservations(observations$Data[config$observation_id, , , drop = F], observations$Times, test.times, forecasts$FLTs)
  dim(obs.aligned) <- dim(obs.aligned)[-1]

  # Defien an iteration function
  iteration <- function(weight.row, weights, forecasts, observations, test.times, search.times, config, metric, obs.aligned) {

    # Change weight
    config$weights <- weights[weight.row, ]

    # Generate analog
    AnEn <- generateAnalogs(forecasts, observations, test.times, search.times, config)

    # Verification
    results <- RAnEnExtra::verify(metric, obs.ver = obs.aligned, anen.ver = AnEn$analogs, verbose = F)

    # The mean statistic is used to summarize the verification
    return(results[[metric]]$mean)
  }

  # The argument, weights, can be either a matrix or a numeric number
  if (is.matrix(weights)) {

    # Weights are provided
    stopifnot(ncol(weights) == length(forecasts$ParameterNames))

  } else if (is.numeric(weights) && length(weights) == 1) {

    # Weights are randomly generated and only the number of iteration is generated.
    random_weights <- matrix(runif(length(forecasts$ParameterNames) * weights), nrow = weights)
    weights <- random_weights / rowSums(random_weights)

  } else {
    stop('Weights should either be a matrix or a single numeral for the number of iterations to try (weights will be randomly generated)')
  }

  ###############
  # Grid search #
  ###############

  # Calculate verification for each weight combination
  cat('Grid searching ...\n')
  verification <- pbapply::pbsapply(1:nrow(weights), iteration, weights = weights, forecasts = forecasts, observations = observations,
                                    test.times = test.times, search.times = search.times, config = config, metric = metric, obs.aligned = obs.aligned)

  cat('Grid search complete!\n')

  if (return.best.only) {

    # This is the best case
    best.row <- which.min(verification)

    return(weights[best.row, ])

  } else {

    # Add verification to weight matrix
    weights <- cbind(weights, verification)

    return(weights)
  }
}
