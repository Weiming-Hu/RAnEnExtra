#' tournament.selection implements the tournament selection
#' algorithm
#'
#' @param fitness fitness value
#'
#' @param num.samples number of samples to select
#'
#' @param num.champions number of champions to choose from
#' each tournament
#'
#' @param tournament.size number of candidates to choose
#' from the population
#'
#' @param replacement whether to select samples with
#' replacement or not
#'
#' @return a vector with indices for the selected samples
#'
#' @export
tournament.selection <- function (
  fitness, num.samples, num.champions = 1,
  tournament.size = 2, replacement = F) {

  # check parameters
  if (!is.vector(fitness)) {
    stop("fitness should be a vector")
  }
  if (length(tournament.size) != 1 ||
      class(tournament.size) != 'numeric') {
    stop('tournament.size should be a single number')
  }
  if (length(num.champions) != 1 ||
      class(num.champions) != 'numeric') {
    stop('num.champions should be a single number')
  }
  if (num.champions > tournament.size) {
    warning(paste('num.champions can not be bigger than',
                  'the tournament.size. Set them to be the same.',
                  sep = ''))
    num.champions = tournament.size
  }

  # tournament selection
  samples <- vector(mode = 'numeric')
  elements <- 1:length(fitness)
  while (length(samples) < num.samples) {

    if (length(elements) < tournament.size) {
      warning(paste("Not enough elements to choose from.",
                    "Please try with replacement"))
      break
    }
    indices <- sample(x = elements, tournament.size)
    champions <- indices[order(fitness[indices],
                               decreasing = T)[1:num.champions]]
    samples <- c(samples, champions)
    if (!replacement) {
      elements <- elements[!elements %in% champions]
    }
  }

  if (length(samples) > num.samples) {
    samples <- samples[1:num.samples]
  }
  return(samples)
}
