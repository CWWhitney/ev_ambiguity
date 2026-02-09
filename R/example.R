#' Example Analysis for Expected Value of Eliminating Causal Ambiguity
#'
#' Generate example data for decision problems with competing causal models
#' and run a demonstration EVECA analysis. This function creates realistic
#' examples for testing and understanding EVECA in various modeling contexts.
#'
#' @param n_decisions Number of decision alternatives (default: 3).
#' @param n_models Number of competing causal models (default: 4).
#' @param seed Random seed for reproducibility (default: 123).
#' @param utility_range Range for generating utility values (default: c(0, 10)).
#'
#' @return A list with the following components:
#'   \item{evca_result}{Output from `compute_evca()` function.}
#'   \item{model_utilities}{Matrix of model utilities used in analysis.}
#'   \item{model_probs}{Vector of model probabilities used in analysis.}
#'   \item{decision_names}{Names of decision alternatives.}
#'   \item{model_names}{Names of competing models.}
#'
#' @export
#' @importFrom stats rnorm runif
#'
#' @examples
#' # Run example analysis with default parameters
#' example_result <- example_evca()
#'
#' # Print key results
#' cat("EVECA:", example_result$evca_result$evca, "\n")
#' optimal_idx <- example_result$evca_result$optimal_decision_bma
#' cat("Optimal decision:", example_result$decision_names[optimal_idx], "\n")
#'
#' # Run example with custom parameters
#' custom_result <- example_evca(
#'   n_decisions = 4,
#'   n_models = 3,
#'   seed = 456
#' )
#'
#' # Access components
#' print(custom_result$model_utilities)
#' print(custom_result$model_probs)
example_evca <- function(n_decisions = 3,
                         n_models = 4,
                         seed = 123,
                         utility_range = c(0, 10)) {
  set.seed(seed)

  # Generate decision and model names
  decision_names <- paste("Decision", 1:n_decisions)
  model_names <- paste("Model", 1:n_models)

  # Generate model utilities matrix
  # Each row is a decision, each column is a model
  model_utilities <- matrix(
    stats::runif(n_decisions * n_models, min = utility_range[1], max = utility_range[2]),
    nrow = n_decisions,
    ncol = n_models,
    dimnames = list(decision_names, model_names)
  )

  # Add some structure: make some decisions better under certain models
  for (j in 1:n_models) {
    # Each model favors a different decision (with some noise)
    favored_decision <- ((j - 1) %% n_decisions) + 1
    model_utilities[favored_decision, j] <- model_utilities[favored_decision, j] +
      stats::runif(1, 2, 4)
  }

  # Generate model probabilities
  # Start with random values and normalize
  model_probs <- stats::runif(n_models, min = 0.1, max = 1)
  model_probs <- model_probs / sum(model_probs)
  names(model_probs) <- model_names

  # Compute EVECA
  evca_result <- compute_evca(
    model_utilities = model_utilities,
    model_probs = model_probs
  )

  # Return comprehensive results
  list(
    evca_result = evca_result,
    model_utilities = model_utilities,
    model_probs = model_probs,
    decision_names = decision_names,
    model_names = model_names
  )
}


#' Example with Multi-Dimensional Outcomes
#'
#' Generate example with explicit outcome dimensions (before utility transformation).
#' Useful for demonstrating multi-attribute utility and trade-offs.
#'
#' @param n_decisions Number of decision alternatives (default: 3).
#' @param n_models Number of competing causal models (default: 4).
#' @param n_outcomes Number of outcome dimensions (default: 2).
#' @param outcome_weights Vector of weights for multi-attribute utility function
#'   (default: equal weights). Must sum to 1.
#' @param seed Random seed for reproducibility (default: 123).
#' @param outcome_range Range for generating outcome values (default: c(0, 1)).
#'
#' @return A list with the following components:
#'   \item{evca_result}{Output from `compute_evca()` function.}
#'   \item{model_outcomes}{Array of outcomes with dimensions: decisions x models x outcomes.}
#'   \item{model_utilities}{Matrix of utilities computed from outcomes.}
#'   \item{model_probs}{Vector of model probabilities.}
#'   \item{outcome_weights}{Outcome weights used in multi-attribute utility.}
#'   \item{decision_names}{Names of decision alternatives.}
#'   \item{model_names}{Names of competing models.}
#'   \item{outcome_names}{Names of outcome dimensions.}
#'
#' @export
#' @importFrom stats rnorm runif
#'
#' @examples
#' # Run example with multi-dimensional outcomes
#' example_result <- example_evca_multidim()
#'
#' # Print key results
#' cat("EVECA:", example_result$evca_result$evca, "\n")
#' optimal_idx <- example_result$evca_result$optimal_decision_bma
#' cat("Optimal decision:", example_result$decision_names[optimal_idx], "\n")
#'
#' # Example with custom weights (e.g., prioritizing first outcome)
#' weighted_result <- example_evca_multidim(
#'   n_outcomes = 3,
#'   outcome_weights = c(0.6, 0.3, 0.1)
#' )
#'
#' # Access multi-dimensional outcomes
#' print(weighted_result$model_outcomes)
example_evca_multidim <- function(n_decisions = 3,
                                  n_models = 4,
                                  n_outcomes = 2,
                                  outcome_weights = NULL,
                                  seed = 123,
                                  outcome_range = c(0, 1)) {
  set.seed(seed)

  # Set default outcome weights if not provided
  if (is.null(outcome_weights)) {
    outcome_weights <- rep(1 / n_outcomes, n_outcomes)
  }

  # Validate outcome weights
  if (length(outcome_weights) != n_outcomes) {
    stop("Length of outcome_weights must equal n_outcomes")
  }

  if (any(outcome_weights < 0)) {
    stop("Outcome weights must be non-negative")
  }

  if (abs(sum(outcome_weights) - 1) > 1e-10) {
    stop("Outcome weights must sum to 1 (within 1e-10 tolerance)")
  }

  # Generate names
  decision_names <- paste("Decision", 1:n_decisions)
  model_names <- paste("Model", 1:n_models)
  outcome_names <- paste("Outcome", 1:n_outcomes)

  # Generate outcome array: decisions x models x outcomes
  model_outcomes <- array(
    stats::runif(n_decisions * n_models * n_outcomes,
      min = outcome_range[1],
      max = outcome_range[2]
    ),
    dim = c(n_decisions, n_models, n_outcomes),
    dimnames = list(decision_names, model_names, outcome_names)
  )

  # Add structure: create trade-offs between outcomes
  for (j in 1:n_models) {
    favored_decision <- ((j - 1) %% n_decisions) + 1
    # Boost outcomes for favored decision under this model
    for (k in 1:n_outcomes) {
      model_outcomes[favored_decision, j, k] <- model_outcomes[favored_decision, j, k] +
        stats::runif(1, 0.3, 0.5)
    }
  }

  # Generate model probabilities
  model_probs <- stats::runif(n_models, min = 0.1, max = 1)
  model_probs <- model_probs / sum(model_probs)
  names(model_probs) <- model_names

  # Compute multi-attribute utilities
  model_utilities <- compute_multi_attribute_utilities(
    model_outcomes = model_outcomes,
    outcome_weights = outcome_weights
  )

  # Compute EVECA
  evca_result <- compute_evca(
    model_utilities = model_utilities,
    model_probs = model_probs
  )

  # Return comprehensive results
  list(
    evca_result = evca_result,
    model_outcomes = model_outcomes,
    model_utilities = model_utilities,
    model_probs = model_probs,
    outcome_weights = outcome_weights,
    decision_names = decision_names,
    model_names = model_names,
    outcome_names = outcome_names
  )
}


#' Compute Multi-Attribute Utilities
#'
#' Helper function to compute utilities from multi-dimensional outcomes
#' using weighted linear combination.
#'
#' @param model_outcomes Array of outcomes with dimensions: decisions x models x outcomes.
#' @param outcome_weights Vector of weights for multi-attribute utility function.
#'   Must sum to 1.
#'
#' @return Matrix of utilities with dimensions: decisions x models.
#'
#' @keywords internal
compute_multi_attribute_utilities <- function(model_outcomes, outcome_weights) {
  n_decisions <- dim(model_outcomes)[1]
  n_models <- dim(model_outcomes)[2]
  n_outcomes <- dim(model_outcomes)[3]

  # Validate
  if (length(outcome_weights) != n_outcomes) {
    stop("Length of outcome_weights must equal number of outcome dimensions")
  }

  utilities <- matrix(NA, nrow = n_decisions, ncol = n_models)
  rownames(utilities) <- dimnames(model_outcomes)[[1]]
  colnames(utilities) <- dimnames(model_outcomes)[[2]]

  # Compute weighted sum for each decision-model combination
  for (i in 1:n_decisions) {
    for (j in 1:n_models) {
      utilities[i, j] <- sum(outcome_weights * model_outcomes[i, j, ])
    }
  }

  return(utilities)
}
