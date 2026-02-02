#' Expected Value of Eliminating Causal Ambiguity (EVECA)
#'
#' Main function for computing the Expected Value of Eliminating Causal Ambiguity
#' using Bayesian model averaging framework.
#'
#' @param model_utilities Matrix where rows are decisions and columns are models,
#'   containing expected utilities for each decision under each model.
#'   Can also be a data frame that can be coerced to matrix.
#' @param model_probs Numeric vector of posterior probabilities for each model.
#'   Must sum to 1 (within floating point tolerance).
#' @param utility_function Function to apply to outcomes (default: identity).
#'   If provided, will be applied to `model_utilities` assuming they represent
#'   outcomes rather than utilities.
#'
#' @return A list with the following components:
#'   \item{bma_expected_utility}{Numeric vector of BMA expected utility for each decision.}
#'   \item{optimal_decision_bma}{Integer index of optimal decision under BMA.}
#'   \item{optimal_utility_bma}{Expected utility of optimal BMA decision.}
#'   \item{optimal_decisions_per_model}{Integer vector of optimal decision under each model.}
#'   \item{optimal_utilities_per_model}{Numeric vector of expected utility of optimal decision under each model.}
#'   \item{perfect_info_expected_utility}{Expected utility with perfect information about which model is true.}
#'   \item{evca}{Expected Value of Eliminating Causal Ambiguity.}
#'   \item{model_probs}{Original model probabilities (for reference).}
#'   \item{n_decisions}{Number of decision alternatives.}
#'   \item{n_models}{Number of competing causal models.}
#'
#' @export
#'
#' @examples
#' # Example with 3 decisions and 4 competing causal models
#' model_utilities <- matrix(
#'   c(
#'     10, 8, 12, 9,
#'     7, 9, 6, 8,
#'     11, 10, 8, 10
#'   ),
#'   nrow = 3, ncol = 4,
#'   dimnames = list(
#'     paste("Decision", 1:3),
#'     paste("Model", 1:4)
#'   )
#' )
#'
#' # Model probabilities from expert elicitation or data
#' model_probs <- c(0.3, 0.25, 0.25, 0.2)
#'
#' # Compute EVECA with risk-neutral utility (identity function)
#' result <- compute_evca(model_utilities, model_probs)
#'
#' # Print key results
#' result$evca
#' result$optimal_decision_bma
#'
#' # Compute EVECA with risk-averse utility
#' result_risk <- compute_evca(
#'   model_utilities,
#'   model_probs,
#'   utility_function = function(x) -exp(-0.1 * x)
#' )
#'
#' # Compare results
#' c(risk_neutral = result$evca, risk_averse = result_risk$evca)
compute_evca <- function(model_utilities, model_probs,
                         utility_function = identity) {
  # Convert to matrix if needed
  if (!is.matrix(model_utilities)) {
    model_utilities <- as.matrix(model_utilities)
  }

  # Apply utility function if provided (assuming model_utilities are outcomes)
  if (!identical(utility_function, identity)) {
    model_utilities <- apply(model_utilities, c(1, 2), utility_function)
  }

  # Basic dimensions
  n_decisions <- nrow(model_utilities)
  n_models <- ncol(model_utilities)

  # Validate inputs
  if (length(model_probs) != n_models) {
    stop(
      "Number of model probabilities (", length(model_probs), ") ",
      "must equal number of model utility columns (", n_models, ")"
    )
  }

  if (abs(sum(model_probs) - 1) > 1e-10) {
    stop("Model probabilities must sum to 1 (within 1e-10 tolerance)")
  }

  if (any(model_probs < 0)) {
    stop("Model probabilities must be non-negative")
  }

  # Compute BMA expected utility
  bma_eu <- bma_expected_utility(model_utilities, model_probs)

  # Optimal decision under BMA
  optimal_decision_bma <- which.max(bma_eu)
  optimal_utility_bma <- bma_eu[optimal_decision_bma]

  # Optimal decision under each model
  optimal_decisions_per_model <- apply(model_utilities, 2, which.max)
  optimal_utilities_per_model <- numeric(n_models)

  for (i in seq_len(n_models)) {
    optimal_utilities_per_model[i] <- model_utilities[optimal_decisions_per_model[i], i]
  }

  # Expected utility with perfect information
  perfect_info_eu <- sum(model_probs * optimal_utilities_per_model)

  # EVECA calculation: difference between perfect information and BMA
  evca <- perfect_info_eu - optimal_utility_bma

  # Return comprehensive results
  list(
    bma_expected_utility = bma_eu,
    optimal_decision_bma = optimal_decision_bma,
    optimal_utility_bma = optimal_utility_bma,
    optimal_decisions_per_model = optimal_decisions_per_model,
    optimal_utilities_per_model = optimal_utilities_per_model,
    perfect_info_expected_utility = perfect_info_eu,
    evca = evca,
    model_probs = model_probs,
    n_decisions = n_decisions,
    n_models = n_models
  )
}
