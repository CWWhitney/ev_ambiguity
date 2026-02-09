#' Bayesian Model Averaging for Expected Value of Eliminating Causal Ambiguity
#'
#' Helper function for computing Bayesian model average expected utilities.
#' Used internally by `compute_evca()`.
#'
#' @param model_utilities Matrix where rows are decisions and columns are models,
#'   containing expected utilities for each decision under each model.
#'   Can also be a data frame that can be coerced to matrix.
#' @param model_probs Numeric vector of posterior probabilities for each model.
#'   Must sum to 1 (within floating point tolerance).
#'
#' @return Numeric vector of BMA expected utilities for each decision,
#'   with length equal to `nrow(model_utilities)`.
#'
#' @keywords internal
bma_expected_utility <- function(model_utilities, model_probs) {
  # Convert to matrix if needed
  if (!is.matrix(model_utilities)) {
    model_utilities <- as.matrix(model_utilities)
  }

  # Validate inputs
  if (length(model_probs) != ncol(model_utilities)) {
    stop(
      "Number of model probabilities (", length(model_probs), ") ",
      "must equal number of model utility columns (", ncol(model_utilities), ")"
    )
  }

  if (any(model_probs < 0)) {
    stop("Model probabilities must be non-negative")
  }

  if (abs(sum(model_probs) - 1) > 1e-10) {
    stop("Model probabilities must sum to 1 (within 1e-10 tolerance)")
  }

  # Compute weighted average: matrix multiplication
  as.vector(model_utilities %*% model_probs)
}
