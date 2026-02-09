#' Bayesian Model Averaging for Expected Utilities
#'
#' Computes Bayesian model average expected utilities across competing causal
#' models. This function calculates the weighted average of utilities where
#' weights are the posterior probabilities of each model being correct.
#'
#' @usage bma_expected_utility(model_utilities, model_probs)
#'
#' @param model_utilities Matrix where rows are decisions and columns are models,
#'   containing expected utilities for each decision under each model.
#'   Can also be a data frame that can be coerced to matrix. Each cell \[i,j\]
#'   represents the expected utility of decision i if model j is true.
#' @param model_probs Numeric vector of posterior probabilities for each model.
#'   Must have length equal to number of columns in model_utilities.
#'   Must be non-negative and sum to 1 (within 1e-10 tolerance).
#'
#' @return Numeric vector of BMA expected utilities for each decision,
#'   with length equal to nrow(model_utilities). Each element represents
#'   the expected utility of that decision averaged across all models
#'   weighted by model probabilities.
#'
#' @details
#' Bayesian Model Averaging (BMA) provides a coherent framework for making
#' decisions under model uncertainty. For each decision d, the BMA expected
#' utility is calculated as:
#'
#' EU_BMA(d) = sum over all models k of: P(M_k) * EU(d|M_k)
#'
#' where P(M_k) is the probability that model k is correct and EU(d|M_k)
#' is the expected utility of decision d given model k.
#'
#' This function uses efficient matrix multiplication: model_utilities %*% model_probs
#'
#' Computational complexity is O(D × M) where D is the number of decisions
#' and M is the number of models.
#'
#' @section Warning:
#' This is an internal function used by compute_evca(). While exported for
#' flexibility, most users should use compute_evca() instead.
#'
#' Model probabilities are validated in a specific order:
#' 1. Non-negative values
#' 2. Sum to 1
#' Ensure probabilities meet the first constraint before checking the second.
#'
#' @references
#' Hoeting, J. A., Madigan, D., Raftery, A. E., & Volinsky, C. T. (1999).
#' Bayesian model averaging: a tutorial. Statistical Science, 14(4), 382-401.
#' @references
#' Claxton, K. (1999). The irrelevance of inference: a decision-making approach
#' to the stochastic evaluation of health care technologies. Journal of Health
#' Economics, 18(3), 341-364.
#'
#' @keywords models decision-analysis utilities internal
#'
#' @examples
#' # Example 1: Simple case with 2 decisions and 2 models
#' model_utilities <- matrix(
#'   c(
#'     10, 5, # Decision 1: utility 10 under M1, 5 under M2
#'     6, 9
#'   ), # Decision 2: utility 6 under M1, 9 under M2
#'   nrow = 2, ncol = 2, byrow = TRUE
#' )
#' model_probs <- c(0.5, 0.5) # Equal probability for both models
#'
#' bma_eu <- bma_expected_utility(model_utilities, model_probs)
#' print(bma_eu) # Should be c(7.5, 7.5)
#'
#' # Example 2: Unequal model probabilities
#' model_probs_unequal <- c(0.7, 0.3)
#' bma_eu_unequal <- bma_expected_utility(model_utilities, model_probs_unequal)
#' print(bma_eu_unequal) # Decision 1: 0.7*10 + 0.3*5 = 8.5
#' # Decision 2: 0.7*6 + 0.3*9 = 6.9
#'
#' # Example 3: Larger example with 3 decisions and 4 models
#' model_utilities_large <- matrix(
#'   c(
#'     8, 6, 9, 7,
#'     7, 9, 5, 8,
#'     10, 7, 6, 9
#'   ),
#'   nrow = 3, ncol = 4, byrow = TRUE
#' )
#' model_probs_large <- c(0.3, 0.3, 0.2, 0.2)
#'
#' bma_eu_large <- bma_expected_utility(model_utilities_large, model_probs_large)
#' print(bma_eu_large)
#'
#' # Example 4: Using with data frame input
#' utility_df <- data.frame(
#'   Model1 = c(10, 6),
#'   Model2 = c(5, 9)
#' )
#' bma_eu_df <- bma_expected_utility(utility_df, c(0.5, 0.5))
#' print(bma_eu_df)
#'
#' @export
#'
bma_expected_utility <- function(model_utilities, model_probs) {
  # Input validation ----

  # Convert to matrix if needed
  if (!is.matrix(model_utilities)) {
    if (is.data.frame(model_utilities)) {
      model_utilities <- as.matrix(model_utilities)
    } else {
      stop(
        "model_utilities must be a matrix or data frame. ",
        "Received object of class: ", class(model_utilities)[1]
      )
    }
  }

  # Check model_probs is numeric
  if (!is.numeric(model_probs)) {
    stop("model_probs must be a numeric vector")
  }

  # Check dimensions match
  if (length(model_probs) != ncol(model_utilities)) {
    stop(
      "Number of model probabilities (", length(model_probs), ") ",
      "must equal number of model utility columns (", ncol(model_utilities), ")"
    )
  }

  # Check probabilities are non-negative (must come before sum check)
  if (any(model_probs < 0)) {
    stop("Model probabilities must be non-negative")
  }

  # Check probabilities sum to 1
  if (abs(sum(model_probs) - 1) > 1e-10) {
    stop(
      "Model probabilities must sum to 1 (within 1e-10 tolerance). ",
      "Current sum: ", round(sum(model_probs), 10)
    )
  }

  # Check for missing values in utilities
  if (any(is.na(model_utilities))) {
    stop(
      "model_utilities contains NA values. ",
      "Please remove or impute missing values before computing BMA."
    )
  }

  # Check for missing values in probabilities
  if (any(is.na(model_probs))) {
    stop("model_probs contains NA values")
  }

  # Compute BMA expected utility ----

  # Matrix multiplication: each decision's utility is weighted average across models
  # Result is a vector of length nrow(model_utilities)
  bma_eu <- as.vector(model_utilities %*% model_probs)

  # Return ----
  return(bma_eu)
}
