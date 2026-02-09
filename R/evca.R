#' Expected Value of Eliminating Causal Ambiguity (EVECA)
#'
#' Computes the Expected Value of Eliminating Causal Ambiguity using Bayesian
#' model averaging framework. EVECA quantifies the maximum value of resolving
#' structural uncertainty about which causal model is correct among competing
#' alternatives.
#'
#' @usage compute_evca(model_utilities, model_probs, utility_function = identity)
#'
#' @param model_utilities Matrix where rows are decisions and columns are models,
#'   containing expected utilities for each decision under each model.
#'   Can also be a data frame that can be coerced to matrix. Each cell \[i,j\]
#'   represents the expected utility of decision i if model j is the true
#'   causal model.
#' @param model_probs Numeric vector of posterior probabilities for each model.
#'   Must have length equal to number of columns in model_utilities.
#'   Must be non-negative and sum to 1 (within 1e-10 tolerance).
#' @param utility_function Function to apply to outcomes (default: identity).
#'   If provided, will be applied to model_utilities assuming they represent
#'   outcomes rather than utilities. Common examples include risk-averse
#'   utility functions like exponential utility: function(x) -exp(-r*x).
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
#' @details
#' The Expected Value of Eliminating Causal Ambiguity (EVECA) extends traditional
#' value of information analysis to handle structural uncertainty. It quantifies
#' the value of definitively identifying the correct causal model.
#'
#' EVECA is calculated as:
#' EVECA = E(max utility with perfect info) - max(E(utility under ambiguity))
#'       = sum_k P(M_k) * max_d EU(d|M_k) - max_d sum_k P(M_k) * EU(d|M_k)
#'
#' The first term represents expected utility if we knew which model is true
#' (we would choose the optimal decision for that model). The second term is
#' the expected utility of the optimal decision under current ambiguity using
#' Bayesian Model Averaging.
#'
#' Interpretation:
#' - EVECA = 0: No value to resolving ambiguity (same decision optimal under all models)
#' - EVECA > 0: Value to identifying correct model (different models favor different decisions)
#' - High EVECA: Research to identify correct model likely worthwhile
#' - Low EVECA: Decision is robust across models; proceed without additional research
#'
#' Computational complexity is O(D × M) where D is the number of decisions
#' and M is the number of models.
#'
#' @section Warning:
#' Model probabilities must sum to exactly 1 (within 1e-10 tolerance).
#' All utility values should be on the same scale for meaningful comparison.
#' If using a utility function, ensure it preserves the preference ordering
#' and is appropriate for the decision context.
#'
#' When comparing EVECA to research costs, ensure both are in the same units
#' (e.g., dollars, utility points).
#'
#' @references
#' Claxton, K. (1999). The irrelevance of inference: a decision-making approach
#' to the stochastic evaluation of health care technologies. Journal of Health
#' Economics, 18(3), 341-364.
#' @references
#' Yokota, F., & Thompson, K. M. (2004). Value of information analysis in
#' environmental health risk management decisions: past, present, and future.
#' Risk Analysis, 24(3), 635-650.
#' @references
#' Hoeting, J. A., Madigan, D., Raftery, A. E., & Volinsky, C. T. (1999).
#' Bayesian model averaging: a tutorial. Statistical Science, 14(4), 382-401.
#'
#' @keywords models decision-analysis value-of-information
#'
#' @examples
#' # Example 1: Basic usage with 3 decisions and 4 competing causal models
#' model_utilities <- matrix(
#'   c(
#'     10, 8, 12, 9,
#'     7, 9, 6, 8,
#'     11, 10, 8, 10
#'   ),
#'   nrow = 3, ncol = 4, byrow = TRUE,
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
#' print(result$evca)
#' print(result$optimal_decision_bma)
#'
#' # Example 2: Making a decision based on EVECA
#' research_cost <- 1.5
#'
#' if (result$evca > research_cost) {
#'   cat("Decision: INVEST IN RESEARCH\n")
#'   cat("Rationale: EVECA (", result$evca, ") > research cost (", research_cost, ")\n")
#' } else {
#'   cat("Decision: PROCEED with decision", result$optimal_decision_bma, "\n")
#'   cat("Rationale: EVECA (", result$evca, ") < research cost (", research_cost, ")\n")
#' }
#'
#' # Example 3: Compute EVECA with risk-averse utility
#' result_risk <- compute_evca(
#'   model_utilities,
#'   model_probs,
#'   utility_function = function(x) -exp(-0.1 * x)
#' )
#'
#' # Compare results
#' cat("Risk-neutral EVECA:", result$evca, "\n")
#' cat("Risk-averse EVECA:", result_risk$evca, "\n")
#'
#' # Example 4: Using with data frame input
#' utility_df <- data.frame(
#'   Model1 = c(10, 7, 11),
#'   Model2 = c(8, 9, 10),
#'   Model3 = c(12, 6, 8),
#'   Model4 = c(9, 8, 10)
#' )
#' rownames(utility_df) <- paste("Decision", 1:3)
#'
#' result_df <- compute_evca(utility_df, model_probs)
#' print(result_df$evca)
#'
#' # Example 5: Examining which decisions are optimal under each model
#' for (i in 1:length(model_probs)) {
#'   optimal_idx <- result$optimal_decisions_per_model[i]
#'   cat(sprintf(
#'     "If %s is true (p=%.2f): Choose %s (utility=%.2f)\n",
#'     colnames(model_utilities)[i],
#'     model_probs[i],
#'     rownames(model_utilities)[optimal_idx],
#'     result$optimal_utilities_per_model[i]
#'   ))
#' }
#'
#' @export
#'
compute_evca <- function(model_utilities, model_probs,
                         utility_function = identity) {
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

  # Apply utility function if provided (assuming model_utilities are outcomes)
  if (!identical(utility_function, identity)) {
    model_utilities <- apply(model_utilities, c(1, 2), utility_function)
  }

  # Get dimensions
  n_decisions <- nrow(model_utilities)
  n_models <- ncol(model_utilities)

  # Check model_probs is numeric
  if (!is.numeric(model_probs)) {
    stop("model_probs must be a numeric vector")
  }

  # Validate dimensions match
  if (length(model_probs) != n_models) {
    stop(
      "Number of model probabilities (", length(model_probs), ") ",
      "must equal number of model utility columns (", n_models, ")"
    )
  }

  # Check probabilities are non-negative (must come before sum check)
  if (any(model_probs < 0)) {
    stop(
      "Model probabilities must be non-negative. ",
      "Found ", sum(model_probs < 0), " negative value(s)."
    )
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
      "Please remove or impute missing values before computing EVECA."
    )
  }

  # Check for missing values in probabilities
  if (any(is.na(model_probs))) {
    stop("model_probs contains NA values")
  }

  # Check for valid dimensions
  if (n_decisions < 1) {
    stop("model_utilities must have at least one row (decision)")
  }

  if (n_models < 1) {
    stop("model_utilities must have at least one column (model)")
  }

  # Compute BMA expected utility ----
  bma_eu <- bma_expected_utility(model_utilities, model_probs)

  # Find optimal decision under BMA ----
  optimal_decision_bma <- which.max(bma_eu)
  optimal_utility_bma <- bma_eu[optimal_decision_bma]

  # Compute optimal decision under each model ----
  optimal_decisions_per_model <- apply(model_utilities, 2, which.max)
  optimal_utilities_per_model <- numeric(n_models)

  for (i in seq_len(n_models)) {
    optimal_utilities_per_model[i] <- model_utilities[optimal_decisions_per_model[i], i]
  }

  # Compute expected utility with perfect information ----
  # If we knew which model is true, we would choose the optimal decision for that model
  # Expected utility is weighted average of these optimal utilities
  perfect_info_eu <- sum(model_probs * optimal_utilities_per_model)

  # Calculate EVECA ----
  # EVECA is the difference between perfect information and BMA
  # Represents maximum value of resolving model ambiguity
  evca <- perfect_info_eu - optimal_utility_bma

  # Return comprehensive results ----
  return(list(
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
  ))
}
