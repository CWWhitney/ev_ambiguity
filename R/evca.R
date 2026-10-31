#' Expected Value of Causal Ambiguity (EVCA)
#'
#' Computes a weighted aggregation shortfall across competing causal models.
#' The same arithmetic has two distinct interpretations: expected value of
#' perfect information when weights are epistemic probabilities, or a
#' conditional aggregation diagnostic when weights represent evaluative
#' standing assigned to stakeholder accounts.
#'
#' @usage compute_evca(model_utilities, model_probs = NULL,
#'   utility_function = identity, model_weights = NULL,
#'   weight_interpretation = NULL)
#'
#' @param model_utilities Matrix where rows are decisions and columns are models,
#'   containing expected utilities for each decision under each model.
#'   Can also be a data frame that can be coerced to matrix. Each cell \[i,j\]
#'   represents the expected utility of decision i under model j.
#' @param model_probs Optional numeric vector of epistemic probabilities over
#'   model columns. Use this argument only when weights express belief about
#'   which model may govern. Must be non-negative and sum to 1.
#' @param model_weights Optional numeric vector of evaluative weights over
#'   model columns, expressing declared standing assigned to stakeholder
#'   accounts. Must be non-negative and sum to 1. Supply either this or
#'   `model_probs`, not both.
#' @param weight_interpretation Optional label, `"epistemic"` or `"evaluative"`.
#'   Inferred from the argument supplied; included explicitly in results.
#' @param utility_function Function to apply to outcomes (default: identity).
#'   If provided, will be applied to model_utilities assuming they represent
#'   outcomes rather than utilities. Common examples include risk-averse
#'   utility functions like exponential utility: function(x) -exp(-r*x).
#'
#' @return A list with the following components:
#'   \item{action_utilities}{Numeric vector of weighted expected utility for each decision.}
#'   \item{optimal_decision}{Integer index of optimal decision under model averaging.}
#'   \item{optimal_utility}{Expected utility of optimal weighted-optimal decision.}
#'   \item{optimal_decisions_per_model}{Integer vector of optimal decision under each model.}
#'   \item{optimal_utilities_per_model}{Numeric vector of expected utility of optimal decision under each model.}
#'   \item{perfect_info_expected_utility}{Disaggregation benchmark; an information benchmark only under epistemic weights.}
#'   \item{evca}{Weighted aggregation shortfall under the supplied weights and utility scale.}
#'   \item{model_probs}{Supplied epistemic probabilities, or NULL for evaluative weights.}
#'   \item{model_weights}{Weights used in the computation.}
#'   \item{weight_interpretation}{Whether weights are epistemic or evaluative.}
#'   \item{model_contributions}{Additive per-model contributions to EVCA; these sum to EVCA.}
#'   \item{n_decisions}{Number of decision alternatives.}
#'   \item{n_models}{Number of competing causal models.}
#'
#' @details
#' The package addresses structural uncertainty, a recognized value-of-information
#' setting. Its calculation is the familiar gap between the weighted sum of
#' model-specific optima and the best weighted aggregate action. Under epistemic
#' probabilities this is EVPI for the model index. Under evaluative weights it
#' measures the shortfall imposed by the selected aggregation rule, not the
#' expected benefit of learning a true model.
#'
#' For weights w_k, the weighted aggregation shortfall is:
#' EVCA = sum_k w_k * max_d EU(d|M_k) - max_d sum_k w_k * EU(d|M_k)
#' With epistemic probabilities this is EVPI for the model index. With
#' evaluative weights, the first term is a disaggregation benchmark.
#'
#' The first term is the disaggregation benchmark; the second is the best
#' utility under one shared action chosen by weighted aggregation. Read the
#' first term as an information benchmark only when the weights are epistemic
#' probabilities. Under evaluative weights, it is a normative benchmark in
#' which each account is scored at its own best action.
#'
#' Interpretation:
#' - EVCA = 0: The best shared action attains the disaggregation benchmark.
#' - Under epistemic probabilities, EVCA is the value of perfect information
#'   about the model index.
#' - Under evaluative weights, EVCA is the shortfall under the declared
#'   aggregation rule; it is not by itself a reason to commission research.
#'
#' Computational complexity is O(D × M) where D is the number of decisions
#' and M is the number of models.
#'
#' @section Scope:
#' This function operates at the utility layer. It takes a pre-computed matrix
#' of expected utilities EU(d|M_k), one value per decision-model combination
#' and computes EVCA from that matrix. The upstream steps that produce this
#' matrix are outside the function's scope:
#' \enumerate{
#'   \item Specifying competing causal models (what are the alternative
#'     theories of how the intervention works?)
#'   \item Estimating model parameters (fitting each causal model to data)
#'   \item Computing expected utilities per model (simulating outcomes under
#'     each model for each available decision)
#'   \item Supplying epistemic probabilities or evaluative standing weights,
#'     depending on the interpretation selected.
#'   \item Normalizing utilities when models use incommensurable welfare units
#'     (see \code{\link{normalize_utilities}()})
#' }
#' Users must complete these steps before calling \code{compute_evca()}.
#' A future extension of this package will provide tools for causal model
#' specification, parameter estimation, and utility computation integrated
#' with Stan and Bayesian network frameworks.
#'
#' @section Relationship to related methods:
#' \strong{Structural-uncertainty VOI.} Decision analysis already represents
#' structural uncertainty and can calculate the value of information when
#' models and outcomes have a defined common representation. This function does
#' not replace EVPI or Bayesian model averaging. Once a common utility bridge
#' and epistemic model probabilities are supplied, its formula is EVPI for a
#' model index. With evaluative stakeholder weights, the same formula instead
#' reports an aggregation shortfall. Non-overlapping variables alone do not
#' make the arithmetic undefined; the unresolved judgment is how to construct
#' and justify a common-scale bridge.
#'
#' \strong{Strong & Oakley (2014)} address the Expected Value of Model
#' Improvement (EVMI), the gain from reducing systematic bias (model
#' discrepancy) within a single model relative to the true data-generating
#' process. This concerns within-model inadequacy: the model is assumed to be
#' structurally correct but imprecise. EVCA instead treats multiple complete
#' causal models as competing accounts. With epistemic probabilities its
#' formula is EVPI over a model index; with evaluative weights it reports
#' the welfare shortfall from imposing one shared action, which is not by
#' itself a value of research. The two address different questions: EVMI
#' concerns within-model bias, EVCA concerns aggregation across models.
#'
#' @section Warning:
#' Model probabilities must sum to exactly 1 (within 1e-10 tolerance).
#' All utility values should be on the same scale for meaningful comparison;
#' use \code{\link{normalize_utilities}()} when welfare units differ across
#' models.
#'
#' When comparing EVCA to research costs, ensure both are in the same units
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
#' # Probabilities that each model governs (epistemic). Use model_weights
#' # for declared standing (evaluative).
#' model_probs <- c(0.3, 0.25, 0.25, 0.2)
#'
#' # Compute EVCA with risk-neutral utility (identity function)
#' result <- compute_evca(model_utilities, model_probs = model_probs)
#'
#' # Print key results
#' print(result$evca)
#' print(result$optimal_decision)
#'
#' # Example 2: Making a decision based on EVCA
#' # Comparing EVCA with research cost needs epistemic probabilities.
#' research_cost <- 1.5
#'
#' if (result$evca > research_cost) {
#'   cat("Decision: INVEST IN RESEARCH\n")
#'   cat("Rationale: EVCA (", result$evca, ") > research cost (", research_cost, ")\n")
#' } else {
#'   cat("Decision: PROCEED with decision", result$optimal_decision, "\n")
#'   cat("Rationale: EVCA (", result$evca, ") < research cost (", research_cost, ")\n")
#' }
#'
#' # Example 3: Compute EVCA with risk-averse utility
#' result_risk <- compute_evca(
#'   model_utilities,
#'   model_probs,
#'   utility_function = function(x) -exp(-0.1 * x)
#' )
#'
#' # Compare results
#' cat("Risk-neutral EVCA:", result$evca, "\n")
#' cat("Risk-averse EVCA:", result_risk$evca, "\n")
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
compute_evca <- function(model_utilities, model_probs = NULL,
                         utility_function = identity,
                         model_weights = NULL,
                         weight_interpretation = NULL) {
  supplied_probs <- !is.null(model_probs)
  supplied_weights <- !is.null(model_weights)
  if (supplied_probs && supplied_weights) {
    stop("Supply either model_probs (epistemic) or model_weights (evaluative), not both.")
  }
  if (!supplied_probs && !supplied_weights) {
    stop("Supply model_probs for epistemic probabilities or model_weights for evaluative standing.")
  }
  if (is.null(weight_interpretation)) {
    weight_interpretation <- if (supplied_weights) "evaluative" else "epistemic"
  }
  weight_interpretation <- match.arg(weight_interpretation, c("epistemic", "evaluative"))
  if (supplied_probs && weight_interpretation != "epistemic") {
    stop("model_probs must use the epistemic interpretation; use model_weights for evaluative standing.")
  }
  if (supplied_weights && weight_interpretation != "evaluative") {
    stop("model_weights must use the evaluative interpretation; use model_probs for epistemic probabilities.")
  }
  if (supplied_weights) model_probs <- model_weights
  model_weights <- model_probs

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
      "Please remove or impute missing values before computing EVCA."
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

  # Compute weighted expected utility ----
  wtd_eu <- weighted_model_utility(model_utilities, model_probs)

  # Find optimal decision under model averaging ----
  optimal_decision <- which.max(wtd_eu)
  optimal_utility <- wtd_eu[optimal_decision]

  # Compute optimal decision under each model ----
  optimal_decisions_per_model <- apply(model_utilities, 2, which.max)
  optimal_utilities_per_model <- numeric(n_models)

  for (i in seq_len(n_models)) {
    optimal_utilities_per_model[i] <- model_utilities[optimal_decisions_per_model[i], i]
  }

  # Disaggregation benchmark: each account is evaluated at its own best action.
  # This is an information benchmark only for epistemic model probabilities.
  disaggregation_eu <- sum(model_weights * optimal_utilities_per_model)

  # Additive contribution of each model to the aggregate shortfall.
  aggregate_by_model <- model_utilities[optimal_decision, ]
  model_contributions <- model_weights *
    (optimal_utilities_per_model - aggregate_by_model)
  names(model_contributions) <- colnames(model_utilities)

  evca <- disaggregation_eu - optimal_utility

  # Return comprehensive results ----
  return(list(
    action_utilities = wtd_eu,
    optimal_decision = optimal_decision,
    optimal_utility = optimal_utility,
    optimal_decisions_per_model = optimal_decisions_per_model,
    optimal_utilities_per_model = optimal_utilities_per_model,
    perfect_info_expected_utility = disaggregation_eu,
    evca = evca,
    model_probs = if (weight_interpretation == "epistemic") model_weights else NULL,
    model_weights = model_weights,
    weight_interpretation = weight_interpretation,
    model_contributions = model_contributions,
    n_decisions = n_decisions,
    n_models = n_models
  ))
}
