#' Example Analysis for Expected Value of Eliminating Causal Ambiguity
#'
#' Generates example data for decision problems with competing causal models
#' and runs a demonstration EVECA analysis. This function creates realistic
#' examples for testing and understanding EVECA in various modeling contexts.
#' Utilities are randomly generated with structure to ensure different models
#' favor different decisions, creating meaningful model ambiguity.
#'
#' @usage example_evca(n_decisions = 3, n_models = 4, seed = 123,
#'   utility_range = c(0, 10))
#'
#' @param n_decisions Integer specifying number of decision alternatives (default: 3).
#'   Must be positive. Represents the number of choices available to the decision maker.
#' @param n_models Integer specifying number of competing causal models (default: 4).
#'   Must be positive. Represents the number of alternative theories about how
#'   decisions affect outcomes.
#' @param seed Integer random seed for reproducibility (default: 123).
#'   Use the same seed to generate identical datasets across function calls.
#' @param utility_range Numeric vector of length 2 specifying the range for
#'   generating utility values (default: c(0, 10)). First element is minimum,
#'   second is maximum. Additional utility is added to create model-specific
#'   preferences.
#'
#' @return A list with the following components:
#'   \item{evca_result}{Output from compute_evca() function containing EVECA value and details.}
#'   \item{model_utilities}{Matrix of model utilities with dimensions decisions x models.}
#'   \item{model_probs}{Numeric vector of model probabilities that sum to 1.}
#'   \item{decision_names}{Character vector of decision alternative names.}
#'   \item{model_names}{Character vector of competing model names.}
#'
#' @details
#' This function generates synthetic data with the following structure:
#' 1. Base utilities are drawn from uniform distribution over utility_range
#' 2. Each model is assigned a "favored decision" that receives additional utility
#' 3. Model probabilities are drawn from uniform distribution and normalized to sum to 1
#'
#' The generated data creates realistic model ambiguity where:
#' - Different models favor different decisions
#' - EVECA will typically be positive (value to resolving ambiguity)
#' - Results are reproducible when using the same seed
#'
#' This function is useful for:
#' - Learning how EVECA works
#' - Testing visualization functions
#' - Demonstrating the framework to others
#' - Generating test data for development
#'
#' @section Warning:
#' This function generates random data for demonstration purposes only.
#' Real applications should use actual utility estimates from data or expert judgment.
#'
#' @references
#' Hoeting, J. A., Madigan, D., Raftery, A. E., & Volinsky, C. T. (1999).
#' Bayesian model averaging: a tutorial. Statistical Science, 14(4), 382-401.
#'
#' @keywords datagen utilities examples
#'
#' @importFrom stats runif
#'
#' @examples
#' # Example 1: Basic usage with default parameters
#' example_result <- example_evca()
#'
#' # Print key results
#' cat("EVECA:", example_result$evca_result$evca, "\n")
#' optimal_idx <- example_result$evca_result$optimal_decision_bma
#' cat("Optimal decision:", example_result$decision_names[optimal_idx], "\n")
#'
#' # View generated utilities
#' print(example_result$model_utilities)
#'
#' # Example 2: Larger problem with more decisions and models
#' large_result <- example_evca(
#'   n_decisions = 5,
#'   n_models = 6,
#'   seed = 456
#' )
#' cat("EVECA for larger problem:", large_result$evca_result$evca, "\n")
#'
#' # Example 3: Different utility range
#' scaled_result <- example_evca(
#'   utility_range = c(-10, 10),
#'   seed = 789
#' )
#' print(range(scaled_result$model_utilities))
#'
#' # Example 4: Reproducibility check
#' result1 <- example_evca(seed = 100)
#' result2 <- example_evca(seed = 100)
#' identical(result1$model_utilities, result2$model_utilities) # Should be TRUE
#'
#' # Example 5: Use generated data for visualization
#' ex <- example_evca()
#' plot_evca(ex$evca_result)
#'
#' @export
example_evca <- function(n_decisions = 3,
                         n_models = 4,
                         seed = 123,
                         utility_range = c(0, 10)) {
  # Input validation ----

  # Check n_decisions is valid
  if (!is.numeric(n_decisions) || length(n_decisions) != 1) {
    stop("n_decisions must be a single numeric value")
  }

  if (n_decisions < 1) {
    stop("n_decisions must be at least 1")
  }

  if (n_decisions != as.integer(n_decisions)) {
    warning("n_decisions should be an integer. Rounding to ", floor(n_decisions))
    n_decisions <- as.integer(n_decisions)
  }

  # Check n_models is valid
  if (!is.numeric(n_models) || length(n_models) != 1) {
    stop("n_models must be a single numeric value")
  }

  if (n_models < 1) {
    stop("n_models must be at least 1")
  }

  if (n_models != as.integer(n_models)) {
    warning("n_models should be an integer. Rounding to ", floor(n_models))
    n_models <- as.integer(n_models)
  }

  # Check utility_range is valid
  if (!is.numeric(utility_range) || length(utility_range) != 2) {
    stop("utility_range must be a numeric vector of length 2")
  }

  if (utility_range[1] >= utility_range[2]) {
    stop(
      "utility_range[1] must be less than utility_range[2]. ",
      "Current values: [", utility_range[1], ", ", utility_range[2], "]"
    )
  }

  # Check seed is valid
  if (!is.numeric(seed) || length(seed) != 1) {
    stop("seed must be a single numeric value")
  }

  # Set random seed for reproducibility
  set.seed(seed)

  # Generate names ----
  decision_names <- paste("Decision", 1:n_decisions)
  model_names <- paste("Model", 1:n_models)

  # Generate base utility matrix ----
  # Each row is a decision, each column is a model
  # Base utilities drawn from uniform distribution over utility_range
  model_utilities <- matrix(
    stats::runif(
      n = n_decisions * n_models,
      min = utility_range[1],
      max = utility_range[2]
    ),
    nrow = n_decisions,
    ncol = n_models,
    dimnames = list(decision_names, model_names)
  )

  # Add model-specific structure ----
  # Each model favors a different decision to create meaningful ambiguity
  # This ensures EVECA will typically be positive
  for (j in 1:n_models) {
    # Assign each model a favored decision (cycling through decisions)
    favored_decision <- ((j - 1) %% n_decisions) + 1

    # Add bonus utility to favored decision (random amount between 2 and 4)
    bonus <- stats::runif(1, min = 2, max = 4)
    model_utilities[favored_decision, j] <- model_utilities[favored_decision, j] + bonus
  }

  # Generate model probabilities ----
  # Start with random values from uniform distribution
  # Normalize to ensure they sum to 1
  model_probs <- stats::runif(n_models, min = 0.1, max = 1)
  model_probs <- model_probs / sum(model_probs)
  names(model_probs) <- model_names

  # Compute EVECA ----
  evca_result <- compute_evca(
    model_utilities = model_utilities,
    model_probs = model_probs
  )

  # Return comprehensive results ----
  return(list(
    evca_result = evca_result,
    model_utilities = model_utilities,
    model_probs = model_probs,
    decision_names = decision_names,
    model_names = model_names
  ))
}


#' Example with Multi-Dimensional Outcomes
#'
#' Generates example data with explicit outcome dimensions before utility
#' transformation. Useful for demonstrating multi-attribute utility functions
#' and trade-offs between competing objectives (e.g., cost vs. effectiveness,
#' yield vs. sustainability).
#'
#' @usage example_evca_multidim(n_decisions = 3, n_models = 4, n_outcomes = 2,
#'   outcome_weights = NULL, seed = 123, outcome_range = c(0, 1))
#'
#' @param n_decisions Integer specifying number of decision alternatives (default: 3).
#'   Must be positive.
#' @param n_models Integer specifying number of competing causal models (default: 4).
#'   Must be positive.
#' @param n_outcomes Integer specifying number of outcome dimensions (default: 2).
#'   Must be positive. Common examples: 2 outcomes for cost/effectiveness trade-off,
#'   3 outcomes for triple-bottom-line (economic/social/environmental).
#' @param outcome_weights Numeric vector of weights for multi-attribute utility function
#'   (default: equal weights). Must be non-negative and sum to 1. Length must equal
#'   n_outcomes. Represents relative importance of each outcome dimension.
#' @param seed Integer random seed for reproducibility (default: 123).
#' @param outcome_range Numeric vector of length 2 specifying range for generating
#'   outcome values (default: c(0, 1)). Applied to all outcome dimensions.
#'
#' @return A list with the following components:
#'   \item{evca_result}{Output from compute_evca() function.}
#'   \item{model_outcomes}{3-dimensional array with dimensions: decisions x models x outcomes.}
#'   \item{model_utilities}{Matrix of utilities computed from weighted outcomes.}
#'   \item{model_probs}{Numeric vector of model probabilities.}
#'   \item{outcome_weights}{Outcome weights used in multi-attribute utility.}
#'   \item{decision_names}{Character vector of decision names.}
#'   \item{model_names}{Character vector of model names.}
#'   \item{outcome_names}{Character vector of outcome dimension names.}
#'
#' @details
#' This function generates multi-dimensional outcomes and computes utilities
#' using a simple additive multi-attribute utility function:
#'
#' U(d,m) = sum over outcomes k of: w_k * outcome_k(d,m)
#'
#' where w_k are the outcome weights and outcome_k(d,m) is the value of
#' outcome k for decision d under model m.
#'
#' The function creates trade-offs by:
#' 1. Generating base outcome values from uniform distribution
#' 2. Adding model-specific bonuses to different outcomes for different decisions
#' 3. Computing weighted utility using outcome_weights
#'
#' This structure ensures that:
#' - Decisions have different outcome profiles
#' - Models favor different decisions
#' - Outcome weights affect which decision is optimal
#'
#' @section Warning:
#' This function generates random data for demonstration purposes only.
#' Real applications should use actual outcome estimates.
#'
#' Outcome weights critically affect the optimal decision. Sensitivity analysis
#' over different weight specifications is recommended.
#'
#' @references
#' Keeney, R. L., & Raiffa, H. (1993). Decisions with multiple objectives:
#' preferences and value trade-offs. Cambridge University Press.
#'
#' @keywords datagen utilities examples multivariate
#'
#' @importFrom stats runif
#'
#' @examples
#' # Example 1: Basic usage with default parameters
#' example_result <- example_evca_multidim()
#'
#' # Print key results
#' cat("EVECA:", example_result$evca_result$evca, "\n")
#' optimal_idx <- example_result$evca_result$optimal_decision_bma
#' cat("Optimal decision:", example_result$decision_names[optimal_idx], "\n")
#'
#' # View outcome array structure
#' cat("Outcome dimensions:", dim(example_result$model_outcomes), "\n")
#'
#' # Example 2: Custom weights prioritizing first outcome
#' weighted_result <- example_evca_multidim(
#'   n_outcomes = 2,
#'   outcome_weights = c(0.8, 0.2) # 80% weight on first outcome
#' )
#' cat("EVECA with 80/20 weights:", weighted_result$evca_result$evca, "\n")
#'
#' # Example 3: Three outcome dimensions
#' triple_result <- example_evca_multidim(
#'   n_outcomes = 3,
#'   outcome_weights = c(0.5, 0.3, 0.2)
#' )
#' print(triple_result$outcome_names)
#'
#' # Example 4: Examine outcomes for one decision
#' ex <- example_evca_multidim()
#' cat("Outcomes for Decision 1 across all models:\n")
#' print(ex$model_outcomes[1, , ])
#'
#' # Example 5: Compare different weight schemes
#' weights_equal <- c(0.5, 0.5)
#' weights_skewed <- c(0.9, 0.1)
#'
#' result_equal <- example_evca_multidim(outcome_weights = weights_equal, seed = 100)
#' result_skewed <- example_evca_multidim(outcome_weights = weights_skewed, seed = 100)
#'
#' cat("EVECA with equal weights:", result_equal$evca_result$evca, "\n")
#' cat("EVECA with skewed weights:", result_skewed$evca_result$evca, "\n")
#'
#' @export
example_evca_multidim <- function(n_decisions = 3,
                                  n_models = 4,
                                  n_outcomes = 2,
                                  outcome_weights = NULL,
                                  seed = 123,
                                  outcome_range = c(0, 1)) {
  # Input validation ----

  # Check n_decisions is valid
  if (!is.numeric(n_decisions) || length(n_decisions) != 1) {
    stop("n_decisions must be a single numeric value")
  }

  if (n_decisions < 1) {
    stop("n_decisions must be at least 1")
  }

  if (n_decisions != as.integer(n_decisions)) {
    warning("n_decisions should be an integer. Rounding to ", floor(n_decisions))
    n_decisions <- as.integer(n_decisions)
  }

  # Check n_models is valid
  if (!is.numeric(n_models) || length(n_models) != 1) {
    stop("n_models must be a single numeric value")
  }

  if (n_models < 1) {
    stop("n_models must be at least 1")
  }

  if (n_models != as.integer(n_models)) {
    warning("n_models should be an integer. Rounding to ", floor(n_models))
    n_models <- as.integer(n_models)
  }

  # Check n_outcomes is valid
  if (!is.numeric(n_outcomes) || length(n_outcomes) != 1) {
    stop("n_outcomes must be a single numeric value")
  }

  if (n_outcomes < 1) {
    stop("n_outcomes must be at least 1")
  }

  if (n_outcomes != as.integer(n_outcomes)) {
    warning("n_outcomes should be an integer. Rounding to ", floor(n_outcomes))
    n_outcomes <- as.integer(n_outcomes)
  }

  # Set default outcome weights if not provided
  if (is.null(outcome_weights)) {
    outcome_weights <- rep(1 / n_outcomes, n_outcomes)
  }

  # Validate outcome weights
  if (!is.numeric(outcome_weights)) {
    stop("outcome_weights must be a numeric vector")
  }

  if (length(outcome_weights) != n_outcomes) {
    stop(
      "Length of outcome_weights (", length(outcome_weights), ") ",
      "must equal n_outcomes (", n_outcomes, ")"
    )
  }

  if (any(outcome_weights < 0)) {
    stop(
      "Outcome weights must be non-negative. ",
      "Found ", sum(outcome_weights < 0), " negative value(s)."
    )
  }

  if (abs(sum(outcome_weights) - 1) > 1e-10) {
    stop(
      "Outcome weights must sum to 1 (within 1e-10 tolerance). ",
      "Current sum: ", round(sum(outcome_weights), 10)
    )
  }

  # Check outcome_range is valid
  if (!is.numeric(outcome_range) || length(outcome_range) != 2) {
    stop("outcome_range must be a numeric vector of length 2")
  }

  if (outcome_range[1] >= outcome_range[2]) {
    stop(
      "outcome_range[1] must be less than outcome_range[2]. ",
      "Current values: [", outcome_range[1], ", ", outcome_range[2], "]"
    )
  }

  # Check seed is valid
  if (!is.numeric(seed) || length(seed) != 1) {
    stop("seed must be a single numeric value")
  }

  # Set random seed for reproducibility
  set.seed(seed)

  # Generate names ----
  decision_names <- paste("Decision", 1:n_decisions)
  model_names <- paste("Model", 1:n_models)
  outcome_names <- paste("Outcome", 1:n_outcomes)

  # Generate base outcome array ----
  # 3-dimensional array: decisions x models x outcomes
  # Each cell contains the value of an outcome for a decision under a model
  model_outcomes <- array(
    stats::runif(
      n = n_decisions * n_models * n_outcomes,
      min = outcome_range[1],
      max = outcome_range[2]
    ),
    dim = c(n_decisions, n_models, n_outcomes),
    dimnames = list(decision_names, model_names, outcome_names)
  )

  # Add model-specific structure ----
  # Create trade-offs by making different models favor different decisions
  # across all outcome dimensions
  for (j in 1:n_models) {
    # Assign each model a favored decision
    favored_decision <- ((j - 1) %% n_decisions) + 1

    # Boost all outcomes for favored decision under this model
    for (k in 1:n_outcomes) {
      bonus <- stats::runif(1, min = 0.3, max = 0.5)
      model_outcomes[favored_decision, j, k] <-
        model_outcomes[favored_decision, j, k] + bonus
    }
  }

  # Generate model probabilities ----
  model_probs <- stats::runif(n_models, min = 0.1, max = 1)
  model_probs <- model_probs / sum(model_probs)
  names(model_probs) <- model_names

  # Compute multi-attribute utilities ----
  # Convert multi-dimensional outcomes to single utility value
  # using weighted linear combination
  model_utilities <- compute_multi_attribute_utilities(
    model_outcomes = model_outcomes,
    outcome_weights = outcome_weights
  )

  # Compute EVECA ----
  evca_result <- compute_evca(
    model_utilities = model_utilities,
    model_probs = model_probs
  )

  # Return comprehensive results ----
  return(list(
    evca_result = evca_result,
    model_outcomes = model_outcomes,
    model_utilities = model_utilities,
    model_probs = model_probs,
    outcome_weights = outcome_weights,
    decision_names = decision_names,
    model_names = model_names,
    outcome_names = outcome_names
  ))
}


#' Compute Multi-Attribute Utilities
#'
#' Computes single utility values from multi-dimensional outcomes using
#' a weighted linear combination. This function aggregates multiple outcome
#' dimensions into a single utility measure based on user-specified weights.
#'
#' @usage compute_multi_attribute_utilities(model_outcomes, outcome_weights)
#'
#' @param model_outcomes 3-dimensional array with dimensions: decisions x models x outcomes.
#'   Each element \[i,j,k\] represents the value of outcome k for decision i
#'   under model j. All values should be on comparable scales or pre-normalized.
#' @param outcome_weights Numeric vector of weights for multi-attribute utility function.
#'   Must be non-negative, sum to 1, and have length equal to the number of
#'   outcome dimensions (third dimension of model_outcomes).
#'
#' @return Matrix of utilities with dimensions: decisions x models.
#'   Each element \[i,j\] represents the aggregated utility of decision i
#'   under model j, computed as the weighted sum of outcomes.
#'
#' @details
#' This function implements a simple additive multi-attribute utility function:
#'
#' U(d,m) = sum over outcomes k of: w_k * outcome_k(d,m)
#'
#' where:
#' - U(d,m) is the utility of decision d under model m
#' - w_k is the weight for outcome k
#' - outcome_k(d,m) is the value of outcome k for decision d under model m
#'
#' This additive form assumes preferential independence among outcomes,
#' meaning the preference for levels of one outcome does not depend on
#' the levels of other outcomes.
#'
#' For more complex preference structures (e.g., non-linear utility,
#' interactions between outcomes), users should compute utilities directly
#' rather than using this helper function.
#'
#' @section Warning:
#' This is a helper function primarily used internally by example_evca_multidim().
#' While exported for flexibility, most users should use higher-level functions.
#'
#' All outcomes should be scaled to comparable ranges or units. Mixing outcomes
#' on vastly different scales (e.g., dollars and proportions) without proper
#' scaling can lead to weights not having their intended effect.
#'
#' @references
#' Keeney, R. L., & Raiffa, H. (1993). Decisions with multiple objectives:
#' preferences and value trade-offs. Cambridge University Press.
#'
#' @keywords utilities multivariate internal
#'
#' @examples
#' # Example 1: Simple 2x2x2 case (2 decisions, 2 models, 2 outcomes)
#' outcomes <- array(
#'   c(
#'     # Decision 1, Model 1
#'     0.8, 0.3, # Outcome 1, Outcome 2
#'     # Decision 1, Model 2
#'     0.6, 0.7, # Outcome 1, Outcome 2
#'     # Decision 2, Model 1
#'     0.4, 0.9, # Outcome 1, Outcome 2
#'     # Decision 2, Model 2
#'     0.5, 0.6 # Outcome 1, Outcome 2
#'   ),
#'   dim = c(2, 2, 2),
#'   dimnames = list(
#'     c("Decision A", "Decision B"),
#'     c("Model 1", "Model 2"),
#'     c("Cost", "Effectiveness")
#'   )
#' )
#'
#' # Equal weights
#' weights_equal <- c(0.5, 0.5)
#' utilities_equal <- compute_multi_attribute_utilities(outcomes, weights_equal)
#' print(utilities_equal)
#'
#' # Example 2: Prioritize first outcome
#' weights_cost <- c(0.8, 0.2) # Prioritize cost over effectiveness
#' utilities_cost <- compute_multi_attribute_utilities(outcomes, weights_cost)
#' print(utilities_cost)
#'
#' # Example 3: Three outcome dimensions
#' outcomes_3d <- array(
#'   runif(3 * 2 * 3), # 3 decisions, 2 models, 3 outcomes
#'   dim = c(3, 2, 3),
#'   dimnames = list(
#'     paste("Decision", 1:3),
#'     paste("Model", 1:2),
#'     c("Economic", "Social", "Environmental")
#'   )
#' )
#'
#' weights_3d <- c(0.5, 0.3, 0.2) # Triple bottom line
#' utilities_3d <- compute_multi_attribute_utilities(outcomes_3d, weights_3d)
#' print(utilities_3d)
#'
#' @export
compute_multi_attribute_utilities <- function(model_outcomes, outcome_weights) {
  # Input validation ----

  # Check model_outcomes is an array
  if (!is.array(model_outcomes)) {
    stop(
      "model_outcomes must be a 3-dimensional array. ",
      "Received object of class: ", class(model_outcomes)[1]
    )
  }

  # Check dimensions
  if (length(dim(model_outcomes)) != 3) {
    stop(
      "model_outcomes must be a 3-dimensional array. ",
      "Received array with ", length(dim(model_outcomes)), " dimensions."
    )
  }

  # Extract dimensions
  n_decisions <- dim(model_outcomes)[1]
  n_models <- dim(model_outcomes)[2]
  n_outcomes <- dim(model_outcomes)[3]

  # Check outcome_weights is numeric
  if (!is.numeric(outcome_weights)) {
    stop("outcome_weights must be a numeric vector")
  }

  # Validate weights length matches outcomes
  if (length(outcome_weights) != n_outcomes) {
    stop(
      "Length of outcome_weights (", length(outcome_weights), ") ",
      "must equal number of outcome dimensions (", n_outcomes, ")"
    )
  }

  # Check weights are non-negative
  if (any(outcome_weights < 0)) {
    stop(
      "Outcome weights must be non-negative. ",
      "Found ", sum(outcome_weights < 0), " negative value(s)."
    )
  }

  # Check weights sum to 1
  if (abs(sum(outcome_weights) - 1) > 1e-10) {
    stop(
      "Outcome weights must sum to 1 (within 1e-10 tolerance). ",
      "Current sum: ", round(sum(outcome_weights), 10)
    )
  }

  # Check for missing values
  if (any(is.na(model_outcomes))) {
    stop(
      "model_outcomes contains NA values. ",
      "Please remove or impute missing values before computing utilities."
    )
  }

  if (any(is.na(outcome_weights))) {
    stop("outcome_weights contains NA values")
  }

  # Initialize utility matrix ----
  utilities <- matrix(NA, nrow = n_decisions, ncol = n_models)

  # Preserve dimension names if they exist
  if (!is.null(dimnames(model_outcomes))) {
    rownames(utilities) <- dimnames(model_outcomes)[[1]]
    colnames(utilities) <- dimnames(model_outcomes)[[2]]
  }

  # Compute weighted sum for each decision-model combination ----
  # U(i,j) = sum_k w_k * outcome_k(i,j)
  for (i in 1:n_decisions) {
    for (j in 1:n_models) {
      utilities[i, j] <- sum(outcome_weights * model_outcomes[i, j, ])
    }
  }

  # Return utility matrix ----
  return(utilities)
}
