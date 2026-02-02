#' Example Analysis for Expected Value of Eliminating Causal Ambiguity in Gender-Nutrition Context
#'
#' Generate example data for gender-nutrition decision problems and run
#' a demonstration EVECA analysis. This function creates realistic examples
#' relevant to gender-sensitive nutrition programming, women's empowerment interventions,
#' and other gender-nutrition decisions with multiple competing causal models.
#'
#' @param n_decisions Number of decision alternatives (default: 3).
#' @param n_models Number of competing causal models (default: 4).
#' @param n_outcomes Number of outcome dimensions (default: 2 for nutrition and gender).
#' @param context Gender-nutrition context for naming (default: "gender_nutrition").
#'   Options: "gender_nutrition", "empowerment_pathways", "nutrition_sensitive".
#' @param outcome_weights Vector of weights for multi-attribute utility function
#'   (default: c(0.6, 0.4) for nutrition vs gender empowerment).
#' @param seed Random seed for reproducibility (default: 123).
#'
#' @return A list with the following components:
#'   \item{evca_result}{Output from `compute_evca()` function.}
#'   \item{model_outcomes}{Array of outcomes [decisions × models × outcomes].}
#'   \item{model_utilities}{Matrix of model utilities used in analysis.}
#'   \item{model_probs}{Vector of model probabilities used in analysis.}
#'   \item{outcome_weights}{Outcome weights used in multi-attribute utility.}
#'   \item{context}{Gender-nutrition context used.}
#'
#' @export
#' @importFrom stats rnorm runif
#'
#' @examples
#' # Run example analysis with default parameters (gender-nutrition context)
#' example_result <- example_development()
#'
#' # Print key results
#' cat("EVECA:", example_result$evca_result$evca, "\n")
#' cat("Optimal intervention:", example_result$evca_result$optimal_decision_bma, "\n")
#'
#' # Run example with empowerment pathways context and custom parameters
#' empowerment_result <- example_development(
#'   n_decisions = 3,
#'   n_models = 3,
#'   n_outcomes = 2,
#'   context = "empowerment_pathways",
#'   outcome_weights = c(0.5, 0.5) # Equal weight on nutrition and empowerment
#' )
#'
#' # Run nutrition-sensitive agriculture example
#' nutrition_result <- example_development(
#'   context = "nutrition_sensitive",
#'   outcome_weights = c(0.7, 0.3) # Higher weight on nutrition
#' )
example_development <- function(n_decisions = 3,
                                n_models = 4,
                                n_outcomes = 2,
                                context = "gender_nutrition",
                                outcome_weights = NULL,
                                seed = 123) {
  set.seed(seed)

  # Set default outcome weights if not provided
  if (is.null(outcome_weights)) {
    outcome_weights <- c(0.6, 0.4) # Default: nutrition (0.6) vs gender empowerment (0.4)
  }

  # Validate outcome weights
  if (abs(sum(outcome_weights) - 1) > 1e-10) {
    stop("Outcome weights must sum to 1 (within 1e-10 tolerance)")
  }

  if (any(outcome_weights < 0)) {
    stop("Outcome weights must be non-negative")
  }

  # Generate realistic outcome arrays based on context
  model_outcomes <- generate_context_outcomes(
    n_decisions = n_decisions,
    n_models = n_models,
    n_outcomes = n_outcomes,
    context = context,
    seed = seed
  )

  # Generate model probabilities (context-specific)
  model_probs <- generate_context_probabilities(
    n_models = n_models,
    context = context,
    seed = seed
  )

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
    context = context,
    decision_names = rownames(model_utilities),
    model_names = colnames(model_utilities),
    outcome_names = dimnames(model_outcomes)[[3]]
  )
}

# Helper function: Generate context-specific outcomes
generate_context_outcomes <- function(n_decisions, n_models, n_outcomes, context, seed) {
  set.seed(seed)

  # Define context-specific parameters for gender-nutrition applications
  context_params <- list(
    gender_nutrition = list(
      decision_names = c(
        "Homestead Food Production", "Women's Empowerment Program", "Nutrition Education",
        "Cash Transfers", "Agricultural Training", "Childcare Support"
      )[1:n_decisions],
      model_names = c(
        "Income Pathway", "Production Pathway", "Empowerment Pathway",
        "Time Pathway", "Integrated Pathway"
      )[1:n_models],
      outcome_names = c(
        "Dietary Diversity", "Women's Empowerment Index", "Child Growth",
        "Feeding Practices", "Decision-Making Power"
      )[1:n_outcomes],
      mean_range = c(0.5, 1.5),
      sd_range = c(0.1, 0.3)
    ),
    empowerment_pathways = list(
      decision_names = c(
        "Asset Transfer", "Leadership Training", "Group Formation",
        "Legal Literacy", "Financial Inclusion"
      )[1:n_decisions],
      model_names = c(
        "Resource Control", "Decision-Making", "Leadership",
        "Social Networks", "Agency"
      )[1:n_models],
      outcome_names = c(
        "Nutrition Outcomes", "Empowerment Score", "Agency Index",
        "Resource Access", "Social Participation"
      )[1:n_outcomes],
      mean_range = c(0.4, 1.3),
      sd_range = c(0.08, 0.25)
    ),
    nutrition_sensitive = list(
      decision_names = c(
        "Biofortified Crops", "Kitchen Gardens", "Animal Husbandry",
        "Food Processing", "Market Linkages"
      )[1:n_decisions],
      model_names = c(
        "Direct Consumption", "Income Generation", "Women's Control",
        "Knowledge Transfer", "Multi-pathway"
      )[1:n_models],
      outcome_names = c(
        "Nutrition Status", "Women's Empowerment", "Food Security",
        "Diet Quality", "Care Practices"
      )[1:n_outcomes],
      mean_range = c(0.6, 1.4),
      sd_range = c(0.1, 0.3)
    )
  )

  # Get parameters for current context
  params <- context_params[[context]]
  if (is.null(params)) {
    stop("Invalid context. Choose from: ", paste(names(context_params), collapse = ", "))
  }

  # Generate outcome array
  outcomes <- array(
    dim = c(n_decisions, n_models, n_outcomes),
    dimnames = list(
      params$decision_names[1:n_decisions],
      params$model_names[1:n_models],
      params$outcome_names[1:n_outcomes]
    )
  )

  # Fill with realistic values
  for (i in 1:n_decisions) {
    for (j in 1:n_models) {
      for (k in 1:n_outcomes) {
        # Context-specific generation
        base_mean <- params$mean_range[1] + (params$mean_range[2] - params$mean_range[1]) *
          stats::runif(1)
        base_sd <- params$sd_range[1] + (params$sd_range[2] - params$sd_range[1]) *
          stats::runif(1)

        # Add decision-model interaction
        decision_effect <- 0.1 * (i - 1)
        model_effect <- 0.05 * (j - 1)

        outcomes[i, j, k] <- stats::rnorm(1,
          mean = base_mean + decision_effect + model_effect,
          sd = base_sd
        )
      }
    }
  }

  return(outcomes)
}

# Helper function: Generate context-specific model probabilities
generate_context_probabilities <- function(n_models, context, seed) {
  set.seed(seed)

  # Context-specific probability patterns for gender-nutrition pathways
  patterns <- list(
    gender_nutrition = c(0.30, 0.25, 0.20, 0.15, 0.10)[1:n_models], # Income pathway most likely
    empowerment_pathways = c(0.25, 0.25, 0.20, 0.20, 0.10)[1:n_models], # More balanced
    nutrition_sensitive = c(0.35, 0.25, 0.20, 0.15, 0.05)[1:n_models] # Direct pathways more likely
  )

  pattern <- patterns[[context]]
  if (is.null(pattern)) {
    # Default: uniform with small random variation
    pattern <- rep(1 / n_models, n_models)
  }

  # Add small random variation
  probs <- pattern + stats::runif(n_models, -0.05, 0.05)
  probs <- pmax(probs, 0) # Ensure non-negative
  probs <- probs / sum(probs) # Normalize

  return(probs)
}

# Helper function: Compute multi-attribute utilities
compute_multi_attribute_utilities <- function(model_outcomes, outcome_weights) {
  n_decisions <- dim(model_outcomes)[1]
  n_models <- dim(model_outcomes)[2]

  utilities <- matrix(NA, nrow = n_decisions, ncol = n_models)
  rownames(utilities) <- dimnames(model_outcomes)[[1]]
  colnames(utilities) <- dimnames(model_outcomes)[[2]]

  for (i in 1:n_decisions) {
    for (j in 1:n_models) {
      # Simple linear multi-attribute utility
      utilities[i, j] <- sum(outcome_weights * model_outcomes[i, j, ])
    }
  }

  return(utilities)
}
