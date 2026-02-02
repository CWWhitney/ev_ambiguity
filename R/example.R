#' Example Analysis for Expected Value of Eliminating Causal Ambiguity in Development Context
#'
#' Generate example data for development/agriculture decision problems and run
#' a demonstration EVECA analysis. This function creates realistic examples
#' relevant to nutrition-sensitive agriculture, climate adaptation, and other
#' development interventions with multiple competing causal models.
#'
#' @param n_decisions Number of decision alternatives (default: 3).
#' @param n_models Number of competing causal models (default: 4).
#' @param n_outcomes Number of outcome dimensions (e.g., SDG indicators, default: 4).
#' @param context Development context for naming (default: "agriculture").
#'   Options: "agriculture", "nutrition", "climate", "health".
#' @param sdg_weights Vector of weights for multi-attribute utility function
#'   (default: equal weights).
#' @param seed Random seed for reproducibility (default: 123).
#'
#' @return A list with the following components:
#'   \item{evca_result}{Output from `compute_evca()` function.}
#'   \item{model_outcomes}{Array of outcomes [decisions × models × outcomes].}
#'   \item{model_utilities}{Matrix of model utilities used in analysis.}
#'   \item{model_probs}{Vector of model probabilities used in analysis.}
#'   \item{sdg_weights}{SDG weights used in multi-attribute utility.}
#'   \item{context}{Development context used.}
#'
#' @export
#' @importFrom stats rnorm runif
#'
#' @examples
#' # Run example analysis with default parameters (agriculture context)
#' example_result <- example_development()
#'
#' # Print key results
#' cat("EVECA:", example_result$evca_result$evca, "\n")
#' cat("Optimal intervention:", example_result$evca_result$optimal_decision_bma, "\n")
#'
#' # Run example with nutrition context and custom parameters
#' nutrition_result <- example_development(
#'   n_decisions = 3,
#'   n_models = 3,
#'   n_outcomes = 3,
#'   context = "nutrition",
#'   sdg_weights = c(0.4, 0.4, 0.2)
#' )
#'
#' # Run climate adaptation example
#' climate_result <- example_development(
#'   context = "climate",
#'   sdg_weights = c(0.3, 0.3, 0.2, 0.2)
#' )
example_development <- function(n_decisions = 3,
                                n_models = 4,
                                n_outcomes = 4,
                                context = "agriculture",
                                sdg_weights = NULL,
                                seed = 123) {
  set.seed(seed)

  # Set default SDG weights if not provided
  if (is.null(sdg_weights)) {
    sdg_weights <- rep(1 / n_outcomes, n_outcomes)
  }

  # Validate SDG weights
  if (abs(sum(sdg_weights) - 1) > 1e-10) {
    stop("SDG weights must sum to 1 (within 1e-10 tolerance)")
  }

  if (any(sdg_weights < 0)) {
    stop("SDG weights must be non-negative")
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
    sdg_weights = sdg_weights
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
    sdg_weights = sdg_weights,
    context = context,
    decision_names = rownames(model_utilities),
    model_names = colnames(model_utilities),
    outcome_names = dimnames(model_outcomes)[[3]]
  )
}

# Helper function: Generate context-specific outcomes
generate_context_outcomes <- function(n_decisions, n_models, n_outcomes, context, seed) {
  set.seed(seed)

  # Define context-specific parameters
  context_params <- list(
    agriculture = list(
      decision_names = c(
        "Homestead Production", "Biofortified Crops", "Climate-Smart Ag",
        "Agroforestry", "Irrigation", "Value Chain"
      )[1:n_decisions],
      model_names = c(
        "Income Pathway", "Production Pathway", "Empowerment Pathway",
        "Environmental Pathway", "Market Pathway"
      )[1:n_models],
      outcome_names = c(
        "Dietary Diversity", "Household Income", "Women's Empowerment",
        "Carbon Sequestration", "Water Use Efficiency"
      )[1:n_outcomes],
      mean_range = c(0.5, 1.5),
      sd_range = c(0.1, 0.3)
    ),
    nutrition = list(
      decision_names = c(
        "Supplementary Feeding", "Nutrition Education", "Fortification",
        "Behavior Change", "Social Protection"
      )[1:n_decisions],
      model_names = c(
        "Direct Consumption", "Income-Mediated", "Knowledge-Mediated",
        "Behavior-Mediated", "Access-Mediated"
      )[1:n_models],
      outcome_names = c(
        "Child HAZ", "Dietary Diversity", "Maternal BMI",
        "Micronutrient Status", "Feeding Practices"
      )[1:n_outcomes],
      mean_range = c(0.3, 1.2),
      sd_range = c(0.05, 0.2)
    ),
    climate = list(
      decision_names = c(
        "Drought-Tolerant Varieties", "Conservation Agriculture",
        "Weather Insurance", "Early Warning Systems",
        "Water Harvesting"
      )[1:n_decisions],
      model_names = c(
        "Yield Stability", "Risk Reduction", "Adaptive Capacity",
        "Ecosystem Services", "Institutional"
      )[1:n_models],
      outcome_names = c(
        "Yield Stability", "Income Stability", "Soil Health",
        "Water Security", "Adaptive Capacity"
      )[1:n_outcomes],
      mean_range = c(0.4, 1.3),
      sd_range = c(0.1, 0.25)
    ),
    health = list(
      decision_names = c(
        "Preventive Care", "Treatment Access", "Health Education",
        "Sanitation", "Vector Control"
      )[1:n_decisions],
      model_names = c(
        "Direct Treatment", "Prevention", "Behavior Change",
        "Environmental", "System Strengthening"
      )[1:n_models],
      outcome_names = c(
        "Mortality Reduction", "Morbidity Reduction", "Quality of Life",
        "Health Equity", "System Resilience"
      )[1:n_outcomes],
      mean_range = c(0.2, 1.0),
      sd_range = c(0.05, 0.15)
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

  # Context-specific probability patterns
  patterns <- list(
    agriculture = c(0.3, 0.25, 0.2, 0.15, 0.1)[1:n_models],
    nutrition = c(0.35, 0.25, 0.2, 0.15, 0.05)[1:n_models],
    climate = c(0.25, 0.25, 0.2, 0.2, 0.1)[1:n_models],
    health = c(0.4, 0.25, 0.2, 0.1, 0.05)[1:n_models]
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
compute_multi_attribute_utilities <- function(model_outcomes, sdg_weights) {
  n_decisions <- dim(model_outcomes)[1]
  n_models <- dim(model_outcomes)[2]

  utilities <- matrix(NA, nrow = n_decisions, ncol = n_models)
  rownames(utilities) <- dimnames(model_outcomes)[[1]]
  colnames(utilities) <- dimnames(model_outcomes)[[2]]

  for (i in 1:n_decisions) {
    for (j in 1:n_models) {
      # Simple linear multi-attribute utility
      utilities[i, j] <- sum(sdg_weights * model_outcomes[i, j, ])
    }
  }

  return(utilities)
}
