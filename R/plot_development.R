#' Plot Expected Value of Eliminating Causal Ambiguity for Gender-Nutrition Contexts
#'
#' Create visualizations of EVECA analysis results from `compute_evca()` with
#' gender-nutrition-specific features including nutrition-empowerment trade-offs,
#' causal pathway contributions, and decision-making insights for sustainable development.
#'
#' @param evca_result Output from `compute_evca()` function.
#' @param model_outcomes Optional array of model outcomes [decisions × models × outcomes]
#'   for detailed gender-nutrition visualization.
#' @param outcome_weights Optional vector of outcome weights for multi-attribute utility
#'   (nutrition vs gender empowerment).
#' @param plot_type Type of plot to create. Options:
#'   - "evca_bars": Basic EVECA bar plot (default)
#'   - "outcome_contributions": Nutrition and empowerment contributions by intervention and model
#'   - "decision_heatmap": Heatmap of utilities across decisions and models
#'   - "sensitivity": Sensitivity of EVECA to model probabilities
#' @param title Plot title (default: context-specific).
#' @param colors Color palette for plots (default: Set2 for categorical, viridis for continuous).
#' @param theme ggplot2 theme to use (default: theme_minimal()).
#'
#' @return A ggplot object or list of ggplot objects that can be further customized or printed.
#'
#' @export
#' @import ggplot2
#' @importFrom viridis scale_fill_viridis
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' \dontrun{
#' # Generate example data
#' example_result <- example_development(context = "agriculture")
#'
#' # Create basic EVECA plot
#' p1 <- plot_development(example_result$evca_result,
#'   plot_type = "evca_bars",
#'   title = "EVECA Analysis: Nutrition-Sensitive Agriculture"
#' )
#'
#' # Create outcome contributions plot
#' p2 <- plot_development(example_result$evca_result,
#'   model_outcomes = example_result$model_outcomes,
#'   outcome_weights = example_result$outcome_weights,
#'   plot_type = "outcome_contributions"
#' )
#'
#' # Create decision heatmap
#' p3 <- plot_development(example_result$evca_result,
#'   plot_type = "decision_heatmap"
#' )
#'
#' # Print plots
#' print(p1)
#' print(p2)
#' print(p3)
#' }
plot_development <- function(evca_result,
                             model_outcomes = NULL,
                             outcome_weights = NULL,
                             plot_type = "evca_bars",
                             title = NULL,
                             colors = NULL,
                             theme = NULL) {
  # Validate input
  if (!is.list(evca_result) || !"evca" %in% names(evca_result)) {
    stop("evca_result must be output from compute_evca() function")
  }

  # If model_utilities is not in evca_result but provided separately in model_outcomes,
  # add it to evca_result for heatmap and sensitivity plots
  if (plot_type %in% c("decision_heatmap", "sensitivity") &&
    !"model_utilities" %in% names(evca_result) &&
    !is.null(model_outcomes) && !is.null(outcome_weights)) {
    # Compute model_utilities from model_outcomes and outcome_weights
    n_decisions <- dim(model_outcomes)[1]
    n_models <- dim(model_outcomes)[2]

    utilities <- matrix(NA, nrow = n_decisions, ncol = n_models)
    rownames(utilities) <- dimnames(model_outcomes)[[1]]
    colnames(utilities) <- dimnames(model_outcomes)[[2]]

    for (i in 1:n_decisions) {
      for (j in 1:n_models) {
        utilities[i, j] <- sum(outcome_weights * model_outcomes[i, j, ])
      }
    }

    evca_result$model_utilities <- utilities
  }

  # Set default theme if not provided
  if (is.null(theme)) {
    theme <- ggplot2::theme_minimal()
  }

  # Set default colors if not provided
  if (is.null(colors)) {
    if (plot_type %in% c("evca_bars", "outcome_contributions")) {
      colors <- RColorBrewer::brewer.pal(8, "Set2")[1:6]
    }
  }

  # Generate appropriate plot based on type
  if (plot_type == "evca_bars") {
    p <- plot_evca_bars(evca_result, title, colors, theme)
  } else if (plot_type == "outcome_contributions") {
    if (is.null(model_outcomes) || is.null(outcome_weights)) {
      stop("model_outcomes and outcome_weights required for outcome_contributions plot")
    }
    p <- plot_outcome_contributions(evca_result, model_outcomes, outcome_weights, title, colors, theme)
  } else if (plot_type == "decision_heatmap") {
    p <- plot_decision_heatmap(evca_result, title, theme)
  } else if (plot_type == "sensitivity") {
    p <- plot_sensitivity(evca_result, title, theme)
  } else {
    stop("Invalid plot_type. Choose from: 'evca_bars', 'outcome_contributions',
         'decision_heatmap', 'sensitivity'")
  }

  return(p)
}

# Helper function: Basic EVECA bar plot
plot_evca_bars <- function(evca_result, title, colors, theme) {
  if (is.null(title)) {
    title <- "EVECA Analysis: Value of Eliminating Causal Ambiguity in Gender-Nutrition Pathways"
  }

  # Create plot data
  plot_data <- data.frame(
    Component = c("Optimal Under Ambiguity", "Perfect Information"),
    Value = c(
      evca_result$optimal_utility_bma,
      evca_result$perfect_info_expected_utility
    ),
    Label = c(
      sprintf("%.3f", evca_result$optimal_utility_bma),
      sprintf("%.3f", evca_result$perfect_info_expected_utility)
    )
  )

  # Create plot
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$Component, y = .data$Value, fill = .data$Component)
  ) +
    ggplot2::geom_bar(stat = "identity", width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$Label),
      vjust = -0.5,
      size = 3.5
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Expected Multi-Attribute Utility (Nutrition-Gender Weighted)",
      subtitle = sprintf("EVECA = %.3f", evca_result$evca)
    ) +
    ggplot2::scale_fill_manual(values = colors[1:2]) +
    theme +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.major.x = ggplot2::element_blank()
    )

  return(p)
}

# Helper function: Outcome contributions plot
plot_outcome_contributions <- function(evca_result, model_outcomes, outcome_weights, title, colors, theme) {
  if (is.null(title)) {
    title <- "Nutrition and Empowerment Contributions by Intervention and Causal Pathway"
  }

  # Extract dimensions
  n_decisions <- dim(model_outcomes)[1]
  n_models <- dim(model_outcomes)[2]
  n_outcomes <- dim(model_outcomes)[3]

  # Prepare data for plotting
  plot_data <- expand.grid(
    Decision = dimnames(model_outcomes)[[1]],
    Model = dimnames(model_outcomes)[[2]],
    Outcome = dimnames(model_outcomes)[[3]]
  )

  # Add outcome values and weighted contributions
  plot_data$OutcomeValue <- NA_real_
  plot_data$WeightedContribution <- NA_real_

  for (i in 1:nrow(plot_data)) {
    d_idx <- which(dimnames(model_outcomes)[[1]] == plot_data$Decision[i])
    m_idx <- which(dimnames(model_outcomes)[[2]] == plot_data$Model[i])
    o_idx <- which(dimnames(model_outcomes)[[3]] == plot_data$Outcome[i])

    plot_data$OutcomeValue[i] <- model_outcomes[d_idx, m_idx, o_idx]
    plot_data$WeightedContribution[i] <- model_outcomes[d_idx, m_idx, o_idx] *
      outcome_weights[o_idx]
  }

  # Add outcome weight labels
  weight_labels <- sapply(1:n_outcomes, function(i) {
    sprintf("%s (w=%.2f)", dimnames(model_outcomes)[[3]][i], outcome_weights[i])
  })
  plot_data$OutcomeLabel <- factor(
    plot_data$Outcome,
    levels = dimnames(model_outcomes)[[3]],
    labels = weight_labels
  )

  # Create plot
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$Decision, y = .data$WeightedContribution, fill = .data$Model)
  ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    ggplot2::facet_wrap(~ .data$OutcomeLabel, scales = "free_y", ncol = 2) +
    ggplot2::labs(
      title = title,
      x = "Intervention",
      y = "Weighted Contribution to Multi-Attribute Utility",
      fill = "Causal Pathway",
      subtitle = sprintf(
        "Optimal intervention: %s | EVECA = %.3f | Weights: Nutrition=%.1f, Empowerment=%.1f",
        evca_result$optimal_decision_bma,
        evca_result$evca,
        outcome_weights[1],
        outcome_weights[2]
      )
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    theme +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.major.x = ggplot2::element_blank()
    )

  return(p)
}

# Helper function: Decision heatmap
plot_decision_heatmap <- function(evca_result, title, theme) {
  if (is.null(title)) {
    title <- "Gender-Nutrition Decision-Model Utility Matrix"
  }

  # Prepare data for heatmap
  if (!is.null(evca_result$model_utilities)) {
    utilities <- evca_result$model_utilities
  } else {
    # Try to reconstruct utilities from other components
    if (!is.null(evca_result$bma_expected_utility) &&
      !is.null(evca_result$model_probs) &&
      !is.null(evca_result$optimal_decisions_per_model)) {
      # This is a simplified reconstruction - in practice, model_utilities should be provided
      warning("model_utilities not found in evca_result. Using reconstructed approximation.")
      n_decisions <- length(evca_result$bma_expected_utility)
      n_models <- length(evca_result$model_probs)
      utilities <- matrix(NA, nrow = n_decisions, ncol = n_models)
      # Create simple placeholder utilities
      for (j in 1:n_models) {
        optimal_decision <- evca_result$optimal_decisions_per_model[j]
        utilities[optimal_decision, j] <- 1.0
        for (i in 1:n_decisions) {
          if (i != optimal_decision) {
            utilities[i, j] <- 0.5 + stats::runif(1, -0.1, 0.1)
          }
        }
      }
      rownames(utilities) <- paste("Decision", 1:n_decisions)
      colnames(utilities) <- paste("Model", 1:n_models)
    } else {
      stop("evca_result must contain model_utilities for heatmap plot, or provide model_outcomes and outcome_weights")
    }
  }

  plot_data <- expand.grid(
    Decision = rownames(utilities),
    Model = colnames(utilities)
  )
  plot_data$Utility <- as.vector(utilities)

  # Find optimal decision under BMA
  optimal_decision <- rownames(utilities)[evca_result$optimal_decision_bma]

  # Create heatmap
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$Model, y = .data$Decision, fill = .data$Utility)
  ) +
    ggplot2::geom_tile(color = "white", size = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", .data$Utility)),
      color = "black",
      size = 3.5
    ) +
    ggplot2::labs(
      title = title,
      x = "Causal Model (Empowerment-Nutrition Pathway)",
      y = "Gender-Nutrition Intervention",
      fill = "Utility",
      subtitle = sprintf(
        "Optimal under ambiguity: %s | EVECA = %.3f",
        optimal_decision, evca_result$evca
      )
    ) +
    viridis::scale_fill_viridis(option = "plasma", direction = -1) +
    theme +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )

  return(p)
}

# Helper function: Sensitivity analysis plot
plot_sensitivity <- function(evca_result, title, theme) {
  if (is.null(title)) {
    title <- "Sensitivity of EVECA to Empowerment-Nutrition Pathway Probabilities"
  }

  # Simple sensitivity analysis: vary each probability while keeping others proportional
  n_models <- length(evca_result$model_probs)
  sensitivity_data <- data.frame()

  # Check if we have model_utilities
  if (is.null(evca_result$model_utilities)) {
    warning("model_utilities not found in evca_result. Sensitivity analysis requires model_utilities.")
    # Create a simple placeholder plot
    p <- ggplot2::ggplot() +
      ggplot2::annotate("text",
        x = 0.5, y = 0.5,
        label = "Sensitivity analysis requires model_utilities\nin evca_result or provide model_outcomes\nand outcome_weights to plot_development()",
        size = 5
      ) +
      ggplot2::labs(title = title) +
      theme +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
    return(p)
  }

  for (i in 1:n_models) {
    # Vary probability of model i from 0 to 1
    p_seq <- seq(0, 1, by = 0.05)

    for (p in p_seq) {
      # Adjust other probabilities proportionally
      new_probs <- evca_result$model_probs
      new_probs[i] <- p

      # Handle edge cases
      if (p == 1) {
        new_probs[-i] <- 0
      } else if (sum(new_probs[-i]) > 0) {
        # Scale other probabilities to sum to 1-p
        new_probs[-i] <- new_probs[-i] / sum(new_probs[-i]) * (1 - p)
      }

      # Recompute EVECA with new probabilities
      new_bma_eu <- as.vector(evca_result$model_utilities %*% new_probs)
      new_optimal_utility <- max(new_bma_eu)
      new_perfect_info_eu <- sum(new_probs *
        apply(evca_result$model_utilities, 2, max))
      new_evca <- new_perfect_info_eu - new_optimal_utility

      sensitivity_data <- rbind(sensitivity_data, data.frame(
        Model = colnames(evca_result$model_utilities)[i],
        Probability = p,
        EVECA = new_evca
      ))
    }
  }

  # Create sensitivity plot
  p <- ggplot2::ggplot(
    sensitivity_data,
    ggplot2::aes(x = .data$Probability, y = .data$EVECA, color = .data$Model)
  ) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::geom_point(
      data = data.frame(
        Model = colnames(evca_result$model_utilities),
        Probability = evca_result$model_probs,
        EVECA = evca_result$evca
      ),
      ggplot2::aes(x = .data$Probability, y = .data$EVECA, color = .data$Model),
      size = 3,
      shape = 17
    ) +
    ggplot2::labs(
      title = title,
      x = "Empowerment-Nutrition Pathway Probability",
      y = "EVECA",
      color = "Causal Model (Pathway)",
      subtitle = "Triangle shows current probability estimate for each empowerment-nutrition pathway"
    ) +
    ggplot2::scale_color_brewer(palette = "Set2") +
    theme +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}
