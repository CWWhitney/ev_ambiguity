#' Plot Expected Value of Eliminating Causal Ambiguity for Development Contexts
#'
#' Create visualizations of EVECA analysis results from `compute_evca()` with
#' development-specific features including SDG trade-offs, causal pathway
#' contributions, and decision-making insights.
#'
#' @param evca_result Output from `compute_evca()` function.
#' @param model_outcomes Optional array of model outcomes [decisions × models × outcomes]
#'   for detailed SDG visualization.
#' @param sdg_weights Optional vector of SDG weights for multi-attribute utility.
#' @param plot_type Type of plot to create. Options:
#'   - "evca_bars": Basic EVECA bar plot (default)
#'   - "sdg_contributions": SDG contributions by intervention and model
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
#' # Create SDG contributions plot
#' p2 <- plot_development(example_result$evca_result,
#'   model_outcomes = example_result$model_outcomes,
#'   sdg_weights = example_result$sdg_weights,
#'   plot_type = "sdg_contributions"
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
                             sdg_weights = NULL,
                             plot_type = "evca_bars",
                             title = NULL,
                             colors = NULL,
                             theme = NULL) {
  # Validate input
  if (!is.list(evca_result) || !"evca" %in% names(evca_result)) {
    stop("evca_result must be output from compute_evca() function")
  }

  # Set default theme if not provided
  if (is.null(theme)) {
    theme <- ggplot2::theme_minimal()
  }

  # Set default colors if not provided
  if (is.null(colors)) {
    if (plot_type %in% c("evca_bars", "sdg_contributions")) {
      colors <- RColorBrewer::brewer.pal(8, "Set2")[1:6]
    }
  }

  # Generate appropriate plot based on type
  if (plot_type == "evca_bars") {
    p <- plot_evca_bars(evca_result, title, colors, theme)
  } else if (plot_type == "sdg_contributions") {
    if (is.null(model_outcomes) || is.null(sdg_weights)) {
      stop("model_outcomes and sdg_weights required for sdg_contributions plot")
    }
    p <- plot_sdg_contributions(evca_result, model_outcomes, sdg_weights, title, colors, theme)
  } else if (plot_type == "decision_heatmap") {
    p <- plot_decision_heatmap(evca_result, title, theme)
  } else if (plot_type == "sensitivity") {
    p <- plot_sensitivity(evca_result, title, theme)
  } else {
    stop("Invalid plot_type. Choose from: 'evca_bars', 'sdg_contributions',
         'decision_heatmap', 'sensitivity'")
  }

  return(p)
}

# Helper function: Basic EVECA bar plot
plot_evca_bars <- function(evca_result, title, colors, theme) {
  if (is.null(title)) {
    title <- "EVECA Analysis: Value of Eliminating Causal Ambiguity"
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
      y = "Expected Multi-Attribute Utility",
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

# Helper function: SDG contributions plot
plot_sdg_contributions <- function(evca_result, model_outcomes, sdg_weights, title, colors, theme) {
  if (is.null(title)) {
    title <- "SDG Contributions by Intervention and Causal Pathway"
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
      sdg_weights[o_idx]
  }

  # Add SDG weight labels
  weight_labels <- sapply(1:n_outcomes, function(i) {
    sprintf("%s (w=%.2f)", dimnames(model_outcomes)[[3]][i], sdg_weights[i])
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
        "Optimal intervention: %s (EVECA = %.3f)",
        evca_result$optimal_decision_bma,
        evca_result$evca
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
    title <- "Decision-Model Utility Matrix"
  }

  # Prepare data for heatmap
  if (!is.null(evca_result$model_utilities)) {
    utilities <- evca_result$model_utilities
  } else {
    stop("evca_result must contain model_utilities for heatmap plot")
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
      x = "Causal Model",
      y = "Intervention",
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
    title <- "Sensitivity of EVECA to Model Probabilities"
  }

  # Simple sensitivity analysis: vary each probability while keeping others proportional
  n_models <- length(evca_result$model_probs)
  sensitivity_data <- data.frame()

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
      if (!is.null(evca_result$model_utilities)) {
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
      x = "Model Probability",
      y = "EVECA",
      color = "Causal Model",
      subtitle = "Triangle shows current probability estimate"
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
