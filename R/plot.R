#' Plot Expected Value of Eliminating Causal Ambiguity (EVECA) Results
#'
#' Create visualizations of EVECA analysis results from `compute_evca()`.
#'
#' @param evca_result Output from `compute_evca()` function.
#' @param title Plot title (default: "EVECA Analysis").
#' @param subtitle Optional subtitle (default: auto-generated with EVECA value).
#' @param colors Vector of colors for bars (default: c("#4E79A7", "#F28E2B")).
#'
#' @return A ggplot object that can be further customized or printed.
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' # Run example analysis
#' result <- example_evca()
#'
#' # Create plot
#' p <- plot_evca(result$evca_result)
#' print(p)
#'
#' # Customize plot
#' p <- plot_evca(result$evca_result,
#'   title = "Value of Resolving Model Uncertainty",
#'   colors = c("#2E8B57", "#FF6347")
#' )
#' print(p)
plot_evca <- function(evca_result,
                      title = "Expected Value of Eliminating Causal Ambiguity",
                      subtitle = NULL,
                      colors = c("#4E79A7", "#F28E2B")) {
  # Validate input
  if (!is.list(evca_result) || !"evca" %in% names(evca_result)) {
    stop("evca_result must be output from compute_evca() function")
  }

  # Create subtitle if not provided
  if (is.null(subtitle)) {
    subtitle <- sprintf("EVECA = %.4f", evca_result$evca)
  }

  # Create plot data
  plot_data <- data.frame(
    Component = c("BMA Optimal", "Perfect Information"),
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
      size = 4
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray50",
      alpha = 0.5
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Expected Utility"
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text = ggplot2::element_text(size = 10),
      panel.grid.major.x = ggplot2::element_blank()
    )

  return(p)
}


#' Plot Model Utilities Heatmap
#'
#' Create a heatmap showing utilities for each decision under each competing model.
#'
#' @param model_utilities Matrix where rows are decisions and columns are models,
#'   containing expected utilities for each decision under each model.
#' @param model_probs Vector of model probabilities (optional, shown if provided).
#' @param optimal_decision Index of optimal decision under BMA (optional, highlighted if provided).
#' @param title Plot title (default: "Decision-Model Utility Matrix").
#'
#' @return A ggplot object.
#'
#' @export
#' @import ggplot2
#' @importFrom viridis scale_fill_viridis
#'
#' @examples
#' # Run example analysis
#' result <- example_evca()
#'
#' # Create heatmap
#' p <- plot_utilities_heatmap(
#'   result$model_utilities,
#'   model_probs = result$model_probs,
#'   optimal_decision = result$evca_result$optimal_decision_bma
#' )
#' print(p)
plot_utilities_heatmap <- function(model_utilities,
                                   model_probs = NULL,
                                   optimal_decision = NULL,
                                   title = "Decision-Model Utility Matrix") {
  # Convert to matrix if needed
  if (!is.matrix(model_utilities)) {
    model_utilities <- as.matrix(model_utilities)
  }

  # Create plot data
  plot_data <- expand.grid(
    Decision = rownames(model_utilities),
    Model = colnames(model_utilities)
  )
  plot_data$Utility <- as.vector(model_utilities)

  # Add optimal decision indicator if provided
  if (!is.null(optimal_decision)) {
    optimal_name <- rownames(model_utilities)[optimal_decision]
    plot_data$Optimal <- plot_data$Decision == optimal_name
  }

  # Create subtitle with model probabilities if provided
  subtitle <- NULL
  if (!is.null(model_probs)) {
    prob_text <- paste(
      colnames(model_utilities),
      sprintf("(p=%.2f)", model_probs),
      collapse = ", "
    )
    subtitle <- paste("Model probabilities:", prob_text)
  }

  # Create base heatmap
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$Model, y = .data$Decision, fill = .data$Utility)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 1) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", .data$Utility)),
      color = "white",
      size = 4,
      fontface = "bold"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Model",
      y = "Decision",
      fill = "Utility"
    ) +
    viridis::scale_fill_viridis(option = "plasma", direction = -1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    )

  # Highlight optimal decision if provided
  if (!is.null(optimal_decision)) {
    optimal_name <- rownames(model_utilities)[optimal_decision]
    p <- p +
      ggplot2::geom_rect(
        data = subset(plot_data, plot_data$Decision == optimal_name),
        ggplot2::aes(
          xmin = as.numeric(factor(.data$Model)) - 0.5,
          xmax = as.numeric(factor(.data$Model)) + 0.5,
          ymin = as.numeric(factor(.data$Decision)) - 0.5,
          ymax = as.numeric(factor(.data$Decision)) + 0.5
        ),
        color = "yellow",
        fill = NA,
        linewidth = 1.5,
        inherit.aes = FALSE
      )
  }

  return(p)
}


#' Plot EVECA Sensitivity to Model Probabilities
#'
#' Create sensitivity analysis plots showing how EVECA changes as model
#' probabilities vary.
#'
#' @param model_utilities Matrix where rows are decisions and columns are models.
#' @param model_probs Vector of model probabilities (baseline).
#' @param vary_model Index or name of model to vary (default: 1).
#' @param prob_seq Sequence of probabilities to test (default: seq(0, 1, 0.05)).
#' @param title Plot title (default: auto-generated).
#'
#' @return A ggplot object.
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' # Run example analysis
#' result <- example_evca()
#'
#' # Sensitivity for first model
#' p <- plot_sensitivity(
#'   result$model_utilities,
#'   result$model_probs,
#'   vary_model = 1
#' )
#' print(p)
plot_sensitivity <- function(model_utilities,
                             model_probs,
                             vary_model = 1,
                             prob_seq = seq(0, 1, by = 0.05),
                             title = NULL) {
  # Convert to matrix if needed
  if (!is.matrix(model_utilities)) {
    model_utilities <- as.matrix(model_utilities)
  }

  # Get model name
  if (is.character(vary_model)) {
    model_idx <- which(colnames(model_utilities) == vary_model)
    model_name <- vary_model
  } else {
    model_idx <- vary_model
    model_name <- colnames(model_utilities)[model_idx]
  }

  # Create title if not provided
  if (is.null(title)) {
    title <- sprintf("EVECA Sensitivity: Varying P(%s)", model_name)
  }

  # Compute EVECA for each probability value
  evca_values <- numeric(length(prob_seq))

  for (i in seq_along(prob_seq)) {
    p <- prob_seq[i]

    # Adjust probabilities
    new_probs <- model_probs
    new_probs[model_idx] <- p

    # Handle edge cases
    if (p == 1) {
      new_probs[-model_idx] <- 0
    } else if (sum(new_probs[-model_idx]) > 0) {
      # Scale other probabilities to sum to 1-p
      new_probs[-model_idx] <- new_probs[-model_idx] / sum(new_probs[-model_idx]) * (1 - p)
    }

    # Compute EVECA with new probabilities
    new_bma_eu <- as.vector(model_utilities %*% new_probs)
    new_optimal_utility <- max(new_bma_eu)
    new_perfect_info_eu <- sum(new_probs * apply(model_utilities, 2, max))
    evca_values[i] <- new_perfect_info_eu - new_optimal_utility
  }

  # Create plot data
  plot_data <- data.frame(
    Probability = prob_seq,
    EVECA = evca_values
  )

  # Baseline probability
  baseline_prob <- model_probs[model_idx]
  baseline_evca <- evca_values[which.min(abs(prob_seq - baseline_prob))]

  # Create plot
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$Probability, y = .data$EVECA)
  ) +
    ggplot2::geom_line(color = "#4E79A7", linewidth = 1.2) +
    ggplot2::geom_point(
      data = data.frame(Probability = baseline_prob, EVECA = baseline_evca),
      ggplot2::aes(x = .data$Probability, y = .data$EVECA),
      color = "#E15759",
      size = 4,
      shape = 17
    ) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf(
        "Baseline: P(%s) = %.3f, EVECA = %.4f",
        model_name, baseline_prob, baseline_evca
      ),
      x = sprintf("Probability of %s", model_name),
      y = "EVECA"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}
