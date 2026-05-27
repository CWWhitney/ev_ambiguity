#' EVCA Sensitivity Analysis via Dirichlet Model Weights
#'
#' Computes EVCA across a distribution of model weight vectors drawn from a
#' Dirichlet distribution. This turns EVCA from a point estimate (at fixed
#' model weights) into a distribution, allowing assessment of how sensitive
#' the BMA-optimal decision is to uncertainty over model weights.
#'
#' @usage compute_evca_dirichlet(model_utilities, alpha, n_draws = 5000,
#'   seed = NULL)
#'
#' @param model_utilities Matrix where rows are decisions and columns are models,
#'   containing expected utilities for each decision under each model.
#'   Can also be a data frame that can be coerced to matrix.
#'   When welfare units are incommensurable across models, apply
#'   min-max normalization before calling this function.
#' @param alpha Numeric vector of Dirichlet concentration parameters, one per
#'   model. Must be strictly positive (> 0). Larger alpha_k concentrates prior
#'   weight on model k; equal values (e.g., `rep(2, K)`) give symmetric
#'   uncertainty across models.
#' @param n_draws Integer. Number of Monte Carlo draws from the Dirichlet
#'   distribution (default: 5000).
#' @param seed Integer or NULL. Random seed for reproducibility (default: NULL).
#'
#' @return A list with the following components:
#'   \item{evca_draws}{Numeric vector of EVCA values, one per draw (length n_draws).}
#'   \item{optimal_actions}{Character vector of BMA-optimal decision names, one per draw.}
#'   \item{alpha}{Dirichlet concentration parameters used.}
#'   \item{n_draws}{Number of Monte Carlo draws performed.}
#'   \item{mean_evca}{Mean EVCA across all draws.}
#'   \item{sd_evca}{Standard deviation of EVCA across all draws.}
#'   \item{action_frequencies}{Named numeric vector: fraction of draws in which each action is BMA-optimal.}
#'   \item{model_utilities}{The utility matrix used (for reference and plotting).}
#'
#' @details
#' Standard EVCA is computed at a single fixed model weight vector (e.g., equal
#' weights). But the same structural uncertainty that motivates EVCA also applies
#' to the weights themselves: analysts who disagree about which causal model is
#' correct will also disagree about prior credences.
#'
#' The Dirichlet distribution provides a coherent prior over the probability
#' simplex. The concentration vector alpha encodes prior beliefs about model
#' credibility:
#'
#' \itemize{
#'   \item Equal alpha (e.g., rep(2, K)): symmetric uncertainty, no model
#'     preferred a priori.
#'   \item High alpha_k relative to others: model k is believed more plausible.
#'   \item Low alpha_k: model k is systematically discounted (e.g., indigenous
#'     knowledge models in standard planning processes).
#' }
#'
#' A high-EVCA distribution signals that the decision is sensitive to model
#' weight uncertainty. A widening EVCA distribution when alpha_k is small for
#' one model signals that marginalising that model increases decision fragility:
#' if the discounted model turns out to be correct, the welfare loss is large.
#'
#' Draws use the standard method: X_k ~ Gamma(alpha_k, 1) independently,
#' then normalize: p_k = X_k / sum(X). This is exact Dirichlet sampling.
#'
#' @references
#' Hoeting, J. A., Madigan, D., Raftery, A. E., & Volinsky, C. T. (1999).
#' Bayesian model averaging: a tutorial. Statistical Science, 14(4), 382-401.
#'
#' @keywords decision-analysis sensitivity dirichlet model-uncertainty
#'
#' @importFrom stats rgamma sd
#'
#' @examples
#' # Luang Prabang road decision
#' model_utilities <- matrix(
#'   c(
#'      5, 40, 80,
#'     90, 82,  8,
#'     72, 78, 55
#'   ),
#'   nrow = 3, ncol = 3, byrow = TRUE,
#'   dimnames = list(
#'     c("No road", "Direct route", "Rerouted road"),
#'     c("Officials", "ProDev_Hmong", "Traditional_Hmong")
#'   )
#' )
#'
#' # Normalize utilities first (required when welfare units differ)
#' norm_col <- function(x) (x - min(x)) / (max(x) - min(x))
#' U_norm <- apply(model_utilities, 2, norm_col)
#'
#' # Equal uncertainty prior
#' result_eq <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2), seed = 42)
#' cat("Mean EVCA:", result_eq$mean_evca, "\n")
#' print(result_eq$action_frequencies)
#'
#' # Development-coalition prior (marginalises Traditional_Hmong)
#' result_dc <- compute_evca_dirichlet(U_norm, alpha = c(8, 8, 2), seed = 42)
#' cat("Mean EVCA (biased prior):", result_dc$mean_evca, "\n")
#' print(result_dc$action_frequencies)
#'
#' # Community-weighted prior (upweights Traditional_Hmong)
#' result_cw <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 8), seed = 42)
#' cat("Mean EVCA (community-weighted):", result_cw$mean_evca, "\n")
#'
#' @export
compute_evca_dirichlet <- function(model_utilities,
                                    alpha,
                                    n_draws = 5000,
                                    seed = NULL) {
  # Input validation ----

  # Coerce data frame to matrix if needed
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

  n_decisions <- nrow(model_utilities)
  n_models    <- ncol(model_utilities)

  if (any(is.na(model_utilities))) {
    stop(
      "model_utilities contains NA values. ",
      "Please remove or impute missing values before computing EVCA."
    )
  }

  # Validate alpha
  if (!is.numeric(alpha)) {
    stop("alpha must be a numeric vector.")
  }
  if (length(alpha) != n_models) {
    stop(
      "Length of alpha (", length(alpha), ") ",
      "must equal number of model columns (", n_models, ")."
    )
  }
  if (any(alpha <= 0)) {
    stop(
      "All alpha values must be strictly positive (> 0). ",
      "Found ", sum(alpha <= 0), " non-positive value(s)."
    )
  }

  # Validate n_draws
  if (!is.numeric(n_draws) || length(n_draws) != 1 || n_draws < 1) {
    stop("n_draws must be a single positive integer.")
  }
  n_draws <- as.integer(n_draws)

  # Validate seed
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("seed must be a single numeric value or NULL.")
    }
    set.seed(seed)
  }

  # Ensure row/col names ----
  if (is.null(rownames(model_utilities))) {
    rownames(model_utilities) <- paste("Decision", seq_len(n_decisions))
  }
  if (is.null(colnames(model_utilities))) {
    colnames(model_utilities) <- paste("Model", seq_len(n_models))
  }

  # Draw from Dirichlet distribution ----
  # X_k ~ Gamma(alpha_k, rate=1); normalize so sum = 1
  raw <- matrix(
    stats::rgamma(n_draws * n_models,
                  shape = rep(alpha, each = n_draws),
                  rate  = 1),
    nrow = n_draws,
    ncol = n_models
  )
  p_draws <- raw / rowSums(raw)

  # Per-model optimal utilities (constant across draws; compute once) ----
  per_model_max <- apply(model_utilities, 2, max)

  # Compute EVCA for each draw ----
  evca_draws    <- numeric(n_draws)
  optimal_draws <- character(n_draws)

  for (i in seq_len(n_draws)) {
    p           <- p_draws[i, ]
    bma_eu_i    <- as.vector(model_utilities %*% p)
    perf_i      <- sum(p * per_model_max)
    evca_draws[i]    <- perf_i - max(bma_eu_i)
    optimal_draws[i] <- rownames(model_utilities)[which.max(bma_eu_i)]
  }

  # Compute action frequencies ----
  action_tbl  <- table(optimal_draws)
  action_freq <- as.numeric(action_tbl) / n_draws
  names(action_freq) <- names(action_tbl)

  # Return ----
  return(list(
    evca_draws        = evca_draws,
    optimal_actions   = optimal_draws,
    alpha             = alpha,
    n_draws           = n_draws,
    mean_evca         = mean(evca_draws),
    sd_evca           = stats::sd(evca_draws),
    action_frequencies = action_freq,
    model_utilities   = model_utilities
  ))
}


#' Plot EVCA Distribution from Dirichlet Sensitivity Analysis
#'
#' Creates a histogram of EVCA values from [compute_evca_dirichlet()], with a
#' vertical dashed line at the mean EVCA. Supports a single result or a named
#' list of results for multi-scenario comparison (faceted).
#'
#' @param dirichlet_result A single list returned by [compute_evca_dirichlet()],
#'   or a named list of such results for multi-scenario comparison.
#' @param title Plot title. Default: "EVCA Distribution under Dirichlet Weight
#'   Uncertainty".
#' @param bins Integer. Number of histogram bins (default: 40).
#'
#' @return A ggplot object that can be further customized or printed.
#'
#' @seealso [compute_evca_dirichlet()], [plot_dirichlet_actions()]
#'
#' @keywords decision-analysis sensitivity dirichlet visualization
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' model_utilities <- matrix(
#'   c(
#'      5, 40, 80,
#'     90, 82,  8,
#'     72, 78, 55
#'   ),
#'   nrow = 3, ncol = 3, byrow = TRUE,
#'   dimnames = list(
#'     c("No road", "Direct route", "Rerouted road"),
#'     c("Officials", "ProDev_Hmong", "Traditional_Hmong")
#'   )
#' )
#' norm_col <- function(x) (x - min(x)) / (max(x) - min(x))
#' U_norm <- apply(model_utilities, 2, norm_col)
#'
#' # Single scenario
#' r1 <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2), seed = 42)
#' plot_dirichlet_evca(r1)
#'
#' # Multiple scenarios (faceted)
#' r2 <- compute_evca_dirichlet(U_norm, alpha = c(8, 8, 2), seed = 42)
#' plot_dirichlet_evca(list("Equal (2,2,2)" = r1, "Dev-coalition (8,8,2)" = r2))
plot_dirichlet_evca <- function(dirichlet_result,
                                 title = "EVCA Distribution under Dirichlet Weight Uncertainty",
                                 bins  = 40) {
  # Allow single result or named list of results ----
  if (is.list(dirichlet_result) && "evca_draws" %in% names(dirichlet_result)) {
    dirichlet_result <- list("Result" = dirichlet_result)
  }

  if (!is.list(dirichlet_result)) {
    stop(
      "dirichlet_result must be output from compute_evca_dirichlet(), ",
      "or a named list of such outputs."
    )
  }

  # Build combined plot data ----
  plot_data <- do.call(rbind, lapply(names(dirichlet_result), function(sname) {
    res <- dirichlet_result[[sname]]
    if (!"evca_draws" %in% names(res)) {
      stop(
        "Each element of dirichlet_result must be output from compute_evca_dirichlet(). ",
        "Element '", sname, "' is missing 'evca_draws'."
      )
    }
    data.frame(
      Scenario  = sname,
      EVCA      = res$evca_draws,
      mean_evca = res$mean_evca,
      stringsAsFactors = FALSE
    )
  }))

  plot_data$Scenario <- factor(plot_data$Scenario,
                               levels = names(dirichlet_result))

  mean_data <- unique(plot_data[, c("Scenario", "mean_evca")])

  n_scenarios <- length(unique(plot_data$Scenario))

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$EVCA, fill = .data$Scenario)
  ) +
    ggplot2::geom_histogram(bins = bins, alpha = 0.75, colour = "white") +
    ggplot2::geom_vline(
      data     = mean_data,
      ggplot2::aes(xintercept = .data$mean_evca),
      linetype = "dashed",
      linewidth = 0.7
    ) +
    ggplot2::labs(
      title    = title,
      subtitle = "Dashed line = mean EVCA | Higher EVCA = decision more sensitive to model choice",
      x        = "EVCA (normalised welfare units)",
      y        = "Count",
      fill     = "Scenario"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  if (n_scenarios > 1) {
    p <- p +
      ggplot2::facet_wrap(~Scenario, ncol = 1, scales = "free_y") +
      ggplot2::theme(legend.position = "none")
  } else {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}


#' Plot BMA-Optimal Action Frequencies from Dirichlet Sensitivity Analysis
#'
#' Creates a stacked bar chart showing what fraction of Dirichlet weight draws
#' result in each action being BMA-optimal. Supports a single result or a named
#' list of results for multi-scenario comparison.
#'
#' @param dirichlet_result A single list returned by [compute_evca_dirichlet()],
#'   or a named list of such results.
#' @param title Plot title. Default: "BMA-Optimal Action across Dirichlet Weight
#'   Draws".
#' @param decision_order Optional character vector specifying the stacking order
#'   of decisions in the bars (default: order found in action_frequencies).
#'
#' @return A ggplot object that can be further customized or printed.
#'
#' @seealso [compute_evca_dirichlet()], [plot_dirichlet_evca()]
#'
#' @keywords decision-analysis sensitivity dirichlet visualization
#'
#' @export
#' @import ggplot2
#'
#' @examples
#' model_utilities <- matrix(
#'   c(
#'      5, 40, 80,
#'     90, 82,  8,
#'     72, 78, 55
#'   ),
#'   nrow = 3, ncol = 3, byrow = TRUE,
#'   dimnames = list(
#'     c("No road", "Direct route", "Rerouted road"),
#'     c("Officials", "ProDev_Hmong", "Traditional_Hmong")
#'   )
#' )
#' norm_col <- function(x) (x - min(x)) / (max(x) - min(x))
#' U_norm <- apply(model_utilities, 2, norm_col)
#'
#' r1 <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2), seed = 42)
#' r2 <- compute_evca_dirichlet(U_norm, alpha = c(8, 8, 2), seed = 42)
#' plot_dirichlet_actions(
#'   list("Equal (2,2,2)" = r1, "Dev-coalition (8,8,2)" = r2),
#'   decision_order = c("No road", "Direct route", "Rerouted road")
#' )
plot_dirichlet_actions <- function(dirichlet_result,
                                    title         = "BMA-Optimal Action across Dirichlet Weight Draws",
                                    decision_order = NULL) {
  # Allow single result or named list ----
  if (is.list(dirichlet_result) && "evca_draws" %in% names(dirichlet_result)) {
    dirichlet_result <- list("Result" = dirichlet_result)
  }

  if (!is.list(dirichlet_result)) {
    stop(
      "dirichlet_result must be output from compute_evca_dirichlet(), ",
      "or a named list of such outputs."
    )
  }

  # Build action-frequency data frame ----
  plot_data <- do.call(rbind, lapply(names(dirichlet_result), function(sname) {
    res   <- dirichlet_result[[sname]]
    if (!"action_frequencies" %in% names(res)) {
      stop(
        "Each element must be output from compute_evca_dirichlet(). ",
        "Element '", sname, "' is missing 'action_frequencies'."
      )
    }
    freqs <- res$action_frequencies
    data.frame(
      Scenario = sname,
      Action   = names(freqs),
      Share    = as.numeric(freqs),
      stringsAsFactors = FALSE
    )
  }))

  plot_data$Scenario <- factor(plot_data$Scenario,
                               levels = names(dirichlet_result))

  # Set action (decision) factor order ----
  all_actions <- unique(plot_data$Action)
  if (!is.null(decision_order)) {
    matched   <- intersect(decision_order, all_actions)
    remaining <- setdiff(all_actions, matched)
    all_actions <- c(matched, remaining)
  }
  plot_data$Action <- factor(plot_data$Action, levels = all_actions)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$Scenario, y = .data$Share, fill = .data$Action)
  ) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(.data$Share * 100), "%")),
      position  = ggplot2::position_stack(vjust = 0.5),
      size      = 3.5,
      colour    = "white",
      fontface  = "bold"
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(round(x * 100), "%")
    ) +
    ggplot2::labs(
      title    = title,
      subtitle = "How often each action is BMA-optimal under sampled model weights",
      x        = "Dirichlet prior scenario",
      y        = "Fraction of draws",
      fill     = "BMA-optimal action"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "top")
}
