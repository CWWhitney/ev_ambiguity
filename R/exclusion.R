#' Compute Exclusion Costs for Each Causal Model
#'
#' For each model, computes the welfare cost of excluding that model from
#' deliberation entirely. When a model is excluded, the BMA-optimal decision
#' is found using only the remaining models, and the excluded model's welfare
#' under that decision is compared to its best possible outcome.
#'
#' @usage compute_exclusion_costs(model_utilities, model_probs = NULL,
#'   normalize = TRUE)
#'
#' @param model_utilities Matrix where rows are decisions and columns are models,
#'   containing expected utilities for each decision under each model.
#'   Can also be a data frame that can be coerced to matrix. Each cell [i,j]
#'   represents the expected utility of decision i under model j.
#' @param model_probs Numeric vector of model probabilities. If provided,
#'   the probabilities for remaining models are renormalized proportionally
#'   when one model is excluded. If NULL (default), equal weights are assigned
#'   to the remaining models, reflecting maximum uncertainty about them.
#' @param normalize Logical. If TRUE (default), applies min-max normalization
#'   to each model column before computing exclusion costs. Required for
#'   meaningful cross-model comparison when welfare units are incommensurable
#'   (e.g., NPV alongside wellbeing scores). The normalization rescales each
#'   model's welfare range to [0, 1], where 0 is the worst outcome and 1 is
#'   the best for that model. This is itself a political assumption (equal stakes
#'   for each model) and should be reported explicitly.
#'
#' @return A data frame with one row per excluded model, containing:
#'   \item{excluded_model}{Name of the excluded model (or "Model k" if unnamed).}
#'   \item{decision_without}{The BMA-optimal decision made without this model.}
#'   \item{welfare_excluded}{The excluded model's welfare under that decision.}
#'   \item{welfare_own_best}{The best welfare the excluded model could receive.}
#'   \item{exclusion_cost}{Welfare loss: welfare_own_best - welfare_excluded.}
#'
#' @details
#' Exclusion cost quantifies the stakes of model exclusion — distinct from
#' EVCA, which quantifies the cost of model uncertainty. Where EVCA asks
#' "what would perfect structural knowledge be worth?", exclusion analysis asks
#' "what does it cost a particular stakeholder group when their model is not
#' at the table?"
#'
#' Provincial decisions routinely proceed under a single dominant framework
#' (e.g., an economic NPV model). The exclusion analysis makes the cost of
#' this structural exclusion explicit and quantified.
#'
#' When welfare units are incommensurable across models, normalization is
#' required to place exclusion costs on a comparable scale. The default
#' min-max normalization makes welfare_own_best = 1 for every model (by
#' construction), so exclusion_cost directly measures the fractional loss
#' relative to the best attainable outcome.
#'
#' @seealso [compute_evca()] for the related but distinct EVCA computation.
#'
#' @references
#' Hoeting, J. A., Madigan, D., Raftery, A. E., & Volinsky, C. T. (1999).
#' Bayesian model averaging: a tutorial. Statistical Science, 14(4), 382-401.
#' @references
#' Yokota, F., & Thompson, K. M. (2004). Value of information analysis in
#' environmental health risk management decisions: past, present, and future.
#' Risk Analysis, 24(3), 635-650.
#'
#' @keywords decision-analysis value-of-information exclusion
#'
#' @examples
#' # Luang Prabang road decision: three stakeholder models
#' model_utilities <- matrix(
#'   c(
#'      5, 40, 80,   # No road
#'     90, 82,  8,   # Direct route
#'     72, 78, 55    # Rerouted road
#'   ),
#'   nrow = 3, ncol = 3, byrow = TRUE,
#'   dimnames = list(
#'     c("No road", "Direct route", "Rerouted road"),
#'     c("Officials", "ProDev_Hmong", "Traditional_Hmong")
#'   )
#' )
#'
#' # With default min-max normalization
#' excl <- compute_exclusion_costs(model_utilities)
#' print(excl)
#'
#' # Excluding Traditional_Hmong is catastrophic (exclusion_cost near 1)
#' subset(excl, excluded_model == "Traditional_Hmong")
#'
#' # With proportional model weights
#' excl_w <- compute_exclusion_costs(
#'   model_utilities,
#'   model_probs = c(0.4, 0.4, 0.2)
#' )
#' print(excl_w)
#'
#' # Without normalization (only when utilities are already on comparable scales)
#' excl_raw <- compute_exclusion_costs(model_utilities, normalize = FALSE)
#' print(excl_raw)
#'
#' @export
compute_exclusion_costs <- function(model_utilities,
                                    model_probs = NULL,
                                    normalize = TRUE) {
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

  # Exclusion requires at least 2 models
  if (n_models < 2) {
    stop(
      "compute_exclusion_costs requires at least 2 models. ",
      "Received ", n_models, " model(s)."
    )
  }

  if (n_decisions < 1) {
    stop("model_utilities must have at least one row (decision).")
  }

  if (any(is.na(model_utilities))) {
    stop(
      "model_utilities contains NA values. ",
      "Please remove or impute missing values before computing exclusion costs."
    )
  }

  # Validate model_probs if provided
  if (!is.null(model_probs)) {
    if (!is.numeric(model_probs)) {
      stop("model_probs must be a numeric vector.")
    }
    if (length(model_probs) != n_models) {
      stop(
        "Length of model_probs (", length(model_probs), ") ",
        "must equal number of model columns (", n_models, ")."
      )
    }
    if (any(model_probs < 0)) {
      stop(
        "model_probs must be non-negative. ",
        "Found ", sum(model_probs < 0), " negative value(s)."
      )
    }
    if (abs(sum(model_probs) - 1) > 1e-10) {
      stop(
        "model_probs must sum to 1 (within 1e-10 tolerance). ",
        "Current sum: ", round(sum(model_probs), 10)
      )
    }
  }

  # Normalize utilities ----
  if (normalize) {
    normalize_col <- function(x) {
      rng <- range(x, na.rm = TRUE)
      if (diff(rng) < .Machine$double.eps) {
        warning(
          "A model column has zero range; all values set to 0.5. ",
          "Check that your utility matrix is correctly specified."
        )
        return(rep(0.5, length(x)))
      }
      (x - rng[1]) / diff(rng)
    }
    model_utilities <- apply(model_utilities, 2, normalize_col)
  }

  # Ensure row/col names ----
  if (is.null(rownames(model_utilities))) {
    rownames(model_utilities) <- paste("Decision", seq_len(n_decisions))
  }
  model_names <- colnames(model_utilities)
  if (is.null(model_names)) {
    model_names <- paste("Model", seq_len(n_models))
    colnames(model_utilities) <- model_names
  }

  # Compute exclusion costs ----
  results <- lapply(seq_len(n_models), function(k) {
    models_in <- seq_len(n_models)[-k]

    # Weights for remaining models
    if (is.null(model_probs)) {
      # Equal weights: maximum uncertainty about remaining models
      weights_in <- rep(1 / length(models_in), length(models_in))
    } else {
      # Renormalize original probabilities proportionally
      p_remaining <- model_probs[models_in]
      weights_in  <- p_remaining / sum(p_remaining)
    }

    # BMA-optimal decision without model k
    bma_excl        <- as.vector(model_utilities[, models_in, drop = FALSE] %*% weights_in)
    names(bma_excl) <- rownames(model_utilities)
    decision_excl   <- rownames(model_utilities)[which.max(bma_excl)]

    # Welfare of excluded group under that decision
    welfare_if_excl  <- model_utilities[decision_excl, k]

    # Best welfare the excluded group could have received
    welfare_own_best <- max(model_utilities[, k])

    data.frame(
      excluded_model   = model_names[k],
      decision_without = decision_excl,
      welfare_excluded = welfare_if_excl,
      welfare_own_best = welfare_own_best,
      exclusion_cost   = welfare_own_best - welfare_if_excl,
      stringsAsFactors = FALSE
    )
  })

  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL
  return(result_df)
}


#' Plot Exclusion Costs by Model
#'
#' Creates a grouped bar chart comparing each excluded model's welfare when
#' that model is absent from deliberation versus its best possible welfare
#' (had the decision been made in its favour). The gap between the bars is
#' the exclusion cost.
#'
#' @param exclusion_df Data frame returned by [compute_exclusion_costs()].
#' @param title Plot title. Default: "Exclusion Costs by Model".
#' @param colors Length-2 character vector of bar colours. The first colour
#'   applies to the "when excluded" scenario; the second to the "own best"
#'   scenario. Default: `c("#c0392b", "#2980b9")`.
#'
#' @return A ggplot object that can be further customized or printed.
#'
#' @seealso [compute_exclusion_costs()] for the underlying computation.
#'
#' @keywords decision-analysis value-of-information exclusion visualization
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
#' excl <- compute_exclusion_costs(model_utilities)
#' plot_exclusion_costs(excl)
#'
#' # Customise colours and title
#' plot_exclusion_costs(excl,
#'   title  = "Cost of Excluding Each Stakeholder Model",
#'   colors = c("#e74c3c", "#3498db")
#' )
plot_exclusion_costs <- function(exclusion_df,
                                  title  = "Exclusion Costs by Model",
                                  colors = c("#c0392b", "#2980b9")) {
  # Validate input ----
  required_cols <- c("excluded_model", "welfare_excluded",
                     "welfare_own_best", "exclusion_cost")
  missing_cols  <- setdiff(required_cols, names(exclusion_df))
  if (length(missing_cols) > 0) {
    stop(
      "exclusion_df is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      ". Use output from compute_exclusion_costs()."
    )
  }

  if (length(colors) != 2) {
    stop("colors must be a length-2 character vector.")
  }

  # Reshape to long format (no tidyr dependency) ----
  n <- nrow(exclusion_df)
  plot_data <- data.frame(
    Model    = rep(exclusion_df$excluded_model, 2),
    Scenario = rep(
      c("When model is excluded", "Own model's best outcome"),
      each = n
    ),
    Welfare  = c(exclusion_df$welfare_excluded, exclusion_df$welfare_own_best),
    stringsAsFactors = FALSE
  )

  plot_data$Model    <- factor(plot_data$Model,
                               levels = exclusion_df$excluded_model)
  plot_data$Scenario <- factor(plot_data$Scenario,
                               levels = c("When model is excluded",
                                          "Own model's best outcome"))

  y_upper <- max(exclusion_df$welfare_own_best, na.rm = TRUE) * 1.2

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$Model, y = .data$Welfare, fill = .data$Scenario)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.7),
      width    = 0.6
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data$Welfare, 2)),
      position = ggplot2::position_dodge(width = 0.7),
      vjust    = -0.4,
      size     = 3.5
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::ylim(0, y_upper) +
    ggplot2::labs(
      title    = title,
      subtitle = "Exclusion cost = gap between bars",
      x        = "Model excluded from deliberation",
      y        = "Welfare [0\u20131, normalised within each model]",
      fill     = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "top")
}
