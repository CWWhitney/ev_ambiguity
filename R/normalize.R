#' Normalize a Utility Matrix for Cross-Model Comparison
#'
#' Applies min-max normalization independently to each column (model) of a
#' utility matrix, rescaling each model's welfare range to \[0, 1\]. This step
#' is required before calling [compute_evca()] or [compute_exclusion_costs()]
#' when competing models use incommensurable welfare units.
#'
#' @usage normalize_utilities(model_utilities, warn_if_normalized = TRUE)
#'
#' @param model_utilities Matrix where rows are decisions and columns are models,
#'   containing expected utilities for each decision under each model.
#'   Can also be a data frame that can be coerced to matrix.
#' @param warn_if_normalized Logical. If TRUE (default), emits a warning when
#'   the matrix appears to already be normalized (all values in \[0, 1\] and
#'   each column spanning at least 90 percent of that range). Set to FALSE to
#'   suppress.
#'
#' @return A matrix with the same dimensions and dimnames as
#'   \code{model_utilities}, with each column independently rescaled to
#'   \[0, 1\]. Within each model column, 0 represents the worst outcome for
#'   that model and 1 represents the best.
#'
#' @details
#' When competing causal models use different welfare units (e.g., NPV for an
#' economic model, household income for a livelihood model, community wellbeing
#' for a cultural model), direct comparison across model columns is not
#' meaningful. Min-max normalization rescales each column so that:
#'
#' \deqn{u_{\text{norm}}[i, k] =
#'   \frac{u[i, k] - \min_i u[i, k]}{\max_i u[i, k] - \min_i u[i, k]}}
#'
#' This places all models on a common \[0, 1\] scale where 0 = worst outcome
#' the model anticipates for any action, and 1 = best.
#'
#' @section Political assumption:
#' Min-max normalization assigns equal stakes to the welfare range of each
#' model. A model whose stakeholders face catastrophic outcomes is treated as
#' equally important as one whose stakeholders face minor inconveniences,
#' because both are scaled to the same \[0, 1\] range. This is a deliberate
#' political assumption: it asserts that the full span of possible outcomes
#' matters equally across all stakeholder frameworks, regardless of absolute
#' magnitude. Analysts should state this assumption explicitly when reporting
#' results, and consider reporting sensitivity to alternative normalization
#' choices.
#'
#' @seealso [compute_evca()], [compute_exclusion_costs()]
#'
#' @references
#' Claxton, K. (1999). The irrelevance of inference: a decision-making approach
#' to the stochastic evaluation of health care technologies. Journal of Health
#' Economics, 18(3), 341-364.
#'
#' @keywords decision-analysis value-of-information normalization
#'
#' @examples
#' # Raw utilities in three incommensurable units:
#' # Officials use NPV, Pro-dev Hmong use income, Traditional Hmong use wellbeing
#' model_utilities <- matrix(
#'   c(
#'     5, 40, 80, # No road
#'     90, 82, 8, # Direct route
#'     72, 78, 55 # Rerouted road
#'   ),
#'   nrow = 3, ncol = 3, byrow = TRUE,
#'   dimnames = list(
#'     c("No road", "Direct route", "Rerouted road"),
#'     c("Officials", "ProDev_Hmong", "Traditional_Hmong")
#'   )
#' )
#'
#' # Normalize before computing EVCA
#' U_norm <- normalize_utilities(model_utilities)
#' print(round(U_norm, 3))
#'
#' # Each column is now in [0, 1]:
#' # 1 = best outcome within that stakeholder's own welfare scale
#' # 0 = worst outcome within that stakeholder's own welfare scale
#'
#' # Pass directly to compute_evca()
#' result <- compute_evca(U_norm, model_probs = c(1 / 3, 1 / 3, 1 / 3))
#' cat("EVCA:", round(result$evca, 3), "\n")
#'
#' # Or pass to compute_exclusion_costs() with normalize = FALSE
#' # (normalization already done)
#' excl <- compute_exclusion_costs(U_norm, normalize = FALSE)
#' print(excl)
#'
#' @export
normalize_utilities <- function(model_utilities, warn_if_normalized = TRUE) {
  # Input validation ----

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

  if (any(is.na(model_utilities))) {
    stop(
      "model_utilities contains NA values. ",
      "Please remove or impute missing values before normalizing."
    )
  }

  if (nrow(model_utilities) < 2) {
    stop(
      "model_utilities must have at least 2 rows (decisions) to normalize. ",
      "Normalization is meaningless for a single decision."
    )
  }

  # Warn if the matrix looks already normalized ----
  if (warn_if_normalized) {
    all_in_01 <- all(model_utilities >= 0 & model_utilities <= 1)
    cols_span <- all(apply(model_utilities, 2, function(x) max(x) - min(x) >= 0.9))
    if (all_in_01 && cols_span) {
      warning(
        "model_utilities appears to already be normalized: all values are in [0, 1] ",
        "and each column spans at least 90% of that range. ",
        "Set warn_if_normalized = FALSE to suppress this warning if intentional."
      )
    }
  }

  # Min-max normalization per column ----
  normalize_col <- function(x) {
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) < .Machine$double.eps) {
      warning(
        "A model column has zero range: all decisions have identical utility ",
        "under that model. Setting all normalized values to 0.5."
      )
      return(rep(0.5, length(x)))
    }
    (x - rng[1]) / diff(rng)
  }

  result <- apply(model_utilities, 2, normalize_col)

  # Preserve dimnames from input ----
  dimnames(result) <- dimnames(model_utilities)

  return(result)
}
