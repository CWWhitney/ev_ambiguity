#' Expected Value of Perfect Information (EVPI) Gap
#'
#' Computes the standard EVPI gap on a decision-by-state utility matrix:
#' expected utility with perfect information about the state minus the
#' expected utility of the optimal decision under uncertainty. Algebraically
#' identical to [compute_evca()] when model columns are treated as states;
#' the distinction is interpretive (see Details).
#'
#' @usage compute_evpi_gap(model_utilities, model_probs)
#'
#' @param model_utilities Matrix where rows are decisions and columns are
#'   states (or models, after a common-scale bridge has been applied).
#' @param model_probs Numeric vector of probabilities over columns. Must sum
#'   to 1 and be non-negative.
#'
#' @return Numeric scalar: the EVPI gap.
#'
#' @details
#' After rescaling incommensurable stakeholder models onto a common scale
#' via [normalize_utilities()], EVCA and EVPI share the same formula. EVPI
#' applies when uncertainty lives within a shared model/state space; EVCA
#' applies when the bridge between structurally incommensurable causal
#' accounts must be constructed first.
#'
#' @seealso [compute_evca()], [normalize_utilities()]
#'
#' @keywords decision-analysis value-of-information internal
#'
#' @examples
#' U <- matrix(
#'   c(10, 5, 6, 9),
#'   nrow = 2, ncol = 2, byrow = TRUE
#' )
#' p <- c(0.5, 0.5)
#' compute_evpi_gap(U, p)
#'
#' @export
compute_evpi_gap <- function(model_utilities, model_probs) {
  result <- compute_evca(model_utilities, model_probs)
  result$evca
}


#' Check Whether EVCA Equals the EVPI Gap
#'
#' Returns TRUE when the EVCA value from [compute_evca()] equals the EVPI
#' gap from [compute_evpi_gap()] within tolerance. They are algebraically
#' identical on any common-scale utility matrix; this helper is useful in
#' tests and vignettes when documenting the EVPI boundary case.
#'
#' @usage evca_equals_evpi(model_utilities, model_probs, tolerance = 1e-10)
#'
#' @param model_utilities Utility matrix (decisions x models/states).
#' @param model_probs Probability vector over columns.
#' @param tolerance Numeric tolerance for equality (default: 1e-10).
#'
#' @return Logical scalar.
#'
#' @keywords decision-analysis value-of-information internal
#'
#' @examples
#' U <- normalize_utilities(matrix(c(10, 5, 6, 9), nrow = 2, byrow = TRUE))
#' evca_equals_evpi(U, c(0.5, 0.5))
#'
#' @export
evca_equals_evpi <- function(model_utilities, model_probs, tolerance = 1e-10) {
  evca <- compute_evca(model_utilities, model_probs)$evca
  evpi <- compute_evpi_gap(model_utilities, model_probs)
  isTRUE(abs(evca - evpi) <= tolerance)
}
