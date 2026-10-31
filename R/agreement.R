#' Check Whether All Models Share a Best Action
#'
#' Uses only each model's own ranking of actions. No rescaling or weights
#' needed.
#'
#' @usage check_action_agreement(model_utilities, model_weights = NULL,
#'   tolerance = 1e-10)
#'
#' @param model_utilities Matrix (decisions x models). Columns may use
#'   different scales. A data frame is coerced.
#' @param model_weights Optional weights over models. Zero-weight models are
#'   ignored.
#' @param tolerance Utilities within this distance of a model's maximum count
#'   as tied for best.
#'
#' @return A list with:
#'   \item{agree}{TRUE if one action is best under every model used.}
#'   \item{common_actions}{Indices of the shared best actions (empty if none).}
#'   \item{best_actions_per_model}{Best action indices for each model.}
#'
#' @details
#' If `agree` is TRUE, [compute_evca()] is 0 for any positive weights and any
#' rescaling. If FALSE, it is above 0 for any positive weights and rescaling.
#' The check gives no size for EVCA and ignores models that were not included.
#'
#' @seealso [compute_evca()], [normalize_utilities()]
#' @keywords decision-analysis value-of-information
#'
#' @examples
#' # Two actions, four models on different scales
#' U <- cbind(
#'   Father = c(900, 400), Brother = c(8, 5),
#'   Aunt = c(0.7, 0.3), Ecologist = c(95, 10)
#' )
#' rownames(U) <- c("Cut", "Keep")
#' check_action_agreement(U)$agree
#'
#' # Aunt now prefers keeping
#' U[, "Aunt"] <- c(0.2, 0.9)
#' check_action_agreement(U)$agree
#'
#' @export
check_action_agreement <- function(model_utilities, model_weights = NULL,
                                   tolerance = 1e-10) {
  # Coerce and validate input
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
    stop("model_utilities contains NA values.")
  }

  # Drop models with zero weight
  if (!is.null(model_weights)) {
    if (!is.numeric(model_weights) || length(model_weights) != ncol(model_utilities)) {
      stop("model_weights must be a numeric vector with one value per model column")
    }
    if (any(model_weights < 0)) {
      stop("model_weights must be non-negative")
    }
    model_utilities <- model_utilities[, model_weights > 0, drop = FALSE]
  }
  if (ncol(model_utilities) < 1 || nrow(model_utilities) < 1) {
    stop("model_utilities must have at least one decision and one model")
  }

  # Best action(s) per model, ties included
  best_actions_per_model <- lapply(seq_len(ncol(model_utilities)), function(k) {
    u <- model_utilities[, k]
    which(u >= max(u) - tolerance)
  })
  names(best_actions_per_model) <- colnames(model_utilities)

  # Actions that are best in every model
  common_actions <- Reduce(intersect, best_actions_per_model)
  if (!is.null(rownames(model_utilities))) {
    names(common_actions) <- rownames(model_utilities)[common_actions]
  }

  list(
    agree = length(common_actions) > 0,
    common_actions = common_actions,
    best_actions_per_model = best_actions_per_model
  )
}
