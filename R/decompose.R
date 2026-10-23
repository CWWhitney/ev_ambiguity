#' Decompose the Shortfall into Evidence-Reducible and Irreducible Parts
#'
#' Splits the gap between the ideal and the current shared-action value into
#' an aggregate expected value of perfect information and the expected EVCA
#' that remains once the state of the world is known.
#'
#' @details
#' Let \eqn{h} index states, \eqn{u_k(a \mid h)} be model \eqn{k}'s payoff for
#' action \eqn{a}, and \eqn{w_k} the model weights. With one expectation over
#' states,
#' \deqn{V_{now} = \max_a \sum_k w_k E[u_k(a \mid h)]}
#' \deqn{V_{agg} = E[\max_a \sum_k w_k u_k(a \mid h)]}
#' \deqn{V_{ideal} = E[\sum_k w_k \max_a u_k(a \mid h)]}
#' and \eqn{V_{ideal} - V_{now} = (V_{agg} - V_{now}) + (V_{ideal} - V_{agg})}.
#' The first term (\code{evpi_agg}) is ordinary EVPI for the aggregate decision
#' and can be reduced by evidence about the state. The second
#' (\code{evca_post}) is the EVCA that remains when the state is known; it is
#' irreducible by evidence given the stated payoffs, scale, weights and action
#' set.
#'
#' The decomposition holds only under these conditions: (1) one probability
#' measure over states, here the empirical measure of the supplied draws
#' (optionally weighted by \code{state_weights}); (2) all models are expressed
#' over the same states; (3) the common scale, weights and action set are fixed
#' across states. Normalization constants therefore must not be re-estimated
#' per state; supply them once through \code{scale_min} and \code{scale_range}.
#'
#' @param payoffs Numeric array with dimensions states x decisions x models.
#'   Each slice \code{payoffs[i, , ]} is one possible state of the world.
#' @param model_weights Non-negative weights over models, summing to 1.
#' @param scale_min,scale_range Optional length-K vectors. If both are given,
#'   payoffs are rescaled as \code{(x - scale_min) / scale_range} per model,
#'   with the constants held fixed across states. If omitted, payoffs are
#'   assumed to be on the common scale already.
#' @param state_weights Optional non-negative weights over states summing to 1
#'   (default: equal).
#'
#' @return A list with \code{v_now}, \code{v_agg}, \code{v_ideal}, \code{gap},
#'   \code{evpi_agg}, \code{evca_post}, and \code{evca_means}, the Layer 3
#'   EVCA computed on state-averaged payoffs.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' mu <- cbind(c(5, 90, 72), c(40, 82, 78), c(80, 8, 55))  # decisions x models
#' draws <- array(NA_real_, c(500, 3, 3))
#' for (i in 1:500) draws[i, , ] <- mu + rnorm(9, sd = 6)
#' evca_decompose(draws, model_weights = rep(1/3, 3),
#'                scale_min = apply(mu, 2, min),
#'                scale_range = apply(mu, 2, function(x) diff(range(x))))
evca_decompose <- function(payoffs, model_weights,
                           scale_min = NULL, scale_range = NULL,
                           state_weights = NULL) {
  if (!is.numeric(payoffs) || length(dim(payoffs)) != 3L || anyNA(payoffs)) {
    stop("payoffs must be a numeric states x decisions x models array without NA.")
  }
  n <- dim(payoffs)[1]; A <- dim(payoffs)[2]; K <- dim(payoffs)[3]
  if (length(model_weights) != K || any(model_weights < 0) ||
      abs(sum(model_weights) - 1) > 1e-10) {
    stop("model_weights must be non-negative, length equal to the number of models, and sum to 1.")
  }
  if (is.null(state_weights)) state_weights <- rep(1 / n, n)
  if (length(state_weights) != n || any(state_weights < 0) ||
      abs(sum(state_weights) - 1) > 1e-10) {
    stop("state_weights must be non-negative, length equal to the number of states, and sum to 1.")
  }
  if (xor(is.null(scale_min), is.null(scale_range))) {
    stop("Supply both scale_min and scale_range, or neither.")
  }
  if (!is.null(scale_min)) {
    if (length(scale_min) != K || length(scale_range) != K || any(scale_range <= 0)) {
      stop("scale_min and scale_range must have length equal to the number of models; scale_range must be positive.")
    }
    for (k in seq_len(K)) {
      payoffs[, , k] <- (payoffs[, , k] - scale_min[k]) / scale_range[k]
    }
  }

  # aggregate payoff per state and decision: states x decisions
  agg <- matrix(matrix(payoffs, n * A, K) %*% model_weights, n, A)
  # each model's best payoff per state: states x models
  best <- apply(payoffs, c(1, 3), max)

  v_now   <- max(colSums(agg * state_weights))
  v_agg   <- sum(state_weights * apply(agg, 1, max))
  v_ideal <- sum(state_weights * as.vector(best %*% model_weights))

  mean_u <- apply(payoffs, c(2, 3), function(x) sum(x * state_weights))
  evca_means <- sum(model_weights * apply(mean_u, 2, max)) -
    max(as.vector(mean_u %*% model_weights))

  list(v_now = v_now, v_agg = v_agg, v_ideal = v_ideal,
       gap = v_ideal - v_now,
       evpi_agg = v_agg - v_now,
       evca_post = v_ideal - v_agg,
       evca_means = evca_means)
}
