test_that("check_action_agreement reports a shared best action", {
  # Columns on different scales
  U <- cbind(a = c(0.9, 0.4), b = c(8, 5), c = c(95, 10))
  rownames(U) <- c("Cut", "Keep")
  res <- check_action_agreement(U)
  expect_true(res$agree)
  expect_equal(unname(res$common_actions), 1)
  expect_named(res$common_actions, "Cut")
})

test_that("check_action_agreement reports disagreement", {
  U <- cbind(a = c(0.9, 0.4), b = c(0.2, 0.9))
  res <- check_action_agreement(U)
  expect_false(res$agree)
  expect_length(res$common_actions, 0)
})

test_that("check_action_agreement handles ties and zero-weight models", {
  # A tie for best still counts as shared
  tied <- cbind(a = c(1, 1), b = c(0, 1))
  expect_true(check_action_agreement(tied)$agree)
  U <- cbind(a = c(0.9, 0.4), b = c(0.2, 0.9))
  expect_true(check_action_agreement(U, model_weights = c(1, 0))$agree)
  expect_error(check_action_agreement(U, model_weights = c(1, 0, 0)), "one value per model")
})

test_that("agreement matches EVCA zero versus positive under any scaling", {
  set.seed(42)
  for (i in 1:200) {
    # Random sizes, scales, offsets and weights
    n_dec <- sample(2:5, 1)
    n_mod <- sample(2:5, 1)
    scale <- rep(10^runif(n_mod, -2, 3), each = n_dec)
    offset <- rep(rnorm(n_mod, 0, 50), each = n_dec)
    U <- matrix(rnorm(n_dec * n_mod), n_dec, n_mod) * scale + offset
    w <- rgamma(n_mod, 1)
    w <- w / sum(w)
    evca <- compute_evca(U, model_probs = w)$evca
    # Agreement means EVCA is 0, disagreement means above 0
    if (check_action_agreement(U)$agree) {
      expect_equal(evca, 0, tolerance = 1e-8)
    } else {
      expect_gt(evca, 0)
    }
  }
})
