test_that("compute_evca distinguishes evaluative weights from epistemic probabilities", {
  utilities <- matrix(
    c(1, 0, 0, 1),
    nrow = 2,
    dimnames = list(c("a1", "a2"), c("account_a", "account_b"))
  )

  evaluative <- compute_evca(utilities, model_weights = c(0.5, 0.5))
  expect_identical(evaluative$weight_interpretation, "evaluative")
  expect_null(evaluative$model_probs)
  expect_equal(evaluative$model_weights, c(0.5, 0.5))
  expect_equal(sum(evaluative$model_contributions), evaluative$evca)
  expect_named(evaluative$model_contributions, c("account_a", "account_b"))

  epistemic <- compute_evca(utilities, model_probs = c(0.5, 0.5))
  expect_identical(epistemic$weight_interpretation, "epistemic")
  expect_equal(epistemic$model_probs, c(0.5, 0.5))
  expect_equal(epistemic$evca, 0.5)

  expect_error(
    compute_evca(utilities, model_probs = c(0.5, 0.5), model_weights = c(0.5, 0.5)),
    "Supply either"
  )
})

test_that("exclusion costs report the selected weight interpretation", {
  utilities <- matrix(
    c(1, 0, 0, 1),
    nrow = 2,
    dimnames = list(c("a1", "a2"), c("account_a", "account_b"))
  )

  equal_standing <- compute_exclusion_costs(utilities)
  expect_identical(attr(equal_standing, "weight_interpretation"), "evaluative")

  epistemic <- compute_exclusion_costs(utilities, model_probs = c(0.5, 0.5))
  expect_identical(attr(epistemic, "weight_interpretation"), "epistemic")
})

test_that("Dirichlet output labels the interpretation of sampled weights", {
  utilities <- matrix(
    c(1, 0, 0, 1),
    nrow = 2,
    dimnames = list(c("a1", "a2"), c("account_a", "account_b"))
  )

  evaluative <- compute_evca_dirichlet(utilities, alpha = c(1, 1), n_draws = 10, seed = 1)
  expect_identical(evaluative$weight_interpretation, "evaluative")

  epistemic <- compute_evca_dirichlet(
    utilities,
    alpha = c(1, 1),
    n_draws = 10,
    seed = 1,
    weight_interpretation = "epistemic"
  )
  expect_identical(epistemic$weight_interpretation, "epistemic")
})
