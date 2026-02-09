# Test file for compute_evca function

test_that("compute_evca returns correct structure", {
  # Create test data
  model_utilities <- matrix(
    c(10, 8, 12, 7, 9, 6, 11, 10, 8),
    nrow = 3, ncol = 3
  )
  model_probs <- c(0.4, 0.35, 0.25)

  result <- compute_evca(model_utilities, model_probs)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c(
    "bma_expected_utility",
    "optimal_decision_bma",
    "optimal_utility_bma",
    "optimal_decisions_per_model",
    "optimal_utilities_per_model",
    "perfect_info_expected_utility",
    "evca",
    "model_probs",
    "n_decisions",
    "n_models"
  ))

  # Check dimensions
  expect_equal(result$n_decisions, 3)
  expect_equal(result$n_models, 3)
  expect_equal(length(result$bma_expected_utility), 3)
  expect_equal(length(result$optimal_decisions_per_model), 3)
  expect_equal(length(result$optimal_utilities_per_model), 3)
})

test_that("compute_evca handles data frames", {
  # Test with data frame input
  utility_df <- data.frame(
    Model1 = c(10, 7, 11),
    Model2 = c(8, 9, 10),
    Model3 = c(12, 6, 8)
  )
  model_probs <- c(0.4, 0.35, 0.25)

  result <- compute_evca(utility_df, model_probs)

  expect_type(result, "list")
  expect_equal(result$n_decisions, 3)
  expect_equal(result$n_models, 3)
})

test_that("compute_evca validates inputs", {
  model_utilities <- matrix(
    c(10, 8, 12, 7, 9, 6, 11, 10, 8),
    nrow = 3, ncol = 3
  )

  # Test invalid probability sum
  expect_error(
    compute_evca(model_utilities, c(0.5, 0.5, 0.5)),
    "Model probabilities must sum to 1"
  )

  # Test negative probabilities
  expect_error(
    compute_evca(model_utilities, c(0.5, 0.5, -0.1)),
    "Model probabilities must be non-negative"
  )

  # Test dimension mismatch
  expect_error(
    compute_evca(model_utilities, c(0.5, 0.5)),
    "must equal number of model utility columns"
  )
})

test_that("compute_evca with utility function", {
  model_utilities <- matrix(
    c(10, 8, 12, 7, 9, 6, 11, 10, 8),
    nrow = 3, ncol = 3
  )
  model_probs <- c(0.4, 0.35, 0.25)

  # Define simple utility function (square root)
  sqrt_utility <- function(x) sqrt(x)

  result <- compute_evca(model_utilities, model_probs, sqrt_utility)

  expect_type(result, "list")
  expect_equal(result$n_decisions, 3)
  expect_equal(result$n_models, 3)

  # Check that utilities were transformed (values should be smaller after sqrt)
  expect_true(all(result$bma_expected_utility <
    compute_evca(model_utilities, model_probs)$bma_expected_utility))
})

test_that("compute_evca returns correct EVECA calculation", {
  # Simple test case where we can manually verify
  model_utilities <- matrix(
    c(
      10, 5, # Decision 1: 10 under Model 1, 5 under Model 2
      6, 9
    ), # Decision 2: 6 under Model 1, 9 under Model 2
    nrow = 2, ncol = 2, byrow = TRUE
  )
  model_probs <- c(0.5, 0.5)

  result <- compute_evca(model_utilities, model_probs)

  # Manual calculation:
  # BMA expected utilities:
  # Decision 1: 0.5*10 + 0.5*5 = 7.5
  # Decision 2: 0.5*6 + 0.5*9 = 7.5
  # Optimal decision under BMA: either (both 7.5)
  # Perfect information:
  # If Model 1 true: choose Decision 1 (10)
  # If Model 2 true: choose Decision 2 (9)
  # Perfect info EU: 0.5*10 + 0.5*9 = 9.5
  # EVECA: 9.5 - 7.5 = 2.0

  expect_equal(result$bma_expected_utility, c(7.5, 7.5))
  expect_equal(result$perfect_info_expected_utility, 9.5)
  expect_equal(result$evca, 2.0)
})

test_that("compute_evca handles edge cases", {
  # Single decision, single model
  model_utilities <- matrix(10, nrow = 1, ncol = 1)
  model_probs <- 1

  result <- compute_evca(model_utilities, model_probs)

  expect_equal(result$n_decisions, 1)
  expect_equal(result$n_models, 1)
  expect_equal(result$optimal_decision_bma, 1)
  expect_equal(result$evca, 0) # No ambiguity to eliminate

  # Multiple decisions, single model
  model_utilities <- matrix(c(10, 8, 12), nrow = 3, ncol = 1)
  model_probs <- 1

  result <- compute_evca(model_utilities, model_probs)

  expect_equal(result$n_decisions, 3)
  expect_equal(result$n_models, 1)
  expect_equal(result$optimal_decision_bma, 3) # Decision 3 has highest utility (12)
  expect_equal(result$evca, 0) # No ambiguity to eliminate
})
