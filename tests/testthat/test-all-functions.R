# Test file for all EVECA package functions

# Source functions directly for testing
source("../../R/bma.R")
source("../../R/evca.R")
source("../../R/example.R")
source("../../R/plot.R")

# Test bma_expected_utility function (internal)
test_that("bma_expected_utility computes correct weighted averages", {
  # Simple test case
  model_utilities <- matrix(
    c(10, 5, 6, 9),
    nrow = 2, ncol = 2
  )
  model_probs <- c(0.5, 0.5)

  result <- bma_expected_utility(model_utilities, model_probs)

  # Manual calculation:
  # Decision 1: 0.5*10 + 0.5*5 = 7.5
  # Decision 2: 0.5*6 + 0.5*9 = 7.5
  expect_equal(result, c(7.5, 7.5), tolerance = 1e-10)

  # Test with unequal probabilities
  model_probs2 <- c(0.7, 0.3)
  result2 <- bma_expected_utility(model_utilities, model_probs2)

  # Decision 1: 0.7*10 + 0.3*5 = 8.5
  # Decision 2: 0.7*6 + 0.3*9 = 6.9
  expect_equal(result2, c(8.5, 6.9), tolerance = 1e-10)
})

test_that("bma_expected_utility validates inputs", {
  model_utilities <- matrix(
    c(10, 5, 6, 9),
    nrow = 2, ncol = 2
  )

  # Test invalid probability sum
  expect_error(
    bma_expected_utility(model_utilities, c(0.5, 0.6)),
    "Model probabilities must sum to 1 (within 1e-10 tolerance)"
  )

  # Test negative probabilities
  expect_error(
    bma_expected_utility(model_utilities, c(0.5, -0.1)),
    "Model probabilities must be non-negative"
  )

  # Test dimension mismatch
  expect_error(
    bma_expected_utility(model_utilities, c(0.5, 0.5, 0.0)),
    "must equal number of model utility columns"
  )
})

# Test example_evca function
test_that("example_evca generates valid example data", {
  # Test with default parameters
  result <- example_evca()

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("evca_result", "model_utilities", "model_probs", "risk_aversion"))

  # Check model_utilities matrix
  expect_equal(dim(result$model_utilities), c(3, 4))
  expect_true(is.matrix(result$model_utilities))

  # Check model_probs
  expect_equal(length(result$model_probs), 4)
  expect_equal(sum(result$model_probs), 1, tolerance = 1e-10)
  expect_true(all(result$model_probs >= 0))

  # Check evca_result is valid
  expect_type(result$evca_result, "list")
  expect_true("evca" %in% names(result$evca_result))
})

test_that("example_evca handles custom parameters", {
  # Test with custom parameters
  result <- example_evca(
    n_decisions = 5,
    n_models = 3,
    risk_aversion = 0.05,
    seed = 999
  )

  # Check dimensions
  expect_equal(dim(result$model_utilities), c(5, 3))
  expect_equal(length(result$model_probs), 3)
  expect_equal(result$risk_aversion, 0.05)

  # Test reproducibility with seed
  result1 <- example_evca(seed = 123)
  result2 <- example_evca(seed = 123)
  expect_equal(result1$model_utilities, result2$model_utilities)
  expect_equal(result1$model_probs, result2$model_probs)

  # Different seeds should produce different results
  result3 <- example_evca(seed = 456)
  expect_false(identical(result1$model_utilities, result3$model_utilities))
})

test_that("example_evca handles risk neutrality", {
  # Test risk neutral (risk_aversion = 0)
  result <- example_evca(risk_aversion = 0)
  expect_equal(result$risk_aversion, 0)

  # The utility function should be identity
  # We can check this indirectly by comparing utilities
  evca_result <- result$evca_result

  # With risk neutrality, the utilities should be the raw values
  # This is an indirect test since we don't have direct access to the utility function
  expect_true(is.numeric(evca_result$evca))
})

# Test plot_evca function
test_that("plot_evca creates valid ggplot object", {
  # First create example data
  example_result <- example_evca()
  evca_result <- example_result$evca_result

  # Create plot
  p <- plot_evca(evca_result)

  # Check it's a ggplot object
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "gg")

  # Check plot data
  plot_data <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(nrow(plot_data), 2) # Two bars: BMA Optimal and Perfect Information
})

test_that("plot_evca validates input", {
  # Test with invalid input
  expect_error(
    plot_evca(list(not_evca = "invalid")),
    "evca_result must be output from compute_evca()"
  )

  # Test with missing evca field
  expect_error(
    plot_evca(list(optimal_utility_bma = 5, perfect_info_expected_utility = 6)),
    "evca_result must be output from compute_evca()"
  )
})

test_that("plot_evca accepts customization parameters", {
  example_result <- example_evca()
  evca_result <- example_result$evca_result

  # Test custom title
  p1 <- plot_evca(evca_result, title = "Custom Title")
  expect_s3_class(p1, "ggplot")

  # Test custom colors
  p2 <- plot_evca(evca_result, colors = c("red", "blue"))
  expect_s3_class(p2, "ggplot")

  # Test custom axis labels
  p3 <- plot_evca(evca_result, xlab = "Decision", ylab = "Value")
  expect_s3_class(p3, "ggplot")

  # All should be valid ggplot objects
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
})

# Test compute_evca function (more comprehensive tests)
test_that("compute_evca handles edge cases correctly", {
  # Single decision, single model
  model_utilities <- matrix(10, nrow = 1, ncol = 1)
  model_probs <- 1

  result <- compute_evca(model_utilities, model_probs)

  expect_equal(result$n_decisions, 1)
  expect_equal(result$n_models, 1)
  expect_equal(result$optimal_decision_bma, 1)
  expect_equal(result$evca, 0) # No ambiguity to eliminate

  # Multiple decisions, single model (no ambiguity)
  model_utilities <- matrix(c(10, 8, 12), nrow = 3, ncol = 1)
  model_probs <- 1

  result <- compute_evca(model_utilities, model_probs)

  expect_equal(result$n_decisions, 3)
  expect_equal(result$n_models, 1)
  expect_equal(result$optimal_decision_bma, 3) # Decision 3 has highest utility (12)
  expect_equal(result$evca, 0) # No ambiguity to eliminate
})

test_that("compute_evca handles utility functions correctly", {
  model_utilities <- matrix(
    c(10, 5, 6, 9),
    nrow = 2, ncol = 2
  )
  model_probs <- c(0.5, 0.5)

  # Define a simple utility function (square)
  square_utility <- function(x) x^2

  result <- compute_evca(model_utilities, model_probs, square_utility)

  # Check that utilities were transformed
  # Original utilities: [10, 5; 6, 9]
  # Squared: [100, 25; 36, 81]
  # BMA: Decision 1: 0.5*100 + 0.5*25 = 62.5
  #      Decision 2: 0.5*36 + 0.5*81 = 58.5
  expect_equal(result$bma_expected_utility, c(62.5, 58.5), tolerance = 1e-10)
  expect_equal(result$optimal_decision_bma, 1)
})

test_that("compute_evca returns correct structure and metadata", {
  model_utilities <- matrix(
    c(10, 8, 12, 7, 9, 6, 11, 10, 8),
    nrow = 3, ncol = 3
  )
  model_probs <- c(0.4, 0.35, 0.25)

  result <- compute_evca(model_utilities, model_probs)

  # Check all expected components are present
  expected_components <- c(
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
  )

  expect_named(result, expected_components)

  # Check metadata
  expect_equal(result$n_decisions, 3)
  expect_equal(result$n_models, 3)
  expect_equal(result$model_probs, model_probs)

  # Check that optimal_decision_bma is within valid range
  expect_true(result$optimal_decision_bma >= 1)
  expect_true(result$optimal_decision_bma <= 3)

  # Check that evca is non-negative (by definition)
  expect_true(result$evca >= 0)

  # Check that perfect_info_expected_utility >= optimal_utility_bma
  expect_true(result$perfect_info_expected_utility >= result$optimal_utility_bma)
})

# Test integration between functions
test_that("functions work together correctly", {
  # Generate example data
  example_result <- example_evca(seed = 777)

  # compute_evca should work with output from example_evca
  result1 <- compute_evca(
    example_result$model_utilities,
    example_result$model_probs
  )

  # plot_evca should work with output from compute_evca
  p <- plot_evca(result1)
  expect_s3_class(p, "ggplot")

  # example_evca should return a valid evca_result for plotting
  p2 <- plot_evca(example_result$evca_result)
  expect_s3_class(p2, "ggplot")
})

# Test error handling and edge cases
test_that("functions handle invalid inputs gracefully", {
  # Test compute_evca with invalid model_utilities
  expect_error(
    compute_evca("not a matrix", c(0.5, 0.5)),
    "must equal number of model utility columns" # Error from dimension check
  )

  # Test compute_evca with empty matrix
  expect_error(
    compute_evca(matrix(nrow = 0, ncol = 0), numeric(0)),
    "must equal number of model utility columns" # Error from dimension check
  )

  # Test example_evca with invalid parameters
  expect_error(
    example_evca(n_decisions = 0),
    "invalid arguments" # Error from rnorm with n=0
  )

  expect_error(
    example_evca(n_models = 0),
    "invalid arguments" # Error from rnorm with n=0
  )
})
