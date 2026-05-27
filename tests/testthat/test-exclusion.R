# Tests for compute_exclusion_costs() and plot_exclusion_costs()

# Shared test matrix: Luang Prabang road decision
mat <- matrix(
  c(
     5, 40, 80,   # No road
    90, 82,  8,   # Direct route
    72, 78, 55    # Rerouted road
  ),
  nrow = 3, ncol = 3, byrow = TRUE,
  dimnames = list(
    c("No road", "Direct route", "Rerouted road"),
    c("Officials", "ProDev_Hmong", "Traditional_Hmong")
  )
)

test_that("compute_exclusion_costs returns correct structure", {
  result <- compute_exclusion_costs(mat)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)  # one row per model
  expect_named(result, c(
    "excluded_model", "decision_without",
    "welfare_excluded", "welfare_own_best", "exclusion_cost"
  ))
})

test_that("compute_exclusion_costs exclusion_cost is non-negative", {
  result <- compute_exclusion_costs(mat)
  expect_true(all(result$exclusion_cost >= 0))
})

test_that("compute_exclusion_costs welfare values are in [0,1] when normalized", {
  result <- compute_exclusion_costs(mat, normalize = TRUE)
  expect_true(all(result$welfare_excluded >= 0))
  expect_true(all(result$welfare_excluded <= 1))
  expect_true(all(result$welfare_own_best >= 0))
  expect_true(all(result$welfare_own_best <= 1))
})

test_that("compute_exclusion_costs welfare_own_best equals 1 when normalized", {
  # Min-max normalization maps the best outcome per model to exactly 1
  result <- compute_exclusion_costs(mat, normalize = TRUE)
  expect_equal(result$welfare_own_best, rep(1, 3), tolerance = 1e-10)
})

test_that("compute_exclusion_costs Traditional_Hmong exclusion is catastrophic", {
  result <- compute_exclusion_costs(mat, normalize = TRUE)
  trad_row <- result[result$excluded_model == "Traditional_Hmong", ]
  expect_equal(nrow(trad_row), 1)
  # Without Traditional_Hmong, Direct route is chosen — catastrophic for them
  expect_gt(trad_row$exclusion_cost, 0.8)
})

test_that("compute_exclusion_costs works without normalization", {
  result <- compute_exclusion_costs(mat, normalize = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_named(result, c(
    "excluded_model", "decision_without",
    "welfare_excluded", "welfare_own_best", "exclusion_cost"
  ))
  # Raw values should not be constrained to [0,1]
  expect_true(any(result$welfare_own_best > 1))
})

test_that("compute_exclusion_costs validates minimum 2 models", {
  single_col <- matrix(c(10, 7, 11), nrow = 3, ncol = 1)
  expect_error(
    compute_exclusion_costs(single_col),
    "at least 2 models"
  )
})

test_that("compute_exclusion_costs validates model_probs sum", {
  expect_error(
    compute_exclusion_costs(mat, model_probs = c(0.5, 0.5, 0.5)),
    "sum to 1"
  )
})

test_that("compute_exclusion_costs validates model_probs non-negative", {
  expect_error(
    compute_exclusion_costs(mat, model_probs = c(0.5, 0.6, -0.1)),
    "non-negative"
  )
})

test_that("compute_exclusion_costs works with model_probs", {
  result <- compute_exclusion_costs(mat, model_probs = c(0.4, 0.4, 0.2))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_named(result, c(
    "excluded_model", "decision_without",
    "welfare_excluded", "welfare_own_best", "exclusion_cost"
  ))
  expect_true(all(result$exclusion_cost >= 0))
})

test_that("compute_exclusion_costs handles unnamed matrix", {
  unnamed_mat <- matrix(
    c(5, 40, 80, 90, 82, 8, 72, 78, 55),
    nrow = 3, ncol = 3, byrow = TRUE
  )
  result <- compute_exclusion_costs(unnamed_mat)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  # Default names should be assigned
  expect_true(all(grepl("^Model", result$excluded_model)))
  expect_true(all(grepl("^Decision", result$decision_without)))
})

test_that("plot_exclusion_costs returns a ggplot object", {
  excl <- compute_exclusion_costs(mat)
  p    <- plot_exclusion_costs(excl)
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "gg")
})

test_that("plot_exclusion_costs validates input columns", {
  bad_df <- data.frame(x = 1:3, y = 1:3)
  expect_error(
    plot_exclusion_costs(bad_df),
    "missing required columns"
  )
})

test_that("plot_exclusion_costs accepts custom title and colors", {
  excl <- compute_exclusion_costs(mat)
  p    <- plot_exclusion_costs(excl,
                               title  = "Custom Title",
                               colors = c("red", "blue"))
  expect_s3_class(p, "ggplot")
})
