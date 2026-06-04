# Tests for normalize_utilities()

# Shared test matrix: Luang Prabang road decision (raw, incommensurable units)
mat <- matrix(
  c(
    5, 40, 80, # No road
    90, 82, 8, # Direct route
    72, 78, 55 # Rerouted road
  ),
  nrow = 3, ncol = 3, byrow = TRUE,
  dimnames = list(
    c("No road", "Direct route", "Rerouted road"),
    c("Officials", "ProDev_Hmong", "Traditional_Hmong")
  )
)

test_that("normalize_utilities returns values in [0, 1]", {
  result <- normalize_utilities(mat)
  expect_true(all(result >= 0))
  expect_true(all(result <= 1))
})

test_that("normalize_utilities gives 0 to worst and 1 to best per column", {
  result <- normalize_utilities(mat)
  # Each column should have min = 0 and max = 1
  col_mins <- apply(result, 2, min)
  col_maxs <- apply(result, 2, max)
  expect_equal(col_mins, c(Officials = 0, ProDev_Hmong = 0, Traditional_Hmong = 0),
    tolerance = 1e-10
  )
  expect_equal(col_maxs, c(Officials = 1, ProDev_Hmong = 1, Traditional_Hmong = 1),
    tolerance = 1e-10
  )
})

test_that("normalize_utilities preserves dimensions and dimnames", {
  result <- normalize_utilities(mat)
  expect_equal(dim(result), dim(mat))
  expect_equal(rownames(result), rownames(mat))
  expect_equal(colnames(result), colnames(mat))
})

test_that("normalize_utilities returns a matrix", {
  result <- normalize_utilities(mat)
  expect_true(is.matrix(result))
})

test_that("normalize_utilities accepts data frame input", {
  df <- as.data.frame(mat)
  result <- normalize_utilities(df)
  expect_true(is.matrix(result))
  expect_true(all(result >= 0 & result <= 1))
})

test_that("normalize_utilities handles negative values correctly", {
  neg_mat <- matrix(c(-10, 0, 10, -5, 5, 15), nrow = 3, ncol = 2)
  result <- normalize_utilities(neg_mat)
  expect_true(all(result >= 0 & result <= 1))
  expect_equal(apply(result, 2, min), c(0, 0), tolerance = 1e-10)
  expect_equal(apply(result, 2, max), c(1, 1), tolerance = 1e-10)
})

test_that("normalize_utilities warns when matrix appears already normalized", {
  already_norm <- normalize_utilities(mat, warn_if_normalized = FALSE)
  expect_warning(
    normalize_utilities(already_norm),
    "already be normalized"
  )
})

test_that("normalize_utilities suppresses already-normalized warning when asked", {
  already_norm <- normalize_utilities(mat, warn_if_normalized = FALSE)
  expect_no_warning(
    normalize_utilities(already_norm, warn_if_normalized = FALSE)
  )
})

test_that("normalize_utilities warns on zero-range column", {
  flat_col <- matrix(c(5, 5, 5, 10, 20, 30), nrow = 3, ncol = 2)
  expect_warning(
    normalize_utilities(flat_col),
    "zero range"
  )
})

test_that("normalize_utilities errors on single-row matrix", {
  single_row <- matrix(c(10, 20, 30), nrow = 1)
  expect_error(
    normalize_utilities(single_row),
    "at least 2 rows"
  )
})

test_that("normalize_utilities errors on NA values", {
  na_mat <- mat
  na_mat[1, 1] <- NA
  expect_error(
    normalize_utilities(na_mat),
    "NA values"
  )
})

test_that("normalize_utilities result integrates with compute_evca()", {
  U_norm <- normalize_utilities(mat)
  result <- compute_evca(U_norm, model_probs = c(1 / 3, 1 / 3, 1 / 3))
  expect_type(result, "list")
  expect_true(result$evca >= 0)
  # Rerouted road should be optimal under equal weights
  expect_equal(rownames(mat)[result$optimal_decision], "Rerouted road")
})

test_that("normalize_utilities result integrates with compute_exclusion_costs()", {
  U_norm <- normalize_utilities(mat)
  # With normalize = FALSE since we already normalized
  excl <- compute_exclusion_costs(U_norm, normalize = FALSE)
  expect_s3_class(excl, "data.frame")
  expect_equal(nrow(excl), 3)
  # welfare_own_best should still be 1 (we normalized manually)
  expect_equal(excl$welfare_own_best, rep(1, 3), tolerance = 1e-10)
})
