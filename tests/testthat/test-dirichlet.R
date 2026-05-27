# Tests for compute_evca_dirichlet(), plot_dirichlet_evca(), plot_dirichlet_actions()

# Shared test data: Luang Prabang road decision (normalized)
mat_raw <- matrix(
  c(
     5, 40, 80,
    90, 82,  8,
    72, 78, 55
  ),
  nrow = 3, ncol = 3, byrow = TRUE,
  dimnames = list(
    c("No road", "Direct route", "Rerouted road"),
    c("Officials", "ProDev_Hmong", "Traditional_Hmong")
  )
)
norm_col <- function(x) (x - min(x)) / (max(x) - min(x))
U_norm <- apply(mat_raw, 2, norm_col)

test_that("compute_evca_dirichlet returns correct structure", {
  result <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2), seed = 42)

  expect_type(result, "list")
  expect_named(result, c(
    "evca_draws", "optimal_actions", "alpha", "n_draws",
    "mean_evca", "sd_evca", "action_frequencies", "model_utilities"
  ))
})

test_that("compute_evca_dirichlet evca_draws has correct length", {
  result <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2),
                                    n_draws = 200, seed = 1)
  expect_equal(length(result$evca_draws), 200)
  expect_equal(result$n_draws, 200L)
})

test_that("compute_evca_dirichlet evca_draws are non-negative", {
  result <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2), seed = 42)
  expect_true(all(result$evca_draws >= 0))
})

test_that("compute_evca_dirichlet action_frequencies sum to 1", {
  result <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2), seed = 42)
  expect_equal(sum(result$action_frequencies), 1, tolerance = 1e-10)
})

test_that("compute_evca_dirichlet is reproducible with seed", {
  r1 <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2), seed = 99)
  r2 <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2), seed = 99)
  expect_equal(r1$evca_draws, r2$evca_draws)
  expect_equal(r1$optimal_actions, r2$optimal_actions)
})

test_that("compute_evca_dirichlet different seeds give different results", {
  r1 <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2), seed = 1)
  r2 <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2), seed = 2)
  expect_false(identical(r1$evca_draws, r2$evca_draws))
})

test_that("compute_evca_dirichlet equal prior favours Rerouted road", {
  result <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2),
                                    n_draws = 2000, seed = 42)
  freq <- result$action_frequencies
  # Rerouted road should be most common BMA-optimal action under equal prior
  expect_true("Rerouted road" %in% names(freq))
  expect_true(freq["Rerouted road"] == max(freq))
})

test_that("compute_evca_dirichlet dev-coalition prior shifts toward Direct route", {
  r_equal <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2),
                                     n_draws = 2000, seed = 42)
  r_biased <- compute_evca_dirichlet(U_norm, alpha = c(8, 8, 2),
                                      n_draws = 2000, seed = 42)

  freq_equal  <- r_equal$action_frequencies
  freq_biased <- r_biased$action_frequencies

  # Direct route frequency should be higher under biased prior
  direct_equal  <- if ("Direct route" %in% names(freq_equal))
    freq_equal["Direct route"] else 0
  direct_biased <- if ("Direct route" %in% names(freq_biased))
    freq_biased["Direct route"] else 0

  expect_gt(direct_biased, direct_equal)
})

test_that("compute_evca_dirichlet validates alpha length", {
  expect_error(
    compute_evca_dirichlet(U_norm, alpha = c(2, 2)),
    "must equal number of model columns"
  )
})

test_that("compute_evca_dirichlet validates alpha positivity", {
  expect_error(
    compute_evca_dirichlet(U_norm, alpha = c(2, 2, 0)),
    "strictly positive"
  )
  expect_error(
    compute_evca_dirichlet(U_norm, alpha = c(2, -1, 2)),
    "strictly positive"
  )
})

test_that("compute_evca_dirichlet validates n_draws", {
  expect_error(
    compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2), n_draws = 0),
    "positive integer"
  )
})

test_that("compute_evca_dirichlet works with data frame input", {
  df_input <- as.data.frame(U_norm)
  result   <- compute_evca_dirichlet(df_input, alpha = c(2, 2, 2),
                                      n_draws = 100, seed = 42)
  expect_type(result, "list")
  expect_equal(length(result$evca_draws), 100)
})

test_that("plot_dirichlet_evca returns ggplot for single result", {
  r <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2),
                               n_draws = 200, seed = 42)
  p <- plot_dirichlet_evca(r)
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "gg")
})

test_that("plot_dirichlet_evca returns ggplot for list of results", {
  r1 <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2), n_draws = 200, seed = 1)
  r2 <- compute_evca_dirichlet(U_norm, alpha = c(8, 8, 2), n_draws = 200, seed = 2)
  p  <- plot_dirichlet_evca(list("Equal" = r1, "Biased" = r2))
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "gg")
})

test_that("plot_dirichlet_actions returns ggplot for single result", {
  r <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2),
                               n_draws = 200, seed = 42)
  p <- plot_dirichlet_actions(r)
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "gg")
})

test_that("plot_dirichlet_actions returns ggplot for list of results", {
  r1 <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2), n_draws = 200, seed = 1)
  r2 <- compute_evca_dirichlet(U_norm, alpha = c(8, 8, 2), n_draws = 200, seed = 2)
  p  <- plot_dirichlet_actions(list("Equal" = r1, "Biased" = r2))
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "gg")
})

test_that("plot_dirichlet_actions respects decision_order", {
  r <- compute_evca_dirichlet(U_norm, alpha = c(2, 2, 2),
                               n_draws = 200, seed = 42)
  p <- plot_dirichlet_actions(
    r,
    decision_order = c("No road", "Direct route", "Rerouted road")
  )
  expect_s3_class(p, "ggplot")
})
