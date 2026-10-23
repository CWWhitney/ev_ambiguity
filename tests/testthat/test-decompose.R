test_that("evca_decompose satisfies the identity and matches compute_evca on means", {
  set.seed(42)
  mu <- cbind(c(5, 90, 72), c(40, 82, 78), c(80, 8, 55))
  draws <- array(NA_real_, c(2000, 3, 3))
  for (i in 1:2000) draws[i, , ] <- mu + rnorm(9, sd = 6)
  w <- c(0.5, 0.3, 0.2)
  r <- evca_decompose(draws, w)
  expect_equal(r$gap, r$evpi_agg + r$evca_post, tolerance = 1e-12)
  expect_gte(r$evpi_agg, 0)
  expect_gte(r$evca_post, 0)
  mean_u <- apply(draws, c(2, 3), mean)
  ref <- compute_evca(mean_u, model_weights = w)
  expect_equal(r$evca_means, ref$evca, tolerance = 1e-10)
})

test_that("with no state uncertainty the evidence-reducible term is zero", {
  mu <- cbind(c(0, 1, 0.6), c(1, 0, 0.6))
  draws <- array(rep(mu, each = 5), c(5, 3, 2))
  draws <- aperm(array(rep(as.vector(mu), each = 5), c(5, 3, 2)), c(1, 2, 3))
  r <- evca_decompose(draws, c(0.5, 0.5))
  expect_equal(r$evpi_agg, 0, tolerance = 1e-12)
  expect_equal(r$evca_post, r$evca_means, tolerance = 1e-12)
})

test_that("evca_decompose validates inputs", {
  d <- array(1, c(3, 2, 2))
  expect_error(evca_decompose(d, c(0.6, 0.6)))
  expect_error(evca_decompose(d, c(0.5, 0.5), scale_min = c(0, 0)))
})
