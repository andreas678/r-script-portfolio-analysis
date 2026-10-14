## shrink_correlation_lw() / current_tickers() (scripts/portfolio_returns.R) — pure.

sim_returns <- function(n, p, rho = 0, seed = 1) {
  set.seed(seed)
  common <- rnorm(n)
  X <- sapply(seq_len(p), function(i) sqrt(rho) * common + sqrt(1 - rho) * rnorm(n))
  colnames(X) <- paste0("A", seq_len(p))
  X
}

test_that("the shrunk matrix is a valid correlation matrix even with more assets than observations", {
  res <- shrink_correlation_lw(sim_returns(n = 30, p = 60, rho = 0.3))

  expect_equal(unname(diag(res$correlation)), rep(1, 60))
  expect_true(isSymmetric(res$correlation))
  expect_gt(min(eigen(res$correlation, symmetric = TRUE, only.values = TRUE)$values), 0)  # invertible
  expect_gt(res$shrinkage, 0)
  expect_lte(res$shrinkage, 1)
  # the raw sample correlation is singular here (rank <= n - 1 < p)
  expect_lt(min(eigen(cor(sim_returns(30, 60, 0.3)), only.values = TRUE)$values), 1e-8)
})

test_that("off-diagonal correlations are the sample correlations scaled by (1 - delta)", {
  X <- sim_returns(n = 100, p = 5, rho = 0.4)

  res <- shrink_correlation_lw(X)
  off <- row(res$correlation) != col(res$correlation)

  expect_equal(res$correlation[off], (1 - res$shrinkage) * cor(X)[off])
})

test_that("shrinkage falls as observations grow and rises when there are few", {
  few  <- shrink_correlation_lw(sim_returns(n = 40,   p = 20, rho = 0.3))$shrinkage
  many <- shrink_correlation_lw(sim_returns(n = 4000, p = 20, rho = 0.3))$shrinkage

  expect_gt(few, many)
  expect_lt(many, 0.05)
})

test_that("shrinkage is a scale-free property of the data: rescaling assets doesn't change it", {
  X <- sim_returns(n = 80, p = 10, rho = 0.2)

  expect_equal(shrink_correlation_lw(X)$shrinkage,
               shrink_correlation_lw(sweep(X, 2, seq(0.5, 5, length.out = 10), "*"))$shrinkage)
})

test_that("bad input is rejected", {
  expect_error(shrink_correlation_lw(cbind(a = c(1, NA, 2), b = c(1, 2, 3))), "NA")
  expect_error(shrink_correlation_lw(cbind(a = c(1, 1, 1), b = c(1, 2, 3))), "zero variance")
  expect_error(shrink_correlation_lw(matrix(1, 1, 2)), "at least 2")
})

test_that("current_tickers drops series that stopped updating, keeps ones within the tolerance", {
  prices <- tibble::tibble(
    ticker = c("LIVE", "LIVE", "FRI", "FRI", "STALE", "STALE"),
    Date = as.Date(c("2026-09-01", "2026-09-29", "2026-09-01", "2026-09-25",
                     "2026-06-01", "2026-06-26")),
    Adjusted_close = 1
  )

  expect_setequal(current_tickers(prices), c("LIVE", "FRI"))
  expect_setequal(current_tickers(prices, max_staleness_days = 2), "LIVE")
})

test_that("build_return_panel drops stale series so they do not cut the shared history", {
  set.seed(3)
  d <- seq(as.Date("2026-01-01"), by = "day", length.out = 200)
  mk <- function(tk, n) tibble::tibble(ticker = tk, Date = d[seq_len(n)],
                                       Adjusted_close = 100 * cumprod(1 + rnorm(n, 0, 0.01)))
  prices <- dplyr::bind_rows(mk("LIVE1", 200), mk("LIVE2", 200), mk("DEAD", 120))

  panel <- build_return_panel(prices, min_observations = 100)

  expect_setequal(setdiff(names(panel), "Date"), c("LIVE1", "LIVE2"))
  expect_equal(attr(panel, "stale_tickers"), "DEAD")
  # without the filter the intersection would end at DEAD's last date
  expect_equal(nrow(panel), 199)
  expect_equal(nrow(compute_daily_log_returns(prices_to_wide(prices, min_observations = 100))), 119)
})

test_that("build_return_panel honours the ticker restriction and min_observations", {
  set.seed(4)
  d <- seq(as.Date("2026-01-01"), by = "day", length.out = 150)
  mk <- function(tk, n) tibble::tibble(ticker = tk, Date = utils::tail(d, n),
                                       Adjusted_close = 100 * cumprod(1 + rnorm(n, 0, 0.01)))
  prices <- dplyr::bind_rows(mk("A", 150), mk("B", 150), mk("SHORT", 50), mk("C", 150))

  panel <- build_return_panel(prices, tickers = c("A", "B", "SHORT"), min_observations = 100)

  expect_setequal(setdiff(names(panel), "Date"), c("A", "B"))
  expect_length(attr(panel, "stale_tickers"), 0)
})

test_that("build_return_panel and current_tickers cope with no prices", {
  empty <- tibble::tibble(ticker = character(), Date = as.Date(character()), Adjusted_close = numeric())

  expect_equal(current_tickers(empty), character())
  expect_equal(nrow(build_return_panel(empty)), 0)
})
