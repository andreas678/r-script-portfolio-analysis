## compute_marginal_risk_contribution() (scripts/portfolio_returns.R) — pure.

corr2 <- function(rho) matrix(c(1, rho, rho, 1), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))

test_that("two uncorrelated assets match the closed-form decomposition", {
  res <- compute_marginal_risk_contribution(c(A = 0.5, B = 0.5), c(A = 0.20, B = 0.10), corr2(0))

  expect_equal(res$portfolio_vol, sqrt(0.0125))
  tab <- res$table[match(c("A", "B"), res$table$ticker), ]
  expect_equal(tab$risk_contribution, c(0.5 * 0.02, 0.5 * 0.005) / sqrt(0.0125))
  expect_equal(tab$risk_contribution_pct, c(80, 20))
})

test_that("contributions sum to portfolio volatility and to 100 percent", {
  set.seed(1)
  k <- 6
  A <- matrix(rnorm(k * 50), 50, k); R <- cor(A); dimnames(R) <- list(letters[1:k], letters[1:k])
  w <- setNames(runif(k), letters[1:k]); s <- setNames(runif(k, 0.1, 0.5), letters[1:k])

  res <- compute_marginal_risk_contribution(w, s, R)

  expect_equal(sum(res$table$risk_contribution), res$portfolio_vol)
  expect_equal(sum(res$table$risk_contribution_pct), 100)
  expect_equal(sum(res$table$weight), 1)
})

test_that("perfectly correlated assets reduce to weight x standalone volatility", {
  res <- compute_marginal_risk_contribution(c(A = 0.6, B = 0.4), c(A = 0.20, B = 0.10), corr2(1))
  tab <- res$table[match(c("A", "B"), res$table$ticker), ]

  expect_equal(tab$risk_contribution, c(0.6 * 0.20, 0.4 * 0.10))
  expect_equal(res$portfolio_vol, 0.6 * 0.20 + 0.4 * 0.10)
})

test_that("a hedging holding gets a negative contribution", {
  res <- compute_marginal_risk_contribution(c(A = 0.9, B = 0.1), c(A = 0.20, B = 0.10), corr2(-0.5))

  b <- res$table[res$table$ticker == "B", ]
  expect_lt(b$risk_contribution, 0)
  expect_lt(b$risk_contribution_pct, 0)
  expect_equal(sum(res$table$risk_contribution_pct), 100)
})

test_that("weights are renormalized and a single holding carries all the risk", {
  res <- compute_marginal_risk_contribution(c(A = 7), c(A = 0.25),
                                            matrix(1, 1, 1, dimnames = list("A", "A")))

  expect_equal(res$table$weight, 1)
  expect_equal(res$table$risk_contribution_pct, 100)
  expect_equal(res$portfolio_vol, 0.25)
})

test_that("inputs are aligned by name, not position", {
  R <- corr2(0.3)[c("B", "A"), c("B", "A")]  # rows/cols reordered

  by_name <- compute_marginal_risk_contribution(c(A = 0.5, B = 0.5), c(B = 0.10, A = 0.20), R)
  by_pos  <- compute_marginal_risk_contribution(c(A = 0.5, B = 0.5), c(A = 0.20, B = 0.10), corr2(0.3))

  expect_equal(by_name$portfolio_vol, by_pos$portfolio_vol)
  expect_equal(by_name$table[order(by_name$table$ticker), ], by_pos$table[order(by_pos$table$ticker), ])
})

test_that("holdings without sigma or correlation are an error, never silently assumed", {
  expect_error(compute_marginal_risk_contribution(c(A = 1, C = 1), c(A = 0.2, C = 0.1), corr2(0)),
               "no sigma / correlation for: C")
  expect_error(compute_marginal_risk_contribution(c(A = 1, B = 1), c(A = 0.2), corr2(0)),
               "no sigma / correlation for: B")
  expect_error(compute_marginal_risk_contribution(c(1, 1), c(0.2, 0.1), corr2(0)), "named")
})
