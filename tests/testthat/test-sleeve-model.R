## build_sleeves() / compute_sleeve_model() / max_drawdown_pct() (scripts/utils/sleeve_model.R) - pure.

source(here::here("scripts", "utils", "sleeve_model.R"))

# ---- fixtures ---------------------------------------------------------------------------------
sleeve_prices <- function(n = 260, seed = 3) {
  set.seed(seed)
  days <- seq(as.Date("2026-01-05"), by = "day", length.out = n)
  days <- days[!format(days, "%u") %in% c("6", "7")]
  mk <- function(tk, mu, sd) tibble::tibble(ticker = tk, Date = days,
                                            Adjusted_close = 100 * exp(cumsum(rnorm(length(days), mu, sd))))
  dplyr::bind_rows(mk("ETF1", 0.0004, 0.010), mk("GLD", 0.0002, 0.012), mk("BTC", 0.0005, 0.030))
}

sleeve_assumptions <- function() {
  tibble::tibble(ticker = c("CASH_X", "BOND_X"), annual_return_pct = c(2, 3), volatility_pct = c(0, 3), note = "")
}

sleeve_fixture <- function() {
  portfolio <- tibble::tibble(
    index = c(rep("EtfOne", 3), "Direct", "Direct", "Cash", "Bonds", "Gone"),
    ticker = c("A", "B", "C", "GLD", "BTC", "CASH_X", "BOND_X", "Z"),
    name = c("a", "b", "c", "Gold", "Bitcoin", "Cash", "Bonds", "z"),
    weight = c(20, 20, 10, 20, 5, 5, 10, 10)
  )
  fund_data <- tibble::tibble(
    file = c("EtfOne.xlsx", "Direct.xlsx", "Cash.xlsx", "Bonds.xlsx", "NoSeries.xlsx"),
    weight = c(0.5, NA, NA, NA, 0.1),
    label = c("ETF One", "Direct", "Cash", "Low Risk Bonds", "No Series Fund"),
    raw = list(tibble::tibble(index = "EtfOne"), tibble::tibble(index = "Direct"), tibble::tibble(index = "Cash"),
               tibble::tibble(index = "Bonds"), tibble::tibble(index = "Gone"))
  )
  listings <- tibble::tibble(fund_file = "EtfOne.xlsx", ticker = "ETF1")
  list(portfolio = portfolio, fund_data = fund_data, listings = listings)
}

# ---- build_sleeves ----------------------------------------------------------------------------
test_that("build_sleeves makes one sleeve per listed fund, one per direct ticker, and flags synthetic ones", {
  f <- sleeve_fixture()

  s <- build_sleeves(f$portfolio, f$fund_data, f$listings, synthetic = c("CASH_X", "BOND_X"))

  expect_equal(s$sleeve, c("ETF1", "GLD", "BTC", "CASH_X", "BOND_X", "NoSeries.xlsx"))
  expect_equal(s$kind, c("fund", "direct", "direct", "synthetic", "synthetic", "fund"))
  expect_equal(s$weight, c(50, 20, 5, 5, 10, 10))                    # the fund's rows summed, not its 3 holdings
  expect_equal(s$series, c("ETF1", "GLD", "BTC", NA, NA, NA))
  expect_equal(s$label[2], "GLD - Gold")
})

test_that("a fund that a scenario removed entirely gets no sleeve", {
  f <- sleeve_fixture()
  p <- dplyr::filter(f$portfolio, index != "EtfOne")

  s <- build_sleeves(p, f$fund_data, f$listings, synthetic = c("CASH_X", "BOND_X"))

  expect_false("ETF1" %in% s$sleeve)
})

# ---- compute_sleeve_model ---------------------------------------------------------------------
model_fixture <- function(window = NULL) {
  f <- sleeve_fixture()
  s <- build_sleeves(f$portfolio, f$fund_data, f$listings, synthetic = c("CASH_X", "BOND_X"))
  px <- sleeve_prices()
  dates <- sort(unique(px$Date))
  list(sleeves = s, prices = px, dates = if (is.null(window)) dates else window(dates),
       model = NULL)
}

test_that("coverage counts only sleeves that have a series or an assumption", {
  m <- model_fixture()

  res <- compute_sleeve_model(m$sleeves, m$prices, m$dates, sleeve_assumptions())

  expect_equal(res$coverage_pct, 90 / 100 * 100)                      # 'NoSeries.xlsx' (10 of 100) is not modelled
  expect_equal(res$unmodelled$sleeve, "NoSeries.xlsx")
  expect_equal(sum(res$sleeves$weight), 100)                          # renormalized over the modelled sleeves
})

test_that("risk shares sum to 100 and cash carries none", {
  m <- model_fixture()

  res <- compute_sleeve_model(m$sleeves, m$prices, m$dates, sleeve_assumptions())

  expect_equal(sum(res$sleeves$risk_share_pct), 100)
  expect_equal(res$sleeves$risk_share_pct[res$sleeves$sleeve == "CASH_X"], 0)
  expect_gt(res$sleeves$risk_share_pct[res$sleeves$sleeve == "BTC"], res$sleeves$weight[res$sleeves$sleeve == "BTC"])
})

test_that("portfolio volatility is sqrt(w' S w) of the sleeves' own covariance plus the assumed vol of synthetic ones", {
  m <- model_fixture()
  res <- compute_sleeve_model(m$sleeves, m$prices, m$dates, sleeve_assumptions())

  # independent re-derivation
  panel <- build_return_panel(m$prices, tickers = c("ETF1", "GLD", "BTC"))
  steps <- aggregate_returns_to_dates(panel, m$dates)[-1, ]
  k <- periods_per_year(m$dates)
  S <- matrix(0, 5, 5, dimnames = list(c("ETF1", "GLD", "BTC", "CASH_X", "BOND_X"), c("ETF1", "GLD", "BTC", "CASH_X", "BOND_X")))
  S[1:3, 1:3] <- cov(as.matrix(steps[, c("ETF1", "GLD", "BTC")])) * k
  S[5, 5] <- 0.03^2
  w <- c(ETF1 = 50, GLD = 20, BTC = 5, CASH_X = 5, BOND_X = 10); w <- w / sum(w)

  expect_equal(res$volatility_pct, sqrt(as.numeric(t(w) %*% S %*% w)) * 100)
})

test_that("cash and bonds accrue their assumed return over the elapsed time", {
  m <- model_fixture()

  res <- compute_sleeve_model(m$sleeves, m$prices, m$dates, sleeve_assumptions())

  years <- as.numeric(max(m$dates) - min(m$dates)) / 365.25
  expect_equal(sum(res$sleeve_steps$CASH_X), log(1.02) * years, tolerance = 1e-9)
  expect_equal(sum(res$sleeve_steps$BOND_X), log(1.03) * years, tolerance = 1e-9)
  expect_equal(res$sleeve_steps$CASH_X[1], 0)                          # baseline row
})

test_that("a coarser window gives the price-to-price return of each sleeve between its first and last date", {
  m <- model_fixture(window = function(d) d[seq(1, length(d), by = 3)])

  res <- compute_sleeve_model(m$sleeves, m$prices, m$dates, sleeve_assumptions())

  gld <- dplyr::filter(m$prices, ticker == "GLD", Date %in% m$dates) |> dplyr::arrange(Date)
  expect_equal(sum(res$sleeve_steps$GLD), log(gld$Adjusted_close[nrow(gld)] / gld$Adjusted_close[1]), tolerance = 1e-9)
  expect_equal(nrow(res$sleeve_steps), length(m$dates))                # one row per window date
})

test_that("the portfolio return series is the daily-rebalanced mix of the sleeves", {
  m <- model_fixture()

  res <- compute_sleeve_model(m$sleeves, m$prices, m$dates, sleeve_assumptions())

  w <- res$sleeves$weight / 100
  manual <- log(as.numeric(exp(as.matrix(res$sleeve_steps[, res$sleeves$sleeve])) %*% w))
  expect_equal(res$portfolio$portfolio_return, manual)
  expect_equal(res$portfolio$portfolio_return[1], 0)
  expect_equal(res$cumulative_return_pct, (exp(sum(manual)) - 1) * 100)
})

test_that("too few steps in the window is an error that says so", {
  m <- model_fixture()

  expect_error(compute_sleeve_model(m$sleeves, m$prices, m$dates[1:10], sleeve_assumptions()), "at least 20")
})

# ---- max_drawdown_pct -------------------------------------------------------------------------
test_that("max_drawdown_pct is the largest peak-to-trough fall of the compounded path", {
  r <- log(c(1.10, 0.90, 0.95, 1.20))       # 1.10 -> 0.99 -> 0.9405 -> 1.1286

  expect_equal(max_drawdown_pct(r), (0.9405 / 1.10 - 1) * 100, tolerance = 1e-9)
  expect_equal(max_drawdown_pct(log(c(1.01, 1.02))), 0)
})

# ---- attribute_cumulative_return ----------------------------------------------------------------
attr_returns <- function() {
  set.seed(11)
  m <- cbind(A = rnorm(120, 0.0006, 0.012), B = rnorm(120, 0.0002, 0.006), C = rnorm(120, 0.001, 0.03))
  list(m = m, w = c(A = 0.5, B = 0.3, C = 0.2))
}

test_that("the contributions add up to the cumulative return of the daily-rebalanced mix, exactly", {
  a <- attr_returns()
  contribution <- attribute_cumulative_return(a$m, a$w)
  portfolio <- as.numeric(log(exp(a$m) %*% a$w))

  expect_equal(sum(contribution), (exp(sum(portfolio)) - 1) * 100, tolerance = 1e-9)
  expect_named(contribution, c("A", "B", "C"))
})

test_that("a single sleeve gets the whole return, equal returns split by weight", {
  one <- matrix(c(0.01, -0.02, 0.03), ncol = 1, dimnames = list(NULL, "A"))
  expect_equal(unname(attribute_cumulative_return(one, c(A = 1))), (exp(sum(one)) - 1) * 100, tolerance = 1e-9)

  same <- cbind(A = c(0.01, 0.02, -0.005), B = c(0.01, 0.02, -0.005))
  out <- attribute_cumulative_return(same, c(A = 0.75, B = 0.25))
  expect_equal(unname(out["A"] / out["B"]), 3, tolerance = 1e-9)
})

test_that("a sleeve that loses has a negative contribution and weights can come in any order", {
  m <- cbind(WIN = rep(0.002, 60), LOSE = rep(-0.001, 60))
  out <- attribute_cumulative_return(m, c(LOSE = 0.4, WIN = 0.6))

  expect_gt(out[["WIN"]], 0)
  expect_lt(out[["LOSE"]], 0)
})

test_that("zero return everywhere gives zero contributions, not NaN", {
  out <- attribute_cumulative_return(matrix(0, 10, 2, dimnames = list(NULL, c("A", "B"))), c(A = 0.5, B = 0.5))

  expect_equal(unname(out), c(0, 0))
})

test_that("the sleeve model reports a contribution per sleeve that adds up to its cumulative return", {
  m <- model_fixture()
  res <- compute_sleeve_model(m$sleeves, m$prices, m$dates, sleeve_assumptions())

  expect_true("return_contribution_pct" %in% names(res$sleeves))
  expect_equal(sum(res$sleeves$return_contribution_pct), res$cumulative_return_pct, tolerance = 1e-9)
})
