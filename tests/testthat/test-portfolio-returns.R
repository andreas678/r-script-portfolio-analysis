## Tests for scripts/portfolio_returns.R's pure functions
## (DEVELOPMENT-PROPOSAL-2026-09-27.md §3.1, item 4) — the return-series
## helpers behind portfolio_report.Rmd's Section 6 (cumulative returns,
## holding-level positioning, and 6.4's sector performance attribution).
##
## No EODHD calls: all fixtures are small hand-built tibbles with values
## chosen so the expected results are computable by hand, same approach as
## the manual verification done during §3.5's implementation — this just
## formalizes that verification into the suite, as the proposal calls for.

test_that("prices_to_wide keeps only tickers meeting min_observations and drops NA rows", {
  prices_long <- tibble::tibble(
    ticker = c(rep("AAA", 5), rep("BBB", 5), rep("THIN", 2)),
    Date = as.Date(c(
      "2026-01-01", "2026-01-02", "2026-01-03", "2026-01-04", "2026-01-05",
      "2026-01-01", "2026-01-02", "2026-01-03", "2026-01-04", "2026-01-05",
      "2026-01-01", "2026-01-02"
    )),
    Adjusted_close = c(100, 101, 102, 103, 104, 50, 51, 52, 53, 54, 10, 11)
  )

  wide <- prices_to_wide(prices_long, min_observations = 5)

  expect_equal(names(wide), c("Date", "AAA", "BBB"))  # THIN dropped (only 2 obs)
  expect_equal(nrow(wide), 5)
  expect_false(anyNA(wide))
  expect_equal(wide$AAA, c(100, 101, 102, 103, 104))
})

test_that("compute_daily_log_returns matches hand-computed log returns and drops the first row", {
  prices_wide <- tibble::tibble(
    Date = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03")),
    AAA = c(100, 110, 121)  # exactly +10% each day
  )

  returns <- compute_daily_log_returns(prices_wide)

  expect_equal(nrow(returns), 2)
  expect_equal(returns$AAA, c(log(1.1), log(1.1)), tolerance = 1e-9)
})

test_that("compute_portfolio_return_series weights and renormalizes correctly", {
  returns_wide <- tibble::tibble(
    Date = as.Date(c("2026-01-01", "2026-01-02")),
    AAA = c(0.02, -0.01),
    BBB = c(0.00, 0.03)
  )

  # Weights deliberately don't sum to 1 (60/60 raw) -> renormalized to 0.5/0.5
  result <- compute_portfolio_return_series(returns_wide, c(AAA = 60, BBB = 60))

  expect_equal(result$portfolio_return,
               log(c(0.5 * exp(0.02) + 0.5 * exp(0.00), 0.5 * exp(-0.01) + 0.5 * exp(0.03))),
               tolerance = 1e-9)
})

test_that("the portfolio return is the log of the weighted SIMPLE return (daily rebalanced), not the weighted log return", {
  # +50% and -40% in one day: the weighted average of log returns (-0.0527) would say the 50/50
  # mix lost 5%, but 50/50 of the simple returns (+50%, -40%) is a +5% day
  returns_wide <- tibble::tibble(Date = as.Date("2026-01-01"), UP = log(1.5), DOWN = log(0.6))

  result <- compute_portfolio_return_series(returns_wide, c(UP = 1, DOWN = 1))

  expect_equal(exp(result$portfolio_return) - 1, 0.5 * 0.5 + 0.5 * -0.4)
  expect_gt(result$portfolio_return, 0.5 * log(1.5) + 0.5 * log(0.6))
})

test_that("a single asset, or identical assets, keep their own return", {
  returns_wide <- tibble::tibble(Date = as.Date(c("2026-01-01", "2026-01-02")),
                                 AAA = c(0.02, -0.03), BBB = c(0.02, -0.03))

  expect_equal(compute_portfolio_return_series(returns_wide, c(AAA = 1))$portfolio_return, c(0.02, -0.03))
  expect_equal(compute_portfolio_return_series(returns_wide, c(AAA = 3, BBB = 1))$portfolio_return, c(0.02, -0.03))
})

test_that("a portfolio of volatile assets compounds above the weighted average of the log returns (Jensen)", {
  set.seed(7)
  returns_wide <- tibble::tibble(Date = as.Date("2026-01-01") + 0:249,
                                 A = rnorm(250, 0, 0.03), B = rnorm(250, 0, 0.03), C = rnorm(250, 0, 0.03))
  w <- c(A = 0.5, B = 0.3, C = 0.2)

  new_series <- compute_portfolio_return_series(returns_wide, w)$portfolio_return
  old_series <- as.numeric(as.matrix(returns_wide[, names(w)]) %*% w)

  expect_true(all(new_series >= old_series))
  expect_gt(sum(new_series) - sum(old_series), 0.01)
})

test_that("compute_portfolio_return_series errors when weights and columns don't overlap at all", {
  returns_wide <- tibble::tibble(Date = as.Date("2026-01-01"), AAA = 0.01)
  expect_error(compute_portfolio_return_series(returns_wide, c(ZZZ = 1)), "no overlap")
})

test_that("rebase_to_one divides by the first element", {
  expect_equal(rebase_to_one(c(2, 4, 6)), c(1, 2, 3))
  expect_equal(rebase_to_one(c(0.5, 1, 1.5)), c(1, 2, 3))
})

# A benchmark with 6 trading days; the portfolio only kept 4 of them (day 3 and 5 missing,
# as when a holding's market was closed).
agg_fixture <- function() {
  days   <- as.Date("2026-03-02") + 0:5
  prices <- c(100, 102, 101, 105, 104, 110)
  list(
    days = days, prices = prices,
    returns = tibble::tibble(Date = days[-1], BENCH = diff(log(prices))),   # 5 daily log returns
    target = days[c(1, 2, 4, 6)]
  )
}

test_that("aggregate_returns_to_dates equals price-to-price growth over the target dates", {
  f <- agg_fixture()

  agg <- aggregate_returns_to_dates(f$returns, f$target)

  expect_equal(agg$Date, f$target)
  expect_equal(agg$BENCH[1], 0)                                  # baseline
  expect_equal(exp(cumsum(agg$BENCH)), f$prices[c(1, 2, 4, 6)] / f$prices[1])
  expect_equal(exp(sum(agg$BENCH)) - 1, 110 / 100 - 1)
})

test_that("filtering to the target dates instead loses the skipped days' returns (the old bug)", {
  f <- agg_fixture()

  filtered <- dplyr::filter(f$returns, Date %in% f$target[-1])

  # compounds 3 of the 5 daily returns (days 2, 4, 6 against their own previous day), not 100 -> 110
  expect_equal(exp(sum(filtered$BENCH)) - 1, (102 / 100) * (105 / 101) * (110 / 104) - 1)
  expect_false(isTRUE(all.equal(exp(sum(filtered$BENCH)) - 1, 0.10)))
})

test_that("aggregate_returns_to_dates handles several series, unordered dates and out-of-range returns", {
  f <- agg_fixture()
  two <- dplyr::mutate(f$returns, OTHER = 2 * BENCH)

  agg <- aggregate_returns_to_dates(two, rev(f$target[2:3]))   # baseline = day 2, last = day 4

  expect_named(agg, c("Date", "BENCH", "OTHER"))
  expect_equal(agg$Date, f$target[2:3])
  expect_equal(exp(agg$BENCH[2]), 105 / 102)                    # returns of days 3 and 4 only
  expect_equal(agg$OTHER, 2 * agg$BENCH)
})

test_that("aggregate_returns_to_dates needs two target dates", {
  f <- agg_fixture()

  expect_error(aggregate_returns_to_dates(f$returns, f$target[1]), "at least two")
})

test_that("annualize_returns scales every series of a panel by the panel's own observation frequency", {
  dates <- as.Date("2026-01-05") + c(0:4, 7:11, 14:18, 21:25)                  # four weekday weeks
  panel <- tibble::tibble(Date = dates, A = seq(-0.01, 0.01, length.out = 20), B = rep(0.002, 20))
  k <- periods_per_year(dates)

  out <- annualize_returns(panel)

  expect_equal(out$ticker, c("A", "B"))
  expect_equal(out$volatility_pct, c(sd(panel$A) * sqrt(k) * 100, 0))
  expect_equal(out$annual_return_pct, c(mean(panel$A) * k * 100, 0.002 * k * 100))
  expect_equal(out$periods_per_year, c(k, k))
})

test_that("annualize_returns takes an explicit frequency, so a window and its benchmark can share one", {
  panel <- tibble::tibble(Date = as.Date("2026-01-05") + 0:9, A = rep(c(0.01, -0.01), 5))

  out <- annualize_returns(panel, periods = 200)

  expect_equal(out$periods_per_year, 200)
  expect_equal(out$volatility_pct, sd(panel$A) * sqrt(200) * 100)
})

test_that("a holding and the portfolio built from it get the same annual figures over one window", {
  set.seed(5)
  d <- as.Date("2026-01-05") + 0:249
  panel <- tibble::tibble(Date = d, ONLY = rnorm(250, 0.0005, 0.012))
  port  <- compute_portfolio_return_series(panel, c(ONLY = 1))

  holding <- annualize_returns(panel)
  portfolio <- annualize_returns(dplyr::rename(port, ONLY = portfolio_return))

  expect_equal(holding$volatility_pct, portfolio$volatility_pct)
  expect_equal(holding$annual_return_pct, portfolio$annual_return_pct)
})

test_that("compute_sector_attribution's contributions sum exactly to the blended return", {
  sector_summary <- tibble::tibble(
    sector = c("Information Technology", "Financials", "Health Care"),
    weight = c(50, 30, 20),  # sums to 100, no renormalization needed
    avg_return = c(0.15, 0.05, -0.02)
  )

  result <- compute_sector_attribution(sector_summary)

  blended <- weighted.mean(sector_summary$avg_return, sector_summary$weight)
  expect_equal(sum(result$contribution_pct), blended, tolerance = 1e-9)
  expect_false("excess_contribution_pct" %in% names(result))
  # sorted descending by contribution_pct
  expect_equal(result$sector[1], "Information Technology")
})

test_that("compute_sector_attribution renormalizes weight and adds excess_contribution_pct vs a benchmark", {
  sector_summary <- tibble::tibble(
    sector = c("A", "B"),
    weight = c(10, 30),  # sums to 40, not 100 -> renormalized to 0.25 / 0.75
    avg_return = c(0.10, 0.02)
  )
  benchmark_return <- 0.08

  result <- compute_sector_attribution(sector_summary, benchmark_return = benchmark_return)

  blended <- 0.25 * 0.10 + 0.75 * 0.02
  expect_equal(sum(result$contribution_pct), blended, tolerance = 1e-9)
  expect_equal(sum(result$excess_contribution_pct), blended - benchmark_return, tolerance = 1e-9)
  # contribution_pct: A = 0.25*0.10 = 0.025, B = 0.75*0.02 = 0.015 -> A first (higher contribution)
  expect_equal(result$sector, c("A", "B"))
  expect_equal(result$normalized_weight, c(0.25, 0.75))
})

## compute_fund_tracking() / compute_fund_tracking_table()
make_prices <- function(n = 200, seed = 11, tickers = c("A", "B", "C")) {
  set.seed(seed)
  d <- seq(as.Date("2026-01-01"), by = "day", length.out = n)
  dplyr::bind_rows(lapply(tickers, function(tk) {
    tibble::tibble(ticker = tk, Date = d, Adjusted_close = 100 * cumprod(1 + rnorm(n, 0, 0.01)))
  }))
}

# a fund whose daily return is exactly the daily-rebalanced mix of its holdings
add_fund <- function(prices, fund = "FUND", w = c(A = 0.5, B = 0.3, C = 0.2), noise = 0) {
  rets <- prices |>
    dplyr::group_by(ticker) |>
    dplyr::arrange(Date, .by_group = TRUE) |>
    dplyr::mutate(r = c(0, diff(log(Adjusted_close)))) |>
    dplyr::ungroup()
  d <- unique(rets$Date)
  fr <- vapply(d, function(dt) log(sum(w[rets$ticker[rets$Date == dt]] * exp(rets$r[rets$Date == dt]))), 0)
  set.seed(99)
  fr <- fr + rnorm(length(fr), 0, noise)
  dplyr::bind_rows(prices, tibble::tibble(ticker = fund, Date = d, Adjusted_close = 100 * exp(cumsum(fr))))
}

test_that("a fund that IS its holdings tracks perfectly", {
  px <- add_fund(make_prices())
  h <- tibble::tibble(ticker = c("A", "B", "C"), weight = c(50, 30, 20))

  r <- compute_fund_tracking(h, "FUND", px)

  expect_equal(r$coverage_pct, 100)
  expect_equal(r$correlation, 1, tolerance = 1e-6)
  expect_equal(r$beta, 1, tolerance = 1e-6)
  expect_lt(r$tracking_error_pct, 1e-4)
  expect_equal(r$note, "")
})

test_that("noise lowers correlation and raises tracking error; wrong weights are visible", {
  px <- add_fund(make_prices(), noise = 0.005)
  good  <- compute_fund_tracking(tibble::tibble(ticker = c("A", "B", "C"), weight = c(50, 30, 20)), "FUND", px)
  wrong <- compute_fund_tracking(tibble::tibble(ticker = c("A", "B", "C"), weight = c(5, 5, 90)), "FUND", px)

  expect_lt(good$correlation, 1)
  expect_gt(good$tracking_error_pct, 1)
  expect_lt(wrong$correlation, good$correlation)
})

test_that("the tracking error is annualised by the panel's own observation frequency", {
  px <- add_fund(make_prices(), noise = 0.005)
  # weekdays only, so the panel is clearly thinner than a 7-day series
  px <- dplyr::filter(px, !format(Date, "%u") %in% c("6", "7"))
  h <- tibble::tibble(ticker = c("A", "B", "C"), weight = c(50, 30, 20))

  r <- compute_fund_tracking(h, "FUND", px)

  panel <- build_return_panel(px, tickers = c("A", "B", "C", "FUND"), min_observations = 100)
  x <- compute_portfolio_return_series(panel[, c("Date", "A", "B", "C")], c(A = 0.5, B = 0.3, C = 0.2))$portfolio_return
  expect_equal(r$tracking_error_pct, sd(panel$FUND - x) * sqrt(periods_per_year(panel$Date)) * 100)
  expect_lt(periods_per_year(panel$Date), 300)   # not the 365 of a 7-day series
})

test_that("holdings without a price history lower coverage but not the covered fit", {
  px <- add_fund(make_prices())
  h <- tibble::tibble(ticker = c("A", "B", "C", "NOPRICE"), weight = c(40, 24, 16, 20))

  r <- compute_fund_tracking(h, "FUND", px)

  expect_equal(r$n_holdings, 4)
  expect_equal(r$n_covered, 3)
  expect_equal(r$coverage_pct, 80)
  expect_equal(r$correlation, 1, tolerance = 1e-6)
})

test_that("compute_fund_tracking explains missing inputs instead of failing", {
  px <- make_prices()
  h <- tibble::tibble(ticker = c("A", "B"), weight = c(50, 50))

  no_fund <- compute_fund_tracking(h, "GONE", px)
  none    <- compute_fund_tracking(tibble::tibble(ticker = "ZZZ", weight = 1), "A", px)
  empty   <- compute_fund_tracking(tibble::tibble(ticker = character(), weight = numeric()), "A", px)

  expect_match(no_fund$note, "no current fund price series")
  expect_true(is.na(no_fund$correlation))
  expect_match(none$note, "no holding with a current price history")
  expect_equal(empty$note, "no holdings")
})

test_that("compute_fund_tracking_table runs only funds that have a listing", {
  px <- add_fund(make_prices())
  fund_data <- tibble::tibble(
    file = c("F.xlsx", "Cash.xlsx"), label = c("Fund", "Cash"),
    raw = list(tibble::tibble(ticker = c("A", "B", "C"), weight = c(50, 30, 20)),
               tibble::tibble(ticker = "CASH_EUR", weight = 100))
  )
  listings <- tibble::tibble(fund_file = "F.xlsx", ticker = "FUND")

  out <- compute_fund_tracking_table(fund_data, listings, px)

  expect_equal(out$label, "Fund")
  expect_equal(out$etf_ticker, "FUND")
  expect_equal(out$correlation, 1, tolerance = 1e-6)
})

# ---- concentration_metrics ---------------------------------------------------------------------
test_that("concentration_metrics: equal weights give ENP = n and the top-k share k/n", {
  m <- concentration_metrics(rep(5, 20))

  expect_equal(m$enp, 20)
  expect_equal(m$hhi, 1 / 20)
  expect_equal(unname(m$top[c("top1", "top5", "top10", "top20")]), c(5, 25, 50, 100))
})

test_that("concentration_metrics measures within the set: scaling the weights changes nothing", {
  w <- c(50, 30, 15, 5)

  expect_equal(concentration_metrics(w)$hhi, concentration_metrics(w / 100)$hhi)
  expect_equal(concentration_metrics(w)$top[["top1"]], 50)
  expect_equal(concentration_metrics(c(1, 0, 0))$enp, 1)
})

test_that("concentration_metrics rejects empty, NA, negative and all-zero weights", {
  expect_error(concentration_metrics(numeric()), "non-empty")
  expect_error(concentration_metrics(c(1, NA)), "non-empty")
  expect_error(concentration_metrics(c(1, -1)), "non-empty")
  expect_error(concentration_metrics(c(0, 0)), "non-empty")
})
