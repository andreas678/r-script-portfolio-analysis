## compute_drawdown_table() (scripts/portfolio_returns.R) - pure.

dd_prices <- function() {
  days <- seq(as.Date("2026-01-05"), by = "day", length.out = 8)
  dplyr::bind_rows(
    # peak 120 on day 3, trough 90 on day 6, last 108
    tibble::tibble(ticker = "PEAKED", Date = days, Adjusted_close = c(100, 110, 120, 100, 95, 90, 100, 108)),
    # rises all the way: no drawdown
    tibble::tibble(ticker = "RISING", Date = days, Adjusted_close = 100 + 1:8),
    tibble::tibble(ticker = "SHORT", Date = days[1], Adjusted_close = 50)
  )
}

test_that("max and current drawdown are measured from the running and the highest peak", {
  out <- compute_drawdown_table(dd_prices(), c("PEAKED", "RISING"), momentum_days = 3)

  p <- out[out$ticker == "PEAKED", ]
  expect_equal(p$max_drawdown_pct, (90 / 120 - 1) * 100)
  expect_equal(p$current_drawdown_pct, (108 / 120 - 1) * 100)
  expect_equal(p$peak_date, as.Date("2026-01-07"))
  expect_equal(p$momentum_pct, (108 / 95 - 1) * 100)             # 3 observations back from the last (8th) is the 5th price, 95
})

test_that("a series that only rises has no drawdown and sits at its peak", {
  r <- compute_drawdown_table(dd_prices(), "RISING")

  expect_equal(r$max_drawdown_pct, 0)
  expect_equal(r$current_drawdown_pct, 0)
  expect_equal(r$peak_date, as.Date("2026-01-12"))
})

test_that("`from` restricts the span and series with fewer than two prices are left out", {
  out <- compute_drawdown_table(dd_prices(), c("PEAKED", "SHORT"), from = as.Date("2026-01-08"))

  expect_equal(out$ticker, "PEAKED")                            # SHORT has one price
  expect_equal(out$first_date, as.Date("2026-01-08"))
  expect_equal(out$max_drawdown_pct, (90 / 100 - 1) * 100)     # peak 100 on 01-08, trough 90
})

test_that("momentum is NA when the series is not longer than the look-back, order follows `tickers`", {
  out <- compute_drawdown_table(dd_prices(), c("RISING", "PEAKED"), momentum_days = 20)

  expect_equal(out$ticker, c("RISING", "PEAKED"))
  expect_true(all(is.na(out$momentum_pct)))
})

test_that("the deepest fall is dated: the high it started from and the low it reached", {
  # peak 120 on 01-07, trough 90 on 01-10 (the deepest fall), last 108
  out <- compute_drawdown_table(dd_prices(), "PEAKED")

  expect_equal(out$max_drawdown_peak_date, as.Date("2026-01-07"))
  expect_equal(out$max_drawdown_trough_date, as.Date("2026-01-10"))
})

test_that("the deepest fall can start from an EARLIER high than the current one", {
  days <- seq(as.Date("2026-03-02"), by = "day", length.out = 9)
  # 100 -> 60 (deepest: -40 %) -> back to 130 (new high) -> 117 now (-10 % from the new high)
  px <- tibble::tibble(ticker = "ZIGZAG", Date = days,
                       Adjusted_close = c(100, 80, 60, 90, 130, 125, 120, 119, 117))
  out <- compute_drawdown_table(px, "ZIGZAG")

  expect_equal(out$max_drawdown_pct, -40)
  expect_equal(out$max_drawdown_peak_date, as.Date("2026-03-02"))
  expect_equal(out$max_drawdown_trough_date, as.Date("2026-03-04"))
  expect_equal(out$current_drawdown_pct, -10)
  expect_equal(out$peak_date, as.Date("2026-03-06"))                 # the high of the CURRENT drawdown
})

test_that("a series that never fell has no drawdown dates", {
  out <- compute_drawdown_table(dd_prices(), "RISING")

  expect_true(is.na(out$max_drawdown_peak_date) && is.na(out$max_drawdown_trough_date))
})
