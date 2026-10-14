test_that("calculate_volatility_from_prices fails gracefully with too little data", {
  prices_df <- tibble::tibble(
    ticker = "AAA",
    Date = as.Date("2026-01-01") + 0:9,
    Adjusted_close = 100 + 0:9
  )

  result <- calculate_volatility_from_prices("AAA", prices_df)

  expect_false(result$success)
  expect_match(result$error, "10 days")
})

test_that("calculate_volatility_from_prices computes ~zero volatility for a constant-return series", {
  n <- 30
  daily_return <- 0.001
  prices_df <- tibble::tibble(
    ticker = "AAA",
    Date = as.Date("2026-01-01") + 0:(n - 1),
    Adjusted_close = 100 * (1 + daily_return)^(0:(n - 1))
  )

  result <- calculate_volatility_from_prices("AAA", prices_df)

  expect_true(result$success)
  expect_equal(result$volatility_pct, 0)
  expect_equal(result$observations, n)
  expect_equal(result$mean_daily_return, round(log(1 + daily_return) * 100, 4))
})

test_that("calculate_volatility_from_prices respects period_days", {
  n <- 40
  prices_df <- tibble::tibble(
    ticker = "AAA",
    Date = as.Date("2026-01-01") + 0:(n - 1),
    Adjusted_close = 100 * 1.001^(0:(n - 1))
  )

  result <- calculate_volatility_from_prices("AAA", prices_df, period_days = 25)

  expect_true(result$success)
  expect_equal(result$observations, 25)
})

test_that("batch_calculate_volatility reports per-ticker success/failure", {
  prices_df <- dplyr::bind_rows(
    tibble::tibble(ticker = "GOOD", Date = as.Date("2026-01-01") + 0:29,
                   Adjusted_close = 100 * 1.001^(0:29)),
    tibble::tibble(ticker = "THIN", Date = as.Date("2026-01-01") + 0:4,
                   Adjusted_close = 50 + 0:4)
  )

  result <- batch_calculate_volatility(c("GOOD", "THIN"), prices_df = prices_df)

  expect_equal(nrow(result), 2)
  good_row <- result[result$ticker == "GOOD", ]
  thin_row <- result[result$ticker == "THIN", ]

  expect_true(good_row$success)
  expect_false(is.na(good_row$volatility_pct))

  expect_false(thin_row$success)
  expect_true(is.na(thin_row$volatility_pct))
  expect_false(is.na(thin_row$error))
})

test_that("batch functions default to the EUR-converted cache, not local-currency prices", {
  set.seed(1)
  dates <- seq(as.Date("2026-01-01"), by = "day", length.out = 300)
  eur <- tibble::tibble(ticker = rep(c("AAA", "URTH"), each = 300), Date = rep(dates, 2),
                        Adjusted_close = rep(100 * cumprod(1 + rnorm(300, 0, 0.01)), 2))
  raw_called <- FALSE
  orig_eur <- load_cached_prices_eur; orig_raw <- load_cached_prices
  withr::defer({ load_cached_prices_eur <<- orig_eur; load_cached_prices <<- orig_raw })
  load_cached_prices_eur <<- function(...) eur
  load_cached_prices     <<- function(...) { raw_called <<- TRUE; eur[0, ] }

  vol  <- suppressMessages(capture.output(v <- batch_calculate_volatility("AAA")))
  beta <- batch_calculate_market_beta("AAA")

  expect_false(raw_called)
  expect_true(v$success)
  expect_equal(beta$market_beta, 1, tolerance = 1e-8)
})
