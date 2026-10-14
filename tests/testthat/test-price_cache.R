## load_cached_prices() (scripts/price_cache.R) always resolves its path via
## here::here("meta", prices_file) — there's no parameter to point it at an
## arbitrary directory. To keep these tests from reading/writing the real,
## git-tracked meta/ folder (which holds real cached price/ticker data),
## local_isolated_meta_dir() mocks here::here() for the scope of one test so
## it resolves into a fresh withr temp directory instead, discarding
## whichever leading path segments (e.g. "meta") the real code passes.
local_isolated_meta_dir <- function(env = parent.frame()) {
  tmp_dir <- withr::local_tempdir(.local_envir = env)
  testthat::local_mocked_bindings(
    here = function(...) file.path(tmp_dir, utils::tail(c(...), 1)),
    .package = "here",
    .env = env
  )
  tmp_dir
}

test_that("load_cached_prices returns an empty typed tibble when the file is missing", {
  local_isolated_meta_dir()

  result <- load_cached_prices("closing_prices_missing.csv")

  expect_equal(nrow(result), 0)
  expect_equal(colnames(result), c("ticker", "Date", "Adjusted_close"))
  expect_s3_class(result$Date, "Date")
})

test_that("load_cached_prices parses, sorts, and dedupes a valid cache", {
  tmp_dir <- local_isolated_meta_dir()
  temp_file <- "closing_prices_valid.csv"

  write.csv(
    data.frame(
      ticker = c("BBB", "AAA", "AAA", "AAA"),
      Date = c("2026-01-02", "2026-01-02", "2026-01-01", "2026-01-01"),
      Adjusted_close = c(20, 10.5, 10, 10.1)
    ),
    file.path(tmp_dir, temp_file), row.names = FALSE
  )

  result <- load_cached_prices(temp_file)

  expect_equal(nrow(result), 3)
  expect_equal(result$ticker, c("AAA", "AAA", "BBB"))
  expect_equal(as.character(result$Date), c("2026-01-01", "2026-01-02", "2026-01-02"))
  # of the two duplicate (AAA, 2026-01-01) rows, distinct() keeps the first
  expect_equal(result$Adjusted_close[1], 10)
})

test_that("load_cached_prices drops rows with a non-numeric Adjusted_close and warns", {
  tmp_dir <- local_isolated_meta_dir()
  temp_file <- "closing_prices_baddata.csv"

  write.csv(
    data.frame(
      ticker = c("AAA", "AAA"),
      Date = c("2026-01-01", "2026-01-02"),
      Adjusted_close = c("10.5", "n/a")
    ),
    file.path(tmp_dir, temp_file), row.names = FALSE
  )

  expect_warning(result <- load_cached_prices(temp_file), "non-numeric Adjusted_close")
  expect_equal(nrow(result), 1)
  expect_equal(result$ticker, "AAA")
})

test_that("load_cached_prices coalesces from the legacy lowercase date column when Date is NA", {
  tmp_dir <- local_isolated_meta_dir()
  temp_file <- "closing_prices_legacy.csv"

  write.csv(
    data.frame(
      ticker = "AAA",
      Date = NA_character_,
      date = "2026-01-01",
      Adjusted_close = 10
    ),
    file.path(tmp_dir, temp_file), row.names = FALSE
  )

  result <- load_cached_prices(temp_file)

  expect_equal(nrow(result), 1)
  expect_equal(as.character(result$Date), "2026-01-01")
})

test_that("find_price_jumps flags a level change from a spliced instrument, not normal moves", {
  prices <- tibble::tibble(
    ticker = rep(c("OK", "SPLICED"), each = 4),
    Date = rep(as.Date("2026-09-01") + 0:3, 2),
    Adjusted_close = c(100, 103, 99, 104,      # ordinary +/-5 %
                       77000, 77500, 35, 36)   # 77,000 -> 35 overnight
  )

  jumps <- find_price_jumps(prices)

  expect_equal(jumps$ticker, "SPLICED")
  expect_equal(jumps$Date, as.Date("2026-09-03"))
  expect_lt(jumps$log_return, -7)
})

test_that("find_price_jumps honours the threshold and returns an empty typed tibble when clean", {
  prices <- tibble::tibble(ticker = "A", Date = as.Date("2026-09-01") + 0:1, Adjusted_close = c(100, 150))

  expect_equal(nrow(find_price_jumps(prices, max_abs_log_return = 0.5)), 0)
  expect_equal(nrow(find_price_jumps(prices, max_abs_log_return = 0.3)), 1)
  expect_equal(names(find_price_jumps(prices[0, ])), c("ticker", "Date", "Adjusted_close", "log_return"))
})

test_that("periods_per_year is the observation frequency of the series, not a fixed day count", {
  daily <- as.Date("2025-01-01") + 0:729                         # quoted every calendar day (crypto)
  wd    <- daily[!format(daily, "%u") %in% c("6", "7")]          # Mon-Fri only (equities)

  expect_equal(periods_per_year(daily), 365.25, tolerance = 1e-3)
  expect_equal(periods_per_year(wd), 365.25 * 5 / 7, tolerance = 5e-3)
  expect_equal(periods_per_year(rev(c(daily, daily))), 365.25, tolerance = 1e-3)  # order, duplicates ignored
  expect_true(is.na(periods_per_year(daily[1])))
  expect_true(is.na(periods_per_year(as.Date(character()))))
})

test_that("annualising by elapsed time recovers the annual volatility for 7-day, weekday-only and holiday-thinned series", {
  set.seed(42)
  sigma_d <- 0.02
  annual_vol <- function(dates, log_price) sd(diff(log_price)) * sqrt(periods_per_year(dates))

  # Crypto: variance accrues with calendar time; quoted every day or only on weekdays
  days <- as.Date("2005-01-01") + 0:7299
  lp   <- cumsum(rnorm(length(days), 0, sigma_d))                # sigma_d per calendar day
  wd   <- !format(days, "%u") %in% c("6", "7")
  truth_crypto <- sigma_d * sqrt(365.25)
  expect_equal(annual_vol(days, lp), truth_crypto, tolerance = 0.05)
  expect_equal(annual_vol(days[wd], lp[wd]), truth_crypto, tolerance = 0.05)

  # Equities: variance accrues per trading day (weekdays); a foreign holiday removes a date
  # from the shared panel and merges two steps into one
  tdays <- days[wd]
  lpe   <- cumsum(rnorm(length(tdays), 0, sigma_d))              # sigma_d per trading day
  truth_equity <- sigma_d * sqrt(365.25 * 5 / 7)
  keep  <- runif(length(tdays)) > 0.15
  expect_equal(annual_vol(tdays, lpe), truth_equity, tolerance = 0.05)
  expect_equal(annual_vol(tdays[keep], lpe[keep]), truth_equity, tolerance = 0.05)

  # a fixed count per observation is clearly off on the thinned panel (the old x252 behaviour)
  naive <- sd(diff(lpe[keep])) * sqrt(365.25 * 5 / 7)
  expect_gt(abs(naive / truth_equity - 1), 0.05)
})

test_that("a series' volatility and annual return are scaled by the same observation frequency", {
  set.seed(2)
  d  <- as.Date("2026-01-01") + 0:119
  px <- tibble::tibble(ticker = "Btc", Date = d, Adjusted_close = 100 * cumprod(1 + rnorm(120, 0.001, 0.02)))
  lr <- diff(log(px$Adjusted_close))
  k  <- periods_per_year(d)

  r <- calculate_volatility_from_prices("Btc", px)

  expect_equal(r$periods_per_year, k)
  expect_equal(r$volatility_pct, sd(lr) * sqrt(k) * 100, tolerance = 1e-3)
  expect_equal(r$annual_return_pct, mean(lr) * k * 100, tolerance = 1e-3)
})

test_that("a weekday-only series is annualised by fewer periods per year than a 7-day series", {
  d  <- as.Date("2026-01-01") + 0:199
  wd <- d[!format(d, "%u") %in% c("6", "7")]
  mk <- function(dates) tibble::tibble(ticker = "X", Date = dates, Adjusted_close = 100 * 1.001^seq_along(dates))

  expect_equal(calculate_volatility_from_prices("X", mk(d))$periods_per_year, 365.25, tolerance = 2e-3)
  expect_equal(calculate_volatility_from_prices("X", mk(wd))$periods_per_year, 365.25 * 5 / 7, tolerance = 2e-2)
})
