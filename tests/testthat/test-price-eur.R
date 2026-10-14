## load_cached_prices_eur() / locf_fx_rate() (scripts/price_cache.R).
## Runs against a temp meta/ dir via the mocked here::here() used in
## test-price_cache.R; no EODHD calls.
local_isolated_meta_dir <- function(env = parent.frame()) {
  tmp_dir <- withr::local_tempdir(.local_envir = env)
  testthat::local_mocked_bindings(
    here = function(...) file.path(tmp_dir, utils::tail(c(...), 1)),
    .package = "here",
    .env = env
  )
  tmp_dir
}

# Writes the three cache files load_cached_prices_eur() reads.
write_eur_fixture <- function(tmp_dir, prices, fx, currency) {
  write.csv(prices, file.path(tmp_dir, PRICE_CACHE_FILE), row.names = FALSE)
  write.csv(fx, file.path(tmp_dir, FX_CACHE_FILE), row.names = FALSE)
  write.csv(currency, file.path(tmp_dir, TICKER_CURRENCY_FILE), row.names = FALSE)
}

fx_fixture <- function(...) {
  data.frame(ticker = "EURUSD.FOREX", Date = c("2026-09-24", "2026-09-25"),
             Adjusted_close = c(1.10, 1.25), ...)
}

test_that("locf_fx_rate carries the last known rate forward and never looks ahead", {
  fx_dates <- as.Date(c("2026-09-25", "2026-09-24"))  # unsorted on purpose
  fx_rates <- c(1.25, 1.10)

  rate <- locf_fx_rate(as.Date(c("2026-09-23", "2026-09-24", "2026-09-26", "2026-09-28")),
                       fx_dates, fx_rates, max_age_days = 7)

  expect_equal(rate, c(NA, 1.10, 1.25, 1.25))  # before first rate -> NA, weekend -> Friday
})

test_that("locf_fx_rate treats a rate older than max_age_days as unknown", {
  rate <- locf_fx_rate(as.Date(c("2026-09-26", "2026-10-10")), as.Date("2026-09-25"), 1.25,
                       max_age_days = 7)

  expect_equal(rate, c(1.25, NA))
})

test_that("USD prices are divided by the EUR<CCY> rate; EUR prices pass through", {
  tmp <- local_isolated_meta_dir()
  write_eur_fixture(
    tmp,
    prices = data.frame(ticker = c("AAPL", "AAPL", "SAP.XETRA"),
                        Date = c("2026-09-24", "2026-09-25", "2026-09-25"),
                        Adjusted_close = c(110, 250, 200)),
    fx = fx_fixture(),
    currency = data.frame(ticker = c("AAPL", "SAP.XETRA"), currency = c("USD", "EUR"),
                          price_scale = 1)
  )

  out <- load_cached_prices_eur()

  expect_equal(names(out), c("ticker", "Date", "Adjusted_close"))
  expect_equal(out$Adjusted_close[out$ticker == "AAPL"], c(100, 200))  # 110/1.10, 250/1.25
  expect_equal(out$Adjusted_close[out$ticker == "SAP.XETRA"], 200)
  expect_equal(nrow(attr(out, "dropped")), 0)
})

test_that("a weekend price uses the previous business day's rate", {
  tmp <- local_isolated_meta_dir()
  write_eur_fixture(
    tmp,
    prices = data.frame(ticker = "BTC", Date = "2026-09-27", Adjusted_close = 125),
    fx = fx_fixture(),
    currency = data.frame(ticker = "BTC", currency = "USD", price_scale = 1)
  )

  expect_equal(load_cached_prices_eur()$Adjusted_close, 100)  # 125 / 1.25 (Friday's rate)
})

test_that("pence quotes are scaled before conversion", {
  tmp <- local_isolated_meta_dir()
  write_eur_fixture(
    tmp,
    prices = data.frame(ticker = "RR.LSE", Date = "2026-09-25", Adjusted_close = 1250),
    fx = data.frame(ticker = "EURGBP.FOREX", Date = "2026-09-25", Adjusted_close = 0.5),
    currency = data.frame(ticker = "RR.LSE", currency = "GBP", price_scale = 0.01)
  )

  expect_equal(load_cached_prices_eur()$Adjusted_close, 25)  # 1250p = 12.50 GBP / 0.5
})

test_that("unconvertible rows are dropped with one warning, never passed through as EUR", {
  tmp <- local_isolated_meta_dir()
  write_eur_fixture(
    tmp,
    prices = data.frame(
      ticker = c("AAPL", "AAPL", "NOMAP", "NOCCY", "TOYOTA"),
      Date = c("2026-09-23", "2026-09-25", "2026-09-25", "2026-09-25", "2026-09-25"),
      Adjusted_close = c(100, 250, 10, 10, 10)
    ),
    fx = fx_fixture(),
    currency = data.frame(ticker = c("AAPL", "NOCCY", "TOYOTA"),
                          currency = c("USD", NA, "JPY"), price_scale = 1)
  )

  expect_warning(out <- load_cached_prices_eur(),
                 "dropped 4 of 5.*NOCCY, NOMAP.*JPY.*1 row")

  expect_equal(out$ticker, "AAPL")
  expect_equal(out$Date, as.Date("2026-09-25"))  # 2026-09-23 predates the first FX rate
  expect_equal(nrow(attr(out, "dropped")), 4)
})

test_that("a missing FX cache drops every non-EUR row instead of returning local prices", {
  tmp <- local_isolated_meta_dir()
  write.csv(data.frame(ticker = c("AAPL", "SAP.XETRA"), Date = "2026-09-25",
                       Adjusted_close = c(250, 200)),
            file.path(tmp, PRICE_CACHE_FILE), row.names = FALSE)
  write.csv(data.frame(ticker = c("AAPL", "SAP.XETRA"), currency = c("USD", "EUR"), price_scale = 1),
            file.path(tmp, TICKER_CURRENCY_FILE), row.names = FALSE)

  expect_warning(out <- load_cached_prices_eur(), "no cached FX series for: USD")
  expect_equal(out$ticker, "SAP.XETRA")
})

test_that("prices_to_wide accepts the EUR loader's output (no extra columns to break the pivot)", {
  tmp <- local_isolated_meta_dir()
  dates <- as.character(as.Date("2026-01-01") + 0:104)
  write_eur_fixture(
    tmp,
    prices = data.frame(ticker = rep(c("A", "B"), each = 105), Date = rep(dates, 2),
                        Adjusted_close = seq_len(210) + 100),
    fx = data.frame(ticker = "EURUSD.FOREX", Date = dates, Adjusted_close = 1.1),
    currency = data.frame(ticker = c("A", "B"), currency = c("USD", "EUR"), price_scale = 1)
  )

  wide <- prices_to_wide(load_cached_prices_eur())

  expect_equal(names(wide), c("Date", "A", "B"))
  expect_equal(nrow(wide), 105)
})
