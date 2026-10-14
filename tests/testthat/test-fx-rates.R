## FX cache helpers: fx_symbol_for() (get_price_data.R) and load_fx_rates()
## (price_cache.R). No EODHD calls; load_fx_rates() runs against a temp meta/
## dir via the mocked here::here() used in test-price_cache.R.
source(here::here("scripts", "get_price_data.R"))

local_isolated_meta_dir <- function(env = parent.frame()) {
  tmp_dir <- withr::local_tempdir(.local_envir = env)
  testthat::local_mocked_bindings(
    here = function(...) file.path(tmp_dir, utils::tail(c(...), 1)),
    .package = "here",
    .env = env
  )
  tmp_dir
}

test_that("fx_symbol_for builds EODHD's EUR<CCY>.FOREX symbol", {
  expect_equal(fx_symbol_for(c("USD", "gbp")), c("EURUSD.FOREX", "EURGBP.FOREX"))
})

test_that("load_fx_rates returns currency/Date/rate from the FX cache", {
  tmp_dir <- local_isolated_meta_dir()
  write.csv(data.frame(
    ticker = c("EURUSD.FOREX", "EURUSD.FOREX", "EURKRW.FOREX"),
    Date = c("2026-09-25", "2026-09-26", "2026-09-25"),
    Adjusted_close = c(1.13, 1.14, 1540)
  ), file.path(tmp_dir, FX_CACHE_FILE), row.names = FALSE)

  fx <- load_fx_rates()

  expect_equal(names(fx), c("currency", "Date", "rate"))
  expect_equal(fx$currency, c("KRW", "USD", "USD"))
  expect_equal(fx$rate[fx$currency == "USD"], c(1.13, 1.14))
})

test_that("load_fx_rates ignores non-FX series and returns zero rows when the cache is missing", {
  tmp_dir <- local_isolated_meta_dir()
  expect_equal(nrow(load_fx_rates()), 0)

  write.csv(data.frame(ticker = c("AAPL", "EURUSD.FOREX"), Date = "2026-09-25",
                       Adjusted_close = c(200, 1.13)),
            file.path(tmp_dir, FX_CACHE_FILE), row.names = FALSE)

  expect_equal(load_fx_rates()$currency, "USD")
})

test_that("update_fx_cache skips EUR and NA and never calls the API for them", {
  local_isolated_meta_dir()

  expect_equal(nrow(update_fx_cache(c("EUR", NA), api_key = "")), 0)
})
