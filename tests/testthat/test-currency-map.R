## scripts/get_currency_map.R — pure functions only; no EODHD calls.
## Sourced here (not in helper-source.R) because it pulls in httr/readxl and
## its standalone-refresh block is guarded on direct Rscript invocation.
source(here::here("scripts", "get_currency_map.R"))

symbol_lists <- list(
  US = tibble(Code = c("AAPL", "SPY", "AI"), Currency = c("USD", "USD", "USD")),
  KO = tibble(Code = "005930", Currency = "KRW"),
  XETRA = tibble(Code = "EOAN", Currency = "EUR"),
  LSE = tibble(Code = c("VOD", "RR"), Currency = c("GBX", "GBP")),
  PA = tibble(Code = "MC", Currency = c("EUR", "USD"))
)

test_that("exchange_code_for_ticker uses the suffix, or US when bare", {
  expect_equal(
    exchange_code_for_ticker(c("AAPL", "005930.KO", "EOAN.XETRA", "brk.b.us")),
    c("US", "KO", "XETRA", "US")
  )
})

test_that("resolve_ticker_currency reads currency from the matching exchange list", {
  res <- resolve_ticker_currency(c("AAPL", "005930.KO", "EOAN.XETRA"), symbol_lists)

  expect_equal(res$currency, c("USD", "KRW", "EUR"))
  expect_true(all(res$source == "symbol_list"))
})

test_that("resolve_ticker_currency applies precedence: manual > crypto > synthetic", {
  manual <- tibble(ticker = "AAPL", currency = "EUR", price_scale = 1)
  res <- resolve_ticker_currency(
    c("AAPL", "BTC", CASH_TICKER, BONDS_LR_TICKER), symbol_lists,
    crypto_tickers = "BTC", manual = manual
  )

  expect_equal(res$source, c("manual", "crypto", "synthetic", "synthetic"))
  expect_equal(res$currency, c("EUR", "USD", "EUR", "EUR"))
})

test_that("resolve_ticker_currency leaves unknown tickers and unfetched exchanges unresolved", {
  res <- resolve_ticker_currency(c("NOPE", "1234.TW"), symbol_lists)

  expect_true(all(is.na(res$currency)))
  expect_true(all(res$source == "unresolved"))
})

test_that("resolve_ticker_currency converts pence quotes to GBP with a 0.01 scale", {
  # VOD is labelled GBX; RR is labelled GBP but sits on the LSE, whose prices
  # are pence either way
  res <- resolve_ticker_currency(c("VOD.LSE", "RR.LSE"), symbol_lists)

  expect_equal(res$currency, c("GBP", "GBP"))
  expect_equal(res$price_scale, c(0.01, 0.01))
})

test_that("an LSE listing the symbol list reports in another currency keeps scale 1", {
  lists <- list(LSE = tibble(Code = "IWDA", Currency = "USD"))

  res <- resolve_ticker_currency("IWDA.LSE", lists)

  expect_equal(res$currency, "USD")
  expect_equal(res$price_scale, 1)
})

test_that("a GBP label outside the LSE is not treated as pence", {
  lists <- list(XX = tibble(Code = "ABC", Currency = "GBP"))

  res <- resolve_ticker_currency("ABC.XX", lists)

  expect_equal(res$price_scale, 1)
})

test_that("resolve_ticker_currency flags a code listed under several currencies", {
  res <- resolve_ticker_currency("MC.PA", symbol_lists)

  expect_equal(res$currency, "EUR")
  expect_equal(res$source, "symbol_list_ambiguous")
})

test_that("resolve_ticker_currency returns one row per unique ticker", {
  res <- resolve_ticker_currency(c("AAPL", "AAPL", "SPY"), symbol_lists)

  expect_equal(res$ticker, c("AAPL", "SPY"))
})

test_that("flag_bare_non_us_tickers lists suffix-less tickers whose country is not the USA", {
  currency_map <- resolve_ticker_currency(c("AAPL", "AI", "005930.KO"), symbol_lists)
  tickerliste <- tibble(
    ticker = c("AAPL", "AI", "005930"),
    alpha3 = c("USA", "FRA", "KOR"),
    companyname = c("Apple", "Air Liquide", "Samsung")
  )

  flagged <- flag_bare_non_us_tickers(currency_map, tickerliste)

  expect_equal(flagged$ticker, "AI")
  expect_equal(flagged$alpha3, "FRA")
})
