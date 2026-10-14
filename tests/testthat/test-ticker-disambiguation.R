## scripts/utils/ticker_disambiguation.R — pure functions, rules passed in
## directly so nothing reads the real config/ file.
source(here::here("scripts", "utils", "ticker_disambiguation.R"))

rules <- tibble(
  ticker       = c("SAN", "SAN", "ROP", "ROP", "NDIA", "DG"),
  name_pattern = c("SANTANDER", "SANOFI", "ROCHE", "ROPER", "INDIA", "VINCI"),
  alpha3       = c(NA, NA, NA, NA, NA, "FRA"),
  new_ticker   = c("SAN.MC", "SAN.PA", "ROP.SW", "ROP", NA, "DG.PA"),
  expected_currency = NA_character_, note = NA_character_
)

holdings <- function(...) tibble(...)

test_that("one colliding ticker is split per company by name", {
  df <- holdings(ticker = c("SAN", "SAN", "SAN"),
                 name = c("Banco Santander SA", "SANOFI SA", "banco santander"))

  out <- apply_ticker_disambiguation(df, rules)

  expect_equal(out$ticker, c("SAN.MC", "SAN.PA", "SAN.MC"))
})

test_that("a rule that keeps the bare ticker leaves it unchanged", {
  out <- apply_ticker_disambiguation(
    holdings(ticker = c("ROP", "ROP"), name = c("Roche Holding AG", "Roper Technologies Inc")),
    rules
  )

  expect_equal(out$ticker, c("ROP.SW", "ROP"))
})

test_that("tickers without a rule pass through untouched", {
  out <- apply_ticker_disambiguation(
    holdings(ticker = c("AAPL", "SAN"), name = c("Apple Inc", "Sanofi SA")), rules
  )

  expect_equal(out$ticker, c("AAPL", "SAN.PA"))
})

test_that("alpha3 restricts a rule when the rule sets it", {
  df <- holdings(ticker = c("DG", "DG"), name = c("Vinci SA", "Vinci SA"), alpha3 = c("FRA", "USA"))

  out <- suppressWarnings(apply_ticker_disambiguation(df, rules))

  expect_equal(out$ticker, c("DG.PA", "DG"))
})

test_that("an unseen company under a colliding ticker warns and stays unchanged", {
  df <- holdings(ticker = "SAN", name = "Some New Company")

  expect_warning(out <- apply_ticker_disambiguation(df, rules), "unmatched")
  expect_equal(out$ticker, "SAN")
})

test_that("a rule with no new_ticker counts as handled and changes nothing", {
  df <- holdings(ticker = "NDIA", name = "ISHARES MSCI INDIA UCITS ETF")

  expect_no_warning(out <- apply_ticker_disambiguation(df, rules))
  expect_equal(out$ticker, "NDIA")
})

test_that("rules match on the original ticker, so they never chain", {
  chained <- tibble(ticker = c("A", "A.X"), name_pattern = c("FOO", "FOO"),
                    alpha3 = NA_character_, new_ticker = c("A.X", "A.Y"),
                    expected_currency = NA_character_, note = NA_character_)

  out <- apply_ticker_disambiguation(holdings(ticker = "A", name = "Foo Inc"), chained)

  expect_equal(out$ticker, "A.X")
})

test_that("the shipped config table covers every ticker it names without warnings", {
  real <- load_ticker_disambiguation()
  skip_if(nrow(real) == 0, "config/ticker_disambiguation.csv not present")

  expect_true(all(c("ticker", "name_pattern", "new_ticker") %in% names(real)))
  expect_false(any(duplicated(real[c("ticker", "name_pattern", "alpha3")])))
})

test_that("leading-zero order-book codes are only ever mapped to the LSE listing", {
  real <- load_ticker_disambiguation()
  skip_if(nrow(real) == 0, "config/ticker_disambiguation.csv not present")

  zero <- real$new_ticker[grepl("^0[A-Z0-9]{2,4}([.]|$)", real$new_ticker)]
  expect_true(all(grepl("[.]LSE$", zero)))
})
