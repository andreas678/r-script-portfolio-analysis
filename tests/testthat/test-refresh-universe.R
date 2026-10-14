## read_refresh_holdings() / refresh_candidates() (scripts/get_price_data.R).
## Temp data dir, no EODHD calls.
source(here::here("scripts", "get_price_data.R"))

no_rules <- tibble::tibble(ticker = character(), name_pattern = character(), alpha3 = character(),
                           new_ticker = character(), expected_currency = character(), note = character())

write_holdings <- function(dir, file, ...) {
  writexl::write_xlsx(tibble::tibble(...), file.path(dir, file))
}

# A one-fund portfolio config, in the shape load_portfolio_configs() returns.
cfg_for <- function(id, output_file, funds) {
  list(id = id, display_name = id, output_file = output_file,
       funds = tibble::tibble(file = names(funds), weight = unname(funds), label = names(funds)))
}

test_that("the universe is built from the fund files, not from the last rendered scenario's output", {
  dir <- withr::local_tempdir()
  write_holdings(dir, "Equities.xlsx", ticker = "AAPL", alpha3 = "USA", weight = 50)
  write_holdings(dir, "Gold.xlsx", ticker = "GLD", alpha3 = "GOLD", weight = 20)
  # what a render with a no-gold scenario would have left behind
  write_holdings(dir, "portfolio.xlsx", ticker = "AAPL", alpha3 = "USA", weight = 50)
  cfg <- cfg_for("p", "portfolio.xlsx", c(Equities.xlsx = 0.5, Gold.xlsx = NA))

  expect_no_warning(tickers <- read_refresh_holdings(list(cfg), 0.1, no_rules, dir))
  expect_setequal(tickers, c("AAPL", "GLD"))
})

test_that("it needs no rendered output file at all when the fund files exist", {
  dir <- withr::local_tempdir()
  write_holdings(dir, "Equities.xlsx", ticker = "AAPL", alpha3 = "USA", weight = 50)

  tickers <- read_refresh_holdings(list(cfg_for("p", "portfolio.xlsx", c(Equities.xlsx = 1))), 0.1, no_rules, dir)

  expect_equal(tickers, "AAPL")
})

test_that("a portfolio with missing fund files falls back to its output file and says so", {
  dir <- withr::local_tempdir()
  write_holdings(dir, "portfolio.xlsx", ticker = "AAPL", alpha3 = "USA", weight = 5)
  cfg <- cfg_for("p", "portfolio.xlsx", c(Missing.xlsx = 1))

  expect_warning(tickers <- read_refresh_holdings(list(cfg), 0.1, no_rules, dir),
                 "Missing fund data file.*Missing.xlsx.*LAST RENDERED SCENARIO")
  expect_equal(tickers, "AAPL")
})

test_that("it errors clearly when neither fund files nor an output file exist for any portfolio", {
  dir <- withr::local_tempdir()

  expect_error(read_refresh_holdings(list(cfg_for("p", "portfolio.xlsx", c(Missing.xlsx = 1))), 0.1, no_rules, dir),
               "Neither the fund files nor a rendered output file")
})

test_that("one portfolio can fall back while another is built from its fund files", {
  dir <- withr::local_tempdir()
  write_holdings(dir, "A.xlsx", ticker = "AAPL", alpha3 = "USA", weight = 5)
  write_holdings(dir, "b_out.xlsx", ticker = "MSFT", alpha3 = "USA", weight = 5)

  expect_warning(
    tickers <- read_refresh_holdings(list(cfg_for("a", "a_out.xlsx", c(A.xlsx = 1)),
                                          cfg_for("b", "b_out.xlsx", c(Gone.xlsx = 1))), 0.1, no_rules, dir),
    "portfolio 'b'"
  )
  expect_setequal(tickers, c("AAPL", "MSFT"))
})

test_that("rows below the weight threshold are dropped after scaling by the fund's allocation", {
  dir <- withr::local_tempdir()
  write_holdings(dir, "F.xlsx", ticker = c("BIG", "SMALL", "-", "NOCTRY"), alpha3 = c("USA", "USA", "USA", NA),
                 weight = c(10, 0.4, 10, 10))
  cfg <- cfg_for("p", "p.xlsx", c(F.xlsx = 0.5))  # 10 -> 5, 0.4 -> 0.2, threshold 0.3

  expect_equal(read_refresh_holdings(list(cfg), 0.3, no_rules, dir), "BIG")
})

test_that("ticker disambiguation is applied and tickers are unioned across portfolios", {
  dir <- withr::local_tempdir()
  rules <- tibble::tibble(ticker = "AI", name_pattern = "LIQUIDE", alpha3 = NA_character_,
                          new_ticker = "AI.PA", expected_currency = NA_character_, note = NA_character_)
  write_holdings(dir, "A.xlsx", ticker = c("AI", "AAPL"), name = c("Air Liquide", "Apple"),
                 alpha3 = c("FRA", "USA"), weight = c(1, 1))
  write_holdings(dir, "B.xlsx", ticker = c("AAPL", "MSFT"), name = c("Apple", "Microsoft"),
                 alpha3 = c("USA", "USA"), weight = c(1, 1))

  tickers <- read_refresh_holdings(list(cfg_for("a", "a.xlsx", c(A.xlsx = 1)),
                                        cfg_for("b", "b.xlsx", c(B.xlsx = 1))), 0.1, rules, dir)

  expect_setequal(tickers, c("AI.PA", "AAPL", "MSFT"))
})

history <- function(...) data.frame(..., stringsAsFactors = FALSE)

test_that("synthetic tickers are never fetched", {
  plan <- refresh_candidates(c("AAPL", CASH_TICKER, BONDS_LR_TICKER), NULL, cached_tickers = "AAPL")

  expect_equal(plan$fetch, "AAPL")
  expect_setequal(plan$skipped_synthetic, c(CASH_TICKER, BONDS_LR_TICKER))
})

test_that("a never-cached ticker is skipped after max_failed_attempts failures, not before", {
  md <- history(ticker = c("BAD", "BAD", "TWICE", "TWICE"), success = FALSE)

  plan <- refresh_candidates(c("BAD", "TWICE", "NEW"), md, cached_tickers = character(),
                             max_failed_attempts = 3)
  expect_equal(plan$fetch, c("BAD", "TWICE", "NEW"))       # BAD has 2 failures: still tried

  md3 <- history(ticker = rep("BAD", 3), success = FALSE)
  plan3 <- refresh_candidates(c("BAD", "NEW"), md3, cached_tickers = character())
  expect_equal(plan3$fetch, "NEW")
  expect_equal(plan3$skipped_failing, "BAD")
})

test_that("a ticker with cached prices is never skipped, however often it failed", {
  md <- history(ticker = rep("GLD", 10), success = FALSE)

  plan <- refresh_candidates("GLD", md, cached_tickers = "GLD")

  expect_equal(plan$fetch, "GLD")
  expect_length(plan$skipped_failing, 0)
})

test_that("successful attempts do not count towards the failure limit", {
  md <- history(ticker = rep("OK", 5), success = TRUE)

  expect_equal(refresh_candidates("OK", md, cached_tickers = character())$fetch, "OK")
})

test_that("string 'TRUE'/'FALSE' success values (as read back from the CSV) are handled", {
  md <- history(ticker = rep("BAD", 3), success = c("FALSE", "FALSE", "FALSE"))

  expect_equal(refresh_candidates("BAD", md, cached_tickers = character())$skipped_failing, "BAD")
})

test_that("a zero threshold keeps every positive-weight holding (currency map uses it)", {
  dir <- withr::local_tempdir()
  write_holdings(dir, "Equities.xlsx", ticker = c("AAPL", "TINY"), alpha3 = "USA", weight = c(50, 0.01))
  cfg <- cfg_for("p", "portfolio.xlsx", c(Equities.xlsx = 1))

  expect_equal(read_refresh_holdings(list(cfg), 0.1, no_rules, dir), "AAPL")
  expect_setequal(read_refresh_holdings(list(cfg), 0, no_rules, dir), c("AAPL", "TINY"))
})

# ---- refresh_tickers --------------------------------------------------------------------------
test_that("refresh_tickers sums a ticker's weight over the funds that hold it before applying the cut-off", {
  holdings <- tibble::tibble(ticker = c("SPLIT", "SPLIT", "SPLIT", "TINY", "BIG", NA),
                             weight = c(0.06, 0.06, 0.06, 0.05, 2, 1))

  expect_setequal(refresh_tickers(holdings, 0.1), c("SPLIT", "BIG"))   # 3 x 0.06 = 0.18 passes; TINY and NA do not
  expect_setequal(refresh_tickers(holdings, 0), c("SPLIT", "TINY", "BIG"))
})

test_that("the report's panel universe and the refresh universe share one cut-off", {
  expect_equal(PRICE_REFRESH_MIN_WEIGHT_PCT, 0.1)
})
