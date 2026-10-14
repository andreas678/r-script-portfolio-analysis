## scripts/utils/exchange_suffix.R — offline, rules/known symbols passed in.
source(here::here("scripts", "utils", "exchange_suffix.R"))

hold <- tibble(
  ticker = c("HSBA", "NOVN", "AAPL", "SAP.XETRA", "NESN", "ABB", "ABB", "-", NA, "SAN"),
  name   = c("HSBC Holdings", "Novartis", "Apple", "SAP", "Nestle", "ABB Ltd", "ABB Inc",
             "Cash", "x", "Santander"),
  alpha3 = c("GBR", "CHE", "USA", "DEU", "CHE", "CHE", "CHE", "CHE", "GBR", "ESP"),
  weight = c(2.3, 1.8, 5, 1.4, 1.5, 0.5, 0.4, 0.1, 0.1, 1)
)

test_that("only bare non-US tickers of mapped countries are proposed", {
  out <- suggest_exchange_suffixes(hold)
  expect_setequal(out$ticker, c("HSBA", "NOVN", "NESN", "ABB", "SAN"))
})

test_that("suffix and currency come from the country", {
  out <- suggest_exchange_suffixes(hold)
  expect_equal(out$suggested[out$ticker == "HSBA"], "HSBA.LSE")
  expect_equal(out$expected_currency[out$ticker == "NOVN"], "CHF")
  expect_equal(out$suggested[out$ticker == "SAN"], "SAN.MC")
})

test_that("known symbols confirm, several names under one ticker are ambiguous", {
  out <- suggest_exchange_suffixes(hold, known_symbols = "HSBA.LSE")
  expect_equal(out$status[out$ticker == "HSBA"], "confirmed")
  expect_equal(out$status[out$ticker == "NOVN"], "unverified")
  expect_equal(out$status[out$ticker == "ABB"], "ambiguous")
  expect_equal(out$weight[out$ticker == "ABB"], 0.9)
})

test_that("leading-zero codes are flagged rather than suffixed", {
  out <- suggest_exchange_suffixes(tibble(ticker = "0RLS", name = "UniCredit", alpha3 = "FRA", weight = 1))
  expect_equal(out$status, "odd_code")
  expect_equal(nrow(proposals_to_rules(out)), 0)
})

test_that("sorted by weight; bare currency reported when a map is given", {
  out <- suggest_exchange_suffixes(hold, symbol_currency = c(HSBA = "USD"))
  expect_equal(out$ticker[1], "HSBA")
  expect_equal(out$bare_currency[out$ticker == "HSBA"], "USD")
})

test_that("tickers that already have a rule are not proposed again, even keep-bare ones", {
  rules <- tibble(ticker = "NOVN", name_pattern = "NOVARTIS", alpha3 = NA_character_,
                  new_ticker = "NOVN.SW", expected_currency = NA_character_, note = NA_character_)
  expect_false("NOVN" %in% suggest_exchange_suffixes(hold, rules)$ticker)
  keep <- mutate(rules, ticker = "HSBA", new_ticker = "HSBA")
  expect_false("HSBA" %in% suggest_exchange_suffixes(hold, keep)$ticker)
})

test_that("empty and no-match input give an empty result", {
  expect_equal(nrow(suggest_exchange_suffixes(hold[0, ])), 0)
  expect_equal(nrow(suggest_exchange_suffixes(hold[hold$alpha3 == "USA", ])), 0)
})

test_that("rules skip ambiguous rows and keep other countries' same ticker", {
  p <- suggest_exchange_suffixes(hold, known_symbols = "HSBA.LSE")
  master <- tibble(ticker = c("HSBA", "ABB", "NOVN"), alpha3 = c("GBR", "USA", "CHE"))
  r <- proposals_to_rules(p, master)
  expect_false("ABB" %in% r$new_ticker)
  expect_true(all(c("HSBA.LSE", "NOVN.SW") %in% r$new_ticker))
  keep <- r[is.na(r$new_ticker), ]
  expect_equal(nrow(keep), 0)   # ABB is ambiguous -> no rule -> no keep row
  p2 <- tibble(ticker = "ABB", alpha3 = "CHE", name = "ABB Ltd", n_names = 1L, weight = 1,
               suggested = "ABB.SW", expected_currency = "CHF", bare_currency = NA_character_,
               status = "unverified")
  keep2 <- proposals_to_rules(p2, master)
  expect_equal(keep2$alpha3[is.na(keep2$new_ticker)], "USA")
})

test_that("generated rules fire through apply_ticker_disambiguation only for the country", {
  rules <- proposals_to_rules(suggest_exchange_suffixes(hold), tibble(ticker = "HSBA", alpha3 = "USA"))
  df <- tibble(ticker = c("HSBA", "HSBA"), name = c("HSBC", "Some US Co"), alpha3 = c("GBR", "USA"))
  expect_equal(apply_ticker_disambiguation(df, rules)$ticker, c("HSBA.LSE", "HSBA"))
})

test_that("appending adds only new rules and keeps existing rows", {
  path <- withr::local_tempfile(fileext = ".csv")
  write.csv(tibble(ticker = "AI", name_pattern = "LIQUIDE", alpha3 = NA_character_,
                   new_ticker = "AI.PA", expected_currency = "EUR", note = "x"),
            path, row.names = FALSE, na = "")
  new <- tibble(ticker = c("AI", "HSBA"), name_pattern = ".", alpha3 = c(NA, "GBR"),
                new_ticker = c("AI.X", "HSBA.LSE"), expected_currency = NA, note = "n")
  expect_equal(append_ticker_disambiguation_rules(new, path), 2L)
  expect_equal(append_ticker_disambiguation_rules(new, path), 0L)
  back <- load_ticker_disambiguation(path)
  expect_equal(back$new_ticker, c("AI.PA", "AI.X", "HSBA.LSE"))
})
