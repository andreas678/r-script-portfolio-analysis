## load_portfolio_configs() / build_complete_portfolio() (scripts/utils/portfolio_build.R).
source(here::here("scripts", "utils", "portfolio_build.R"))

no_rules <- tibble::tibble(ticker = character(), name_pattern = character(), alpha3 = character(),
                           new_ticker = character(), expected_currency = character(), note = character())

write_fund <- function(dir, file, ...) writexl::write_xlsx(tibble::tibble(...), file.path(dir, file))

cfg <- function(funds) {
  list(id = "p", display_name = "P", output_file = "p.xlsx",
       funds = tibble::tibble(file = names(funds), weight = unname(funds), label = names(funds)))
}

test_that("a fund's constituent weights are scaled by its allocation, self-weighted funds are kept as-is", {
  dir <- withr::local_tempdir()
  write_fund(dir, "Etf.xlsx", ticker = c("AAPL", "MSFT"), alpha3 = "USA", weight = c(60, 40))
  write_fund(dir, "Cash.xlsx", ticker = "CASH_EUR", alpha3 = "EUR", weight = 7)

  res <- build_complete_portfolio(cfg(c(Etf.xlsx = 0.5, Cash.xlsx = NA)), dir, no_rules)

  expect_equal(res$portfolio$weight[res$portfolio$ticker == "AAPL"], 30)   # 0.5 * 60
  expect_equal(res$portfolio$weight[res$portfolio$ticker == "CASH_EUR"], 7)
  expect_equal(sum(res$portfolio$weight), 30 + 20 + 7)
})

test_that("fund_data carries each fund's share: the allocation, or the sum of a self-weighted fund's weights / 100", {
  dir <- withr::local_tempdir()
  write_fund(dir, "Etf.xlsx", ticker = "AAPL", alpha3 = "USA", weight = 100)
  write_fund(dir, "Singles.xlsx", ticker = c("X", "Y"), alpha3 = "USA", weight = c(3, 2))

  res <- build_complete_portfolio(cfg(c(Etf.xlsx = 0.4, Singles.xlsx = NA)), dir, no_rules)

  expect_equal(res$fund_data$pct, c(0.4, 0.05))
})

test_that("the holdings filter drops non-positive weights and rows without ticker or country", {
  dir <- withr::local_tempdir()
  write_fund(dir, "F.xlsx", ticker = c("OK", "ZERO", "DASH", "NOCTRY", NA), alpha3 = c("USA", "USA", "USA", NA, "USA"),
             weight = c(1, 0, 1, 1, 1))
  dash_row <- which(c("OK", "ZERO", "DASH", "NOCTRY", NA) == "DASH")
  stopifnot(length(dash_row) == 1)
  writexl::write_xlsx(tibble::tibble(ticker = c("OK", "ZERO", "-", "NOCTRY", NA),
                                     alpha3 = c("USA", "USA", "USA", NA, "USA"),
                                     weight = c(1, 0, 1, 1, 1)), file.path(dir, "F.xlsx"))

  res <- build_complete_portfolio(cfg(c(F.xlsx = 1)), dir, no_rules)

  expect_equal(res$portfolio$ticker, "OK")
})

test_that("comma decimals in a fund file's weight column are read as numbers", {
  dir <- withr::local_tempdir()
  write_fund(dir, "F.xlsx", ticker = "AAPL", alpha3 = "USA", weight = "1,5")

  expect_equal(build_complete_portfolio(cfg(c(F.xlsx = 1)), dir, no_rules)$portfolio$weight, 1.5)
})

test_that("ticker disambiguation is applied while loading", {
  dir <- withr::local_tempdir()
  rules <- tibble::tibble(ticker = "AI", name_pattern = "LIQUIDE", alpha3 = NA_character_,
                          new_ticker = "AI.PA", expected_currency = NA_character_, note = NA_character_)
  write_fund(dir, "F.xlsx", ticker = "AI", name = "Air Liquide", alpha3 = "FRA", weight = 2)

  expect_equal(build_complete_portfolio(cfg(c(F.xlsx = 1)), dir, rules)$portfolio$ticker, "AI.PA")
})

test_that("a missing fund file is an error naming the portfolio and the file", {
  dir <- withr::local_tempdir()

  expect_error(build_complete_portfolio(cfg(c(Gone.xlsx = 1)), dir, no_rules),
               "Missing fund data file\\(s\\) for portfolio 'p': Gone.xlsx")
})

test_that("load_portfolio_configs groups funds per portfolio and rejects inconsistent display names", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("portfolio,display_name,output_file,fund_file,weight,label",
               "a,Alpha,a.xlsx,F1.xlsx,0.5,One",
               "a,Alpha,a.xlsx,F2.xlsx,,Two",
               "b,Beta,b.xlsx,F3.xlsx,1,Three"), path)

  cfgs <- load_portfolio_configs(path)

  expect_equal(names(cfgs), c("a", "b"))
  expect_equal(cfgs$a$id, "a")
  expect_equal(cfgs$a$funds$file, c("F1.xlsx", "F2.xlsx"))
  expect_true(is.na(cfgs$a$funds$weight[2]))

  bad <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("portfolio,display_name,output_file,fund_file,weight,label",
               "a,Alpha,a.xlsx,F1.xlsx,1,One", "a,Other,a.xlsx,F2.xlsx,1,Two"), bad)
  expect_error(load_portfolio_configs(bad), "inconsistent display_name/output_file")
})

## fund proxies (config/fund_proxies.csv)
proxy_row <- function(fund, proxy, exclude = "") {
  tibble::tibble(fund_file = fund, proxy_file = proxy, exclude_alpha3 = exclude, note = "")
}

test_that("build_proxy_holdings drops the excluded countries, renormalizes to 100 and relabels the index", {
  raw <- tibble::tibble(ticker = c("AAPL", "SAP", "TSM", "NVDA"), alpha3 = c("USA", "DEU", "TWN", "USA"),
                        weight = c(40, 20, 20, 20), index = "ACWI")

  out <- build_proxy_holdings(raw, "TWN KOR", index_label = "CoreWorld")

  expect_equal(out$ticker, c("AAPL", "SAP", "NVDA"))
  expect_equal(sum(out$weight), 100)
  expect_equal(out$weight[out$ticker == "AAPL"], 50)
  expect_true(all(out$index == "CoreWorld"))
})

test_that("build_proxy_holdings accepts comma separators and errors when nothing is left", {
  raw <- tibble::tibble(ticker = "TSM", alpha3 = "TWN", weight = 1, index = "x")

  expect_error(build_proxy_holdings(raw, "TWN", "y"), "empty after excluding")
  expect_equal(nrow(build_proxy_holdings(dplyr::bind_rows(raw, raw |> dplyr::mutate(alpha3 = "USA")),
                                         "TWN, KOR", "y")), 1)
})

test_that("a proxied fund is built from the proxy's holdings but keeps its own allocation and index", {
  dir <- withr::local_tempdir()
  write_fund(dir, "Swap.xlsx", ticker = "AMD", alpha3 = "USA", weight = 100, index = "Swap")
  write_fund(dir, "Acwi.xlsx", ticker = c("AAPL", "TSM"), alpha3 = c("USA", "TWN"), weight = c(80, 20), index = "Acwi")

  res <- build_complete_portfolio(cfg(c(Swap.xlsx = 0.2)), dir, no_rules,
                                  proxies = proxy_row("Swap.xlsx", "Acwi.xlsx", "TWN"))

  expect_equal(res$portfolio$ticker, "AAPL")
  expect_equal(res$portfolio$weight, 20)          # 0.2 * (renormalized 100)
  expect_equal(res$portfolio$index, "Swap")
  expect_equal(res$fund_data$proxy_file, "Acwi.xlsx")
})

test_that("funds without a proxy row are untouched", {
  dir <- withr::local_tempdir()
  write_fund(dir, "Etf.xlsx", ticker = "AAPL", alpha3 = "USA", weight = 100, index = "Etf")

  res <- build_complete_portfolio(cfg(c(Etf.xlsx = 1)), dir, no_rules,
                                  proxies = proxy_row("Other.xlsx", "Acwi.xlsx"))

  expect_equal(res$portfolio$ticker, "AAPL")
  expect_true(is.na(res$fund_data$proxy_file))
})

test_that("a missing proxy file warns and falls back to the published holdings", {
  dir <- withr::local_tempdir()
  write_fund(dir, "Swap.xlsx", ticker = "AMD", alpha3 = "USA", weight = 100, index = "Swap")

  expect_warning(
    res <- build_complete_portfolio(cfg(c(Swap.xlsx = 1)), dir, no_rules,
                                    proxies = proxy_row("Swap.xlsx", "Gone.xlsx")),
    "Proxy file Gone.xlsx"
  )
  expect_equal(res$portfolio$ticker, "AMD")
})

test_that("no proxy file means no proxies", {
  expect_equal(nrow(load_fund_proxies(file.path(withr::local_tempdir(), "nope.csv"))), 0)
})
