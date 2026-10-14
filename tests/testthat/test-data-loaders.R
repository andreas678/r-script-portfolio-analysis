## Tests for scripts/utils/data_loaders.R — the two helper functions shared
## by 6 of the 8 per-fund cleaner scripts (DEVELOPMENT-PROPOSAL-2026-09-27.md
## §3.1, item 2).
##
## Scope, deliberately: this tests the *shared helpers* the cleaners depend
## on, not each cleaner script end-to-end — the cleaners are top-level
## scripts (not functions) with hardcoded data/raw/ paths and real EODHD
## calls for any ISIN not already cached, neither of which is practical to
## fixture per-script. Testing data_loaders.R's read_holdings_table_by_isin()
## (pure) and resolve_and_upsert_tickers() (API calls only for
## not-yet-cached ISINs, so fully exercisable offline when every ISIN in the
## fixture is pre-resolved) covers the actual shared logic and the failure
## mode the proposal names: "raw export column names changed" /
## unresolvable ISINs.
##
## resolve_and_upsert_tickers()'s own network path (get_ticker_data_from_isin)
## is intentionally not exercised here — no EODHD calls in this suite, same
## reasoning as multi-agent's §2.3 test suite.

source(here::here("scripts", "utils", "data_loaders.R"))

test_that("read_holdings_table_by_isin finds the header row and drops non-ISIN rows", {
  raw_path <- withr::local_tempfile(fileext = ".xlsx")
  write_fixture_raw_export(raw_path)

  df <- read_holdings_table_by_isin(raw_path, weight_col = "Gewichtung")

  expect_equal(names(df), c("ISIN", "Name", "Gewichtung"))
  expect_equal(nrow(df), 3)  # the malformed-ISIN row and the meta/footer rows are dropped
  expect_true(all(stringr::str_detect(df$ISIN, "^[A-Z]{2}[A-Z0-9]{9}[0-9]$")))
  expect_setequal(df$ISIN, c("IE00B4L5Y983", "US0378331005", "DE0007164600"))
})

test_that("read_holdings_table_by_isin errors clearly when no matching header exists", {
  raw_path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(Sheet1 = tibble::tibble(V1 = "no header here", V2 = NA_character_)),
                       raw_path, col_names = FALSE)

  expect_error(read_holdings_table_by_isin(raw_path, weight_col = "Gewichtung"),
               "no sheet")
})

test_that("resolve_and_upsert_tickers returns required_cols when every ISIN is already resolved", {
  raw_path <- withr::local_tempfile(fileext = ".xlsx")
  write_fixture_raw_export(raw_path)
  df <- read_holdings_table_by_isin(raw_path, weight_col = "Gewichtung")

  ticker_path <- withr::local_tempfile(fileext = ".xlsx")
  write_fixture_tickerliste(ticker_path, isins = df$ISIN)

  # api_key = "" would normally fail fast for *new* ISINs (see next test) —
  # here every ISIN in df is already in the fixture tickerliste, so no
  # resolution (and no API call) is needed at all.
  result <- resolve_and_upsert_tickers(df, ticker_path, api_key = "")

  expected_cols <- c("ISIN", "ticker", "alpha2", "alpha3", "countryname",
                      "companyname", "asset_class", "sector", "industry", "last_updated")
  expect_equal(names(result), expected_cols)
  expect_setequal(result$ISIN, df$ISIN)
  expect_false(any(is.na(result$ticker)))
})

test_that("resolve_and_upsert_tickers fails fast on new ISINs when EODHD_API_KEY is unset", {
  ticker_path <- withr::local_tempfile(fileext = ".xlsx")
  write_fixture_tickerliste(ticker_path, isins = character(0))

  df <- tibble::tibble(ISIN = "US0378331005", Name = "Apple Inc", Gewichtung = "5,10")

  expect_error(resolve_and_upsert_tickers(df, ticker_path, api_key = ""),
               "EODHD_API_KEY is not set")
})

test_that("resolve_and_upsert_tickers backfills missing required_cols as NA", {
  ticker_path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(tibble::tibble(ISIN = "US0378331005", ticker = "AAPL"), ticker_path)

  df <- tibble::tibble(ISIN = "US0378331005", Name = "Apple Inc", Gewichtung = "5,10")
  result <- resolve_and_upsert_tickers(df, ticker_path, api_key = "")

  expected_cols <- c("ISIN", "ticker", "alpha2", "alpha3", "countryname",
                      "companyname", "asset_class", "sector", "industry", "last_updated")
  expect_equal(names(result), expected_cols)
  expect_equal(result$ticker, "AAPL")
  expect_true(is.na(result$sector))
})

test_that("resolve_and_upsert_tickers keeps extra columns such as sector_orig", {
  ticker_path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    tibble::tibble(ISIN = "US0378331005", ticker = "AAPL", sector = "Information Technology",
                   sector_orig = "Technology"),
    ticker_path
  )
  df <- tibble::tibble(ISIN = "US0378331005", Name = "Apple Inc", Gewichtung = "5,10")

  result <- resolve_and_upsert_tickers(df, ticker_path, api_key = "")
  on_disk <- readxl::read_excel(ticker_path)

  expect_true("sector_orig" %in% names(result))
  expect_equal(result$sector_orig, "Technology")
  expect_equal(on_disk$sector_orig, "Technology")
  # required columns still lead, in their documented order
  expect_equal(names(result)[1:10], c("ISIN", "ticker", "alpha2", "alpha3", "countryname",
                                      "companyname", "asset_class", "sector", "industry", "last_updated"))
})
