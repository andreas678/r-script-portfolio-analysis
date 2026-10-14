## Shared fixture builders for the testthat suite.
##
## xlsx fixtures are generated at test time (via writexl, into a tempfile)
## rather than committed as binary files, so their contents stay visible and
## diffable in this source file instead of living in an opaque .xlsx blob.
## testthat auto-loads every tests/testthat/helper*.R file before running
## tests (test_dir()'s load_helpers = TRUE default), so these are available
## in every test file without an explicit source().

## Writes a small raw fund-export-style xlsx mimicking what
## read_holdings_table_by_isin() (scripts/utils/data_loaders.R) is built to
## tolerate: a title/metadata row above the real header (no ISIN/weight_col
## in it), the ISIN/Name/weight table, one malformed-ISIN row embedded mid
## table, and a footer total row — none of which carry a valid ISIN, so
## read_holdings_table_by_isin()'s ISIN-format filter must strip all of them
## while keeping the three genuine holdings rows.
write_fixture_raw_export <- function(path, weight_col = "Gewichtung") {
  meta_row   <- tibble::tibble(V1 = "Stand: 2026-09-01", V2 = NA_character_, V3 = NA_character_)
  header_row <- tibble::tibble(V1 = "ISIN", V2 = "Name", V3 = weight_col)
  data_rows <- tibble::tribble(
    ~V1,             ~V2,                        ~V3,
    "IE00B4L5Y983",  "iShares Core MSCI World",  "45,20",
    "XX-NOT-AN-ISIN", "Malformed row",           "1,00",
    "US0378331005",  "Apple Inc",                "5,10",
    "DE0007164600",  "SAP SE",                   "3,00"
  )
  footer_row <- tibble::tibble(V1 = NA_character_, V2 = "Total", V3 = NA_character_)

  sheet <- dplyr::bind_rows(meta_row, header_row, data_rows, footer_row)
  writexl::write_xlsx(list(`01.09.2026` = sheet), path, col_names = FALSE)
}

## Writes a small tickerliste.xlsx fixture with `required_cols` (from
## resolve_and_upsert_tickers()) fully populated for the given ISINs, so a
## test can exercise the "already resolved, no API call needed" path
## offline. Extra ISINs (not in the fixture raw export) are fine — they're
## just unrelated existing rows.
write_fixture_tickerliste <- function(path, isins = c("IE00B4L5Y983", "US0378331005", "DE0007164600")) {
  df <- tibble::tibble(
    ISIN = isins,
    ticker = paste0("TCK", seq_along(isins)),
    alpha2 = "IE",
    alpha3 = "IRL",
    countryname = "Ireland",
    companyname = paste("Company", seq_along(isins)),
    asset_class = "Equity",
    sector = "Financials",
    industry = NA_character_,
    last_updated = as.character(Sys.Date())
  )
  writexl::write_xlsx(df, path)
}
