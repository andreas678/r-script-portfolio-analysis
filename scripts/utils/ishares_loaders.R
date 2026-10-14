library(tidyverse)
library(readr)
library(xml2)
library(countrycode)

source(here::here("scripts", "utils", "logging.R"))

# German to English country name mapping - iShares' CSV/XML exports return
# German country names, countrycode expects English names. Union of the
# country sets seen across iSharesEMIMI/iSharesACWI (emerging + a few
# developed markets) and iSharesEuro600 (Western Europe) - shared across all
# three funds rather than duplicated per fund, since an unused row here is a
# harmless no-op in the left_join.
country_mapping <- tribble(
  ~german, ~english,
  "Indien", "India",
  "Deutschland", "Germany",
  "Brasilien", "Brazil",
  "Südafrika", "South Africa",
  "Saudi-Arabien", "Saudi Arabia",
  "Mexiko", "Mexico",
  "Ungarn", "Hungary",
  "Irland", "Ireland",
  "Polen", "Poland",
  "Ver. Arabische Emirate", "United Arab Emirates",
  "Vereinigte Staaten", "United States",
  "Indonesien", "Indonesia",
  "Philippinen", "Philippines",
  "Griechenland", "Greece",
  "Türkei", "Turkey",
  "Kolumbien", "Colombia",
  "Ägypten", "Egypt",
  "Tschechien", "Czechia",
  "Kanada", "Canada",
  "Singapur", "Singapore",
  "Russland", "Russia",
  "Vereinigtes Königreich", "United Kingdom",
  "Schweiz", "Switzerland",
  "Belgien", "Belgium",
  "Dänemark", "Denmark",
  "Finnland", "Finland",
  "Frankreich", "France",
  "Italien", "Italy",
  "Niederlande", "Netherlands",
  "Norwegen", "Norway",
  "Österreich", "Austria",
  "Portugal", "Portugal",
  "Schweden", "Sweden",
  "Spanien", "Spain",
  "Australien", "Australia"
)

# EODHD exchange suffix per iShares "Börse" (exchange) value. Bare local
# tickers (e.g. "2330", "000660") are ambiguous across exchanges - EODHD's
# EOD endpoint requires the exchange-qualified form. Verified directly
# against the live EOD endpoint (not iShares/Yahoo naming, which differs -
# e.g. Korea is ".KO" here, not the Yahoo convention ".KS"). US exchanges
# map to NA since EODHD's default (no suffix) already resolves those.
# Exchanges not listed here are left unmapped (ticker stays bare) rather
# than guessed.
exchange_suffix_map <- tribble(
  ~Börse,                                  ~suffix,
  "Taiwan Stock Exchange",                 "TW",
  "Korea Exchange (Stock Market)",         "KO",
  "Korea Exchange (Kosdaq)",               "KQ",
  "Hong Kong Exchanges And Clearing Ltd",  "HK",
  "NASDAQ",                                NA_character_,
  "New York Stock Exchange Inc.",          NA_character_
)

# Imports ETF holdings from an iShares "fileType=csv" export - the plain-CSV
# shape shared by iSharesEMIMI and iSharesEuro600 (iSharesACWI's fund only
# offers a "fileType=xls" export that's actually SpreadsheetML XML in
# disguise, so it keeps its own importer in iSharesACWI.R).
import_ishares_csv <- function(url, indexName) {
  read_csv(url, skip = 2, show_col_types = FALSE) %>%
    rename(
      weight = "Gewichtung (%)",
      country = "Standort",
      ticker = "Emittententicker"
    ) %>%
    mutate(weight = as.double(sub(",", ".", weight, fixed = TRUE))) %>%
    add_column(index = indexName)
}

# Translates a raw iShares df's German country names to English via
# `country_map`, derives alpha2/alpha3/countryname from that, and appends
# the EODHD exchange suffix (e.g. "2330" -> "2330.TW") via `suffix_map` so
# downstream price fetches hit the correct listing instead of a bare,
# exchange-ambiguous code. Leaves the `suffix` column in the result (still
# needed by report_unmapped_exchanges()) - drop it with select(-suffix)
# once that's been called.
translate_country_and_suffix <- function(df, country_map = country_mapping, suffix_map = exchange_suffix_map) {
  df %>%
    left_join(country_map, by = c("country" = "german")) %>%
    left_join(suffix_map, by = "Börse") %>%
    mutate(
      country_en = coalesce(english, country),  # Use English translation if available, else keep original
      alpha2 = countrycode(country_en, "country.name", "iso2c"),
      alpha3 = countrycode(country_en, "country.name", "iso3c"),
      countryname = countrycode(country_en, "country.name", "country.name"),
      ticker = if_else(!is.na(suffix), paste(ticker, suffix, sep = "."), ticker)
    ) %>%
    select(-english, -country_en)
}

# Surfaces exchanges with no EODHD suffix mapped yet (ticker stays bare for
# these) so gaps are visible instead of silently wrong, both to the console
# and to the pipeline issue log. Call after translate_country_and_suffix(),
# before dropping its `suffix` column.
report_unmapped_exchanges <- function(df, suffix_map = exchange_suffix_map) {
  unmapped_exchanges <- df %>%
    filter(!is.na(weight), weight > 0, is.na(suffix), !Börse %in% suffix_map$Börse) %>%
    distinct(Börse) %>%
    pull(Börse)

  if (length(unmapped_exchanges) > 0) {
    cat("\nNote: no EODHD exchange suffix mapped for:\n -",
        paste(unmapped_exchanges, collapse = "\n - "),
        "\nTickers on these exchanges are kept bare and may fail to fetch price/volatility data.\n\n")
    log_pipeline_issue(paste0("No EODHD exchange suffix mapped for: ",
                               paste(unmapped_exchanges, collapse = ", "),
                               " - tickers on these exchanges are kept bare and may fail to fetch price/volatility data."))
  }
}

# ============================================================================
# import_ishares (XML/SpreadsheetML export)
# ============================================================================
# Imports an iShares holdings export in the SpreadsheetML ("xls") format. The
# header row is found by its first cell ("Emittententicker"); only rows with the
# same cell count are kept as data. iSharesACWI.R still carries its own copy of
# the original ("Positionen" sheet, old download URL) version of this function.
#
# Args:
#   url: download URL
#   indexName: value of the `index` column
#   sheet: worksheet holding the holdings: "Positionen" in the old export, "Holdings"
#     in the workbook of the product page's current download API (which also has no
#     per-holding country (`Standort`) or exchange (`Börse`) column - the caller must
#     add them; `country` is then NA)
#
# Returns: tibble with the export's columns, `weight` (percent, numeric; the new
#   export uses a decimal comma), `ticker`, `country` and `index`.

import_ishares <- function(url, indexName, sheet = "Positionen") {
  parse_ishares_workbook(httr::content(httr::GET(url), as = "raw"), indexName, sheet)
}

# The pure parsing half of import_ishares() (takes the downloaded bytes), so it can
# be tested without a network call.
parse_ishares_workbook <- function(raw, indexName, sheet = "Positionen") {
  bom <- as.raw(c(0xef, 0xbb, 0xbf))
  while (length(raw) >= 3 && identical(raw[1:3], bom)) raw <- raw[-(1:3)]

  doc <- read_xml(raw)
  ns <- xml_ns(doc)
  worksheet <- xml_find_first(doc, sprintf(".//ss:Worksheet[@ss:Name='%s']", sheet), ns)
  if (inherits(worksheet, "xml_missing")) stop("iShares export has no worksheet named '", sheet, "'")
  rows <- xml_find_all(worksheet, "./ss:Table/ss:Row", ns)

  row_list <- lapply(rows, function(row) {
    cells <- xml_find_all(row, "./ss:Cell", ns)
    vapply(cells, function(cell) {
      data_node <- xml_find_first(cell, "./ss:Data", ns)
      if (inherits(data_node, "xml_missing")) NA_character_ else xml_text(data_node)
    }, character(1))
  })

  header_idx <- which(vapply(row_list, function(r) length(r) > 0 && !is.na(r[1]) && r[1] == "Emittententicker", logical(1)))[1]
  if (is.na(header_idx)) stop("iShares export: header row ('Emittententicker') not found in sheet '", sheet, "'")
  header <- row_list[[header_idx]]
  data_rows <- row_list[(header_idx + 1):length(row_list)]
  data_rows <- Filter(function(r) length(r) == length(header), data_rows)

  df <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)
  names(df) <- header

  out <- as_tibble(df) %>%
    rename(weight = "Gewichtung (%)", ticker = "Emittententicker") %>%
    mutate(weight = as.double(sub(",", ".", weight, fixed = TRUE)))
  if ("Standort" %in% names(out)) out <- rename(out, country = "Standort") else out$country <- NA_character_
  add_column(out, index = indexName)
}


# ============================================================================
# Current iShares download API + country/exchange enrichment
# ============================================================================
# The old ".../<id>/<slug>/1478358465952.ajax?fileType=..." links return 404 since
# 2026-10. The product page's current download API
# (blackrock.com/varnish-api/.../get-fund-document, portfolioId = the product id in
# the page URL) returns a SpreadsheetML workbook whose "Holdings" sheet has ticker,
# name, sector, asset class, weight and currency, but NO per-holding country
# (`Standort`) and NO exchange (`Börse`), which the pipeline needs for alpha3 and for
# the EODHD exchange suffix. add_country_exchange() fills both from earlier exports.

ishares_download_url <- function(portfolio_id) {
  paste0("https://www.blackrock.com/varnish-api/uk-retail01-product-data/product-data/api/v1/get-fund-document",
         "?appType=PRODUCT_PAGE&appSubType=ISHARES&targetSite=de-ishares-v2&locale=de_DE&portfolioId=",
         portfolio_id, "&component=fundDownloadV2&userType=individual")
}

# Fills `country` and `Börse` where missing, from donor exports (tibbles with ticker,
# Name, country, Börse), in this order: (1) values the export already has,
# (2) a donor row with the same ticker AND name (first donor wins), (3) a donor
# ticker that maps to exactly ONE (country, exchange) pair across all donors - a bare
# ticker such as "ROP" can mean different companies on different exchanges, so an
# ambiguous ticker is left unmatched rather than guessed.
#
# Returns df with `country`/`Börse` filled; attribute "unmatched_weight" = the weight
# (percent) of rows still without a country or exchange.
add_country_exchange <- function(df, donors) {
  empty_donor <- tibble::tibble(ticker = character(), Name = character(), country = character(), `Börse` = character())
  donor <- dplyr::bind_rows(c(list(empty_donor), donors)) %>%   # the template keeps the columns for an empty list
    filter(!is.na(country), !is.na(`Börse`)) %>%
    select(ticker, Name, country, `Börse`)
  by_name <- donor %>%
    distinct(ticker, Name, .keep_all = TRUE) %>%
    rename(country_n = country, boerse_n = `Börse`)
  by_ticker <- donor %>%
    group_by(ticker) %>%
    filter(n_distinct(country, `Börse`) == 1) %>%
    slice(1) %>%
    ungroup() %>%
    select(ticker, country_t = country, boerse_t = `Börse`) %>%
    distinct(ticker, .keep_all = TRUE)

  if (!"country" %in% names(df)) df$country <- NA_character_
  if (!"Börse" %in% names(df)) df$`Börse` <- NA_character_

  out <- df %>%
    left_join(by_name, by = c("ticker", "Name")) %>%
    left_join(by_ticker, by = "ticker") %>%
    mutate(
      country = coalesce(country, country_n, country_t),
      `Börse` = coalesce(`Börse`, boerse_n, boerse_t)
    ) %>%
    select(-country_n, -boerse_n, -country_t, -boerse_t)

  attr(out, "unmatched_weight") <- sum(out$weight[is.na(out$country) | is.na(out$`Börse`)], na.rm = TRUE)
  out
}

# Reads the raw export of an earlier run (data/raw/<file>) as a donor; an absent file
# yields an empty donor.
read_ishares_donor <- function(file, raw_dir = here::here("data", "raw")) {
  path <- file.path(raw_dir, file)
  empty <- tibble::tibble(ticker = character(), Name = character(), country = character(), `Börse` = character())
  if (!file.exists(path)) return(empty)
  d <- readxl::read_excel(path)
  if (!all(c("ticker", "Name", "country", "Börse") %in% names(d))) return(empty)
  d %>% select(ticker, Name, country, `Börse`) %>% mutate(across(everything(), as.character))
}

# The standard donor list for fund `own_file`: its own previous raw export first (so
# names stay mapped as before), then the other iShares exports, which carry
# country/exchange for most of the same securities.
ishares_donors <- function(own_file, raw_dir = here::here("data", "raw")) {
  files <- unique(c(own_file, "iSharesACWI.xlsx", "iSharesEMIMI.xlsx", "iSharesEuro600.xlsx", "iSharesWorld.xlsx"))
  lapply(files, read_ishares_donor, raw_dir = raw_dir)
}

# Imports one fund through the current API: equities only (the export also lists
# money market, cash collateral, FX and futures), country/exchange added from the
# donors. Prints how much weight could not be matched (it is dropped later by the
# portfolio's alpha3 filter).
import_ishares_fund <- function(portfolio_id, indexName, own_file, raw_dir = here::here("data", "raw")) {
  donors <- ishares_donors(own_file, raw_dir)   # read BEFORE the caller overwrites own_file
  raw <- import_ishares(ishares_download_url(portfolio_id), indexName, sheet = "Holdings") %>%
    filter(Anlageklasse == "Aktien")
  out <- add_country_exchange(raw, donors)
  total <- sum(out$weight, na.rm = TRUE)
  um <- attr(out, "unmatched_weight")
  cat(sprintf("%s: country/exchange found for %.2f of %.2f %% of the weight; %.2f %% unmatched (dropped later)\n",
              indexName, total - um, total, um))
  out
}
