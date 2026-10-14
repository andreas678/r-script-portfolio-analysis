# Proposes EODHD home-exchange suffixes for the bare, non-US tickers of one
# cleaned fund file, offline (no API call). See scripts/utils/exchange_suffix.R.
#
# Usage: set FUND_FILE, run, review data/output/exchange_suffix_proposals_<fund>.csv,
# then set APPEND <- TRUE to add the rules to config/ticker_disambiguation.csv
# (applied at report time by load_etf(), so data/clean/ needs no regeneration).
# Afterwards run get_currency_map.R and get_price_data.R to fetch the prices.

library(tidyverse)
library(readxl)
library(here)

source(here::here("scripts", "utils", "exchange_suffix.R"))

FUND_FILE <- "AmundiCoreStoxxEurope600.xlsx"
APPEND    <- FALSE
STATUSES  <- c("confirmed", "unverified")   # "ambiguous" is never written

holdings <- read_excel(here::here("data", "clean", FUND_FILE))
master   <- read_excel(here::here("meta", "tickerliste.xlsx"), col_types = "text")
rules    <- load_ticker_disambiguation()

read_optional <- function(path, ...) if (file.exists(path)) read_csv(path, show_col_types = FALSE, ...) else NULL
currency_map <- read_optional(here::here("meta", "ticker_currency.csv"))
prices       <- read_optional(here::here("meta", "closing_prices.csv"), col_select = "ticker")

known <- unique(c(master$ticker, currency_map$ticker, prices$ticker))
symbol_currency <- if (!is.null(currency_map)) setNames(currency_map$currency, currency_map$ticker)

# ticker/country pairs of ALL cleaned fund files: a bare ticker that is also held
# in another country (e.g. MRK = Merck KGaA and Merck & Co) needs a keep rule there
fund_files <- list.files(here::here("data", "clean"), pattern = "\\.xlsx$", full.names = TRUE)
fund_files <- fund_files[!grepl("portfolio|Portfolio B|correlation", basename(fund_files))]
pairs <- map_dfr(fund_files, \(f) {
  d <- read_excel(f, col_types = "text")
  if (all(c("ticker", "alpha3") %in% names(d))) distinct(d, ticker, alpha3) else NULL
}) |> distinct()

proposals <- suggest_exchange_suffixes(holdings, rules, known, symbol_currency)

cat("\nBare non-US tickers:", nrow(proposals), "| weight", round(sum(proposals$weight), 2), "%\n")
print(proposals |> count(status, wt = weight, name = "weight"))
print(proposals |> select(ticker, name, alpha3, weight, suggested, bare_currency, status), n = 25)

out <- here::here("data", "output", paste0("exchange_suffix_proposals_", tools::file_path_sans_ext(FUND_FILE), ".csv"))
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
write_csv(proposals, out)
cat("Written:", out, "\n")

if (APPEND) {
  n <- append_ticker_disambiguation_rules(
    proposals_to_rules(proposals, pairs, STATUSES),
    here::here("config", "ticker_disambiguation.csv"))
  cat("Rules added to config/ticker_disambiguation.csv:", n, "\n")
}
