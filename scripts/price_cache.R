## Central Price Cache Reader
##
## Single source of truth for reading meta/closing_prices.csv. Every script
## or report chunk that needs cached daily prices should call
## load_cached_prices() rather than re-implementing the read/clean logic —
## previously this was duplicated (with subtly different behavior) across
## portfolio_report.Rmd, correlation_analysis.R, and get_volatility_data.R.
##
## Also the single source of truth for which tickers are cryptocurrencies
## (get_crypto_tickers()) — get_price_data.R and get_volatility_data.R both
## need to treat a 24/7 asset differently (fetch symbol format, annualization
## day-count) and must agree on which tickers those are.
##
## Pure/side-effect-free: safe to source() from anywhere.

library(tidyverse)
library(here)
library(readxl)

PRICE_CACHE_FILE <- "closing_prices.csv"
TICKERLISTE_FILE <- "tickerliste.xlsx"

# ============================================================================
# FUNCTION: load_cached_prices
# ============================================================================
# Reads and cleans the price cache into a canonical shape.
#
# Args:
#   prices_file: filename within meta/ (default: closing_prices.csv)
#
# Returns:
#   tibble(ticker, Date, Adjusted_close) — deduped on (ticker, Date), sorted.
#   Returns a zero-row tibble with the right columns if the cache is missing,
#   so callers can rely on the schema without checking file.exists() first.

load_cached_prices <- function(prices_file = PRICE_CACHE_FILE) {
  prices_path <- here::here("meta", prices_file)

  if (!file.exists(prices_path)) {
    return(tibble(
      ticker = character(),
      Date = as.Date(character()),
      Adjusted_close = double()
    ))
  }

  raw <- read.csv(prices_path, stringsAsFactors = FALSE)

  # fetch_single_ticker_prices() (get_price_data.R) only ever writes `Date`.
  # This coalesce exists solely to tolerate a pre-existing closing_prices.csv
  # written by an older, buggy version of that function which left a stray,
  # all-NA lowercase `date` column — not an ongoing dual schema.
  lower_date <- if ("date" %in% names(raw)) as.Date(raw$date) else as.Date(NA)

  clean <- raw |>
    mutate(Date = dplyr::coalesce(as.Date(Date), lower_date)) |>
    transmute(ticker, Date, Adjusted_close = suppressWarnings(as.numeric(Adjusted_close))) |>
    filter(!is.na(Date), !is.na(ticker))

  # A non-numeric Adjusted_close (e.g. a corrupted cell from a manual CSV
  # edit) silently becomes NA above; drop those rows too, but say so, since
  # this is the canonical loader every other script trusts without
  # re-checking.
  bad_price <- is.na(clean$Adjusted_close)
  if (any(bad_price)) {
    warning(sum(bad_price), " row(s) in ", prices_file,
            " had a non-numeric Adjusted_close and were dropped: ",
            paste(unique(clean$ticker[bad_price]), collapse = ", "))
  }

  clean |>
    filter(!bad_price) |>
    arrange(ticker, Date) |>
    distinct(ticker, Date, .keep_all = TRUE)
}


# ============================================================================
# FUNCTION: get_crypto_tickers
# ============================================================================
# Returns the tickers classified as "Cryptocurrency" in the asset_class
# column of meta/tickerliste.xlsx.
#
# Why this matters: a cryptocurrency trades every calendar day, not just the
# ~252 trading days/year equities do. Fetching it through an equity-oriented
# API path and/or annualizing its volatility with sqrt(252) both silently
# assume it's an equity. get_price_data.R uses this to fetch crypto tickers
# via EODHD's "<SYMBOL>-USD.CC" pair format instead of the bare equity
# symbol; get_volatility_data.R uses it to annualize with sqrt(365) instead
# of sqrt(252). Centralized here so the two scripts can't drift on which
# tickers count as crypto.
#
# Args:
#   tickerliste_file: filename within meta/ (default: tickerliste.xlsx)
#
# Returns:
#   character vector of tickers (possibly empty). Never errors — a missing
#   file or missing asset_class column just yields no crypto tickers, since
#   that's the safe (equity-like) fallback for everything already working.

get_crypto_tickers <- function(tickerliste_file = TICKERLISTE_FILE) {
  path <- here::here("meta", tickerliste_file)
  if (!file.exists(path)) return(character())

  tl <- read_excel(path)
  if (!"asset_class" %in% names(tl)) return(character())

  tl |>
    filter(asset_class == "Cryptocurrency") |>
    pull(ticker) |>
    unique()
}
