## Central Price Cache Reader
##
## Single source of truth for reading meta/closing_prices.csv. Every script
## or report chunk that needs cached daily prices should call
## load_cached_prices() rather than re-implementing the read/clean logic —
## previously this was duplicated (with subtly different behavior) across
## portfolio_report.Rmd, correlation_analysis.R, and get_volatility_data.R.
##
## Also the single source of truth for which tickers are cryptocurrencies
## (get_crypto_tickers()) — a 24/7 asset is fetched through a different symbol
## format (get_price_data.R) and gets its currency from the price source
## (get_currency_map.R), and those must agree on which tickers those are.
## periods_per_year() annualizes any series by its own quote frequency.
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
# FUNCTION: find_price_jumps
# ============================================================================
# Flags implausible single-day moves in a price series - the signature of two
# different instruments spliced into one ticker (e.g. a crypto series whose
# later rows were fetched under the bare equity symbol: 77,000 -> 35 overnight,
# which annualized to ~650 % volatility and made it "the" dominant risk in the
# report). A real move that large in one day is rare enough for a data-quality
# WARNING to be worth raising: none of the ~180 series cached at the time of
# writing exceeds it.
#
# Args:
#   prices: tibble(ticker, Date, Adjusted_close), any order
#   max_abs_log_return: threshold on |log(P_t / P_t-1)| (default 0.4 = -33 % / +49 %)
#
# Returns: tibble(ticker, Date, Adjusted_close, log_return), the offending days.

find_price_jumps <- function(prices, max_abs_log_return = 0.4) {
  prices |>
    arrange(ticker, Date) |>
    group_by(ticker) |>
    mutate(log_return = log(Adjusted_close / dplyr::lag(Adjusted_close))) |>
    ungroup() |>
    filter(!is.na(log_return), abs(log_return) > max_abs_log_return) |>
    select(ticker, Date, Adjusted_close, log_return)
}


# ============================================================================
# FUNCTION: load_fx_rates
# ============================================================================
# Reads the EUR foreign-exchange cache (meta/fx_rates_raw.csv, written by
# update_fx_cache() in get_price_data.R). The cache reuses the price-cache
# format with one "EUR<CCY>.FOREX" series per currency, so it goes through the
# same reader/cleaner as prices.
#
# Convention: rate = units of <CCY> per 1 EUR (EODHD's EURUSD = 1.13 means
# 1 EUR buys 1.13 USD), so a price in CCY converts to EUR by DIVIDING by rate.
#
# Args:
#   fx_file: filename within meta/ (default: fx_rates_raw.csv)
#
# Returns:
#   tibble(currency, Date, rate); zero rows if the cache is missing. EUR itself
#   is not stored (its rate is 1 by definition).

FX_CACHE_FILE <- "fx_rates_raw.csv"

load_fx_rates <- function(fx_file = FX_CACHE_FILE) {
  load_cached_prices(fx_file) |>
    filter(grepl("^EUR[A-Z]{3}\\.FOREX$", ticker)) |>
    transmute(currency = substr(ticker, 4, 6), Date, rate = Adjusted_close)
}


# ============================================================================
# FUNCTION: load_ticker_currency
# ============================================================================
# Reads meta/ticker_currency.csv (built by scripts/get_currency_map.R).
#
# Returns:
#   tibble(ticker, currency, price_scale); zero rows if the file is missing.
#   price_scale converts a raw quote to whole currency units (0.01 for pence).

TICKER_CURRENCY_FILE <- "ticker_currency.csv"

load_ticker_currency <- function(currency_file = TICKER_CURRENCY_FILE) {
  path <- here::here("meta", currency_file)
  if (!file.exists(path)) {
    return(tibble(ticker = character(), currency = character(), price_scale = double()))
  }
  read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA")) |>
    as_tibble() |>
    transmute(ticker, currency, price_scale = as.numeric(price_scale))
}


# ============================================================================
# FUNCTION: locf_fx_rate
# ============================================================================
# Looks up an FX rate for each date by carrying the last KNOWN rate forward
# (never a later one - that would use information from the future). FX series
# have no rate on some days a stock trades (weekends/holidays on either side).
#
# Args:
#   dates: Date vector to look rates up for
#   fx_dates, fx_rates: one currency's rate history (Date, numeric); any order
#   max_age_days: a carried-forward rate older than this many days is treated as
#     unknown, so a stale FX cache can't silently price today's data at an old rate
#
# Returns: numeric vector, NA where no rate exists on or before the date
#   (before the first rate) or the latest one is older than max_age_days.

locf_fx_rate <- function(dates, fx_dates, fx_rates, max_age_days = 7) {
  ord <- order(fx_dates)
  fx_dates <- as.numeric(fx_dates[ord])
  fx_rates <- fx_rates[ord]

  d   <- as.numeric(dates)
  idx <- findInterval(d, fx_dates)
  rate <- ifelse(idx > 0, fx_rates[pmax(idx, 1)], NA_real_)
  age  <- ifelse(idx > 0, d - fx_dates[pmax(idx, 1)], NA_real_)
  rate[!is.na(age) & age > max_age_days] <- NA_real_
  rate
}


# ============================================================================
# FUNCTION: load_cached_prices_eur
# ============================================================================
# load_cached_prices() converted to EUR: price_eur = price * price_scale / rate,
# with the rate carried forward from the last known FX rate (locf_fx_rate()).
# Use this - not the raw loader - for every return/volatility/correlation
# calculation: the cache holds each listing's LOCAL currency, so raw prices mix
# USD, HKD, TWD, KRW, ... and leave out the currency risk a EUR investor bears.
# The raw loader stays for update_price_cache(), which manages the cache itself.
#
# Rows that cannot be converted are DROPPED, never passed through as if they
# were EUR, and reported in one warning: tickers missing from the currency map
# or without a currency, currencies without any cached FX series, and
# individual dates with no usable rate (before the first FX date, or older
# than max_fx_age_days).
#
# Args:
#   prices_file, fx_file, currency_file: filenames within meta/
#   max_fx_age_days: see locf_fx_rate() (default 7)
#
# Returns:
#   tibble(ticker, Date, Adjusted_close) in EUR - exactly the raw loader's
#   shape (prices_to_wide() pivots on it, so no extra columns), sorted by
#   ticker, Date. The dropped rows are attached as attr(, "dropped").

load_cached_prices_eur <- function(prices_file = PRICE_CACHE_FILE, fx_file = FX_CACHE_FILE,
                                   currency_file = TICKER_CURRENCY_FILE, max_fx_age_days = 7) {
  prices <- load_cached_prices(prices_file)
  cur    <- load_ticker_currency(currency_file) |> distinct(ticker, .keep_all = TRUE)
  fx     <- load_fx_rates(fx_file)

  joined <- prices |>
    left_join(cur, by = "ticker") |>
    mutate(price_local = Adjusted_close * price_scale)

  fx_by_cur <- split(fx, fx$currency)

  joined$rate <- NA_real_
  is_eur <- !is.na(joined$currency) & joined$currency == "EUR"
  joined$rate[is_eur] <- 1

  for (ccy in setdiff(unique(joined$currency), c("EUR", NA))) {
    if (is.null(fx_by_cur[[ccy]])) next
    rows <- which(!is.na(joined$currency) & joined$currency == ccy)
    joined$rate[rows] <- locf_fx_rate(joined$Date[rows], fx_by_cur[[ccy]]$Date,
                                      fx_by_cur[[ccy]]$rate, max_fx_age_days)
  }

  usable <- !is.na(joined$rate)
  dropped <- joined[!usable, c("ticker", "Date", "currency")]

  if (nrow(dropped) > 0) {
    no_ccy   <- unique(dropped$ticker[is.na(dropped$currency)])
    ccy_only <- dropped[!is.na(dropped$currency), ]
    no_fx    <- setdiff(unique(ccy_only$currency), unique(fx$currency))
    dated    <- ccy_only[ccy_only$currency %in% unique(fx$currency), ]
    parts <- c(
      if (length(no_ccy) > 0) paste0(length(no_ccy), " ticker(s) with no currency in the currency map: ",
                                     paste(no_ccy, collapse = ", ")),
      if (length(no_fx) > 0) paste0("no cached FX series for: ", paste(no_fx, collapse = ", ")),
      if (nrow(dated) > 0) paste0(nrow(dated), " row(s) with no FX rate on/before the date (before the first ",
                                  "rate or older than ", max_fx_age_days, " days)")
    )
    warning("load_cached_prices_eur dropped ", nrow(dropped), " of ", nrow(prices), " price row(s): ",
            paste(parts, collapse = "; "),
            ". Run scripts/get_currency_map.R and scripts/get_price_data.R to refresh.", call. = FALSE)
  }

  result <- joined[usable, ] |>
    transmute(ticker, Date, Adjusted_close = price_local / rate) |>
    arrange(ticker, Date)
  attr(result, "dropped") <- as_tibble(dropped)
  result
}


# ============================================================================
# FUNCTION: get_crypto_tickers
# ============================================================================
# Returns the tickers classified as "Cryptocurrency" in the asset_class
# column of meta/tickerliste.xlsx.
#
# Why this matters: a cryptocurrency trades every calendar day, not just the
# ~252 trading days/year equities do. Fetching it through an equity-oriented
# API path silently assumes it's an equity. get_price_data.R uses this to fetch
# crypto tickers via EODHD's "<SYMBOL>-USD.CC" pair format instead of the bare
# equity symbol, and get_currency_map.R to price them in USD. (Annualization no
# longer needs the list: periods_per_year() reads the quote frequency off the
# series itself.) Centralized here so the scripts can't drift on which tickers
# count as crypto.
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


# ============================================================================
# FUNCTION: periods_per_year
# ============================================================================
# How many return observations a series produces per year, from the calendar time
# its dates span: (n - 1) / elapsed years, i.e. 1 / mean interval between
# observations. This is the factor that turns a per-observation mean and standard
# deviation into annual figures: mean * k, sd * sqrt(k).
#
# Why not a fixed 252 (or 365 for crypto): the number of quotes per year differs by
# series. A cryptocurrency is quoted on all 7 weekdays (k ~ 365), a stock only on
# its exchange's trading days (k ~ 250), and the aligned return panel
# (prices_to_wide()) keeps only the dates on which EVERY holding has a price, so a
# Korean, Taiwanese or European holiday removes a date for all of them and merges two
# steps into one (k ~ 220 in the current window). Variance accrues with calendar time
# for crypto and with trading days for equities, but in both cases the returns tile
# the elapsed time without gaps, so the realised variance per year is
# k * (variance per observation) whatever the quote frequency - which is what this
# factor applies. A fixed 252 over a panel of 145 observations in 240 calendar days
# overstated the annual mean by ~15% and the volatility by ~7%.
#
# Args:
#   dates: dates of a series' observations (prices, or the dates of a return panel);
#     order and duplicates are ignored
# Returns: numeric; NA when fewer than two distinct dates

periods_per_year <- function(dates) {
  dates <- sort(unique(as.Date(dates)))
  n <- length(dates)
  if (n < 2) return(NA_real_)
  (n - 1) * 365.25 / as.numeric(max(dates) - min(dates))
}
