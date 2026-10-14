## Ticker -> Trading-Currency Map
##
## EODHD's EOD endpoint returns prices in each listing's LOCAL trading
## currency and carries no currency column, so meta/closing_prices.csv mixes
## USD, EUR, HKD, TWD, KRW, ... without saying which is which. Converting to
## EUR (a later phase) needs the currency per ticker first; this script
## builds that map into meta/ticker_currency.csv (ticker, exchange, currency,
## price_scale, source).
##
## The currency comes from EODHD's exchange-symbol-list endpoint (per-symbol
## `Currency` = "trading currency of the listing"), NOT from the ticker
## suffix: a suffix identifies the exchange, and a bare ticker (no suffix) is
## looked up on EODHD's US list, which is exactly where a non-US company
## quoted without a suffix would silently resolve to the wrong listing.
## Such tickers (bare, but tickerliste.xlsx says the company is non-US) are
## surfaced by flag_bare_non_us_tickers() rather than corrected here.
##
## Run standalone to refresh: `Rscript scripts/get_currency_map.R`

library(tidyverse)
library(httr)
library(readxl)
library(here)

source(here::here("scripts", "price_cache.R"))
source(here::here("scripts", "utils", "security.R"))
source(here::here("scripts", "utils", "logging.R"))
source(here::here("scripts", "utils", "sector_taxonomy.R"))
source(here::here("scripts", "utils", "ticker_disambiguation.R"))
source(here::here("scripts", "utils", "portfolio_build.R"))  # read_refresh_holdings(), BENCHMARK_TICKERS

CURRENCY_MAP_FILE <- TICKER_CURRENCY_FILE  # price_cache.R; read back by load_cached_prices_eur()

# Manual overrides, checked before every other source (e.g. a ticker whose
# listing EODHD reports wrongly). Columns: ticker, currency, price_scale
# (multiply the raw price by this to get whole currency units, e.g. 0.01 for
# a pence quote reported as GBP). Empty by default.
MANUAL_CURRENCY <- tibble(
  ticker = character(), currency = character(), price_scale = double()
)

# ============================================================================
# FUNCTION: exchange_code_for_ticker
# ============================================================================
# Maps a portfolio-facing ticker to the EODHD exchange code whose symbol list
# contains it: the suffix after the last "." ("005930.KO" -> "KO"), or "US"
# for a bare ticker (EODHD's default for symbols without an exchange).
# Vectorized.

exchange_code_for_ticker <- function(ticker) {
  has_suffix <- grepl(".", ticker, fixed = TRUE)
  ifelse(has_suffix, toupper(sub("^.*\\.", "", ticker)), "US")
}

# ============================================================================
# FUNCTION: fetch_exchange_symbol_list
# ============================================================================
# One EODHD exchange-symbol-list call.
#
# Returns: tibble(Code, Currency) on success (all other columns dropped), or
#   list(success = FALSE, error = <message>) on failure. api_key is redacted
#   from any caught error text.

fetch_exchange_symbol_list <- function(exchange, api_key) {
  tryCatch({
    url <- sprintf(
      "https://eodhd.com/api/exchange-symbol-list/%s?api_token=%s&fmt=json",
      exchange, api_key
    )
    # a full exchange list is several MB, hence the longer timeout; without one a
    # stalled connection hangs the whole build
    response <- httr::GET(url, httr::timeout(120))
    if (response$status_code != 200) {
      return(list(success = FALSE, error = paste("HTTP", response$status_code)))
    }
    parsed <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
    if (!is.data.frame(parsed) || !all(c("Code", "Currency") %in% names(parsed))) {
      return(list(success = FALSE, error = "Unexpected response shape"))
    }
    as_tibble(parsed[, c("Code", "Currency")])
  }, error = function(e) {
    list(success = FALSE, error = redact_secret(conditionMessage(e), api_key))
  })
}

# ============================================================================
# FUNCTION: resolve_ticker_currency
# ============================================================================
# Builds the ticker -> currency map. Pure: takes already-fetched symbol lists,
# does no I/O.
#
# Precedence per ticker: manual override > crypto (USD, fetched as
# "<SYMBOL>-USD.CC") > synthetic cash/bond tickers (EUR) > the exchange's
# symbol list > unresolved (currency NA).
#
# Args:
#   tickers: character vector of portfolio-facing tickers
#   symbol_lists: named list, exchange code -> tibble(Code, Currency)
#     (from fetch_exchange_symbol_list); a missing/failed exchange just leaves
#     its tickers unresolved
#   crypto_tickers: from get_crypto_tickers()
#   manual: tibble(ticker, currency, price_scale), default MANUAL_CURRENCY
#
# Returns:
#   tibble(ticker, exchange, currency, price_scale, source), one row per
#   unique ticker. source is one of manual / crypto / synthetic / symbol_list
#   / symbol_list_ambiguous (the list held several currencies for the code;
#   the first is used) / unresolved. Pence quotes (GBX/GBp, and any GBP-
#   labelled LSE listing) come back as currency GBP with price_scale 0.01.

resolve_ticker_currency <- function(tickers, symbol_lists, crypto_tickers = character(),
                                    manual = MANUAL_CURRENCY) {
  tibble(ticker = unique(tickers)) |>
    mutate(exchange = exchange_code_for_ticker(ticker)) |>
    rowwise() |>
    mutate(res = list({
      base <- toupper(sub("\\.[^.]*$", "", ticker))
      cur  <- NA_character_
      src  <- "unresolved"
      scale <- 1

      if (ticker %in% manual$ticker) {
        m <- manual[manual$ticker == ticker, ][1, ]
        cur <- m$currency; scale <- m$price_scale; src <- "manual"
      } else if (ticker %in% crypto_tickers) {
        cur <- "USD"; src <- "crypto"
      } else if (ticker %in% c(CASH_TICKER, BONDS_LR_TICKER)) {
        cur <- "EUR"; src <- "synthetic"
      } else if (!is.null(symbol_lists[[exchange]])) {
        hits <- symbol_lists[[exchange]]$Currency[toupper(symbol_lists[[exchange]]$Code) == base]
        hits <- hits[!is.na(hits) & nzchar(hits)]
        if (length(hits) > 0) {
          cur <- hits[1]
          src <- if (length(unique(hits)) > 1) "symbol_list_ambiguous" else "symbol_list"
        }
      }

      # Pence quotes: EODHD's LSE prices are in pence (verified 2026-09-29:
      # RR.LSE 1481 vs. RYCEF, its 1:1 USD OTC line, 19.56 USD at ~1.33
      # USD/GBP), yet exchange-symbol-list labels the LSE listings "GBP", so a
      # GBP label on the LSE means pence. Listings the list reports in another
      # currency (USD/EUR-denominated ETFs on the LSE) are left as reported.
      if (!is.na(cur) && (cur %in% c("GBX", "GBp") || (exchange == "LSE" && cur == "GBP"))) {
        cur <- "GBP"; scale <- 0.01
      }
      list(currency = cur, price_scale = scale, source = src)
    })) |>
    ungroup() |>
    mutate(
      currency    = map_chr(res, "currency"),
      price_scale = map_dbl(res, "price_scale"),
      source      = map_chr(res, "source")
    ) |>
    select(ticker, exchange, currency, price_scale, source)
}

# ============================================================================
# FUNCTION: flag_bare_non_us_tickers
# ============================================================================
# Tickers without an exchange suffix whose tickerliste.xlsx country is not the
# USA. EODHD resolves a bare ticker on its US list, so such a ticker is either
# a genuine US listing of a foreign company (an ADR) or a wrong-instrument
# match whose price series belongs to a different company entirely. The
# currency map cannot tell those apart; this lists them for manual review.
#
# Args:
#   currency_map: output of resolve_ticker_currency()
#   tickerliste: tibble with ticker, alpha3, companyname (meta/tickerliste.xlsx)
#
# Returns: tibble(ticker, alpha3, companyname, currency), possibly empty.

flag_bare_non_us_tickers <- function(currency_map, tickerliste) {
  tl <- tickerliste |>
    filter(!is.na(ticker), !is.na(alpha3)) |>
    distinct(ticker, .keep_all = TRUE) |>
    select(ticker, alpha3, companyname)

  currency_map |>
    filter(exchange == "US", source %in% c("symbol_list", "symbol_list_ambiguous", "unresolved")) |>
    inner_join(tl, by = "ticker") |>
    filter(alpha3 != "USA") |>
    select(ticker, alpha3, companyname, currency)
}

# ============================================================================
# Standalone refresh: Rscript scripts/get_currency_map.R
# ============================================================================
# Same direct-invocation guard as get_price_data.R: only runs when THIS file
# was the one passed to Rscript, not when another script source()s it.

invoked_file <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
invoked_directly <- length(invoked_file) > 0 && basename(invoked_file[1]) == "get_currency_map.R"

if (invoked_directly) {

  api_key <- Sys.getenv("EODHD_API_KEY")
  if (api_key == "") stop("EODHD_API_KEY is not set")

  cached_tickers <- load_cached_prices() |> distinct(ticker) |> pull(ticker)

  # Every held ticker of the COMPLETE portfolio, not only those with cached
  # prices (min weight 0: the report's currency exposure covers small positions
  # too). Same function as the price refresh: it builds from the fund files,
  # not from data/clean/portfolio.xlsx, which every render overwrites with that
  # render's scenario-filtered holdings. Disambiguation is applied inside, so
  # the map is keyed by the same corrected tickers the report uses.
  held_tickers <- read_refresh_holdings(load_portfolio_configs(), 0, load_ticker_disambiguation())

  # the funds' own series (config/etf_listings.csv) carry their currency in the
  # config, so they enter as manual overrides (the EUFUND NAV series have no
  # symbol list to read it from)
  etf_listings <- load_etf_listings()
  manual_currency <- bind_rows(MANUAL_CURRENCY,
                               etf_listings |> select(ticker, currency, price_scale))

  all_tickers <- unique(c(cached_tickers, held_tickers, BENCHMARK_TICKERS, CASH_TICKER, BONDS_LR_TICKER,
                          etf_listings$ticker))

  exchanges <- sort(unique(exchange_code_for_ticker(all_tickers)))
  cat("Fetching symbol lists for", length(exchanges), "exchange(s):",
      paste(exchanges, collapse = ", "), "\n")

  symbol_lists <- list()
  for (ex in exchanges) {
    res <- fetch_exchange_symbol_list(ex, api_key)
    if (is.data.frame(res)) {
      symbol_lists[[ex]] <- res
      cat(sprintf("  %-8s %d symbols\n", ex, nrow(res)))
    } else {
      msg <- paste0("Symbol list for exchange ", ex, " failed: ", res$error)
      cat("  ", msg, "\n")
      log_pipeline_issue(msg)
    }
    Sys.sleep(0.2)
  }

  currency_map <- resolve_ticker_currency(all_tickers, symbol_lists, get_crypto_tickers(), manual = manual_currency)

  write.csv(currency_map, here::here("meta", CURRENCY_MAP_FILE), row.names = FALSE)
  cat("\nSaved ", nrow(currency_map), " rows to meta/", CURRENCY_MAP_FILE, "\n\n", sep = "")

  cat("Currency distribution:\n")
  print(count(currency_map, currency, sort = TRUE))

  unresolved <- filter(currency_map, source == "unresolved")
  if (nrow(unresolved) > 0) {
    msg <- paste0(nrow(unresolved), " ticker(s) without a currency: ",
                  paste(unresolved$ticker, collapse = ", "))
    cat("\n", msg, "\n", sep = "")
    log_pipeline_issue(msg)
  }

  ambiguous <- filter(currency_map, source == "symbol_list_ambiguous")
  if (nrow(ambiguous) > 0) {
    msg <- paste0(nrow(ambiguous), " ticker(s) listed under several currencies (first used): ",
                  paste(ambiguous$ticker, collapse = ", "))
    cat("\n", msg, "\n", sep = "")
    log_pipeline_issue(msg)
  }

  tickerliste_path <- here::here("meta", TICKERLISTE_FILE)
  if (file.exists(tickerliste_path)) {
    suspects <- flag_bare_non_us_tickers(currency_map, read_excel(tickerliste_path))
    if (nrow(suspects) > 0) {
      msg <- paste0(nrow(suspects), " suffix-less ticker(s) with a non-US country in ",
                    "tickerliste.xlsx (possible wrong-listing match): ",
                    paste(suspects$ticker, collapse = ", "))
      cat("\n", msg, "\n", sep = "")
      print(suspects, n = Inf)
      log_pipeline_issue(msg)
    }
  }
}
