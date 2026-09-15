## Retrieve Historical Price Data from EODHD API
##
## Fetches daily prices and appends them to the shared cache at
## meta/closing_prices.csv (read via load_cached_prices() in price_cache.R,
## the canonical reader used by every consumer of this data).
##
## Run standalone to refresh the cache: `Rscript scripts/get_price_data.R`

library(tidyverse)
library(httr)
library(readxl)
library(here)

source(here::here("scripts", "price_cache.R"))
source(here::here("scripts", "utils", "security.R"))

PRICE_METADATA_FILE <- "price_fetch_metadata.csv"

# Benchmark tickers tracked alongside portfolio holdings, for the report's
# cumulative-returns and efficient-frontier visualizations (Section 7).
# Single source of truth: referenced both by the standalone refresh below
# and by portfolio_report.Rmd, which source()s this file for the constant.
BENCHMARK_TICKERS <- c("SPY", "URTH", "EEM")

# ============================================================================
# FUNCTION: eodhd_symbol_for
# ============================================================================
# Maps a portfolio-facing ticker to the symbol EODHD's EOD endpoint expects.
# Equities are passed through unchanged. Cryptocurrencies use EODHD's
# "<SYMBOL>-USD.CC" pair format (e.g. "Eth" -> "ETH-USD.CC") — querying the
# bare label instead (as this code used to do for every ticker) hits EODHD's
# equity-symbol resolution instead of its crypto pairs, which is how a crypto
# holding ended up with a price series gapped to equity trading days despite
# actually trading every day of the week.
#
# Args:
#   ticker: portfolio-facing ticker (the label used everywhere else)
#   crypto_tickers: character vector from get_crypto_tickers() (price_cache.R)
#
# Returns: the symbol string to send to the EODHD API.

eodhd_symbol_for <- function(ticker, crypto_tickers) {
  if (ticker %in% crypto_tickers) paste0(toupper(ticker), "-USD.CC") else ticker
}


# ============================================================================
# FUNCTION: fetch_single_ticker_prices
# ============================================================================
# Fetches price data for a single ticker/date-range from the EODHD API.
#
# Args:
#   ticker: Portfolio-facing ticker symbol — stored as-is in the returned
#     `ticker` column, regardless of what api_symbol was queried, so
#     downstream consumers (closing_prices.csv, portfolio holdings) see the
#     same label they already use.
#   api_key: EODHD API key
#   start_date, end_date: "YYYY-MM-DD" strings
#   api_symbol: the symbol actually sent to EODHD (default: ticker itself).
#     Pass eodhd_symbol_for(ticker, crypto_tickers) for a cryptocurrency.
#
# Returns:
#   tibble(ticker, Date, Adjusted_close) on success, or
#   list(success = FALSE, error = <message>) on failure

fetch_single_ticker_prices <- function(ticker, api_key, start_date, end_date,
                                        api_symbol = ticker) {

  tryCatch({
    url <- sprintf(
      "https://eodhd.com/api/eod/%s?api_token=%s&fmt=csv&from=%s&to=%s",
      api_symbol, api_key, start_date, end_date
    )

    response <- httr::GET(url)

    if (response$status_code != 200) {
      return(list(success = FALSE, error = paste("HTTP", response$status_code)))
    }

    content <- httr::content(response, as = "text", encoding = "UTF-8")

    if (content == "" || nchar(content) < 10) {
      return(list(success = FALSE, error = "Empty response"))
    }

    read.csv(text = content, stringsAsFactors = FALSE) |>
      transmute(
        ticker = ticker,
        Date = as.Date(Date),
        Adjusted_close = as.numeric(Adjusted_close)
      )

  }, error = function(e) {
    # httr embeds the full request URL (incl. api_key) in network-level
    # error messages (timeout, DNS failure, ...) - redact before this
    # reaches cat() in update_price_cache() below or any redirected log.
    list(success = FALSE, error = redact_secret(conditionMessage(e), api_key))
  })
}


# ============================================================================
# FUNCTION: update_price_cache
# ============================================================================
# Fetches only what's actually missing: new tickers get `period_days` of
# history, tickers already in the cache only get fetched from the day after
# their latest cached date through `end_date`. This replaces the previous
# behavior, which tracked "has this ticker ever been fetched" and therefore
# never refreshed a ticker once it appeared in the cache once.
#
# Args:
#   tickers: vector of ticker symbols to ensure are cached & current
#   api_key: EODHD API key
#   period_days: lookback window used only for tickers with no cached history
#   end_date: date to fetch up to (default: today)
#   min_staleness_days: a ticker is skipped only once its cache reaches
#     strictly past this many days short of end_date (default 0: refetch
#     any ticker whose cache doesn't yet include end_date itself)
#   prices_file, metadata_file: filenames within meta/
#
# Returns:
#   The full updated price cache (same shape as load_cached_prices()).
#   Also writes meta/<prices_file> and meta/<metadata_file>.

update_price_cache <- function(tickers, api_key, period_days = 252,
                                end_date = Sys.Date(), min_staleness_days = 0,
                                prices_file = PRICE_CACHE_FILE,
                                metadata_file = PRICE_METADATA_FILE) {

  meta_dir <- here::here("meta")
  if (!dir.exists(meta_dir)) {
    dir.create(meta_dir, showWarnings = FALSE)
    cat("Created directory:", meta_dir, "\n")
  }

  cached <- load_cached_prices(prices_file)

  last_cached_date <- cached |>
    group_by(ticker) |>
    summarise(last_date = max(Date), .groups = "drop")

  plan <- tibble(ticker = unique(tickers)) |>
    left_join(last_cached_date, by = "ticker") |>
    mutate(
      fetch_from = if_else(is.na(last_date), end_date - period_days, last_date + 1),
      needs_fetch = is.na(last_date) | fetch_from <= (end_date - min_staleness_days)
    ) |>
    filter(needs_fetch)

  if (nrow(plan) == 0) {
    cat("All", length(tickers), "tickers already up to date through", format(end_date), "\n")
    return(cached)
  }

  cat("Fetching", nrow(plan), "of", length(tickers), "tickers (",
      length(tickers) - nrow(plan), "already current)\n\n")

  new_rows <- list()
  metadata_rows <- list()

  # Tickers needing the "<SYMBOL>-USD.CC" EODHD crypto pair format instead
  # of the bare equity symbol (see eodhd_symbol_for()); read once, not once
  # per ticker in the loop below.
  crypto_tickers <- get_crypto_tickers()

  for (i in seq_len(nrow(plan))) {
    ticker <- plan$ticker[i]
    api_symbol <- eodhd_symbol_for(ticker, crypto_tickers)
    from_str <- format(plan$fetch_from[i], "%Y-%m-%d")
    to_str <- format(end_date, "%Y-%m-%d")
    cat(sprintf("[%d/%d] %s (%s to %s)... ", i, nrow(plan), ticker, from_str, to_str))

    Sys.sleep(0.5)
    result <- fetch_single_ticker_prices(ticker, api_key, from_str, to_str, api_symbol = api_symbol)
    success <- is.data.frame(result)

    metadata_rows[[i]] <- tibble(
      ticker = ticker, start_date = from_str, end_date = to_str,
      fetch_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), success = success
    )

    if (success) {
      new_rows[[i]] <- result
      cat("OK (", nrow(result), "rows)\n", sep = "")
    } else {
      cat("Failed (", result$error, ")\n", sep = "")
    }
  }

  updated <- bind_rows(cached, new_rows) |>
    arrange(ticker, Date) |>
    distinct(ticker, Date, .keep_all = TRUE)

  prices_path <- here::here("meta", prices_file)
  write.csv(updated, prices_path, row.names = FALSE)
  cat("\nSaved", nrow(updated), "rows for", n_distinct(updated$ticker),
      "tickers to", prices_path, "\n")

  metadata_path <- here::here("meta", metadata_file)
  old_metadata <- if (file.exists(metadata_path)) {
    read.csv(metadata_path, stringsAsFactors = FALSE)
  } else {
    NULL
  }

  # Append this run's fetch records rather than collapsing to one row per
  # ticker, so intermittent failures (a ticker that fails on some runs and
  # succeeds on others) stay visible in the file for later diagnosis instead
  # of only ever showing the single most recent attempt.
  metadata <- bind_rows(old_metadata, bind_rows(metadata_rows))
  write.csv(metadata, metadata_path, row.names = FALSE)

  cat("\n=== Final Summary ===\n")
  cat("Total unique tickers:", n_distinct(updated$ticker), "\n")
  cat("Total price records:", nrow(updated), "\n")

  updated
}


# ============================================================================
# Standalone refresh: Rscript scripts/get_price_data.R
# ============================================================================
# Pulls the ticker universe from both portfolios' cleaned holdings files
# (data/clean/portfolio_a.xlsx, data/clean/portfolio_b.xlsx — produced by
# rendering portfolio_report.Rmd for each portfolio; see README's Data
# pipeline section), so a periodic refresh always covers whatever is
# currently held rather than a hand-maintained ticker list.
#
# Guarded on whether THIS file — not just any file — was passed directly to
# Rscript. Checking only for the presence of a `--file=` argument is not
# enough: that argument reflects the top-level invocation, so it's also set
# when some other script is run via `Rscript` and merely source()s this file
# for its functions, which should not trigger a live refresh. Comparing the
# invoked file's basename to this script's own filename closes that gap.
invoked_file <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
invoked_directly <- length(invoked_file) > 0 && basename(invoked_file[1]) == "get_price_data.R"

if (invoked_directly) {

  api_key <- Sys.getenv("EODHD_API_KEY")
  if (api_key == "") stop("EODHD_API_KEY is not set")

  # Only fetch prices for holdings at or above this portfolio weight (%).
  # Tiny index-fund tail positions are excluded to keep the cache lean.
  MIN_WEIGHT_PCT <- 0.1

  portfolio_files <- here::here("data", "clean", c("portfolio_a.xlsx", "portfolio_b.xlsx"))
  portfolio_files <- portfolio_files[file.exists(portfolio_files)]

  if (length(portfolio_files) == 0) {
    stop("No cleaned portfolio files found in data/clean/ — render portfolio_report.Rmd ",
         "for each portfolio at least once before running this refresh.")
  }

  all_tickers <- portfolio_files |>
    purrr::map(~ read_excel(.x) |>
          filter(weight >= MIN_WEIGHT_PCT, ticker != "-", !is.na(ticker), !is.na(alpha3)) |>
          pull(ticker)) |>
    unlist(use.names = FALSE) |>
    c(BENCHMARK_TICKERS) |>
    unique()

  cat("Refreshing price cache for", length(all_tickers), "tickers >=", MIN_WEIGHT_PCT,
      "% weight (incl.", length(BENCHMARK_TICKERS), "benchmarks) across",
      length(portfolio_files), "portfolio(s)\n\n")

  update_price_cache(all_tickers, api_key = api_key, period_days = 252)
}

