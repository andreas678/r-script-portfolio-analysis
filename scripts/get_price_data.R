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
source(here::here("scripts", "utils", "ticker_disambiguation.R"))
source(here::here("scripts", "utils", "portfolio_build.R"))
source(here::here("scripts", "utils", "sector_taxonomy.R"))  # CASH_TICKER / BONDS_LR_TICKER

PRICE_METADATA_FILE <- "price_fetch_metadata.csv"

# Calendar days of history a ticker gets when it is new to the cache (and the depth the cache is
# rebuilt to, see plan_price_rebuilds()). The default used to be 252, which is the number of
# TRADING days in a year, passed as CALENDAR days: ~8 months of history, so every ticker added
# later started on a later date and the shared analysis window shrank to the newest one's
# (26 Jan 2026 on a cache with 80 late additions). The free EODHD plan serves one year at most,
# so 365 is also the useful ceiling.
PRICE_HISTORY_DAYS <- 365

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

    # timeout: without one a stalled connection hangs the whole refresh loop
    response <- httr::GET(url, httr::timeout(30))

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
      ) |>
      # A free-plan response ends with a plain-text line ("Data is limited by
      # one year as you have free subscription") that read.csv() turns into a
      # row with no valid date; drop it rather than persisting a junk row.
      filter(!is.na(Date))

  }, error = function(e) {
    # httr embeds the full request URL (incl. api_key) in network-level
    # error messages (timeout, DNS failure, ...) - redact before this
    # reaches cat() in update_price_cache() below or any redirected log.
    list(success = FALSE, error = redact_secret(conditionMessage(e), api_key))
  })
}


# ============================================================================
# FUNCTION: last_business_day / plan_price_fetches
# ============================================================================
# Which tickers need an API call. A ticker used to count as stale whenever its
# last cached date was before TODAY - so on a Saturday or Sunday, when the
# newest possible price is Friday's, EVERY ticker (and every FX series) was
# re-requested on every run and came back empty: 100+ calls a run, all wasted.
# A ticker is now stale only if its last cached date is before the latest date
# a price can exist, i.e. the last weekday on or before end_date.
#
# Weekdays only - no exchange holiday calendar: on a holiday the run still asks
# once per ticker and gets an empty answer, then the next weekday fills the gap
# (fetching always resumes from last cached date + 1, so no data is lost).
# Cryptocurrencies also trade at weekends, but nothing is lost either: their
# missing Saturday/Sunday rows are fetched on the next weekday.
#
# Args:
#   date: a Date (last_business_day)
#   tickers: wanted tickers; cached: tibble(ticker, Date, ...) like
#     load_cached_prices(); period_days: lookback for tickers with no cache;
#     end_date; min_staleness_days: tolerated lag behind the latest possible date
#
# Returns: last_business_day(): the Date. plan_price_fetches(): tibble(ticker,
#   last_date, fetch_from, needs_fetch = TRUE), one row per ticker to fetch.

last_business_day <- function(date) {
  dow <- as.integer(format(date, "%u"))  # 1 = Monday ... 7 = Sunday, locale-independent
  date - pmax(dow - 5L, 0L)
}

plan_price_fetches <- function(tickers, cached, period_days, end_date = Sys.Date(),
                               min_staleness_days = 0) {
  latest_possible <- last_business_day(end_date)

  last_cached_date <- cached |>
    group_by(ticker) |>
    summarise(last_date = max(Date), .groups = "drop")

  tibble(ticker = unique(tickers)) |>
    left_join(last_cached_date, by = "ticker") |>
    mutate(
      fetch_from = if_else(is.na(last_date), end_date - period_days, last_date + 1),
      needs_fetch = is.na(last_date) | last_date < (latest_possible - min_staleness_days)
    ) |>
    filter(needs_fetch)
}


# ============================================================================
# FUNCTION: plan_price_rebuilds
# ============================================================================
# Tickers whose cached history starts later than it could: they were fetched with a shorter
# lookback (252 calendar days before PRICE_HISTORY_DAYS replaced it) or added late. Their
# history is re-fetched in full, from `end_date - period_days` to `end_date`, and REPLACES the
# cached rows, rather than appending only the missing early part: EODHD's adjusted close is
# adjusted for every dividend / split up to the fetch date, so two pieces fetched on different
# days can sit on different levels and the join would show up as a false return.
#
# A ticker is not planned again once a successful fetch has already started at (or before) the
# wanted start date, even if its series still begins later (a young listing): without that the
# same IPO would be requested on every run. Tolerance absorbs the weekend / holiday between the
# wanted start and the first trading day.
#
# Args:
#   tickers: wanted tickers; cached: tibble(ticker, Date, ...) as load_cached_prices();
#   metadata: fetch history (ticker, start_date, success), one row per attempt, or NULL
#   period_days: wanted depth in calendar days; end_date: fetch up to here
#   tolerance_days: a first cached date within this many days of the wanted start counts as covered
#
# Returns: tibble(ticker, first_date, fetch_from, fetch_to), one row per ticker to rebuild.

plan_price_rebuilds <- function(tickers, cached, metadata, period_days = PRICE_HISTORY_DAYS,
                                end_date = Sys.Date(), tolerance_days = 10) {
  wanted_from <- end_date - period_days

  already_tried <- if (is.null(metadata) || nrow(metadata) == 0) {
    character()
  } else {
    ok <- as.logical(metadata$success) & as.Date(metadata$start_date) <= wanted_from + tolerance_days
    unique(metadata$ticker[ok & !is.na(ok)])
  }

  cached |>
    filter(ticker %in% tickers) |>
    group_by(ticker) |>
    slice_min(Date, n = 1, with_ties = FALSE) |>      # not summarise(min()): warns on an empty cache
    ungroup() |>
    transmute(ticker, first_date = Date) |>
    filter(first_date > wanted_from + tolerance_days, !ticker %in% already_tried) |>
    mutate(fetch_from = wanted_from, fetch_to = end_date)
}


# ============================================================================
# FUNCTION: update_price_cache
# ============================================================================
# Fetches only what's actually missing: new tickers get `period_days` of
# history, tickers already in the cache only get fetched from the day after
# their latest cached date through `end_date` - except tickers whose cached history starts
# later than `period_days` back (plan_price_rebuilds()), which are re-fetched in full and
# replaced. This replaces the previous
# behavior, which tracked "has this ticker ever been fetched" and therefore
# never refreshed a ticker once it appeared in the cache once.
#
# Args:
#   tickers: vector of ticker symbols to ensure are cached & current
#   api_key: EODHD API key
#   period_days: lookback window for tickers with no cached history, and the depth a
#     cached ticker with a shorter history is rebuilt to
#   rebuild: set FALSE to skip plan_price_rebuilds() (the FX cache: its lookback is far
#     beyond what the plan serves, so every currency would count as short)
#   end_date: date to fetch up to (default: today)
#   min_staleness_days: a ticker is skipped only once its cache reaches
#     strictly past this many days short of end_date (default 0: refetch
#     any ticker whose cache doesn't yet include end_date itself)
#   prices_file, metadata_file: filenames within meta/
#
# Returns:
#   The full updated price cache (same shape as load_cached_prices()).
#   Also writes meta/<prices_file> and meta/<metadata_file>.

update_price_cache <- function(tickers, api_key, period_days = PRICE_HISTORY_DAYS,
                                end_date = Sys.Date(), min_staleness_days = 0, rebuild = TRUE,
                                prices_file = PRICE_CACHE_FILE,
                                metadata_file = PRICE_METADATA_FILE) {

  meta_dir <- here::here("meta")
  if (!dir.exists(meta_dir)) {
    dir.create(meta_dir, showWarnings = FALSE)
    cat("Created directory:", meta_dir, "\n")
  }

  cached <- load_cached_prices(prices_file)

  metadata_path <- here::here("meta", metadata_file)
  old_metadata <- if (file.exists(metadata_path)) {
    read.csv(metadata_path, stringsAsFactors = FALSE)
  } else {
    NULL
  }

  rebuilds <- if (rebuild) {
    plan_price_rebuilds(tickers, cached, old_metadata, period_days, end_date)
  } else {
    plan_price_rebuilds(character(), cached, NULL, period_days, end_date)
  }
  forward <- plan_price_fetches(setdiff(unique(tickers), rebuilds$ticker), cached, period_days,
                                end_date, min_staleness_days)
  plan <- bind_rows(
    rebuilds |> transmute(ticker, fetch_from, fetch_to, kind = "rebuild"),
    forward |> transmute(ticker, fetch_from, fetch_to = end_date, kind = "forward")
  )

  if (nrow(plan) == 0) {
    cat("All", length(tickers), "tickers already up to date through", format(end_date), "\n")
    return(cached)
  }

  cat("Fetching", nrow(plan), "of", length(tickers), "tickers (",
      length(tickers) - nrow(plan), "already current;", sum(plan$kind == "rebuild"),
      "rebuilt to", period_days, "days of history)\n\n")

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
    to_str <- format(plan$fetch_to[i], "%Y-%m-%d")
    cat(sprintf("[%d/%d] %s (%s to %s%s)... ", i, nrow(plan), ticker, from_str, to_str,
                if (plan$kind[i] == "rebuild") ", rebuild" else ""))

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

  # A rebuilt ticker's fresh series replaces its cached rows, provided it reaches back at least as
  # far as the cache did (a fetch that came back shorter keeps the old rows and is merged instead).
  fresh <- bind_rows(new_rows)
  replace_tickers <- character()
  if (nrow(fresh) > 0) {
    reach <- fresh |> group_by(ticker) |> summarise(fresh_first = min(Date), .groups = "drop") |>
      inner_join(rebuilds |> select(ticker, first_date), by = "ticker") |>
      filter(fresh_first <= first_date)
    replace_tickers <- reach$ticker
  }

  updated <- bind_rows(cached |> filter(!ticker %in% replace_tickers), fresh) |>
    arrange(ticker, Date) |>
    distinct(ticker, Date, .keep_all = TRUE)

  # A series that suddenly changes level by a huge factor usually means the
  # refresh appended a DIFFERENT instrument under this ticker (wrong symbol
  # format, wrong listing). Warn before it is trusted by any volatility figure.
  jumps <- find_price_jumps(updated |> filter(ticker %in% plan$ticker))
  if (nrow(jumps) > 0) {
    warning("price jump(s) beyond the sanity threshold after this refresh - possible mixed ",
            "instruments: ", paste0(jumps$ticker, " on ", jumps$Date, " (",
                                    round((exp(jumps$log_return) - 1) * 100), "%)", collapse = "; "),
            call. = FALSE)
  }

  prices_path <- here::here("meta", prices_file)
  write.csv(updated, prices_path, row.names = FALSE)
  cat("\nSaved", nrow(updated), "rows for", n_distinct(updated$ticker),
      "tickers to", prices_path, "\n")

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
# FUNCTION: fx_symbol_for / update_fx_cache
# ============================================================================
# EUR foreign-exchange rates for converting local-currency prices to EUR.
# EODHD serves FX pairs through the same EOD endpoint as prices, as
# "EUR<CCY>.FOREX" (units of <CCY> per 1 EUR). update_fx_cache() reuses
# update_price_cache()'s incremental fetch with its own cache file, so FX
# series never mix into closing_prices.csv; read them back with
# load_fx_rates() (price_cache.R).
#
# Args:
#   currency / currencies: ISO codes (e.g. "USD"); "EUR" is skipped (rate is 1)
#   api_key: EODHD API key
#   period_days: lookback for a currency not cached yet. Deliberately long
#     (~10 years): FX history is one cheap call per currency, and the price
#     history it must cover is meant to grow well beyond one year.
#
# Returns: fx_symbol_for(): the symbol string. update_fx_cache(): the updated
#   FX cache (invisibly), also written to meta/fx_rates_raw.csv.

fx_symbol_for <- function(currency) paste0("EUR", toupper(currency), ".FOREX")

update_fx_cache <- function(currencies, api_key, period_days = 3650, ...) {
  currencies <- setdiff(unique(toupper(na.omit(currencies))), "EUR")
  if (length(currencies) == 0) return(invisible(load_cached_prices(FX_CACHE_FILE)))

  invisible(update_price_cache(
    fx_symbol_for(currencies), api_key = api_key, period_days = period_days,
    prices_file = FX_CACHE_FILE, metadata_file = "fx_fetch_metadata.csv", rebuild = FALSE, ...
  ))
}


# ============================================================================
# FUNCTION: refresh_candidates
# ============================================================================
# Splits the wanted tickers into those worth an API call and those to skip:
#   - synthetic pseudo-holdings (Cash, Low Risk Bonds) have no market price at
#     all; every run spent quota on a guaranteed HTTP 404
#   - tickers with no cached prices that already failed max_failed_attempts
#     times (typically a bare local ticker EODHD can't resolve) - retrying them
#     on every run wastes quota on the same failure. A ticker that has cached
#     prices is never skipped, however often a refresh of it failed.
# To retry a skipped ticker, delete its rows from meta/price_fetch_metadata.csv
# or raise max_failed_attempts.
#
# Args:
#   tickers: wanted tickers
#   metadata: fetch history, tibble/data.frame(ticker, success), one row per attempt
#   cached_tickers: tickers that already have cached prices
#   synthetic: tickers with no market price (default: CASH_TICKER, BONDS_LR_TICKER)
#   max_failed_attempts: skip a never-cached ticker after this many failures
#
# Returns: list(fetch = tickers to fetch, skipped_synthetic = ..., skipped_failing = ...)

refresh_candidates <- function(tickers, metadata, cached_tickers,
                               synthetic = c(CASH_TICKER, BONDS_LR_TICKER),
                               max_failed_attempts = 3) {
  tickers <- unique(tickers)

  failures <- if (is.null(metadata) || nrow(metadata) == 0) {
    integer()
  } else {
    table(metadata$ticker[!as.logical(metadata$success)])
  }
  n_failed <- function(tk) if (tk %in% names(failures)) as.integer(failures[[tk]]) else 0L

  is_synthetic <- tickers %in% synthetic
  is_failing <- !is_synthetic & !(tickers %in% cached_tickers) &
    vapply(tickers, n_failed, integer(1)) >= max_failed_attempts

  list(
    fetch = tickers[!is_synthetic & !is_failing],
    skipped_synthetic = tickers[is_synthetic],
    skipped_failing = tickers[is_failing]
  )
}


# ============================================================================
# Standalone refresh: Rscript scripts/get_price_data.R
# ============================================================================
# Pulls the ticker universe from both portfolios' cleaned holdings files
# (data/clean/portfolio.xlsx, data/clean/portfolio_b.xlsx — produced by
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
  MIN_WEIGHT_PCT <- PRICE_REFRESH_MIN_WEIGHT_PCT   # shared with the report (portfolio_build.R)

  # The cleaned portfolio files may predate config/ticker_disambiguation.csv,
  # so the rules are applied here too: without them the refresh would keep
  # fetching the wrong (bare, US-resolved) series for suffix-less non-US tickers.
  wanted_tickers <- c(
    read_refresh_holdings(load_portfolio_configs(), MIN_WEIGHT_PCT, load_ticker_disambiguation()),
    BENCHMARK_TICKERS,
    # each fund's own price series (config/etf_listings.csv), whatever its weight
    load_etf_listings()$ticker
  ) |> unique()

  etf_gaps <- etf_listings_gaps(load_etf_listings())
  if (length(etf_gaps$missing_listing) > 0) {
    warning("Funds with an ISIN but no row in config/etf_listings.csv: ",
            paste(etf_gaps$missing_listing, collapse = ", "), call. = FALSE)
  }

  metadata_path <- here::here("meta", PRICE_METADATA_FILE)
  fetch_history <- if (file.exists(metadata_path)) read.csv(metadata_path, stringsAsFactors = FALSE) else NULL
  plan <- refresh_candidates(wanted_tickers, fetch_history,
                             cached_tickers = unique(load_cached_prices()$ticker))

  cat("Refreshing price cache for", length(plan$fetch), "of", length(wanted_tickers),
      "tickers >=", MIN_WEIGHT_PCT, "% weight (incl.", length(BENCHMARK_TICKERS), "benchmarks)\n")
  if (length(plan$skipped_synthetic) > 0) {
    cat("  skipped (no market price):", paste(plan$skipped_synthetic, collapse = ", "), "\n")
  }
  if (length(plan$skipped_failing) > 0) {
    cat("  skipped (failed repeatedly, never cached):", length(plan$skipped_failing), "ticker(s) - e.g.",
        paste(head(plan$skipped_failing, 6), collapse = ", "),
        "\n  (delete their rows from meta/price_fetch_metadata.csv to retry)\n")
  }
  cat("\n")

  invisible(update_price_cache(plan$fetch, api_key = api_key, period_days = PRICE_HISTORY_DAYS))

  # EUR FX rates for every currency in meta/ticker_currency.csv (built by
  # scripts/get_currency_map.R - run that first after adding tickers).
  currency_map_path <- here::here("meta", "ticker_currency.csv")
  if (file.exists(currency_map_path)) {
    currencies <- unique(read.csv(currency_map_path, stringsAsFactors = FALSE)$currency)
    cat("\nRefreshing EUR FX rates for:", paste(setdiff(na.omit(currencies), "EUR"), collapse = ", "), "\n\n")
    update_fx_cache(currencies, api_key = api_key)
  } else {
    warning("meta/ticker_currency.csv not found - FX rates not refreshed. ",
            "Run scripts/get_currency_map.R first.", call. = FALSE)
  }
}

