## Calculate Historical Volatility from Local Price Data
## 
## This script provides functions to calculate annualized volatility
## from locally stored price data (CSV files).

library(tidyverse)
library(here)

source(here::here("scripts", "price_cache.R"))

# ============================================================================
# FUNCTION: calculate_volatility_from_prices
# ============================================================================
# Calculates annualized volatility from a price dataframe
#
# Args:
#   ticker: Stock ticker symbol
#   prices_df: Data frame with columns: Date, Adjusted_close, ticker
#   period_days: Optional filter to use only last N trading days
#   crypto_tickers: character vector of tickers to annualize with sqrt(365)
#     instead of sqrt(252) — a cryptocurrency trades every calendar day, not
#     just the ~252 trading days/year equities do. Defaults to
#     get_crypto_tickers() (price_cache.R); batch_calculate_volatility()
#     passes it explicitly so it's only read from tickerliste.xlsx once per
#     batch instead of once per ticker.
#
# Returns:
#   List containing:
#     - ticker: The stock ticker
#     - volatility_pct: Annualized volatility as percentage
#     - mean_daily_return: Average daily return (%)
#     - std_dev_daily: Daily standard deviation (%)
#     - observations: Number of trading days used
#     - start_date: Start date of data
#     - end_date: End date of data
#     - success: TRUE if successful, FALSE otherwise
#     - error: Error message if unsuccessful

calculate_volatility_from_prices <- function(ticker, prices_df, period_days = NULL,
                                              crypto_tickers = NULL) {

  # Filter to specific ticker
  ticker_data <- prices_df |>
    filter(ticker == !!ticker)
  
  if (nrow(ticker_data) < 20) {
    return(list(
      ticker = ticker,
      success = FALSE,
      error = paste("Only", nrow(ticker_data), "days of data available")
    ))
  }
  
  # Filter to period_days if specified
  if (!is.null(period_days)) {
    ticker_data <- ticker_data |>
      arrange(Date) |>
      tail(period_days)
  }
  
  ticker_data <- ticker_data |> arrange(Date)
  
  # Calculate returns from adjusted closing prices
  closes <- as.numeric(ticker_data$Adjusted_close)
  daily_returns <- diff(log(closes))
  
  if (length(daily_returns) < 20) {
    return(list(
      ticker = ticker,
      success = FALSE,
      error = "Insufficient data for volatility calculation"
    ))
  }
  
  if (is.null(crypto_tickers)) crypto_tickers <- get_crypto_tickers()
  annualization_days <- if (ticker %in% crypto_tickers) 365 else 252

  # Annualized volatility: daily std dev × sqrt(annualization_days)
  # (252 trading days/year for equities, 365 calendar days/year for crypto)
  volatility <- sd(daily_returns, na.rm = TRUE) * sqrt(annualization_days) * 100
  
  return(list(
    ticker = ticker,
    volatility_pct = round(volatility, 2),
    mean_daily_return = round(mean(daily_returns, na.rm = TRUE) * 100, 4),
    std_dev_daily = round(sd(daily_returns, na.rm = TRUE) * 100, 4),
    observations = nrow(ticker_data),
    start_date = format(min(ticker_data$Date), "%Y-%m-%d"),
    end_date = format(max(ticker_data$Date), "%Y-%m-%d"),
    success = TRUE
  ))
}


# ============================================================================
# FUNCTION: batch_calculate_volatility
# ============================================================================
# Calculates volatility for multiple tickers from local price data
#
# Args:
#   tickers: Vector of ticker symbols
#   prices_df: Data frame with columns: Date, Adjusted_close, ticker
#   period_days: Number of trading days for lookback (optional)
#   prices_file: Path to CSV file (if prices_df is not provided)
#
# Returns:
#   Tibble with columns:
#     - ticker: Stock ticker
#     - volatility_pct: Annualized volatility
#     - mean_daily_return_pct: Average daily return
#     - observations: Number of trading days
#     - success: TRUE/FALSE
#     - error: Error message (if applicable)

batch_calculate_volatility <- function(tickers, prices_df = NULL, period_days = NULL,
                                       prices_file = "closing_prices.csv") {

  # Load prices from the shared cache if not provided, via the same
  # canonical loader every other consumer uses (price_cache.R), so
  # cleaning/dedup behavior can't drift between call sites.
  if (is.null(prices_df)) {
    cat("Loading price data from meta/", prices_file, "\n\n", sep = "")
    prices_df <- load_cached_prices(prices_file)
    if (nrow(prices_df) == 0) {
      stop("Price cache not found or empty at: ", here::here("meta", prices_file))
    }
  }

  cat("Calculating volatility for", length(tickers), "tickers (from local data)...\n\n")

  crypto_tickers <- get_crypto_tickers()

  results <- purrr::map_df(tickers, ~{
    result <- calculate_volatility_from_prices(.x, prices_df, period_days, crypto_tickers = crypto_tickers)
    tibble(
      ticker = result$ticker,
      volatility_pct = if(result$success) result$volatility_pct else NA,
      mean_daily_return_pct = if(result$success) result$mean_daily_return else NA,
      observations = if(result$success) result$observations else NA,
      start_date = if(result$success) result$start_date else NA,
      end_date = if(result$success) result$end_date else NA,
      success = result$success,
      error = if(!result$success) result$error else NA
    )
  })
  
  return(results)
}


# ============================================================================
# FUNCTION: calculate_market_beta_from_prices
# ============================================================================
# CAPM-style market beta: how strongly a holding has historically amplified
# (>1) or damped (<1) moves in a broad equity benchmark. The benchmark has
# beta 1 by construction. This is a genuine cov/var systematic-risk beta —
# a DIFFERENT quantity from the volatility ratio reported as
# `relative_volatility` in portfolio_report.Rmd, which carries no
# correlation term at all.
#
# Two estimates are returned:
#   beta_sync  : plain contemporaneous slope, cov(r_i, r_mkt) / var(r_mkt).
#   market_beta: lag-adjusted beta — the sum of the slopes from regressing
#     the asset's return on the benchmark's contemporaneous AND once-lagged
#     returns (a one-sided Dimson (1979) / trade-to-trade adjustment). This
#     corrects the downward bias beta_sync suffers for holdings that close
#     before the benchmark does: URTH is a US-listed world-equity ETF, so a
#     Korea- or Taiwan-listed holding's daily close is ~13-15h stale
#     relative to it and only partly reflects the same day's global moves —
#     the rest shows up the next day and is captured by the lag term. The
#     lag (not lead) term is the right one because every non-US holding
#     here sits east of the US and trades earlier in the day. For US
#     holdings the lag coefficient is near zero and market_beta ~ beta_sync.
#
# Returns are measured on the benchmark's trading calendar: prices are
# inner-joined on Date before differencing, so both series span the same
# intervals even for a 24/7 asset like crypto (its weekend closes are
# dropped here rather than diffed against a stale benchmark).
#
# Args:
#   ticker: stock ticker symbol
#   prices_df: long price tibble (ticker, Date, Adjusted_close)
#   benchmark: benchmark ticker, must be present in prices_df
#     (default "URTH" — iShares MSCI World)
#   min_overlap: minimum usable return observations required to estimate a
#     beta; below this everything comes back NA (default 60, ~3 months)
#
# Returns:
#   list(ticker, market_beta, beta_sync, r_squared, obs, benchmark,
#        success, error). r_squared is for the Dimson regression.

calculate_market_beta_from_prices <- function(ticker, prices_df, benchmark = "URTH",
                                              min_overlap = 60) {

  na_result <- function(err, obs = 0L) {
    list(ticker = ticker, market_beta = NA_real_, beta_sync = NA_real_,
         r_squared = NA_real_, obs = obs, benchmark = benchmark,
         success = FALSE, error = err)
  }

  bench_px <- prices_df |>
    filter(ticker == !!benchmark) |>
    arrange(Date) |>
    distinct(Date, .keep_all = TRUE) |>
    select(Date, bench_px = Adjusted_close)

  tk_px <- prices_df |>
    filter(ticker == !!ticker) |>
    arrange(Date) |>
    distinct(Date, .keep_all = TRUE) |>
    select(Date, asset_px = Adjusted_close)

  joined <- dplyr::inner_join(tk_px, bench_px, by = "Date") |> arrange(Date)

  if (nrow(joined) <= min_overlap) {
    return(na_result(paste("Only", max(nrow(joined) - 1L, 0L),
                           "overlapping returns vs", benchmark),
                     obs = max(nrow(joined) - 1L, 0L)))
  }

  asset_ret <- diff(log(as.numeric(joined$asset_px)))
  bench_ret <- diff(log(as.numeric(joined$bench_px)))

  if (anyNA(c(asset_ret, bench_ret)) || var(bench_ret) == 0) {
    return(na_result("Zero benchmark variance or NA returns", obs = length(asset_ret)))
  }

  beta_sync <- cov(asset_ret, bench_ret) / var(bench_ret)

  # Lag-adjusted regression: asset_t ~ bench_t + bench_{t-1}
  reg <- tibble(
    a  = asset_ret,
    b0 = bench_ret,
    bl = dplyr::lag(bench_ret)
  ) |> tidyr::drop_na()

  if (nrow(reg) <= min_overlap) {
    return(na_result("Too few observations for lag-adjusted regression", obs = nrow(reg)))
  }

  fit <- stats::lm(a ~ b0 + bl, data = reg)
  co <- stats::coef(fit)
  beta_adj <- unname(co[["b0"]] + co[["bl"]])
  # R^2 without summary.lm(), which warns noisily on the benchmark's own
  # perfect self-fit; cor(y, yhat)^2 is the OLS R^2 for a model with intercept.
  r2 <- stats::cor(reg$a, stats::fitted(fit))^2

  list(ticker = ticker, market_beta = round(beta_adj, 3),
       beta_sync = round(beta_sync, 3), r_squared = round(r2, 3),
       obs = nrow(reg), benchmark = benchmark, success = TRUE, error = NA)
}


# ============================================================================
# FUNCTION: batch_calculate_market_beta
# ============================================================================
# Market beta (see calculate_market_beta_from_prices) for multiple tickers.
#
# Args:
#   tickers: vector of ticker symbols
#   prices_df: long price tibble; loaded from the shared cache via
#     load_cached_prices() if NULL
#   benchmark: benchmark ticker (default "URTH")
#   min_overlap: passed through (default 60)
#   prices_file: cache filename within meta/ (used only when prices_df is NULL)
#
# Returns:
#   Tibble(ticker, market_beta, beta_sync, r_squared, obs, benchmark).
#   market_beta/beta_sync/r_squared are NA for tickers without enough
#   overlapping history (e.g. cash, low-risk bonds, or a holding whose
#   prices aren't cached).

batch_calculate_market_beta <- function(tickers, prices_df = NULL, benchmark = "URTH",
                                        min_overlap = 60,
                                        prices_file = "closing_prices.csv") {

  if (is.null(prices_df)) {
    prices_df <- load_cached_prices(prices_file)
    if (nrow(prices_df) == 0) {
      stop("Price cache not found or empty at: ", here::here("meta", prices_file))
    }
  }

  if (!benchmark %in% prices_df$ticker) {
    warning("Benchmark '", benchmark, "' not in price cache — every market_beta will be NA. ",
            "Run get_price_data.R to fetch it.")
  }

  purrr::map_df(unique(tickers), ~{
    r <- calculate_market_beta_from_prices(.x, prices_df, benchmark = benchmark,
                                           min_overlap = min_overlap)
    tibble(
      ticker = r$ticker,
      market_beta = r$market_beta,
      beta_sync = r$beta_sync,
      r_squared = r$r_squared,
      obs = r$obs,
      benchmark = r$benchmark
    )
  })
}


# ============================================================================
# EXAMPLE USAGE
# ============================================================================
# 
# # Load price data and calculate volatilities
# tickers <- c("TSLA", "NVDA", "AAPL", "MSFT", "V", "2330", "000660")
#
# # From CSV file in meta/ folder
# volatilities <- batch_calculate_volatility(
#   tickers = tickers,
#   prices_file = "closing_prices.csv"
#   )
# print(volatilities)
#
# # With specific lookback period (e.g., last 126 trading days = 6 months)
# volatilities_6m <- batch_calculate_volatility(
#   tickers = tickers,
#   prices_file = "closing_prices.csv",
#   period_days = 126
# )
# print(volatilities_6m)
#
# # If prices already loaded in memory
# volatilities <- batch_calculate_volatility(
#   tickers = tickers,
#   prices_df = prices  # assuming prices is already in environment
# )

