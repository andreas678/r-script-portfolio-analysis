## Portfolio Daily-Return Series
##
## Shared, pure building blocks for turning cached daily prices
## (load_cached_prices(), price_cache.R) into daily log-return series and
## a single portfolio-weighted return series. Used by the Advanced
## Visualizations section of portfolio_report.Rmd (cumulative returns vs.
## benchmarks, and the efficient-frontier Monte Carlo simulation).
##
## Pure/side-effect-free: safe to source() from anywhere.

library(tidyverse)

# ============================================================================
# FUNCTION: prices_to_wide
# ============================================================================
# Restricts prices_long to tickers with at least min_observations rows,
# pivots to one price column per ticker, then drops any date row where at
# least one surviving ticker is still missing a price — i.e. restricts to
# the strict intersection of dates where every included ticker has data.
# Mirrors the good_tickers / >=100-observations / pivot_wider pattern in
# the correlation-analysis-prices chunk; the min_observations pre-filter
# exists specifically so one thinly-covered ticker doesn't truncate every
# other ticker's usable history down to its own short range.
#
# Args:
#   prices_long: tibble(ticker, Date, Adjusted_close), e.g. load_cached_prices()
#   tickers: optional vector to restrict to before the min-observations
#            filter (default: all tickers present in prices_long)
#   min_observations: minimum trading-day rows required to keep a ticker (default 100)
#
# Returns:
#   tibble(Date, <ticker1>, <ticker2>, ...) with zero remaining NAs, sorted
#   by Date. Tickers dropped for insufficient history are silently excluded
#   from the output columns.

prices_to_wide <- function(prices_long, tickers = NULL, min_observations = 100) {
  if (!is.null(tickers)) {
    prices_long <- prices_long |> filter(ticker %in% tickers)
  }

  good_tickers <- prices_long |>
    count(ticker, name = "n_dates") |>
    filter(n_dates >= min_observations) |>
    pull(ticker)

  prices_long |>
    filter(ticker %in% good_tickers) |>
    pivot_wider(names_from = ticker, values_from = Adjusted_close) |>
    arrange(Date) |>
    tidyr::drop_na()
}

# ============================================================================
# FUNCTION: compute_daily_log_returns
# ============================================================================
# Converts a wide price matrix (Date + ticker columns, e.g. from
# prices_to_wide()) into daily log returns via log(x) - log(lag(x)) per
# ticker column. The first row (no prior price to diff against) is
# dropped. Because prices_to_wide() already guarantees zero NAs in its
# input, the output is fully rectangular too — no further NA handling is
# needed downstream (cov(), matrix multiplication, etc. are all safe as-is).
#
# Args:
#   prices_wide: tibble with Date + one numeric column per ticker (no NAs)
#
# Returns:
#   tibble with Date + one log-return column per ticker (same names),
#   one row shorter than the input.

compute_daily_log_returns <- function(prices_wide) {
  prices_wide |>
    arrange(Date) |>
    mutate(across(-Date, ~ log(.x) - log(dplyr::lag(.x)))) |>
    slice(-1)
}

# ============================================================================
# FUNCTION: compute_portfolio_return_series
# ============================================================================
# Builds a single portfolio-weighted daily log-return series from a wide
# per-ticker return matrix and a named weight vector.
#
# Args:
#   returns_wide: tibble with Date + one log-return column per ticker
#                 (e.g. from compute_daily_log_returns())
#   weights: named numeric vector, names = tickers, values = weights.
#            Need not sum to 1 or match returns_wide's columns exactly —
#            renormalized internally to sum to 1 over the intersection of
#            names(weights) and the tickers actually present in returns_wide.
#
# Returns:
#   tibble(Date, portfolio_return) — one row per date in returns_wide,
#   portfolio_return = sum(weight_i * return_i) over the matched tickers.
#   Errors if there is no overlap at all between weights and returns_wide.

compute_portfolio_return_series <- function(returns_wide, weights) {
  available <- setdiff(names(returns_wide), "Date")
  tickers <- intersect(names(weights), available)

  if (length(tickers) == 0) {
    stop("compute_portfolio_return_series: no overlap between `weights` names ",
         "and `returns_wide` columns")
  }

  w <- weights[tickers]
  w <- w / sum(w)

  returns_matrix <- as.matrix(returns_wide[, tickers, drop = FALSE])

  tibble(
    Date = returns_wide$Date,
    portfolio_return = as.numeric(returns_matrix %*% w)
  )
}

# ============================================================================
# FUNCTION: rebase_to_one
# ============================================================================
# Rebases a cumulative-growth vector so its first value is exactly 1.0.
# Use after restricting multiple series (e.g. portfolio + several
# benchmarks) to a common date range, so each line starts at the same
# point on the chart regardless of what its own first-day return happened
# to be.
#
# Args:
#   cumulative_return: numeric vector of cumulative growth-of-1 values
#     (e.g. exp(cumsum(log_returns)))
#
# Returns:
#   The same vector divided by its first element.

rebase_to_one <- function(cumulative_return) {
  cumulative_return / cumulative_return[1]
}
