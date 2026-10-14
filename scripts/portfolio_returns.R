## Portfolio Daily-Return Series
##
## Shared, pure building blocks for turning cached daily prices
## (load_cached_prices(), price_cache.R) into daily log-return series and
## a single portfolio-weighted return series. Used by the Advanced
## Visualizations section of portfolio_report.Rmd (cumulative returns vs.
## benchmarks, and the holding-level risk/return positioning).
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
# Builds a single portfolio daily log-return series from a wide per-ticker return
# matrix and a named weight vector: the portfolio held at constant weights, rebalanced
# every day. Its simple return is the weighted average of the holdings' simple returns,
# so the log return is log(sum(w_i * exp(r_i))). Weighting the LOG returns directly
# (sum(w_i * r_i)) is lower by roughly half the gap between the holdings' average
# variance and the portfolio's variance - about 4-5 points of cumulative return over
# eight months for a portfolio holding semiconductors at 80-100% volatility.
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
#   portfolio_return = log(sum(weight_i * exp(return_i))) over the matched tickers.
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
    portfolio_return = as.numeric(log(exp(returns_matrix) %*% w))
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

# ============================================================================
# FUNCTION: aggregate_returns_to_dates
# ============================================================================
# Puts a daily log-return panel onto a coarser set of dates WITHOUT losing the
# returns of the days in between. The portfolio's return panel only keeps dates on
# which every holding has a price (so e.g. a Korean or Taiwanese holiday removes the
# day), and its return on the next kept date spans the gap. A benchmark panel has
# those days; filtering it to the portfolio's dates (`filter(Date %in% dates)`)
# throws their returns away, so its cumulative return came out wrong (URTH +18.3%
# against +15.0% price-to-price over the same window). Log returns add up, so each
# benchmark return is summed into the first target date on or after it instead.
#
# Args:
#   returns_wide: tibble with Date + one log-return column per series
#                 (e.g. from compute_daily_log_returns() / build_return_panel())
#   dates: the target dates (any order; duplicates are ignored), at least two
#
# Returns:
#   tibble(Date, <series>...) with one row per target date. The first row is the
#   baseline and is 0; every later row is the sum of the series' returns on the
#   dates after the previous target date up to and including this one, so
#   exp(cumsum()) is the price-to-price growth from the first target date. Returns
#   dated before/at the first or after the last target date are not used.

aggregate_returns_to_dates <- function(returns_wide, dates) {
  dates <- sort(unique(as.Date(dates)))
  if (length(dates) < 2) stop("aggregate_returns_to_dates: need at least two target dates")

  series  <- setdiff(names(returns_wide), "Date")
  # index of the first target date on or after each return date; 1 = on/before the
  # baseline date, length(dates) + 1 = after the last target date
  bucket  <- findInterval(returns_wide$Date, dates, left.open = TRUE) + 1L
  keep    <- bucket >= 2L & bucket <= length(dates)

  out <- matrix(0, nrow = length(dates), ncol = length(series), dimnames = list(NULL, series))
  if (any(keep)) {
    sums <- rowsum(as.matrix(returns_wide[keep, series, drop = FALSE]), bucket[keep])
    out[as.integer(rownames(sums)), ] <- sums
  }

  dplyr::bind_cols(tibble(Date = dates), tibble::as_tibble(out))
}

# ============================================================================
# FUNCTION: annualize_returns
# ============================================================================
# Annual volatility and annual return of every series of a return panel, over exactly
# the panel's rows. The report puts the portfolio, each holding and each benchmark
# through this on the SAME dates, so their figures can be compared with one another
# (holdings used to be measured over their own, partly longer histories against a
# portfolio point on the shared panel: GLD showed -3% p.a. in one place and -20% in
# another).
#
# Args:
#   returns_wide: tibble with Date + one log-return column per series; every row is
#     one step (daily, or a gap-spanning step as in build_return_panel())
#   periods: observations per year; default periods_per_year(returns_wide$Date)
#     (price_cache.R). Pass it explicitly when the rows are a subset of a window whose
#     frequency should be kept (e.g. benchmark steps without the baseline row).
#
# Returns: tibble(ticker, volatility_pct, annual_return_pct, periods_per_year); the
#   return is the annualized MEAN LOG return (mean x periods), like everywhere else.

annualize_returns <- function(returns_wide, periods = periods_per_year(returns_wide$Date)) {
  m <- as.matrix(returns_wide[, setdiff(names(returns_wide), "Date"), drop = FALSE])
  tibble(
    ticker = colnames(m),
    volatility_pct = unname(apply(m, 2, stats::sd)) * sqrt(periods) * 100,
    annual_return_pct = unname(colMeans(m)) * periods * 100,
    periods_per_year = periods
  )
}

# ============================================================================
# FUNCTION: compute_sector_attribution
# ============================================================================
# Decomposes portfolio return by sector -- how much each sector contributed
# to (a) the portfolio's own blended return, and (b) its excess return over
# a single benchmark figure, when one is given.
#
# This is a return-contribution decomposition, NOT a full Brinson
# allocation/selection split. A true Brinson split needs the BENCHMARK's own
# sector weights (e.g. MSCI World's actual sector composition) to separate
# "we were overweight sector X" (allocation effect) from "our sector-X
# holdings beat sector X's own benchmark return" (selection effect) --
# get_price_data.R's BENCHMARK_TICKERS only fetches benchmark *price*
# series (SPY/URTH/EEM), never benchmark sector composition, so that split
# isn't computable from data this pipeline actually has. What IS computable
# from data already on hand, and what this function returns: each sector's
# share of total return, and (if benchmark_return is given) each sector's
# share of the portfolio's excess return over that one overall benchmark
# figure. Legitimate, useful, and honestly coarser than Brinson.
#
# Args:
#   sector_summary: tibble with `sector`, `weight`, `avg_return` columns --
#     e.g. sector_bubble_summary from portfolio_report.Rmd's
#     sector-bubble-calc chunk (Section 6.2). `weight` need not sum to 100
#     (e.g. restricted to priced tickers only) -- renormalized internally,
#     mirroring compute_portfolio_return_series()'s convention. `avg_return`
#     and `benchmark_return` must be on the same basis; this report uses
#     annualized mean return per observation x periods_per_year() throughout
#     Sections 6.2-6.3, so that's what this function assumes too.
#   benchmark_return: optional single numeric. When supplied, adds
#     excess_contribution_pct; when NULL (default), that column is omitted
#     rather than computed against a meaningless placeholder.
#
# Returns:
#   sector_summary with `contribution_pct` added (and `excess_contribution_pct`
#   when benchmark_return is given), sorted descending by contribution_pct.
#   sum(contribution_pct) == weighted.mean(avg_return, weight) (the
#   priced-subset's own blended return); sum(excess_contribution_pct), when
#   present, == that same blended return minus benchmark_return.

compute_sector_attribution <- function(sector_summary, benchmark_return = NULL) {
  result <- sector_summary %>%
    mutate(
      normalized_weight = weight / sum(weight),
      contribution_pct = normalized_weight * avg_return
    )

  if (!is.null(benchmark_return)) {
    result <- result %>%
      mutate(excess_contribution_pct = normalized_weight * (avg_return - benchmark_return))
  }

  result %>% arrange(desc(contribution_pct))
}

# ============================================================================
# FUNCTION: compute_currency_exposure
# ============================================================================
# Splits portfolio weight by TRADING (listing) currency - the currency a
# position's price is quoted in, hence the FX risk a EUR investor carries on
# it. Not economic exposure: a multinational's revenues span many currencies.
#
# Each holding's currency comes from, in order:
#   1. the ticker -> currency map (meta/ticker_currency.csv, from
#      scripts/get_currency_map.R)  -> currency_basis "listing"
#   2. the currency of the holding's country (ISO 3166-1 alpha-3 -> ISO 4217
#      via countrycode)             -> "country" (an estimate: used for small
#      positions whose fund export carries a bare local ticker EODHD can't
#      resolve)
#   3. "Unassigned"                 -> "none" (e.g. GOLD/CRYPTO pseudo-countries
#      without a map entry)
#
# Args:
#   portfolio: tibble with ticker, alpha3, weight
#   ticker_currency: tibble(ticker, currency, ...) from load_ticker_currency()
#
# Returns:
#   list(by_holding = portfolio + listing_ccy/country_ccy/currency/currency_basis,
#        exposure   = tibble(currency, weight, holdings), heaviest first)

compute_currency_exposure <- function(portfolio, ticker_currency) {
  country_currency <- tibble(alpha3 = unique(portfolio$alpha3)) |>
    mutate(country_ccy = suppressWarnings(countrycode::countrycode(alpha3, "iso3c", "iso4217c")))

  by_holding <- portfolio |>
    left_join(ticker_currency |> distinct(ticker, .keep_all = TRUE) |>
                select(ticker, listing_ccy = currency), by = "ticker") |>
    left_join(country_currency, by = "alpha3") |>
    mutate(
      currency = coalesce(listing_ccy, country_ccy, "Unassigned"),
      currency_basis = case_when(!is.na(listing_ccy) ~ "listing",
                                 !is.na(country_ccy) ~ "country",
                                 TRUE ~ "none")
    )

  exposure <- by_holding |>
    group_by(currency) |>
    summarise(weight = sum(weight), holdings = n_distinct(ticker), .groups = "drop") |>
    arrange(desc(weight))

  list(by_holding = by_holding, exposure = exposure)
}

# ============================================================================
# FUNCTION: concentration_metrics
# ============================================================================
# Concentration of a set of weights, measured within that set (weights are re-expressed as
# shares of their own sum): Herfindahl index, effective number of positions (1 / HHI) and the
# combined share (%) of the largest k positions.
#
# Args:
#   weights: non-negative numeric vector (any unit; NA/negative is an error)
#   top_n: how many of the largest positions to sum
#
# Returns: list(n, hhi, enp, top = named numeric of shares in %, names "top1", "top5", ...)

concentration_metrics <- function(weights, top_n = c(1, 5, 10, 20)) {
  if (!is.numeric(weights) || length(weights) == 0 || anyNA(weights) || any(weights < 0) || sum(weights) <= 0) {
    stop("concentration_metrics: weights must be a non-empty, non-negative numeric vector with a positive sum")
  }
  shares <- sort(weights / sum(weights), decreasing = TRUE)
  list(
    n = length(shares),
    hhi = sum(shares^2),
    enp = 1 / sum(shares^2),
    top = stats::setNames(vapply(top_n, function(k) sum(utils::head(shares, k)), numeric(1)) * 100,
                          paste0("top", top_n))
  )
}

# ============================================================================
# FUNCTION: compute_marginal_risk_contribution
# ============================================================================
# Euler decomposition of portfolio volatility into per-holding contributions:
#
#   sigma_p^2 = w' S w,   S = diag(sigma) R diag(sigma)
#   marginal contribution   MC_i = (S w)_i / sigma_p        (d sigma_p / d w_i)
#   risk contribution       RC_i = w_i * MC_i
#   sum_i RC_i = sigma_p    (exactly)
#
# Unlike weight x standalone volatility - which is only right if every pair is
# perfectly correlated - RC_i accounts for how each holding moves with the rest
# of the portfolio. A holding that is volatile but diversifying gets a small
# RC_i; one that hedges the rest (negative correlation, small enough weight) can
# get a NEGATIVE contribution.
#
# Only holdings present in the correlation matrix can be decomposed: nothing is
# assumed for holdings without return history (no invented correlations), so the
# result describes the covered subset only. Callers should report how much
# portfolio weight that subset represents.
#
# Args:
#   weights: named numeric vector (names = tickers); renormalized to sum to 1
#     over the covered tickers, so contributions are shares of the covered part
#   sigma: named numeric vector of annualized volatilities as DECIMALS (0.2 =
#     20 %), same names
#   correlation: correlation matrix with dimnames = tickers. Every ticker in
#     `weights` must appear in `sigma` and in the matrix (extra tickers there
#     are ignored); the three are aligned by name, not by position.
#
# Returns:
#   list(
#     table = tibble(ticker, weight [renormalized share], sigma, marginal_contribution,
#                    risk_contribution [same units as sigma, sums to portfolio_vol],
#                    risk_contribution_pct [sums to 100; can be negative]),
#     portfolio_vol = sigma_p (decimal)
#   ), the table sorted by risk_contribution descending.

compute_marginal_risk_contribution <- function(weights, sigma, correlation) {
  tickers <- names(weights)
  if (is.null(tickers) || anyNA(tickers) || anyDuplicated(tickers)) {
    stop("compute_marginal_risk_contribution: `weights` must be named with unique tickers")
  }
  missing_sigma <- setdiff(tickers, names(sigma))
  missing_corr  <- setdiff(tickers, rownames(correlation))
  if (length(missing_sigma) > 0 || length(missing_corr) > 0) {
    stop("compute_marginal_risk_contribution: no sigma / correlation for: ",
         paste(union(missing_sigma, missing_corr), collapse = ", "))
  }

  w  <- weights / sum(weights)
  s  <- sigma[tickers]
  R  <- correlation[tickers, tickers, drop = FALSE]
  S  <- R * (s %o% s)

  Sw      <- as.numeric(S %*% w)
  sigma_p <- sqrt(sum(w * Sw))
  rc      <- as.numeric(w) * Sw / sigma_p  # as.numeric: keep the tibble columns unnamed

  list(
    table = tibble(
      ticker = tickers,
      weight = as.numeric(w),
      sigma = as.numeric(s),
      marginal_contribution = Sw / sigma_p,
      risk_contribution = rc,
      risk_contribution_pct = rc / sigma_p * 100
    ) |> arrange(desc(risk_contribution)),
    portfolio_vol = sigma_p
  )
}

# ============================================================================
# FUNCTION: current_tickers
# ============================================================================
# Tickers whose series is still being updated: its last date is within
# max_staleness_days of the newest date in the whole cache. A series that
# stopped weeks or months ago (a delisted or renamed ticker, or one the
# refresh no longer covers) is not a current holding's price, and - because
# prices_to_wide() keeps only dates on which EVERY ticker has a price - a
# single stale series would cut the shared history of all the others down to
# its own end date.
#
# Args:
#   prices_long: tibble(ticker, Date, Adjusted_close)
#   max_staleness_days: allowed gap between a ticker's last date and the
#     newest date in prices_long (default 5: a weekend plus one exchange
#     holiday; the cache's newest date can be a crypto weekend quote, so
#     equities are routinely 1-3 days behind it. 7 admitted a series that
#     simply had not been refreshed for a week and let it cut the window)
#
# Returns: character vector of tickers.

current_tickers <- function(prices_long, max_staleness_days = 5) {
  if (nrow(prices_long) == 0) return(character())
  newest <- max(prices_long$Date)
  prices_long |>
    group_by(ticker) |>
    summarise(last_date = max(Date), .groups = "drop") |>
    filter(last_date >= newest - max_staleness_days) |>
    pull(ticker)
}

# ============================================================================
# FUNCTION: build_return_panel
# ============================================================================
# The one way to turn cached prices into an aligned daily log-return panel:
# keep only tickers that are still updating (current_tickers()), then
# prices_to_wide() (needs min_observations, strict date intersection), then
# compute_daily_log_returns().
#
# Why the staleness filter is part of it: prices_to_wide() intersects dates
# across ALL tickers, so a single series that stopped updating cuts the shared
# history of every other ticker to its own end date (a stale gold series once
# did exactly that). Only the marginal-risk table applied the filter; the
# correlation matrix, cumulative returns and the frontier now go through here
# too and so see the same universe.
#
# Args:
#   prices_long: tibble(ticker, Date, Adjusted_close)
#   tickers: restrict to these (NULL = all in prices_long)
#   min_observations: passed to prices_to_wide() (default 100)
#   max_staleness_days: passed to current_tickers() (default 5)
#
# Returns: tibble(Date, <ticker>...) of log returns, as
#   compute_daily_log_returns(); attribute "stale_tickers" lists the requested
#   tickers dropped for staleness (they had prices, but stopped updating).

build_return_panel <- function(prices_long, tickers = NULL, min_observations = 100,
                               max_staleness_days = 5) {
  if (!is.null(tickers)) prices_long <- prices_long |> filter(ticker %in% tickers)

  current <- current_tickers(prices_long, max_staleness_days)
  stale   <- setdiff(unique(prices_long$ticker), current)

  panel <- prices_long |>
    prices_to_wide(tickers = current, min_observations = min_observations) |>
    compute_daily_log_returns()

  attr(panel, "stale_tickers") <- stale
  panel
}

# ============================================================================
# FUNCTION: compute_fund_tracking
# ============================================================================
# Cross-checks a fund's look-through holdings against the fund's OWN price
# series: rebuilds a return series from the fund's constituents (current
# weights, renormalized over the holdings that have a current price history)
# and compares it with the real ETF/fund return on the same dates.
#
# What it catches: wrong ticker -> wrong company mappings (the 26 wrong listings
# of 2026-09-29 would show up as a low correlation / high tracking error),
# holdings with no price history (low coverage), and an ISIN that points to a
# different fund than the holdings file. What it does NOT measure: the fund's
# true tracking error - the weights are a TODAY snapshot applied to the whole
# window and only the covered part of the fund is rebuilt, so a high figure can
# also mean low coverage. Read it together with coverage_pct.
#
# Args:
#   holdings: tibble(ticker, weight) - the fund's own constituents (weight in %,
#     any scale; duplicates are summed)
#   etf_ticker: the fund's price-series symbol (config/etf_listings.csv)
#   prices_long: tibble(ticker, Date, Adjusted_close), e.g. load_cached_prices_eur()
#   min_observations: minimum shared return days for the statistics (default 60)
#
# Returns: one-row tibble(n_holdings, n_covered, coverage_pct, n_obs, correlation,
#   beta, r_squared, tracking_error_pct, cum_fund_pct, cum_rebuilt_pct, note).
#   Statistics are NA and `note` says why when there is no fund series, no
#   covered holding, or too few shared days. tracking_error_pct is the annualized
#   (x sqrt(periods_per_year() of the shared panel, price_cache.R)) st.dev. of the
#   return difference per panel step; beta regresses the fund on the rebuilt series.

compute_fund_tracking <- function(holdings, etf_ticker, prices_long, min_observations = 60) {
  empty <- function(note, n_cov = 0L, cov_pct = 0, n_hold = 0L, n_obs = NA_integer_) {
    tibble::tibble(n_holdings = n_hold, n_covered = n_cov, coverage_pct = cov_pct, n_obs = n_obs,
                   correlation = NA_real_, beta = NA_real_, r_squared = NA_real_,
                   tracking_error_pct = NA_real_, cum_fund_pct = NA_real_,
                   cum_rebuilt_pct = NA_real_, note = note)
  }

  h <- holdings |>
    filter(!is.na(ticker), ticker != "-", !is.na(weight), weight > 0) |>
    group_by(ticker) |>
    summarise(weight = sum(weight), .groups = "drop")
  if (nrow(h) == 0) return(empty("no holdings"))

  panel <- build_return_panel(prices_long, tickers = c(h$ticker, etf_ticker), min_observations = 100)
  if (!etf_ticker %in% names(panel)) {
    return(empty("no current fund price series", n_hold = nrow(h)))
  }

  covered <- intersect(h$ticker, names(panel))
  cov_pct <- sum(h$weight[h$ticker %in% covered]) / sum(h$weight) * 100
  if (length(covered) == 0) {
    return(empty("no holding with a current price history", n_hold = nrow(h)))
  }

  w <- stats::setNames(h$weight[match(covered, h$ticker)], covered)
  rebuilt <- compute_portfolio_return_series(panel[, c("Date", covered)], w)
  fund <- panel[[etf_ticker]]
  n_obs <- nrow(panel)
  if (n_obs < min_observations) {
    return(empty(paste("only", n_obs, "shared days"), length(covered), cov_pct, nrow(h), n_obs))
  }

  x <- rebuilt$portfolio_return
  tibble::tibble(
    n_holdings = nrow(h), n_covered = length(covered), coverage_pct = cov_pct, n_obs = n_obs,
    correlation = stats::cor(fund, x),
    beta = stats::cov(fund, x) / stats::var(x),
    r_squared = stats::cor(fund, x)^2,
    tracking_error_pct = stats::sd(fund - x) * sqrt(periods_per_year(panel$Date)) * 100,
    cum_fund_pct = (exp(sum(fund)) - 1) * 100,
    cum_rebuilt_pct = (exp(sum(x)) - 1) * 100,
    note = ""
  )
}

# One row per fund: runs compute_fund_tracking() for every fund of a portfolio
# that has a listing in config/etf_listings.csv (self-weighted pseudo-funds such
# as Einzelaktien/Cash have none and are skipped).
#
# Args:
#   fund_data: build_complete_portfolio()$fund_data (file, label, raw, ...)
#   listings: load_etf_listings()
#   prices_long: EUR price tibble
# Returns: tibble(label, fund_file, etf_ticker, <compute_fund_tracking columns>)
compute_fund_tracking_table <- function(fund_data, listings, prices_long) {
  fund_data |>
    inner_join(listings |> select(fund_file, etf_ticker = ticker), by = c("file" = "fund_file")) |>
    (\(d) purrr::pmap_dfr(list(d$label, d$file, d$etf_ticker, d$raw), \(lab, f, tk, raw) {
      dplyr::bind_cols(tibble::tibble(label = lab, fund_file = f, etf_ticker = tk),
                       compute_fund_tracking(raw |> select(ticker, weight), tk, prices_long))
    }))()
}

# ============================================================================
# FUNCTION: shrink_correlation_lw
# ============================================================================
# Ledoit-Wolf (2004) shrinkage of the sample correlation matrix towards the
# identity. With many holdings and about one year of daily data, the
# sample covariance is noisy and - when there are more assets than
# observations - singular (rank <= T-1): its extreme eigenvalues are the
# estimation error, and a risk decomposition built on it inherits it. Shrinking
# gives a well-conditioned, positive-definite matrix whose off-diagonal
# entries are the sample correlations scaled by (1 - delta).
#
# Works on standardized returns (z-scores, population sd), so the target's
# scale is right by construction and the result stays a correlation matrix
# (unit diagonal). Closed-form intensity (Ledoit & Wolf 2004, "A well-
# conditioned estimator for large-dimensional covariance matrices"):
#   S = Z'Z / n,   d2 = ||S - I||_F^2 / p,
#   b2 = min( (1/n^2) sum_k ||z_k z_k' - S||_F^2 / p , d2 ),   delta = b2 / d2
# using sum_k ||z_k z_k' - S||_F^2 = sum_k ||z_k||^4 - n ||S||_F^2.
#
# Args:
#   returns: numeric matrix or data frame, one column per asset (no Date column),
#     no NAs, n rows (observations) > 1
#
# Returns:
#   list(correlation = p x p matrix with the input's column names,
#        shrinkage = delta in [0, 1], n_obs = n, n_assets = p)

shrink_correlation_lw <- function(returns) {
  X <- as.matrix(returns)
  if (anyNA(X)) stop("shrink_correlation_lw: returns must not contain NA")
  n <- nrow(X); p <- ncol(X)
  if (n < 2 || p < 1) stop("shrink_correlation_lw: need at least 2 observations and 1 asset")

  Z <- scale(X, center = TRUE, scale = FALSE)
  sd_pop <- sqrt(colMeans(Z^2))
  if (any(sd_pop == 0)) stop("shrink_correlation_lw: an asset has zero variance")
  Z <- sweep(Z, 2, sd_pop, "/")

  S  <- crossprod(Z) / n
  d2 <- sum((S - diag(p))^2) / p
  b2 <- min((sum(rowSums(Z^2)^2) - n * sum(S^2)) / n^2 / p, d2)
  delta <- if (d2 > 0) b2 / d2 else 1

  R <- delta * diag(p) + (1 - delta) * S
  dimnames(R) <- list(colnames(X), colnames(X))

  list(correlation = R, shrinkage = delta, n_obs = n, n_assets = p)
}

# ============================================================================
# FUNCTION: drawdown_dates
# ============================================================================
# Dates of the deepest drawdown of a price series: the high it fell from and the low it reached.
# Returns list(peak, trough); both NA when the series never fell below a previous high.
drawdown_dates <- function(dates, prices) {
  running_peak <- cummax(prices)
  drawdown <- prices / running_peak - 1
  trough <- which.min(drawdown)
  if (drawdown[trough] >= 0) return(list(peak = as.Date(NA), trough = as.Date(NA)))
  list(peak = dates[max(which(prices[seq_len(trough)] == running_peak[trough]))], trough = dates[trough])
}

# ============================================================================
# FUNCTION: compute_drawdown_table
# ============================================================================
# Per-ticker drawdown and short-term momentum from EUR price series, for the multi-agent export
# (the multi-agent system interprets these figures; it does not recompute them).
#
# Over the observations from `from` on (default: the whole series):
#   max_drawdown_pct          deepest fall from a running peak (<= 0)
#   max_drawdown_peak_date    the high that fall started from (NA when there was no fall)
#   max_drawdown_trough_date  the low it reached (NA when there was no fall)
#   current_drawdown_pct      last price vs. the highest price in the span (<= 0; 0 = at the peak)
#   peak_date                 date of that highest price: the high the CURRENT drawdown is measured
#                             from, not the date of any loss
#   momentum_pct              change over the last `momentum_days` observations
#
# Args:
#   prices_long: tibble(ticker, Date, Adjusted_close), e.g. load_cached_prices_eur()
#   tickers: tickers to include (those with fewer than 2 prices in the span are left out)
#   from: optional Date; earlier observations are ignored (the window the report is measured on)
#   momentum_days: observations for the momentum look-back (NA when the series is shorter)
#
# Returns: tibble(ticker, first_date, last_date, n_obs, max_drawdown_pct, max_drawdown_peak_date,
#   max_drawdown_trough_date, current_drawdown_pct, peak_date, momentum_pct), in the order of `tickers`.

compute_drawdown_table <- function(prices_long, tickers, from = NULL, momentum_days = 20) {
  px <- prices_long |>
    filter(ticker %in% tickers, !is.na(Adjusted_close), Adjusted_close > 0)
  if (!is.null(from)) px <- px |> filter(Date >= as.Date(from))

  px |>
    arrange(ticker, Date) |>
    group_by(ticker) |>
    filter(n() >= 2) |>
    summarise(
      first_date = min(Date),
      last_date = max(Date),
      n_obs = n(),
      max_drawdown_pct = min(Adjusted_close / cummax(Adjusted_close) - 1) * 100,
      max_drawdown_peak_date = drawdown_dates(Date, Adjusted_close)$peak,
      max_drawdown_trough_date = drawdown_dates(Date, Adjusted_close)$trough,
      current_drawdown_pct = (last(Adjusted_close) / max(Adjusted_close) - 1) * 100,
      peak_date = Date[max(which(Adjusted_close == max(Adjusted_close)))],
      momentum_pct = if (n() > momentum_days) (last(Adjusted_close) / nth(Adjusted_close, n() - momentum_days) - 1) * 100
                     else NA_real_,
      .groups = "drop"
    ) |>
    arrange(match(ticker, tickers))
}
