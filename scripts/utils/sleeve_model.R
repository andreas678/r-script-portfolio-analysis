## Sleeve model: the whole portfolio as a handful of building blocks
##
## The look-through analysis in portfolio_report.Rmd rebuilds risk and return from the
## ~160 holdings that have a price series, which is about half of the portfolio's weight:
## Cash, bonds and the long tail of the ETFs have none, the rest is renormalized, and
## gold ends up as 40% of the "covered" portfolio instead of 20%. Each fund's own price
## series (config/etf_listings.csv) already contains its whole portfolio, costs and sampling
## included, so the portfolio is modelled here as
##   - one sleeve per fund with a price series (the fund's own EUR series),
##   - one sleeve per directly held security with a price series (gold, crypto, single stocks),
##   - the synthetic Cash / Low Risk Bonds sleeves, which have no price history and run on
##     the assumed return and volatility in config/sleeve_assumptions.csv (uncorrelated with
##     everything else),
## and risk, return and the Euler risk shares are computed on those sleeves. Look-through
## stays for exposure (countries, sectors, single names), where it is the right tool.
##
## Sleeves without a series (e.g. a fund with no listing) are left out and reported as
## unmodelled; `coverage_pct` says how much of the weight the model describes.
##
## Pure/side-effect-free: safe to source() from anywhere. Needs price_cache.R
## (periods_per_year) and portfolio_returns.R to be sourced as well.

library(tidyverse)
library(here)

source(here::here("scripts", "utils", "sector_taxonomy.R"))   # CASH_TICKER / BONDS_LR_TICKER

# ============================================================================
# FUNCTION: load_sleeve_assumptions
# ============================================================================
# Returns: tibble(ticker, annual_return_pct, volatility_pct, note) for the synthetic sleeves
# (config/sleeve_assumptions.csv). These are ASSUMPTIONS, not measurements.

load_sleeve_assumptions <- function(path = here::here("config", "sleeve_assumptions.csv")) {
  readr::read_csv(path, col_types = readr::cols(ticker = "c", annual_return_pct = "d",
                                                volatility_pct = "d", note = "c"),
                  na = character(), show_col_types = FALSE)
}

# ============================================================================
# FUNCTION: build_sleeves
# ============================================================================
# Turns the (scenario-filtered) portfolio holdings into sleeves. A fund with a fixed
# allocation becomes one sleeve with its fund-level weight; a self-weighted file
# (Einzelaktien, Cash, Low Risk Bonds) becomes one sleeve per ticker.
#
# Args:
#   portfolio: holdings with index, ticker, name, weight (% of the portfolio)
#   fund_data: build_complete_portfolio()$fund_data (file, weight, label, raw)
#   listings: load_etf_listings() (fund_file -> price-series ticker)
#   synthetic: tickers without market prices (default Cash / Low Risk Bonds)
#
# Returns: tibble(sleeve [= column name / price-series ticker, unique], label,
#   kind [fund | direct | synthetic], series [price-series ticker or NA], weight [% of portfolio]).
#   A fund without a listing keeps its file name as sleeve id and series = NA; a fund
#   that a scenario removed entirely is not listed.

build_sleeves <- function(portfolio, fund_data, listings, synthetic = c(CASH_TICKER, BONDS_LR_TICKER)) {
  one_fund <- function(file, weight, label, raw) {
    rows <- portfolio[portfolio$index == raw$index[1], ]
    if (nrow(rows) == 0) return(NULL)

    if (!is.na(weight)) {
      series <- listings$ticker[match(file, listings$fund_file)]
      return(tibble::tibble(sleeve = if (is.na(series)) file else series, label = label,
                            kind = "fund", series = series, weight = sum(rows$weight)))
    }

    rows |>
      mutate(.ord = dplyr::row_number()) |>
      group_by(ticker) |>
      summarise(name = dplyr::first(name), weight = sum(weight), .ord = min(.ord), .groups = "drop") |>
      arrange(.ord) |>
      transmute(sleeve = ticker,
                label = if_else(ticker %in% synthetic, label, paste0(ticker, " - ", name)),
                kind = if_else(ticker %in% synthetic, "synthetic", "direct"),
                series = if_else(ticker %in% synthetic, NA_character_, ticker),
                weight)
  }

  purrr::pmap(list(fund_data$file, fund_data$weight, fund_data$label, fund_data$raw), one_fund) |>
    bind_rows() |>
    group_by(sleeve) |>
    mutate(weight = sum(weight)) |>
    ungroup() |>
    distinct(sleeve, .keep_all = TRUE)
}

# ============================================================================
# FUNCTION: max_drawdown_pct
# ============================================================================
# Largest peak-to-trough fall (%, <= 0) of the path compounded from log returns,
# starting at 1.

max_drawdown_pct <- function(log_returns) {
  path <- exp(c(0, cumsum(log_returns)))
  min(path / cummax(path) - 1) * 100
}

# ============================================================================
# FUNCTION: attribute_cumulative_return
# ============================================================================
# Splits the cumulative return of the daily-rebalanced mix of sleeves into one contribution
# per sleeve, in percentage points. The contributions add up exactly to the portfolio's
# cumulative return, (exp(sum of the portfolio's log returns) - 1) * 100.
#
# Method (log-linked): at step t the portfolio's simple return is x_t = sum_i w_i * s_it with
# s_it = exp(r_it) - 1, and its log return is log1p(x_t). Sleeve i's part of that log return
# is w_i * s_it * log1p(x_t) / x_t (the factor tends to 1 as x_t -> 0). Summed over the steps
# these parts add to the portfolio's total log return, which is then scaled to the cumulative
# simple return by expm1(total) / total.
#
# Args:
#   returns_matrix: steps x sleeves matrix of LOG returns (no baseline row), colnames = sleeves
#   weights: named numeric vector over the same sleeves, summing to 1
#
# Returns: named numeric vector (percentage points), one per sleeve.
attribute_cumulative_return <- function(returns_matrix, weights) {
  stopifnot(is.matrix(returns_matrix), !is.null(colnames(returns_matrix)),
            setequal(colnames(returns_matrix), names(weights)))
  w <- weights[colnames(returns_matrix)]
  parts <- sweep(exp(returns_matrix) - 1, 2, w, `*`)   # w_i * s_it
  x <- rowSums(parts)                                  # the portfolio's simple return per step
  factor <- ifelse(abs(x) < 1e-12, 1, log1p(x) / x)    # log1p(x) / x, 1 in the limit
  contribution_log <- colSums(parts * factor)
  total_log <- sum(contribution_log)                   # = sum of log1p(x_t)
  scale <- if (abs(total_log) < 1e-12) 1 else expm1(total_log) / total_log
  contribution_log * scale * 100
}

# ============================================================================
# FUNCTION: compute_sleeve_model
# ============================================================================
# Args:
#   sleeves: build_sleeves()
#   prices_long: tibble(ticker, Date, Adjusted_close) in EUR (load_cached_prices_eur())
#   window_dates: the dates the whole report is measured on (the first one is the baseline);
#     the sleeves' own daily returns are summed up to these dates (aggregate_returns_to_dates())
#     so nothing is lost on dates they skip
#   assumptions: load_sleeve_assumptions()
#   min_observations: a sleeve series needs this many prices, else it is unmodelled
#
# Method: the sleeves' covariance is estimated on the window's steps (sample covariance,
# annualized with periods_per_year()); a synthetic sleeve contributes its assumed volatility
# and zero correlation; portfolio volatility and the Euler risk shares come from
# compute_marginal_risk_contribution(). The return path is the daily-rebalanced mix of the
# sleeves (compute_portfolio_return_series()), with synthetic sleeves accruing their assumed
# return over the elapsed time (deterministic: their assumed volatility enters the risk
# numbers, not the path).
#
# Returns: list(
#   sleeves = tibble(sleeve, label, kind, weight [% of the modelled part, sums to 100],
#             volatility_pct, annual_return_pct [real: annualized mean log return; synthetic:
#             assumed], risk_share_pct, risk_vs_weight,
#             return_contribution_pct [percentage points of the window's cumulative return;
#             sums to cumulative_return_pct, see attribute_cumulative_return()]),
#   unmodelled = tibble of the sleeves left out, coverage_pct = modelled share of the weight,
#   portfolio = tibble(Date, portfolio_return) [log return per step; first row = baseline 0],
#   sleeve_steps = tibble(Date, <sleeve>...) of the steps incl. synthetic accruals,
#   volatility_pct, annual_return_pct [mean log x periods], cumulative_return_pct,
#   max_drawdown_pct, diversification_ratio [sum(w sigma) / sigma_p], correlation,
#   periods_per_year, n_steps, window = c(first, last date) )

compute_sleeve_model <- function(sleeves, prices_long, window_dates, assumptions, min_observations = 100) {
  window_dates <- sort(unique(as.Date(window_dates)))
  n_steps <- length(window_dates) - 1
  if (n_steps < 20) {
    stop("compute_sleeve_model: the window needs at least 20 steps, got ", n_steps)
  }
  k <- periods_per_year(window_dates)

  # --- sleeves with a price series ---------------------------------------------------------
  real <- sleeves |> filter(kind != "synthetic", !is.na(series))
  panel <- build_return_panel(prices_long, tickers = real$series, min_observations = min_observations)
  real_ok <- real |> filter(series %in% setdiff(names(panel), "Date"))
  if (nrow(real_ok) == 0) stop("compute_sleeve_model: no sleeve has a usable price series")
  stopifnot(all(real_ok$sleeve == real_ok$series))
  if (window_dates[1] < min(panel$Date) - 4) {
    stop("compute_sleeve_model: the window starts at ", window_dates[1],
         ", before the sleeves' price history (first return ", min(panel$Date), ")")
  }

  steps <- aggregate_returns_to_dates(panel[, c("Date", real_ok$series)], window_dates)

  # --- synthetic sleeves: assumed return accrues over the elapsed time -----------------------
  synth <- sleeves |>
    filter(kind == "synthetic") |>
    inner_join(assumptions, by = c("sleeve" = "ticker"))
  years_step <- c(0, diff(as.numeric(steps$Date)) / 365.25)
  for (i in seq_len(nrow(synth))) {
    steps[[synth$sleeve[i]]] <- log1p(synth$annual_return_pct[i] / 100) * years_step
  }

  modelled <- bind_rows(real_ok, synth |> select(any_of(names(real_ok))))
  unmodelled <- sleeves |> filter(!sleeve %in% modelled$sleeve)
  coverage_pct <- sum(modelled$weight) / sum(sleeves$weight) * 100
  w <- stats::setNames(modelled$weight / sum(modelled$weight), modelled$sleeve)

  # --- risk: sample covariance of the real sleeves, assumed volatility for the synthetic ones ----
  body <- steps[-1, ]
  real_stats <- annualize_returns(body[, real_ok$sleeve, drop = FALSE], k)
  sigma <- c(stats::setNames(real_stats$volatility_pct / 100, real_stats$ticker),
             stats::setNames(synth$volatility_pct / 100, synth$sleeve))[modelled$sleeve]
  corr <- diag(nrow(modelled)); dimnames(corr) <- list(modelled$sleeve, modelled$sleeve)
  corr[real_ok$sleeve, real_ok$sleeve] <- stats::cor(as.matrix(body[, real_ok$sleeve, drop = FALSE]))

  euler <- compute_marginal_risk_contribution(w, sigma, corr)

  # --- return path ------------------------------------------------------------------------------
  portfolio <- compute_portfolio_return_series(steps[, c("Date", modelled$sleeve)], w)
  r <- portfolio$portfolio_return[-1]

  annual_return <- c(stats::setNames(real_stats$annual_return_pct, real_stats$ticker),
                     stats::setNames(synth$annual_return_pct, synth$sleeve))[modelled$sleeve]
  contribution <- attribute_cumulative_return(as.matrix(body[, modelled$sleeve, drop = FALSE]), w)

  table <- modelled |>
    transmute(sleeve, label, kind, weight = unname(w[sleeve]) * 100) |>
    mutate(volatility_pct = unname(sigma[sleeve]) * 100,
           annual_return_pct = unname(annual_return[sleeve]),
           risk_share_pct = euler$table$risk_contribution_pct[match(sleeve, euler$table$ticker)],
           risk_vs_weight = risk_share_pct / weight,
           return_contribution_pct = unname(contribution[sleeve]))

  list(
    sleeves = table,
    unmodelled = unmodelled,
    coverage_pct = coverage_pct,
    portfolio = portfolio,
    sleeve_steps = steps[, c("Date", modelled$sleeve)],
    volatility_pct = euler$portfolio_vol * 100,
    annual_return_pct = mean(r) * k * 100,
    cumulative_return_pct = (exp(sum(r)) - 1) * 100,
    max_drawdown_pct = max_drawdown_pct(r),
    diversification_ratio = sum(w * sigma) / euler$portfolio_vol,
    correlation = corr,
    periods_per_year = k,
    n_steps = n_steps,
    window = range(window_dates)
  )
}
