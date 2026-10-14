## Portfolio Construction (fund files + config -> holdings)
##
## The one place that turns the per-fund cleaned holdings under data/clean/
## plus config/portfolio_configs.csv into a portfolio's COMPLETE holdings
## (every fund, before any scenario exclusions). portfolio_report.Rmd builds its
## portfolio from it, and scripts/get_price_data.R builds the price-refresh
## universe from it - so the refresh covers the whole portfolio whatever
## scenario was rendered last, and needs no render at all.

library(tidyverse)
library(readxl)
library(here)

source(here::here("scripts", "utils", "ticker_disambiguation.R"))

# Benchmark tickers tracked alongside portfolio holdings, for the report's
# cumulative-returns visualizations and benchmark comparisons (Section 6). Single
# source of truth for the standalone price refresh, get_currency_map.R and
# portfolio_report.Rmd.
BENCHMARK_TICKERS <- c("SPY", "URTH", "EEM")

# ============================================================================
# FUNCTION: load_portfolio_configs
# ============================================================================
# Reads config/portfolio_configs.csv (one row per fund, columns
# portfolio/display_name/output_file/fund_file/weight/label). A blank `weight`
# means "self-weighted": the fund's own weight column is used as-is (this is how
# the individual-securities sheet, and the Cash / Low Risk Bonds pseudo-holdings
# from scripts/CashAndBonds.R, are handled). Any other `weight` is a fixed
# allocation fraction that scales that fund's constituent weights.
#
# NOTE: the set of portfolio ids is also hardcoded in two other places that must
# be kept in sync by hand when a portfolio is added/removed: portfolio_report.Rmd's
# YAML `params: portfolio: choices:`, and scripts/render_reports.R's `report_targets`.
#
# Returns: named list (by portfolio id) of
#   list(id, display_name, output_file, funds = tibble(file, weight, label))

load_portfolio_configs <- function(path = here::here("config", "portfolio_configs.csv")) {
  readr::read_csv(
    path,
    col_types = readr::cols(
      portfolio    = readr::col_character(),
      display_name = readr::col_character(),
      output_file  = readr::col_character(),
      fund_file    = readr::col_character(),
      weight       = readr::col_double(),
      label        = readr::col_character()
    )
  ) |>
    (\(d) split(d, d$portfolio))() |>
    purrr::map(function(df) {
      if (n_distinct(df$display_name) != 1 || n_distinct(df$output_file) != 1) {
        stop("config/portfolio_configs.csv: inconsistent display_name/output_file ",
             "within portfolio '", df$portfolio[1], "' - every row for one portfolio ",
             "must repeat the same values.")
      }
      list(
        id = df$portfolio[1],
        display_name = df$display_name[1],
        output_file  = df$output_file[1],
        funds = df |> select(file = fund_file, weight, label)
      )
    })
}

# ============================================================================
# FUNCTION: load_fund_holdings
# ============================================================================
# Reads one cleaned fund file and rewrites suffix-less tickers that EODHD would
# resolve to the wrong company's US listing (or that stand for several
# companies at once) via config/ticker_disambiguation.csv.

load_fund_holdings <- function(filename, data_dir = here::here("data", "clean"),
                               rules = load_ticker_disambiguation()) {
  read_excel(file.path(data_dir, filename)) |>
    as_tibble() |>
    mutate(weight = as.double(sub(",", ".", weight, fixed = TRUE))) |>
    apply_ticker_disambiguation(rules)
}

# ============================================================================
# FUNCTION: load_fund_proxies / build_proxy_holdings
# ============================================================================
# config/fund_proxies.csv lets a fund's published holdings be replaced by another
# fund's, for the case where the published holdings are NOT the fund's economic
# exposure. Example: a synthetic (swap-based) ETF publishes its collateral basket;
# the return it delivers is the index's, so the basket would put e.g. a tech-heavy
# portfolio into every sector/country/risk view. Opt-in per row; delete the row to
# use the published holdings again.
#
# Columns: fund_file (the fund to replace), proxy_file (a cleaned fund file to
# take the holdings from), exclude_alpha3 (space-separated country codes dropped
# from the proxy, e.g. emerging markets to turn an all-world fund into a
# developed-markets one), note.
#
# Returns: tibble(fund_file, proxy_file, exclude_alpha3, note); empty when the
#   file is absent.

load_fund_proxies <- function(path = here::here("config", "fund_proxies.csv")) {
  if (!file.exists(path)) {
    return(tibble::tibble(fund_file = character(), proxy_file = character(),
                          exclude_alpha3 = character(), note = character()))
  }
  readr::read_csv(path, col_types = readr::cols(.default = "c"), na = character(),
                  show_col_types = FALSE) |>
    dplyr::mutate(dplyr::across(c(fund_file, proxy_file), trimws))
}

# Takes the proxy fund's holdings, drops the excluded countries, renormalizes the
# weights to sum to 100 (the convention build_complete_portfolio() assumes for a
# fixed-allocation fund) and labels every row with the REPLACED fund's `index`, so
# scenario exclusions and fund attribution still work on the original fund.
build_proxy_holdings <- function(proxy_raw, exclude_alpha3, index_label) {
  excluded <- strsplit(trimws(exclude_alpha3), "[[:space:],;]+")[[1]]
  out <- proxy_raw |>
    dplyr::filter(!is.na(weight), weight > 0, !(alpha3 %in% excluded))
  if (nrow(out) == 0) stop("Proxy holdings are empty after excluding: ", paste(excluded, collapse = " "))
  out |>
    dplyr::mutate(weight = weight / sum(weight) * 100, index = index_label)
}


# ============================================================================
# FUNCTION: build_complete_portfolio
# ============================================================================
# Loads every fund of a portfolio, scales each constituent's weight by the
# fund's allocation (self-weighted funds keep their own weights), and combines
# them. Always the COMPLETE portfolio - scenario exclusions are applied by the
# caller (portfolio_report.Rmd), never here.
#
# The final filter (weight > 0, ticker present and not "-", alpha3 present) is
# applied uniformly: it was missing from the old portfolio_b report, which let
# zero/negative-weight and ticker/alpha3-less rows leak into holdings counts and
# Data Quality figures.
#
# Args:
#   cfg: one element of load_portfolio_configs()
#   data_dir: folder with the cleaned fund files (default data/clean)
#   rules: ticker disambiguation rules
#
# Returns: list(
#   fund_data = tibble(file, weight, label, proxy_file, raw, pct, scaled)  [proxy_file = the
#     fund whose holdings replaced this one's (config/fund_proxies.csv), else NA; pct = the fund's
#     share of the portfolio, for the composition text]
#   portfolio = tibble of holdings
# ); errors if a fund file is missing.

build_complete_portfolio <- function(cfg, data_dir = here::here("data", "clean"),
                                     rules = load_ticker_disambiguation(),
                                     proxies = load_fund_proxies()) {
  missing_funds <- cfg$funds$file[!file.exists(file.path(data_dir, cfg$funds$file))]
  if (length(missing_funds) > 0) {
    stop("Missing fund data file(s) for portfolio '", cfg$id, "': ",
         paste(missing_funds, collapse = ", "))
  }

  # purrr::map is namespaced: library(maps) (loaded by the report) also exports
  # a map() that would otherwise shadow it.
  fund_data <- cfg$funds |>
    mutate(
      raw    = purrr::map(file, load_fund_holdings, data_dir = data_dir, rules = rules),
      proxy_file = proxies$proxy_file[match(file, proxies$fund_file)],
      raw    = purrr::pmap(list(file, raw, proxy_file), function(f, df, px) {
        if (is.na(px)) return(df)
        pr <- proxies[proxies$fund_file == f, ]
        if (!file.exists(file.path(data_dir, px))) {
          warning("Proxy file ", px, " for ", f, " is missing - using the published holdings.", call. = FALSE)
          return(df)
        }
        build_proxy_holdings(load_fund_holdings(px, data_dir = data_dir, rules = rules),
                             pr$exclude_alpha3[1], index_label = df$index[1])
      }),
      pct    = purrr::map2_dbl(raw, weight, \(df, w) if (is.na(w)) sum(df$weight, na.rm = TRUE) / 100 else w),
      scaled = purrr::map2(raw, weight, \(df, w) if (is.na(w)) df else mutate(df, weight = w * weight))
    )

  portfolio <- fund_data |>
    pull(scaled) |>
    bind_rows() |>
    filter(weight > 0, ticker != "-", !is.na(ticker), !is.na(alpha3))

  list(fund_data = fund_data, portfolio = portfolio)
}


# ============================================================================
# FUNCTION: refresh_tickers
# ============================================================================
# The tickers the price refresh maintains: those whose weight, SUMMED over the funds that hold
# them, is at least `min_weight_pct` of the portfolio. Summed, not per row: a stock held by three
# funds at 0.06 % each is a 0.18 % position. The report restricts its return panel to the same
# set - a held ticker below the cut-off is not refreshed, so its cached series stops at some old
# date and, because return panels keep only the dates every ticker has, would cut the shared
# window of the whole report to its own start and end.
#
# Args: holdings: tibble(ticker, weight) (weight in % of the portfolio);
#   min_weight_pct: threshold (default PRICE_REFRESH_MIN_WEIGHT_PCT)
# Returns: character vector of unique tickers.

PRICE_REFRESH_MIN_WEIGHT_PCT <- 0.1

refresh_tickers <- function(holdings, min_weight_pct = PRICE_REFRESH_MIN_WEIGHT_PCT) {
  holdings |>
    filter(!is.na(ticker)) |>
    group_by(ticker) |>
    summarise(weight = sum(weight, na.rm = TRUE), .groups = "drop") |>
    filter(weight >= min_weight_pct) |>
    pull(ticker)
}


# ============================================================================
# FUNCTION: read_refresh_holdings
# ============================================================================
# The tickers the price refresh AND the currency map should cover: every holding of every portfolio, built
# straight from the per-fund files + config/portfolio_configs.csv
# (build_complete_portfolio(), scripts/utils/portfolio_build.R) - the COMPLETE
# portfolio, independent of any scenario and of whether a report was rendered.
#
# Why not read data/clean/portfolio.xlsx / portfolio_b.xlsx: every render
# overwrites those with that render's scenario-filtered holdings. After a render
# that excluded gold, the refresh silently stopped updating the gold series (20 %
# of the portfolio); it went stale for weeks and cut the shared history of every
# other series when returns were aligned.
#
# If a portfolio's fund files are not all present (e.g. a checkout without the
# raw exports), that portfolio falls back to its rendered output file, with a
# warning naming the missing files and the risk (that file reflects the last
# rendered scenario and may omit excluded holdings).
#
# Args:
#   configs: load_portfolio_configs()
#   min_weight_pct: keep tickers whose summed portfolio weight is at or above this (%), see refresh_tickers()
#   rules: ticker disambiguation rules (load_ticker_disambiguation())
#   data_dir: folder holding the cleaned files (default data/clean)
#
# Returns: character vector of unique tickers; errors if no portfolio could be
#   read at all.

read_refresh_holdings <- function(configs, min_weight_pct, rules,
                                  data_dir = here::here("data", "clean")) {
  tickers <- character()
  read_any <- FALSE

  for (cfg in configs) {
    holdings <- tryCatch(
      build_complete_portfolio(cfg, data_dir, rules)$portfolio,
      error = function(e) {
        fallback <- file.path(data_dir, cfg$output_file)
        if (!file.exists(fallback)) return(NULL)
        warning(conditionMessage(e), " - using ", cfg$output_file, " for '", cfg$id, "' instead, which ",
                "reflects the LAST RENDERED SCENARIO and may omit excluded holdings (e.g. gold).",
                call. = FALSE)
        # guess_max = 1e5: a column empty for the first 1000 rows (industry) and text later makes
        # readxl warn about hundreds of coerced cells, burying the warning above
        read_excel(fallback, guess_max = 1e5) |>
          filter(weight > 0, ticker != "-", !is.na(ticker), !is.na(alpha3)) |>
          apply_ticker_disambiguation(rules)
      }
    )
    if (is.null(holdings)) next
    read_any <- TRUE
    tickers <- c(tickers, refresh_tickers(holdings, min_weight_pct))
  }

  if (!read_any) {
    stop("Neither the fund files nor a rendered output file exist for any portfolio in ", data_dir,
         " - run the per-fund cleaner scripts (or render portfolio_report.Rmd) first.")
  }
  unique(tickers)
}


# ============================================================================
# FUNCTION: load_etf_listings / etf_listings_gaps
# ============================================================================
# config/etf_listings.csv maps each fund (by fund_file and ISIN) to the EODHD
# symbol of the instrument that represents it as ONE price series: the XETRA
# listing in EUR for the ETFs (so no FX conversion is needed), the fund's NAV
# series (exchange EUFUND) for the two non-exchange-traded funds. The listing is
# chosen by hand because an ISIN has many listings (see pick_listing_from_search()
# for the single-stock equivalent). `currency` / `price_scale` feed the currency
# map as a manual override, so these tickers need no exchange symbol list.
#
# Returns: tibble(fund_file, isin, ticker, currency, price_scale, note); empty
#   when the file is absent.

load_etf_listings <- function(path = here::here("config", "etf_listings.csv")) {
  if (!file.exists(path)) {
    return(tibble::tibble(fund_file = character(), isin = character(), ticker = character(),
                          currency = character(), price_scale = double(), note = character()))
  }
  readr::read_csv(path, col_types = readr::cols(
    fund_file = "c", isin = "c", ticker = "c", currency = "c", price_scale = "d", note = "c"
  ), na = character(), show_col_types = FALSE) |>
    dplyr::mutate(dplyr::across(c(fund_file, isin, ticker, currency), trimws))
}

# Funds in portfolio_configs.csv that carry an ISIN but have no listing (they would
# silently lack an ETF series), and listings whose ISIN is no longer in the configs
# (stale rows). Reads the isin column straight from the CSV: load_portfolio_configs()
# deliberately keeps its funds tibble at (file, weight, label).
etf_listings_gaps <- function(listings,
                              config_path = here::here("config", "portfolio_configs.csv")) {
  cfg <- readr::read_csv(config_path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
  cfg_isins <- if ("isin" %in% names(cfg)) unique(stats::na.omit(trimws(cfg$isin[nzchar(cfg$isin)]))) else character()
  list(
    missing_listing = setdiff(cfg_isins, listings$isin),
    stale_listing   = setdiff(listings$isin, cfg_isins)
  )
}
