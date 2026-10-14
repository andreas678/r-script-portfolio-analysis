## Multi-agent export of the report (data/output/<portfolio>.xlsx)
##
## Merges the weight-based structural view (the top-20 holdings with their risk
## contribution) with the risk/return-based quality view (holding-level
## positioning) for the positions chosen by select_export_tickers(), so the multi-agent
## system sees "my biggest positions", "the positions that carry the risk" and "the
## positions helping/hurting my risk-adjusted return". Kept out of
## portfolio_report.Rmd so the export's column layout survives changes to the
## report's sections, and so it can be tested without rendering.

library(tidyverse)

# Fails with the missing column names instead of dplyr's generic "Can't subset columns".
require_columns <- function(df, cols, what) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop("build_multiagent_export(): `", what, "` is missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
}

# ---------------------------------------------------------------------------------------------
# Which positions are listed individually
#
# The look-through holds ~4,000 equities; the multi-agent system cannot read them all, and below
# ~0.1 % of the portfolio a holding has no price series (so no volatility, return or positioning).
# The listed rows are the union of four rules that adapt as the portfolio changes:
#   weight      equity weight >= min_weight_pct of the portfolio
#   risk        the fewest equities that reach risk_coverage_pct of the equities' POSITIVE look-through
#               risk (Euler share): few names carry most of it, whatever their weight
#   hedge       the n_hedges equities with the most negative risk contribution (they reduce portfolio
#               risk: what a trim should not touch)
#   pick        the risk/return picks (worst / best / pure improvers) of the positioning analysis
#   block       every non-equity row (gold, crypto, cash, bonds): that is where rebalancing happens
# The list is capped at max_rows by dropping weight-only rows (smallest weight first), then
# pick-only rows; risk, hedge and block rows are kept. Below min_rows the heaviest equities not yet
# listed are added.
#
# Args:
#   ticker_summary: ticker, weight_pct, asset_class (one row per ticker)
#   risk_shares:    tibble(ticker, risk_contribution_pct): look-through Euler shares (mrc_table)
#   pick_tickers:   tickers of the positioning picks (character(0) when there is no positioning)
#
# Returns: tibble(ticker, selected_because), the reasons joined by " + ", in the order
# block, weight, risk, hedge, pick; heaviest first.
select_export_tickers <- function(ticker_summary, risk_shares, pick_tickers = character(0),
                                  min_weight_pct = 0.15, risk_coverage_pct = 90, n_hedges = 3,
                                  min_rows = 25, max_rows = 60) {
  require_columns(ticker_summary, c("ticker", "weight_pct", "asset_class"), "ticker_summary")
  require_columns(risk_shares, c("ticker", "risk_contribution_pct"), "risk_shares")

  ts <- ticker_summary |> distinct(ticker, .keep_all = TRUE)
  equities <- ts |> filter(asset_class == "Equities")
  eq_risk <- risk_shares |> filter(ticker %in% equities$ticker, !is.na(risk_contribution_pct))

  positive <- eq_risk |> filter(risk_contribution_pct > 0) |> arrange(desc(risk_contribution_pct))
  risk_tickers <- if (nrow(positive) == 0) character(0) else {
    cumulative <- cumsum(positive$risk_contribution_pct) / sum(positive$risk_contribution_pct) * 100
    positive$ticker[seq_len(which(cumulative >= risk_coverage_pct)[1])]
  }
  hedge_tickers <- eq_risk |>
    filter(risk_contribution_pct < 0) |>
    slice_min(risk_contribution_pct, n = n_hedges, with_ties = FALSE) |>
    pull(ticker)

  chosen <- ts |>
    transmute(
      ticker, weight_pct,
      block  = asset_class != "Equities",
      weight = asset_class == "Equities" & weight_pct >= min_weight_pct,
      risk   = ticker %in% risk_tickers,
      hedge  = ticker %in% hedge_tickers,
      pick   = ticker %in% pick_tickers
    ) |>
    filter(block | weight | risk | hedge | pick)

  # Cap: drop the least informative rows first, never a risk / hedge / block row.
  if (nrow(chosen) > max_rows) {
    droppable <- chosen |>
      filter(!block, !risk, !hedge) |>
      arrange(pick, weight_pct)               # weight-only rows (pick FALSE) first, lightest first
    n_drop <- min(nrow(chosen) - max_rows, nrow(droppable))
    chosen <- chosen |> filter(!ticker %in% droppable$ticker[seq_len(n_drop)])
  }
  # Floor: add the heaviest equities not listed yet.
  if (nrow(chosen) < min_rows) {
    filler <- equities |>
      filter(!ticker %in% chosen$ticker) |>
      slice_max(weight_pct, n = min_rows - nrow(chosen), with_ties = FALSE) |>
      transmute(ticker, weight_pct, block = FALSE, weight = TRUE, risk = FALSE, hedge = FALSE, pick = FALSE)
    chosen <- bind_rows(chosen, filler)
  }

  chosen |>
    arrange(desc(weight_pct)) |>
    mutate(selected_because = pmap_chr(list(block, weight, risk, hedge, pick), function(b, w, r, h, p) {
      paste(c("building block", "weight", "risk", "hedge", "risk/return pick")[c(b, w, r, h, p)], collapse = " + ")
    })) |>
    select(ticker, selected_because)
}

# risk_contribution:   top-20 table (tornado-chart-analysis chunk) with ticker, name,
#                      weight_pct, sector, index, volatility, vol_source, market_beta,
#                      risk_contribution_pct
# holding_positioning: per-ticker positioning (holding-positioning-calc chunk)
# selection:           select_export_tickers() result: which rows to list and why
# ticker_summary:      fund-source label per ticker (+ name, weight_pct, sector when present: they
#                      fill the rows that neither view has, e.g. a heavy holding without a price series)
# extra_beta:          optional tibble(ticker, market_beta) for rows risk_contribution (top 20 only) lacks
#
# Cash/Low Risk Bonds and any other unpriced row have no row in holding_positioning (no market
# price history), so volatility/vol_source/annual_return/quadrant/weighted_impact stay NA for them -
# that correctly signals "no return data available" rather than fabricating a number.
build_multiagent_export <- function(risk_contribution, holding_positioning, selection, ticker_summary,
                                    extra_beta = NULL) {
  require_columns(risk_contribution, c("ticker", "name", "weight_pct", "sector", "index", "volatility",
                                       "vol_source", "market_beta", "risk_contribution_pct"),
                  "risk_contribution")
  require_columns(holding_positioning, c("ticker", "name", "weight_pct", "sector", "volatility_pct",
                                         "annual_return", "quadrant", "risk_return_score", "weighted_impact"),
                  "holding_positioning")
  require_columns(ticker_summary, c("ticker", "index"), "ticker_summary")
  require_columns(selection, c("ticker", "selected_because"), "selection")

  # ticker_summary's own columns, for rows risk_contribution (top 20) and holding_positioning
  # (priced holdings) never had.
  ts <- ticker_summary %>% distinct(ticker, .keep_all = TRUE)
  optional <- function(col, as) if (col %in% names(ts)) as(ts[[col]]) else as(rep(NA, nrow(ts)))
  ticker_lookup <- tibble(
    ticker = ts$ticker, ts_index = ts$index,
    ts_name = optional("name", as.character), ts_weight_pct = optional("weight_pct", as.numeric),
    ts_sector = optional("sector", as.character)
  )

  extra_beta <- if (is.null(extra_beta)) tibble(ticker = character(0), extra_market_beta = numeric(0)) else
    extra_beta %>% distinct(ticker, .keep_all = TRUE) %>% select(ticker, extra_market_beta = market_beta)

  tibble(ticker = unique(selection$ticker)) %>%
    left_join(
      risk_contribution %>%
        select(ticker, rc_name = name, rc_weight_pct = weight_pct, rc_sector = sector,
               index, volatility, vol_source, market_beta, risk_contribution_pct),
      by = "ticker") %>%
    left_join(
      holding_positioning %>%
        select(ticker, hp_name = name, hp_weight_pct = weight_pct, hp_sector = sector,
               hp_volatility_pct = volatility_pct, annual_return, quadrant,
               risk_return_score, weighted_impact),
      by = "ticker") %>%
    left_join(ticker_lookup, by = "ticker") %>%
    left_join(extra_beta, by = "ticker") %>%
    left_join(selection %>% distinct(ticker, .keep_all = TRUE), by = "ticker") %>%
    transmute(
      ticker,
      name       = coalesce(rc_name, hp_name, ts_name),
      weight_pct = coalesce(rc_weight_pct, hp_weight_pct, ts_weight_pct),
      sector     = coalesce(rc_sector, hp_sector, ts_sector),
      index      = coalesce(index, ts_index),
      # the positioning view's volatility is on the same window as annual_return and quadrant;
      # risk_contribution's (the holding's own, longer history) only fills in where there is none
      volatility = coalesce(hp_volatility_pct, volatility),
      vol_source = coalesce(vol_source, if_else(!is.na(hp_volatility_pct), "Historical", NA_character_)),
      market_beta = coalesce(market_beta, extra_market_beta),
      risk_contribution_pct,
      annual_return,
      quadrant = as.character(quadrant),
      weighted_impact,
      positioning_source = selected_because
    ) %>%
    arrange(desc(weight_pct)) %>%
    { if (nrow(.) == 0) stop("Multi-agent export is empty: the selection has no tickers.")
      else . }
}

## ---------------------------------------------------------------------------------------------
## Workbook for the multi-agent system: positions + the report's own figures
##
## The multi-agent system does not recompute risk, concentration, drawdown or exposure figures;
## it interprets what this workbook says. Sheet 1 (`positions`) keeps the layout the older
## multi-agent readers expect (new columns are only appended), so the reader can be updated
## after the export. Everything else is a plain copy of objects the report already computed.
##
## schema_version 2 = this workbook (version 1 = the single-sheet file without a summary).

MULTIAGENT_SCHEMA_VERSION <- 2L

# positions:         build_multiagent_export() output (or the weight-only fallback); gains asset_class,
#                    euler_risk_share_pct (look-through share of the covered part) and
#                    sleeve_risk_share_pct (share of the whole portfolio; positions that are building blocks themselves)
# ticker_summary:    ticker, asset_class (ticker-level aggregate of the report)
# mrc_table:         look-through Euler risk table (ticker, risk_contribution_pct, share of the covered part)
# sleeve_model:      compute_sleeve_model() result
# asset_classes:     tibble(asset_class, weight) in % of the portfolio
# equity_concentration: concentration_metrics() of the equity part
# sectors:           tibble(sector, weight_pct, share_of_equities_pct) - equity part only
# countries:         tibble(alpha3, country_name, weight_pct, equity_share_pct) - equity part only
# currency_exposure: compute_currency_exposure()$exposure (currency, weight, holdings)
# drawdown:          compute_drawdown_table() result
# caveats:           report_caveats() result
# meta:              list(scenario, data_as_of, rendered_at, equity_weight_pct,
#                         lookthrough_coverage_pct, euler_coverage_pct)
#
# Returns: named list of tibbles = the sheets, `positions` first.
build_multiagent_sheets <- function(positions, ticker_summary, mrc_table, sleeve_model, asset_classes,
                                    equity_concentration, sectors, countries, currency_exposure,
                                    drawdown, caveats, meta) {
  require_columns(positions, c("ticker", "weight_pct"), "positions")
  require_columns(ticker_summary, c("ticker", "asset_class"), "ticker_summary")
  require_columns(mrc_table, c("ticker", "risk_contribution_pct"), "mrc_table")
  require_columns(sleeve_model$sleeves, c("sleeve", "label", "kind", "weight", "volatility_pct",
                                          "annual_return_pct", "risk_share_pct", "risk_vs_weight",
                                          "return_contribution_pct"),
                  "sleeve_model$sleeves")
  require_columns(asset_classes, c("asset_class", "weight"), "asset_classes")
  require_columns(sectors, c("sector", "weight_pct", "share_of_equities_pct"), "sectors")
  require_columns(countries, c("alpha3", "country_name", "weight_pct", "equity_share_pct"), "countries")
  require_columns(currency_exposure, c("currency", "weight", "holdings"), "currency_exposure")
  require_columns(drawdown, c("ticker", "max_drawdown_pct", "max_drawdown_peak_date", "max_drawdown_trough_date",
                              "current_drawdown_pct", "peak_date", "momentum_pct"),
                  "drawdown")
  if (nrow(sleeve_model$sleeves) == 0) stop("build_multiagent_sheets(): the sleeve model has no sleeves.")
  stopifnot(is.list(meta), all(c("scenario", "data_as_of", "rendered_at", "equity_weight_pct",
                                 "lookthrough_coverage_pct", "euler_coverage_pct") %in% names(meta)))

  positions_out <- positions |>
    left_join(ticker_summary |> distinct(ticker, .keep_all = TRUE) |> select(ticker, asset_class), by = "ticker") |>
    left_join(mrc_table |> distinct(ticker, .keep_all = TRUE) |>
                select(ticker, euler_risk_share_pct = risk_contribution_pct), by = "ticker") |>
    # A directly held or synthetic position (gold, crypto, single stock, cash, bonds) is a building block of
    # the whole-portfolio model, so it also has a share of the WHOLE portfolio's risk; fund constituents do not (only the
    # fund as a whole does).
    left_join(sleeve_model$sleeves |> filter(kind != "fund") |>
                select(ticker = sleeve, sleeve_risk_share_pct = risk_share_pct), by = "ticker")

  # Risk share and return contribution per asset class, summed from the sleeves, so that nobody
  # has to add the funds up: a fund is an equity fund, a direct or synthetic sleeve takes the asset
  # class of its ticker. Classes without a modelled sleeve stay NA.
  asset_class_sums <- sleeve_model$sleeves |>
    left_join(ticker_summary |> distinct(ticker, .keep_all = TRUE) |> select(ticker, sleeve_class = asset_class),
              by = c("sleeve" = "ticker")) |>
    mutate(asset_class = if_else(kind == "fund", "Equities", coalesce(sleeve_class, "Equities"))) |>
    group_by(asset_class) |>
    summarise(risk_share_pct = sum(risk_share_pct), return_contribution_pct = sum(return_contribution_pct),
              .groups = "drop")

  eq <- equity_concentration
  summary <- tibble(
    schema_version = MULTIAGENT_SCHEMA_VERSION,
    rendered_at = as.character(meta$rendered_at),
    data_as_of = as.character(meta$data_as_of),
    scenario = as.character(meta$scenario),
    window_start = as.character(sleeve_model$window[1]),
    window_end = as.character(sleeve_model$window[2]),
    window_months = as.numeric(diff(sleeve_model$window)) / 30.44,
    n_steps = sleeve_model$n_steps,
    sleeve_coverage_pct = sleeve_model$coverage_pct,
    lookthrough_coverage_pct = meta$lookthrough_coverage_pct,
    euler_coverage_pct = meta$euler_coverage_pct,
    portfolio_vol_pct = sleeve_model$volatility_pct,
    annual_return_pct = sleeve_model$annual_return_pct,
    cumulative_return_pct = sleeve_model$cumulative_return_pct,
    max_drawdown_pct = sleeve_model$max_drawdown_pct,
    diversification_ratio = sleeve_model$diversification_ratio,
    equity_weight_pct = meta$equity_weight_pct,
    equity_positions = eq$n,
    equity_hhi = eq$hhi * 10000,            # 0-10000 scale (the multi-agent system's HHI scale)
    equity_enp = eq$enp,
    equity_top1_pct = eq$top[["top1"]],
    equity_top5_pct = eq$top[["top5"]],
    equity_top10_pct = eq$top[["top10"]],
    equity_top20_pct = eq$top[["top20"]]
  )

  list(
    positions = positions_out,
    summary = summary,
    sleeves = sleeve_model$sleeves |>
      transmute(sleeve, label, kind, weight_pct = weight, volatility_pct, annual_return_pct,
                risk_share_pct, risk_vs_weight, return_contribution_pct),
    asset_classes = asset_classes |>
      transmute(asset_class, weight_pct = weight) |>
      left_join(asset_class_sums, by = "asset_class"),
    sectors = sectors |> select(sector, weight_pct, share_of_equities_pct),
    countries = countries |> select(alpha3, country_name, weight_pct, equity_share_pct),
    currency = currency_exposure |>
      transmute(currency, weight_pct = weight / sum(weight) * 100, holdings),
    drawdown = drawdown |>
      transmute(ticker, max_drawdown_pct,
                max_drawdown_peak_date = as.character(max_drawdown_peak_date),
                max_drawdown_trough_date = as.character(max_drawdown_trough_date),
                current_drawdown_pct, peak_date = as.character(peak_date), momentum_pct),
    caveats = tibble(order = seq_along(caveats), text = as.character(caveats))
  )
}
