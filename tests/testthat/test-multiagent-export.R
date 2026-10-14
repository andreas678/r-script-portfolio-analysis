## build_multiagent_export() (scripts/utils/multiagent_export.R) — pure. Guards the
## column layout of data/output/<portfolio>.xlsx, which the multi-agent system reads.

source(here::here("scripts", "utils", "multiagent_export.R"))

export_fixture <- function() {
  list(
    risk_contribution = tibble(
      ticker = c("AAA", "CASH_EUR", "ZZZ"),
      name = c("Alpha Inc", "Cash", "Zed Corp"),
      weight_pct = c(5, 4, 0.5), sector = c("Information Technology", "Cash & Equivalents", "Energy"),
      index = c("FundX", "Cash", "FundX"), volatility = c(30, 0, 25),
      vol_source = c("Historical", "Manual", "Historical"),
      market_beta = c(1.2, NA, 0.8), risk_contribution_pct = c(60, 0, 40)
    ),
    holding_positioning = tibble(
      ticker = c("AAA", "BBB", "ZZZ"),
      name = c("Alpha Inc", "Beta SA", "Zed Corp"),
      weight_pct = c(5, 1.5, 0.5), sector = c("Information Technology", "Financials", "Energy"),
      volatility_pct = c(30, 20, 25), annual_return = c(40, 12, -5),
      quadrant = factor(c("Higher risk, higher return", "Improves risk/return", "Drags risk/return")),
      risk_return_score = c(1, 2, 3), weighted_impact = c(5, 3, -1)
    ),
    ticker_summary = tibble(ticker = c("AAA", "BBB", "ZZZ", "CASH_EUR"), index = c("FundX", "FundY", "FundX", "Cash"))
  )
}

sel <- function(...) {
  x <- c(...)
  tibble(ticker = names(x), selected_because = unname(x))
}

test_that("the export keeps its documented column layout", {
  f <- export_fixture()
  out <- build_multiagent_export(f$risk_contribution, f$holding_positioning,
                                 sel(AAA = "weight", CASH_EUR = "building block", BBB = "risk/return pick"),
                                 f$ticker_summary)

  expect_named(out, c("ticker", "name", "weight_pct", "sector", "index", "volatility", "vol_source",
                      "market_beta", "risk_contribution_pct", "annual_return", "quadrant",
                      "weighted_impact", "positioning_source"))
})

test_that("rows are exactly the selection, heaviest first, with the reason carried through", {
  f <- export_fixture()
  out <- build_multiagent_export(f$risk_contribution, f$holding_positioning,
                                 sel(AAA = "weight + risk", CASH_EUR = "building block", BBB = "risk/return pick"),
                                 f$ticker_summary)

  expect_equal(out$ticker, c("AAA", "CASH_EUR", "BBB"))  # ZZZ is in the views but not selected
  expect_equal(out$positioning_source, c("weight + risk", "building block", "risk/return pick"))
})

test_that("unpriced rows keep NA return data, rows outside the top-20 table fall back to the other views", {
  f <- export_fixture()
  out <- build_multiagent_export(f$risk_contribution, f$holding_positioning,
                                 sel(AAA = "weight", CASH_EUR = "building block", BBB = "risk/return pick"),
                                 f$ticker_summary)

  cash <- out[out$ticker == "CASH_EUR", ]
  expect_true(is.na(cash$annual_return) && is.na(cash$quadrant) && is.na(cash$weighted_impact))
  expect_equal(cash$vol_source, "Manual")

  bbb <- out[out$ticker == "BBB", ]
  expect_equal(bbb$name, "Beta SA")
  expect_equal(bbb$weight_pct, 1.5)
  expect_equal(bbb$sector, "Financials")
  expect_equal(bbb$index, "FundY")          # from ticker_summary: risk_contribution has no row
  expect_equal(bbb$volatility, 20)
  expect_equal(bbb$vol_source, "Historical")
  expect_type(out$quadrant, "character")
})

test_that("a selected holding that neither view has is filled from ticker_summary, without invented figures", {
  f <- export_fixture()
  f$ticker_summary <- tibble(ticker = c("AAA", "BBB", "ZZZ", "CASH_EUR", "NOPRICE"),
                             index = c("FundX", "FundY", "FundX", "Cash", "FundZ"),
                             name = c("Alpha Inc", "Beta SA", "Zed Corp", "Cash", "No Price Ltd"),
                             weight_pct = c(5, 1.5, 0.5, 4, 0.4),
                             sector = c("Information Technology", "Financials", "Energy", "Cash & Equivalents", "Utilities"))
  out <- build_multiagent_export(f$risk_contribution, f$holding_positioning,
                                 sel(AAA = "weight", NOPRICE = "weight"), f$ticker_summary)

  np <- out[out$ticker == "NOPRICE", ]
  expect_equal(c(np$name, np$sector, np$index), c("No Price Ltd", "Utilities", "FundZ"))
  expect_equal(np$weight_pct, 0.4)
  expect_true(is.na(np$volatility) && is.na(np$annual_return) && is.na(np$market_beta))
})

test_that("extra_beta fills the beta of rows the top-20 table lacks, without overriding an existing one", {
  f <- export_fixture()
  out <- build_multiagent_export(f$risk_contribution, f$holding_positioning,
                                 sel(AAA = "weight", BBB = "risk"), f$ticker_summary,
                                 extra_beta = tibble(ticker = c("AAA", "BBB"), market_beta = c(9, 0.7)))

  expect_equal(out$market_beta[out$ticker == "AAA"], 1.2)    # the top-20 table's value stays
  expect_equal(out$market_beta[out$ticker == "BBB"], 0.7)
})

test_that("a missing input column fails with its name, not a generic dplyr error", {
  f <- export_fixture()
  broken <- dplyr::select(f$risk_contribution, -market_beta)

  expect_error(
    build_multiagent_export(broken, f$holding_positioning, sel(AAA = "weight"), f$ticker_summary),
    "risk_contribution.*missing column.*market_beta"
  )
  expect_error(
    build_multiagent_export(f$risk_contribution, f$holding_positioning, tibble(ticker = "AAA"), f$ticker_summary),
    "selection.*missing column.*selected_because"
  )
})

test_that("an empty selection is an error, not an empty workbook", {
  f <- export_fixture()

  expect_error(
    build_multiagent_export(f$risk_contribution, f$holding_positioning, tibble(ticker = character(0), selected_because = character(0)), f$ticker_summary),
    "export is empty"
  )
})

test_that("volatility comes from the same window as the return and quadrant when positioning data exists", {
  f <- export_fixture()
  f$holding_positioning$volatility_pct[f$holding_positioning$ticker == "AAA"] <- 28   # shared window; risk_contribution says 30

  out <- build_multiagent_export(f$risk_contribution, f$holding_positioning,
                                 sel(AAA = "weight", CASH_EUR = "building block"), f$ticker_summary)

  expect_equal(out$volatility[out$ticker == "AAA"], 28)
  expect_equal(out$volatility[out$ticker == "CASH_EUR"], 0)     # no positioning row: the manual value stays
})

## ---- select_export_tickers() ------------------------------------------------------------------

selection_fixture <- function() {
  # 12 equities with falling weight, gold, cash. Look-through risk: E01-E03 carry most of it,
  # E09 (light) is a big risk contributor, E11/E12 hedge.
  tickers <- sprintf("E%02d", 1:12)
  ts <- tibble(
    ticker = c(tickers, "GLD", "CASH_EUR"),
    weight_pct = c(2, 1.5, 1, 0.8, 0.6, 0.4, 0.3, 0.2, 0.1, 0.08, 0.05, 0.04, 20, 5),
    asset_class = c(rep("Equities", 12), "Gold", "Cash")
  )
  risk <- tibble(
    ticker = c(tickers, "GLD"),
    risk_contribution_pct = c(30, 20, 15, 5, 4, 3, 2, 1, 12, 0.5, -3, -1, 7)
  )
  list(ts = ts, risk = risk)
}

test_that("every non-equity block is always listed, and the reason names each rule that applies", {
  f <- selection_fixture()
  out <- select_export_tickers(f$ts, f$risk, pick_tickers = "E10", min_rows = 1)

  expect_true(all(c("GLD", "CASH_EUR") %in% out$ticker))
  expect_equal(out$selected_because[out$ticker == "GLD"], "building block")
  expect_equal(out$selected_because[out$ticker == "E01"], "weight + risk")
  expect_equal(out$selected_because[out$ticker == "E10"], "risk/return pick")
})

test_that("the weight rule takes equities at or above the threshold, nothing below it", {
  f <- selection_fixture()
  out <- select_export_tickers(f$ts, f$risk, min_weight_pct = 0.5, risk_coverage_pct = 0.01, n_hedges = 0, min_rows = 1)

  weight_rows <- out$ticker[grepl("weight", out$selected_because)]
  expect_setequal(weight_rows, c("E01", "E02", "E03", "E04", "E05"))
})

test_that("the risk rule lists the fewest equities reaching the coverage of the POSITIVE risk, whatever their weight", {
  f <- selection_fixture()
  # positive equity risk 92.5 (30+20+15+12+5+4+3+2+1+0.5); cumulative 32 %, 54 %, 70 %, 83 % at E01, E02, E03, E09
  out <- select_export_tickers(f$ts, f$risk, min_weight_pct = 99, n_hedges = 0, risk_coverage_pct = 80, min_rows = 1)

  risk_rows <- out$ticker[grepl("risk", out$selected_because)]
  expect_setequal(risk_rows, c("E01", "E02", "E03", "E09"))
  expect_true("E09" %in% risk_rows)                              # weight 0.1 %, risk 12 %
})

test_that("hedges are the most negative equity contributors and are named as such", {
  f <- selection_fixture()
  out <- select_export_tickers(f$ts, f$risk, min_weight_pct = 99, risk_coverage_pct = 1, n_hedges = 2, min_rows = 1)

  expect_equal(out$selected_because[out$ticker == "E11"], "hedge")
  expect_equal(out$selected_because[out$ticker == "E12"], "hedge")
  expect_false("E10" %in% out$ticker)                            # positive contributor, light, no rule applies
})

test_that("the cap drops weight-only rows first (lightest first) and never a risk, hedge or block row", {
  f <- selection_fixture()
  out <- select_export_tickers(f$ts, f$risk, min_weight_pct = 0.05, risk_coverage_pct = 60, n_hedges = 1,
                               min_rows = 1, max_rows = 8)

  # kept regardless of the cap: GLD, CASH_EUR (blocks), E01-E03 (risk to 60 %... 30,50,65 -> E01-E03), E11 (hedge)
  expect_true(all(c("GLD", "CASH_EUR", "E01", "E02", "E03", "E11") %in% out$ticker))
  expect_equal(nrow(out), 8)
  # of the weight-only rows (E04..E10 at >= 0.05 %) the heaviest survive
  expect_true("E04" %in% out$ticker)
  expect_false("E10" %in% out$ticker)
})

test_that("below the floor the heaviest unlisted equities are added", {
  f <- selection_fixture()
  out <- select_export_tickers(f$ts, f$risk, min_weight_pct = 99, risk_coverage_pct = 1, n_hedges = 0, min_rows = 6)

  expect_equal(nrow(out), 6)                                      # GLD, CASH, E01 (risk) + 3 heaviest others
  expect_true(all(c("E02", "E03", "E04") %in% out$ticker))
})

test_that("the result is heaviest first, one row per ticker, and needs its columns", {
  f <- selection_fixture()
  out <- select_export_tickers(f$ts, f$risk, min_rows = 1)

  expect_equal(out$ticker[1], "GLD")
  expect_false(anyDuplicated(out$ticker) > 0)
  expect_error(select_export_tickers(dplyr::select(f$ts, -asset_class), f$risk), "ticker_summary.*asset_class")
  expect_equal(nrow(select_export_tickers(f$ts, f$risk[0, ], min_rows = 1)) >= 2, TRUE)   # no risk data: weight + blocks only
})

## ---- build_multiagent_sheets() ----------------------------------------------------------------

sheets_fixture <- function() {
  f <- export_fixture()
  positions <- build_multiagent_export(f$risk_contribution, f$holding_positioning,
                                       sel(AAA = "weight", CASH_EUR = "building block", BBB = "risk/return pick"),
                                       f$ticker_summary)
  list(
    positions = positions,
    ticker_summary = tibble(ticker = c("AAA", "BBB", "CASH_EUR"), asset_class = c("Equities", "Equities", "Cash")),
    mrc_table = tibble(ticker = c("AAA", "BBB"), risk_contribution_pct = c(70, 30)),
    sleeve_model = list(
      sleeves = tibble(sleeve = c("FUND1", "CASH_EUR"), label = c("Fund One", "Cash"), kind = c("fund", "synthetic"),
                       weight = c(90, 10), volatility_pct = c(18, 0), annual_return_pct = c(9, 2),
                       risk_share_pct = c(100, 0), risk_vs_weight = c(1.11, 0),
                       return_contribution_pct = c(6.9, 0.2)),
      window = as.Date(c("2025-10-10", "2026-10-02")), n_steps = 240, coverage_pct = 99.9,
      volatility_pct = 16.2, annual_return_pct = 8.1, cumulative_return_pct = 7.5,
      max_drawdown_pct = -12.3, diversification_ratio = 1.1
    ),
    asset_classes = tibble(asset_class = c("Equities", "Cash"), weight = c(90, 10)),
    equity_concentration = concentration_metrics(c(50, 30, 20)),
    sectors = tibble(sector = c("Information Technology", "Financials"), weight_pct = c(60, 30),
                     share_of_equities_pct = c(66.7, 33.3)),
    countries = tibble(alpha3 = "USA", country_name = "United States", weight_pct = 90, equity_share_pct = 100),
    currency_exposure = tibble(currency = c("USD", "EUR"), weight = c(60, 40), holdings = c(5L, 2L)),
    drawdown = tibble(ticker = "AAA", max_drawdown_pct = -20,
                      max_drawdown_peak_date = as.Date("2026-01-15"), max_drawdown_trough_date = as.Date("2026-04-02"),
                      current_drawdown_pct = -5, peak_date = as.Date("2026-09-01"), momentum_pct = 3),
    caveats = c("- **Short window.** x", "- **Currency is the trading currency** y"),
    meta = list(scenario = "complete", data_as_of = as.Date("2026-10-02"), rendered_at = as.POSIXct("2026-10-03 09:19:00", tz = "UTC"),
                equity_weight_pct = 90, lookthrough_coverage_pct = 54.2, euler_coverage_pct = 51.7)
  )
}

build_fixture_sheets <- function(f = sheets_fixture()) {
  do.call(build_multiagent_sheets, f)
}

test_that("the workbook has the positions sheet first and the documented companion sheets", {
  sheets <- build_fixture_sheets()

  expect_named(sheets, c("positions", "summary", "sleeves", "asset_classes", "sectors", "countries",
                         "currency", "drawdown", "caveats"))
})

test_that("the positions sheet keeps its layout; the new columns are only appended", {
  sheets <- build_fixture_sheets()

  expect_equal(names(sheets$positions)[1:13],
               c("ticker", "name", "weight_pct", "sector", "index", "volatility", "vol_source",
                 "market_beta", "risk_contribution_pct", "annual_return", "quadrant",
                 "weighted_impact", "positioning_source"))
  expect_equal(names(sheets$positions)[14:16], c("asset_class", "euler_risk_share_pct", "sleeve_risk_share_pct"))
  expect_equal(nrow(sheets$positions), 3)                       # no row gained or lost by the joins
  expect_equal(sheets$positions$asset_class[sheets$positions$ticker == "CASH_EUR"], "Cash")
  expect_equal(sheets$positions$euler_risk_share_pct[sheets$positions$ticker == "AAA"], 70)
  expect_true(is.na(sheets$positions$euler_risk_share_pct[sheets$positions$ticker == "CASH_EUR"]))
})

test_that("only building blocks of the portfolio (not fund constituents) get a whole-portfolio (sleeve) risk share", {
  f <- sheets_fixture()
  f$sleeve_model$sleeves <- tibble(sleeve = c("AAA", "FUND1"), label = c("AAA - Alpha", "Fund One"),
                                   kind = c("direct", "fund"), weight = c(10, 90), volatility_pct = c(30, 12),
                                   annual_return_pct = c(5, 6), risk_share_pct = c(25, 75), risk_vs_weight = c(2.5, 0.83),
                                   return_contribution_pct = c(1, 5))
  out <- build_fixture_sheets(f)$positions

  expect_equal(out$sleeve_risk_share_pct[out$ticker == "AAA"], 25)
  expect_true(is.na(out$sleeve_risk_share_pct[out$ticker == "BBB"]))   # a fund constituent, not a sleeve
  expect_equal(nrow(out), 3)
})

test_that("summary is one typed row: schema version, window, whole-portfolio and equity-concentration figures", {
  s <- build_fixture_sheets()$summary

  expect_equal(nrow(s), 1)
  expect_equal(s$schema_version, MULTIAGENT_SCHEMA_VERSION)
  expect_equal(s$data_as_of, "2026-10-02")
  expect_equal(s$window_start, "2025-10-10")
  expect_equal(s$window_months, as.numeric(as.Date("2026-10-02") - as.Date("2025-10-10")) / 30.44)
  expect_equal(s$portfolio_vol_pct, 16.2)
  expect_equal(s$max_drawdown_pct, -12.3)
  # HHI of shares 0.5/0.3/0.2 = 0.38, exported on the 0-10000 scale
  expect_equal(s$equity_hhi, 3800)
  expect_equal(s$equity_top1_pct, 50)
  expect_equal(s$equity_top5_pct, 100)
  expect_type(s$rendered_at, "character")
})

test_that("sleeves, currency and caveats are copied in the shape the reader expects", {
  sheets <- build_fixture_sheets()

  expect_named(sheets$sleeves, c("sleeve", "label", "kind", "weight_pct", "volatility_pct",
                                 "annual_return_pct", "risk_share_pct", "risk_vs_weight", "return_contribution_pct"))
  expect_equal(sheets$sleeves$weight_pct, c(90, 10))
  expect_equal(sheets$currency$weight_pct, c(60, 40))           # re-expressed as % of the exported total
  expect_equal(sheets$caveats$order, 1:2)
  expect_equal(sheets$caveats$text[1], "- **Short window.** x")
  expect_type(sheets$drawdown$peak_date, "character")
})

test_that("the drawdown sheet names the high and the low of the deepest fall apart from the current high", {
  d <- build_fixture_sheets()$drawdown

  expect_named(d, c("ticker", "max_drawdown_pct", "max_drawdown_peak_date", "max_drawdown_trough_date",
                    "current_drawdown_pct", "peak_date", "momentum_pct"))
  expect_equal(d$max_drawdown_peak_date, "2026-01-15")
  expect_equal(d$max_drawdown_trough_date, "2026-04-02")
  expect_equal(d$peak_date, "2026-09-01")          # the high the CURRENT drawdown is measured from
})

test_that("the asset-class sheet carries the risk share and return contribution summed from the sleeves", {
  f <- sheets_fixture()
  f$ticker_summary <- tibble(ticker = c("AAA", "BBB", "CASH_EUR", "GLD", "MSFT"),
                             asset_class = c("Equities", "Equities", "Cash", "Gold", "Equities"))
  f$sleeve_model$sleeves <- tibble(
    sleeve = c("FUND1", "FUND2", "MSFT", "GLD", "CASH_EUR"), label = c("Fund One", "Fund Two", "MSFT", "Gold", "Cash"),
    kind = c("fund", "fund", "direct", "direct", "synthetic"), weight = c(30, 25, 5, 30, 10),
    volatility_pct = 10, annual_return_pct = 5, risk_share_pct = c(25, 20, 5, 50, 0), risk_vs_weight = 1,
    return_contribution_pct = c(7, 5, 1, 2, 0.2))
  f$asset_classes <- tibble(asset_class = c("Equities", "Gold", "Cash", "Crypto"), weight = c(60, 30, 10, 0))
  a <- build_fixture_sheets(f)$asset_classes

  expect_named(a, c("asset_class", "weight_pct", "risk_share_pct", "return_contribution_pct"))
  expect_equal(a$risk_share_pct[a$asset_class == "Equities"], 50)           # both funds + MSFT
  expect_equal(a$return_contribution_pct[a$asset_class == "Equities"], 13)
  expect_equal(a$risk_share_pct[a$asset_class == "Gold"], 50)
  expect_true(is.na(a$risk_share_pct[a$asset_class == "Crypto"]))            # no sleeve in that class
  expect_equal(sum(a$risk_share_pct, na.rm = TRUE), 100)
})

test_that("an empty sleeve model is an error, and a missing input column is named", {
  f <- sheets_fixture()
  f$sleeve_model$sleeves <- f$sleeve_model$sleeves[0, ]
  expect_error(build_fixture_sheets(f), "no sleeves")

  f <- sheets_fixture()
  f$drawdown <- dplyr::select(f$drawdown, -momentum_pct)
  expect_error(build_fixture_sheets(f), "drawdown.*missing column.*momentum_pct")
})

test_that("the workbook round-trips through xlsx with every sheet readable", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  path <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(build_fixture_sheets(), path)

  expect_equal(readxl::excel_sheets(path),
               c("positions", "summary", "sleeves", "asset_classes", "sectors", "countries",
                 "currency", "drawdown", "caveats"))
  expect_equal(readxl::read_excel(path, sheet = "summary")$schema_version, 2)
})
