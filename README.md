# R-based portfolio analysis toolkit

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
![R](https://img.shields.io/badge/R-4.x-276DC3?logo=r)

R-based analysis toolkit for tracking and evaluating "look-through" investment portfolios — portfolios built from ETFs (each expanded to its underlying constituent holdings) plus individual stocks. It pulls historical price/volatility data from the [EODHD](https://eodhd.com/) API, computes risk and diversification metrics, and renders a parametrized HTML/PDF/Word report per portfolio.

## Why

Most brokerage dashboards show your portfolio as a list of funds, not as what you actually own underneath them. Two ETFs with different names can both be 20% NVIDIA once you look through their holdings — and you won't see that concentration risk until you aggregate at the constituent level. This project does that look-through aggregation, then layers on:

- **Concentration & diversification metrics** — Herfindahl-Hirschman Index (HHI) at the ticker, sector, and country level
- **Risk contribution** — a volatility-weighted "tornado chart" showing which holdings actually drive portfolio risk, using historical volatility where available and falling back to sector-level estimates otherwise
- **Correlation analysis** — a price-correlation heatmap across all holdings
- **Geographic & sector composition** — bar charts, a world map, and a sector sunburst
- **Performance** — cumulative returns vs. benchmarks (S&P 500, MSCI World, MSCI Emerging Markets), a Monte Carlo-simulated efficient frontier, and a sector risk/return bubble chart

It's built around a two-portfolio setup (see `config/portfolio_configs.csv`) but the fund-cleaner pattern and report generalize to any number of portfolios — see [Adding a New Portfolio](#adding-a-new-portfolio) below.

**Companion project:** [multi-agent-portfolio-analysis](https://github.com/andreas678/multi-agent-portfolio-analysis) is a multi-agent analysis system built alongside this pipeline.

> **Note on data:** this repo ships the code and pipeline only. Raw fund exports, cleaned holdings, price caches, and rendered reports are gitignored — they contain real portfolio data and are never committed. You bring your own fund exports and EODHD API key; see [Prerequisites](#prerequisites) and [Quick Start](#quick-start-first-time-setup).

------------------------------------------------------------------------

## Prerequisites

**R packages** — install once:

``` r
install.packages(c(
  "tidyverse", "readxl", "writexl", "here", "countrycode",
  "maps", "kableExtra", "plotly", "httr", "jsonlite",
  "scales", "rmarkdown", "stringdist"
))
```

(`stringdist` is only used by `CTLuxEuroSmComp.R`'s fuzzy ISIN-less ticker matching.)

**EODHD API key** — required for price/volatility data and ISIN-to-ticker resolution. Add it to your `.Renviron` file (create it at `~/.Renviron` if it doesn't exist):

```         
EODHD_API_KEY=your_key_here
```

**FINNHUB API key** — optional, only needed for Phase 3 of `tickerliste_maintenance.R` (sector data for US-listed equities):

```         
FINNHUB_API_KEY=your_key_here
```

Then restart R (`Ctrl+Shift+F10` in RStudio) so the variables are picked up by `Sys.getenv(...)`.

------------------------------------------------------------------------

## Quick Start (first-time setup)

Run these steps **in order** the first time you set up the project.

### Step 1 — Open the project

Open `portfolio.Rproj` in RStudio. All paths are managed by `here::here()`, so this is required for the project to resolve files correctly.

### Step 2 — Export and clean fund holdings

Download the latest holdings export (`.xlsx` or `.csv`) from each ETF provider's website and place them in `data/raw/`. Then run the corresponding cleaner script for each fund:

``` r
source("scripts/AmundiCoreWorld.R")
source("scripts/DekaEuroStoxx50.R")
source("scripts/XtrackersWorldExUSA.R")
# ... and so on for each fund
```

Each script reads the raw export, resolves missing tickers via the shared `resolve_and_upsert_tickers()` helper (`scripts/utils/data_loaders.R`) against the EODHD ISIN search API, enriches records with country codes (ISO alpha-3), and writes a standardized file to `data/clean/<FundName>.xlsx`.

Three funds don't follow this pattern:

- **`iSharesEMIMI.R`** fetches holdings live via URL — no manual download or raw file needed, just run the script.
- **`CTLuxEuroSmComp.R`** has no ISIN in its raw export; tickers are matched via fuzzy name-matching (`stringdist`) instead.
- **`single_tickers.R`** and **`CashAndBonds.R`** have no raw export at all — direct stock holdings and cash/low-risk-bonds allocations are maintained as small tables directly inside the scripts. Edit the table, then re-run the script:

``` r
source("scripts/single_tickers.R")   # direct stock holdings
source("scripts/CashAndBonds.R")   # cash / low-risk-bonds pseudo-holdings
```

### Step 3 — Update the ticker master list

`meta/tickerliste.xlsx` is the master lookup table mapping every ticker to its sector and industry classification. Keep it current using:

``` r
source("scripts/tickerliste_maintenance.R")
source("scripts/updating_tickerlist.R")
```

`tickerliste_maintenance.R`'s last phase harmonizes raw sector labels (German fund exports, Finnhub sub-industry names, etc.) onto the 11 canonical GICS sector names the report's sector charts expect — without it, mismatched labels silently fall through to the 22% default volatility estimate instead of a proper sector-level one.

Without this file, the report renders but sector-level analysis will be sparse.

### Step 4 — Render the report once (bootstraps the portfolio cache)

`get_price_data.R` derives its ticker universe from the rendered `portfolio.xlsx`/`portfolio_b.xlsx` files, so you need to render the report at least once before refreshing prices:

``` r
rmarkdown::render("scripts/portfolio_report.Rmd",
                   params = list(portfolio = "portfolio_a"))
```

Or in RStudio: **Knit ▾ → Knit with Parameters...** and pick `portfolio_a` or `portfolio_b` from the dropdown.

### Step 5 — Fetch historical prices

With the portfolio cache bootstrapped, run the price fetcher to populate `meta/closing_prices.csv`:

``` r
source("scripts/get_price_data.R")
```

This calls the EODHD API incrementally — each ticker is only fetched for the date range it is missing, so subsequent runs are fast. It requires `EODHD_API_KEY` to be set (see Prerequisites).

### Step 6 — Re-render the report

Knit again. Now the report will have full historical volatility and correlation data:

``` r
# Render a named output for each portfolio
rmarkdown::render("scripts/portfolio_report.Rmd",
                   params = list(portfolio = "portfolio_a"),
                   output_file = "../results/portfolio_a_report.html")

rmarkdown::render("scripts/portfolio_report.Rmd",
                   params = list(portfolio = "portfolio_b"),
                   output_file = "../results/portfolio_b_report.html")
```

> **Note:** Both portfolios default to the same output filename (`portfolio_report.html`) when knitting interactively. Use an explicit `output_file` to keep both versions side by side.

------------------------------------------------------------------------

## Periodic Maintenance (ongoing)

| Task | Frequency | How |
|----|----|----|
| Refresh fund holdings | Monthly / quarterly | Re-run the per-fund cleaner scripts (Step 2) after downloading new exports |
| Update ticker master list | When new tickers appear | Run `tickerliste_maintenance.R` |
| Refresh price cache | Weekly | `source("scripts/get_price_data.R")` |
| Re-render report | After any of the above | Knit `portfolio_report.Rmd` |

------------------------------------------------------------------------

## Testing

`price_cache.R`, `get_volatility_data.R`, and `portfolio_returns.R` are pure, side-effect-free functions with a `testthat` suite covering them:

``` r
install.packages("testthat")
```

``` bash
Rscript tests/run_tests.R
```

This project isn't an R package, so there's no `devtools::test()` — run the file above directly. These are the only scripts covered; the per-fund cleaners and the report itself have no automated tests (verify those by rendering and inspecting the output, as noted above).

------------------------------------------------------------------------

## Data Pipeline (detailed)

```         
data/raw/          ──(per-fund scripts)──►  data/clean/*.xlsx
                                                    │
meta/tickerliste.xlsx ──────────────────────────────┤
                                                    │
meta/closing_prices.csv  ◄──(get_price_data.R)──────┤
         │                                          │
         └──(price_cache.R: load_cached_prices())───┤
                                                    │
config/portfolio_configs.csv ───────────────────────┤
                                                    │
                                         portfolio_report.Rmd
                                                    │
                          results/*.html / .pdf / .docx
                          data/clean/correlation_matrix_<portfolio>.csv
```

0.  **`scripts/utils/`** — shared helpers sourced by the steps below: `data_loaders.R` (`resolve_and_upsert_tickers()`, used by 6 of 8 per-fund cleaners), `sector_taxonomy.R` (single source of truth for the 11 GICS sectors plus the Cash/Bonds pseudo-sectors and their pseudo-tickers), and `logging.R` (persists ISIN-resolution and matching warnings to `logs/pipeline.log`, so they survive a headless `Rscript` run instead of only appearing in an interactive console).
1.  **`data/raw/`** — fund holdings exported manually from each ETF provider's website (xlsx/csv, gitignored).
2.  **Per-fund cleaners** (`scripts/AmundiCoreWorld.R`, `DekaEuroStoxx50.R`, etc.) — standardize raw exports and write to `data/clean/<Fund>.xlsx`.
3.  **`meta/tickerliste.xlsx`** — master ticker → country/sector/industry lookup, maintained by `tickerliste_maintenance.R` and `updating_tickerlist.R`.
4.  **`get_price_data.R` + `price_cache.R`** — fetch daily adjusted closing prices from EODHD into `meta/closing_prices.csv`. `price_cache.R`'s `load_cached_prices()` is the single canonical reader for that cache — every downstream script and report chunk uses it.
5.  **`get_volatility_data.R`** — computes annualized historical volatility from cached prices (`calculate_volatility_from_prices`, `batch_calculate_volatility`).
6.  **`portfolio_returns.R`** — pure helper functions (`prices_to_wide()`, `compute_daily_log_returns()`, `compute_portfolio_return_series()`, `rebase_to_one()`) that turn cached prices into a portfolio-weighted daily return series, used by the report's cumulative-returns and efficient-frontier sections.
7.  **`config/portfolio_configs.csv`** — per-portfolio fund lists, allocation weights, and display metadata (one row per fund); loaded by `portfolio_report.Rmd` at render time (see "Adding a New Portfolio" below).
8.  **`portfolio_report.Rmd`** — main entry point. Loads fund holdings, scales by fund weight, combines into one `portfolio` data frame, and computes risk/concentration metrics, the volatility-weighted tornado chart, a correlation heatmap (also exported to `data/clean/correlation_matrix_<portfolio>.csv`), geographic/sector breakdowns, and a data-quality summary.

------------------------------------------------------------------------

## Project Structure

```         
portfolio/
├── config/
│   └── portfolio_configs.csv       # Per-portfolio fund lists/weights (not gitignored)
├── scripts/
│   ├── portfolio_report.Rmd        # Main report (knit this)
│   ├── get_price_data.R            # EODHD price fetcher
│   ├── price_cache.R               # Canonical price cache reader
│   ├── get_volatility_data.R       # Volatility calculation helpers
│   ├── portfolio_returns.R         # Return-series helpers (report Section 7)
│   ├── tickerliste_maintenance.R   # Ticker master list upkeep
│   ├── updating_tickerlist.R       # Ticker list updater
│   ├── AmundiCoreWorld.R           # Per-fund cleaner scripts
│   ├── iSharesEMIMI.R              #   fetches live via URL
│   ├── CTLuxEuroSmComp.R           #   fuzzy ISIN-less ticker matching
│   ├── single_tickers.R            #   manually maintained, no raw export
│   ├── CashAndBonds.R              #   manually maintained, no raw export
│   ├── ...                         #   (one per fund)
│   └── utils/                      # Shared helpers (see Data Pipeline below)
│       ├── data_loaders.R
│       ├── sector_taxonomy.R
│       └── logging.R
├── tests/
│   ├── run_tests.R                 # Rscript tests/run_tests.R
│   └── testthat/                   # Tests for the pure helper scripts above
├── meta/                           # Ticker list, price cache (gitignored)
│   ├── tickerliste.xlsx
│   ├── closing_prices.csv
│   └── sectormapping.csv
├── data/
│   ├── raw/                        # Raw fund exports (gitignored)
│   └── clean/                      # Standardized holdings files (gitignored)
├── outputs/                        # Generated charts (gitignored)
├── results/                        # Rendered reports (gitignored)
└── logs/                           # pipeline.log, price_fetch.log (gitignored)
```

------------------------------------------------------------------------

## Adding a New Portfolio

To add a third portfolio, add rows to `config/portfolio_configs.csv` — one
row per fund, columns `portfolio,display_name,output_file,fund_file,weight,label`.
Leave `weight` blank for a fund that should use its own weight column as-is
(individual securities, Cash, Low Risk Bonds):

```
portfolio,display_name,output_file,fund_file,weight,label
newname,New Person's Portfolio,NewPerson.xlsx,single_tickers_new.xlsx,,Individual Securities
newname,New Person's Portfolio,NewPerson.xlsx,SomeFund.xlsx,0.60,Some Fund
```

`display_name` and `output_file` must be repeated identically on every row
for that portfolio — `portfolio_report.Rmd` fails fast with a clear error
if they don't match.

Then add `"newname"` to the `choices` list in the YAML params block at the
top of `scripts/portfolio_report.Rmd`, **and** add a `newname = "portfolio_report_newname.html"`
entry to `report_targets` in `scripts/render_reports.R` — both are separate
hardcoded lists of portfolio ids that the CSV does not drive.

------------------------------------------------------------------------

## Notes

- See `AGENTS.md` for detailed API notes, past analysis findings, and volatility benchmarks by sector.
- For non-US tickers (Korea, Hong Kong, Taiwan) the EODHD API requires an exchange suffix — e.g. `.TW` (Taiwan), `.KO` (Korea Exchange stock market), `.KQ` (Korea Exchange Kosdaq), `.HK` (Hong Kong). Note this differs from Yahoo's Korea convention (`.KS`). `iSharesEMIMI.R`'s `exchange_suffix_map` applies these automatically based on the fund export's exchange column; unmapped exchanges are left bare and flagged via a console warning.
- Non-fatal warnings (unresolved ISINs, unmapped exchanges, fuzzy-match losses) are persisted to `logs/pipeline.log` in addition to the console, so they're not lost when a script runs headlessly via `Rscript`.

------------------------------------------------------------------------

## License

MIT — see [LICENSE](LICENSE).
