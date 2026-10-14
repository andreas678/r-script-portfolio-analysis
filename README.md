# R-based portfolio analysis toolkit

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
![R](https://img.shields.io/badge/R-4.x-276DC3?logo=r)

R-based analysis toolkit for tracking and evaluating "look-through" investment portfolios — portfolios built from ETFs (each expanded to its underlying constituent holdings) plus individual stocks. It pulls historical price/volatility data from the [EODHD](https://eodhd.com/) API, computes risk and diversification metrics, and renders a parametrized HTML/PDF/Word report per portfolio.

## Why

Most brokerage dashboards show your portfolio as a list of funds, not as what you actually own underneath them. Two ETFs with different names can both be 20% NVIDIA once you look through their holdings — and you won't see that concentration risk until you aggregate at the constituent level. This project does that look-through aggregation, then layers on:

- **Concentration & diversification metrics** — Herfindahl-Hirschman Index (HHI) at the ticker, sector, and country level, computed on the equity part of the portfolio
- **Whole-portfolio risk & return (sleeve model)** — the portfolio as a handful of building blocks (each fund's own EUR price series, directly held securities, and assumed-return Cash / Low Risk Bonds sleeves), with portfolio volatility, drawdown and Euler risk shares
- **Risk contribution** — a volatility-weighted "tornado chart" showing which holdings actually drive portfolio risk, using historical volatility where available and falling back to sector-level estimates otherwise
- **Correlation analysis** — a price-correlation heatmap across all holdings
- **Geographic, sector & currency composition** — bar charts, a world map, a sector sunburst, and exposure by trading currency
- **Performance** — cumulative returns vs. benchmarks (S&P 500, MSCI World, MSCI Emerging Markets), a Monte Carlo-simulated efficient frontier, sector risk/return and attribution charts, and holding-level risk/return positioning
- **Look-through validation** — each fund's holdings rebuilt into a return series and compared with the fund's own price series (tracking error, beta, R²)
- **Scenarios** — re-run the whole report with positions excluded (e.g. without gold, ETFs only), the excluded weight reallocated proportionally; scenarios are defined in `config/scenarios.csv`

It's built around a two-portfolio setup (see `config/portfolio_configs.csv`) but the fund-cleaner pattern and report generalize to any number of portfolios — see [Adding a New Portfolio](#adding-a-new-portfolio) below. Prices are converted to EUR, so all risk and return figures are in EUR.

**Companion project:** [multi-agent-portfolio-analysis](https://github.com/andreas678/multi-agent-portfolio-analysis) is a multi-agent analysis system built alongside this pipeline.

> **Note on data:** this repo ships the code, the pipeline, the definitional config in `config/`, and the market-data caches in `meta/` (ticker list, prices, FX rates — market data, not personal). Raw fund exports, cleaned holdings, and rendered reports are gitignored — they contain real portfolio data and are never committed. You bring your own fund exports and EODHD API key; see [Prerequisites](#prerequisites) and [Quick Start](#quick-start-first-time-setup).

------------------------------------------------------------------------

## Prerequisites

**R packages** — install once:

``` r
install.packages(c(
  "tidyverse", "readxl", "writexl", "here", "countrycode",
  "maps", "kableExtra", "plotly", "httr", "jsonlite",
  "scales", "rmarkdown", "stringdist", "xml2",
  "testthat", "withr"
))
```

(`stringdist` is only used by `CTLuxEuroSmComp.R`'s fuzzy ISIN-less ticker matching, `xml2` by the iShares loaders, and `testthat`/`withr` by the test suite.)

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

Download the latest holdings export (`.xlsx` or `.csv`) from each ETF provider's website and place them in `data/raw/`. Then run the corresponding cleaner script for each fund (the shipped scripts cover Amundi, Deka, Xtrackers, Vanguard, iShares and CT (Lux); copy one as the template for a new fund):

``` r
source("scripts/AmundiCoreWorld.R")
source("scripts/DekaEuroStoxx50.R")
source("scripts/XtrackersWorldExUSA.R")
# ... and so on for each fund
```

Each script reads the raw export, resolves missing tickers via the shared `resolve_and_upsert_tickers()` helper (`scripts/utils/data_loaders.R`) against the EODHD ISIN search API, enriches records with country codes (ISO alpha-3), and writes a standardized file to `data/clean/<FundName>.xlsx`.

Some scripts don't follow this pattern:

- **`iSharesEMIMI.R`, `iSharesACWI.R`, `iSharesEuro600.R`** fetch holdings live from the iShares API (shared parsing in `scripts/utils/ishares_loaders.R`) — no manual download needed, just run the script. `iSharesWorld.R` works the same way but isn't a portfolio fund itself: it provides the *proxy* holdings for a swap-based ETF (see [Fund proxies](#fund-proxies)).
- **`CTLuxEuroSmComp.R`** has no ISIN in its raw export; tickers are matched via fuzzy name-matching (`stringdist`) instead.
- **`single_tickers.R`** and **`CashAndBonds.R`** have no fund export — direct stock holdings (read from `data/raw/single_tickers.csv`, a semicolon-delimited `ISIN;Gattungsbezeichnung;% Fondsvermögen` file) and cash/low-risk-bonds allocations (a small table inside the script) are maintained by hand. Edit the input, then re-run the script:

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

### Step 4 — Map tickers to trading currencies

EODHD returns every price in its listing's *local* currency, so the cache mixes USD, EUR, HKD, … Build the ticker → currency map first; the price refresh uses it to know which FX rates to fetch, and the report uses it to convert to EUR and to compute currency exposure:

``` r
source("scripts/get_currency_map.R")   # or: Rscript scripts/get_currency_map.R
```

This writes `meta/ticker_currency.csv`. Re-run it after adding tickers.

### Step 5 — Fetch historical prices and FX rates

``` r
source("scripts/get_price_data.R")   # or: Rscript scripts/get_price_data.R
```

The refresh universe is derived directly from the cleaned fund files and `config/portfolio_configs.csv` (holdings at or above 0.1% portfolio weight), plus the benchmark tickers (`SPY`, `URTH`, `EEM`) and each fund's own ETF listing from `config/etf_listings.csv` — so no report render is needed first. It populates `meta/closing_prices.csv` and the EUR FX cache `meta/fx_rates_raw.csv`.

The EODHD calls are incremental — each ticker is only fetched for the date range it is missing, so subsequent runs are fast. It requires `EODHD_API_KEY` to be set (see Prerequisites).

### Step 6 — Render the report

``` r
# One portfolio, complete scenario
rmarkdown::render("scripts/portfolio_report.Rmd",
                   params = list(portfolio = "portfolio_a", scenario = "complete"),
                   output_file = "../results/portfolio_a_report.html")

# Or every portfolio from config/portfolio_configs.csv in one pass -> reports/
Rscript scripts/render_reports.R
```

Or in RStudio: **Knit ▾ → Knit with Parameters...** and pick the portfolio and scenario from the dropdowns.

> **Note:** Interactive knitting writes every portfolio to the same default filename (`portfolio_report.html`). Use an explicit `output_file`, or `render_reports.R`, to keep versions side by side.

Besides the HTML/PDF/Word report, a render writes the combined holdings to `data/clean/<portfolio>.xlsx`, the correlation matrix to `data/clean/correlation_matrix_<portfolio>.csv`, and a multi-agent export to `data/output/` (see [Multi-agent export](#multi-agent-export)).

### Scenarios

`config/scenarios.csv` defines, per portfolio, named scenarios that exclude holdings by exact ticker (`exclude_tickers`), by fund/index label (`exclude_index`, e.g. `Cash`, `Einzelaktien`), or by the synthetic country code of non-country pseudo-holdings (`exclude_alpha3`, e.g. `GOLD`, `CRYPTO`). A row is excluded if it matches any criterion; the excluded weight is reallocated proportionally so the total stays at 100%. Picking a scenario that isn't defined for the chosen portfolio falls back to `complete`, with a visible note in the report. To add a scenario, add a row to the CSV **and** to the `scenario` `choices` list in the Rmd's YAML header.

### Fund proxies

Some ETFs publish holdings that don't represent their index exposure — e.g. a swap-based ETF reports its collateral basket. `config/fund_proxies.csv` maps such a fund to a physically replicated fund tracking the same index; the proxy's holdings (renormalized to 100) are used in its place, and the report flags this wherever it applies. Delete the row to use the fund's own published holdings.

### Fund price series

`config/etf_listings.csv` maps each fund file to its ETF's EODHD listing (ISIN, ticker, trading currency, price scale). These series drive the sleeve model (the fund as one building block) and the look-through validation in the report's Section 7.2. `config/sleeve_assumptions.csv` holds the assumed annual return and volatility for the Cash and Low Risk Bonds sleeves, which have no price history — these are assumptions, not measurements; replace them with your real figures.

### Ticker disambiguation

A fund export's bare ticker (`AI`, `SAN`) is resolved by EODHD on its US exchange, which can silently select a different company's price series. `config/ticker_disambiguation.csv` maps `(ticker, company-name pattern)` to the correct EODHD symbol. The rules are applied when a fund file is loaded, so the cleaned files in `data/clean/` don't need regenerating when a rule changes. `scripts/suggest_exchange_suffixes.R` proposes new rules offline for one fund file; review its proposals, then re-run `get_currency_map.R` and `get_price_data.R`.

### Multi-agent export

Each render also writes a workbook to `data/output/` for the companion [multi-agent-portfolio-analysis](https://github.com/andreas678/multi-agent-portfolio-analysis) system: the positions that matter (largest weights, largest risk contributors, hedges, risk/return picks and all non-equity blocks) plus the report's own whole-portfolio figures and reading caveats, so the agents interpret the report's numbers instead of recomputing them. The logic lives in `scripts/utils/multiagent_export.R`.

------------------------------------------------------------------------

## Periodic Maintenance (ongoing)

| Task | Frequency | How |
|----|----|----|
| Refresh fund holdings | Monthly / quarterly | Re-run the per-fund cleaner scripts (Step 2) after downloading new exports |
| Update ticker master list | When new tickers appear | Run `tickerliste_maintenance.R` |
| Refresh currency map | After new tickers appear | `source("scripts/get_currency_map.R")` |
| Refresh price + FX cache | Weekly | `source("scripts/get_price_data.R")` |
| Re-render report | After any of the above | Knit `portfolio_report.Rmd` or run `scripts/render_reports.R` |

------------------------------------------------------------------------

## Testing

The pure, side-effect-free helpers — `price_cache.R`, `get_volatility_data.R`, `portfolio_returns.R` and most of `scripts/utils/` (portfolio construction, sleeve model, ticker disambiguation, exchange suffixes, currency exposure, sector taxonomy, multi-agent export, report caveats, …) — plus the shape of the `config/*.csv` files are covered by a `testthat` suite. No test calls EODHD or Finnhub.

``` bash
Rscript tests/run_tests.R
```

This project isn't an R package, so there's no `devtools::test()` — run the file above directly. A GitHub Actions workflow (`.github/workflows/ci.yml`) runs the same suite on every push or pull request touching `scripts/**` or `tests/**`. The per-fund cleaners and the report itself have no automated tests (verify those by rendering and inspecting the output).

------------------------------------------------------------------------

## Data Pipeline (detailed)

```         
data/raw/ (or iShares API) ──(per-fund scripts)──►  data/clean/*.xlsx
                                                          │
meta/tickerliste.xlsx ────────────────────────────────────┤
config/ticker_disambiguation.csv, fund_proxies.csv ───────┤
                                                          │
meta/ticker_currency.csv ◄──(get_currency_map.R)          │
meta/closing_prices.csv  ◄──(get_price_data.R)            │
meta/fx_rates_raw.csv    ◄──(get_price_data.R)            │
         │                                                │
         └──(price_cache.R: load_cached_prices[_eur]())───┤
                                                          │
config/portfolio_configs.csv, scenarios.csv,              │
  etf_listings.csv, sleeve_assumptions.csv ───────────────┤
                                                          │
                                               portfolio_report.Rmd
                                                          │
                          results/*.html / .pdf / .docx   (reports/ via render_reports.R)
                          data/clean/<portfolio>.xlsx, correlation_matrix_<portfolio>.csv
                          data/output/  (multi-agent export)
```

0.  **`scripts/utils/`** — shared helpers sourced by the steps below. The main ones: `portfolio_build.R` (config + fund files → complete holdings; the single definition of the portfolio shared by the report and the price refresh, plus `BENCHMARK_TICKERS`), `data_loaders.R` (`resolve_and_upsert_tickers()` for ISIN → ticker resolution), `ishares_loaders.R` (live iShares imports), `ticker_disambiguation.R` and `exchange_suffix.R` (fixing bare non-US tickers), `sector_taxonomy.R` (single source of truth for the 11 GICS sectors plus the Cash/Bonds pseudo-sectors and pseudo-tickers), `sleeve_model.R` (whole-portfolio building-block model), `multiagent_export.R` and `report_caveats.R` (multi-agent export and the report's "Read With Care" text), `security.R` (API-key redaction in logged errors) and `logging.R` (persists ISIN-resolution and matching warnings to `logs/pipeline.log`, so they survive a headless `Rscript` run).
1.  **`data/raw/`** — fund holdings exported manually from each ETF provider's website (xlsx/csv, gitignored); iShares funds are fetched live instead.
2.  **Per-fund cleaners** (`scripts/AmundiCoreWorld.R`, `DekaEuroStoxx50.R`, `VanguardFTSE.R`, `iSharesACWI.R`, etc.) — standardize raw exports and write to `data/clean/<Fund>.xlsx`.
3.  **`meta/tickerliste.xlsx`** — master ticker → country/sector/industry lookup, maintained by `tickerliste_maintenance.R` and `updating_tickerlist.R`.
4.  **`get_currency_map.R`** — builds `meta/ticker_currency.csv` (trading currency and price scale per ticker) from EODHD's exchange symbol lists.
5.  **`get_price_data.R` + `price_cache.R`** — fetch daily adjusted closing prices and EUR FX rates from EODHD into `meta/closing_prices.csv` and `meta/fx_rates_raw.csv`. `price_cache.R`'s `load_cached_prices()` / `load_cached_prices_eur()` are the single canonical readers for those caches — every downstream script and report chunk uses them.
6.  **`get_volatility_data.R`** — computes annualized historical volatility from cached prices (`calculate_volatility_from_prices`, `batch_calculate_volatility`).
7.  **`portfolio_returns.R`** — pure helper functions (`prices_to_wide()`, `compute_daily_log_returns()`, `compute_portfolio_return_series()`, `rebase_to_one()`) that turn cached prices into a portfolio-weighted daily return series, used by the report's performance sections.
8.  **`config/`** — definitional config, tracked in git: `portfolio_configs.csv` (per-portfolio fund lists, allocation weights, display metadata; one row per fund), `scenarios.csv`, `etf_listings.csv`, `fund_proxies.csv`, `sleeve_assumptions.csv`, `ticker_disambiguation.csv`.
9.  **`portfolio_report.Rmd`** — main entry point. Loads fund holdings, scales by fund weight, applies the chosen scenario, and computes the report's eight sections: (1) executive summary, (2) risk profile & concentration (whole-portfolio sleeve model, correlation heatmap, risk-weighted diversification), (3) geographic distribution, (4) holdings, (5) sector composition, (6) advanced visualizations & performance, (7) data quality & the look-through validation against each fund's own price series, (8) important information.

------------------------------------------------------------------------

## Project Structure

```         
portfolio/
├── config/                         # Definitional config (tracked in git)
│   ├── portfolio_configs.csv       #   per-portfolio fund lists/weights
│   ├── scenarios.csv               #   named exclusion scenarios
│   ├── etf_listings.csv            #   each fund's own EODHD price series
│   ├── fund_proxies.csv            #   proxy holdings for swap-based ETFs
│   ├── sleeve_assumptions.csv      #   assumed return/vol for Cash & Bonds
│   └── ticker_disambiguation.csv   #   bare ticker -> correct EODHD symbol
├── scripts/
│   ├── portfolio_report.Rmd        # Main report (knit this)
│   ├── render_reports.R            # Renders every portfolio to reports/
│   ├── get_currency_map.R          # Ticker -> trading currency map
│   ├── get_price_data.R            # EODHD price + FX fetcher
│   ├── price_cache.R               # Canonical price/FX cache readers
│   ├── get_volatility_data.R       # Volatility calculation helpers
│   ├── portfolio_returns.R         # Return-series helpers (report Section 6)
│   ├── tickerliste_maintenance.R   # Ticker master list upkeep
│   ├── updating_tickerlist.R       # Ticker list updater
│   ├── suggest_exchange_suffixes.R # Proposes suffix rules for bare tickers
│   ├── AmundiCoreWorld.R           # Per-fund cleaner scripts
│   ├── VanguardFTSE.R              #   Vanguard CSV export
│   ├── iSharesEMIMI.R              #   iShares: fetched live via API
│   ├── CTLuxEuroSmComp.R           #   fuzzy ISIN-less ticker matching
│   ├── single_tickers.R            #   manually maintained, no fund export
│   ├── CashAndBonds.R              #   manually maintained, no fund export
│   ├── ...                         #   (one per fund)
│   └── utils/                      # Shared helpers (see Data Pipeline below)
├── tests/
│   ├── run_tests.R                 # Rscript tests/run_tests.R
│   └── testthat/                   # Offline tests for the helpers and config
├── meta/                           # Market-data caches and ticker list (tracked)
│   ├── tickerliste.xlsx
│   ├── closing_prices.csv
│   ├── fx_rates_raw.csv
│   ├── ticker_currency.csv
│   ├── sectormapping.csv
│   └── *_fetch_metadata.csv        # incremental-fetch bookkeeping
├── data/
│   ├── raw/                        # Raw fund exports (gitignored)
│   ├── clean/                      # Standardized holdings files (gitignored)
│   └── output/                     # Multi-agent export (gitignored)
├── outputs/                        # Generated charts (gitignored)
├── results/, reports/              # Rendered reports (gitignored)
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

Then add `"newname"` to the `portfolio` `choices` list in the YAML params block at the
top of `scripts/portfolio_report.Rmd` — that dropdown is the one hardcoded list of
portfolio ids the CSV does not drive. (`scripts/render_reports.R` and the price
refresh read the ids straight from the config.) Add at least a `complete` row for the
new portfolio to `config/scenarios.csv`; further scenarios are optional.

------------------------------------------------------------------------

## Notes

- See `AGENTS.md` for detailed API notes, past analysis findings, and volatility benchmarks by sector.
- For non-US tickers (Korea, Hong Kong, Taiwan) the EODHD API requires an exchange suffix — e.g. `.TW` (Taiwan), `.KO` (Korea Exchange stock market), `.KQ` (Korea Exchange Kosdaq), `.HK` (Hong Kong). Note this differs from Yahoo's Korea convention (`.KS`). `iSharesEMIMI.R`'s `exchange_suffix_map` applies these automatically based on the fund export's exchange column; unmapped exchanges are left bare and flagged via a console warning.
- Non-fatal warnings (unresolved ISINs, unmapped exchanges, fuzzy-match losses) are persisted to `logs/pipeline.log` in addition to the console, so they're not lost when a script runs headlessly via `Rscript`.

------------------------------------------------------------------------

## License

MIT — see [LICENSE](LICENSE).
