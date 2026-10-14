# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

R-based toolkit for tracking and evaluating two personal investment portfolios ("portfolio_a" and "portfolio_b"), each built as a look-through aggregation of several ETFs plus individual stocks, gold/crypto, cash and low-risk bonds. It pulls historical prices and EUR FX rates from the EODHD API, computes risk/diversification/correlation/currency metrics, and renders a parametrized (portfolio × scenario) HTML/PDF/Word report. This is not a software product or R package — scripts are run interactively/manually in RStudio or via `Rscript`. There is a `testthat` suite for the pure helpers and a GitHub Actions workflow that runs it; there is no linter.

Companion project: [multi-agent-portfolio-analysis](https://github.com/andreas678/multi-agent-portfolio-analysis) — a multi-agent analysis system built alongside this pipeline; it consumes the workbook the report writes to `data/output/`.

## Commands

Tests (offline — no EODHD/Finnhub calls; also run by `.github/workflows/ci.yml` on pushes/PRs touching `scripts/**` or `tests/**`):

```bash
Rscript tests/run_tests.R
```

Refresh market data (requires `EODHD_API_KEY`, e.g. in `.Renviron`; run in this order, after the per-fund cleaners and any `tickerliste_maintenance.R` run):

```bash
Rscript scripts/get_currency_map.R   # meta/ticker_currency.csv (trading currency per ticker)
Rscript scripts/get_price_data.R     # meta/closing_prices.csv + meta/fx_rates_raw.csv
```

Render the report:

```r
rmarkdown::render("scripts/portfolio_report.Rmd",
                   params = list(portfolio = "portfolio_b", scenario = "complete"),
                   output_file = "../results/portfolio_b_report.html")
```

Or in RStudio: **Knit ▾ → Knit with Parameters...** to pick the portfolio and scenario from dropdowns. Plain "Knit" reuses whichever values were picked last. Interactive knits all write to `portfolio_report.html` next to the Rmd — pass an explicit `output_file`, or run `Rscript scripts/render_reports.R`, which renders every portfolio in `config/portfolio_configs.csv` (complete scenario) to `reports/portfolio_report_<id>.html`.

The per-fund cleaners and the report have no automated tests — verify those by rendering and inspecting the output tables/charts.

## Architecture

**Data pipeline (raw → clean → report):**

1. **`data/raw/`** — fund holdings exported manually from each ETF provider's website (xlsx/csv, mixed German/English column names, gitignored). Per-fund scripts document the manual download/cleanup steps in comments (e.g. `AmundiCoreWorld.R`: download xlsx from Amundi's site, delete header/footer rows, delete the leading empty column; `VanguardFTSE.R` needs no manual editing).
2. **Per-fund cleaner scripts** in `scripts/` (`AmundiCoreWorld.R`, `AmundiCoreStoxxEurope600.R`, `AmundiDisruptiveTech.R`, `CTLuxEuroSmComp.R`, `DekaEuroStoxx50.R`, `DekaWorldClimateChange.R`, `VanguardFTSE.R`, `XtrackersWorldExUSA.R`, `iShares*.R`, `single_tickers.R`) — each reads the matching raw file, resolves missing tickers via EODHD's ISIN search API (`resolve_and_upsert_tickers()` / `get_ticker_data_from_isin()` in `scripts/utils/data_loaders.R`, 1s rate-limited per call, preferring the ISIN country's home exchange via `ISIN_HOME_EXCHANGES`), enriches with country codes, and writes a standardized holdings file to `data/clean/<Fund>.xlsx`. Variants:
   - `CTLuxEuroSmComp.R` has no ISIN, so it does fuzzy name matching (Jaro-Winkler via `stringdist`, threshold 0.8 with 0.7 fallback).
   - `iSharesEMIMI.R`, `iSharesACWI.R`, `iSharesEuro600.R` download holdings live from the iShares API (shared parsing in `scripts/utils/ishares_loaders.R`, which translates German country/exchange names and appends the EODHD exchange suffix, e.g. `2330` → `2330.TW`; unmapped exchanges are surfaced via a warning and left bare). `iSharesWorld.R` is the same but feeds a fund proxy (below), not a portfolio fund.
   - These are run manually/individually, not sourced by anything else — re-run a fund's script after refreshing its raw export. Some scripts hardcode the source spreadsheet's sheet name (a date string) — update it when the raw file's sheet changes.

   `scripts/CashAndBonds.R` follows a variant of this pattern for the "Cash" and "Low Risk Bonds" pseudo-holdings: there's no raw fund export to clean, so allocation percentages are maintained directly in a small tribble at the top of the script (edit and re-run to update `Cash_A.xlsx`, `LowRiskBonds_A.xlsx`, `Cash_B.xlsx`, `LowRiskBonds_B.xlsx`). It also upserts the synthetic `CASH_EUR`/`BONDS_LR_EUR` tickers into `meta/tickerliste.xlsx` with fixed sector labels ("Cash & Equivalents" / "Fixed Income") so they classify correctly in sector charts.
3. **`meta/tickerliste.xlsx`** — master ticker → country/sector/industry lookup shared across all funds and reports, kept up to date by `tickerliste_maintenance.R` (4-phase enrichment: country from ISIN prefix, asset class by pattern match, sector for US equities via Finnhub — requires `FINNHUB_API_KEY` — and a 4th GICS-harmonization phase that maps every raw sector label (German fund/Finnhub-locale names, Finnhub sub-industry names, etc.) onto the 11 canonical English GICS sector names in `scripts/utils/sector_taxonomy.R`, preserving the pre-harmonization label in `sector_orig`; unmapped labels are left unchanged and printed for follow-up) and `updating_tickerlist.R` (backfills missing company names via EODHD's search API). The report also completes sector lookups through the ISIN (`sector_lookup_by_isin()`), since report tickers carry exchange suffixes the ticker list does not.
4. **Ticker disambiguation** (`config/ticker_disambiguation.csv`, `scripts/utils/ticker_disambiguation.R`, `exchange_suffix.R`) — a bare fund-export ticker is resolved by EODHD on its US exchange, which can silently pick a different company (bare `AI` is C3.ai, not Air Liquide; `SAN` is both Santander and Sanofi). The CSV maps `(ticker, name_pattern[, alpha3])` → correct EODHD symbol. Rules are applied when fund files are *loaded* (the report's `load_etf()`, the price refresh), so `data/clean/` doesn't need regenerating when a rule changes. `scripts/suggest_exchange_suffixes.R` proposes new rules offline from a fund file's country column.
5. **`scripts/get_currency_map.R`** — builds `meta/ticker_currency.csv` (`ticker, exchange, currency, price_scale, source`) from EODHD's exchange-symbol-list endpoint. EODHD's EOD endpoint returns prices in each listing's local currency without saying which; the map (and its `price_scale`, e.g. 0.01 for pence quotes) is what makes EUR conversion possible. Run it after adding tickers, before the price refresh.
6. **`scripts/get_price_data.R`** — fetches historical daily prices from EODHD with incremental caching (skips tickers/date-ranges already fetched), writing `meta/closing_prices.csv` and `meta/price_fetch_metadata.csv`; also EUR FX pairs (`EUR<CCY>.FOREX`) into `meta/fx_rates_raw.csv` / `meta/fx_fetch_metadata.csv`. New tickers get `PRICE_HISTORY_DAYS` (365) of history — the free EODHD plan's ceiling. The refresh universe is built from `config/portfolio_configs.csv` + the cleaned fund files via `scripts/utils/portfolio_build.R` (holdings at or above `PRICE_REFRESH_MIN_WEIGHT_PCT` = 0.1%), plus `BENCHMARK_TICKERS` (`SPY`, `URTH`, `EEM`) and every fund's own listing from `config/etf_listings.csv`. No report render is required first. Tickers that failed repeatedly are skipped (delete their rows from `meta/price_fetch_metadata.csv` to retry). Run periodically to keep volatility/correlation/return inputs fresh.
7. **`scripts/price_cache.R`** — the single canonical reader of the caches: `load_cached_prices()` (raw local-currency prices), `load_cached_prices_eur()` (converted via ticker currency × FX with a 7-day last-observation-carried-forward limit; unconvertible rows are dropped and reported in `attr(, "dropped")`), `load_fx_rates()`, `load_ticker_currency()`, `get_crypto_tickers()`, `periods_per_year()`. Use these — don't read the CSVs directly.
8. **`scripts/get_volatility_data.R`** — `calculate_volatility_from_prices()`, `batch_calculate_volatility()` and market-beta helpers; annualization uses the series' own quote frequency (`periods_per_year()`: ~250 for exchange-traded, ~365 for crypto). Sourced by the report at knit time.
9. **`scripts/portfolio_returns.R`** — pure helper functions (`prices_to_wide()`, `compute_daily_log_returns()`, `compute_portfolio_return_series()`, `rebase_to_one()`) that turn cached daily prices into a portfolio-weighted daily log-return series, used by the report's Section 6.
10. **`config/`** (tracked in git, see Directory layout) drives portfolio construction, scenarios, fund price series, proxies and sleeve assumptions.
11. **`scripts/portfolio_report.Rmd`** — the main entry point. Reads `params$portfolio` and `params$scenario`, looks up the portfolio's fund list/weights from `config/portfolio_configs.csv`, and builds the *complete* holdings via `build_complete_portfolio()` (`scripts/utils/portfolio_build.R`): loads each fund's cleaned holdings from `data/clean/` (with disambiguation rules applied), scales constituent weights by the fund's allocation (or uses the fund's own weight column as-is for the individual-securities sheet and the Cash / Low Risk Bonds pseudo-holdings, signaled by `weight = NA` in the config), and substitutes proxy holdings where `config/fund_proxies.csv` says so. It then applies the chosen scenario (below), writes the combined holdings to `data/clean/<output_file>` (e.g. `portfolio_a.xlsx`), and computes the eight report sections: (1) executive summary incl. "Read With Care" caveats (`scripts/utils/report_caveats.R`); (2) risk profile — the **sleeve model** (below), concentration/HHI on the equity part, currency exposure, a volatility-weighted "tornado chart" of risk contribution (falling back historical → manual override from `config/sleeve_assumptions.csv` (Cash/Low Risk Bonds only) → sector estimate from `SECTOR_VOL_ESTIMATES` → 22% default), a price-correlation heatmap (also exported as `data/clean/correlation_matrix_<portfolio>.csv`, tidy long form — `ticker1, ticker2, correlation`), risk-weighted diversification; (3) geographic distribution (bar chart + world map via `maps::map_data`); (4) top-holdings/concentration tables; (5) sector sunburst (`plotly`); (6) cumulative returns vs. benchmarks, Monte Carlo efficient frontier, sector risk/return, holding-level risk/return positioning, performance attribution, and the multi-agent export; (7) data quality, plus a look-through-vs-fund-own-price-series validation (tracking error, beta, R²); (8) important information.

**Scenarios** (`config/scenarios.csv`: `scenario, portfolio, exclude_tickers, exclude_index, exclude_alpha3, description`): a holding is excluded if it matches *any* of exact ticker, fund/index label (`Cash`, `LowRiskBonds`, `Einzelaktien`, …) or synthetic country code (`GOLD`, `CRYPTO`). Excluded weight is reallocated proportionally so total weight is unchanged. A scenario not defined for the chosen portfolio falls back to `complete`, surfaced in the report. Every portfolio needs a `complete` row.

**Sleeve model** (`scripts/utils/sleeve_model.R`): the look-through rebuilds risk from only the ~half of the portfolio's weight that has a price series (cash, bonds and the ETFs' long tail have none; renormalizing inflates e.g. gold from 20% to 40%). So Section 2.1 models the portfolio as building blocks: one sleeve per fund with a price series (the fund's own EUR series, `config/etf_listings.csv`), one per directly held security with a price series (gold, crypto, single stocks), and synthetic Cash / Low Risk Bonds sleeves run on the *assumed* return/volatility in `config/sleeve_assumptions.csv` (uncorrelated with everything else). Risk, return and Euler risk shares are computed on the sleeves. The look-through sections remain for holdings-level detail.

**Fund proxies** (`config/fund_proxies.csv`: `fund_file, proxy_file, exclude_alpha3, note`): a fund whose published holdings don't represent its index exposure (e.g. swap-based ETFs publish their collateral basket) is replaced by another cleaned fund's holdings, renormalized to 100 (optionally dropping some countries). Delete the row to use the fund's own holdings.

**Multi-agent export** (`scripts/utils/multiagent_export.R`, `multiagent-export` chunk): each render writes `data/output/<portfolio>.xlsx` — selected positions (largest weights, top risk contributors, hedges, risk/return picks, all non-equity blocks) plus the report's own whole-portfolio figures and caveats, so the multi-agent system interprets rather than recomputes them. Kept out of the Rmd so the column layout survives report changes and can be tested.

**To add a new fund to a portfolio**: write/copy a per-fund cleaner script following the existing pattern (read raw file → resolve tickers via ISIN → write `data/clean/<Fund>.xlsx` with columns `ISIN, name, weight, ticker, alpha2, alpha3, countryname, index`), add a row for it to the relevant portfolio in `config/portfolio_configs.csv`, and — to give it a sleeve and a look-through check — a row in `config/etf_listings.csv`.

**To add a new portfolio**: add rows to `config/portfolio_configs.csv` (`display_name` and `output_file` must repeat identically on every row of a portfolio), a `complete` row in `config/scenarios.csv`, and the id to the `portfolio` `choices` list in the Rmd's YAML header — the only hardcoded list left (`render_reports.R` and the price refresh read ids from the config).

**Key shared conventions in `portfolio_report.Rmd`:**
- `ticker_summary` and `country_summary` (built once in the `risk-metrics-calculation` chunk) are the single source of truth for all downstream ticker/country-level aggregates — don't recompute them elsewhere in the doc.
- `ticker_sector_lookup` (built once in setup, extended by ISIN) is reused for every sector `left_join` instead of re-filtering `tickerliste` per chunk.
- Countries, sectors and position concentration are statements about *equities* only; use `asset_class_of()` (`sector_taxonomy.R`) to separate Equities / Gold / Crypto / Low Risk Bonds / Cash.
- Portfolio construction always filters `weight > 0, ticker != "-", !is.na(ticker), !is.na(alpha3)` (in `build_complete_portfolio()`) — apply this consistently if adding new aggregation logic, since dropping it lets malformed rows leak into holdings counts and Data Quality figures.
- Chunks with `include=FALSE` discard *all* output, `cat()` included; put user-visible notices in a chunk that is shown.

## Directory layout

- `scripts/` — all R logic; `portfolio_report.Rmd` is the only report file in active use. `scripts/utils/` holds the shared helpers (`portfolio_build.R`, `data_loaders.R`, `ishares_loaders.R`, `ticker_disambiguation.R`, `exchange_suffix.R`, `sector_taxonomy.R`, `sleeve_model.R`, `multiagent_export.R`, `report_caveats.R`, `security.R`, `logging.R`).
- `config/` — definitional config, tracked in git like code: `portfolio_configs.csv` (per-portfolio fund lists, allocation weights, display metadata), `scenarios.csv`, `etf_listings.csv` (each fund's own EODHD listing: ISIN, ticker, currency, price scale), `fund_proxies.csv`, `sleeve_assumptions.csv`, `ticker_disambiguation.csv`.
- `tests/` — `run_tests.R` and `testthat/` (offline tests for the helpers and config files).
- `.github/workflows/ci.yml` — runs the test suite.
- `data/raw/`, `data/clean/`, `data/output/` — pipeline stages described above (gitignored; `data/raw/single_tickers.csv` is a tracked header-only template).
- `meta/` — shared market-data reference: ticker list, sector mapping, cached prices, ticker→currency map, FX rates, API fetch metadata. Tracked in git (market data, not personal).
- `outputs/` — generated chart PNGs (gitignored).
- `results/`, `reports/` — rendered report HTML/PDF/Word (gitignored).
- `AGENTS.md` — API/operational gotchas not covered here (EODHD rate limits, exchange-suffix quirks for non-US tickers, volatility methodology); points back to this file for architecture.

## Data privacy & retention

**Credentials:** `EODHD_API_KEY` / `FINNHUB_API_KEY` live only in `.Renviron` (gitignored) and are read via `Sys.getenv()` — never hardcode a key in a script. Both APIs authenticate solely via a URL query parameter (no header-based alternative), so the key is unavoidably present in every request URL. `scripts/utils/security.R`'s `redact_secret()` strips it from caught error messages before they reach `cat()`/`warning()` in `get_price_data.R`, `get_currency_map.R` and `tickerliste_maintenance.R`, since a network-level `httr::GET()` failure otherwise embeds the full URL (key included) in `conditionMessage(e)` — apply the same pattern to any new EODHD/Finnhub call site that logs its error text. `logs/pipeline.log` (via `log_pipeline_issue()`, `scripts/utils/logging.R`) is covered by the `*.log` gitignore rule.

**Personal data:** rendered reports (`reports/`, `results/`), `data/clean/portfolio_*.xlsx` and `data/output/` identify the portfolio owner by name and list full holdings — all gitignored, never commit these. Treat them as sensitive if exporting/sharing outside this repo.

**Retention:** `meta/closing_prices.csv`, `meta/fx_rates_raw.csv`, `meta/ticker_currency.csv` and `meta/tickerliste.xlsx` are long-lived caches, safe to keep indefinitely (market data, not personal). `meta/tickerliste_backup.xlsx` (written by `tickerliste_maintenance.R` before each run) is safe to delete once the run it backed up is verified. `data/clean/*.xlsx` and `reports/*.html` are regenerated from source data on each render — delete freely; nothing there is the source of truth.

## graphify

This project has a graphify knowledge graph at .graphify/.

Rules:
- For codebase or architecture questions, when `.graphify/graph.json` exists, first run `graphify query "<question>"` (or `graphify path "<A>" "<B>"` / `graphify explain "<concept>"`); these return a scoped subgraph, usually much smaller than `GRAPH_REPORT.md` or raw grep output
- If .graphify/wiki/index.md exists, navigate it instead of reading raw files
- If .graphify/graph.json is missing but graphify-out/graph.json exists, run `graphify migrate-state --dry-run` first; if tracked legacy artifacts are reported, ask before using the recommended `git mv -f graphify-out .graphify` and commit message
- If .graphify/needs_update exists or .graphify/branch.json has stale=true, warn before relying on semantic results and run /graphify . --update when appropriate
- Before proposing or committing .graphify artifacts, run `graphify portable-check .graphify`; commit-safe graph artifacts must use repo-relative paths, and never commit .graphify/branch.json, .graphify/worktree.json, .graphify/needs_update, or .graphify/cache/. If a repo already tracks any of them, first add them to .gitignore, then propose `git rm --cached .graphify/branch.json .graphify/worktree.json .graphify/needs_update` and `git rm -r --cached .graphify/cache`; never mutate git state without asking
- Before deep graph traversal, prefer `graphify summary --graph .graphify/graph.json` for compact first-hop orientation
- For review impact on changed files, use `graphify review-delta --graph .graphify/graph.json` instead of generic traversal
- Read `.graphify/GRAPH_REPORT.md` only for broad architecture review or when `query` / `path` / `explain` do not surface enough context
- After modifying code files in this session, run `npx graphify hook-rebuild` to keep the graph current
