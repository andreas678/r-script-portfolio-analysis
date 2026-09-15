# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

R-based toolkit for tracking and evaluating two personal investment portfolios ("portfolio_a" and "portfolio_b"), each built as a look-through aggregation of several ETFs plus individual stocks. It pulls historical price/volatility data from the EODHD API, computes risk/diversification/correlation metrics, and renders a parametrized HTML/PDF/Word report per portfolio. This is not a software product or R package — there is no test suite, linter, or CI; scripts are run interactively/manually in RStudio.

Companion project: [multi-agent-portfolio-analysis](https://github.com/andreas678/multi-agent-portfolio-analysis) — a multi-agent analysis system built alongside this pipeline.

## Commands

Render the main report (requires `EODHD_API_KEY` set, e.g. in `.Renviron`):

```r
rmarkdown::render("scripts/portfolio_report.Rmd", params = list(portfolio = "portfolio_a"))
```

Or in RStudio: **Knit ▾ → Knit with Parameters...** to pick `portfolio_a` or `portfolio_b` from a dropdown. Plain "Knit" reuses whichever portfolio was picked last. Both portfolios render to the same default output filename (`portfolio_report.html`) next to the Rmd — pass an explicit `output_file` to keep both side by side, e.g.:

```r
rmarkdown::render("scripts/portfolio_report.Rmd",
                   params = list(portfolio = "portfolio_b"),
                   output_file = "../results/portfolio_b_report.html")
```

There are no automated tests, build steps, or linters in this repo — verification is done by rendering the report and inspecting the output tables/charts.

## Architecture

**Data pipeline (raw → clean → report):**

1. **`data/raw/`** — fund holdings exported manually from each ETF provider's website (xlsx/csv, mixed German/English column names, gitignored). Per-fund scripts document the manual download/cleanup steps in comments (e.g. `AmundiCoreWorld.R`: download xlsx from Amundi's site, delete header/footer rows, delete the leading empty column).
2. **Per-fund cleaner scripts** in `scripts/` (`AmundiCoreWorld.R`, `AmundiDisruptiveTech.R`, `CTLuxEuroSmComp.R`, `DekaEuroStoxx50.R`, `DekaWorldClimateChange.R`, `single_tickers.R`, `iSharesEMIMI.R`, `XtrackersWorldExUSA.R`) — each reads the matching raw file, resolves missing tickers via EODHD's ISIN search API (`get_ticker_data_from_isin()`, 1s rate-limited per call), enriches with country codes, and writes a standardized holdings file to `data/clean/<Fund>.xlsx`. `CTLuxEuroSmComp.R` additionally does fuzzy ISIN-less name matching (Jaro-Winkler via `stringdist`, threshold 0.8 with 0.7 fallback). `iSharesEMIMI.R` additionally appends an EODHD exchange suffix (e.g. `2330` → `2330.TW`) to non-US tickers based on the fund's "Börse" exchange column (`exchange_suffix_map` tribble), since bare local tickers are exchange-ambiguous for EODHD's EOD endpoint; unmapped exchanges are surfaced via a `cat()` warning and left bare. These are run manually/individually, not sourced by anything else — re-run a fund's script after refreshing its raw export. Note: some scripts hardcode the source spreadsheet's sheet name (a date string) — update it when the raw file's sheet changes.

   `scripts/CashAndBonds.R` follows a variant of this pattern for the "Cash" and "Low Risk Bonds" pseudo-holdings: there's no raw fund export to clean, so allocation percentages are maintained directly in a small tribble at the top of the script (edit and re-run to update `Cash_A.xlsx`, `LowRiskBonds_A.xlsx`, `Cash_B.xlsx`, `LowRiskBonds_B.xlsx`). It also upserts the synthetic `CASH_EUR`/`BONDS_LR_EUR` tickers into `meta/tickerliste.xlsx` with fixed sector labels ("Cash & Equivalents" / "Fixed Income") so they classify correctly in sector charts.
3. **`meta/tickerliste.xlsx`** — master ticker → country/sector/industry lookup shared across all funds and reports, kept up to date by `tickerliste_maintenance.R` (4-phase enrichment: country from ISIN prefix, asset class by pattern match, sector for US equities via Finnhub — requires `FINNHUB_API_KEY` — and a 4th GICS-harmonization phase that maps every raw sector label (German fund/Finnhub-locale names, Finnhub sub-industry names, etc.) onto the 11 canonical English GICS sector names that `portfolio_report.Rmd`'s `volatility_estimates` sector join expects exactly, preserving the pre-harmonization label in `sector_orig`; unmapped labels are left unchanged and printed for follow-up) and `updating_tickerlist.R` (backfills missing company names via EODHD's search API).
4. **`scripts/get_price_data.R`** — fetches historical daily prices from EODHD with incremental caching (skips tickers/date-ranges already fetched), writing `meta/closing_prices.csv` and `meta/price_fetch_metadata.csv`. Also defines `BENCHMARK_TICKERS` (`SPY`, `URTH`, `EEM`), included alongside portfolio holdings in the standalone refresh's ticker universe. The standalone refresh only fetches holdings at or above `MIN_WEIGHT_PCT` (0.1% portfolio weight) to keep the cache lean. Run this periodically to keep volatility/correlation/return inputs fresh (the report reminds you to via a `cat()` message when knitting).
5. **`scripts/get_volatility_data.R`** — defines `calculate_volatility_from_prices()` and `batch_calculate_volatility()`, computing annualized volatility (and mean daily return) from `meta/closing_prices.csv`. Sourced by the report at knit time.
6. **`scripts/portfolio_returns.R`** — pure helper functions (`prices_to_wide()`, `compute_daily_log_returns()`, `compute_portfolio_return_series()`, `rebase_to_one()`) that turn cached daily prices into a portfolio-weighted daily log-return series. Also sourced by the report at knit time, for Section 7 (Advanced Visualizations & Performance).
7. **`scripts/portfolio_report.Rmd`** — the main entry point. Reads `params$portfolio` (`"portfolio_a"` or `"portfolio_b"`), looks up that portfolio's fund list/weights from `config/portfolio_configs.csv` (loaded in the setup chunk into a `portfolio_configs` R list, one entry per portfolio), loads each fund's cleaned holdings from `data/clean/`, scales constituent weights by the fund's allocation (or uses the fund's own weight column as-is for the individual-securities sheet and the Cash / Low Risk Bonds pseudo-holdings, signaled by `weight = NA` in the config), and combines everything into one `portfolio` data frame (written to `data/clean/portfolio.xlsx` or `data/clean/portfolio_b.xlsx`). From there it computes: concentration/HHI risk metrics, a volatility-weighted "tornado chart" of risk contribution (falling back historical → manual override (Cash/Low Risk Bonds only) → sector-estimate → default 22% when historical volatility is unavailable), a price-correlation heatmap (also exported as `data/clean/correlation_matrix_<portfolio>.csv`, tidy long form — `ticker1, ticker2, correlation`), geographic distribution (bar chart + world map via `maps::map_data`), top-holdings and concentration tables, a sector sunburst (via `plotly`, using `ticker_summary`/`ticker_sector_lookup` built once in the setup chunk), a data-quality/completeness summary, and (Section 7) cumulative returns vs. benchmarks, a Monte Carlo-simulated efficient frontier, a sector risk/return bubble chart, and a fund→sector→country composition icicle chart.

**To add a new fund to a portfolio**: write/copy a per-fund cleaner script following the existing pattern (read raw file → resolve tickers via ISIN → write `data/clean/<Fund>.xlsx` with columns `ISIN, name, weight, ticker, alpha2, alpha3, countryname, index`), then add a row for it to the relevant portfolio in `config/portfolio_configs.csv`.

**Key shared conventions in `portfolio_report.Rmd`:**
- `ticker_summary` and `country_summary` (built once in the `risk-metrics-calculation` chunk) are the single source of truth for all downstream ticker/country-level aggregates — don't recompute them elsewhere in the doc.
- `ticker_sector_lookup` (built once in setup) is reused for every sector `left_join` instead of re-filtering `tickerliste` per chunk.
- Portfolio construction always filters `weight > 0, ticker != "-", !is.na(ticker), !is.na(alpha3)` — apply this consistently if adding new aggregation logic, since dropping it lets malformed rows leak into holdings counts and Data Quality figures.

## Directory layout

- `scripts/` — all R logic; `portfolio_report.Rmd` is the only report file in active use.
- `config/` — `portfolio_configs.csv`: per-portfolio fund lists, allocation weights, and display metadata, loaded by `portfolio_report.Rmd` at render time (not gitignored — this is definitional config, tracked in git like code).
- `data/raw/`, `data/clean/`, `data/output/` — pipeline stages described above (all gitignored).
- `meta/` — shared reference data: ticker list, sector mapping, cached prices/volatility, API fetch metadata (gitignored).
- `outputs/` — generated chart PNGs (gitignored).
- `results/` — rendered report HTML/PDF/Word (gitignored).
- `AGENTS.md` — API/operational gotchas not covered here (EODHD rate limits, exchange-suffix quirks for non-US tickers, volatility methodology); points back to this file for architecture.

## Data privacy & retention

**Credentials:** `EODHD_API_KEY` / `FINNHUB_API_KEY` live only in `.Renviron` (gitignored) and are read via `Sys.getenv()` — never hardcode a key in a script. Both APIs authenticate solely via a URL query parameter (no header-based alternative), so the key is unavoidably present in every request URL. `scripts/utils/security.R`'s `redact_secret()` strips it from caught error messages before they reach `cat()`/`warning()` in `get_price_data.R` and `tickerliste_maintenance.R`, since a network-level `httr::GET()` failure otherwise embeds the full URL (key included) in `conditionMessage(e)` — apply the same pattern to any new EODHD/Finnhub call site that logs its error text. `logs/pipeline.log` (via `log_pipeline_issue()`, `scripts/utils/logging.R`) is covered by the `*.log` gitignore rule.

**Personal data:** rendered reports (`reports/`, `results/`) and `data/clean/portfolio.xlsx` / `portfolio_b.xlsx` identify the portfolio owner by name and list full holdings — all gitignored, never commit these. Treat them as sensitive if exporting/sharing outside this repo.

**Retention:** `meta/closing_prices.csv` and `meta/tickerliste.xlsx` are long-lived caches, safe to keep indefinitely (market data, not personal). `meta/tickerliste_backup.xlsx` (written by `tickerliste_maintenance.R` before each run) is safe to delete once the run it backed up is verified. `data/clean/*.xlsx` and `reports/*.html` are regenerated from source data on each render — delete freely; nothing there is the source of truth.

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
