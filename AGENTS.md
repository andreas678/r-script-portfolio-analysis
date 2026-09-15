# Portfolio Analysis Project - Agent Memory

See `CLAUDE.md` for the full architecture, commands, directory layout, and
data pipeline walkthrough. This file covers operational/API details and
gotchas that aren't there.

## Key Architecture Gotchas

- **`scripts/price_cache.R`** — single source of truth for reading `meta/closing_prices.csv`. All scripts and report chunks call `load_cached_prices()` rather than reading the CSV directly.
- **`scripts/get_price_data.R`** — requires a rendered `data/clean/portfolio_a.xlsx` or `portfolio_b.xlsx` to exist first (bootstrapping dependency) — see CLAUDE.md's data pipeline for render order.
- Adding a new portfolio to `config/portfolio_configs.csv` also requires adding its id to the `choices` list in `portfolio_report.Rmd`'s YAML params block, and to `report_targets` in `scripts/render_reports.R` — three separate places currently need the portfolio id kept in sync by hand.

## EODHD API

- **Key**: `Sys.getenv("EODHD_API_KEY")` (set in `~/.Renviron`)
- **Rate limit**: \~0.5 seconds between requests
- **Data**: daily adjusted closing prices, CSV format

### Exchange Suffixes for Non-US Tickers

EODHD requires an exchange suffix for non-US stocks. Example mappings:

| Market                     | Suffix | Example tickers                     |
|----------------------------|--------|--------------------------------------|
| Taiwan                     | `.TW`  | 2317 (Hon Hai / Foxconn)             |
| Korea (Stock Market/KOSPI) | `.KO`  | 005380 (Hyundai Motor)               |
| Korea (Kosdaq)             | `.KQ`  | —                                     |
| Hong Kong                  | `.HK`  | 941 (China Mobile)                   |
| Germany                    | `.XETRA` | SAP (SAP SE)                       |

Verified directly against the live EODHD EOD endpoint (see
`scripts/iSharesEMIMI.R`'s `exchange_suffix_map`) — this is EODHD's own
convention, not Yahoo Finance's (which uses `.KS`/`.KQ` differently).

Some tickers have been intermittently unavailable via EODHD; retry affected
tickers individually if you hit gaps in the price cache.

## Volatility Methodology

- Window: 252 trading days (1 year)
- Annualization: daily log-return std × √252
- Prices used: adjusted close (accounts for splits and dividends)
- Sector-based fallback estimates used when historical data is unavailable (see `volatility_estimates` tibble in the report's `tornado-chart-analysis` chunk)
