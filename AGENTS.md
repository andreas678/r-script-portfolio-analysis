# Portfolio Analysis Project - Agent Memory

See `CLAUDE.md` for the full architecture, commands, directory layout, and
data pipeline walkthrough. This file covers operational/API details and
gotchas that aren't there.

## Key Architecture Gotchas

- **`scripts/price_cache.R`** — single source of truth for reading the caches. All scripts and report chunks call `load_cached_prices()` (raw, local currency) or `load_cached_prices_eur()` (converted) rather than reading `meta/closing_prices.csv` directly.
- **Local currency**: EODHD prices are in each listing's trading currency with no currency column. `meta/ticker_currency.csv` (from `scripts/get_currency_map.R`) carries currency and `price_scale` (0.01 for pence quotes); `meta/fx_rates_raw.csv` carries `EUR<CCY>.FOREX` rates. After adding tickers run `get_currency_map.R` **before** `get_price_data.R`, or the FX refresh is skipped with a warning.
- **`scripts/get_price_data.R`** — builds its ticker universe from `config/portfolio_configs.csv` and the cleaned fund files (`scripts/utils/portfolio_build.R`), so no report render is needed first, but the per-fund cleaners must have been run. Tickers that failed repeatedly are skipped; delete their rows from `meta/price_fetch_metadata.csv` to retry.
- **Bare non-US tickers** resolve on EODHD's US exchange and can silently select the wrong company. Fix with a rule in `config/ticker_disambiguation.csv` (applied at load time, no need to regenerate `data/clean/`); `scripts/suggest_exchange_suffixes.R` proposes rules offline.
- **Swap-based ETFs** publish a collateral basket, not index exposure; `config/fund_proxies.csv` substitutes a physical fund's holdings. A fund with a price series needs a row in `config/etf_listings.csv` (otherwise it has no sleeve and no look-through check).
- **Cash / Low Risk Bonds** have no price history; their return and volatility are *assumptions* in `config/sleeve_assumptions.csv`.
- Adding a new portfolio to `config/portfolio_configs.csv` also requires a `complete` row in `config/scenarios.csv` and its id in the `choices` list of `portfolio_report.Rmd`'s YAML params block. `render_reports.R` and the price refresh read ids from the config, so the YAML dropdown is the only hand-synced list.
- **Tests**: `Rscript tests/run_tests.R` (offline; CI runs it). Run it after touching anything in `scripts/utils/`, `price_cache.R`, `get_volatility_data.R`, `portfolio_returns.R` or `config/*.csv` — several tests validate the config files' shape.

## EODHD API

- **Key**: `Sys.getenv("EODHD_API_KEY")` (set in `~/.Renviron`)
- **Rate limit**: price/FX fetches sleep \~0.5 s between requests (`get_price_data.R`), the exchange-symbol-list calls 0.2 s (`get_currency_map.R`), and ISIN-search lookups 1 s (`get_ticker_data_from_isin()`)
- **Data**: daily adjusted closing prices, CSV format; FX as `EUR<CCY>.FOREX` pairs through the same endpoint; crypto as `<SYMBOL>-USD.CC`
- **History**: the free plan serves at most one year, hence `PRICE_HISTORY_DAYS = 365` in `get_price_data.R`

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

- Window: the report's shared analysis window (stated in Section 1 of the rendered report; it ends on the last date every price series has a quote), at most ~1 year of cached history
- Annualization: log-return std × √(periods per year), where `periods_per_year()` (`price_cache.R`) infers the series' own quote frequency (~250 for exchange-traded, ~365 for crypto quoted 7 days a week); return uses the same factor
- Prices used: adjusted close (accounts for splits and dividends), converted to EUR for the report's risk and return figures
- Cash / Low Risk Bonds: assumed return and volatility from `config/sleeve_assumptions.csv`, uncorrelated with everything else
- Sector-based fallback estimates when historical data is unavailable: `SECTOR_VOL_ESTIMATES` in `scripts/utils/sector_taxonomy.R` (11 GICS sectors, Digital Assets 70%, Commodities 16%, Unclassified/default 22%) — rough long-run assumptions, not data
