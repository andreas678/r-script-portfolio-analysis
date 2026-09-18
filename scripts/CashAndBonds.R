# Cash and Low Risk Bonds pseudo-holdings
#
# Unlike the per-fund cleaner scripts, there's no raw export to clean here —
# cash and low-risk bonds are declared allocations, not fund holdings with
# ISINs to resolve. Maintain the table below manually: enter each portfolio's
# actual cash / low-risk-bond allocation as a percentage of the whole
# portfolio in the `weight` column. Rows with weight = 0 are excluded from
# the report (the portfolio construction filter in portfolio_report.Rmd
# requires weight > 0), so nothing shows up until you fill in real numbers.

library(tidyverse)
library(readxl)
library(writexl)
library(here)

source(here::here("scripts", "utils", "sector_taxonomy.R"))
source(here::here("scripts", "utils", "data_loaders.R"))

data_dir   <- here::here("data", "clean")
ticker_path <- here::here("meta", "tickerliste.xlsx")

# Maintain this table manually.
cash_bonds_allocations <- tribble(
  ~portfolio, ~name,             ~weight, ~ticker,          ~alpha2, ~alpha3, ~countryname, ~index,
  "portfolio_a",  "Cash",            0,       CASH_TICKER,      "DE",    "DEU",   "Germany",    "Cash",
  "portfolio_a",  "Low Risk Bonds",  0,       BONDS_LR_TICKER,  "DE",    "DEU",   "Germany",    "LowRiskBonds",
  "portfolio_b",   "Cash",            0,       CASH_TICKER,      "DE",    "DEU",   "Germany",    "Cash",
  "portfolio_b",   "Low Risk Bonds",  0,       BONDS_LR_TICKER,  "DE",    "DEU",   "Germany",    "LowRiskBonds"
)

portfolio_labels <- c(portfolio_a = "Portfolio A", portfolio_b = "Portfolio B")

# ── Write one clean fund file per (portfolio, asset) row ───────────────────
# Schema matches every other fund's clean file: ISIN, name, weight, ticker,
# alpha2, alpha3, countryname, index. ISIN is NA — these aren't real
# ISIN-bearing securities.
cash_bonds_allocations %>%
  pwalk(function(portfolio, name, weight, ticker, alpha2, alpha3, countryname, index) {
    out <- tibble(
      ISIN        = NA_character_,
      name        = name,
      weight      = weight,
      ticker      = ticker,
      alpha2      = alpha2,
      alpha3      = alpha3,
      countryname = countryname,
      index       = index
    )
    filename <- paste0(index, "_", portfolio_labels[[portfolio]], ".xlsx")
    write_xlsx(out, file.path(data_dir, filename))
    cat("Wrote", filename, "(weight =", weight, "%)\n")
  })

# ── Upsert CASH_EUR / BONDS_LR_EUR into meta/tickerliste.xlsx ──────────────
# Gives both tickers a real sector so they show up correctly in the sector
# sunburst and Data Quality sector-coverage figures instead of falling into
# "Unclassified". tickerliste is keyed by ticker here (not ISIN, since these
# pseudo-holdings have none) — matches how portfolio_report.Rmd's
# ticker_sector_lookup joins (by ticker).
#
# NOTE: read without col_types = "text" here, unlike portfolio_report.Rmd's
# read — coercing every column to text on write-back would corrupt the
# existing last_updated Date column (and any other typed columns) for all
# ~4600 rows just to add these 2. Preserve native types instead.
tickerliste <- read_excel(ticker_path) %>%
  as_tibble() %>%
  mutate(last_updated = normalize_last_updated(last_updated))

new_ticker_rows <- tribble(
  ~ticker,          ~alpha2, ~alpha3, ~countryname, ~companyname,     ~asset_class, ~sector,                        ~industry,
  CASH_TICKER,      "DE",    "DEU",   "Germany",    "Cash",           "Cash",       PSEUDO_SECTORS[["cash"]],       NA_character_,
  BONDS_LR_TICKER,  "DE",    "DEU",   "Germany",    "Low Risk Bonds", "Bond",       PSEUDO_SECTORS[["bonds"]],      NA_character_
) %>%
  mutate(last_updated = Sys.Date())

tickerliste <- tickerliste %>%
  filter(!ticker %in% new_ticker_rows$ticker) %>%
  bind_rows(new_ticker_rows)

write_xlsx(tickerliste, ticker_path)

cat("\nUpdated meta/tickerliste.xlsx with CASH_EUR / BONDS_LR_EUR sector info.\n")
