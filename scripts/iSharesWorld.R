library(tidyverse)
library(readxl)
library(countrycode)
library(writexl)
library(xml2)

source(here::here("scripts", "utils", "logging.R"))
source(here::here("scripts", "utils", "data_loaders.R"))
source(here::here("scripts", "utils", "ishares_loaders.R"))

# iShares Core MSCI World UCITS ETF (IE00B4L5Y983), physically replicated. Used as
# the PROXY holdings for the Amundi Core MSCI World SWAP ETF, whose own published
# holdings are its swap collateral basket, not the index exposure
# (config/fund_proxies.csv; see CLAUDE.md "Fund proxies"). The fund itself is not in
# config/portfolio_configs.csv, so this file only feeds the proxy.

# Country/exchange are not in this export: taken from earlier exports of the other iShares
# funds (utils/ishares_loaders.R, add_country_exchange). Needs at least one of them in data/raw.
raw_data <- import_ishares_fund(251882, "iSharesWorld", own_file = "iSharesWorld.xlsx")

input_path  <- here::here("data", "raw", "iSharesWorld.xlsx")
output_path <- here::here("data", "clean", "iSharesWorld.xlsx")

write_xlsx(raw_data, input_path)

# Translate German country names/exchanges, append EODHD suffix
df <- translate_country_and_suffix(raw_data)
report_unmapped_exchanges(df)
df <- df %>% select(-suffix)

iSharesWorld <- df %>%
  rename(name = "Name") %>%
  mutate(
    weight = round(weight, 2),
    index = "iSharesWorld",
    ISIN = NA_character_  # iShares doesn't provide an ISIN - column kept for structural consistency
  ) %>%
  filter(!is.na(weight)) %>%
  select(
    ISIN,
    name,
    weight,
    ticker,
    countryname,
    alpha2,
    alpha3,
    sector_german = Sektor,
    country_german = country,
    index
  )

write_xlsx(iSharesWorld, output_path)

print_fund_data_quality(iSharesWorld, "iSharesWorld", sector_col = "sector_german")

# Auto-update tickerliste (new tickers only; existing rows are left untouched)
ticker_path <- here::here("meta", "tickerliste.xlsx")
upsert_tickerliste_by_ticker(iSharesWorld, ticker_path, sector_col = "sector_german")
