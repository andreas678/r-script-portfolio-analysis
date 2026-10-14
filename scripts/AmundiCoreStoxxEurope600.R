library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(writexl)
library(countrycode)
library(here)

source(here::here("scripts", "utils", "data_loaders.R"))

# Amundi Core STOXX Europe 600 UCITS ETF Acc (LU0908500753, physically replicated).
# Download the holdings XLS from the fund's page on amundietf.de
# https://www.amundietf.de/de/privatanleger/products/equity/amundi-core-stoxx-europe-600-ucits-etf-acc/lu0908500753
# and save it unchanged as data/raw/AmundiCoreStoxxEurope600.xlsx - no manual editing needed.

input_path <- here::here("data", "raw", "AmundiCoreStoxxEurope600.xlsx")
output_path <- here::here("data", "clean", "AmundiCoreStoxxEurope600.xlsx")
ticker_path <- here::here("meta", "tickerliste.xlsx")

df <- read_holdings_table_by_isin(input_path, weight_col = "Gewichtung")

tickerliste <- resolve_and_upsert_tickers(df, ticker_path)

# --- Build the final holdings dataset ---
AmundiCoreStoxxEurope600 <- df %>%
  left_join(tickerliste, by = "ISIN") %>%
  rename(weight = "Gewichtung", name = "Name") %>%
  mutate(weight = parse_weight_pct(weight)) %>%
  filter(!is.na(weight)) %>%
  mutate(index = "AmundiCoreStoxxEurope600") %>%
  select(ISIN, name, weight, ticker, alpha2, alpha3, countryname, index)

write_xlsx(AmundiCoreStoxxEurope600, output_path)

print(AmundiCoreStoxxEurope600)
