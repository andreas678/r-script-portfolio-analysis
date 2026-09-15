library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(writexl)
library(countrycode)
library(here)

source(here::here("scripts", "utils", "data_loaders.R"))

# Download the XLS file from
# https://www.amundietf.de/de/privatanleger/products/equity/amundi-core-msci-world-swap-ucits-etf-dist/lu2572257124
# and save it unchanged as data/raw/AmundiCoreWorld.xlsx - no manual editing needed.

input_path <- here::here("data", "raw", "AmundiCoreWorld.xlsx")
output_path <- here::here("data", "clean", "AmundiCoreWorld.xlsx")
ticker_path <- here::here("meta", "tickerliste.xlsx")

df <- read_holdings_table_by_isin(input_path, weight_col = "Gewichtung")

tickerliste <- resolve_and_upsert_tickers(df, ticker_path)

# --- Build the final holdings dataset ---
AmundiCoreWorld <- df %>%
  left_join(tickerliste, by = "ISIN") %>%
  rename(weight = "Gewichtung", name = "Name") %>%
  mutate(
    weight = as.double(sub(",", ".", weight, fixed = TRUE)),
    weight = if_else(weight > 0 & weight < 1, weight * 100, weight),  # raw export uses a 0-1 fraction
    weight = round(weight, 2)
  ) %>%
  filter(!is.na(weight)) %>%
  mutate(index = "AmundiCoreWorld") %>%
  select(ISIN, name, weight, ticker, alpha2, alpha3, countryname, index)

write_xlsx(AmundiCoreWorld, output_path)

print(AmundiCoreWorld)
