library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(writexl)
library(countrycode)
library(here)

source(here::here("scripts", "utils", "data_loaders.R"))

# Download the XLS file from
# https://www.deka-etf.de/etfs/Deka-MSCI-World-Climate-Change-ESG-CTB-UCITS-ETF
# — no manual editing needed, the sheet/header detection
# below finds the table automatically.

# File paths
input_path <- here::here("data", "raw", "DekaWorldClimateChange.xlsx")
output_path <- here::here("data", "clean", "DekaWorldClimateChange.xlsx")
ticker_path <- here::here("meta", "tickerliste.xlsx")

# Read Excel data
df <- read_holdings_table_by_isin(input_path, weight_col = "Gewichtung")

tickerliste <- resolve_and_upsert_tickers(df, ticker_path)

# --- Build result dataset ---
DekaWorldClimateChange <- df %>%
  left_join(tickerliste, by = "ISIN") %>%
  rename(
    weight = "Gewichtung",
    name   = "Holding Name"
  ) %>%
  mutate(
    weight = as.double(sub(",", ".", weight, fixed = TRUE)),
    weight = if_else(weight > 0 & weight < 1, weight * 100, weight),  # More explicit condition
    weight = round(weight, 2)
  ) %>%
  filter(!is.na(weight)) %>%  # Remove rows with invalid weights
  mutate(index = "DekaWorldClimateChange") %>%
  select(ISIN, name, weight, ticker, alpha2, alpha3, countryname, index)

# Save result
write_xlsx(DekaWorldClimateChange, output_path)

print(DekaWorldClimateChange)
