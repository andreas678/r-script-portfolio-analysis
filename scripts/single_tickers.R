library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(writexl)
library(countrycode)
library(here)

source(here::here("scripts", "utils", "data_loaders.R"))

# File paths
input_path <- here::here("data", "raw", "single_tickers.csv")
output_path <- here::here("data", "clean", "single_tickers.xlsx")
ticker_path <- here::here("meta", "tickerliste.xlsx")
# sheet_name <- "18-06-2026"

# Maintain this table manually.
# Enter weights directly >> the "ETF Einzelaktien" pseudo-fund is weighted at 1.0!

# Load CSV and convert to tibble (keep original column names)
df <- read.csv2(input_path, colClasses = "character", check.names = FALSE) |> as_tibble()

if (!"ISIN" %in% colnames(df)) {
  stop("Error: Column 'ISIN' not found! Please check column names.")
}

tickerliste <- resolve_and_upsert_tickers(df, ticker_path)

# Create final dataset
Einzelaktien <- df |>
  left_join(tickerliste, by = "ISIN") |>
  rename(
    weight = "% Fondsvermögen",
    name   = "Gattungsbezeichnung"
  ) |>
  mutate(weight = parse_weight_pct(weight)) |>
  filter(!is.na(weight)) |>  # Remove rows with invalid weights
  mutate(index = "Einzelaktien") |>
  select(ISIN, name, weight, ticker, alpha2, alpha3, countryname, index)

# Save results
write_xlsx(Einzelaktien, output_path)

print(Einzelaktien)

