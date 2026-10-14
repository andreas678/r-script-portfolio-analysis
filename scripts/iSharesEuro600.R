library(tidyverse)
library(readxl)
library(countrycode)
library(writexl)

source(here::here("scripts", "utils", "logging.R"))
source(here::here("scripts", "utils", "data_loaders.R"))
source(here::here("scripts", "utils", "ishares_loaders.R"))

# Download data (current iShares API; product id 251860 = iShares MSCI Europe UCITS ETF
# (Inc), see utils/ishares_loaders.R)
raw_data <- import_ishares_fund(251860, "iSharesEuro600", own_file = "iSharesEuro600.xlsx")

# File paths
input_path <- here::here("data", "raw", "iSharesEuro600.xlsx")
output_path <- here::here("data", "clean", "iSharesEuro600.xlsx")

# Save raw data
write_xlsx(raw_data, input_path)

# Process data: translate German country names/exchanges, append EODHD suffix
df <- translate_country_and_suffix(raw_data)
report_unmapped_exchanges(df)
df <- df %>% select(-suffix)

# Create final dataset
iSharesEuro600 <- df %>%
  rename(name = "Name") %>%
  mutate(
    weight = round(weight, 2),
    index = "iSharesEuro600",
    ISIN = NA_character_  # iShares doesn't provide an ISIN - keep the column present for structural consistency (see Portfolio to-dos A.2)
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

# Save cleaned data
write_xlsx(iSharesEuro600, output_path)

print_fund_data_quality(iSharesEuro600, "iSharesEuro600", sector_col = "sector_german")

# ============================================================================
# AUTO-UPDATE TICKERLISTE
# ============================================================================

ticker_path <- here::here("meta", "tickerliste.xlsx")
upsert_tickerliste_by_ticker(iSharesEuro600, ticker_path, sector_col = "sector_german")

