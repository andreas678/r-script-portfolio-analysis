library(tidyverse)
library(readxl)
library(countrycode)
library(writexl)

source(here::here("scripts", "utils", "logging.R"))
source(here::here("scripts", "utils", "data_loaders.R"))

# API-Key
api_key <- Sys.getenv("EODHD_API_KEY")

# German to English country name mapping
# iShares API returns German country names, countrycode expects English names
country_mapping <- tribble(
  ~german, ~english,
  "Indien", "India",
  "Deutschland", "Germany",
  "Brasilien", "Brazil",
  "Südafrika", "South Africa",
  "Saudi-Arabien", "Saudi Arabia",
  "Mexiko", "Mexico",
  "Ungarn", "Hungary",
  "Irland", "Ireland",
  "Polen", "Poland",
  "Ver. Arabische Emirate", "United Arab Emirates",
  "Vereinigte Staaten", "United States",
  "Indonesien", "Indonesia",
  "Philippinen", "Philippines",
  "Griechenland", "Greece",
  "Türkei", "Turkey",
  "Kolumbien", "Colombia",
  "Ägypten", "Egypt",
  "Tschechien", "Czechia",
  "Kanada", "Canada",
  "Singapur", "Singapore",
  "Russland", "Russia",
  "Vereinigtes Königreich", "United Kingdom",
  "Schweiz", "Switzerland"
)

# EODHD exchange suffix per iShares "Börse" (exchange) value. Bare local
# tickers (e.g. "2330", "000660") are ambiguous across exchanges - EODHD's
# EOD endpoint requires the exchange-qualified form. Verified directly
# against the live EOD endpoint (not iShares/Yahoo naming, which differs -
# e.g. Korea is ".KO" here, not the Yahoo convention ".KS"). US exchanges
# map to NA since EODHD's default (no suffix) already resolves those.
# Exchanges not listed here are left unmapped (ticker stays bare, same as
# before this fix) rather than guessed.
exchange_suffix_map <- tribble(
  ~Börse,                                  ~suffix,
  "Taiwan Stock Exchange",                 "TW",
  "Korea Exchange (Stock Market)",         "KO",
  "Korea Exchange (Kosdaq)",               "KQ",
  "Hong Kong Exchanges And Clearing Ltd",  "HK",
  "NASDAQ",                                NA_character_,
  "New York Stock Exchange Inc.",          NA_character_
)

# Function to import ETF holdings from iShares CSV
import_ishares <- function(url, indexName) {
  read_csv(url, skip = 2, show_col_types = FALSE) %>%
    rename(
      weight = "Gewichtung (%)",
      country = "Standort",
      ticker = "Emittententicker"
    ) %>%
    mutate(weight = as.double(sub(",", ".", weight, fixed = TRUE))) %>%
    add_column(index = indexName)
}

# Download data
raw_data <- import_ishares(
  "https://www.ishares.com/de/privatanleger/de/produkte/264659/ishares-msci-emerging-markets-imi-ucits-etf/1478358465952.ajax?fileType=csv&fileName=SXRV_holdings&dataType=fund",
  "iSharesEMIMI"
)

# File paths
input_path <- here::here("data", "raw", "iSharesEMIMI.xlsx")
output_path <- here::here("data", "clean", "iSharesEMIMI.xlsx")

# Save raw data
write_xlsx(raw_data, input_path)

# Process data: translate German country names and add country codes
df <- raw_data %>%
  # Translate country names from German to English
  left_join(country_mapping, by = c("country" = "german")) %>%
  # Append the EODHD exchange suffix (e.g. "2330" -> "2330.TW") so downstream
  # price fetches hit the correct listing instead of a bare, exchange-ambiguous code
  left_join(exchange_suffix_map, by = "Börse") %>%
  mutate(
    country_en = coalesce(english, country),  # Use English translation if available, else keep original
    # Add country codes using English names
    alpha2 = countrycode(country_en, "country.name", "iso2c"),
    alpha3 = countrycode(country_en, "country.name", "iso3c"),
    countryname = countrycode(country_en, "country.name", "country.name"),
    ticker = if_else(!is.na(suffix), paste(ticker, suffix, sep = "."), ticker)
  ) %>%
  select(-english, -country_en)  # Remove temporary columns

# Surface exchanges we haven't verified/mapped yet (ticker stays bare for
# these, same as before this fix) so gaps are visible instead of silently wrong
unmapped_exchanges <- df %>%
  filter(!is.na(weight), weight > 0, is.na(suffix), !Börse %in% exchange_suffix_map$Börse) %>%
  distinct(Börse) %>%
  pull(Börse)

if (length(unmapped_exchanges) > 0) {
  cat("\nNote: no EODHD exchange suffix mapped for:\n -",
      paste(unmapped_exchanges, collapse = "\n - "),
      "\nTickers on these exchanges are kept bare and may fail to fetch price/volatility data.\n\n")
  log_pipeline_issue(paste0("No EODHD exchange suffix mapped for: ",
                             paste(unmapped_exchanges, collapse = ", "),
                             " - tickers on these exchanges are kept bare and may fail to fetch price/volatility data."))
}

df <- df %>% select(-suffix)

# Create final dataset
iSharesEMIMI <- df %>%
  rename(name = "Name") %>%
  mutate(
    weight = round(weight, 2),
    index = "iSharesEMIMI",
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
    alpha2,
    alpha3,
    index
  )

# Save cleaned data
write_xlsx(iSharesEMIMI, output_path)


# Data quality report
cat("\n", paste(rep("=", 50), collapse = ""), "\n")
cat("iSharesEMIMI Data Summary\n")
cat(paste(rep("=", 50), collapse = ""), "\n\n")

cat("Total holdings:", nrow(iSharesEMIMI), "\n")
cat("Total weight:", sum(iSharesEMIMI$weight, na.rm = TRUE), "%\n")
cat("Unique countries:", n_distinct(iSharesEMIMI$countryname, na.rm = TRUE), "\n\n")

# Check for unmapped countries
unmapped <- iSharesEMIMI %>%
  filter(is.na(alpha2)) 

if (nrow(unmapped) > 0) {
  cat("Note: The following locations have no country codes (intentional):\n")
  print(unmapped)
  cat("\nThese are typically regional classifications or special categories.\n\n")
}

# Top 10 holdings
cat("Top 10 Holdings:\n")
print(
  iSharesEMIMI %>%
    arrange(desc(weight)) %>%
    select(name, weight, countryname, sector_german) %>%
    head(10)
)

# ============================================================================
# AUTO-UPDATE TICKERLISTE
# ============================================================================

# Define paths
ticker_path <- here::here("meta", "tickerliste.xlsx")
tickerliste <- read_excel(ticker_path) %>%
  as_tibble() %>%
  mutate(last_updated = normalize_last_updated(last_updated))

# 1. Identify new tickers (those in iShares but not in tickerliste)
new_entries <- iSharesEMIMI %>%
  filter(!ticker %in% tickerliste$ticker) %>%
  # Create/Map columns to match tickerliste structure
  mutate(
    ISIN = NA_character_,
    companyname = name,
    asset_class = "Equity",
    sector = sector_german,
    industry = NA_character_,
    last_updated = Sys.Date()
  ) %>%
  # Select and order columns to match tickerliste exactly
  select(any_of(names(tickerliste)))

# 2. Append if new entries exist
if (nrow(new_entries) > 0) {
  # Use bind_rows for a safe merge
  tickerliste_updated <- bind_rows(tickerliste, new_entries)

  # Remove duplicates just in case of race conditions
  tickerliste_updated <- tickerliste_updated %>%
    distinct(ticker, .keep_all = TRUE)
  
  write_xlsx(tickerliste_updated, ticker_path)
  message(paste("Successfully added", nrow(new_entries), "new tickers to", ticker_path))
} else {
  message("No new tickers to add.")
}

