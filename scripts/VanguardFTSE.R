library(tidyverse)
library(readxl)
library(countrycode)
library(writexl)

source(here::here("scripts", "utils", "logging.R"))
source(here::here("scripts", "utils", "data_loaders.R"))

# Raw file: manually downloaded from Vanguard's "Portfolio holdings" export
# (CSV, semicolon-delimited, German locale) and saved unchanged as
# data/raw/VanguardFTSE.csv - no manual editing needed.
# https://www.de.vanguard/private-anleger/anlageprodukte/etf/aktien/9679/ftse-all-world-ucits-etf-usd-accumulating
input_path <- here::here("data", "raw", "VanguardFTSE.csv")
output_path <- here::here("data", "clean", "VanguardFTSE.xlsx")
ticker_path <- here::here("meta", "tickerliste.xlsx")

# Import holdings. The export's first 6 lines are metadata (download date,
# fund name, as-of date, a blank line) before the real header on line 7, so
# skip = 6. "% der Assets" arrives as e.g. "4,4401 %" - col_number() (the "n"
# col type) strips the trailing "%"/non-breaking space, and read_csv2's
# German locale (decimal_mark = ",") handles the comma decimal.
raw_data <- read_csv2(
  input_path,
  show_col_types = TRUE,
  col_types = cols(`% der Assets` = "n"),
  skip = 6
) %>%
  rename(
    name        = `Wertpapiere`,
    weight      = `% der Assets`,
    alpha2      = `Region`,
    ticker      = `Ticker`,
    sector_orig = `Sektor`
  )

# Process data: derive country name/codes from the export's ISO2 "Region"
# column (already a country code, unlike iShares' free-text German country
# names, so no name-translation lookup is needed here).
df <- raw_data %>%
  mutate(
    alpha2         = tolower(alpha2),
    country_german = countrycode(alpha2, origin = "iso2c", destination = "country.name.de"),
    countryname    = countrycode(alpha2, origin = "iso2c", destination = "country.name"),
    alpha3         = countrycode(alpha2, origin = "iso2c", destination = "iso3c"),
    alpha2         = toupper(alpha2),
    index          = "VanguardFTSE",
    ISIN           = NA_character_  # Vanguard's holdings export doesn't include a per-holding ISIN
  )

# Unlike the iShares exports, Vanguard's export gives only the holding's
# country (Region), not its exchange - so there's no reliable basis to
# append an EODHD exchange suffix here (e.g. bare "2330" for a Taiwan
# holding). Left bare rather than guessed, same convention as unmapped
# exchanges in iSharesACWI.R/iSharesEMIMI.R; surfaced once so the gap is
# visible instead of silently wrong.
non_us_holdings <- df %>%
  filter(!is.na(weight), weight > 0, alpha2 != "US") %>%
  nrow()

if (non_us_holdings > 0) {
  msg <- paste0(non_us_holdings, " non-US holding(s) have a bare ticker with no EODHD exchange ",
                "suffix - Vanguard's export doesn't provide the exchange name needed to append one.")
  cat("\nNote:", msg, "\n\n")
  log_pipeline_issue(msg)
}

# Create final dataset. Only NA weight is dropped here (not weight == 0) so
# the long tail of tiny FTSE All-World constituents (many round to 0.00%)
# survives into data/clean/ - portfolio_report.Rmd's own construction step
# already filters weight > 0 downstream, same convention as the other
# per-fund cleaner scripts.
VanguardFTSE <- df %>%
  mutate(weight = round(weight, 2)) %>%
  filter(!is.na(weight)) %>%
  select(
    ISIN,
    name,
    weight,
    ticker,
    countryname,
    alpha2,
    alpha3,
    sector_orig,
    country_german,
    index
  )

# Save cleaned data
write_xlsx(VanguardFTSE, output_path)

print_fund_data_quality(VanguardFTSE, "VanguardFTSE", sector_col = "sector_orig")

# ============================================================================
# AUTO-UPDATE TICKERLISTE
# ============================================================================

upsert_tickerliste_by_ticker(VanguardFTSE, ticker_path, sector_col = "sector_orig")
