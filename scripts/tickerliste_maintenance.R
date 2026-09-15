# ============================================================================
# Tickerliste Maintenance & Enrichment
# 
# Purpose:
#   - Fix gaps in existing tickerliste (missing country codes, tickers)
#   - Add high-value enrichment (company_name, sector, industry, asset_class)
#   - Minimize API calls in fund processing scripts
#
# Phases:
#   Phase 1: Fill gaps from ISIN (deterministic, no API)
#   Phase 2: Enrich with API data (one-time cost, reusable)
#   Phase 3: Sector enrichment via Finnhub (US equities, requires API key)
#   Phase 4: Harmonize sector labels to the 11 canonical GICS sectors (no API)
#
# ============================================================================

library(tidyverse)
library(readxl)
library(writexl)
library(countrycode)
library(here)
library(httr)
library(jsonlite)

source(here::here("scripts", "utils", "sector_taxonomy.R"))
source(here::here("scripts", "utils", "security.R"))

# ============================================================================
# CONFIG
# ============================================================================

api_key <- Sys.getenv("EODHD_API_KEY")
ticker_path <- here::here("meta", "tickerliste.xlsx")
backup_path <- here::here("meta", "tickerliste_backup.xlsx")

# ============================================================================
# PHASE 1: Fill Data Gaps (No API Required)
# ============================================================================

# Load current tickerliste
tickerliste <- read_excel(ticker_path) %>% as_tibble()

cat("Initial state:\n")
cat("  Rows:", nrow(tickerliste), "\n")
cat("  Missing tickers:", sum(is.na(tickerliste$ticker)), "\n")
cat("  Missing alpha2:", sum(is.na(tickerliste$alpha2)), "\n")
cat("  Missing alpha3:", sum(is.na(tickerliste$alpha3)), "\n")
cat("  Missing country:", sum(is.na(tickerliste$countryname)), "\n\n")

# Backup original
write_xlsx(tickerliste, backup_path)

# --- 1.1: Recover country codes from ISIN prefix ---
# ISIN format: CC + NSIN (country + national security id number)
# First 2 chars = ISO 3166-1 alpha-2 country code

tickerliste <- tickerliste %>%
  mutate(
    isin_prefix = substr(ISIN, 1, 2),
    
    # Fill missing alpha2 from ISIN prefix; blank/invalid alpha2 values become NA
    # so countrycode() doesn't warn on placeholders like "__"
    alpha2 = {
      candidate <- coalesce(alpha2, isin_prefix)
      if_else(grepl("^[A-Z]{2}$", candidate), candidate, NA_character_)
    },
    
    # Regenerate country codes from alpha2 (handles newly filled values)
    alpha3 = countrycode(alpha2, "iso2c", "iso3c"),
    countryname = countrycode(alpha2, "iso2c", "country.name")
  ) %>%
  select(-isin_prefix)

cat("After Phase 1 - ISIN-based recovery:\n")
cat("  Missing alpha2:", sum(is.na(tickerliste$alpha2)), "\n")
cat("  Missing alpha3:", sum(is.na(tickerliste$alpha3)), "\n")
cat("  Missing country:", sum(is.na(tickerliste$countryname)), "\n\n")

# Check for any remaining issues
missing_country <- tickerliste %>%
  filter(is.na(alpha2) | is.na(countryname)) %>%
  select(ISIN, ticker, alpha2, countryname)

if (nrow(missing_country) > 0) {
  cat("Still missing country info:\n")
  print(missing_country)
  cat("\nThese ISINs may be invalid or from unlisted exchanges.\n\n")
}

# ============================================================================
# PHASE 2: Enrich with API Data (Optional - Controlled One-Time Cost)
# ============================================================================

# --- Function to fetch company fundamentals ---
get_company_fundamentals <- function(ticker, max_retries = 2) {
  Sys.sleep(1)  # Rate limiting
  
  url <- paste0(
    "https://eodhistoricaldata.com/api/fundamentals/",
    ticker,
    "?api_token=", api_key
  )
  
  tryCatch({
    response <- GET(url, timeout(10))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      
      return(list(
        company_name = data$General$Name %||% NA_character_,
        sector = data$General$Sector %||% NA_character_,
        industry = data$General$Industry %||% NA_character_,
        currency = data$General$Currency %||% NA_character_,
        exchange = data$General$Exchange %||% NA_character_
      ))
    } else {
      return(list(
        company_name = NA_character_,
        sector = NA_character_,
        industry = NA_character_,
        currency = NA_character_,
        exchange = NA_character_
      ))
    }
  }, error = function(e) {
    # url embeds api_key (EODHD auth is query-string only, no header option)
    # - redact before this reaches warning(), which stdout/stderr from a
    # headless run may pipe to a log file.
    warning(paste("API error for ticker", ticker, ":", redact_secret(e$message, api_key)))
    return(list(
      company_name = NA_character_,
      sector = NA_character_,
      industry = NA_character_,
      currency = NA_character_,
      exchange = NA_character_
    ))
  })
}

# --- Option A: Interactive - Enrich only new/missing entries ---
# Uncomment to enrich missing company details
#
# missing_enrichment <- tickerliste %>%
#   filter(is.na(company_name) | is.na(sector)) %>%
#   filter(!is.na(ticker))
#
# if (nrow(missing_enrichment) > 0) {
#   cat("Enriching", nrow(missing_enrichment), "entries with API data...\n")
#
#   enriched <- missing_enrichment %>%
#     mutate(
#       fundamentals = map(ticker, get_company_fundamentals),
#       company_name = map_chr(fundamentals, "company_name"),
#       sector = map_chr(fundamentals, "sector"),
#       industry = map_chr(fundamentals, "industry"),
#       currency = map_chr(fundamentals, "currency"),
#       exchange = map_chr(fundamentals, "exchange"),
#       .keep = "unused"
#     )
#   
#   # Merge back into tickerliste
#   tickerliste <- tickerliste %>%
#     rows_update(enriched, by = "ISIN")
# }

# --- Option B: Add asset class classification (deterministic) ---

tickerliste <- tickerliste %>%
  mutate(
    # Simple heuristic: identify funds/ETFs by ticker pattern
    asset_class = case_when(
      is.na(ticker) ~ "Unknown",
      str_detect(ticker, "(?i)etf|exchange") ~ "ETF",
      str_detect(ticker, "(?i)fund") ~ "Fund",
      nchar(ticker) <= 5 & !str_detect(ticker, "\\.") ~ "Equity",
      TRUE ~ "Other"
    ),
    last_updated = Sys.Date()
  )

cat("After Phase 2 - Asset classification added:\n")
cat("  Asset class distribution:\n")
print(tickerliste %>% count(asset_class))
cat("\n")

# ============================================================================
# Save Enhanced Tickerliste
# ============================================================================

write_xlsx(tickerliste, ticker_path)

cat("✓ Enhanced tickerliste saved to", ticker_path, "\n")
cat("  Total rows:", nrow(tickerliste), "\n")
cat("  New columns: asset_class, last_updated\n")
cat("  Backup saved to", backup_path, "\n\n")

# ============================================================================
# Summary Statistics
# ============================================================================

cat("Data Quality Report:\n")
cat(strrep("─", 60), "\n")

quality_report <- tickerliste %>%
  summarise(
    Total_Entries = n(),
    Complete_ISIN = sum(!is.na(ISIN)),
    Complete_Ticker = sum(!is.na(ticker)),
    Complete_Country = sum(!is.na(countryname)),
    Complete_AssetClass = sum(!is.na(asset_class)),
    .groups = "drop"
  ) %>%
  mutate(
    ISIN_Pct = round(Complete_ISIN / Total_Entries * 100, 1),
    Ticker_Pct = round(Complete_Ticker / Total_Entries * 100, 1),
    Country_Pct = round(Complete_Country / Total_Entries * 100, 1),
    AssetClass_Pct = round(Complete_AssetClass / Total_Entries * 100, 1)
  )

print(quality_report)

cat("\nTop 10 Countries:\n")
print(
  tickerliste %>%
    count(countryname, name = "Count") %>%
    arrange(desc(Count)) %>%
    head(10)
)

cat("\nAsset Class Distribution:\n")
print(
  tickerliste %>%
    count(asset_class, name = "Count") %>%
    arrange(desc(Count))
)

cat("\n✓ Tickerliste maintenance complete!\n")

# ============================================================================
# PHASE 3: Sector Enrichment via Finnhub (US stocks, free tier)
#
# Finnhub free tier supports profile2 by symbol for US-listed stocks only.
# Non-US stocks require a paid plan (or EODHD upgrade — see Phase 2 Option A).
#
# Prerequisite: FINNHUB_API_KEY must be set in .Renviron
# Run time: ~642 tickers × 1.1s ≈ 12 minutes
# ============================================================================

finnhub_key <- Sys.getenv("FINNHUB_API_KEY")

if (nchar(finnhub_key) == 0) {
  stop("FINNHUB_API_KEY not found in environment. Set it in ~/.Renviron.")
}

# --- Fetch sector + industry from Finnhub profile2 ---
get_finnhub_sector <- function(ticker) {
  Sys.sleep(1.1)  # stay within 60 calls/min free-tier limit
  url <- paste0(
    "https://finnhub.io/api/v1/stock/profile2?symbol=", ticker,
    "&token=", finnhub_key
  )
  tryCatch({
    resp <- GET(url, timeout(10))
    if (status_code(resp) == 200) {
      d <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
      list(
        sector   = d$finnhubIndustry %||% NA_character_,
        industry = NA_character_  # Finnhub returns one combined "finnhubIndustry"
      )
    } else {
      list(sector = NA_character_, industry = NA_character_)
    }
  }, error = function(e) {
    list(sector = NA_character_, industry = NA_character_)
  })
}

# --- Identify US equities needing sector enrichment ---
tickerliste <- read_excel(ticker_path) |> as_tibble()

# Add columns if not present; cast to character in all cases because readxl
# reads all-NA columns as <logical>, which causes rows_update() to type-error.
if (!"sector" %in% names(tickerliste)) {
  tickerliste <- tickerliste |> mutate(sector = NA_character_, .after = asset_class)
}
if (!"industry" %in% names(tickerliste)) {
  tickerliste <- tickerliste |> mutate(industry = NA_character_, .after = sector)
}
tickerliste <- tickerliste |>
  mutate(sector = as.character(sector), industry = as.character(industry))

us_to_enrich <- tickerliste |>
  filter(
    asset_class == "Equity",
    !is.na(ticker),
    str_sub(ISIN, 1, 2) == "US",
    is.na(sector)
  )

cat("US equities to enrich:", nrow(us_to_enrich), "\n")

# --- Run enrichment ---
enriched_us <- us_to_enrich |>
  mutate(
    result   = purrr::map(ticker, get_finnhub_sector),
    sector   = purrr::map_chr(result, "sector"),
    industry = purrr::map_chr(result, "industry"),
    .keep    = "unused"
  )

cat("Enriched:", sum(!is.na(enriched_us$sector)), "/", nrow(enriched_us), "with sector data\n")

# --- Merge back ---
tickerliste <- tickerliste |>
  rows_update(
    enriched_us |> select(ISIN, sector, industry),
    by = "ISIN"
  )

cat("\nSector distribution (US equities):\n")
print(
  tickerliste |>
    filter(str_sub(ISIN, 1, 2) == "US", asset_class == "Equity") |>
    count(sector, sort = TRUE)
)

# --- Save ---
write_xlsx(tickerliste, ticker_path)
cat("\n✓ Phase 3 complete. Sector data written to", ticker_path, "\n")

# ============================================================================
# PHASE 4: GICS Sector Harmonization
#
# sector labels collected above (raw Finnhub finnhubIndustry values, which
# mix GICS sector- and sub-industry-level names) and the German sector
# labels some fund exports carry (e.g. "Finanzwesen", "Zyklische
# Konsumgüter") are not consistent with each other or with the GICS
# standard. portfolio_report.Rmd's volatility_estimates table (and its
# `left_join(volatility_estimates, by = "sector")`) expects the 11 canonical
# English GICS sector names exactly - anything else silently fails that
# join and falls through to the 22% default. This phase maps every known
# raw label onto its canonical GICS sector, preserving the pre-harmonization
# value in `sector_orig` for traceability. No API calls, deterministic.
#
# Cash/bonds pseudo-holdings ("Cash & Equivalents", "Fixed Income", and the
# German "Cash und/oder Derivate") are not real GICS sectors (see
# scripts/CashAndBonds.R) and are normalized to the two pseudo-sector labels
# used elsewhere rather than forced into one of the 11 GICS sectors.
# ============================================================================

gics_sector_map <- c(
  # German GICS-level labels (fund exports / Finnhub German locale)
  "Industrie"                  = "Industrials",
  "IT"                         = "Information Technology",
  "Finanzwesen"                = "Financials",
  "Materialien"                = "Materials",
  "Zyklische Konsumgüter"      = "Consumer Discretionary",
  "Gesundheitsversorgung"      = "Health Care",
  "Nichtzyklische Konsumgüter" = "Consumer Staples",
  "Immobilien"                 = "Real Estate",
  "Kommunikation"              = "Communication Services",
  "Versorger"                  = "Utilities",
  "Energie"                    = "Energy",

  # Canonical English GICS sector names (identity - already correct)
  "Information Technology"     = "Information Technology",
  "Health Care"                = "Health Care",
  "Financials"                 = "Financials",
  "Industrials"                = "Industrials",
  "Consumer Discretionary"     = "Consumer Discretionary",
  "Communication Services"     = "Communication Services",
  "Consumer Staples"           = "Consumer Staples",
  "Energy"                     = "Energy",
  "Utilities"                  = "Utilities",
  "Real Estate"                = "Real Estate",
  "Materials"                  = "Materials",

  # Finnhub sub-industry / alternate-name labels -> parent GICS sector
  "Technology"                        = "Information Technology",
  "Semiconductors"                    = "Information Technology",
  "Financial Services"                = "Financials",
  "Banking"                           = "Financials",
  "Insurance"                         = "Financials",
  "Retail"                            = "Consumer Discretionary",
  "Electrical Equipment"              = "Industrials",
  "Media"                             = "Communication Services",
  "Biotechnology"                     = "Health Care",
  "Hotels, Restaurants & Leisure"     = "Consumer Discretionary",
  "Machinery"                         = "Industrials",
  "Life Sciences Tools & Services"    = "Health Care",
  "Food Products"                     = "Consumer Staples",
  "Professional Services"             = "Industrials",
  "Consumer products"                 = "Consumer Staples",
  "Chemicals"                         = "Materials",
  "Aerospace & Defense"               = "Industrials",
  "Communications"                    = "Communication Services",
  "Construction"                      = "Industrials",
  "Pharmaceuticals"                   = "Health Care",
  "Building"                          = "Industrials",
  "Beverages"                         = "Consumer Staples",
  "Commercial Services & Supplies"    = "Industrials",
  "Trading Companies & Distributors"  = "Industrials",
  "Road & Rail"                       = "Industrials",
  "Packaging"                         = "Materials",
  "Telecommunication"                 = "Communication Services",
  "Textiles, Apparel & Luxury Goods"  = "Consumer Discretionary",
  "Automobiles"                       = "Consumer Discretionary",
  "Logistics & Transportation"        = "Industrials",
  "Metals & Mining"                   = "Materials",
  "Airlines"                          = "Industrials",
  "Distributors"                      = "Consumer Discretionary",
  "Diversified Consumer Services"     = "Consumer Discretionary",
  "Industrial Conglomerates"          = "Industrials",
  "Marine"                            = "Industrials",

  # Cash / fixed-income pseudo-holdings - normalized, not GICS-mapped
  "Cash und/oder Derivate" = "Cash & Equivalents",
  "Cash & Equivalents"     = "Cash & Equivalents",
  "Fixed Income"           = "Fixed Income"
)

# Guards against a typo'd target value in gics_sector_map above silently
# introducing a sector name that portfolio_report.Rmd's volatility_estimates
# join (keyed on GICS_SECTORS) doesn't recognize.
stopifnot(all(gics_sector_map %in% c(GICS_SECTORS, PSEUDO_SECTORS)))

tickerliste <- read_excel(ticker_path) |> as_tibble()

# Preserve the pre-harmonization label. On re-runs, only backfill sector_orig
# where it's still empty, so an already-harmonized value never overwrites
# the true raw label captured on a prior run.
if (!"sector_orig" %in% names(tickerliste)) {
  tickerliste <- tickerliste |> mutate(sector_orig = sector, .after = sector)
} else {
  tickerliste <- tickerliste |> mutate(sector_orig = coalesce(sector_orig, sector))
}

sector_trimmed <- str_trim(tickerliste$sector)
unmapped <- setdiff(unique(sector_trimmed[!is.na(sector_trimmed)]), names(gics_sector_map))

if (length(unmapped) > 0) {
  cat("\nSector values with no GICS mapping (left unchanged):\n")
  print(unmapped)
}

# Unknown/unmapped labels fall back to their existing value via coalesce()
# rather than becoming NA, so an incomplete mapping table degrades safely.
tickerliste <- tickerliste |>
  mutate(sector = unname(coalesce(gics_sector_map[sector_trimmed], sector)))

cat("\nHarmonized sector distribution:\n")
print(tickerliste |> count(sector, sort = TRUE))

file.copy(ticker_path, backup_path, overwrite = TRUE)
write_xlsx(tickerliste, ticker_path)
cat("\n✓ Phase 4 complete. Sector labels harmonized to GICS, written to", ticker_path, "\n")
