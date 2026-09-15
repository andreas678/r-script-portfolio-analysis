library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(writexl)
library(countrycode)
library(here)
library(stringdist)

source(here::here("scripts", "utils", "logging.R"))

# API key
api_key <- Sys.getenv("EODHD_API_KEY")

# Download the CSV file from
# https://www.columbiathreadneedle.com/de/de/private/fund-details/ct-lux-european-smaller-companies-1e-eur_sxesmc_lu1864952335/

# File paths
input_path <- here::here("data", "raw", "CTLuxEuroSmComp.csv")
output_path <- here::here("data", "clean", "CTLuxEuroSmComp.xlsx")
ticker_path <- here::here("meta", "tickerliste.xlsx")

# Read CSV data
df <- read.csv(input_path, encoding = "UTF-8") %>% as_tibble()

# Inspect columns
cat("CSV columns:\n")
print(names(df))

# Identify the column with company names
# Column is named "Bezeichnung.der.Investition" or similar
company_col <- grep("Bezeichnung|Investment|Name", names(df), ignore.case = TRUE)[1]
weight_col <- grep("Gewicht|Weight", names(df), ignore.case = TRUE)[1]

cat("\nColumns found:\n")
cat(sprintf("  Company: %s\n", names(df)[company_col]))
cat(sprintf("  Weight: %s\n", names(df)[weight_col]))

# Prepare raw data
df <- df %>%
  select(
    name = all_of(company_col),
    weight = all_of(weight_col)
  ) %>%
  mutate(
    # Convert weight from string to numeric
    weight = as.numeric(gsub(",", ".", weight, fixed = TRUE))
  ) %>%
  filter(!is.na(weight), weight > 0) %>%
  distinct(name, .keep_all = TRUE)

cat("\nRaw data loaded:\n")
cat(sprintf("  Rows: %d\n", nrow(df)))
cat(sprintf("  Columns: %s\n\n", paste(names(df), collapse = ", ")))

# ============================================================================
# Load tickerliste
# ============================================================================

tickerliste <- read_excel(ticker_path) %>%
  as_tibble() %>%
  select(
    ISIN,
    ticker,
    companyname,
    alpha2,
    alpha3,
    countryname
  )

cat("Tickerliste loaded:\n")
cat(sprintf("  Rows: %d\n", nrow(tickerliste)))
cat(sprintf("  With companyname: %d\n\n", sum(!is.na(tickerliste$companyname))))

# ============================================================================
# Matching: company names → ISIN and ticker from tickerliste
# ============================================================================

cat("Matching company names against tickerliste...\n")
cat(strrep("─", 70), "\n\n")

# Function for fuzzy-matching company names
match_company_to_ticker <- function(company_name, tickerliste_db, threshold = 0.8) {

  # Clean both names for better matching
  clean_name <- function(x) {
    x %>%
      toupper() %>%
      # Strip AG, SA, SE, Ltd, Inc, Plc, etc.
      gsub("\\s+(AG|SA|SE|SPA|Ltd|Inc|Plc|PLC|NV|OYJ|A/S|ASA)$", "", .) %>%
      # Strip umlauts and special characters
      gsub("[ÄÖÜäöü]", "", .) %>%
      trimws()
  }

  company_clean <- clean_name(company_name)

  # Filter tickerliste to entries with a companyname
  candidates <- tickerliste_db %>%
    filter(!is.na(companyname)) %>%
    mutate(
      companyname_clean = clean_name(companyname),
      # Compute string similarity
      similarity = stringsim(company_clean, companyname_clean, method = "jw")
    ) %>%
    arrange(desc(similarity)) %>%
    slice(1)

  # Only return a match if similarity is above the threshold
  if (nrow(candidates) > 0 && candidates$similarity[1] >= threshold) {
    return(list(
      ISIN = candidates$ISIN[1],
      ticker = candidates$ticker[1],
      alpha2 = candidates$alpha2[1],
      alpha3 = candidates$alpha3[1],
      countryname = candidates$countryname[1],
      similarity = candidates$similarity[1],
      found = TRUE
    ))
  } else {
    return(list(
      ISIN = NA_character_,
      ticker = NA_character_,
      alpha2 = NA_character_,
      alpha3 = NA_character_,
      countryname = NA_character_,
      similarity = NA_real_,
      found = FALSE
    ))
  }
}

# Apply matching to all company names
match_results <- purrr::map(df$name, match_company_to_ticker, tickerliste_db = tickerliste)

df_enriched <- df %>%
  mutate(
    ISIN = purrr::map_chr(match_results, "ISIN"),
    ticker = purrr::map_chr(match_results, "ticker"),
    alpha2 = purrr::map_chr(match_results, "alpha2"),
    alpha3 = purrr::map_chr(match_results, "alpha3"),
    countryname = purrr::map_chr(match_results, "countryname"),
    similarity = purrr::map_dbl(match_results, "similarity"),
    match_found = purrr::map_lgl(match_results, "found")
  )

# Summary of matching
cat("Matching results:\n")
cat(sprintf("  Total: %d\n", nrow(df_enriched)))
cat(sprintf("  Found (>0.8): %d (%.1f%%)\n",
            sum(df_enriched$match_found),
            sum(df_enriched$match_found) / nrow(df_enriched) * 100))
cat(sprintf("  Not found: %d\n\n", sum(!df_enriched$match_found)))

# Show unmatched entries
unmatched <- df_enriched %>%
  filter(!match_found) %>%
  select(name)

if (nrow(unmatched) > 0) {
  cat("Unmatched company names:\n")
  print(unmatched)
  cat("\n")
}

# For unmatched entries: retry with a lower threshold
unmatched_rows <- which(!df_enriched$match_found)

if (length(unmatched_rows) > 0) {
  cat(sprintf("Retrying matching with a lower threshold (0.7) for %d entries...\n", length(unmatched_rows)))
  cat(strrep("─", 70), "\n")

  for (idx in unmatched_rows) {
    company_name <- df_enriched$name[idx]

    # Retry with a lower threshold
    result <- match_company_to_ticker(company_name, tickerliste, threshold = 0.7)

    if (result$found) {
      df_enriched$ISIN[idx] <- result$ISIN
      df_enriched$ticker[idx] <- result$ticker
      df_enriched$alpha2[idx] <- result$alpha2
      df_enriched$alpha3[idx] <- result$alpha3
      df_enriched$countryname[idx] <- result$countryname
      df_enriched$similarity[idx] <- result$similarity
      df_enriched$match_found[idx] <- TRUE

      cat(sprintf("  ✓ %s → %s (%.2f)\n", company_name, result$ticker, result$similarity))
    }
  }

  cat("\n")
}

# Final results
final_result <- df_enriched %>%
  filter(match_found) %>%
  select(ISIN, name, weight, ticker, alpha2, alpha3, countryname) %>%
  mutate(
    index = "CTLuxEuroSmComp",
    weight = round(weight, 2)
  ) %>%
  select(ISIN, name, weight, ticker, alpha2, alpha3, countryname, index)

cat("Final results:\n")
cat(strrep("─", 70), "\n")
cat(sprintf("  With ISIN and ticker: %d / %d (%.1f%%)\n",
            nrow(final_result),
            nrow(df),
            nrow(final_result) / nrow(df) * 100))

print(head(final_result, 10))

# Unmatched holdings are permanently dropped from the file here (only the
# console log above documents them) - also surface them via warning() so
# the loss isn't only findable in the console scrollback.
dropped_names <- df_enriched$name[!df_enriched$match_found]
if (length(dropped_names) > 0) {
  dropped_msg <- paste0(length(dropped_names), " of ", nrow(df), " holdings could not be matched to ",
                         "an ISIN/ticker via fuzzy matching and were dropped from the final file: ",
                         paste(dropped_names, collapse = ", "))
  warning(dropped_msg, call. = FALSE)
  log_pipeline_issue(dropped_msg)
}

# Save result
write_xlsx(final_result, output_path)

cat("\n✓ File saved: ", output_path, "\n")
cat("\nAlso saving to environment:\n")

# Also make available as a tibble
CTLuxEuroSmComp <- final_result
print(CTLuxEuroSmComp)
