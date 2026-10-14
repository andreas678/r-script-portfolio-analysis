library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(writexl)
library(countrycode)

source(here::here("scripts", "utils", "logging.R"))
source(here::here("scripts", "utils", "security.R"))

# ISIN country prefix -> EODHD exchange code(s) of that country's home market,
# in order of preference. An ISIN identifies the security, not a listing, and
# EODHD's search returns one row per listing (e.g. Air Liquide on PA, LSE,
# XETRA, ...); the home market is the listing the rest of the pipeline
# (prices, currency, sector) should default to. Countries without an entry
# (funds domiciled in IE/LU/KY, ...) fall back to the search's own ranking.
ISIN_HOME_EXCHANGES <- list(
  US = "US", CA = "TO", GB = "LSE", FR = "PA", DE = "XETRA", NL = "AS", BE = "BR",
  CH = "SW", ES = "MC", AT = "VI", AU = "AU", DK = "CO", IT = "MI", SE = "ST",
  NO = "OL", FI = "HE", PT = "LS", IE = "IR", JP = "TSE", HK = "HK",
  KR = c("KO", "KQ"), TW = c("TW", "TWO"), SG = "SG"
)

# Picks one listing from an EODHD search response and formats it as the
# ticker the rest of the pipeline expects: bare `Code` for a US listing (the
# convention, and what EODHD assumes for a suffix-less symbol), otherwise
# `Code.Exchange` (e.g. "AI.PA").
#
# Why the exchange must be kept: the search returns several listings per
# ISIN. Keeping only `Code` produced suffix-less tickers for non-US
# companies, which EODHD's EOD endpoint then resolved on the US exchange -
# silently pulling a different company's prices (bare "AI" is C3.ai, not
# Air Liquide; bare "MC" is Moelis, not LVMH).
#
# Args:
#   hits: parsed search response (data.frame with Code and, ideally, Exchange
#     and ISIN columns), or anything else for "no result"
#   isin: the ISIN that was searched; its country prefix selects the home market
#
# Returns: ticker string, or NA_character_ if there is no usable hit.
pick_listing_from_search <- function(hits, isin) {
  if (!is.data.frame(hits) || nrow(hits) == 0 || !"Code" %in% names(hits)) return(NA_character_)

  hits <- hits[!is.na(hits$Code) & nzchar(hits$Code), , drop = FALSE]
  if (nrow(hits) == 0) return(NA_character_)

  # Search by ISIN should only return that security; guard anyway.
  if ("ISIN" %in% names(hits) && any(hits$ISIN == isin, na.rm = TRUE)) {
    hits <- hits[!is.na(hits$ISIN) & hits$ISIN == isin, , drop = FALSE]
  }

  chosen <- 1L
  if ("Exchange" %in% names(hits)) {
    for (ex in ISIN_HOME_EXCHANGES[[substr(isin, 1, 2)]]) {
      m <- which(hits$Exchange == ex)
      if (length(m) > 0) { chosen <- m[1]; break }
    }
  }

  code <- hits$Code[chosen]
  ex   <- if ("Exchange" %in% names(hits)) hits$Exchange[chosen] else NA_character_
  if (is.na(ex) || !nzchar(ex) || ex == "US") code else paste0(code, ".", ex)
}

# Looks up a ticker for an ISIN via EODHD's search API (rate-limited to 1 req/sec).
# Returns "Code.Exchange" for the ISIN's home market (see pick_listing_from_search).
get_ticker_data_from_isin <- function(isin, api_key) {
  Sys.sleep(1)
  url <- paste0("https://eodhistoricaldata.com/api/search/", isin, "?api_token=", api_key)

  tryCatch({
    response <- GET(url, timeout(10))
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      return(pick_listing_from_search(data, isin))
    }
    return(NA_character_)
  }, error = function(e) {
    # httr embeds the request URL (incl. api_key) in network-level errors
    warning(paste("API error for ISIN", isin, ":", redact_secret(conditionMessage(e), api_key)))
    return(NA_character_)
  })
}

# Resolves tickers for ISINs in `df` that aren't yet in the ticker list at
# `ticker_path` (via EODHD's ISIN search API), derives country codes from
# the ISIN prefix, upserts the result into the ticker list on disk, and
# returns the updated ticker list (deduplicated, all required columns
# present, any extra columns preserved) for the caller to left_join against `df` by ISIN.
resolve_and_upsert_tickers <- function(df, ticker_path, api_key = Sys.getenv("EODHD_API_KEY")) {
  required_cols <- c("ISIN", "ticker", "alpha2", "alpha3", "countryname",
                      "companyname", "asset_class", "sector", "industry", "last_updated")

  tickerliste <- if (file.exists(ticker_path)) {
    read_excel(ticker_path) %>% as_tibble()
  } else {
    tibble(ISIN = character(), ticker = character())
  }

  isin_offen <- df %>%
    filter(!ISIN %in% tickerliste$ISIN)

  # Fail fast, before the rate-limited (1s/ISIN) resolution loop starts,
  # rather than silently returning NA for every single ISIN and only
  # surfacing as the "unresolved" warning below - same convention as
  # get_price_data.R's api_key check. Skipped when there's nothing new to
  # resolve, so a fully-cached run doesn't require a key at all.
  if (nrow(isin_offen) > 0 && identical(api_key, "")) {
    stop("EODHD_API_KEY is not set, but ", nrow(isin_offen),
         " new ISIN(s) need ticker resolution via EODHD's search API. ",
         "Set EODHD_API_KEY (e.g. in .Renviron) before running this script.")
  }

  ticker_neu <- isin_offen %>%
    mutate(
      ticker = purrr::map_chr(ISIN, get_ticker_data_from_isin, api_key = api_key),
      isin_prefix = substr(ISIN, 1, 2),
      alpha2 = countrycode(isin_prefix, "iso2c", "iso2c"),
      alpha3 = countrycode(isin_prefix, "iso2c", "iso3c"),
      countryname = countrycode(isin_prefix, "iso2c", "country.name")
    ) %>%
    # only the master-list columns: the holdings input's own columns (Name,
    # Gewichtung, ...) must not leak into the ticker list
    select(any_of(required_cols)) %>%
    filter(!is.na(ticker))

  unresolved <- setdiff(isin_offen$ISIN, ticker_neu$ISIN)
  if (length(unresolved) > 0) {
    unresolved_msg <- paste0(length(unresolved), " of ", nrow(isin_offen), " new ISIN(s) could not be resolved ",
                              "to a ticker via EODHD's search API and were dropped: ",
                              paste(unresolved, collapse = ", "))
    warning(unresolved_msg, call. = FALSE)
    log_pipeline_issue(unresolved_msg)
  }

  tickerliste <- bind_rows(tickerliste, ticker_neu)
  # remove duplicate ISINs (dedupe NA-ISIN rows separately by ticker,
  # since distinct() would otherwise treat all NA ISINs as one group)
  tickerliste <- bind_rows(
    tickerliste %>% filter(!is.na(ISIN)) %>% distinct(ISIN, .keep_all = TRUE),
    tickerliste %>% filter(is.na(ISIN)) %>% distinct(ticker, .keep_all = TRUE)
  )

  missing_cols <- setdiff(required_cols, colnames(tickerliste))
  if (length(missing_cols) > 0) {
    tickerliste[missing_cols] <- NA_character_
  }

  # Required columns first, but keep every other column (e.g. sector_orig,
  # written by tickerliste_maintenance.R phase 4): dropping them here used to
  # destroy the pre-harmonization sector label on every cleaner run.
  tickerliste <- tickerliste %>%
    select(all_of(required_cols), everything())

  write_xlsx(tickerliste, ticker_path)

  tickerliste
}

# tickerliste.xlsx's last_updated column has accumulated mixed
# representations across past runs (raw Excel serial-date numbers stored as
# text, ISO date strings, genuine Date/numeric values from read_excel), which
# makes read_excel() guess `character` for the whole column. That silently
# breaks any later bind_rows() against a real Date column (e.g. new rows
# stamped with last_updated = Sys.Date()). Call this right after reading
# tickerliste to normalize it back to a clean Date vector before use.
normalize_last_updated <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))

  x_chr <- as.character(x)
  # ISO date, optionally with a trailing " HH:MM:SS" time-of-day - readxl
  # sometimes renders a genuine Date-typed cell back as this full timestamp
  # text on re-read, rather than as a native Date/POSIXct value.
  is_iso    <- !is.na(x_chr) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x_chr)
  is_serial <- !is.na(x_chr) & !is_iso & grepl("^[0-9]+(\\.[0-9]+)?$", x_chr)

  out <- as.Date(rep(NA_real_, length(x_chr)))
  out[is_iso]    <- as.Date(substr(x_chr[is_iso], 1, 10))
  out[is_serial] <- as.Date(as.numeric(x_chr[is_serial]), origin = "1899-12-30")

  unparsed <- !is.na(x_chr) & !is_serial & !is_iso
  if (any(unparsed)) {
    warning(sum(unparsed), " last_updated value(s) could not be parsed and were set to NA: ",
            paste(unique(x_chr[unparsed]), collapse = ", "), call. = FALSE)
  }

  out
}

# Adds any tickers present in `clean_df` but missing from the ticker list at
# `ticker_path` - keyed on ticker rather than ISIN, for funds (iShares live
# fetches, Vanguard) whose cleaned holdings carry no per-holding ISIN, so
# resolve_and_upsert_tickers()'s ISIN-based upsert doesn't apply. New rows get
# sector = clean_df[[sector_col]] (the fund's own raw sector label - not yet
# GICS-harmonized, same as resolve_and_upsert_tickers()'s new entries) and
# last_updated = Sys.Date(); existing rows are left untouched. Mirrors
# resolve_and_upsert_tickers()'s upsert/dedup/write-back shape.
upsert_tickerliste_by_ticker <- function(clean_df, ticker_path, sector_col) {
  tickerliste <- read_excel(ticker_path) %>%
    as_tibble() %>%
    mutate(last_updated = normalize_last_updated(last_updated))

  new_entries <- clean_df %>%
    filter(!ticker %in% tickerliste$ticker) %>%
    mutate(
      ISIN = NA_character_,
      companyname = name,
      asset_class = "Equity",
      sector = .data[[sector_col]],
      industry = NA_character_,
      last_updated = Sys.Date()
    ) %>%
    select(any_of(names(tickerliste)))

  if (nrow(new_entries) > 0) {
    tickerliste_updated <- bind_rows(tickerliste, new_entries) %>%
      distinct(ticker, .keep_all = TRUE)
    write_xlsx(tickerliste_updated, ticker_path)
    message(paste("Successfully added", nrow(new_entries), "new tickers to", ticker_path))
  } else {
    message("No new tickers to add.")
  }
}

# Finds the sheet and header row automatically (header row = the row
# containing both "ISIN" and `weight_col` - guards against a lone "ISIN"
# cell in a metadata block above the table), drops unlabeled columns, and
# drops footer/blank rows below the table via an ISIN-format check. Makes
# raw exports whose sheet name changes with every download (e.g. a date
# string) safe to read without updating the script, and also makes manual
# header/footer/leading-column cleanup of the raw file unnecessary.
read_holdings_table_by_isin <- function(path, weight_col = "Gewichtung") {
  for (sheet in readxl::excel_sheets(path)) {
    raw_sheet <- read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text")

    header_row <- which(apply(raw_sheet, 1, function(r) all(c("ISIN", weight_col) %in% trimws(r))))[1]
    if (is.na(header_row)) next

    header <- trimws(as.character(unlist(raw_sheet[header_row, ])))
    valid_cols <- header != "" & !is.na(header)

    return(
      raw_sheet[(header_row + 1):nrow(raw_sheet), valid_cols, drop = FALSE] %>%
        setNames(header[valid_cols]) %>%
        filter(str_detect(ISIN, "^[A-Z]{2}[A-Z0-9]{9}[0-9]$"))
    )
  }

  stop("Error: no sheet with an 'ISIN'/'", weight_col, "' header row found! Please check the raw file.")
}

# Normalizes a raw weight column to a percentage: converts the German-locale
# comma decimal ("4,4401") to a dot, then scales a 0-1 fraction (some raw
# exports report weight as a fraction rather than a percentage) up to a
# percentage. Values already >= 1 are assumed to already be percentages and
# are left as-is.
parse_weight_pct <- function(x) {
  x <- as.double(sub(",", ".", x, fixed = TRUE))
  x <- if_else(x > 0 & x < 1, x * 100, x)
  round(x, 2)
}

# Prints the standard "Data quality report" block (holdings/weight/country
# counts, unmapped-country note, top-10 table) that every per-fund cleaner
# script runs after writing its cleaned data/clean/<Fund>.xlsx. `sector_col`
# is the fund-specific raw sector column name (e.g. "sector_german" for the
# iShares funds, "sector_orig" for VanguardFTSE) shown in the top-10 table.
print_fund_data_quality <- function(clean_df, fund_name, sector_col) {
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat(fund_name, "Data Summary\n")
  cat(paste(rep("=", 50), collapse = ""), "\n\n")

  cat("Total holdings:", nrow(clean_df), "\n")
  cat("Total weight:", sum(clean_df$weight, na.rm = TRUE), "%\n")
  cat("Unique countries:", n_distinct(clean_df$countryname, na.rm = TRUE), "\n\n")

  unmapped <- clean_df %>% filter(is.na(alpha2))
  if (nrow(unmapped) > 0) {
    cat("Note: The following locations have no country codes (intentional):\n")
    print(unmapped)
    cat("\nThese are typically regional classifications or special categories.\n\n")
  }

  cat("Top 10 Holdings:\n")
  print(
    clean_df %>%
      arrange(desc(weight)) %>%
      select(name, weight, countryname, all_of(sector_col)) %>%
      head(10)
  )
}
