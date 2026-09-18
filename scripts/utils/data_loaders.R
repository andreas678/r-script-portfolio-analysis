library(tidyverse)
library(readxl)
library(httr)
library(jsonlite)
library(writexl)
library(countrycode)

source(here::here("scripts", "utils", "logging.R"))

# Looks up a ticker for an ISIN via EODHD's search API (rate-limited to 1 req/sec)
get_ticker_data_from_isin <- function(isin, api_key) {
  Sys.sleep(1)
  url <- paste0("https://eodhistoricaldata.com/api/search/", isin, "?api_token=", api_key)

  tryCatch({
    response <- GET(url, timeout(10))
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
      if (length(data) > 0 && !is.null(data$Code)) {
        return(data$Code[1])
      }
    }
    return(NA)
  }, error = function(e) {
    warning(paste("API error for ISIN", isin, ":", e$message))
    return(NA)
  })
}

# Resolves tickers for ISINs in `df` that aren't yet in the ticker list at
# `ticker_path` (via EODHD's ISIN search API), derives country codes from
# the ISIN prefix, upserts the result into the ticker list on disk, and
# returns the updated ticker list (deduplicated, all required columns
# present) for the caller to left_join against `df` by ISIN.
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
    select(-isin_prefix) %>%
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

  tickerliste <- tickerliste %>%
    select(all_of(required_cols))

  write_xlsx(tickerliste, ticker_path)

  tickerliste
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

  stop("Fehler: Kein Tabellenblatt mit einer 'ISIN'/'", weight_col, "'-Kopfzeile gefunden! Bitte Rohdatei pruefen.")
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
