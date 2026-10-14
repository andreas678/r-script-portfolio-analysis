## Ticker Disambiguation
##
## The fund holdings files key every position by a bare `ticker`, and EODHD
## resolves a suffix-less ticker on its US exchange. For non-US companies that
## silently selects a different company's price series (e.g. bare "AI" is
## C3.ai, not Air Liquide), and one bare ticker can stand for several real
## companies at once (e.g. "SAN" is both Banco Santander and Sanofi).
##
## config/ticker_disambiguation.csv maps (ticker, company-name pattern) to the
## correct EODHD symbol; apply_ticker_disambiguation() rewrites a holdings
## data frame accordingly. Applied to each fund file as it is loaded
## (portfolio_report.Rmd's load_etf()), so the cleaned files under
## data/clean/ don't need regenerating when a rule changes.
##
## Pure/side-effect-free apart from a warning on ambiguous input: safe to
## source() from anywhere.

library(tidyverse)
library(here)

TICKER_DISAMBIGUATION_FILE <- "ticker_disambiguation.csv"

# ============================================================================
# FUNCTION: load_ticker_disambiguation
# ============================================================================
# Reads config/ticker_disambiguation.csv.
#
# Returns:
#   tibble(ticker, name_pattern, alpha3, new_ticker, expected_currency, note).
#   Blank cells are NA. A row with NA new_ticker marks a known case that is
#   deliberately left unmapped (kept only so it isn't reported as unmatched).

load_ticker_disambiguation <- function(path = here::here("config", TICKER_DISAMBIGUATION_FILE)) {
  if (!file.exists(path)) {
    return(tibble(ticker = character(), name_pattern = character(), alpha3 = character(),
                  new_ticker = character(), expected_currency = character(), note = character()))
  }
  read.csv(path, stringsAsFactors = FALSE, na.strings = "") |>
    as_tibble() |>
    mutate(across(everything(), as.character))
}

# ============================================================================
# FUNCTION: apply_ticker_disambiguation
# ============================================================================
# Rewrites `ticker` in a holdings data frame per the rules.
#
# A rule fires for a row when row$ticker == rule$ticker, the row's name matches
# rule$name_pattern (case-insensitive regex), and — if the rule sets alpha3 —
# the row's alpha3 equals it. Every rule is matched against the row's ORIGINAL
# ticker, so rules never chain.
#
# Rows whose ticker appears in the rules but that match no rule are left
# unchanged and reported in one warning: a new, unseen company sharing a known
# colliding ticker is exactly the case this table can't have anticipated.
# Rows matching several rules use the first and are reported too.
#
# Args:
#   df: data frame with `ticker` and `name` columns (alpha3 optional)
#   rules: tibble from load_ticker_disambiguation()
#   name_col: name of the company-name column (default "name")
#
# Returns: df with `ticker` rewritten.

apply_ticker_disambiguation <- function(df, rules = load_ticker_disambiguation(),
                                        name_col = "name") {
  if (nrow(df) == 0 || nrow(rules) == 0) return(df)

  nm      <- toupper(as.character(df[[name_col]]))
  nm[is.na(nm)] <- ""
  tk      <- as.character(df$ticker)
  a3      <- if ("alpha3" %in% names(df)) as.character(df$alpha3) else rep(NA_character_, nrow(df))
  new_tk  <- tk
  n_match <- integer(nrow(df))

  for (i in seq_len(nrow(rules))) {
    r <- rules[i, ]
    hit <- !is.na(tk) & tk == r$ticker &
      grepl(r$name_pattern, nm, ignore.case = TRUE, perl = TRUE) &
      (is.na(r$alpha3) | (!is.na(a3) & a3 == r$alpha3))
    n_match[hit] <- n_match[hit] + 1L
    if (!is.na(r$new_ticker)) {
      first_hit <- hit & n_match == 1L
      new_tk[first_hit] <- r$new_ticker
    }
  }

  unmatched <- !is.na(tk) & tk %in% rules$ticker & n_match == 0L
  multi     <- n_match > 1L
  if (any(unmatched | multi)) {
    problem <- which(unmatched | multi)
    msg <- paste0(
      "ticker disambiguation: ", length(problem), " row(s) unmatched or matching several rules ",
      "(ticker left as-is / first rule used): ",
      paste(unique(paste0(tk[problem], " [", substr(nm[problem], 1, 25), "]")), collapse = "; "),
      ". Add or adjust a rule in config/", TICKER_DISAMBIGUATION_FILE, "."
    )
    warning(msg, call. = FALSE)
  }

  df$ticker <- new_tk
  df
}
