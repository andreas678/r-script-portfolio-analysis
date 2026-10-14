## Exchange-suffix proposals for bare tickers
##
## A fund export without an exchange column (e.g. Amundi) leaves local tickers
## bare ("HSBA", "NOVN"), and EODHD resolves a bare ticker on its US exchange.
## suggest_exchange_suffixes() proposes the home-market symbol from the
## holding's COUNTRY alone - no API call. It only covers countries whose home
## exchange is unambiguous; everything else is left for a manual rule.
##
## Evidence is checked offline against symbols already known to the repo
## (price cache, currency map, ticker master list): a proposal whose symbol is
## already known is "confirmed", otherwise "unverified" (the symbol may not
## exist on EODHD - check before relying on it).
##
## proposals_to_rules() turns proposals into config/ticker_disambiguation.csv
## rows; nothing here writes a file.

library(tidyverse)
library(here)

source(here::here("scripts", "utils", "ticker_disambiguation.R"))

# alpha3 -> EODHD exchange suffix and the currency that listing trades in.
# ITA is absent because EODHD has no Borsa Italiana (no `MI` exchange: its
# exchanges-list has 70 entries, none Italian; `UCG.MI` returns 404). Italian
# shares are only priced via London international-order-book symbols, e.g.
# `0RLS.LSE` (EUR) - see the `0RLS` rules in ticker_disambiguation.csv.
# Left out on purpose: JEY, BMU, LUX, CYP and other domiciles whose shares are
# listed on varying exchanges; USA/CAN/etc. need no suffix or have several.
COUNTRY_EXCHANGE <- tribble(
  ~alpha3, ~suffix, ~currency,
  "GBR",   "LSE",   "GBP",
  "CHE",   "SW",    "CHF",
  "DEU",   "XETRA", "EUR",
  "NLD",   "AS",    "EUR",
  "FRA",   "PA",    "EUR",
  "ESP",   "MC",    "EUR",
  "SWE",   "ST",    "SEK",
  "DNK",   "CO",    "DKK",
  "FIN",   "HE",    "EUR",
  "NOR",   "OL",    "NOK",
  "BEL",   "BR",    "EUR",
  "AUT",   "VI",    "EUR",
  "IRL",   "IR",    "EUR",
  "PRT",   "LS",    "EUR",
  "GRC",   "AT",    "EUR"
)

# ============================================================================
# FUNCTION: suggest_exchange_suffixes
# ============================================================================
# Args:
#   df: holdings with `ticker`, `name`, `alpha3`, `weight`
#   rules: tibble from load_ticker_disambiguation(); tickers it already lists
#     are not proposed again
#   known_symbols: character vector of symbols known to exist (evidence)
#   symbol_currency: optional named character vector (symbol -> currency),
#     used to report what currency the BARE ticker currently resolves to
#   country_map: tibble(alpha3, suffix, currency)
#
# Returns: tibble(ticker, alpha3, name, n_names, weight, suggested,
#   expected_currency, bare_currency, status), largest weight first. `status`:
#   "confirmed" (suggested symbol known), "unverified", "ambiguous" (the
#   same bare ticker + country stands for several company names - needs a
#   name-specific rule) or "odd_code" (a leading-zero code like "0RLS", an
#   LSE international-order-book code: it is already a symbol on EODHD's
#   LSE list as `<code>.LSE`, not a local ticker, so a country suffix would
#   be wrong). The last two are not proposed automatically.

suggest_exchange_suffixes <- function(df, rules = NULL, known_symbols = character(),
                                      symbol_currency = NULL, country_map = COUNTRY_EXCHANGE) {
  empty <- tibble(ticker = character(), alpha3 = character(), name = character(),
                  n_names = integer(), weight = numeric(), suggested = character(),
                  expected_currency = character(), bare_currency = character(),
                  status = character())
  if (nrow(df) == 0) return(empty)

  # A ticker that already has a rule is a human decision (even "keep bare",
  # e.g. SAP as the US ADR): never override or double-map it.
  if (!is.null(rules) && nrow(rules) > 0) {
    df <- filter(df, !ticker %in% rules$ticker)
  }

  bare <- df |>
    filter(!is.na(ticker), ticker != "-", !grepl(".", ticker, fixed = TRUE),
           !is.na(alpha3), alpha3 %in% country_map$alpha3, !is.na(weight)) |>
    arrange(desc(weight))
  if (nrow(bare) == 0) return(empty)

  bare |>
    group_by(ticker, alpha3) |>
    summarise(n_names = n_distinct(toupper(name)), name = first(name),
              weight = sum(weight), .groups = "drop") |>
    left_join(country_map, by = "alpha3") |>
    mutate(
      suggested = paste0(ticker, ".", suffix),
      expected_currency = currency,
      bare_currency = if (is.null(symbol_currency)) NA_character_
                      else unname(symbol_currency[ticker]),
      status = case_when(
        n_names > 1                  ~ "ambiguous",
        grepl("^0[A-Z0-9]{2,4}$", ticker) ~ "odd_code",
        suggested %in% known_symbols ~ "confirmed",
        TRUE                         ~ "unverified"
      )
    ) |>
    select(ticker, alpha3, name, n_names, weight, suggested, expected_currency,
           bare_currency, status) |>
    arrange(desc(weight))
}

# ============================================================================
# FUNCTION: proposals_to_rules
# ============================================================================
# Builds ticker_disambiguation.csv rows from proposals (statuses in `status`).
# A rule applies to rows with that ticker AND country, so a same-named ticker
# of another country (typically a real US company) is untouched. Because
# apply_ticker_disambiguation() warns about every row whose ticker is in the
# rules but matches none, a "keep" row (NA new_ticker) is added for each other
# country the bare ticker occurs in within `master` (any data frame with
# `ticker` and `alpha3`, e.g. the stacked cleaned fund files).

proposals_to_rules <- function(proposals, master = NULL, status = c("confirmed", "unverified")) {
  p <- proposals |> filter(status %in% !!status)
  cols <- tibble(ticker = character(), name_pattern = character(), alpha3 = character(),
                 new_ticker = character(), expected_currency = character(), note = character())
  if (nrow(p) == 0) return(cols)

  main <- p |>
    transmute(ticker, name_pattern = ".", alpha3, new_ticker = suggested,
              expected_currency,
              note = paste0(name, " (auto: ", alpha3, " home exchange, ", status, ")"))

  keep <- cols
  if (!is.null(master) && nrow(master) > 0) {
    keep <- master |>
      filter(ticker %in% p$ticker, !is.na(alpha3)) |>
      distinct(ticker, alpha3) |>
      anti_join(p, by = c("ticker", "alpha3")) |>
      transmute(ticker, name_pattern = ".", alpha3, new_ticker = NA_character_,
                expected_currency = NA_character_,
                note = "auto: keep, same bare ticker as a home-exchange rule")
  }

  bind_rows(main, keep)
}

# ============================================================================
# FUNCTION: append_ticker_disambiguation_rules
# ============================================================================
# Appends rules not yet present (same ticker + alpha3 + name_pattern) to the
# CSV; returns the number added. Existing rows are never changed.

append_ticker_disambiguation_rules <- function(new_rules, path) {
  existing <- read.csv(path, stringsAsFactors = FALSE, na.strings = "") |>
    as_tibble() |> mutate(across(everything(), as.character))
  key <- function(d) paste(d$ticker, d$alpha3, d$name_pattern, sep = "|")
  add <- new_rules |>
    mutate(across(everything(), as.character)) |>
    filter(!key(new_rules) %in% key(existing))
  if (nrow(add) == 0) return(0L)
  write.csv(bind_rows(existing, add), path, row.names = FALSE, na = "")
  nrow(add)
}
