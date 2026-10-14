## Canonical Sector Taxonomy & Cash/Bonds Pseudo-Holding Constants
##
## Single source of truth for the 11 canonical GICS sector names used
## across the pipeline: tickerliste_maintenance.R's Phase 4 harmonizes
## every raw sector label onto this set (gics_sector_map), and
## portfolio_report.Rmd's tornado-chart-analysis chunk joins on it
## (volatility_estimates). Keeping both consumers pointed at the same
## constant instead of independently-typed literals prevents them from
## drifting out of sync, which would silently break the sector left_join
## and fall through to the 22% default volatility.
##
## Also centralizes the CASH_EUR / BONDS_LR_EUR pseudo-tickers: previously
## hardcoded independently in scripts/CashAndBonds.R (where they're created)
## and portfolio_report.Rmd's manual_volatility_overrides (which matches on
## them by literal string) - a rename in one place would have silently
## broken the other.
##
## Pure/side-effect-free: safe to source() from anywhere.

GICS_SECTORS <- c(
  "Information Technology", "Health Care", "Financials", "Industrials",
  "Consumer Discretionary", "Communication Services", "Consumer Staples",
  "Energy", "Utilities", "Real Estate", "Materials"
)

# Non-GICS pseudo-sectors — not part of the GICS standard, but part of the
# same harmonization/join target space as GICS_SECTORS. cash/bonds back the
# CashAndBonds.R pseudo-holdings; crypto/commodities cover real tickers in
# meta/tickerliste.xlsx (BTC/ETH/SOL/XRP, GLD) whose asset class has no GICS
# sector at all — added 2026-09-27 after the sector-taxonomy test (§3.1)
# caught them as unmapped raw labels falling through gics_sector_map below.
PSEUDO_SECTORS <- c(
  cash = "Cash & Equivalents", bonds = "Fixed Income",
  crypto = "Digital Assets", commodities = "Commodities"
)

# Fallback annualized volatility (%) per sector, used by the report's risk table
# when a holding has no price history. NAMED, so a reordering of GICS_SECTORS
# can't silently shift values onto the wrong sector (it was positional before).
# Cash / Fixed Income are NOT here: they have manual overrides in the report.
# "Digital Assets" and "Commodities" were missing, so a crypto or gold holding
# without history fell through to the generic 22 % default; the values below are
# rough long-run assumptions (crypto ~70, gold-like commodities ~16), not data.
SECTOR_VOL_ESTIMATES <- c(
  "Information Technology" = 32, "Health Care" = 15, "Financials" = 20,
  "Industrials" = 18, "Consumer Discretionary" = 22, "Communication Services" = 20,
  "Consumer Staples" = 14, "Energy" = 28, "Utilities" = 14, "Real Estate" = 16,
  "Materials" = 22,
  "Digital Assets" = 70, "Commodities" = 16,
  "Unclassified" = 22
)

# Synthetic tickers for the Cash / Low Risk Bonds pseudo-holdings (see
# scripts/CashAndBonds.R). Referenced by name (not by string literal) at
# every consumer so the two stay in sync if ever renamed.
CASH_TICKER <- "CASH_EUR"
BONDS_LR_TICKER <- "BONDS_LR_EUR"

# Asset class of a portfolio row. Gold and crypto carry the pseudo-country codes GOLD / CRYPTO
# (single_tickers.R), cash and bonds are the synthetic tickers above; everything else is an equity.
# Countries, sectors and equity concentration are only meaningful for the "Equities" class:
# gold has no country, and the cash / bond rows carry a country (DEU) only because every row needs one.
ASSET_CLASSES <- c("Equities", "Gold", "Crypto", "Low Risk Bonds", "Cash")

asset_class_of <- function(alpha3, ticker) {
  dplyr::case_when(
    ticker == CASH_TICKER      ~ "Cash",
    ticker == BONDS_LR_TICKER  ~ "Low Risk Bonds",
    alpha3 %in% "GOLD"         ~ "Gold",
    alpha3 %in% "CRYPTO"       ~ "Crypto",
    TRUE                       ~ "Equities"
  )
}

## Raw-label -> canonical-GICS-sector harmonization map.
##
## Moved here (2026-09-27, from tickerliste_maintenance.R's Phase 4) so it's
## a plain, side-effect-free data object testable on its own — the script it
## used to live in does live Finnhub/EODHD API calls and file I/O before
## Phase 4 ever runs, which made the map itself untestable without those.
## tickerliste_maintenance.R now sources this file and uses gics_sector_map
## directly; see its Phase 4 comment for how the map is applied (coalesce()
## fallback to the existing value for anything not listed here, logged as
## "unmapped" rather than silently dropped).
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
  "Telecommunications"                = "Communication Services",
  "Textiles, Apparel & Luxury Goods"  = "Consumer Discretionary",
  "Automobiles"                       = "Consumer Discretionary",
  "Logistics & Transportation"        = "Industrials",
  "Metals & Mining"                   = "Materials",
  "Basic Materials"                   = "Materials",
  "Airlines"                          = "Industrials",
  "Distributors"                      = "Consumer Discretionary",
  "Diversified Consumer Services"     = "Consumer Discretionary",
  "Industrial Conglomerates"          = "Industrials",
  "Marine"                            = "Industrials",

  # Cash / fixed-income pseudo-holdings - normalized, not GICS-mapped
  "Cash und/oder Derivate" = "Cash & Equivalents",
  "Cash & Equivalents"     = "Cash & Equivalents",
  "Fixed Income"           = "Fixed Income",

  # Crypto / commodity pseudo-holdings - also not GICS-mapped (identity
  # mappings; see PSEUDO_SECTORS above)
  "Digital Assets"         = "Digital Assets",
  "Commodities"            = "Commodities"
)

# Guards against a typo'd target value in gics_sector_map above silently
# introducing a sector name that portfolio_report.Rmd's volatility_estimates
# join (keyed on GICS_SECTORS) doesn't recognize.
stopifnot(all(gics_sector_map %in% c(GICS_SECTORS, PSEUDO_SECTORS)))

# Heuristic asset class from the ticker string alone (the original phase 2 of
# tickerliste_maintenance.R). Only a fallback: it cannot know about crypto,
# gold, cash or bonds, which are curated by hand (CashAndBonds.R, manual edits).
heuristic_asset_class <- function(ticker) {
  dplyr::case_when(
    is.na(ticker) ~ "Unknown",
    stringr::str_detect(ticker, "(?i)etf|exchange") ~ "ETF",
    stringr::str_detect(ticker, "(?i)fund") ~ "Fund",
    nchar(ticker) <= 5 & !stringr::str_detect(ticker, "\\.") ~ "Equity",
    TRUE ~ "Other"
  )
}

# Fills asset_class ONLY where it is empty and stamps last_updated only on the
# rows it changed. An existing value is never overwritten: the heuristic used
# to recompute the whole column on every run, which silently turned curated
# classes (Cryptocurrency, Cash, Bond) into Equity/Other and made
# get_crypto_tickers() return character(0) without any warning.
fill_asset_class <- function(tickerliste) {
  if (!"asset_class" %in% names(tickerliste)) tickerliste$asset_class <- NA_character_
  empty <- is.na(tickerliste$asset_class) | trimws(tickerliste$asset_class) == ""
  tickerliste$asset_class[empty] <- heuristic_asset_class(tickerliste$ticker[empty])
  if ("last_updated" %in% names(tickerliste)) tickerliste$last_updated[empty] <- Sys.Date()
  tickerliste
}

# ============================================================================
# FUNCTION: sector_lookup_by_isin
# ============================================================================
# Sector/industry for the tickers the report actually works with, found through the ISIN.
#
# meta/tickerliste.xlsx knows a company under the bare ticker of its first resolution (ASML), but
# the report's tickers are the disambiguated ones (ASML.AS, HSBA.LSE: config/ticker_disambiguation.csv),
# so a join on ticker lost the sector of every suffixed holding. The ISIN does not change with the
# listing, so it is the safe bridge (a bare-ticker fallback would reintroduce collisions: SU is
# Suncor in one list and Schneider in another).
#
# Args:
#   portfolio: holdings with ISIN, ticker, weight (rows without an ISIN are ignored)
#   tickerliste: tibble with ISIN, sector, industry
#
# Returns: tibble(ticker, sector, industry), one row per ticker. A ticker held under several ISINs
# (a collision) takes the ISIN of its heaviest row.
sector_lookup_by_isin <- function(portfolio, tickerliste) {
  by_isin <- tickerliste |>
    dplyr::filter(!is.na(ISIN), !is.na(sector)) |>
    dplyr::distinct(ISIN, .keep_all = TRUE) |>
    dplyr::select(ISIN, sector, industry)

  portfolio |>
    dplyr::filter(!is.na(ISIN), !is.na(ticker)) |>
    dplyr::arrange(dplyr::desc(weight)) |>
    dplyr::distinct(ticker, ISIN, .keep_all = TRUE) |>
    dplyr::inner_join(by_isin, by = "ISIN") |>
    dplyr::distinct(ticker, .keep_all = TRUE) |>
    dplyr::select(ticker, sector, industry)
}

# ============================================================================
# FUNCTION: extend_sector_lookup
# ============================================================================
# `lookup` (ticker, sector, industry, as built from tickerliste) plus the ISIN-derived rows for
# tickers that have no sector in it yet. An existing sector is never overwritten.
extend_sector_lookup <- function(lookup, portfolio, tickerliste) {
  known <- lookup |> dplyr::filter(!is.na(sector))
  extra <- sector_lookup_by_isin(portfolio, tickerliste) |>
    dplyr::filter(!ticker %in% known$ticker)
  dplyr::bind_rows(known, extra)
}
