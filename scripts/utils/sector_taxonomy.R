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

# Non-GICS pseudo-sectors used for cash/bond pseudo-holdings (see
# scripts/CashAndBonds.R) — not part of the GICS standard, but part of
# the same harmonization/join target space as GICS_SECTORS.
PSEUDO_SECTORS <- c(cash = "Cash & Equivalents", bonds = "Fixed Income")

# Synthetic tickers for the Cash / Low Risk Bonds pseudo-holdings (see
# scripts/CashAndBonds.R). Referenced by name (not by string literal) at
# every consumer so the two stay in sync if ever renamed.
CASH_TICKER <- "CASH_EUR"
BONDS_LR_TICKER <- "BONDS_LR_EUR"
