## Tests for scripts/utils/sector_taxonomy.R's gics_sector_map — the
## raw-label -> canonical-GICS-sector harmonization table used by
## tickerliste_maintenance.R's Phase 4 (see DEVELOPMENT-PROPOSAL-2026-09-27.md
## §3.1, item 1). The map itself lives in sector_taxonomy.R specifically so
## it's testable as a plain data object, without running the rest of
## tickerliste_maintenance.R's Finnhub/EODHD API calls and file writes.
##
## The proposal's original ask — assert every label *currently seen in
## meta/tickerliste.xlsx* maps to one of the 11 canonical sectors — can only
## run where that file exists: it's gitignored personal data, never
## committed, so it won't be present in CI or a fresh clone. That check runs
## conditionally below (skipped when the file is absent) as the real
## regression guard for local runs; the two tests above it are static and
## always run, covering the map's own internal validity plus a spot-check of
## known labels so CI still exercises this file on every push.

source(here::here("scripts", "utils", "sector_taxonomy.R"))

test_that("every gics_sector_map value is a canonical GICS or pseudo sector", {
  expect_true(all(gics_sector_map %in% c(GICS_SECTORS, PSEUDO_SECTORS)))
})

test_that("GICS_SECTORS has exactly the 11 canonical sectors, no duplicates", {
  expect_length(GICS_SECTORS, 11)
  expect_length(unique(GICS_SECTORS), 11)
})

test_that("known raw labels map to the expected canonical sector", {
  known <- c(
    "Finanzwesen"                   = "Financials",
    "Zyklische Konsumgüter"         = "Consumer Discretionary",
    "Technology"                    = "Information Technology",
    "Semiconductors"                = "Information Technology",
    "Biotechnology"                 = "Health Care",
    "Cash und/oder Derivate"        = "Cash & Equivalents",
    "Fixed Income"                  = "Fixed Income",
    "Information Technology"        = "Information Technology"  # identity mapping
  )
  expect_equal(unname(gics_sector_map[names(known)]), unname(known))
})

test_that("every sector label in the real tickerliste (if present) is mapped", {
  ticker_path <- here::here("meta", "tickerliste.xlsx")
  skip_if_not(file.exists(ticker_path), "meta/tickerliste.xlsx is gitignored personal data, not present here")

  tickerliste <- readxl::read_excel(ticker_path)
  skip_if_not("sector" %in% names(tickerliste), "tickerliste has no sector column yet (pre-Phase-3)")

  observed <- unique(stringr::str_trim(tickerliste$sector))
  observed <- observed[!is.na(observed)]
  unmapped <- setdiff(observed, names(gics_sector_map))

  expect_equal(unmapped, character(0),
    info = paste("Unmapped sector label(s) found in meta/tickerliste.xlsx:",
                 paste(unmapped, collapse = ", "),
                 "- add them to gics_sector_map in scripts/utils/sector_taxonomy.R"))
})

test_that("fill_asset_class never overwrites a curated class and only stamps rows it filled", {
  old <- as.Date("2026-01-01")
  tl <- tibble::tibble(
    ticker = c("Btc", "AAPL", "VOD.LSE", NA, "SPYETF"),
    asset_class = c("Cryptocurrency", NA, "", "Cash", "Bond"),
    last_updated = old
  )

  out <- fill_asset_class(tl)

  expect_equal(out$asset_class, c("Cryptocurrency", "Equity", "Other", "Cash", "Bond"))
  expect_equal(out$last_updated, c(old, Sys.Date(), Sys.Date(), old, old))
})

test_that("fill_asset_class is idempotent and tolerates a missing asset_class column", {
  tl <- tibble::tibble(ticker = c("AAPL", NA), last_updated = Sys.Date())

  once  <- fill_asset_class(tl)
  twice <- fill_asset_class(once)

  expect_equal(once$asset_class, c("Equity", "Unknown"))
  expect_equal(twice, once)
})

test_that("heuristic_asset_class keeps the original rules", {
  expect_equal(heuristic_asset_class(c("IWDA.ETF", "MYFUND", "AAPL", "VOD.LSE", NA)),
               c("ETF", "Fund", "Equity", "Other", "Unknown"))
})

test_that("SECTOR_VOL_ESTIMATES is named and covers every sector the risk table can see", {
  expect_false(anyDuplicated(names(SECTOR_VOL_ESTIMATES)) > 0)
  expect_true(all(is.finite(SECTOR_VOL_ESTIMATES) & SECTOR_VOL_ESTIMATES > 0))
  # every GICS sector, the crypto/commodity pseudo-sectors, and the no-sector bucket;
  # Cash / Fixed Income are covered by manual overrides in the report instead
  expect_true(all(c(GICS_SECTORS, "Digital Assets", "Commodities", "Unclassified") %in%
                    names(SECTOR_VOL_ESTIMATES)))
  # the values must not have moved when the positional literal became a named vector
  expect_equal(unname(SECTOR_VOL_ESTIMATES[c(GICS_SECTORS, "Unclassified")]),
               c(32, 15, 20, 18, 22, 20, 14, 28, 14, 16, 22, 22))
})

test_that("asset_class_of separates cash, bonds, gold and crypto from equities", {
  expect_equal(
    asset_class_of(c("USA", "GOLD", "CRYPTO", "DEU", "DEU", NA),
                   c("AAPL", "GLD", "BTC", CASH_TICKER, BONDS_LR_TICKER, "XYZ")),
    c("Equities", "Gold", "Crypto", "Cash", "Low Risk Bonds", "Equities"))
  expect_true(all(asset_class_of("USA", "AAPL") %in% ASSET_CLASSES))
})

test_that("sector_lookup_by_isin bridges a suffixed ticker to its sector through the ISIN", {
  tl <- tibble::tibble(ISIN = c("NL0010273215", "CA8672241079", "FR0000121972", "GB0005405286"),
                       ticker = c("ASML", "SU", "SU", "HSBA"),
                       sector = c("Information Technology", NA, "Industrials", NA),
                       industry = c("Semis", NA, "Electrical", NA))
  pf <- tibble::tibble(ISIN = c("NL0010273215", "FR0000121972", "GB0005405286", NA),
                       ticker = c("ASML.AS", "SU", "HSBA.LSE", "NOISIN"),
                       weight = c(0.7, 0.2, 0.3, 0.1))

  out <- sector_lookup_by_isin(pf, tl)

  expect_equal(sort(out$ticker), c("ASML.AS", "SU"))          # HSBA has no sector anywhere; NOISIN has no ISIN
  expect_equal(out$sector[out$ticker == "ASML.AS"], "Information Technology")
  expect_equal(out$sector[out$ticker == "SU"], "Industrials")  # Schneider's ISIN, not Suncor's (sector NA)
})

test_that("a ticker held under two ISINs takes the sector of its heaviest row", {
  tl <- tibble::tibble(ISIN = c("AAA", "BBB"), sector = c("Energy", "Financials"), industry = NA_character_)
  pf <- tibble::tibble(ISIN = c("AAA", "BBB"), ticker = c("X", "X"), weight = c(0.1, 0.5))

  expect_equal(sector_lookup_by_isin(pf, tl)$sector, "Financials")
})

test_that("extend_sector_lookup adds missing sectors and never overwrites an existing one", {
  lookup <- tibble::tibble(ticker = c("AAPL", "SU", "NOSEC"), sector = c("Information Technology", "Energy", NA),
                           industry = NA_character_)
  tl <- tibble::tibble(ISIN = c("US1", "FR1", "XX1"), sector = c("Wrong", "Industrials", "Utilities"),
                       industry = NA_character_)
  pf <- tibble::tibble(ISIN = c("US1", "FR1", "XX1"), ticker = c("AAPL", "SU", "NOSEC"), weight = 1)

  out <- extend_sector_lookup(lookup, pf, tl)

  expect_equal(out$sector[out$ticker == "AAPL"], "Information Technology")   # kept, not "Wrong"
  expect_equal(out$sector[out$ticker == "SU"], "Energy")                      # kept: the lookup already had one
  expect_equal(out$sector[out$ticker == "NOSEC"], "Utilities")                # filled through the ISIN
  expect_false(anyDuplicated(out$ticker) > 0)
})
