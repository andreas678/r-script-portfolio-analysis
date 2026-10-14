## pick_listing_from_search() (scripts/utils/data_loaders.R) — pure, no EODHD
## calls. Fixtures mirror the shape of EODHD's /search/<ISIN> response
## (one row per listing, primary/ranked first).
source(here::here("scripts", "utils", "data_loaders.R"))

listings <- function(...) {
  rows <- list(...)
  data.frame(
    Code     = vapply(rows, `[[`, "", 1),
    Exchange = vapply(rows, `[[`, "", 2),
    ISIN     = vapply(rows, `[[`, "", 3),
    stringsAsFactors = FALSE
  )
}

test_that("a non-US ISIN resolves to its home-market listing with an exchange suffix", {
  hits <- listings(c("LRLCF", "US", "FR0000120321"), c("LOR", "XETRA", "FR0000120321"),
                   c("OR", "PA", "FR0000120321"))

  expect_equal(pick_listing_from_search(hits, "FR0000120321"), "OR.PA")
})

test_that("a US ISIN resolves to the bare code", {
  hits <- listings(c("AAPL", "US", "US0378331005"), c("APC", "F", "US0378331005"))

  expect_equal(pick_listing_from_search(hits, "US0378331005"), "AAPL")
})

test_that("the first hit is used when the ISIN's country has no home-market entry", {
  hits <- listings(c("NU", "US", "KYG6683N1034"), c("M1Z", "F", "KYG6683N1034"))

  expect_equal(pick_listing_from_search(hits, "KYG6683N1034"), "NU")
})

test_that("a US-only listing of a non-US ISIN stays bare, and a non-US first hit gets its suffix", {
  expect_equal(
    pick_listing_from_search(listings(c("FLUT", "US", "IE00BWT6H894"), c("PPB", "F", "IE00BWT6H894")),
                             "IE00BWT6H894"),
    "FLUT"
  )
  expect_equal(
    pick_listing_from_search(listings(c("PPB", "F", "IE00BWT6H894")), "IE00BWT6H894"),
    "PPB.F"
  )
})

test_that("Korean ISINs prefer KO over KQ", {
  hits <- listings(c("035420", "KQ", "KR7035420009"), c("035420", "KO", "KR7035420009"))

  expect_equal(pick_listing_from_search(hits, "KR7035420009"), "035420.KO")
})

test_that("hits for a different ISIN are ignored when the searched ISIN is present", {
  hits <- listings(c("ZZZ", "PA", "FR0000000001"), c("OR", "PA", "FR0000120321"))

  expect_equal(pick_listing_from_search(hits, "FR0000120321"), "OR.PA")
})

test_that("a response without an Exchange column falls back to the bare code", {
  hits <- data.frame(Code = "AAPL", stringsAsFactors = FALSE)

  expect_equal(pick_listing_from_search(hits, "US0378331005"), "AAPL")
})

test_that("no usable hit yields NA", {
  expect_true(is.na(pick_listing_from_search(NULL, "FR0000120321")))
  expect_true(is.na(pick_listing_from_search(list(), "FR0000120321")))
  expect_true(is.na(pick_listing_from_search(data.frame(), "FR0000120321")))
  expect_true(is.na(pick_listing_from_search(data.frame(Code = NA_character_), "FR0000120321")))
  expect_true(is.na(pick_listing_from_search(data.frame(Name = "x"), "FR0000120321")))
})
