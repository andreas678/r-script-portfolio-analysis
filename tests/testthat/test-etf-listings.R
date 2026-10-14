## config/etf_listings.csv + load_etf_listings() / etf_listings_gaps()
## (scripts/utils/portfolio_build.R). Offline.
source(here::here("scripts", "utils", "portfolio_build.R"))

write_listings <- function(path, ...) readr::write_csv(tibble::tibble(...), path)

test_that("load_etf_listings reads the columns and trims padding", {
  p <- withr::local_tempfile(fileext = ".csv")
  write_listings(p, fund_file = "A.xlsx", isin = " IE0000000001", ticker = "ABC.XETRA ",
                 currency = "EUR", price_scale = 1, note = "x")

  out <- load_etf_listings(p)

  expect_equal(out$isin, "IE0000000001")
  expect_equal(out$ticker, "ABC.XETRA")
  expect_equal(out$price_scale, 1)
})

test_that("a missing file yields an empty listing table, not an error", {
  out <- load_etf_listings(file.path(withr::local_tempdir(), "nope.csv"))

  expect_equal(nrow(out), 0)
  expect_true(all(c("ticker", "currency", "price_scale") %in% names(out)))
})

test_that("etf_listings_gaps finds funds without a listing and listings without a fund", {
  cfg <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("portfolio,display_name,output_file,fund_file,weight,label,isin",
               "p,P,p.xlsx,A.xlsx,0.5,A,ISIN_A",
               "p,P,p.xlsx,B.xlsx,0.5,B,ISIN_B",
               "p,P,p.xlsx,Cash.xlsx,,Cash,"), cfg)
  listings <- tibble::tibble(isin = c("ISIN_A", "ISIN_OLD"), ticker = c("A.XETRA", "OLD.XETRA"))

  gaps <- etf_listings_gaps(listings, cfg)

  expect_equal(gaps$missing_listing, "ISIN_B")
  expect_equal(gaps$stale_listing, "ISIN_OLD")
})

test_that("a config without an isin column has no gaps to report", {
  cfg <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("portfolio,display_name,output_file,fund_file,weight,label", "p,P,p.xlsx,A.xlsx,1,A"), cfg)

  gaps <- etf_listings_gaps(tibble::tibble(isin = character()), cfg)

  expect_length(gaps$missing_listing, 0)
})

test_that("the shipped etf_listings.csv covers every ISIN in portfolio_configs.csv exactly once", {
  path <- here::here("config", "etf_listings.csv")
  skip_if_not(file.exists(path))
  listings <- load_etf_listings()

  expect_false(anyDuplicated(listings$ticker) > 0)
  expect_false(anyDuplicated(listings$isin) > 0)
  expect_true(all(listings$currency == "EUR" | nzchar(listings$currency)))
  expect_true(all(is.finite(listings$price_scale) & listings$price_scale > 0))
  # the committed config may lack the isin column; only check when it is there
  cfg_path <- here::here("config", "portfolio_configs.csv")
  if ("isin" %in% names(readr::read_csv(cfg_path, n_max = 1, show_col_types = FALSE))) {
    gaps <- etf_listings_gaps(listings)
    expect_length(gaps$missing_listing, 0)
    expect_length(gaps$stale_listing, 0)
  }
})
