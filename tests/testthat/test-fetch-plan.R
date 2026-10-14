## last_business_day() / plan_price_fetches() (scripts/get_price_data.R) — pure.
source(here::here("scripts", "get_price_data.R"))

cached <- function(...) {
  rows <- list(...)
  tibble::tibble(ticker = vapply(rows, `[[`, "", 1), Date = as.Date(vapply(rows, `[[`, "", 2)),
                 Adjusted_close = 1)
}

# 2026-09-25 is a Friday, 2026-09-26 Saturday, 2026-09-27 Sunday, 2026-09-28 Monday
test_that("last_business_day maps the weekend to Friday and leaves weekdays alone", {
  d <- as.Date(c("2026-09-21", "2026-09-22", "2026-09-23", "2026-09-24", "2026-09-25",
                 "2026-09-26", "2026-09-27", "2026-09-28"))

  expect_equal(last_business_day(d), as.Date(c("2026-09-21", "2026-09-22", "2026-09-23", "2026-09-24",
                                               "2026-09-25", "2026-09-25", "2026-09-25", "2026-09-28")))
})

test_that("on a weekend a ticker whose last price is Friday's is NOT re-requested", {
  c0 <- cached(c("AAPL", "2026-09-25"))

  expect_equal(nrow(plan_price_fetches("AAPL", c0, 252, end_date = as.Date("2026-09-26"))), 0)  # Saturday
  expect_equal(nrow(plan_price_fetches("AAPL", c0, 252, end_date = as.Date("2026-09-27"))), 0)  # Sunday
})

test_that("on Monday the same ticker is fetched, starting the day after its last cached date", {
  plan <- plan_price_fetches("AAPL", cached(c("AAPL", "2026-09-25")), 252, end_date = as.Date("2026-09-28"))

  expect_equal(plan$ticker, "AAPL")
  expect_equal(plan$fetch_from, as.Date("2026-09-26"))  # nothing lost: weekend rows are covered by the range
})

test_that("a ticker that is genuinely behind is fetched even on a weekend", {
  plan <- plan_price_fetches("STALE", cached(c("STALE", "2026-09-22")), 252, end_date = as.Date("2026-09-27"))

  expect_equal(plan$ticker, "STALE")
  expect_equal(plan$fetch_from, as.Date("2026-09-23"))
})

test_that("a ticker with no cache is always fetched, with the full lookback", {
  plan <- plan_price_fetches("NEW", cached(c("AAPL", "2026-09-25")), 252, end_date = as.Date("2026-09-27"))

  expect_equal(plan$ticker, "NEW")
  expect_equal(plan$fetch_from, as.Date("2026-09-27") - 252)
})

test_that("during the week today's price is still expected, so yesterday's cache is stale", {
  plan <- plan_price_fetches("AAPL", cached(c("AAPL", "2026-09-23")), 252, end_date = as.Date("2026-09-24"))

  expect_equal(plan$ticker, "AAPL")
  expect_equal(nrow(plan_price_fetches("AAPL", cached(c("AAPL", "2026-09-24")), 252,
                                       end_date = as.Date("2026-09-24"))), 0)
})

test_that("min_staleness_days tolerates a lag behind the latest possible date", {
  c0 <- cached(c("AAPL", "2026-09-22"))

  expect_equal(nrow(plan_price_fetches("AAPL", c0, 252, end_date = as.Date("2026-09-24"), min_staleness_days = 2)), 0)
  expect_equal(nrow(plan_price_fetches("AAPL", c0, 252, end_date = as.Date("2026-09-24"), min_staleness_days = 1)), 1)
})

test_that("duplicate wanted tickers yield one plan row", {
  plan <- plan_price_fetches(c("NEW", "NEW"), cached(c("AAPL", "2026-09-25")), 252, end_date = as.Date("2026-09-24"))

  expect_equal(nrow(plan), 1)
})

# ---- plan_price_rebuilds ----------------------------------------------------------------------
meta_row <- function(ticker, start, success = TRUE) {
  data.frame(ticker = ticker, start_date = start, end_date = "2026-10-02", success = success)
}

test_that("a ticker whose history starts later than the wanted depth is planned for a full rebuild", {
  c0 <- dplyr::bind_rows(cached(c("LATE", "2026-01-23"), c("LATE", "2026-10-01")),
                         cached(c("FULL", "2025-10-03"), c("FULL", "2026-10-01")))

  plan <- plan_price_rebuilds(c("LATE", "FULL"), c0, metadata = NULL, period_days = 365,
                              end_date = as.Date("2026-10-03"))

  expect_equal(plan$ticker, "LATE")
  expect_equal(plan$fetch_from, as.Date("2026-10-03") - 365)     # the whole depth, not just the missing part
  expect_equal(plan$fetch_to, as.Date("2026-10-03"))
  expect_equal(plan$first_date, as.Date("2026-01-23"))
})

test_that("the first trading day a few days after the wanted start still counts as covered", {
  c0 <- cached(c("OK", "2025-10-09"), c("OK", "2026-10-01"))   # wanted start 2025-10-03, 6 days earlier

  expect_equal(nrow(plan_price_rebuilds("OK", c0, NULL, 365, as.Date("2026-10-03"))), 0)
})

test_that("a young listing is not rebuilt again once a fetch from the wanted start already succeeded", {
  c0 <- cached(c("IPO", "2026-06-01"), c("IPO", "2026-10-01"))
  d <- as.Date("2026-10-03")

  expect_equal(nrow(plan_price_rebuilds("IPO", c0, NULL, 365, d)), 1)                                   # never tried
  expect_equal(nrow(plan_price_rebuilds("IPO", c0, meta_row("IPO", "2025-10-03"), 365, d)), 0)          # tried, series is short
  expect_equal(nrow(plan_price_rebuilds("IPO", c0, meta_row("IPO", "2025-10-03", FALSE), 365, d)), 1)   # failed: try again
  expect_equal(nrow(plan_price_rebuilds("IPO", c0, meta_row("IPO", "2026-01-23"), 365, d)), 1)          # shorter earlier fetch
})

test_that("tickers outside the wanted set and tickers without a cache are not in the rebuild plan", {
  c0 <- cached(c("OTHER", "2026-03-01"), c("OTHER", "2026-10-01"))

  expect_equal(nrow(plan_price_rebuilds(c("NEW"), c0, NULL, 365, as.Date("2026-10-03"))), 0)   # NEW: plan_price_fetches' job
})
