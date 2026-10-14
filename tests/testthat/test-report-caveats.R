## report_caveats() (scripts/utils/report_caveats.R) - pure.

source(here::here("scripts", "utils", "report_caveats.R"))

assumed_blocks <- function() tibble::tibble(label = c("Cash", "Low Risk Bonds"), weight = c(4.6, 10.4), risk_share_pct = c(0, 1.2))

test_that("the always-on caveats are present with the figures filled in", {
  out <- report_caveats(11.6, 214, assumed_blocks(), 54.2)

  expect_true(any(grepl("Short window.*11.6 months \\(214 steps\\)", out)))
  expect_true(any(grepl("Assumed building blocks.*Cash and Low Risk Bonds.*15% of the weight, 1.2% of the risk", out)))
  expect_true(any(grepl("Only 54.2% of the weight", out)))
  expect_true(any(grepl("Currency is the trading currency", out)))
  expect_true(all(startsWith(out, "- ")))
})

test_that("optional caveats appear only when they apply", {
  plain <- report_caveats(11.6, 214, assumed_blocks()[0, ], 54.2)
  expect_false(any(grepl("Assumed building blocks|Not modelled|Proxied|Unclassified", plain)))

  full <- report_caveats(11.6, 214, assumed_blocks(), 54.2, unmodelled_labels = c("Fund X"),
                         proxied_labels = "Amundi Core World", unclassified_equity_pct = 1.2)
  expect_true(any(grepl("Not modelled:\\*\\* Fund X", full)))
  expect_true(any(grepl("Proxied holdings:\\*\\* Amundi Core World", full)))
  expect_true(any(grepl("Unclassified sectors.*1.2% of the equity weight", full)))
})

test_that("a negligible unclassified share is not worth a bullet", {
  out <- report_caveats(11.6, 214, assumed_blocks(), 54.2, unclassified_equity_pct = 0.02)

  expect_false(any(grepl("Unclassified", out)))
})
