## compute_currency_exposure() (scripts/portfolio_returns.R) — pure.

currency_map <- tibble::tibble(
  ticker = c("AAPL", "SAP.XETRA", "CASH_EUR", "GLD"),
  currency = c("USD", "EUR", "EUR", "USD")
)

test_that("a mapped ticker uses its listing currency, not its country's", {
  pf <- tibble::tibble(ticker = "AAPL", alpha3 = "IRL", weight = 5)  # US listing of an Irish-domiciled firm

  res <- compute_currency_exposure(pf, currency_map)

  expect_equal(res$by_holding$currency, "USD")
  expect_equal(res$by_holding$currency_basis, "listing")
})

test_that("an unmapped ticker falls back to its country's currency and is flagged as an estimate", {
  pf <- tibble::tibble(ticker = c("NESN", "DSV", "XYZ"), alpha3 = c("CHE", "DNK", "DEU"),
                       weight = c(3, 2, 1))

  res <- compute_currency_exposure(pf, currency_map)

  expect_equal(res$by_holding$currency, c("CHF", "DKK", "EUR"))
  expect_true(all(res$by_holding$currency_basis == "country"))
})

test_that("a holding with neither a map entry nor a real country is Unassigned", {
  pf <- tibble::tibble(ticker = "MYSTERY", alpha3 = "GOLD", weight = 4)

  res <- compute_currency_exposure(pf, currency_map)

  expect_equal(res$by_holding$currency, "Unassigned")
  expect_equal(res$by_holding$currency_basis, "none")
})

test_that("exposure sums weights per currency, heaviest first, and conserves total weight", {
  pf <- tibble::tibble(
    ticker = c("AAPL", "GLD", "SAP.XETRA", "CASH_EUR", "NESN"),
    alpha3 = c("USA", "GOLD", "DEU", "DEU", "CHE"),
    weight = c(10, 20, 15, 5, 2)
  )

  res <- compute_currency_exposure(pf, currency_map)

  expect_equal(res$exposure$currency, c("USD", "EUR", "CHF"))
  expect_equal(res$exposure$weight, c(30, 20, 2))
  expect_equal(res$exposure$holdings, c(2, 2, 1))
  expect_equal(sum(res$exposure$weight), sum(pf$weight))
})

test_that("duplicate map rows for a ticker don't fan out holdings", {
  dup_map <- dplyr::bind_rows(currency_map, currency_map[1, ])
  pf <- tibble::tibble(ticker = "AAPL", alpha3 = "USA", weight = 7)

  res <- compute_currency_exposure(pf, dup_map)

  expect_equal(nrow(res$by_holding), 1)
  expect_equal(res$exposure$weight, 7)
})
