## config/*.csv — tracked definitional config; guards against hand-edit slips.
read_config <- function(name) {
  path <- here::here("config", name)
  skip_if_not(file.exists(path), paste(name, "not present"))
  readr::read_csv(path, col_types = readr::cols(.default = "c"), na = character())
}

test_that("scenarios.csv has no padded fields and only known portfolio ids", {
  sc  <- read_config("scenarios.csv")
  cfg <- read_config("portfolio_configs.csv")

  expect_true(all(sc$portfolio == trimws(sc$portfolio)))
  expect_true(all(sc$scenario == trimws(sc$scenario)))
  expect_true(all(sc$portfolio %in% cfg$portfolio))
})

test_that("portfolio_configs.csv is comma-delimited with the columns the loader needs", {
  cfg <- read_config("portfolio_configs.csv")

  expect_true(all(c("portfolio", "display_name", "output_file", "fund_file", "weight", "label") %in% names(cfg)))
})
