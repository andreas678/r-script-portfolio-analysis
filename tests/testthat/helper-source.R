## Sources the pure/side-effect-free scripts under test. testthat auto-runs
## every tests/testthat/helper-*.R file before the test files themselves.
library(tidyverse)
library(here)

source(here::here("scripts", "price_cache.R"))
source(here::here("scripts", "get_volatility_data.R"))
source(here::here("scripts", "portfolio_returns.R"))
