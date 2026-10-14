#!/usr/bin/env Rscript
## Entry point for portfolio's testthat suite (DEVELOPMENT-PROPOSAL-2026-09-27.md
## §3.1). Run from the repo root:
##   Rscript tests/run_tests.R
## .github/workflows/ci.yml runs this on every push/PR touching scripts/** or
## tests/**. No EODHD/Finnhub calls anywhere in this suite — every test
## exercises a pure function or an offline-safe code path only, same
## reasoning as multi-agent's own test suite (§2.3).
##
## test_dir()'s stop_on_failure defaults to TRUE, so a failing test aborts
## this script with a non-zero exit code, which is what makes it usable as a
## CI gate.

library(testthat)
library(here)

test_dir(here::here("tests", "testthat"))
