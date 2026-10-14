## parse_ishares_workbook() (scripts/utils/ishares_loaders.R): the pure half of the
## iShares SpreadsheetML import. Fixtures are generated XML, no network.
source(here::here("scripts", "utils", "ishares_loaders.R"))

xml_cell <- function(x) sprintf('<ss:Cell><ss:Data ss:Type="String">%s</ss:Data></ss:Cell>', x)
xml_row  <- function(...) paste0("<ss:Row>", paste(vapply(list(...), xml_cell, ""), collapse = ""), "</ss:Row>")
workbook <- function(sheet_name, header, rows) {
  charToRaw(paste0(
    '<?xml version="1.0"?><ss:Workbook xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet">',
    '<ss:Worksheet ss:Name="Other"><ss:Table>', xml_row("not", "holdings"), '</ss:Table></ss:Worksheet>',
    '<ss:Worksheet ss:Name="', sheet_name, '"><ss:Table>',
    xml_row("Fund name"), xml_row("Per", "30.Sept.2026"),     # metadata rows with fewer cells
    do.call(xml_row, as.list(header)),
    paste(rows, collapse = ""),
    xml_row("footer note"),                                     # trailing row with fewer cells
    '</ss:Table></ss:Worksheet></ss:Workbook>'
  ))
}

test_that("the new export format: 'Holdings' sheet, decimal comma, no Standort column", {
  raw <- workbook("Holdings",
                  c("Emittententicker", "Name", "Sektor", "Anlageklasse", "Gewichtung (%)"),
                  c(xml_row("NVDA", "NVIDIA", "IT", "Aktien", "5,79"),
                    xml_row("AAPL", "APPLE", "IT", "Aktien", "5,39")))

  out <- parse_ishares_workbook(raw, "iSharesWorld", sheet = "Holdings")

  expect_equal(out$ticker, c("NVDA", "AAPL"))
  expect_equal(out$weight, c(5.79, 5.39))
  expect_true(all(is.na(out$country)))
  expect_true(all(out$index == "iSharesWorld"))
})

test_that("the old export format keeps its Standort column as country", {
  raw <- workbook("Positionen",
                  c("Emittententicker", "Name", "Gewichtung (%)", "Standort"),
                  xml_row("SAP", "SAP", "1.56", "Deutschland"))

  out <- parse_ishares_workbook(raw, "iSharesACWI")

  expect_equal(out$weight, 1.56)
  expect_equal(out$country, "Deutschland")
})

test_that("a missing sheet or header row fails with a clear message", {
  raw <- workbook("Holdings", c("Emittententicker", "Name", "Gewichtung (%)"), xml_row("A", "A", "1"))

  expect_error(parse_ishares_workbook(raw, "x", sheet = "Positionen"), "no worksheet named 'Positionen'")
  expect_error(parse_ishares_workbook(workbook("Holdings", c("Ticker", "Name"), xml_row("A", "A")), "x", sheet = "Holdings"),
               "header row")
})

test_that("a doubled BOM in front of the XML is tolerated", {
  raw <- c(as.raw(c(0xef, 0xbb, 0xbf, 0xef, 0xbb, 0xbf)),
           workbook("Holdings", c("Emittententicker", "Name", "Gewichtung (%)"), xml_row("A", "A", "1,5")))

  expect_equal(parse_ishares_workbook(raw, "x", sheet = "Holdings")$weight, 1.5)
})

## add_country_exchange() / ishares_download_url() / ishares_donors()
holdings <- function(...) tibble::tibble(...)
donor <- function(...) tibble::tibble(...)

test_that("ishares_download_url carries the product id", {
  expect_match(ishares_download_url(251850), "portfolioId=251850&component=fundDownloadV2")
})

test_that("country and exchange are matched on ticker + name first", {
  df <- holdings(ticker = c("ROP", "ROP"), Name = c("ROCHE", "ROPER TECH"), weight = c(2, 1))
  d  <- donor(ticker = c("ROP", "ROP"), Name = c("ROCHE", "ROPER TECH"),
              country = c("Schweiz", "Vereinigte Staaten"), `Börse` = c("SIX Swiss Exchange", "NYSE"))

  out <- add_country_exchange(df, list(d))

  expect_equal(out$country, c("Schweiz", "Vereinigte Staaten"))
  expect_equal(out$`Börse`, c("SIX Swiss Exchange", "NYSE"))
  expect_equal(attr(out, "unmatched_weight"), 0)
})

test_that("a ticker-only match is used when unique, and refused when the ticker is ambiguous", {
  df <- holdings(ticker = c("ASML", "ROP"), Name = c("ASML HOLDING NV", "SOME RENAMED NAME"), weight = c(3, 1))
  d  <- donor(ticker = c("ASML", "ROP", "ROP"), Name = c("ASML HOLDING", "ROCHE", "ROPER"),
              country = c("Niederlande", "Schweiz", "Vereinigte Staaten"),
              `Börse` = c("Euronext Amsterdam", "SIX", "NYSE"))

  out <- add_country_exchange(df, list(d))

  expect_equal(out$country, c("Niederlande", NA))          # ASML unique -> matched; ROP ambiguous -> not guessed
  expect_equal(attr(out, "unmatched_weight"), 1)
})

test_that("the first donor wins and existing values are kept", {
  df <- holdings(ticker = c("A", "B"), Name = c("A", "B"), weight = c(1, 1),
                 country = c(NA, "Kanada"), `Börse` = c(NA, "TSX"))
  first  <- donor(ticker = "A", Name = "A", country = "Japan", `Börse` = "Tokyo")
  second <- donor(ticker = c("A", "B"), Name = c("A", "B"), country = c("Indien", "Indien"), `Börse` = c("NSE", "NSE"))

  out <- add_country_exchange(df, list(first, second))

  expect_equal(out$country, c("Japan", "Kanada"))
  expect_equal(out$`Börse`, c("Tokyo", "TSX"))
})

test_that("donors without country/exchange and an empty donor list are harmless", {
  df <- holdings(ticker = "A", Name = "A", weight = 5)
  d  <- donor(ticker = "A", Name = "A", country = NA_character_, `Börse` = NA_character_)

  expect_equal(attr(add_country_exchange(df, list(d)), "unmatched_weight"), 5)
  expect_equal(attr(add_country_exchange(df, list()), "unmatched_weight"), 5)
})

test_that("read_ishares_donor reads a previous raw export, an absent file is an empty donor", {
  dir <- withr::local_tempdir()
  writexl::write_xlsx(tibble::tibble(ticker = "A", Name = "A", country = "Japan", `Börse` = "Tokyo", weight = 1),
                      file.path(dir, "iSharesX.xlsx"))
  writexl::write_xlsx(tibble::tibble(ticker = "A"), file.path(dir, "NoCols.xlsx"))

  expect_equal(read_ishares_donor("iSharesX.xlsx", dir)$country, "Japan")
  expect_equal(nrow(read_ishares_donor("gone.xlsx", dir)), 0)
  expect_equal(nrow(read_ishares_donor("NoCols.xlsx", dir)), 0)
  expect_equal(length(ishares_donors("iSharesX.xlsx", dir)), 5)
})
