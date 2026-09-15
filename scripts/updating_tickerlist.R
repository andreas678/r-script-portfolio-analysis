
# Identifiziere alle fehlenden Tickers
missing_tickers_all <- tickerliste %>%
  filter(is.na(companyname), !is.na(ticker)) %>%
  distinct(ticker) %>%
  pull(ticker) %>%
  sort()

cat("EODHD Search API - Batch-Anreicherung\n")
cat(strrep("═", 70), "\n\n")
cat(sprintf("Zu verarbeitende Tickers: %d\n\n", length(missing_tickers_all)))

# Batch-Verarbeitung
batch_size <- 50
num_batches <- ceiling(length(missing_tickers_all) / batch_size)

api_results <- tibble()

for (batch_num in 1:num_batches) {
  start_idx <- (batch_num - 1) * batch_size + 1
  end_idx <- min(batch_num * batch_size, length(missing_tickers_all))
  
  batch_tickers <- missing_tickers_all[start_idx:end_idx]
  batch_size_actual <- length(batch_tickers)
  
  cat(sprintf("[Batch %d/%d] Verarbeite %d Tickers... ", 
              batch_num, num_batches, batch_size_actual))
  flush.console()
  
  batch_data <- tibble()
  
  for (ticker in batch_tickers) {
    result <- get_company_name_from_ticker(ticker, api_key)
    
    batch_data <- bind_rows(batch_data, tibble(
      ticker = ticker,
      companyname = result$name,
      exchange = result$exchange,
      api_found = result$found
    ))
  }
  
  # Count successful entries
  success_count <- sum(batch_data$api_found)
  cat(sprintf("✓ %d / %d found\n", success_count, batch_size_actual))
  
  api_results <- bind_rows(api_results, batch_data)
}

cat("\n" , strrep("═", 70), "\n")
cat("BATCH-ANREICHERUNG ABGESCHLOSSEN\n")
cat(strrep("═", 70), "\n\n")

# Statistiken
total_api_success <- sum(api_results$api_found)
total_api_failed <- sum(!api_results$api_found)

cat("Results:\n")
cat(sprintf("  Total tickers processed: %d\n", nrow(api_results)))
cat(sprintf("  Successfully found:          %d (%.1f%%)\n", 
            total_api_success, 
            total_api_success / nrow(api_results) * 100))
cat(sprintf("  Not found:                   %d (%.1f%%)\n\n", 
            total_api_failed,
            total_api_failed / nrow(api_results) * 100))

# Show successful examples
cat("Examples of successful API lookups:\n")
print(
  api_results %>%
    filter(api_found) %>%
    select(ticker, companyname, exchange) %>%
    slice(1:20)
)

cat("\n")

# Show not found
if (total_api_failed > 0) {
  cat(sprintf("Tickers not found (%d):\n", total_api_failed))
  print(
    api_results %>%
      filter(!api_found) %>%
      select(ticker) %>%
      head(20)
  )
}

