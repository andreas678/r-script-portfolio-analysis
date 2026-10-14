## Secret Redaction Helper
##
## EODHD (and Finnhub) authenticate solely via a query-string parameter
## (`?api_token=...`) - there is no header-based alternative, so the key
## necessarily appears in every request URL. httr::GET() embeds that full
## URL in its error message on network-level failures (timeout, DNS
## failure, SSL error), which tryCatch() handlers here then cat()/warning()
## to the console or, when stdout/stderr from a headless run is redirected
## to a file (cron, Docker), to a log stored on disk. redact_secret() strips
## the key from any string before it reaches cat()/warning()/log_pipeline_issue(),
## so a caught error can be printed/logged without leaking credentials.

#' Replaces every occurrence of `secret` in `text` with a placeholder.
#' No-op if secret is empty/NA, so callers can pass an unset api_key safely.
redact_secret <- function(text, secret) {
  if (is.null(text) || is.na(text) || is.null(secret) || is.na(secret) || nchar(secret) == 0) {
    return(text)
  }
  gsub(secret, "***REDACTED***", text, fixed = TRUE)
}
