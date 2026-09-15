## Persistent Pipeline Log
##
## cat()/message()/warning() calls only reach an interactive RStudio console -
## when a fund-cleaner script is run headlessly via `Rscript scripts/X.R`
## (no console attached), that output is gone the moment the process exits.
## log_pipeline_issue() appends the same notice to logs/pipeline.log so it
## survives headless runs and can be grepped after the fact. Call it
## alongside (not instead of) the existing cat()/warning() - the console
## output stays useful for interactive runs.
##
## Pure/side-effect-limited to appending one line to logs/pipeline.log: safe
## to source() from anywhere.

#' Appends one line to logs/pipeline.log: timestamp, invoking script name
#' (detected from Rscript's `--file=` argument; "interactive" when run from
#' RStudio/the console, where the message is already visible live), and the
#' message text.
log_pipeline_issue <- function(message) {
  log_path <- here::here("logs", "pipeline.log")
  dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)

  script_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", script_args, value = TRUE)
  script <- if (length(file_arg) > 0) basename(sub("^--file=", "", file_arg[1])) else "interactive"

  cat(sprintf("[%s] %s: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), script, message),
      file = log_path, append = TRUE)
}
