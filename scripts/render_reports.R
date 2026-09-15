# Renders portfolio_report.Rmd for both "portfolio_a" and "portfolio_b" in one pass,
# each to its own dedicated filename in reports/ (portfolio_report.Rmd itself
# always writes to the same default filename next to the Rmd, so a headless
# render with explicit output_file per portfolio - as the Rmd's own header
# comment recommends - is the only way to keep both outputs side by side).
#
# Usage (from the portfolio/ project root, e.g. via Rscript or source()):
#   Rscript scripts/render_reports.R

reports_dir <- here::here("reports")
dir.create(reports_dir, showWarnings = FALSE, recursive = TRUE)

# portfolio id -> dedicated output filename
report_targets <- c(
  portfolio_a = "portfolio_report_a.html",
  portfolio_b  = "portfolio_report_b.html"
)

for (portfolio_id in names(report_targets)) {
  message("Rendering '", portfolio_id, "' portfolio report...")
  rmarkdown::render(
    input       = here::here("scripts", "portfolio_report.Rmd"),
    output_format = "html_document",
    params      = list(portfolio = portfolio_id),
    output_file = report_targets[[portfolio_id]],
    output_dir  = reports_dir,
    envir       = new.env() # isolate each render's chunk environment
  )
}

message("Done. Reports written to: ", reports_dir)
