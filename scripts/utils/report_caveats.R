## Reading caveats of the report (Section 1.3 "Read With Care") as plain data.
##
## One source for the report text and for the multi-agent export's `caveats` sheet, so the agents
## are told exactly what the reader of the report is told. Pure/side-effect-free.

# Args:
#   window_months, n_steps: the analysis window the whole report is measured on
#   assumed: tibble(label, weight, risk_share_pct) of the synthetic building blocks (sleeves)
#   lookthrough_coverage_pct: weight with a usable price history in the holdings-level sections
#   unmodelled_labels: building blocks left out of the sleeve model (no price series)
#   proxied_labels: funds modelled with another fund's holdings
#   unclassified_equity_pct: % of the equity weight without a sector (NA/0 = no bullet)
#
# Returns: character vector, one markdown bullet ("- **Topic.** text") per caveat, most
# important first; a bullet whose subject does not apply is omitted.

report_caveats <- function(window_months, n_steps, assumed, lookthrough_coverage_pct,
                           unmodelled_labels = character(0), proxied_labels = character(0),
                           unclassified_equity_pct = 0) {
  out <- c(
    paste0("- **Short window.** ", round(window_months, 1), " months (", n_steps,
           " steps): a single market regime. The annualized figures are extrapolations; the return over the window is not annualized. ",
           "The price source's free plan caps history at one year."),
    if (nrow(assumed) > 0) {
      paste0("- **Assumed building blocks.** ", paste(assumed$label, collapse = " and "),
             " have no price history and run on the assumed return and volatility in `config/sleeve_assumptions.csv` (",
             round(sum(assumed$weight), 1), "% of the weight, ", round(sum(assumed$risk_share_pct), 1), "% of the risk).")
    },
    paste0("- **Look-through sections cover part of the weight.** Only ", round(lookthrough_coverage_pct, 1),
           "% of the weight has a usable price history in the holdings-level analysis (Sections 2.2-2.5, 6.2-6.4); ",
           "countries, sectors and single names (Sections 3-5) cover all of it."),
    if (length(unmodelled_labels) > 0) {
      paste0("- **Not modelled:** ", paste(unmodelled_labels, collapse = ", "), " (no price series).")
    },
    if (length(proxied_labels) > 0) {
      paste0("- **Proxied holdings:** ", paste(proxied_labels, collapse = ", "),
             " is modelled with another fund's holdings (Section 7).")
    },
    if (!is.na(unclassified_equity_pct) && unclassified_equity_pct > 0.05) {
      paste0("- **Unclassified sectors.** ", round(unclassified_equity_pct, 1),
             "% of the equity weight has no sector in the ticker list and shows as \"Unclassified\" in sector tables.")
    },
    "- **Currency is the trading currency** of each position, not the economic exposure of the underlying companies."
  )
  out
}
