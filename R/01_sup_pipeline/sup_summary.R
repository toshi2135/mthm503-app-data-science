# 01_sup_pipeline/sup_summary.R

sup_summarise <- function(
  rf_summary,
  log_summary,
  rf_summary_weighted,
  rf_summary_final
) {
  # Create a summary table with all models
  sup_summary <- bind_rows(
    rf_summary,
    log_summary,
    rf_summary_weighted,
    rf_summary_final
  ) %>%
    mutate(
      model = factor(
        model,
        levels = c(
          "Random Forest",
          "Logistic Regression",
          "Random Forest with Case Weights",
          "Final Random Forest"
        )
      )
    ) %>%
    knitr::kable(
      caption = "Model Comparison Summary"
    )
  sup_summary
}
