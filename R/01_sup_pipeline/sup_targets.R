# 01_sup_pipeline/sup_targets.R

# Load necessary libraries
library(targets)
library(tarchetypes)
library(here)

# Source custom functions and data loading script
source(here("R/01_sup_pipeline", "sup_load_data.R"))
source(here("R/01_sup_pipeline", "sup_preprocess.R"))
source(here("R/01_sup_pipeline", "sup_model_rf_baseline.R"))
source(here("R/01_sup_pipeline", "sup_model_log.R"))
source(here("R/01_sup_pipeline", "sup_model_rf_weighted.R"))
source(here("R/01_sup_pipeline", "sup_model_rf_tuned.R"))
source(here("R/01_sup_pipeline", "sup_summary.R"))

# Define the targets for the supervised learning pipeline
sup_targets <- list(
  # Load data
  tar_target(sup_raw_data, sup_load_data()),
  # Preprocess data
  tar_target(sup_clean_data, sup_preprocess_data(sup_raw_data)),
  tar_target(sup_full_data, sup_clean_data$sup_data),
  tar_target(sup_train, sup_clean_data$train),
  tar_target(sup_test, sup_clean_data$test),

  # Train and evaluate Random Forest baseline model
  tar_target(sup_rf_model, sup_rf_fit(sup_train)),
  tar_target(sup_rf_summary, sup_rf_eval(sup_rf_model, sup_test)),

  # Train and evaluate Logistic Regression model
  tar_target(sup_log_model, sup_log_fit(sup_train)),
  tar_target(sup_log_summary, sup_log_eval(sup_log_model, sup_test)),

  # Train and evaluate Random Forest with weights
  tar_target(sup_split_weighted_data, sup_rf_weighted_split(sup_full_data)),
  tar_target(sup_train_weighted_data, sup_split_weighted_data$train),
  tar_target(sup_test_weighted_data, sup_split_weighted_data$test),
  tar_target(
    sup_rf_weighted_model,
    sup_rf_weighted_fit(sup_train_weighted_data)
  ),
  tar_target(
    sup_rf_weighted_summary,
    sup_rf_weighted_eval(sup_rf_weighted_model, sup_test_weighted_data)
  ),

  # Tune Random Forest model
  tar_target(sup_rf_tuned_model, sup_rf_tuned_fit(sup_train)),
  tar_target(
    sup_rf_tuned_summary,
    sup_rf_tuned_eval(sup_rf_tuned_model, sup_test)
  ),
  # Combine model summaries
  tar_target(
    sup_model_summary,
    sup_summarise(
      sup_rf_summary,
      sup_log_summary,
      sup_rf_weighted_summary,
      sup_rf_tuned_summary
    )
  )
)
