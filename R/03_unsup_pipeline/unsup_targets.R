# 03_unsup_pipeline/unsup_targets.R

# Load necessary libraries
library(targets)
library(tarchetypes)
library(here)

# Source custom functions and data loading script
source(here("R/03_unsup_pipeline", "unsup_load_data.R"))
source(here("R/03_unsup_pipeline", "unsup_eda.R"))
source(here("R/03_unsup_pipeline", "unsup_preprocess.R"))
source(here("R/03_unsup_pipeline", "unsup_pca.R"))

# Define the targets for the supervised learning pipeline
unsup_targets <- list(
  # Load data
  tar_target(unsup_raw_data, unsup_load_data()),
  # Explanatory Data Analysis
  tar_target(unsup_eda_results, unsup_eda(unsup_raw_data)),

  # Preprocess data
  tar_target(unsup_clean_data, unsup_preprocess(unsup_raw_data)),

  # Perform PCA
  tar_target(unsup_pca_results, unsup_perform_pca(unsup_clean_data)$pca_results)
  
)