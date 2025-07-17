# 03_unsup_pipeline/unsup_targets.R

# Load necessary libraries
library(targets)
library(tarchetypes)
library(here)

# Source custom functions and data loading script
source(here("R/03_unsup_pipeline", "unsup_load_data.R"))
source(here("R/03_unsup_pipeline", "unsup_eda.R"))

# Define the targets for the supervised learning pipeline
unsup_targets <- list(
  # Load data
  tar_target(unsup_raw_data, unsup_load_data()),
  # Explaratory Data Analysis
  tar_target(unsup_eda_results, unsup_eda(unsup_raw_data))
  # 
  # # Preprocess data
  # tar_target(unsup_clean_data, preprocess_data(unsup_raw_data)),
  # 
  # # Perform PCA
  # tar_target(unsup_pca_results, perform_pca(unsup_clean_data)),
  # 
  # # Perform clustering
  # tar_target(unsup_clustering_results, perform_clustering(unsup_clean_data)),
  # 
  # # Visualize PCA results
  # tar_target(unsup_pca_plot, plot_pca(unsup_pca_results)),
  # 
  # # Visualize clustering results
  # tar_target(unsup_clustering_plot, plot_clustering(unsup_clustering_results))
)