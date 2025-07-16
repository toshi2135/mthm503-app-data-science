# 02_reg_pipeline/reg_targets.R

# Load necessary libraries
library(targets)
library(tarchetypes)
library(here)

# Source custom functions and data loading script
source(here("R/02_reg_pipeline", "reg_load_data.R"))
source(here("R/02_reg_pipeline", "reg_preprocess.R"))

# Define the targets for the supervised learning pipeline
reg_targets <- list(
  # Load data
  tar_target(reg_raw_data, reg_load_data()),
  # Preprocess data
  tar_target(reg_clean_data, reg_preprocess_data(reg_raw_data))
)
