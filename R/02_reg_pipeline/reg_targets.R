# 02_reg_pipeline/reg_targets.R

# Load necessary libraries
library(targets)
library(tarchetypes)
library(here)

# Source custom functions and data loading script
source(here("R/02_reg_pipeline", "reg_load_data.R"))
source(here("R/02_reg_pipeline", "reg_preprocess.R"))
source(here("R/02_reg_pipeline", "reg_model_multi_glm.R"))
source(here("R/02_reg_pipeline", "reg_model_multi_glm_updated.R"))
source(here("R/02_reg_pipeline", "reg_model_multi_gam.R"))
source(here("R/02_reg_pipeline", "reg_model_binom.R"))
source(here("R/02_reg_pipeline", "reg_chi_squared_test.R"))
source(here("R/02_reg_pipeline", "reg_model_poisson.R"))
source(here("R/02_reg_pipeline", "reg_summary.R"))

# Define the targets for the supervised learning pipeline
reg_targets <- list(
  # Load data
  tar_target(reg_raw_data, reg_load_data()),
  # Preprocess data
  tar_target(reg_clean_data, reg_preprocess_data(reg_raw_data)),
  # Modelling using Multinomial GLM
  tar_target(reg_glm_model, reg_glm_fit(reg_clean_data)),
  # Modelling using updated Multinomial GLM
  tar_target(reg_updated_data, reg_update_data(reg_clean_data)),
  tar_target(reg_glm_updated_model, reg_glm_updated_fit(reg_updated_data)),
  # Modelling using Multinomial GAM
  tar_target(reg_gam_model_3df, reg_gam_fit_3df(reg_updated_data)),
  tar_target(reg_gam_model_2df, reg_gam_fit_2df(reg_updated_data)),
  # Modelling using Binomial Regression
  tar_target(reg_binom_models, reg_binom_fit(reg_updated_data)),
  # Chi-squared test
  tar_target(reg_chi_squared, reg_chi_squared_test(reg_updated_data)),
  # Modelling using Poisson Regression
  tar_target(reg_poisson_model, reg_poisson_fit(reg_updated_data)),
  # Summarise model results
  tar_target(
    reg_model_summary,
    reg_summarise(
      glm_model = reg_glm_model,
      glm_updated_model = reg_glm_updated_model,
      gam_3df_model = reg_gam_model_3df,
      gam_2df_model = reg_gam_model_2df,
      binom_models = reg_binom_models,
      poission = reg_poisson_model
    )
  )
)
