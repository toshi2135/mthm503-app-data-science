# 02_reg_pipeline/reg_model_multi_gam.R

reg_gam_fit_3df <- function(fire_rescue_clean) {
  # Apply Multinomial GAM model
  library(dplyr)
  library(VGAM)
  ## Create numeric age_band variable
  fire_rescue_clean <- fire_rescue_clean %>%
    mutate(age_band_num = as.numeric(age_band))
  ## Fit the Multinomial GAM model
  model_gam_3df <- vglm(
    extrication ~ sm.ns(age_band_num, df = 3),
    family = multinomial,
    data = fire_rescue_clean
  )
  ## Check the summary of the Multinomial GAM model
  summary(model_gam_3df)
  model_gam_3df
}

reg_gam_fit_2df <- function(fire_rescue_clean) {
  # Apply Multinomial GAM model
  library(dplyr)
  library(VGAM)
  ## Create numeric age_band variable
  fire_rescue_clean <- fire_rescue_clean %>%
    mutate(age_band_num = as.numeric(age_band))
  ## Fit the Multinomial GAM model
  ## Change the level to 2 degrees of freedom
  model_gam_2df <- vglm(
    extrication ~ sm.ns(age_band_num, df = 2),
    family = multinomial,
    data = fire_rescue_clean
  )
  ## Check the summary of the Multinomial GAM model with 2 degrees of freedom
  summary(model_gam_2df)
  model_gam_2df
}
