# 03_unsup_pipeline/unsup_preprocess.R

unsup_preprocess <- function(olive_oil) {
  # Data Preprocessing
  library(dplyr)
  ## Remove ID column
  olive_oil_clean <- olive_oil %>% select(-id)
  ## Standardise data
  olive_oil_scaled <- olive_oil_clean %>%
    mutate(across(where(is.numeric), scale))
  ## Check the structure of the scaled data
  str(olive_oil_scaled)
  ## Return the cleaned and scaled data
  olive_oil_scaled
}
