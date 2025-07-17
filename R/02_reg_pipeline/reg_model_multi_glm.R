# 02_reg_pipeline/reg_model_multi_glm.R

reg_glm_fit <- function(fire_rescue_clean) {
  # Statistical analysis using Multinomial GLM
  library(nnet)
  ## Fit the Multinomial GLM
  model_multi_glm <- nnet::multinom(
    extrication ~ age_band + sex + age_band:sex,
    data = fire_rescue_clean
  )
  ## Check the summary of the model
  summary(model_multi_glm)
  ## Check the model coefficients
  coef(model_multi_glm)
  ## Check the model residuals
  residuals(model_multi_glm)
  # ---
  model_multi_glm
}
