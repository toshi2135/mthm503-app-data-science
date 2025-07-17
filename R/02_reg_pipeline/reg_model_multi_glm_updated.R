# 02_reg_pipeline/reg_model_multi_glm_updated.R

reg_glm_updated_fit <- function(fire_rescue_clean) {
  # Upgrade the model, use age_band as nominal factor instead of ordered factor
  ## Fit the Multinomial GLM again
  library(nnet)
  model_multi2 <- nnet::multinom(
    extrication ~ age_band + sex + age_band:sex,
    data = fire_rescue_clean
  )
  ## Check the summary of the upgraded model
  summary(model_multi2)
  ## Check the model coefficients
  coef(model_multi2)
  ## Check the model residuals
  residuals(model_multi2)
  model_multi2
}
