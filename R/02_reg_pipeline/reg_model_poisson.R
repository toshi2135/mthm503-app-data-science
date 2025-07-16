# 02_reg_pipeline/reg_mode_poisson.R

reg_poisson_fit <- function(fire_rescue_clean) {
  # Apply Poisson Regression Model
  model_poisson <- glm(
    n_casualties ~ age_band + sex + extrication,
    data = fire_rescue_clean,
    family = poisson()
  )
  summary(model_poisson)
  model_poisson
}
