# 02_reg_pipeline/reg_summary.R

reg_summarise <- function(
  glm_model,
  glm_updated_model,
  gam_3df_model,
  gam_2df_model,
  binom_model,
  poission
) {
  # Summarise model results
  library(dplyr)
  library(broom)
  tidy_vglm <- function(model, model_label = "VGLM") {
    coefs <- coef(model)
    se <- sqrt(diag(vcov(model)))
    tibble(
      term = names(coefs),
      estimate = exp(coefs),
      std.error = se,
      conf.low = exp(coefs - 1.96 * se),
      conf.high = exp(coefs + 1.96 * se),
      model = model_label
    )
  }
  gam_2df_tidy <- tidy_vglm(model_gam_test_2df, "Multinomial GAM 2df")
  gam_3df_tidy <- tidy_vglm(model_gam_test_3df, "Multinomial GAM 3df")
  reg_model_summary <- bind_rows(
    tidy(model_multi, conf.int = TRUE, exponentiate = TRUE) %>%
      mutate(model = "Multinomial GLM"),
    tidy(model_multi2, conf.int = TRUE, exponentiate = TRUE) %>%
      mutate(model = "Updated Multinomial GLM"),
    binary_models_df %>% mutate(model = "Binomial Regression"),
    gam_2df_tidy,
    gam_3df_tidy,
    tidy(poisson, conf.int = TRUE, exponentiate = TRUE) %>%
      mutate(model = "Poisson Regression")
  )
  reg_model_summary
}
