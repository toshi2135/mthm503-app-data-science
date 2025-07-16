# 02_reg_pipeline/reg_model_binom.R

reg_binom_fit <- function(fire_rescue_clean) {
  # Apply Binomial Regression (Logistic Regression) Model
  library(dplyr)
  library(broom)
  ## Create a function to fit a binary model for each extrication method
  make_binary_model <- function(df, method) {
    df <- df %>%
      mutate(
        extricated = factor(
          ifelse(extrication == method, 1, 0),
          levels = c(0, 1)
        )
      )
    model <- glm(
      extricated ~ age_band + sex + age_band:sex,
      data = df,
      family = binomial(link = "logit")
    )
    broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  }
  ## Fit the binary model for each extrication method
  methods <- levels(fire_rescue_clean$extrication)
  binary_models <- lapply(methods, function(method) {
    make_binary_model(fire_rescue_clean, method)
  })
  ## Combine the results into a single data frame
  binary_models_df <- do.call(rbind, binary_models)
  ## Check the binary models results
  binary_models_df
}
