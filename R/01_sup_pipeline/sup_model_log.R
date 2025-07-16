# 01_sup_pipeline/sup_log_model.R

sup_log_fit <- function(train_data) {
  # Build Logistic Regression baseline model
  library(nnet)
  library(tidyverse)
  library(tidymodels)
  ## Build the recipe
  log_rec <- recipe(casualty_severity ~ ., data = train_data) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors())
  ## Build the model specification
  log_spec <- multinom_reg(penalty = 0) %>%
    set_engine("nnet") %>%
    set_mode("classification")
  ## Build the workflow
  log_wf <- workflow() %>%
    add_recipe(log_rec) %>%
    add_model(log_spec)
  ## Fit the model
  log_fit <- log_wf %>%
    fit(data = train_data)
  ## Return the model
  log_fit
}

sup_log_eval <- function(log_fit, test_data) {
  # Evaluate the model
  ## Check the model
  log_preds <- predict(log_fit, test_data, type = "prob") %>%
    bind_cols(predict(log_fit, test_data)) %>%
    bind_cols(test_data)
  ## Check the ROC curve
  roc_curve(
    log_preds,
    truth = casualty_severity,
    .pred_Slight,
    .pred_Serious,
    .pred_Fatal
  ) %>%
    autoplot() +
    labs(
      title = "ROC Curve for Logistic Regression Model",
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_minimal()
  ## Check the confusion matrix
  conf_mat(log_preds, truth = casualty_severity, estimate = .pred_class) %>%
    autoplot(type = "heatmap") +
    labs(
      title = "Confusion Matrix for Logistic Regression Model",
      x = "Predicted",
      y = "Actual"
    ) +
    theme_minimal()
  ## Check the accuracy, precision, recall, and F1 score
  log_accuracy <- log_preds %>%
    accuracy(truth = casualty_severity, estimate = .pred_class)
  log_precision <- log_preds %>%
    precision(truth = casualty_severity, estimate = .pred_class)
  log_recall <- log_preds %>%
    recall(truth = casualty_severity, estimate = .pred_class)
  log_f1 <- log_preds %>%
    f_meas(truth = casualty_severity, estimate = .pred_class)
  ## Create a summary table and show it
  log_summary <- tibble(
    model = "Logistic Regression",
    accuracy = log_accuracy$.estimate,
    precision = log_precision$.estimate,
    recall = log_recall$.estimate,
    f1_score = log_f1$.estimate
  )
  log_summary %>%
    knitr::kable(caption = "Logistic Regression Model Summary")
  ## Return the summary
  log_summary
}
