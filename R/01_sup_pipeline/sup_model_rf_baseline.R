# 01_sup_pipeline/sup_model_rf_baseline.R

sup_rf_fit <- function(train_data) {
  # Build Random Forest baseline model
  library(ranger)
  ## Build the recipe
  rf_rec <- recipe(casualty_severity ~ ., data = train_data)
  ## Build the model specification
  rf_spec <- rand_forest(trees = 500) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("classification")
  ## Build the workflow
  rf_wf <- workflow() %>%
    add_recipe(rf_rec) %>%
    add_model(rf_spec)
  ## Fit the model
  rf_fit <- rf_wf %>%
    fit(data = train_data)
  ## Return the model
  rf_fit
}

sup_rf_eval <- function(rf_fit, test_data) {
  # Evaluate the Random Forest baseline model
  ## Check the model
  rf_preds <- predict(rf_fit, test_data, type = "prob") %>%
    bind_cols(predict(rf_fit, test_data)) %>%
    bind_cols(test_data)
  ## Check the ROC curve
  roc_curve(
    rf_preds,
    truth = casualty_severity,
    .pred_Slight,
    .pred_Serious,
    .pred_Fatal
  ) %>%
    autoplot() +
    labs(
      title = "ROC Curve for Random Forest Model",
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_minimal()
  ## Check the confusion matrix
  conf_mat(rf_preds, truth = casualty_severity, estimate = .pred_class) %>%
    autoplot(type = "heatmap") +
    labs(
      title = "Confusion Matrix for Random Forest Model",
      x = "Predicted",
      y = "Actual"
    ) +
    theme_minimal()
  ## Check the accuracy, precision, recall, and F1 score
  rf_accuracy <- rf_preds %>%
    accuracy(truth = casualty_severity, estimate = .pred_class)
  rf_precision <- rf_preds %>%
    precision(truth = casualty_severity, estimate = .pred_class)
  rf_recall <- rf_preds %>%
    recall(truth = casualty_severity, estimate = .pred_class)
  rf_f1 <- rf_preds %>%
    f_meas(truth = casualty_severity, estimate = .pred_class)
  ## Create a summary table and show it
  rf_summary <- tibble(
    model = "Random Forest",
    accuracy = rf_accuracy$.estimate,
    precision = rf_precision$.estimate,
    recall = rf_recall$.estimate,
    f1_score = rf_f1$.estimate
  )
  rf_summary %>%
    knitr::kable(caption = "Random Forest Model Summary")
  ## Find the importance of the features
  vip::vip(rf_fit, num_features = 10) +
    labs(title = "Feature Importance for Random Forest Model") +
    theme_minimal()
  ## Return the summary
  rf_summary
}
