# 01_sup_pipeline/sup_rf_weighted.R

sup_rf_weighted_split <- function(sup_data) {
  # Calculate case weights based on the target variable distribution
  class_weights <- sup_data %>%
    dplyr::count(casualty_severity) %>%
    mutate(weight = 1 / n) %>%
    select(casualty_severity, weight)
  ## Join the case weights to the training data
  sup_data_weighted <- sup_data %>%
    left_join(class_weights, by = "casualty_severity")
  ## Split the weighted data for train and test
  split_weighted <- initial_split(sup_data_weighted, strata = casualty_severity)
  train_data_weighted <- training(split_weighted)
  test_data_weighted <- testing(split_weighted)
  list(
    train = train_data_weighted,
    test = test_data_weighted
  )
}

sup_rf_weighted_fit <- function(train_data_weighted) {
  # Apply case weights to the Random Forest model
  ## Build the recipe
  rf_rec_weighted <- recipe(
    casualty_severity ~ .,
    data = train_data_weighted
  ) %>%
    update_role(weight, new_role = "case_weight")
  ## Build the model specification
  rf_spec_weighted <- rand_forest(trees = 500) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("classification")
  ## Build the workflow
  rf_wf_weighted <- workflow() %>%
    add_recipe(rf_rec_weighted) %>%
    add_model(rf_spec_weighted)
  ## Fit the model with case weights
  rf_fit_weighted <- rf_wf_weighted %>%
    fit(data = train_data_weighted)
  ## Return the fitted model and the weighted test data
  rf_fit_weighted
}

sup_rf_weighted_eval <- function(rf_fit_weighted, test_data_weighted) {
  # Evaluate the Random Forest model with case weights
  ## Check the model
  rf_preds_weighted <- predict(
    rf_fit_weighted,
    test_data_weighted,
    type = "prob"
  ) %>%
    bind_cols(predict(rf_fit_weighted, test_data_weighted)) %>%
    bind_cols(test_data_weighted)
  ## Check the ROC curve
  roc_curve(
    rf_preds_weighted,
    truth = casualty_severity,
    .pred_Slight,
    .pred_Serious,
    .pred_Fatal
  ) %>%
    autoplot() +
    labs(
      title = "ROC Curve for Random Forest Model with Case Weights",
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_minimal()
  ## Check the confusion matrix
  conf_mat(
    rf_preds_weighted,
    truth = casualty_severity,
    estimate = .pred_class
  ) %>%
    autoplot(type = "heatmap") +
    labs(
      title = "Confusion Matrix for Random Forest Model with Case Weights",
      x = "Predicted",
      y = "Actual"
    ) +
    theme_minimal()
  ## Check the accuracy, precision, recall, and F1 score
  rf_accuracy_weighted <- rf_preds_weighted %>%
    accuracy(truth = casualty_severity, estimate = .pred_class)
  rf_precision_weighted <- rf_preds_weighted %>%
    precision(truth = casualty_severity, estimate = .pred_class)
  rf_recall_weighted <- rf_preds_weighted %>%
    recall(truth = casualty_severity, estimate = .pred_class)
  rf_f1_weighted <- rf_preds_weighted %>%
    f_meas(truth = casualty_severity, estimate = .pred_class)
  ## Create a summary table and show it
  rf_summary_weighted <- tibble(
    model = "Random Forest with Case Weights",
    accuracy = rf_accuracy_weighted$.estimate,
    precision = rf_precision_weighted$.estimate,
    recall = rf_recall_weighted$.estimate,
    f1_score = rf_f1_weighted$.estimate
  )
  rf_summary_weighted %>%
    knitr::kable(caption = "Random Forest Model with Case Weights Summary")
  ## Return the summary
  rf_summary_weighted
}
