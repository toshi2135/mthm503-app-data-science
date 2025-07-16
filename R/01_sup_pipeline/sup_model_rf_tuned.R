# 01_sup_pipeline/sup_rf_tuned.R

sup_rf_tuned_fit <- function(train_data) {
  # Hyperparameter tuning for Random Forest model
  library(tune)
  ## Define the grid for hyperparameter tuning
  rf_grid <- grid_random(
    mtry(range = c(3, 15)),
    min_n(range = c(2, 20)),
    trees(range = c(100, 1000)),
    size = 20
  )
  ## Build the specification for hyperparameter tuning
  rf_spec_tune <- rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = tune(),
  ) %>%
    set_mode("classification") %>%
    set_engine("ranger", importance = "impurity")
  ## Build the recipe
  rf_rec <- recipe(casualty_severity ~ ., data = train_data)
  ## Build the workflow for hyperparameter tuning
  rf_wf_tune <- workflow() %>%
    add_recipe(rf_rec) %>%
    add_model(rf_spec_tune)
  ## Perform hyperparameter tuning using cross-validation
  set.seed(123)
  rf_tune_results <- rf_wf_tune %>%
    tune_grid(
      resamples = vfold_cv(train_data, v = 5, strata = casualty_severity),
      grid = rf_grid,
      metrics = metric_set(accuracy, f_meas, roc_auc, precision, recall)
    )
  ## Check the tuning results
  rf_tune_results %>%
    collect_metrics() %>%
    arrange(desc(mean)) %>%
    knitr::kable(
      caption = "Hyperparameter Tuning Results for Random Forest Model"
    )
  ## Select the best hyperparameters
  collect_metrics(rf_tune_results) %>% distinct(.metric)
  best_rf_params <- select_best(rf_tune_results, metric = "f_meas")
  best_rf_params # (mtry=3, trees=855, min_n=8)
  ## Finalize the workflow with the best hyperparameters
  rf_wf_final <- rf_wf_tune %>%
    finalize_workflow(best_rf_params)
  ## Fit the final model with the best hyperparameters
  rf_fit_final <- rf_wf_final %>%
    fit(data = train_data)
  ## Return the fitted model
  rf_fit_final
}

sup_rf_tuned_eval <- function(rf_fit_final, test_data) {
  # Evaluate the tuned Random Forest model
  ## Check the final model
  rf_preds_final <- predict(rf_fit_final, test_data, type = "prob") %>%
    bind_cols(predict(rf_fit_final, test_data)) %>%
    bind_cols(test_data)
  ## Check the ROC curve for the final model
  roc_curve(
    rf_preds_final,
    truth = casualty_severity,
    .pred_Slight,
    .pred_Serious,
    .pred_Fatal
  ) %>%
    autoplot() +
    labs(
      title = "ROC Curve for Final Random Forest Model",
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_minimal()
  ## Check the confusion matrix for the final model
  conf_mat(
    rf_preds_final,
    truth = casualty_severity,
    estimate = .pred_class
  ) %>%
    autoplot(type = "heatmap") +
    labs(
      title = "Confusion Matrix for Final Random Forest Model",
      x = "Predicted",
      y = "Actual"
    ) +
    theme_minimal()
  ## Check the accuracy, precision, recall, and F1 score for the final model
  rf_accuracy_final <- rf_preds_final %>%
    accuracy(truth = casualty_severity, estimate = .pred_class)
  rf_precision_final <- rf_preds_final %>%
    precision(truth = casualty_severity, estimate = .pred_class)
  rf_recall_final <- rf_preds_final %>%
    recall(truth = casualty_severity, estimate = .pred_class)
  rf_f1_final <- rf_preds_final %>%
    f_meas(truth = casualty_severity, estimate = .pred_class)
  ## Create a summary table for the final model
  rf_summary_final <- tibble(
    model = "Final Random Forest",
    accuracy = rf_accuracy_final$.estimate,
    precision = rf_precision_final$.estimate,
    recall = rf_recall_final$.estimate,
    f1_score = rf_f1_final$.estimate
  )
  rf_summary_final %>%
    knitr::kable(caption = "Final Random Forest Model Summary")
  ## Return the summary for the final model
  rf_summary_final
}
