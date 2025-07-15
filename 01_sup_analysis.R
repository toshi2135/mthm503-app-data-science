# Initial analysis for Supervised Classification Task
# Goal: Predict pedestrian crash severity from stats19 data

# Load packages
library(tidyverse)
library(tidymodels)
library(stats19)
library(janitor)
library(here)

# Preprocessing data
## Import R scripts
# source(here("R/load_data.R"))

## Load pedestrian casualty data from supabase
# sup_data <- load_data()$sup

## For now, use the stats19 package to load the data
casualty_pedestrian <- get_stats19(
  2022, 
  type = "casualties", 
  file_name = "dft-road-casualty-statistics-casualty-2022.csv") %>%
  filter(casualty_type == "Pedestrian")
names(casualty_pedestrian)
accident <- get_stats19(
  2022,
  type = "accidents",
  file_name = "dft-road-casualty-statistics-collision-2022.csv")
names(accident)
vehicle <- get_stats19(
  2022,
  type = "vehicles",
  file_name = "dft-road-casualty-statistics-vehicle-2022.csv")
names(vehicle)
## Select the data
casualty_sel <- casualty_pedestrian %>%
  select(accident_index, casualty_severity, sex_of_casualty, age_of_casualty,
         casualty_type, casualty_class, casualty_home_area_type, casualty_imd_decile)
accident_sel <- accident %>%
  select(accident_index, weather_conditions, light_conditions, urban_or_rural_area,
         road_surface_conditions, speed_limit, day_of_week, time, junction_detail)
vehicle_sel <- vehicle %>%
  select(accident_index, sex_of_driver, age_of_driver, vehicle_type, engine_capacity_cc,
         vehicle_manoeuvre, skidding_and_overturning, age_of_vehicle) %>%
  group_by(accident_index) %>%
  slice(1) %>%
  ungroup()
## Join the data
sup_data <- casualty_sel %>%
  left_join(accident_sel, by = "accident_index") %>%
  left_join(vehicle_sel, by = "accident_index") %>%
  clean_names() %>%
  drop_na()
## Check data
glimpse(sup_data)
## Check the distribution of the target variable
sup_data %>%
  count(casualty_severity) %>%
  mutate(prop = n / sum(n))
## Check for missing values
sup_data %>% skimr::skim()
colSums(is.na(sup_data))
sup_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))
## Encode categorical variables to factors
sup_data <- sup_data %>%
  mutate(across(where(is.character), as.factor))
## Check the data again
glimpse(sup_data)
## Check the distribution of the target variable
sup_data %>%
  count(casualty_severity) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = casualty_severity, y = percentage)) +
  geom_col() +
  labs(title = "Distribution of Casualty Severity",
       x = "Casualty Severity",
       y = "Percentage") +
  theme_minimal()
## Split the data for train and test
split <- initial_split(sup_data, strata = casualty_severity)
train_data <- training(split)
test_data <- testing(split)

# ---
# Build Random Forest baseline model
library(ranger)
## Build the recipe
rf_rec <- recipe(casualty_severity ~ ., data = train_data)
## Build the model specification
rf_spec <- rand_forest(trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("classification")
## Build the workflow
rf_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec)
## Fit the model
rf_fit <- rf_wf %>%
  fit(data = train_data)
## Check the model
rf_preds <- predict(rf_fit, test_data, type = "prob") %>%
  bind_cols(predict(rf_fit, test_data)) %>%
  bind_cols(test_data)
## Check the ROC curve
rf_roc <- roc_curve(rf_preds, truth = casualty_severity,
                    .pred_Slight, .pred_Serious, .pred_Fatal)
autoplot(rf_roc)
## Check the confusion matrix
rf_conf_mat <- conf_mat(rf_preds, truth = casualty_severity, estimate = .pred_class)
rf_conf_mat %>%
  autoplot(type = "heatmap") +
  labs(title = "Confusion Matrix for Random Forest Model",
       x = "Predicted",
       y = "Actual") +
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

# ---
# Build Logistic Regression baseline model
library(nnet)
## Build the recipe
log_rec <- recipe(casualty_severity ~ ., data = train_data) %>%
  step_rm(accident_index, casualty_type, casualty_class, time) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())
## Build the model specification
log_spec <- multinom_reg() %>%
  set_engine("nnet", maxit = 50) %>%
  set_mode("classification")
## Build the workflow
log_wf <- workflow() %>%
  add_recipe(log_rec) %>%
  add_model(log_spec)
## Fit the model on small train
log_fit <- log_wf %>%
  fit(data = train_data)
## Check the model
log_preds <- predict(log_fit, test_data, type = "prob") %>%
  bind_cols(predict(log_fit, test_data)) %>%
  bind_cols(test_data)
## Check the ROC curve
log_roc <- roc_curve(log_preds, truth = casualty_severity,
                           .pred_Slight, .pred_Serious, .pred_Fatal)
autoplot(log_roc) +
  labs(title = "ROC Curve for Logistic Regression Model",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()
## Check the confusion matrix
log_conf_mat <- conf_mat(log_preds, truth = casualty_severity, estimate = .pred_class)
log_conf_mat %>%
  autoplot(type = "heatmap") +
  labs(title = "Confusion Matrix for Logistic Regression Model",
       x = "Predicted",
       y = "Actual") +
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
## Create a summary table with both models
model_comparison <- bind_rows(rf_summary, log_summary) %>%
  mutate(model = factor(model, levels = c("Random Forest", "Logistic Regression")))
## Show the comparison table
model_comparison %>%
  knitr::kable(caption = "Model Comparison Summary")

# ---
# Add cross-validation to Random Forest
set.seed(42)
rf_cv <- vfold_cv(train_data, v = 5, strata = casualty_severity)
rf_res <- rf_wf %>%
  fit_resamples(
    resamples = rf_cv,
    metrics = metric_set(accuracy, precision, recall, f_meas, roc_auc),
    control = control_resamples(save_pred = TRUE)
  )
# Check the results
rf_res %>%
  collect_metrics() %>%
  mutate(model = "Random Forest") %>%
  select(model, everything()) %>%
  knitr::kable(caption = "Random Forest 5-fold Cross-Validation Results")
# Plot the ROC curve
rf_roc_cv <- rf_res %>%
  collect_predictions() %>%
  roc_curve(truth = casualty_severity, .pred_Slight, .pred_Serious, .pred_Fatal)
autoplot(rf_roc_cv) +
  labs(title = "ROC Curve for Random Forest 5-fold Cross-Validation",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()
# Plot confusion matrix on every folds
rf_conf_mat_cv <- rf_res %>%
  collect_predictions() %>%
  conf_mat(truth = casualty_severity, estimate = .pred_class)
rf_conf_mat_cv %>%
  autoplot(type = "heatmap") +
  labs(title = "Confusion Matrix for Random Forest 5-fold Cross-Validation",
       x = "Predicted",
       y = "Actual") +
  theme_minimal()

# Apply case weights to Random Forest to handle class imbalance
## Calculate class weights
class_counts <- train_data %>%
  count(casualty_severity)
## Calculate weights as inverse of class frequencies
class_weights <- 1 / class_counts$n
## Normalize weights to sum to 1
names(class_weights) <- class_counts$casualty_severity
## Create a new column in the training data with the weights
train_data_weighted <- train_data %>%
  mutate(weight = class_weights[casualty_severity])

# Update the Random Forest model to use case weights
rf_spec_weighted <- rand_forest(trees = 500) %>%
  set_engine("ranger", case.weights = train_data_weighted$weight) %>%
  set_mode("classification")
rf_wf_weighted <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec_weighted)
# Fit the weighted model
rf_fit_weighted <- rf_wf_weighted %>%
  fit(data = train_data_weighted)
# Check the weighted model
rf_preds_weighted <- predict(rf_fit_weighted, test_data, type = "prob") %>%
  bind_cols(predict(rf_fit_weighted, test_data)) %>%
  bind_cols(test_data)
# Check the metrics for the weighted model
rf_metrics_weighted <- rf_preds_weighted %>%
  metrics(truth = casualty_severity, estimate = .pred_class)
rf_roc_weighted <- roc_curve(rf_preds_weighted, truth = casualty_severity,
                             .pred_Slight, .pred_Serious, .pred_Fatal)

autoplot(rf_roc_weighted) +
  labs(title = "ROC Curve for Weighted Random Forest Model",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()
# Check the confusion matrix for the weighted model
rf_conf_mat_weighted <- conf_mat(rf_preds_weighted, truth = casualty_severity, estimate = .pred_class)
rf_conf_mat_weighted %>%
  autoplot(type = "heatmap") +
  labs(title = "Confusion Matrix for Weighted Random Forest Model",
       x = "Predicted",
       y = "Actual") +
  theme_minimal()
# Check the accuracy, precision, recall, and F1 score for the weighted model
rf_accuracy_weighted <- rf_preds_weighted %>%
  accuracy(truth = casualty_severity, estimate = .pred_class)
rf_precision_weighted <- rf_preds_weighted %>%
  precision(truth = casualty_severity, estimate = .pred_class)
rf_recall_weighted <- rf_preds_weighted %>%
  recall(truth = casualty_severity, estimate = .pred_class)
rf_f1_weighted <- rf_preds_weighted %>%
  f_meas(truth = casualty_severity, estimate = .pred_class)
# Create a summary table for the weighted model
rf_summary_weighted <- tibble(
  model = "Weighted Random Forest",
  accuracy = rf_accuracy_weighted$.estimate,
  precision = rf_precision_weighted$.estimate,
  recall = rf_recall_weighted$.estimate,
  f1_score = rf_f1_weighted$.estimate
)
# Show the summary table
rf_summary_weighted %>%
  knitr::kable(caption = "Weighted Random Forest Model Summary")

# Compare the weighted model with the baseline models
model_comparison_weighted <- model_comparison %>%
  bind_rows(rf_summary_weighted) %>%
  mutate(model = factor(model, levels = c("Random Forest", "Logistic Regression", "Weighted Random Forest")))
# Plot the model comparison with the weighted model
model_comparison_weighted %>%
  pivot_longer(-model, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = model, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  labs(title = "Model Comparison with Weighted Random Forest",
       x = "Model",
       y = "Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Combine cross-validation with case weights
## Create new train data with case weights columns
class_weights <- train_data %>%
  count(casualty_severity) %>%
  mutate(weight = 1 / n) %>%
  select(casualty_severity, weight)
train_data_weighted <- train_data %>%
  left_join(class_weights, by = "casualty_severity")
## Create cross-validation with case weights
set.seed(42)
rf_cv_weighted <- vfold_cv(train_data_weighted, v = 5, strata = casualty_severity)
## Build the spec with case weights
rf_spec_weighted <- rand_forest(trees = 500) %>%
  set_engine("ranger", case.weights = TRUE) %>%
  set_mode("classification")
## Build the recipe with case weights
rf_rec_weighted <- recipe(casualty_severity ~ ., data = train_data_weighted)
## Build the workflow with case weights
rf_wf_weighted <- workflow() %>%
  add_recipe(rf_rec_weighted) %>%
  add_model(rf_spec_weighted)
# Fit the model with case weights
rf_res_weighted <- rf_wf_weighted %>%
  fit_resamples(
    resamples = rf_cv_weighted,
    metrics = metric_set(accuracy, precision, recall, f_meas, roc_auc),
    control = control_resamples(save_pred = TRUE)
  )
# Check the results of the cross-validation with case weights
rf_res_weighted %>%
  collect_metrics() %>%
  mutate(model = "Weighted Random Forest CV") %>%
  select(model, everything()) %>%
  knitr::kable(caption = "Weighted Random Forest 5-fold Cross-Validation Results")
# Plot the ROC curve for the cross-validation with case weights
rf_roc_weighted_cv <- rf_res_weighted %>%
  collect_predictions() %>%
  roc_curve(truth = casualty_severity, .pred_Slight, .pred_Serious, .pred_Fatal)
autoplot(rf_roc_weighted_cv) +
  labs(title = "ROC Curve for Weighted Random Forest 5-fold Cross-Validation",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()
# Plot confusion matrix on every folds for the cross-validation with case weights
rf_conf_mat_weighted_cv <- rf_res_weighted %>%
  collect_predictions() %>%
  conf_mat(truth = casualty_severity, estimate = .pred_class)
rf_conf_mat_weighted_cv %>%
  autoplot(type = "heatmap") +
  labs(title = "Confusion Matrix for Weighted Random Forest 5-fold Cross-Validation",
       x = "Predicted",
       y = "Actual") +
  theme_minimal()

# Fine-tuning the model
# Use the `tune` package to fine-tune the Random Forest model
library(tune)
# Define a grid of hyperparameters to tune
rf_grid <- grid_regular(
  trees(range = c(100, 1000)),
  min_n(range = c(1, 10)),
  mtry(range = c(1, ncol(train_data) - 1)),
  levels = 5
)
# Create a new workflow with the Random Forest model
rf_wf_tune <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec)
# Perform hyperparameter tuning using cross-validation
rf_tune_res <- rf_wf_tune %>%
  tune_grid(
    resamples = rf_cv_weighted,
    grid = rf_grid,
    metrics = metric_set(accuracy, precision, recall, f_meas, roc_auc),
    control = control_grid(save_pred = TRUE)
  )
# Check the results of the hyperparameter tuning
rf_tune_res %>%
  collect_metrics()
rf_tune_res %>%
  collect_metrics() %>%
  filter(.metric == "f_meas") %>%
  arrange(desc(mean))
# Plot the results of the hyperparameter tuning
rf_tune_res %>%
  collect_predictions() %>%
  roc_curve(truth = casualty_severity, .pred_Slight, .pred_Serious, .pred_Fatal) %>%
  autoplot() +
  labs(title = "ROC Curve for Tuned Random Forest Model",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()
# Get the best hyperparameters
best_rf_params <- rf_tune_res %>%
  select_best(metric = "f_meas")
# Finalize the workflow with the best hyperparameters
rf_wf_final <- rf_wf_tune %>%
  finalize_workflow(best_rf_params)
# Fit the final model on the training data
rf_fit_final <- rf_wf_final %>%
  fit(data = train_data_weighted)
# Check the final model
rf_preds_final <- predict(rf_fit_final, test_data, type = "prob") %>%
  bind_cols(predict(rf_fit_final, test_data)) %>%
  bind_cols(test_data)
# Check the metrics for the final model
rf_metrics_final <- rf_preds_final %>%
  metrics(truth = casualty_severity, estimate = .pred_class)
rf_roc_final <- roc_curve(rf_preds_final, truth = casualty_severity,
                          .pred_Slight, .pred_Serious, .pred_Fatal)
autoplot(rf_roc_final) +
  labs(title = "ROC Curve for Final Tuned Random Forest Model",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()
# Check the confusion matrix for the final model
rf_conf_mat_final <- conf_mat(rf_preds_final, truth = casualty_severity, estimate = .pred_class)
rf_conf_mat_final %>%
  autoplot(type = "heatmap") +
  labs(title = "Confusion Matrix for Final Tuned Random Forest Model",
       x = "Predicted",
       y = "Actual") +
  theme_minimal()
# Check the accuracy, precision, recall, and F1 score for the final model
rf_accuracy_final <- rf_preds_final %>%
  accuracy(truth = casualty_severity, estimate = .pred_class)
rf_precision_final <- rf_preds_final %>%
  precision(truth = casualty_severity, estimate = .pred_class)
rf_recall_final <- rf_preds_final %>%
  recall(truth = casualty_severity, estimate = .pred_class)
rf_f1_final <- rf_preds_final %>%
  f_meas(truth = casualty_severity, estimate = .pred_class)
# Create a summary table for the final model
rf_summary_final <- tibble(
  model = "Final Tuned Random Forest",
  accuracy = rf_accuracy_final$.estimate,
  precision = rf_precision_final$.estimate,
  recall = rf_recall_final$.estimate,
  f1_score = rf_f1_final$.estimate
)
# Show the summary table for the final model
rf_summary_final %>%
  knitr::kable(caption = "Final Tuned Random Forest Model Summary")
# Compare the final model with the baseline models
model_comparison_final <- model_comparison_weighted %>%
  bind_rows(rf_summary_final) %>%
  mutate(model = factor(model, levels = c("Random Forest", "Logistic Regression", "Weighted Random Forest", "Final Tuned Random Forest")))
# Plot the model comparison with the final model
model_comparison_final %>%
  pivot_longer(-model, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = model, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  labs(title = "Model Comparison with Final Tuned Random Forest",
       x = "Model",
       y = "Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Apply SMOTE to handle class imbalance using `themis`
library(themis)
## Create a recipe with SMOTE
rf_rec_smote <- recipe(casualty_severity ~ ., data = train_data_new) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_smote(casualty_severity)
## Create the model
rf_spec_smote <- rand_forest(trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("classification")
## Create the workflow with SMOTE
rf_wf_smote <- workflow() %>%
  add_recipe(rf_rec_smote) %>%
  add_model(rf_spec_smote)
## Fit the model with SMOTE
rf_fit_smote <- rf_wf_smote %>%
  fit(data = train_data_new)
## Check the model with SMOTE
rf_preds_smote <- predict(rf_fit_smote, test_data_new, type = "prob") %>%
  bind_cols(predict(rf_fit_smote, test_data_new)) %>%
  bind_cols(test_data_new)
## Check the metrics with SMOTE
rf_metrics_smote <- rf_preds_smote %>%
  metrics(truth = casualty_severity, estimate = .pred_class)
rf_roc_smote <- roc_curve(rf_preds_smote, truth = casualty_severity,
                             .pred_Slight, .pred_Serious, .pred_Fatal)
autoplot(rf_roc_smote) +
  labs(title = "ROC Curve for Random Forest Model with SMOTE",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()
## Check the confusion matrix with SMOTE
rf_conf_mat_smote <- conf_mat(rf_preds_smote, truth = casualty_severity, estimate = .pred_class)
rf_conf_mat_smote %>%
  autoplot(type = "heatmap") +
  labs(title = "Confusion Matrix for Random Forest Model with SMOTE",
       x = "Predicted",
       y = "Actual") +
  theme_minimal()
## Check the accuracy, precision, recall, and F1 score with SMOTE
rf_accuracy_smote <- rf_preds_smote %>%
  accuracy(truth = casualty_severity, estimate = .pred_class)
rf_precision_smote <- rf_preds_smote %>%
  precision(truth = casualty_severity, estimate = .pred_class)
rf_recall_smote <- rf_preds_smote %>%
  recall(truth = casualty_severity, estimate = .pred_class)
rf_f1_smote <- rf_preds_smote %>%
  f_meas(truth = casualty_severity, estimate = .pred_class)
## Create a summary table for the model with SMOTE
rf_summary_smote <- tibble(
  model = "Random Forest with SMOTE",
  accuracy = rf_accuracy_smote$.estimate,
  precision = rf_precision_smote$.estimate,
  recall = rf_recall_smote$.estimate,
  f1_score = rf_f1_smote$.estimate
)
## Show the summary table for the model with SMOTE
rf_summary_smote %>%
  knitr::kable(caption = "Random Forest Model with SMOTE Summary")
## Analyse the accuracy, precision, recall and F1 score on each class with SMOTE
rf_preds_smote %>%
  group_by(casualty_severity) %>%
  summarise(
    precision = precision_vec(casualty_severity, .pred_class),
    recall = recall_vec(casualty_severity, .pred_class),
    f1_score = f_meas_vec(casualty_severity, .pred_class)
  )
## Compare the model with SMOTE with the previous models
model_comparison_smote <- model_comparison_new %>%
  bind_rows(rf_summary_smote) %>%
  mutate(model = factor(model, levels = c("Random Forest", "Logistic Regression", "Weighted Random Forest", "Final Tuned Random Forest", "Random Forest with New Features", "Random Forest with SMOTE")))
## Show the comparison
model_comparison_smote %>%
  pivot_longer(-model, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = model, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  labs(title = "Model Comparison with SMOTE",
       x = "Model",
       y = "Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
