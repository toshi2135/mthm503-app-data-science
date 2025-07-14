# Initial analysis for Supervised Classification Task
# Goal: Predict pedestrian crash severity from stats19 data

# Load packages
library(tidyverse)
library(tidymodels)
library(stats19)
library(janitor)
library(here)

# Import R scripts
# source(here("R/load_data.R"))

# Load pedestrian casualty data from supabase
# sup_data <- load_data()$sup

# For now, use the stats19 package to load the data
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

# Select the data
casualty_sel <- casualty_pedestrian %>%
  select(accident_index, casualty_severity, sex_of_casualty, age_of_casualty)
accident_sel <- accident %>%
  select(accident_index, weather_conditions, light_conditions, urban_or_rural_area)
vehicle_sel <- vehicle %>%
  select(accident_index, sex_of_driver, age_of_driver) %>%
  group_by(accident_index) %>%
  slice(1) %>%
  ungroup()

# Join the data
sup_data <- casualty_sel %>%
  left_join(accident_sel, by = "accident_index") %>%
  left_join(vehicle_sel, by = "accident_index") %>%
  clean_names() %>%
  drop_na()

# Check data
glimpse(sup_data)
sup_data %>% count(casualty_severity)

# Check for missing values
sup_data %>% skimr::skim()
colSums(is.na(sup_data))
sup_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))

# Encode categorical variables to factors
sup_data <- sup_data %>%
  mutate(across(
    c(casualty_severity, sex_of_casualty, sex_of_driver,
      weather_conditions, light_conditions, urban_or_rural_area),
    as.factor
  ))
# Check the data again
glimpse(sup_data)
# Check the distribution of the target variable
sup_data %>%
  count(casualty_severity) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = casualty_severity, y = percentage)) +
  geom_col() +
  labs(title = "Distribution of Casualty Severity",
       x = "Casualty Severity",
       y = "Percentage") +
  theme_minimal()

# Split the data for train and test
split <- initial_split(sup_data, strata = casualty_severity)
train_data <- training(split)
test_data <- testing(split)
# Check the split
train_data %>%
  count(casualty_severity) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = casualty_severity, y = percentage)) +
  geom_col() +
  labs(title = "Distribution of Casualty Severity in Training Data",
       x = "Casualty Severity",
       y = "Percentage") +
  theme_minimal()
test_data %>%
  count(casualty_severity) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = casualty_severity, y = percentage)) +
  geom_col() +
  labs(title = "Distribution of Casualty Severity in Test Data",
       x = "Casualty Severity",
       y = "Percentage") +
  theme_minimal()

# Build Random Forest baseline model
library(ranger)
rf_rec <- recipe(casualty_severity ~ ., data = train_data)
rf_spec <- rand_forest(trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("classification")
rf_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec)
# Fit the model
rf_fit <- rf_wf %>%
  fit(data = train_data)

# Check the model
rf_preds <- predict(rf_fit, test_data, type = "prob") %>%
  bind_cols(predict(rf_fit, test_data)) %>%
  bind_cols(test_data)
rf_metrics <- rf_preds %>%
  metrics(truth = casualty_severity, estimate = .pred_class)
rf_roc <- roc_curve(rf_preds, truth = casualty_severity,
                    .pred_Slight, .pred_Serious, .pred_Fatal)
autoplot(rf_roc)
# Check the confusion matrix
rf_conf_mat <- conf_mat(rf_preds, truth = casualty_severity, estimate = .pred_class)
rf_conf_mat %>%
  autoplot(type = "heatmap") +
  labs(title = "Confusion Matrix for Random Forest Model",
       x = "Predicted",
       y = "Actual") +
  theme_minimal()
# Check the accuracy
rf_accuracy <- rf_preds %>%
  accuracy(truth = casualty_severity, estimate = .pred_class)
rf_accuracy %>%
  mutate(accuracy = round(.estimate, 3)) %>%
  select(-.estimate) %>%
  knitr::kable(caption = "Random Forest Model Accuracy")
# Check the precision
rf_precision <- rf_preds %>%
  precision(truth = casualty_severity, estimate = .pred_class)
rf_precision %>%
  mutate(precision = round(.estimate, 3)) %>%
  select(-.estimate) %>%
  knitr::kable(caption = "Random Forest Model Precision")
# Check the recall
rf_recall <- rf_preds %>%
  recall(truth = casualty_severity, estimate = .pred_class)
rf_recall %>%
  mutate(recall = round(.estimate, 3)) %>%
  select(-.estimate) %>%
  knitr::kable(caption = "Random Forest Model Recall")
# Check the F1 score
rf_f1 <- rf_preds %>%
  f_meas(truth = casualty_severity, estimate = .pred_class)
rf_f1 %>%
  mutate(f1 = round(.estimate, 3)) %>%
  select(-.estimate) %>%
  knitr::kable(caption = "Random Forest Model F1 Score")

# Build Logistic Regression baseline model
log_spec <- multinom_reg() %>%
  set_engine("nnet") %>%
  set_mode("classification")
log_rec <- recipe(casualty_severity ~ ., data = train_data)
log_wf <- workflow() %>%
  add_recipe(log_rec) %>%
  add_model(log_spec)
# Small train data for faster fitting
small_train <- train_data %>% slice_sample(n = 1000)
# Fit the model on small train
log_fit_small <- log_wf %>%
  fit(data = small_train)
# Check levels
library(forcats)
fct_count(train_data$weather_conditions)
fct_count(train_data$light_conditions)
fct_count(train_data$urban_or_rural_area)
# Lower the levels of the categorical variables
train_data_small <- small_train %>%
  mutate(
    weather_conditions = fct_lump_n(weather_conditions, n = 5),
    light_conditions = fct_lump_n(light_conditions, n = 3),
    urban_or_rural_area = fct_lump_n(urban_or_rural_area, n = 2),
    sex_of_casualty = fct_lump_n(sex_of_casualty, n = 2),
    sex_of_driver = fct_lump_n(sex_of_driver, n = 2)
  )
log_rec <- recipe(casualty_severity ~ ., data = train_data_small)
# Fit the model on the small train data
log_fit <- workflow() %>%
  add_recipe(log_rec) %>%
  add_model(log_spec) %>%
  fit(data = train_data_small)
# Drop the columns for smaller data
log_data_small <- train_data_small %>%
  select(
    casualty_severity,
    sex_of_casualty,
    age_of_casualty,
    light_conditions,
    age_of_driver
  )
log_rec <- recipe(casualty_severity ~ ., data = log_data_small) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors())
# Fit the model on the small train data
log_fit <- workflow() %>%
  add_recipe(log_rec) %>%
  add_model(log_spec) %>%
  fit(data = log_data_small)
# Check the model
log_preds <- predict(log_fit, test_data, type = "prob") %>%
  bind_cols(predict(log_fit, test_data)) %>%
  bind_cols(test_data)
# Check the metrics
log_metrics <- log_preds %>%
  metrics(truth = casualty_severity, estimate = .pred_class)
log_roc <- roc_curve(log_preds, truth = casualty_severity,
                     .pred_Slight, .pred_Serious, .pred_Fatal)
autoplot(log_roc)
# Check the confusion matrix
log_conf_mat <- conf_mat(log_preds, truth = casualty_severity, estimate = .pred_class)
log_conf_mat %>%
  autoplot(type = "heatmap") +
  labs(title = "Confusion Matrix for Logistic Regression Model",
       x = "Predicted",
       y = "Actual") +
  theme_minimal()
# Check the accuracy
log_accuracy <- log_preds %>%
  accuracy(truth = casualty_severity, estimate = .pred_class)
log_accuracy %>%
  mutate(accuracy = round(.estimate, 3)) %>%
  select(-.estimate) %>%
  knitr::kable(caption = "Logistic Regression Model Accuracy")
# Check the precision
log_precision <- log_preds %>%
  precision(truth = casualty_severity, estimate = .pred_class)
log_precision %>%
  mutate(precision = round(.estimate, 3)) %>%
  select(-.estimate) %>%
  knitr::kable(caption = "Logistic Regression Model Precision")
# Check the recall
log_recall <- log_preds %>%
  recall(truth = casualty_severity, estimate = .pred_class)
log_recall %>%
  mutate(recall = round(.estimate, 3)) %>%
  select(-.estimate) %>%
  knitr::kable(caption = "Logistic Regression Model Recall")
# Check the F1 score
log_f1 <- log_preds %>%
  f_meas(truth = casualty_severity, estimate = .pred_class)
log_f1 %>%
  mutate(f1 = round(.estimate, 3)) %>%
  select(-.estimate) %>%
  knitr::kable(caption = "Logistic Regression Model F1 Score")

# Compare the models
model_comparison <- tibble(
  model = c("Random Forest", "Logistic Regression"),
  accuracy = c(rf_accuracy$.estimate, log_accuracy$.estimate),
  precision = c(rf_precision$.estimate, log_precision$.estimate),
  recall = c(rf_recall$.estimate, log_recall$.estimate),
  f1_score = c(rf_f1$.estimate, log_f1$.estimate)
)
# Plot the model comparison
model_comparison %>%
  pivot_longer(-model, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = model, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  labs(title = "Model Comparison",
       x = "Model",
       y = "Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

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
rf_rec <- recipe(casualty_severity ~ ., data = train_data_weighted) %>%
  update_role(weight, new_role = "case_weights")
## Build the workflow with case weights
rf_wf_weighted <- workflow() %>%
  add_recipe(rf_rec) %>%
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
