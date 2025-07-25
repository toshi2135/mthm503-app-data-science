# Initial analysis for Supervised Classification Task
# Goal: Predict pedestrian crash severity from stats19 data

# Load packages
library(tidyverse)
library(tidymodels)
library(here)
library(DBI)
library(RPostgres)
# ---

# ---
# Data querying
## Use the .Renviron file to set the environment variables and connect to DB
## Read .Renviron file
readRenviron(".Renviron")
## Check if the environment variables are set
Sys.getenv("PGRHOST")
## Connect to the database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("PGRDATABASE"),
  host = Sys.getenv("PGRHOST"),
  user = Sys.getenv("PGRUSER"),
  password = Sys.getenv("PGRPASSWORD"),
  port = Sys.getenv("PGRPORT")
)
## Check the connection
DBI::dbIsValid(conn)
## Check the tables in the database
tables <- DBI::dbListTables(conn)
tables
## Check the first few rows of the casualty_pedestrian table
casualty_pedestrian <- DBI::dbReadTable(conn, "stats19_casualties")
names(casualty_pedestrian)
glimpse(casualty_pedestrian)
## Check the first few rows of the accident table
accident <- DBI::dbReadTable(conn, "stats19_accidents")
names(accident)
glimpse(accident)
## Check the first few rows of the vehicle table
vehicle <- DBI::dbReadTable(conn, "stats19_vehicles")
names(vehicle)
glimpse(vehicle)
## Read SQL query to join the tables
sql_query <- readLines(here("sql", "01_sup_data_query.sql"))
query <- paste(sql_query, collapse = "\n")
sup_data <- dbGetQuery(conn, query)
## Check the data
names(sup_data)
glimpse(sup_data)
summary(sup_data)
## Close the connection
DBI::dbDisconnect(conn)
# ---

# ---
# Data Preprocessing
## Drop `obs_date` column
sup_data <- sup_data %>% select(-obs_date)
## Ensure the target variable is a factor
sup_data$casualty_severity <- as.factor(sup_data$casualty_severity)
## Check the distribution of the target variable
sup_data %>%
  count(casualty_severity) %>%
  mutate(prop = n / sum(n))
## Check for missing values
sup_data %>% skimr::skim()
colSums(is.na(sup_data))
sup_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))
## Impute missing data
### Impute NA features
library(forcats)
library(dplyr)
sup_data <- sup_data %>%
  mutate(
    vehicle_manoeuvre = fct_na_value_to_level(
      vehicle_manoeuvre,
      level = "Missing"
    ),
    driver_home_area_type = fct_na_value_to_level(
      driver_home_area_type,
      level = "Missing"
    ),
    casualty_home_area_type = fct_na_value_to_level(
      casualty_home_area_type,
      level = "Missing"
    ),
    pedestrian_crossing_physical_facilities = fct_na_value_to_level(
      pedestrian_crossing_physical_facilities,
      level = "Missing"
    ),
    carriageway_hazards = fct_na_value_to_level(
      carriageway_hazards,
      level = "Missing"
    ),
    road_surface_conditions = fct_na_value_to_level(
      road_surface_conditions,
      level = "Missing"
    ),
    junction_detail = fct_na_value_to_level(junction_detail, level = "Missing"),
    sex_of_casualty = fct_na_value_to_level(sex_of_casualty, level = "Missing")
  )
### Impute `age_of_vehicle` with median
median_age_vehicle <- median(sup_data$age_of_vehicle, na.rm = TRUE)
sup_data <- sup_data %>%
  mutate(
    age_of_vehicle = if_else(
      is.na(age_of_vehicle),
      median_age_vehicle,
      age_of_vehicle
    )
  )
## Check the data again
glimpse(sup_data)
summary(sup_data)
sup_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "missing") %>%
  filter(missing > 0)
## Encode categorical variables to factors
sup_data <- sup_data %>%
  mutate(
    pedestrian_movement = as.factor(pedestrian_movement),
    weather_conditions = as.factor(weather_conditions),
    light_conditions = as.factor(light_conditions),
    urban_or_rural_area = as.factor(urban_or_rural_area),
    road_type = as.factor(road_type),
    sex_of_driver = as.factor(sex_of_driver),
    journey_purpose_of_driver = as.factor(journey_purpose_of_driver),
    vehicle_type = as.factor(vehicle_type),
    hour_of_day = as.factor(hour_of_day),
    day_of_week = as.factor(day_of_week),
    is_weekend = as.factor(is_weekend)
  )
sup_data <- sup_data %>%
  mutate(
    day_of_week = factor(
      day_of_week,
      levels = 0:6,
      labels = c(
        "Sunday",
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday"
      )
    ),
    is_weekend = factor(
      is_weekend,
      levels = c(0, 1),
      labels = c("Weekday", "Weekend")
    ),
    casualty_type = factor(casualty_type),
    vehicle_type = factor(vehicle_type)
  )
## Check the data again
glimpse(sup_data)
sup_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "missing") %>%
  filter(missing > 0)
## Remove `accident_index` column
sup_data <- sup_data %>% select(-accident_index)
## Remove `casualty_type` column
sup_data <- sup_data %>% select(-casualty_type)
## Split the data for train and test
split <- initial_split(sup_data, strata = casualty_severity)
train_data <- training(split)
count(train_data)
test_data <- testing(split)
count(test_data)
# ---

# ---
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

# Evaluate the model
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
# ---

# ---
# Build Logistic Regression baseline model
library(nnet)
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
## Create a summary table with both models
bind_rows(rf_summary, log_summary) %>%
  mutate(
    model = factor(model, levels = c("Random Forest", "Logistic Regression"))
  ) %>%
  knitr::kable(caption = "Model Comparison Summary")
# ---

# ---
# Apply case weights to the Random Forest model
## Calculate case weights based on the target variable distribution
class_weights <- sup_data %>%
  count(casualty_severity) %>%
  mutate(weight = 1 / n) %>%
  select(casualty_severity, weight)
## Join the case weights to the training data
sup_data_weighted <- sup_data %>%
  left_join(class_weights, by = "casualty_severity")
## Split the weighted data for train and test
split_weighted <- initial_split(sup_data_weighted, strata = casualty_severity)
train_data_weighted <- training(split_weighted)
count(train_data_weighted)
test_data_weighted <- testing(split_weighted)
count(test_data_weighted)
## Build the recipe
rf_rec_weighted <- recipe(casualty_severity ~ ., data = train_data_weighted) %>%
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
## Create a summary table with all models
bind_rows(rf_summary, log_summary, rf_summary_weighted) %>%
  mutate(
    model = factor(
      model,
      levels = c(
        "Random Forest",
        "Logistic Regression",
        "Random Forest with Case Weights"
      )
    )
  ) %>%
  knitr::kable(caption = "Model Comparison Summary with Case Weights")
# ---

# ---
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
conf_mat(rf_preds_final, truth = casualty_severity, estimate = .pred_class) %>%
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
# ---

# Create a summary table with all models
sup_summary <- bind_rows(
  rf_summary,
  log_summary,
  rf_summary_weighted,
  rf_summary_final
) %>%
  mutate(
    model = factor(
      model,
      levels = c(
        "Random Forest",
        "Logistic Regression",
        "Random Forest with Case Weights",
        "Final Random Forest"
      )
    )
  ) %>%
  knitr::kable(
    caption = "Model Comparison Summary"
  )
sup_summary
# ---
