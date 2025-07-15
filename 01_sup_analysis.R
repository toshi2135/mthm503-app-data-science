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
### Select relevant columns from casualty_pedestrian
casualty_sel <- casualty_pedestrian %>%
  select(accident_index, casualty_severity, sex_of_casualty, age_of_casualty)
### Check the data
glimpse(casualty_sel)
### Select relevant columns from accident
accident_sel <- accident %>%
  select(accident_index, weather_conditions, light_conditions, 
         urban_or_rural_area, day_of_week, date, time)
### Check the data
glimpse(accident_sel)
### Select relevant columns from vehicle
vehicle_sel <- vehicle %>%
  select(accident_index, sex_of_driver, age_of_driver) %>%
  group_by(accident_index) %>%
  slice(1) %>%
  ungroup()
### Check the data
glimpse(vehicle_sel)
## Join the data
sup_data <- casualty_sel %>%
  left_join(accident_sel, by = "accident_index") %>%
  left_join(vehicle_sel, by = "accident_index") %>%
  clean_names()
## Check the data
glimpse(sup_data)
## Convert time to hour
library(lubridate)
sup_data <- sup_data %>%
  mutate(hour = hour(hm(time))) %>%
  select(-time)
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
## Impute missing data
### Calculate median for numeric variables
median_age_driver <- median(sup_data$age_of_driver, na.rm = TRUE)
median_age_casualty <- median(sup_data$age_of_casualty, na.rm = TRUE)
### Impute missing values
sup_data <- sup_data %>%
  mutate(
    age_of_driver = if_else(is.na(age_of_driver), median_age_driver, age_of_driver),
    age_of_casualty = if_else(is.na(age_of_casualty), median_age_casualty, age_of_casualty)
  )
## Check the missing values again
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
roc_curve(rf_preds, truth = casualty_severity, .pred_Slight, .pred_Serious, .pred_Fatal) %>%
  autoplot() +
  labs(title = "ROC Curve for Random Forest Model",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()
## Check the confusion matrix
conf_mat(rf_preds, truth = casualty_severity, estimate = .pred_class) %>%
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
log_rec <- recipe(casualty_severity ~ ., data = sup_data) %>%
  step_rm(accident_index) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())
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
## Check the model
log_preds <- predict(log_fit, test_data, type = "prob") %>%
  bind_cols(predict(log_fit, test_data)) %>%
  bind_cols(test_data)
## Check the ROC curve
roc_curve(log_preds, truth = casualty_severity, .pred_Slight, .pred_Serious, .pred_Fatal) %>%
  autoplot() +
  labs(title = "ROC Curve for Logistic Regression Model",
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()
## Check the confusion matrix
conf_mat(log_preds, truth = casualty_severity, estimate = .pred_class) %>%
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
## Plot the model comparison
model_comparison %>%
  pivot_longer(-model, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = model, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  labs(title = "Model Comparison",
       x = "Model",
       y = "Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
