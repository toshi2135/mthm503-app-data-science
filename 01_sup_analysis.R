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

# Change categorical variables to factors
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
