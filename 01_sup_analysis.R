# Initial analysis for Supervised Classification Task
# Goal: Predict pedestrian crash severity from stats19 data

# Load packages
library(tidyverse)
library(tidymodels)
library(stats19)
library(janitor)
library(here)
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
sql_query <- readLines(here("01_sup_data_query.sql"))
query <- paste(sql_query, collapse = "\n")
sup_data <- dbGetQuery(conn, query)
## Check the data
names(sup_data)
glimpse(sup_data)
summary(sup_data)
## Close the connection
DBI::dbDisconnect(conn)
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
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))
## Drop columns with too many missing values
sup_data <- sup_data %>% select(-vehicle_manoeuvre)
## Impute missing data
### Impute `age_of_vehicle` with median
median_age_vehicle <- median(sup_data$age_of_vehicle, na.rm = TRUE)
sup_data <- sup_data %>%
  mutate(age_of_vehicle = if_else(is.na(age_of_vehicle), median_age_vehicle, age_of_vehicle))
### Impute categorical variables with Unknown
sup_data <- sup_data %>%
  mutate(
    casualty_home_area_type = fct_explicit_na(as.factor(casualty_home_area_type), na_level = "Missing"),
    junction_detail = fct_explicit_na(as.factor(junction_detail), na_level = "Missing"),
    sex_of_casualty = fct_explicit_na(as.factor(sex_of_casualty), na_level = "Missing")
  )
## Check the data again
glimpse(sup_data)
summary(sup_data)
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
## Check the data again
glimpse(sup_data)
## Remove `accident_index` column
sup_data <- sup_data %>% select(-accident_index)
## Split the data for train and test
split <- initial_split(sup_data, strata = casualty_severity)
train_data <- training(split)
count(train_data)
test_data <- testing(split)
count(test_data)
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
## Find the importance of the features
vip::vip(rf_fit, num_features = 10) +
  labs(title = "Feature Importance for Random Forest Model") +
  theme_minimal()
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
bind_rows(rf_summary, log_summary) %>%
  mutate(model = factor(model, levels = c("Random Forest", "Logistic Regression"))) %>%
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
# ---
# 