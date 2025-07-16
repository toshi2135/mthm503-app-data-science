# 01_sup_pipeline/sup_preprocess.R
sup_preprocess_data <- function(sup_data) {
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
      junction_detail = fct_na_value_to_level(
        junction_detail,
        level = "Missing"
      ),
      sex_of_casualty = fct_na_value_to_level(
        sex_of_casualty,
        level = "Missing"
      )
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

  # Split the data into training and testing sets
  split <- initial_split(sup_data, strata = casualty_severity)
  train_data <- training(split)
  test_data <- testing(split)

  ## Return the preprocessed data
  list(
    sup_data = sup_data,
    train = train_data,
    test = test_data
  )
}
