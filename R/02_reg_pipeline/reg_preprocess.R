# 02_reg_pipeline/reg_preprocess.R

reg_preprocess_data <- function(fire_rescue_data) {
  # Data preprocessing
  library(dplyr)
  fire_rescue_clean <- fire_rescue_data %>%
    filter(
      !is.na(extrication),
      !is.na(sex),
      !is.na(age_band),
      !is.na(extrication_rate),
      extrication != "Unknown"
    ) %>%
    mutate(
      extrication = factor(extrication),
      sex = factor(sex),
      age_band = factor(age_band,
                        levels = c("0-16", "17-24", "25-39", "40-64", "65-74", "75+"),
                        ordered = TRUE
      )
    )
  str(fire_rescue_clean)
  ## Check the levels of the factors
  levels(fire_rescue_clean$extrication)
  levels(fire_rescue_clean$sex)
  levels(fire_rescue_clean$age_band)
  ## Remove rows with Unknown value in sex
  fire_rescue_clean <- fire_rescue_clean %>%
    filter(sex != "Unknown") %>%
    droplevels()
  ## Remove rows with NA value in age_band
  fire_rescue_clean <- fire_rescue_clean %>%
    filter(!is.na(age_band)) %>%
    droplevels()
  ## Check the structure of the cleaned data
  str(fire_rescue_clean)
  ## Plot the data
  library(ggplot2)
  ggplot(fire_rescue_clean, aes(x = age_band, fill = extrication)) +
    geom_bar(position = "fill") +
    labs(
      title = "Extrication Method by Age Band",
      x = "Age Band",
      y = "Proportion",
      fill = "Extrication Method"
    ) +
    theme_minimal()
  # ---
  fire_rescue_clean
}