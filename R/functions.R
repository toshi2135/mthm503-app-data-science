library(dplyr)
utils::globalVariables(c("casualty_severity"))

summarise_data <- function(df) {
  df %>%
    dplyr::group_by(casualty_severity) %>% # nolint
    dplyr::summarise(Casualties = n())
}
