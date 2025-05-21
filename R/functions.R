library(dplyr)
utils::globalVariables(c("casualty_severity"))

summarise_data <- function(df) {
  df %>%
    dplyr::mutate(casualty_severity = as.factor(casualty_severity)) %>% # nolint
    dplyr::group_by(casualty_severity) %>%
    dplyr::summarise(Casualties = n())
}
