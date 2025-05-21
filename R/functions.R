summarise_data <- function(df) {
  df %>%
    dplyr::group_by(casualty_severity) %>%
    dplyr::summarise(Casualties = n())
}
