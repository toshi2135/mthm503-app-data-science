library(targets)
library(tarchetypes)
library(here)

# Load custom functions and data loading script
source(here("R", "functions.R"))
source(here("R", "load_data.R"))
source(here("R", "utils.R"))

# Set global packages
tar_option_set(packages = c("dplyr", "DBI", "RPostgres"))

# Load sub-targets from the 01_sup_pipeline directory
sub_targets <- purrr::map(
  list.files(here("01_sup_pipeline"), pattern = "^sup_.*\\.R$", full.names = TRUE),
  source
)

# Define the targets pipeline
list(
  tar_target(
    raw_data,
    load_data()
  ),
  tar_target(
    summary_data,
    summarise_data(raw_data)
  ),
  tar_target(
    summary_casualty_sex,
    summarise_casualty_sex(raw_data)
  ),
  tar_render(
    report,
    "vignettes/Report.Rmd"
  ),
  is_CI <- Sys.getenv("CI") == "true",
  if (is_CI != "true") {
    # Production path
    sub_targets
    }
)
