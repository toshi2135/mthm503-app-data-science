library(targets)
library(tarchetypes)
library(here)

# Load custom functions and data loading script
source(here("R", "functions.R"))
source(here("R", "load_data.R"))
source(here("R", "utils.R"))

# Set global packages
tar_option_set(packages = c("dplyr", "DBI", "RPostgres"))

# Detect CI environment
is_CI <- Sys.getenv("CI", unset = "false") == "true"

# Load supervised targets only if not in CI
if (!is_CI) {
  message("✅ Loading supervised learning pipeline")
  source(here("01_sup_pipeline", "sup_targets.R"))  # defines sup_targets
} else {
  message("⏭️ Skipping supervised pipeline for CI")
  sup_targets <- list()  # fallback
}

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
  sup_targets
)
