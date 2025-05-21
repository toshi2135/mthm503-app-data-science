library(targets)
library(tarchetypes)
library(here)

source(here("R", "functions.R"))
source(here("R", "load_data.R"))
source(here("R", "utils.R"))


tar_option_set(packages = c("dplyr", "DBI", "RPostgres"))

list(
  tar_target(
    raw_data,
    load_data()
  ),
  tar_target(
    summary_data,
    summarise_data(raw_data)
  ),
  tar_render(
    report,
    "vignettes/Report.Rmd"
  )
)
