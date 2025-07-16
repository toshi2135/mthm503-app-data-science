# 01_sup_pipeline/sup_load_data.R

source(here("R", "load_data.R"))

sup_load_data <- function() {
  # Data querying
  ## Use the .Renviron file to set the environment variables and connect to DB
  ## Read .Renviron file
  readRenviron(here(".Renviron"))
  ## Check if the environment variables are set
  Sys.getenv("PGRHOST")
  ## Connect to the database
  conn <- get_db_connection()
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
  sql_query <- readLines(here("sql", "01_sup_data_query.sql"))
  query <- paste(sql_query, collapse = "\n")
  sup_data <- dbGetQuery(conn, query)
  ## Check the data
  names(sup_data)
  glimpse(sup_data)
  summary(sup_data)
  ## Close the connection
  DBI::dbDisconnect(conn)
  ## Return the data
  sup_data
}
