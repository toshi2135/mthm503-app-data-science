# 02_reg_pipeline/reg_load_data.R

source(here("R", "load_data.R"))

reg_load_data <- function() {
  # Data querying
  ## Use the .Renviron file to set the environment variables and connect to DB
  ## Read .Renviron file
  readRenviron(here(".Renviron"))
  ## Check if the environment variables are set
  Sys.getenv("PGRHOST")
  ## Connect to the database using the load_data function
  conn <- get_db_connection()
  ## Check the tables in the database
  tables <- DBI::dbListTables(conn)
  tables
  ## Check first few rows of fire_rescue_extrication_casualties table
  fire_rescue <- DBI::dbReadTable(conn, "fire_rescue_extrication_casualties")
  ## Check the structure of the data
  str(fire_rescue)
  ## Check the column names
  colnames(fire_rescue)
  ## Check the first few rows
  head(fire_rescue)
  ## Check stats19_by_financial_year table
  stats19_by_financial_year <- DBI::dbReadTable(
    conn,
    "stats19_by_financial_year"
  )
  ## Check the structure of the data
  str(stats19_by_financial_year)
  ## Check the column names
  colnames(stats19_by_financial_year)
  ## Read SQL query to join the table
  sql_query <- readLines(here("sql", "02_reg_data_query.sql"))
  query <- paste(sql_query, collapse = "\n")
  fire_rescue_data <- DBI::dbGetQuery(conn, query)
  ## Check the structure of the data
  str(fire_rescue_data)
  ## Close the connection
  DBI::dbDisconnect(conn)
  ## Return the data
  fire_rescue_data
}
