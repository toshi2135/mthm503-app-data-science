load_data <- function() {
  is_ci <- Sys.getenv("CI") == "true"

  if (is_ci) {
    source(here::here("mock_data", "mock_df.R"))
    df <- mock_data() # nolint
  } else {
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("PGRDATABASE"),
      host = Sys.getenv("PGRHOST"),
      user = Sys.getenv("PGRUSER"),
      password = Sys.getenv("PGRPASSWORD"),
      port = Sys.getenv("PGRPORT")
    )
    sql_text <-
      "SELECT casualty_severity
       FROM dft.stats19_casualties"
    df <- DBI::dbGetQuery(con, sql_text)
    DBI::dbDisconnect(con)
  }
  df
}
