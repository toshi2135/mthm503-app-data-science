load_data <- function() {
    print(Sys.getenv("PGRDATABASE"))
  is_ci <- Sys.getenv("CI") == "true"

  if (is_ci) {
    source(here::here("mock_data", "mock_df.R"))
    df <- mock_data()
  } else {
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("PGRDATABASE"),
      host = Sys.getenv("PGRHOST"),
      user = Sys.getenv("PGRUSER"),
      password = Sys.getenv("PGRPASSWORD"),
      port = Sys.getenv("PGRPORT")
    )
    df <- DBI::dbGetQuery(con, "SELECT casualty_severity FROM dft.stats19_casualties")
    DBI::dbDisconnect(con)
    }

  return(df)
}

