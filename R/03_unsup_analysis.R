# Analyse the adulteration of olive oil using unsupervised methods

# Load libraries

# Load data
# Load libraries
library(DBI)
library(RPostgres)
library(stats19)
# Load load_data function
source("R/load_data.R")
# ---
# Data querying
## Use the .Renviron file to set the environment variables and connect to DB
## Read .Renviron file
readRenviron(".Renviron")
## Check if the environment variables are set
Sys.getenv("PGRHOST")
## Connect to the database using the load_data function
conn <- get_db_connection()
## Check the tables in the database
tables <- DBI::dbListTables(conn)
tables
## Check first few rows of olive_oil table
olive_oil <- DBI::dbReadTable(conn, "olive_oil")
## Check the structure of the data
str(olive_oil)
## Glimpse the first few rows
dplyr::glimpse(olive_oil)
## Check the summary of the data
summary(olive_oil)
