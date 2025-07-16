# Analyse the effect of casualty age and sex on the method of extrication
# used by the fire brigade.

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
## Check first few rows of fire_rescue_extrication_casualties table
fire_rescue <- DBI::dbReadTable(conn, "fire_rescue_extrication_casualties")
## Check the structure of the data
str(fire_rescue)
## Check the column names
colnames(fire_rescue)
## Check the first few rows
head(fire_rescue)
## Check stats19_by_financial_year table
stats19_by_financial_year <- DBI::dbReadTable(conn, "stats19_by_financial_year")
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
# ---
# Data preprocessing
library(dplyr)

fire_rescue_clean <- fire_rescue_data %>%
  filter(
    !is.na(extrication),
    !is.na(sex),
    !is.na(age_band),
    !is.na(extrication_rate),
    extrication != "Unknown"
  ) %>%
  mutate(
    extrication = factor(extrication),
    sex = factor(sex),
    age_band = factor(age_band,
      levels = c("0-16", "17-24", "25-39", "40-64", "65-74", "75+"),
      ordered = TRUE
    )
  )
str(fire_rescue_clean)
## Check the levels of the factors
levels(fire_rescue_clean$extrication)
levels(fire_rescue_clean$sex)
levels(fire_rescue_clean$age_band)
## Remove rows with Unknown value in sex
fire_rescue_clean <- fire_rescue_clean %>%
  filter(sex != "Unknown") %>%
  droplevels()
## Remove rows with NA value in age_band
fire_rescue_clean <- fire_rescue_clean %>%
  filter(!is.na(age_band)) %>%
  droplevels()
## Check the structure of the cleaned data
str(fire_rescue_clean)

## Plot the data
library(ggplot2)
ggplot(fire_rescue_clean, aes(x = age_band, fill = extrication)) +
  geom_bar(position = "fill") +
  labs(
    title = "Extrication Method by Age Band",
    x = "Age Band",
    y = "Proportion",
    fill = "Extrication Method"
  ) +
  theme_minimal()
# ---

# Statistical analysis using Multinomial GLM
library(nnet)
# Fit the Multinomial GLM
model_multi <- multinom(extrication ~ age_band + sex + age_band:sex,
  data = fire_rescue_clean
)
# Check the summary of the model
summary(model_multi)

library(broom)
# Tidy the model output
tidy(model_multi, conf.int = TRUE, exponentiate = TRUE)
