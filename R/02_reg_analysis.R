# Analyse the effect of casualty age and sex on the method of extrication
# used by the fire brigade.

# Load libraries
library(DBI)
library(RPostgres)
library(here)
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
    age_band = factor(
      age_band,
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
## Fit the Multinomial GLM
model_multi <- nnet::multinom(
  extrication ~ age_band + sex + age_band:sex,
  data = fire_rescue_clean
)
## Check the summary of the model
summary(model_multi)
library(broom)
## Tidy the model output
tidy(model_multi, conf.int = TRUE, exponentiate = TRUE)
## Check the model coefficients
coef(model_multi)
## Check the model residuals
residuals(model_multi)
# ---

# Upgrade the model, use age_band as nominal factor instead of ordered factor
## Change age_band to nominal factor
fire_rescue_clean$age_band <- factor(
  fire_rescue_clean$age_band,
  ordered = FALSE
)
## Fit the Multinomial GLM again
library(nnet)
model_multi2 <- nnet::multinom(
  extrication ~ age_band + sex + age_band:sex,
  data = fire_rescue_clean
)
## Check the summary of the upgraded model
summary(model_multi2)
## Tidy the upgraded model output
tidy(model_multi2, conf.int = TRUE, exponentiate = TRUE)
## Check the model coefficients
coef(model_multi2)
## Check the model residuals
residuals(model_multi2)
## Compare the models
AIC(model_multi)
AIC(model_multi2)
# ---

# Apply Multinomial GAM model
library(dplyr)
library(VGAM)
## Create numeric age_band variable
fire_rescue_clean <- fire_rescue_clean %>%
  mutate(age_band_num = as.numeric(age_band))
## Fit the Multinomial GAM model
model_gam_test_3df <- vglm(
  extrication ~ sm.ns(age_band_num, df = 3),
  family = multinomial,
  data = fire_rescue_clean
)
## Check the summary of the Multinomial GAM model
summary(model_gam_test_3df)

## Change the level to 2 degrees of freedom
model_gam_test_2df <- vglm(
  extrication ~ sm.ns(age_band_num, df = 2),
  family = multinomial,
  data = fire_rescue_clean
)
## Check the summary of the Multinomial GAM model with 2 degrees of freedom
summary(model_gam_test_2df)
## Compare the models
AIC(model_multi2)
AIC(model_gam_test_3df)
AIC(model_gam_test_2df)
# ---

# Apply Binomial Regression (Logistic Regression) Model
library(dplyr)
library(broom)
## Create a function to fit a binary model for each extrication method
make_binary_model <- function(df, method) {
  df <- df %>%
    mutate(
      extricated = factor(ifelse(extrication == method, 1, 0), levels = c(0, 1))
    )
  glm(
    extricated ~ age_band + sex + age_band:sex,
    data = df,
    family = binomial(link = "logit")
  )
}
## Fit the binary model for each extrication method
methods <- levels(fire_rescue_clean$extrication)
binary_models <- lapply(methods, function(method) {
  make_binary_model(fire_rescue_clean, method)
})
## Combine the results into a single data frame
binary_models_df <- do.call(rbind, binary_models)
## Check the binary models results
binary_models_df
## Assign names to the models
names(binary_models) <- methods
## Check the names of the binary models
names(binary_models)
## Get the AIC values for the binary models
binary_aic <- sapply(binary_models, AIC)
binary_models
# ---

# Model diagnostics with Chi-squared test
table_age <- table(fire_rescue_clean$age_band, fire_rescue_clean$extrication)
chisq.test(table_age)
table_sex <- table(fire_rescue_clean$sex, fire_rescue_clean$extrication)
chisq.test(table_sex)
# Chi-squared test to examine the association between age_band and extrication
chisq_test_result <- chisq.test(table(
  fire_rescue_clean$age_band,
  fire_rescue_clean$extrication
))

# Print the results of the Chi-squared test
print(chisq_test_result)

# Return the Chi-squared test result
chisq_test_result
# ---
# Apply Poisson Regression Model
model_poisson <- glm(
  n_casualties ~ age_band + sex + extrication,
  data = fire_rescue_clean,
  family = poisson()
)
summary(model_poisson)
# ---

# Summarise model results
# ---
