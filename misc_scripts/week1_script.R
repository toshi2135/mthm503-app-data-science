library(tidyverse)

# We only want this package for this demonstration
renv::install("mdsr")
# So you might not want to record it permanently
renv::record("mdsr")

library(mdsr)

# Create a database connection
db <- dbConnect_scidb("airlines")

# Connect to two tables on the database server
flights <- tbl(db, "flights")
class(flights)
flights %>%
  object.size() %>%
  print(units = "Kb")

carriers <- tbl(db, "carriers")

# Specify a pipeline for the data in tidyverse (dplyr)

q <- flights %>%
  filter(
    year == 2013 & month == 9,
    dest == 'JFK'
) %>%
  inner_join(carriers, by = c("carrier" = "carrier")) %>%
  group_by(.data$name) %>%
  summarise(
    n_flights = n(),
    pct_ontime = sum(arr_delay <= 15) / n()
) %>%
  filter(n_flights > 100) %>%
  arrange(desc(.data$pct_ontime))



class(q)
show_query(q)


head(q, 4)


dbDisconnect()

