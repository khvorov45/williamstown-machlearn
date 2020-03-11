# Working with databases
# Arseniy Khvorov

library(tidyverse)
library(dbplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")

copy_to(
  con, nycflights13::flights, "flights",
  temporary = FALSE,
  indexes = list(
    c("year", "month", "day"),
    "carrier",
    "tailnum",
    "dest"
  )
)

flights_db <- tbl(con, "flights")

tailnum_delay_db <- flights_db %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay),
    n = n()
  ) %>%
  arrange(desc(delay)) %>%
  filter(n > 100)
