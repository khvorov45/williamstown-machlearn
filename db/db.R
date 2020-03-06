# Working with databases
# Arseniy Khvorov

library(tidyverse)
library(dbplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")

copy_to(
  con, mtcars, "mtcars",
  temporary = FALSE
)

mtcars_db <- tbl(con, "mtcars")

mtcars_db %>%
  select(mpg, cyl, drat) %>%
  filter(drat > 2) %>%
  show_query()
