install.packages("remotes")
remotes::update_packages(
  c(
    "tidyverse", "extraDistr", "rpart", "rpart.plot", "RSQLite", "nycflights13"
  ),
  upgrade = "always"
)
