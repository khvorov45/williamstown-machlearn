# Simulated data
# Arseniy Khvorov

library(tidyverse)
library(extraDistr)

# Directories used
data_dir <- "data"

# Functions ===================================================================

sim_dat <- function(n = 1000) {
  tibble(
    age = runif(n, 20, 80),
    male = as.integer(rbern(n, 0.5)),
    log_los_exp = 1 + 0.03 * age + 0.02 * male - 0.01 * age * male,
    log_los = rnorm(n, log_los_exp, sd = 0.1)
  )
}

cond_missing <- function(vec, pna, naval) {
  if_else(as.logical(rbern(length(vec), pna)), naval, vec)
}

add_miss <- function(dat, pna = 0.2) {
  mutate(
    dat,
    age = cond_missing(age, pna, NA_real_),
    male = cond_missing(male, pna, NA_integer_)
  )
}

add_vars <- function(dat) {
  mutate(
    dat,
    sex = if_else(male == 1L, "male", "female"),
    los = as.integer(exp(log_los))
  )
}

save_data <- function(dat, name) {
  write_csv(dat, file.path(data_dir, glue::glue("{name}.csv")))
}

gen_dat <- function(n, pna, name) {
  sim_dat(n) %>%
    add_miss(pna) %>%
    add_vars() %>%
    save_data(name)
}

# Script ======================================================================

gen_dat(1e3, 0, "sim-nomiss")
gen_dat(1e3, 0.5, "sim-miss")

gen_dat(1e3, 0, "sim-nomiss-test")
gen_dat(1e3, 0.5, "sim-miss-test")
