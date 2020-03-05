# Decision tree
# Arseniy Khvorov

library(tidyverse)
library(rpart)
library(rpart.plot)

# Directories used
data_dir <- "data"
tree_dir <- "tree"

# Functions ===================================================================

read_data <- function(name) {
  read_csv(
    file.path(data_dir, glue::glue("{name}.csv")),
    col_types = cols(
      male = col_integer(),
      sex = col_factor(),
      los = col_integer()
    )
  )
}

save_tree <- function(tree, name) {
  pdf(file.path(tree_dir, glue::glue("{name}.pdf")))
  prp(tree)
  dev.off()
}

# Script ======================================================================

# Read simulated data
sim_nomiss <- read_data("sim-nomiss")
sim_miss <- read_data("sim-miss")

# What would be kept if we couldn't handle missing values
sim_miss_complete <- sim_miss[complete.cases(sim_miss), ]

# Trees and plots
tree_nomiss <- rpart(los ~ age + sex, sim_nomiss)
save_tree(tree_nomiss, "nomiss")

tree_miss <- rpart(los ~ age + sex, sim_miss)
save_tree(tree_nomiss, "miss")

# Predictions
