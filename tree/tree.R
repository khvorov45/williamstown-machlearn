# Decision tree
# Arseniy Khvorov

library(tidyverse)
library(ggdark) # devtools::install_github("khvorov45/ggdark")
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

read_data_test <- function(name) {
  read_data(name) %>%
    select(age, sex, los_real = los, -starts_with("log_los"))
}

save_tree <- function(tree, name) {
  pdf(file.path(tree_dir, glue::glue("{name}.pdf")))
  prp(tree)
  dev.off()
}

add_predicted <- function(tree, newdata) {
  newdata %>%
    mutate(
      los_predicted = predict(tree, newdata)
    )
}

plot_scatter_acc <- function(preds,
                             x_name = "los_predicted",
                             y_name = "los_real") {
  preds %>%
    ggplot(aes(!!rlang::sym(x_name), !!rlang::sym(y_name))) +
    dark_theme_bw(verbose = FALSE) +
    geom_jitter()
}

save_plot <- function(plot, name) {
  ggsave_dark(
    file.path(tree_dir, glue::glue("{name}.pdf")),
    plot, dark = FALSE,
    width = 7.5, height = 7.5, units = "cm"
  )
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
sim_nomiss_test <- read_data_test("sim-nomiss-test")
sim_miss_test <- read_data_test("sim-miss-test")

sim_nomiss_pred <- add_predicted(tree_nomiss, sim_nomiss_test)
sim_miss_pred <- add_predicted(tree_miss, sim_miss_test)

sc_nomiss <- plot_scatter_acc(sim_nomiss_pred)
save_plot(sc_nomiss, "sc-nomiss")

sc_miss <- plot_scatter_acc(sim_miss_pred)
save_plot(sc_miss, "sc-miss")
