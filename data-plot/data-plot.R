# Plots of data
# Arseniy Khvorov

library(tidyverse)
library(ggdark) # devtools::install_github("khvorov45/ggdark")

# Directories used
data_dir <- "data"
data_plot_dir <- "data-plot"

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

plot_hist <- function(dat) {
  dat %>%
    ggplot(aes(los)) +
    dark_theme_bw(verbose = FALSE) +
    geom_histogram(binwidth = 0.5)
}

plot_scatter <- function(dat) {
  dat %>%
    ggplot(aes(age, los, col = sex)) +
    dark_theme_bw(verbose = FALSE) +
    geom_point()
}

save_plot <- function(plot, name) {
  ggsave_dark(
    file.path(data_plot_dir, glue::glue("{name}.pdf")),
    plot, dark = FALSE,
    width = 7.5, height = 7.5, units = "cm"
  )
}

# Script ======================================================================

sim_nomiss <- read_data("sim-nomiss")
los <- plot_hist(sim_nomiss)
save_plot(los, "nomiss-los")

sct <- plot_scatter(sim_nomiss)
save_plot(sct, "nomiss-sct")
