#~############################################################################~#
# General preparation ----
#~############################################################################~#

# Clean
rm(list=ls())

# prepare a seed for simulations
set.seed(2023)

# Set number of runs for our Monte Carlo simulations
MC_runs <- 100000

# Set number of cores
my_cores <- parallel::detectCores()

#~############################################################################~#
# Packages ----
#~############################################################################~#

#~=======================================================~=
## Installing ----
#~=======================================================~=
# Making sure to install everything that is needed (but without loading it)
# because something we only use one function so we want to avoid overwriting
# and taking up too much space.

# Here's a function that will install any missing library.
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (length(find.package(pkg, quiet = TRUE)) == 0) {
      install.packages(pkg)
    }
  }
}

install_if_missing(c(
  "cowplot",
  "kableExtra",
  "patchwork",
  "readxl",
  "tidyverse"
))

#~=======================================================~=
## Loading the packages ----
#~=======================================================~=
# Load only the packages that we want to fully load.
library(tidyverse)
library(cowplot)
library(patchwork)

# Libraries for rmd
library(kableExtra)

# get rid of summarise messages
options(dplyr.summarise.inform = FALSE)

#~############################################################################~#
# Loading custom functions ----
#~############################################################################~#

# Load custom functions that we have in our githubs.
# For basic functions
suppressMessages(devtools::source_url("https://raw.githubusercontent.com/Happier-Lives-Institute/general-functions/main/functions_basic.R"))
# For reporting functions
suppressMessages(devtools::source_url("https://raw.githubusercontent.com/Happier-Lives-Institute/general-functions/main/functions_reporting.R"))
# For Cohen's d functions
suppressMessages(devtools::source_url("https://raw.githubusercontent.com/Happier-Lives-Institute/general-functions/main/functions_cohen_d.R"))
# For meta-analysis functions
suppressMessages(devtools::source_url("https://raw.githubusercontent.com/Happier-Lives-Institute/general-functions/main/functions_meta_analysis.R"))
# Custom themes and styles
suppressMessages(devtools::source_url("https://raw.githubusercontent.com/Happier-Lives-Institute/general-functions/main/functions_styles.R"))
# grid approximation function
suppressMessages(devtools::source_url("https://raw.githubusercontent.com/Karakaii/grid_approximation/main/grid_approximation.R"))
