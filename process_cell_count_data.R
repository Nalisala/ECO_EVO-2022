
# ECO-EVO Baltic Sea phytoplankton: Experiment 2
# Date start: 2022/06/03
# Authors: James Hagan (james_hagan(at)outlook.com)

# Title: Process cell count data

# Session information:
# R version 4.1.2 (2021-11-01)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)

# attached base packages: stats, graphics, grDevices, utils, datasets, methods, base

# load relevant libraries

library(dplyr) # data wrangling
library(readr) # read data files
library(tidyr) # data wrangling
library(ggplot2) # data visualisation
library(stringr) # work with strings
library(lubridate) # work with dates
library(here) # path management

# check where here set our working directory
here()

# check that we have a Data folder to output our processed data
if(! dir.exists(here("Data"))){
  dir.create(here("Data"))
}

