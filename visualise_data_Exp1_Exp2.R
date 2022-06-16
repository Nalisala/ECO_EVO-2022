
# ECO-EVO Baltic Sea phytoplankton: Experiment 1 and 2
# Date start: 2022/06/03
# Authors: James Hagan (james_hagan(at)outlook.com)

# Title: Visualise the data

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

# check that we have a Figures folder to output our processed data
if(! dir.exists(here("Figures"))){
  dir.create(here("Figures"))
}

# if we want to plot multiple plots, we can copy-paste
ggplot(data = pl_data.p %>% filter(TREAT == "AMB_DC"),
       mapping = aes(x = Date, y = RFU, colour = TREAT)) +
  geom_point(size = 2, alpha = 0.5)

ggplot(data = pl_data.p %>% filter(TREAT == "AMB_MC"),
       mapping = aes(x = Date, y = RFU, colour = TREAT)) +
  geom_point(size = 2, alpha = 0.5)

# use facet_wrap() for multiple plots
# plot for each TREAT, each plot on own scale
ggplot(data = pl_data.p,
       mapping = aes(x = Date, y = RFU)) +
  geom_line() +
  geom_point() +
  facet_wrap(~TREAT, scale = "free") +
  scale_y_continuous(trans = "log10") +
  ylab("Fluorescence") +
  xlab("Sample date") +
  # theme_classic()
  theme_bw()

### END
