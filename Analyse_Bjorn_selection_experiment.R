
# ECO-EVO Baltic Sea phytoplankton: Test data from Bjorn's selection experiment
# Date start: 2022/06/16
# Authors: James Hagan (james_hagan(at)outlook.com)

# Title: Process Bjorn's selection experiment data as a test

# Session information:
# R version 4.1.2 (2021-11-01)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)

# attached base packages: stats, graphics, grDevices, utils, datasets, methods, base

# set working directory
setwd("C:/Users/james/Dropbox/IMBRSea_2021")

# check that the working directory is correct
getwd()

# load relevant libraries

library(dplyr) # data wrangling
library(readr) # read data files
library(tidyr) # data wrangling
library(ggplot2) # data visualisation
library(stringr) # work with strings
library(lubridate) # work with dates
library(here) # path management

# load the data
Barcoding <- read.table(file = here("R_projects/SelectionExperiment/Input/All_strains.txt"), 
                        sep = '\t', header = TRUE)
sapply(Barcoding, class)
str(Barcoding)
unique(Barcoding$Treatment)

# remove the strain samples
unique(Barcoding$Strain)

Barcoding2 <- 
  Barcoding %>%
  filter(Experiment %in% c("VG", "GP"))

# calculate averages for the barcoding data
Barcoding_Ave <- 
  Barcoding2 %>%
  group_by(Population, Treatment, Timepoint, Strain) %>%
  summarise(mean = mean(Relative_abundance),
            sd = sd(Relative_abundance), .groups = "drop")

# remove RO5 samples
Barcoding_Ave <-
  Barcoding_Ave %>%
  filter(Population %in% c("VG", "GP"))

# lets also Remove RO5AC from individual samples
unique(Barcoding_Ave$Treatment)
Barcoding_Ave <- 
  Barcoding_Ave %>%
  filter(Strain != 'RO5AC')

# clone start values to Cu
Clone0 <- 
  Barcoding_Ave %>%
  filter(Timepoint == "0")
Clone0$Treatment <- recode_factor(Clone0$Treatment, "Control" = "Copper")
Barcoding_Ave <- rbind(Barcoding_Ave, Clone0)

# plot Bjorn's graph
library(RColorBrewer)
n <- 58
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))

#Lets make it all in one graph
sapply(Barcoding_Ave, class)
Barcoding_Ave$Timepoint <- as.numeric(as.character(Barcoding_Ave$Timepoint))

p1 <- 
  ggplot(data = Barcoding_Ave, 
         mapping = aes(x = Timepoint, y = mean , fill=Strain)) + 
  geom_area(position='fill') +
  facet_grid(rows = vars(Population), cols = vars(Treatment)) +
  labs (x="Time (days)", y=("Relative biomass"), title = "") +
  scale_fill_viridis_d(option = "C") +
  theme_bw()
p1


