
# ECO-EVO Baltic Sea phytoplankton: Test data from Bjorn's selection experiment
# Date start: 2022/06/16
# Authors: James Hagan (james_hagan(at)outlook.com)

# Title: Process Bjorn's selection experiment data as a test

# Session information:
# R version 4.1.2 (2021-11-01)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)

# attached base packages: stats, graphics, grDevices, utils, datasets, methods, base

# load relevant libraries

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(here)

# where is here()'s working directory?
getwd()
here("All_strains.txt")

# load the data: '\t' - separator for text files; head = TRUE - if the file has column names
Barcoding <- read.table(file = "C:/Users/james/Dropbox/IMBRSea_2021/R_projects/SelectionExperiment/Input/All_strains.txt",
                        sep = '\t', header = TRUE)

head(Barcoding) # views the first 6 rows
str(Barcoding) # shows the structure of the different variables (i.e. numeric, character etc.)
unique(Barcoding$Treatment) # shows unique values of the variable: control versus copper enrichned
unique(Barcoding$Population) # two different S. marinoi populations

# remove the strains samples i.e. Population == "RO5AV" and Experiment == NA
Barcoding2 <- 
  Barcoding %>%
  filter(Experiment %in% c("VG", "GP")) # %in% very important!
unique(Barcoding2$Treatment)

# we can't use == for more than one value matching
Barcoding %>%
  filter(Experiment == c("VG", "GP"))

# why? 
x <- c("A", "A", "B", "A", "C")
x[x == c("A", "B")]
x[x %in% c("A", "B")] # this is the correct wat

# view the data
View(Barcoding2)

# calculate mean relative abundance for each population, treatment, timepoint and strain
# across the five replicates

Barcoding_Ave <- 
  Barcoding2 %>%
  group_by(Population, Treatment, Timepoint, Strain) %>% # groups by unique combinations of these factors
  summarise(mean = mean(Relative_abundance), # takes the mean across replicates for each unique combination of factors specified in group_by()
            sd = sd(Relative_abundance), # takes the sd across replicates
            n = n(), # calculates the number of replicates within each group (as set up by group_by)
            .groups = "drop" ) # .groups = "drop" removes the grouping structure
View(Barcoding_Ave)

# remove the R05 samples
unique(Barcoding_Ave$Population)

# select the two populations we want
Barcoding_Ave <- 
  Barcoding_Ave %>%
  filter( Population %in% c("VG", "GP") )

# we unselect the population we don't want: ! means 'not'
# Barcoding_Ave %>%
# filter(Population != "R05AC")

# a note on the ! sign
x <- c(TRUE, FALSE, TRUE)
x
!x  

# all populations that are not VG or GP
# Barcoding_Ave %>%
# filter( !(Population %in% c("VG", "GP")) )

# let's remove the R05AV from individual samples
Barcoding_Ave <- 
  Barcoding_Ave %>%
  filter(Strain != "RO5AC")

View(Barcoding_Ave)

# clone start values to copper treatment
Clone0 <- 
  Barcoding_Ave %>%
  filter(Timepoint == 0)
View(Clone0)

# recode the control value to copper
Clone0$Treatment <- recode_factor(Clone0$Treatment, "Control" = "Copper")

# stick the Clone0 file onto the bottom of the Barcoding_Ave data.frame
Barcoding_Ave <- rbind(Barcoding_Ave, Clone0)
View(Barcoding_Ave)

# plot the graph
p1 <- 
  ggplot(data = Barcoding_Ave,
         mapping = aes(x = Timepoint, y = mean, fill = Strain)) +
  geom_area(position = "fill") +
  facet_grid(rows = vars(Population), cols = vars(Treatment) ) +
  ylab("Relative abundance (average)") +
  xlab("Time") +
  scale_fill_viridis_d(option = "C") +
  theme_bw()
p1  

# check that we have a Data folder to output our processed data
if(! dir.exists(here("Figures"))){
  dir.create(here("Figures"))
}


