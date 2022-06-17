
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
library(here) # path management

# check that we have a Data folder to output our processed data
if(! dir.exists(here("Data"))){
  dir.create(here("Data"))
}

# load the data
cells <- read_csv(file = "C:/Users/james/Dropbox/IMBRSea_2021/R_projects/counting exp1 exp2 - counting exp2 data.csv")
View(cells)

# extract first 42 lines
cells <- cells[1:42, ]
View(cells)

# extract relevant t28 columns
names(cells)
cells_t28 <- cells[, c(1, 3, 5, 7)]
View(cells_t28)

# extract rows with all data (i.e. no NAs)
cells_t28 <- cells_t28[complete.cases(cells_t28), ]
View(cells_t28)
names(cells_t28)

# rename the columns
cells_t28 <- 
  cells_t28 %>%
  rename(day = DAY...1, # new_name = old_name (i.e. how rename works)
         treatment = TREATMENT...3,
         skeletonema = `skeletonema/L mean...5`,
         chaetoceros = `chaetoceros/L mean...7`)

# OR:
# names(cells_t28) <- c("days", "treatment", "skeletonema", "chaetoceros")
View(cells_t28)


# t7
# extract relevant t28 columns
names(cells)
cells_t7 <- cells[, c(19, 20, 22, 24)]
View(cells_t7)

# extract rows with all data (i.e. no NAs)
cells_t7 <- cells_t7[complete.cases(cells_t7), ]
View(cells_t7)
names(cells_t7)

# rename the columns
cells_t7 <- 
  cells_t7 %>%
  rename(day = DAY...19, # new_name = old_name (i.e. how rename works)
         treatment = TREATMENT...20,
         skeletonema = `skeletonema/L mean...22`,
         chaetoceros = `chaetoceros/L mean...24`)
names(cells_t7)

# we have these two data.frames
str(cells_t28)
str(cells_t7)

# convert skeletonema and chaetoceros variables to numeric variables in cells_t28

# use mutate from dplyr
cells_t28 <- 
  cells_t28 %>%
  mutate(skeletonema = as.numeric(skeletonema),
         chaetoceros = as.numeric(chaetoceros))
str(cells_t28)

# using base R
# cells_t28$skeletonema <- as.numeric(cells_t28$skeletonema)
# cells_t28$chaetoceros <- as.numeric(cells_t28$chaetoceros)

# bind the two datasets
cells_all <- rbind(cells_t28, cells_t7)
View(cells_all)

# extract individual salinity and competition treatments
cells_all$treatment[1]

stringr::word(cells_all$treatment[1], 1, 1)
stringr::word(cells_all$treatment[1], 2, 2)
stringr::word(cells_all$treatment[1], 3, 3)

# use this to create three separate columns for each "word"
cells_all <- 
  cells_all %>%
  mutate(salinity = word(treatment, 1, 1),
         competition = word(treatment, 2, 2),
         replicate = word(treatment, 3, 3))
View(cells_all)

# convert skeletonema and chaetoceros to biovolume (Hillebrand et al. 1999)
# https://onlinelibrary.wiley.com/doi/abs/10.1046/j.1529-8817.1999.3520403.x?casa_token=N2jmEnQ_K-4AAAAA:TlaoHU4eVWhO7OwFbUw4L1h6piABNQ_USmHmkjTdAhZh6PduiYG36I-eZm5ZMQ-JS5NTpbijsIWMyTs
cells_all <- 
  cells_all %>%
  mutate(skeletonema = formula, # how to do this calculation?
         chaetoceros = formula)

# use pivot_longer() to make the dataset into the long format by species
cells_all <- 
  cells_all %>%
  pivot_longer(cols = c("skeletonema", "chaetoceros"),
               names_to = "species",
               values_to = "cells")
View(cells_all)

# use group_by and mutate to calculate total abundance per replicate
cells_all <- 
  cells_all %>%
  group_by(day, salinity, competition, replicate) %>%
  mutate(total = sum(cells)) %>% # first calculate total abundance per replicate
  ungroup() %>%
  mutate(cells = cells/total) %>% # then, calculate relative abundance
  select(-total) # then, remove the total column
View(cells_all)

# write this into a .csv file
write_csv(x = cells_all, file = here("Data/cell_counts_exp2.csv"))

### END
