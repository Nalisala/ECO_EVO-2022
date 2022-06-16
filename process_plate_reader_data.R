
# ECO-EVO Baltic Sea phytoplankton: Experiment 2
# Date start: 2022/06/03
# Authors: James Hagan (james_hagan(at)outlook.com)

# Title: Process chlorophyll-fluorescence plate-reader data

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

# what does the here() package do?
here()
here("Results/")

# load one of the data files
pl <- read_tsv(file = here("Results/PlateReader/Gosias_data/Eco-evo_ex2_t14_plate1_18052022 (1).txt"),
               skip = 28)
head(pl)
View(pl)

# remove the extra columns
pl <- 
  pl %>%
  select(-starts_with("..."))

# check the structure of this data.frame
str(pl)
summary(pl)

# get identifier information
pl.id <- read_tsv(file = here("Results/PlateReader/Gosias_data/Eco-evo_ex2_t7_plate1_11052022.txt"),
               n_max = 19)

# check this parsing
head(pl.id)
View(pl.id)

# remove the columns with only NA values

# 1. manually
pl.id[, c(1, 2, 6)]

# 2. tidyverse
pl.id %>% 
  select(
    where(
      ~sum(!is.na(.x)) > 0
    )
  )

# 3. base R

# for each column, ask if all of the values are NAs
x <- apply(pl.id, 2, function(x) { all(is.na(x)) } )
names(x) <- NULL
print(x)

# use this vector of TRUE and FALSE values to remove the relevant columns
pl.id[, !x]
pl.id <- pl.id[, !x]
head(pl.id)

# clean up this data.frame
pl.id %>%
  rename(General = `General Info`,
         Group = ...2,
         Value = ...6)

# also use base R
names(pl.id) <- c("General", "Group", "Value")
print(pl.id) 

# remove the missing value information
y <- which( is.na(pl.id$General) )
y <- c( 1, diff(y), 2 )
y <- diff( c(1, which(y == 2)) )

z <- unique(pl.id$General)

# remove the NA values from the rows
pl.id <- 
  pl.id %>%
  filter(!(is.na(Group) & is.na(Value)) )

pl.id$General <- rep(z[!is.na(z)],  y)

# check if this worked correctly
head(pl.id)
View(pl.id)


# process the raw data

# calculate the average across measurements for each well
pl.sum <- 
  pl %>%
  group_by(Well) %>%
  summarise(Value.m = mean(Value, na.rm = TRUE), .groups = "drop")
head(pl.sum)

# load the plate design file
pd <- read_csv(file = here("Results/PlateReader/Exp2_plate_design.csv"))
head(pd)

# which plate is this?
x <- 
  pl.id %>%
  filter(Group == "Run name") %>%
  pull(Value)

# extract the correct plate id
if (grepl(pattern = "plate1|plate_1|plate.1", x = x)) {
  plate_id <- 1
} else if (grepl(pattern = "plate2|plate_2|plate.2", x = x)) {
  plate_id <- 2
} else {
  stop("Cannot determine which plate to use! Check `Run name` information")
}
print(plate_id)

# join these data.frames together
pl.sum <- 
  full_join(pd %>%
            filter(PLATE_ID == plate_id),
            pl.sum %>%
              rename(WELL = Well),
            by = c("WELL")
            )

# extract the blank from this dataset
blank.m <- 
  pl.sum %>%
  filter(TREAT == "BLANK") %>%
  pull(Value.m)

# substract the blank from the mean fluorescence of each well
pl.sum$Value.m <- (pl.sum$Value.m - blank.m)
head(pl.sum)

# remove the missing columns i.e. empty wells
pl.sum <- 
  pl.sum %>%
  filter(!is.na(TREAT), !(TREAT == "BLANK"))

# add the date to this analysis
date <- 
  pl.id %>%
  filter(Group == "Run started") %>%
  pull(Value)

# add date information
pl.sum$Date <- as.Date(word(date, 1), tryFormats = c("%d/%m/%Y"))

# reorder the columns
pl.sum <- 
  pl.sum %>%
  select(Date, EXP, PLATE_ID, WELL, TREAT, REP, Value.m) %>%
  rename(RFU = Value.m)
View(pl.sum)

log10(pl.sum$RFU)

# plot this on a log scale, in ggplot2, we use
scale_x_continuous(trans = "log10")




