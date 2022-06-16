
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

# get a list of files
files <- list.files(path = here("Results/PlateReader/Gosias_data/plate_reader_sample_data/"))
print(files)

# initialise an empty list to store our results
pl_loop <- vector("list", length = length(files))
print(pl_loop)
for (i in 1:length(files)) {
  
  dpath <- paste("Results/PlateReader/Gosias_data/plate_reader_sample_data/", files[i], sep = "")
  
  # load one of the data files
  pl <- read_tsv(file = here(dpath),
                 skip = 28)
  # head(pl)
  
  # remove the extra columns
  # select(pl, Well, Value)
  
  # pl %>%
  # select(Well, Value)
  
  pl <- 
    pl %>%
    select(-starts_with("..."))
  
  # check the structure of the data.frame
  # str(pl)
  # summary(pl)
  
  # get identifier information
  pl.id <- read_tsv(file = here(dpath),
                    n_max = 19)
  
  # check the pl.id parsing
  # head(pl.id)
  # View(pl.id)
  
  # remove the columns with only NA values
  
  # 1. manually
  
  # use the select function
  # select(pl.id, `General Info`, ...2, ...6)
  
  # selecting column numbers
  # pl.id[, c(1, 2, 6)]
  
  # 2. base R
  x <- apply(pl.id, MARGIN = 2, function(x) { all( is.na(x) )  } )
  # print(x)
  # names(x)
  
  # get rid of the names
  names(x) <- NULL
  # print(x)
  
  # only keep columns where x == FALSE
  pl.id <- pl.id[ , !x]
  # head(pl.id)
  
  # clean up this data.frame
  # pl.id %>%
  # rename( General = `General Info`,
  # Group = ...2, 
  # Value = ...6)
  
  # also rename using base R
  names(pl.id) <- c("General", "Group", "Value")
  # print(pl.id)
  
  # view the data
  # View(pl.id)
  
  # remove the missing value information
  y <- which( is.na(pl.id$General) )
  y <- c( 1, diff(y), 2 )
  y <- diff( c(1, which(y == 2)) )
  
  # get the unique values from the general column
  z <- unique(pl.id$General)
  
  # remove the NA values from the rows i.e. where both Group and Value are NA
  pl.id <- 
    pl.id %>%
    filter(!(is.na(Group) & is.na(Value)) )
  
  # overwrite the General column with the cleaned vector
  pl.id$General <- rep(z[!is.na(z)],  y)
  
  # check if this worked correctly
  # head(pl.id)
  # View(pl.id)
  # View(pl)
  
  
  # process the raw data
  
  # calculate the average across measurements for each well
  pl.sum <- 
    pl %>%
    group_by(Well) %>%
    summarise(Value.m = mean(Value, na.rm = TRUE), .groups = "drop")
  # head(pl.sum)
  
  # load the plate design file
  pd <- read_csv(file = here("Results/PlateReader/Exp2_plate_design.csv"))
  # head(pd)
  
  # which plate is this?
  x <- 
    pl.id %>%
    filter(Group == "Run name") %>%
    pull(Value)
  # print(x)
  
  # extract the correct plate id
  if ( grepl(pattern = "plate1|plate_1|plate.1", x = x) ) {
    plate_id <- 1
  } else if ( grepl(pattern = "plate2|plate_2|plate.2", x = x) ) {
    plate_id <- 2
  } else {
    stop("Cannot determine which plate to use! Check `Run name` information")
  }
  # print(plate_id)
  
  # join these data.frames together
  # pd %>%
  # filter(PLATE_ID == plate_id) 
  
  # pl.sum %>%
  # rename(WELL = Well)
  
  pl.sum <- 
    full_join(pd %>%
                filter(PLATE_ID == plate_id) ,
              pl.sum %>%
                rename(WELL = Well),
              by = c("WELL")
    ) 
  # View(pl.sum)
  
  # first, we need to extract the blank value
  blank.m <- 
    pl.sum %>%
    filter(TREAT == "BLANK") %>%
    pull(Value.m)
  # print(blank.m)
  
  # substract the blank from the mean fluorescence of each well
  # pl.sum %>%
  # mutate(Value.m = Value.m - blank.m)
  
  pl.sum$Value.m <- (pl.sum$Value.m - blank.m)
  # View(pl.sum)
  
  # remove the missing rows i.e. the empty wells and the blank
  pl.sum <- 
    pl.sum %>%
    filter(!is.na(TREAT), !(TREAT == "BLANK"))
  # View(pl.sum)
  
  # add date information
  date <- 
    pl.id %>%
    filter(Group == "Run started") %>%
    pull(Value)
  # print(date)
  
  # as.Date(word(date, 1), tryFormats = "%d/%m/%Y")
  
  # add date information
  pl.sum$Date <- as.Date(word(date, 1), tryFormats = "%m/%d/%Y")
  # View(pl.sum)
  
  # reorder the columns
  pl.sum <- 
    pl.sum %>%
    select(Date, EXP, PLATE_ID, WELL, TREAT, REP, Value.m) %>%
    rename(RFU = Value.m)
  # View(pl.sum)
  
  # fill up the list for our output
  pl_loop[[i]] <- pl.sum
  
}
pl_data <- bind_rows(pl_loop)
View(pl_data)

# arrange the data
pl_data <- 
  pl_data %>%
  arrange(Date, EXP, TREAT, REP)
View(pl_data)


# Visualise the data

# before we visualise the data, we need to add in the dilution step

# pivot_longer()
# pivot_wider() from the tidyr package

pl_data.p <- 
  pl_data %>%
  mutate(RFU_dilute = RFU/100) %>%
  pivot_longer(cols = c("RFU", "RFU_dilute"),
               names_to = "RFU_type",
               values_to = "RFU")

# summarise the data
pl_data.p <- 
  pl_data.p %>%
  group_by(Date, EXP, TREAT, RFU_type) %>%
  summarise(RFU = mean(RFU))

# if values are less than 0, then make them very close to zero
min(pl_data.p$RFU[pl_data.p$RFU > 0])
quantile(pl_data.p$RFU, 0.2)
round(pl_data.p$RFU, 3)

# use the ifelse() function to set all RFU values below zero to 0.0001
# ifelse(condition, value if TRUE, value if FALSE)
pl_data.p <- 
  pl_data.p %>%
  mutate(RFU = ifelse(RFU < 0, 0.0001, RFU))

# base R plotting
plot()
hist(pl_data.p$RFU) 

# plotting is quite good in base R but much easier with ggplot2

# if we want to plot multiple plots, we can copy-paste
ggplot(data = pl_data.p %>% filter(TREAT == "AMB_DC"),
       mapping = aes(x = Date, y = RFU, colour = TREAT)) +
  geom_point(size = 2, alpha = 0.5)

# geom_point()
# size = size of points
# alpha = transparency (0 = transparent, 1 = un-transparent)

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

# changing the axes values
# scale_y_continuous(): limits = min and max shown on axis, breaks are the tick values
# scale_x_continuous()

### END