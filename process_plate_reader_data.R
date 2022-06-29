
# ECO-EVO Baltic Sea phytoplankton: Experiment 2
# Date start: 2022/06/03
# Authors: James Hagan (james_hagan(at)outlook.com)

# Title: Process chlorophyll-fluorescence plate-reader data

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

# set the path to the dropbox
dbox_path <- "C:/Users/james/Dropbox/IMBRSea_2021/Results/PlateReader/Gosias_data/plate_reader_sample_data/"

# get a list of data files
files <- list.files(path = dbox_path)
print(files)

# load the plate design file
pd1 <- read_csv(file = "C:/Users/james/Dropbox/IMBRSea_2021/R_projects/Exp2_plate_design1.csv")
pd2 <- read_csv(file = "C:/Users/james/Dropbox/IMBRSea_2021/R_projects/Exp2_plate_design2.csv")
pd3 <- read_csv(file = "C:/Users/james/Dropbox/IMBRSea_2021/R_projects/Exp2_plate_design3.csv")
pd4 <- read_csv(file = "C:/Users/james/Dropbox/IMBRSea_2021/R_projects/Exp2_plate_design4.csv")

# write this into a list
plates <- list(pd1, pd2, pd3, pd4)
plates[[2]]

pd_order <- c(1, 1, 2, 1, 1, 1, 1, 1)

# files: vector of file names that are associated with the plate reader data
# files_location: path where the files are stored

# plate: list of .csv files or data.frames describing the different plate configurations
# plate_files: integer vector describing which file corresponds to which plate deisgn

# output_path: path of the folder to output our cleaned data
# output_name: name of a .csv file that we will output

plate_reader_cleaner <- function(files, file_location, 
                                  plate, plate_files = NA,
                                  output_path,
                                  output_name) {
  
  # set-up a condition to make sure we have plate and plate_files data
  if (length(plate) > 1 & is.na(plate_files) ) {
    stop("multiple plate files but no vector specifying order")
  }
  
  # initialise an empty list to store our results
  pl_loop <- vector("list", length = length(files))
  print(pl_loop)
  for (i in 1:length(files)) {
    
    dpath <- paste(file_location, files[i], sep = "")
    
    # load one of the data files
    pl <- read_tsv(file = (dpath),
                   skip = 28)
    
    pl <- 
      pl %>%
      select(-starts_with("..."))
    
    # get identifier information
    pl.id <- read_tsv(file = (dpath),
                      n_max = 19)
    
    # 2. base R
    x <- apply(pl.id, MARGIN = 2, function(x) { all( is.na(x) )  } )
    
    # get rid of the names
    names(x) <- NULL
    
    # only keep columns where x == FALSE
    pl.id <- pl.id[ , !x]
    
    # also rename using base R
    names(pl.id) <- c("General", "Group", "Value")
    
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
    
    # process the raw data
    
    # calculate the average across measurements for each well
    pl.sum <- 
      pl %>%
      group_by(Well) %>%
      summarise(Value.m = mean(Value, na.rm = TRUE), .groups = "drop")
    
    # which plate is this?
    x <- 
      pl.id %>%
      filter(Group == "Run name") %>%
      pull(Value)
    
    # extract the correct plate id
    if ( grepl(pattern = "plate1|plate_1|plate.1", x = x) ) {
      plate_id <- 1
    } else if ( grepl(pattern = "plate2|plate_2|plate.2", x = x) ) {
      plate_id <- 2
    } else {
      stop("Cannot determine which plate to use! Check `Run name` information")
    }
    
    # we need to specify what pd is for each file
    if (length(plate) > 1 ){
      pd <- plate[[ plate_files[i] ]]
    } else {
      pd <- plate[[1]]
    }
    
    pl.sum <- 
      full_join(pd %>%
                  filter(PLATE_ID == plate_id) ,
                pl.sum %>%
                  rename(WELL = Well),
                by = c("WELL")
      ) 
    
    # first, we need to extract the blank value
    blank.m <- 
      pl.sum %>%
      filter(TREAT == "BLANK") %>%
      pull(Value.m)
    
    pl.sum$Value.m <- (pl.sum$Value.m - blank.m)
    
    # remove the missing rows i.e. the empty wells and the blank
    pl.sum <- 
      pl.sum %>%
      filter(!is.na(TREAT), !(TREAT == "BLANK"))
    
    # add date information
    date <- 
      pl.id %>%
      filter(Group == "Run started") %>%
      pull(Value)
    
    # add date information
    pl.sum$Date <- as.Date(word(date, 1), tryFormats = "%m/%d/%Y")
    
    # reorder the columns
    pl.sum <- 
      pl.sum %>%
      select(Date, EXP, PLATE_ID, WELL, TREAT, REP, Value.m) %>%
      rename(RFU = Value.m)
    
    # fill up the list for our output
    pl_loop[[i]] <- pl.sum
    
  }
  pl_data <- bind_rows(pl_loop)
  
  # arrange the data
  pl_data <- 
    pl_data %>%
    arrange(Date, EXP, TREAT, REP)
  
  # add in the dilution step
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
  
  # export this file into the data folder
  out.path <- paste(output_path, output_name, sep ="/")
  
  write_csv(pl_data.p, 
            file = out.path)
  
  ### END
  
}

# test the function
plate_reader_cleaner(files = files,
                     file_location = dbox_path,
                     plate = plates,
                     plate_files = pd_order,
                     output_path = here("Data"),
                     output_name = "plate_reader_exp21.csv")


