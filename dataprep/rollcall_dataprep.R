#### Roll Call Votes - Dataprep

# Load packages
library(tidyverse)
library(scales)
library(lubridate)
library(countrycode)

# Clean environment
rm(list = ls())

# Set working directory
setwd("C:/Users/chrstein/Documents/pwicloud_MyDrive/ECOSOC/ECOSOC_Status")

# Load dataset
load("./cleandata/roll_call_votes_8.RData")
data <- roll_call_votes_8
rm(roll_call_votes_8)

# Extract year-variable for merging of country-year variables
data$year <- year(data$session_date)

# Get rid of NA
data <- data[!is.na(data$Country), ]

# Add COW code numeric to the dataset
data$COW <- countrycode(data$Country, "country.name", "cown")

# Create unique identifier
data$cow_year <- paste(data$COW, "_", data$year)


#### Load and merge external datasets ####

# Load Fariss scores
fariss <- read.csv("./rawdata/fariss/HumanRightsProtectionScores_v4.01.csv")

# Select relevant variables
fariss <- fariss %>% select(COW, YEAR, theta_mean, theta_sd)

# Create unique identifier
fariss$cow_year <- paste(fariss$COW, "_", fariss$YEAR)
fariss$COW <- fariss$YEAR <- NULL

# Left_join Fariss HR scores
data <- left_join(data, fariss, by = "cow_year") ### Note that Fariss only available up to 2019




