#### Data analysis ECOSOC NGO project

# Load packages
library(tidyverse)
library(readxl) 
library(tidyr)

# Clean environment
rm(list = ls())

# Set working directory
setwd("C:/Users/chrstein/Documents/pwicloud_MyDrive/ECOSOC/ECOSOC_Status")

# Load datasets
status_granted_1 <- read_xlsx("./rawdata/1_ecosoc_status_granted.xlsx")
name_changes_2 <- read_xlsx("./rawdata/2_name_changes.xlsx")
closedrequests_failedrespond_3 <- read_xlsx("./rawdata/3_ecosoc_closedrequests_failedrespond.xlsx")
application_withdrawn_4 <- read_xlsx("./rawdata/4_ecosoc_application_withdrawn.xlsx")
status_declined_5 <- read_xlsx("./rawdata/5_ecosoc_status_declined.xlsx")
status_withdrawn_ownrequest_6 <- read_xlsx("./rawdata/6_ecosoc_status_withdrawn_ownrequest.xlsx")
applications_deferred_7 <- read_xlsx("./rawdata/7_applications_deferred.xlsx", col_types = c("guess","guess", "text"))
roll_call_votes_8 <- read_xlsx("./rawdata/8_roll_call_votes.xlsx")
newapplications_status_directly_granted_9 <- read_xlsx("./rawdata/9_newapplications_status_directly_granted.xlsx")
newapplications_deferred_10 <- read_xlsx("./rawdata/10_newapplications_deferred.xlsx")
deferred_status_reclassification_11 <- read_xlsx("./rawdata/11_deferred_status_reclassification.xlsx")
mergers_organizations_12 <- read_xlsx("./rawdata/12_mergers_organizations.xlsx")

# Prepare dates
status_granted_1$session_date <- as.Date(status_granted_1$Session_number, format = "%Y-%m-%d")
name_changes_2$session_date <- as.Date(name_changes_2$Session_number, format = "%Y-%m-%d")
closedrequests_failedrespond_3$session_date <- as.Date(closedrequests_failedrespond_3$Session_number, format = "%Y-%m-%d")
application_withdrawn_4$session_date <- as.Date(application_withdrawn_4$Session_number, format = "%Y-%m-%d")
status_declined_5$session_date <- as.Date(status_declined_5$Session_number, format = "%Y-%m-%d")
status_withdrawn_ownrequest_6$session_date <- as.Date(status_withdrawn_ownrequest_6$Session_number, format = "%Y-%m-%d")
applications_deferred_7$session_date <- as.Date(applications_deferred_7$Session_number, format = "%Y-%m-%d")
roll_call_votes_8$session_date <- as.Date(roll_call_votes_8$Session_number, format = "%Y-%m-%d")
newapplications_status_directly_granted_9$session_date <- as.Date(newapplications_status_directly_granted_9$Session_number, format = "%Y-%m-%d")
newapplications_deferred_10$session_date <- as.Date(newapplications_deferred_10$Session_number, format = "%Y-%m-%d")
deferred_status_reclassification_11$session_date <- as.Date(deferred_status_reclassification_11$Session_number, format = "%Y-%m-%d")
mergers_organizations_12$session_date <- as.Date(mergers_organizations_12$Session_number, format = "%Y-%m-%d")

# Prepare roll call votes data
roll_call_votes_8 <- roll_call_votes_8 %>%
  fill(Vote_outcome, session_date, Session_number, Vote_topic, .direction = "down")

# Prepare additional columns
status_withdrawn_ownrequest_6 <- status_withdrawn_ownrequest_6 %>% rename(info = ...3)
