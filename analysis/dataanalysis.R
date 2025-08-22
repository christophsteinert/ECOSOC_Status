#### Data analysis ECOSOC NGO project

# Load packages
library(tidyverse)
library(scales)

# Clean environment
rm(list = ls())

# Set working directory
setwd("C:/Users/chrstein/Documents/pwicloud_MyDrive/ECOSOC/ECOSOC_Status")

# Load datasets
load("./cleandata/status_granted_1.RData")
load("./cleandata/name_changes_2.RData")
load("./cleandata/closedrequests_failedrespond_3.RData")
load("./cleandata/application_withdrawn_4.RData")
load("./cleandata/status_declined_5.RData")
load("./cleandata/status_withdrawn_ownrequest_6.RData")
load("./cleandata/applications_deferred_7.RData")
load("./cleandata/roll_call_votes_8.RData")
load("./cleandata/newapplications_status_directly_granted_9.RData")
load("./cleandata/newapplications_deferred_10.RData")
load("./cleandata/deferred_status_reclassification_11.RData")
load("./cleandata/mergers_organizations_12.RData")

# Group datasets by session
status_granted_1_gp <- status_granted_1 %>% group_by(session_date) %>% 
  summarize(count_granted = n())
name_changes_2_gp <- name_changes_2 %>% group_by(session_date) %>% 
  summarize(count_namechange = n())
closedrequests_failedrespond_3_gp <- closedrequests_failedrespond_3 %>% group_by(session_date) %>% 
  summarize(failedrespond = n())
application_withdrawn_4_gp <- application_withdrawn_4 %>% group_by(session_date) %>% 
  summarize(withdrawn = n())
status_declined_5_gp <- status_declined_5 %>% group_by(session_date) %>% 
  summarize(declined = n())
status_withdrawn_ownrequest_6_gp <- status_withdrawn_ownrequest_6 %>% group_by(session_date) %>% 
  summarize(withdrawn = n())
application_deferred_7_gp <- applications_deferred_7 %>% group_by(session_date) %>% 
  summarize(deferred = n())
roll_call_votes_8_gp <- roll_call_votes_8 %>% group_by(session_date) %>% 
  summarize(n_votes = n_distinct(Vote_topic))
newapplications_status_directly_granted_9_gp <- newapplications_status_directly_granted_9 %>% group_by(session_date) %>% 
  summarize(directgrant = n())
newapplications_deferred_10_gp <- newapplications_deferred_10 %>% group_by(session_date) %>% 
  summarize(deferred = n())
deferred_status_reclassification_11_gp <- deferred_status_reclassification_11 %>% group_by(session_date) %>% 
  summarize(deferreclas = n())
mergers_organizations_12_gp <- mergers_organizations_12 %>% group_by(session_date) %>% 
  summarize(mergers = n())


# Plot over time
### Status granted
ggplot(status_granted_1_gp, aes(x = session_date, y = count_granted)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed") +
  labs(title = "ECOSOC status granted over time", x = "Date", y = "# of NGOs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = seq(0, max(status_granted_1_gp$count_granted, na.rm = TRUE), by = 50)
  )
## Name changes
ggplot(name_changes_2_gp, aes(x = session_date, y = count_namechange)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed") +
  labs(title = "Name changes over time", x = "Date", y = "# of NGOs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = seq(0, max(name_changes_2_gp$count_namechange, na.rm = TRUE), by = 4)
  )
## Closed request because failed respond
ggplot(closedrequests_failedrespond_3_gp, aes(x = session_date, y = failedrespond)) +
  geom_line() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed") +
  geom_point() +
  labs(title = "Closed request because failed respond over time", x = "Date", y = "# of NGOs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) 
## Application withdrawn
ggplot(application_withdrawn_4_gp, aes(x = session_date, y = withdrawn)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed") +
  labs(title = "Applications withdrawn over time", x = "Date", y = "# of NGOs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = seq(0, 4, by = 1),
    limits = c(0, 4)
  )
## Status declined
ggplot(status_declined_5_gp, aes(x = session_date, y = declined)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed") +
  labs(title = "ECOSOC status declined over time", x = "Date", y = "# of NGOs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, by = 2),
    limits = c(0, 10)
  )
## Status withdrawn own request
ggplot(status_withdrawn_ownrequest_6_gp, aes(x = session_date, y = withdrawn)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed") +
  labs(title = "Status withdrawn own request over time", x = "Date", y = "# of NGOs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  )  +
  scale_y_continuous(
    breaks = seq(0, 4, by = 1),
    limits = c(0, 4)
  )
## Applications deferred
ggplot(application_deferred_7_gp, aes(x = session_date, y = deferred)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed") +
  labs(title = "NGO Applications Deferred Over Time", x = "", y = "# of deferred NGO applications") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text     = element_text(size = 16)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  )  +
  scale_y_continuous(
    breaks = seq(0, 300, by = 50),
    limits = c(0, 300)
  ) 
## Roll call votes
ggplot(roll_call_votes_8_gp, aes(x = session_date, y = n_votes)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed") +
  labs(title = "Roll Call Votes in the UN Committee on NGOs Over Time", x = "", y = "# of votes") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text     = element_text(size = 16)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  )  +
  scale_y_continuous(
    breaks = seq(0, 10, by = 2),
    limits = c(0, 10)
  ) 
### Status directly granted
newapplications_status_directly_granted_9_gp <- newapplications_status_directly_granted_9_gp %>% filter(session_date > as.Date("2010-01-01"))
ggplot(newapplications_status_directly_granted_9_gp, aes(x = session_date, y = directgrant)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed") +
  labs(title = "ECOSOC status directly granted over time", x = "Date", y = "# of NGOs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = seq(0, max(newapplications_status_directly_granted_9_gp$directgrant, na.rm = TRUE), by = 50)
  )
## New applications deferred
ggplot(newapplications_deferred_10_gp, aes(x = session_date, y = deferred)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed") +
  labs(title = "New NGO Applications Deferred Over Time", x = "", y = "# of deferred NGO applications") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text     = element_text(size = 16)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) 
## Deferred status reclassification
ggplot(deferred_status_reclassification_11_gp, aes(x = session_date, y = deferreclas)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed") +
  labs(title = "Status Reclassifications Deferred Over Time", x = "", y = "# of deferred reclassifications") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text     = element_text(size = 16)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  )
## Mergers over time
ggplot(mergers_organizations_12_gp, aes(x = session_date, y = mergers)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed") +
  labs(title = "Organization Merging Over Time", x = "", y = "# of merges") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text     = element_text(size = 16)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = seq(0, 5, by = 1),
    limits = c(0, 5)
  ) 


#### Check proportion of deferred to status granted
combined_deferred_granted <- merge(status_granted_1_gp, application_deferred_7_gp, by = "session_date", all.x = T, all.y = T)
combined_deferred_granted <- combined_deferred_granted[!is.na(combined_deferred_granted$session_date),]
combined_deferred_granted$defgrant_ratio <- (combined_deferred_granted$deferred / combined_deferred_granted$count_granted)
# Plot
ggplot(combined_deferred_granted, aes(x = session_date, y = defgrant_ratio)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = 1.0, linetype = "dotted", color = "red", size = 1) +
  labs(title = "NGO Applications Deferred to Granted Ratio", x = "", y = "Deferred ÷ Granted") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text     = element_text(size = 16)) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = seq(0, 3, by = 0.5),
    limits = c(0, 3)
  ) 

