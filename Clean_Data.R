# Name: Christopher Catterick
# Version: 3.0
# Desc: Base program to clean data for 2025 Summer Research. This program cleans data 
# of BC employment assistance cases with respect to top 75 CSDs and also includes number of recips 
# and depchlds. 
#

library(dplyr)
library(ggplot2)
library(tidyverse)
library(MatchIt)

Data <- bcea_cmaca
Data_new <- bcea_top_municipalities

#Mutate Data
Data_new <- Data_new %>%
  mutate(
    year = as.integer(substr(as.character(ym),1 ,4 )),
    csdname = str_trim(str_to_title(as.character(csdname)))
  )

#Create agg_yearly to aggregate the values from raw dataset into a Year based format
agg_yearly <- Data_new %>%
  group_by(csdname, year) %>%
  summarize(
    cases = sum(cases, na.rm = TRUE),
    recips = sum(recips, na.rm = TRUE),
    depchld = sum(depchld, na.rm = TRUE),
    .groups = "drop"
  )

# Creates a lists of CMA codes for split
csd_list <- split(agg_yearly, agg_yearly$csdname)

#outputs a unique CSV file for each CMA
for (code in names(csd_list)){
  write.csv(csd_list[[code]], paste0("CSD_",code, "_yearly.csv"),row.names = FALSE)
}

#ggplot to plot all CMAs included in Data set for comparison. CMA dictionary included in REPO
ggplot(dplyr::filter(agg_yearly),
  mapping = aes(x = year,y = cases, group = csdname, color = csdname)
  ) +
  scale_x_continuous(
    limits = c(1995,2014),
    breaks= seq(1995, 2014, by = 2) 
  ) +
  scale_y_continuous(
    limits = c(0,50000),
    breaks = seq(0,50000, by = 5000)
  ) +
  geom_line(linewidth = 1) +
  labs(title = "BC CSD Employment Assistance Usage",
      x = "Year",
      y = "Total Cases")




#Function to filter data by date(YYYYMM) and CMACA given by respective dictionary code,
# data: takes in data set to filter
# Area: Takes in string of area code to filter by respective area code
# Min: Takes the min timeframe (YYYYMM)
# Max: Take the max timeframe (YYYYMM)
#
# Function is grandfathered

# fdata <- function(data, area, min, max) {
#   data |>
#     filter(cmaca == area, ym >= min, ym <= max)
# }

