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
library(fixest)

Data <- bcea_cmaca
Data_new <- bcea_top_municipalities

#Mutate Data
Data_new <- Data_new %>%
  mutate(
    year = as.integer(substr(as.character(ym),1 ,4 )),
    csdname = str_trim(str_to_title(as.character(csdname)))
  )

closure_data <- data.frame(
  csdname = c("Mackenzie","Ashcroft","Chetwynd","Lillooet","Bella Coola","Fort St. James","Invermere","Hazelton","Valemount","Revelstoke","Houston",
              "Squamish","Fernie","Burns Lake","Queen Charlotte City","Dease Lake","Sparwood","Creston","Delta","Coquitlam","Sardis","Port Moody",
              "Castlegar","Parksville","Pitt Meadows","Sidney","White Rock","Clearwater","Sooke","Agassiz","Summerland","Kimberly","Aldergrove"),
  closure_year = c(2006,2004,2004,2004,2004,2004,2004,2004,2004,2004,2004,2005,2004,2007,2007,2003,2014,2005,2011,2011,2008,2007,2004,2004,2004,2004,
                   2003,2003,2003,2003,2003,2003,1999)
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

agg_yearly <- agg_yearly %>%
  left_join(closure_data, by = "csdname")

# Creates a lists of CSD codes for split
csd_list <- split(agg_yearly, agg_yearly$csdname)

#outputs a unique CSV file for each CSD
#for (code in names(csd_list)){
 # write.csv(csd_list[[code]], paste0("CSD_",code, "_yearly.csv"),row.names = FALSE)
#}

agg_yearly <- agg_yearly %>%
  mutate(post_closure = ifelse(year >= closure_year, 1, 0),
         event_time = year - closure_year
  )

agg_trimmed <- agg_yearly %>%
  filter(event_time >= -12 & event_time <= 12)

cases <- feols(cases ~ sunab(closure_year, year) | csdname + year, data = agg_trimmed)

iplot(cases)


ggplot(agg_trimmed, aes(x = event_time, y = cases)) +
  geom_line(color = "black", linewidth =0.75 ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red" )+
  facet_wrap(~ csdname, scales = "free_y",) +
  labs(
    Title = "BC Employment Assistance Office Closures Effect on Cases",
    x = "Years Since Closure",
    y = "Total Cases"
  )

etable(cases)
  

# #ggplot to plot all CSDs included in Data set for comparison.
# ggplot(dplyr::filter(agg_yearly, csdname %in% c("Pitt Meadows")),
#   aes(x = year,y = cases, group = csdname, color = csdname)
#   ) +
#   scale_x_continuous(
#     limits = c(1995,2014),
#     breaks= seq(1995, 2014, by = 1)
#   ) +
#   scale_y_continuous(
#     limits = c(0,10000),
#     breaks = seq(0,10000, by = 1000)
#   ) +
#   geom_line(linewidth = 1) +
#   labs(title = "BC CSD Employment Assistance Usage",
#       x = "Year",
#       y = "Total Cases") +
#       geom_vline(data = filter(agg_yearly, csdname == "Pitt Meadows"), aes(xintercept = closure_year),
#                  linetype = "dashed", color = "red", linewidth = 1)




summary(cases)



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

