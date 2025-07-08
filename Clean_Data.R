# Name: Christopher Catterick
# Version: 1.2
# Desc: Base program to clean data for 2025 Summer Research. This program cleans data 
# of BC employment assistance cases with respect to CMACAs and also includes number of recips 
# and depchlds. 
#

library(dplyr)
library(ggplot2)

#Assign Data to represent dataset
Data <- bcea_cmaca 

#Assigning objects for each column
ym <- Data$ym
cmaca <- Data$cmaca 
cases <- Data$cases
recips <- Data$recips
depchld <- Data$depchld

#Mutate Data to create columns for year and CMA in the correct format
Data <- Data %>%
  mutate(
    year = as.integer(substr(as.character(ym),1 ,4 )),
    cmaca = as.character(cmaca)
  )

#Create agg_yearly to aggregate the values from raw dataset into a Year based format
agg_yearly <- Data %>%
  group_by(cmaca, year) %>%
  summarize(
    cases = sum(cases, na.rm = TRUE),
    recips = sum(recips, na.rm = TRUE),
    depchld = sum(depchld, na.rm = TRUE),
    .groups = "drop"
  )

# Creates a list and seperates the data for each CMA aggregated by year
cma_list <- split(agg_yearly, agg_yearly$cmaca)

#outputs a unique CSV file for each CMA
for (code in names(cma_list)){
  write.csv(cma_list[[code]], paste0("CMA_",code, "_yearly.csv"),row.names = FALSE)
}
  
ggplot(agg_yearly, 
  mapping = aes(x = year,y = cases, group = cmaca, color = cmaca)
  ) +
  geom_line(linewidth = 1) +
  labs(title = "BC CMA Employment Assistance Usage",
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

