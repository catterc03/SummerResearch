# Name: Christopher Catterick
# Version: 1.02
# Desc: Base program to clean data for 2025 Summer Research. This program cleans data 
# of BC employment assistance cases with respect to CMACAs and also includes number of recips 
# and depchlds. 

library(dplyr)

#Assign Data to represent dataset
Data <- bcea_cmaca 

#Assigning objects for each column
ym <- Data$ym
cmaca <- Data$cmaca 
cases <- Data$cases
recips <- Data$recips
depchld <- Data$depchld

#Function to filter data by date(YYYYMM) and CMACA given by respective dictionary code,
# data: takes in data set to filter
# Area: Takes in string of area code to filter by respective area code
# Min: Takes the min timeframe (YYYYMM)
# Max: Take the max timeframe (YYYYMM)

fdata <- function(data, area, min, max) {
  data |>
    filter(cmaca == area, ym >= min, ym <= max)
}

View(fdata(Data, "907", 200001, 200012))

