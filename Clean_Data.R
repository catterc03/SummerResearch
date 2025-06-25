# Name: Christopher Catterick
# Version: 1.01
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

#Method to filter data by date(YYYYMM) and CMACA given by respective dictionary code
fdata <- Data |>
  filter(cmaca == "905" , ym >= 200001, ym <= 200012)

View(fdata)


