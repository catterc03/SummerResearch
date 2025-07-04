# Name: Christopher Catterick
# Version: 1.1
# Desc: Base program to clean data for 2025 Summer Research. This program cleans data 
# of BC employment assistance cases with respect to CMACAs and also includes number of recips 
# and depchlds. 
#
# This code was developed with assistance from OpenAI's ChatGPT (GPT-4) language model.
# https://openai.com/chatgpt

library(dplyr)

#Assign Data to represent dataset
Data <- bcea_cmaca 
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

# Lists all CMA codes present in the data set
cma_list <- split(agg_yearly, agg_yearly$cmaca)

cma_datasets <- list()

for (cmaca_code in names(cma_list)){
cma_datasets[[cmaca_code]] <- cma_list[[cmaca_code]]
}

print(names(cma_datasets))


#Function to filter data by date(YYYYMM) and CMACA given by respective dictionary code,
# data: takes in data set to filter
# Area: Takes in string of area code to filter by respective area code
# Min: Takes the min timeframe (YYYYMM)
# Max: Take the max timeframe (YYYYMM)

# fdata <- function(data, area, min, max) {
#   data |>
#     filter(cmaca == area, ym >= min, ym <= max)
# }

