# Name: Christopher Catterick
# Version: 1.01
# Desc: Base program to clean data for 2025 Summer Research. This program cleans data 
# of BC employment assistance cases with respect to CMACAs and also includes number of recips 
# and depchlds. 

#Assign Data to represent dataset
Data <- bcea_cmaca 

#Assigning objects for each column
cmaca <- Data$cmaca 
cases <- Data$cases
recips <- Data$recips
depchld <- Data$depchld


