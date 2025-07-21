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
Data_pop1 <- BC.POP...2001_2011...Sheet1
Data_pop2 <- BC.POP.2011_2024...Sheet1

#Mutate Data
Data_new <- Data_new %>%
  mutate(
    year = as.integer(substr(as.character(ym),1 ,4 )),
    csdname = str_trim(str_to_title(as.character(csdname)))
  )

#Reformat excel sheet data for pop. 2001-2011
pop_long <- Data_pop1 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(
    year = as.integer(sub("X","", year))
  ) %>%
  select(Name, year, population)

#Reformat Excel Sheet data for pop. 2012-2024
pop_long2 <- Data_pop2 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(
    year = as.integer(sub("X","", year))
  ) %>%
  select(Name, year, population)

#combine 2 sheets
combined_pop <- bind_rows(pop_long, pop_long2)


closure_data <- data.frame(
  csdname = c("Mackenzie","Ashcroft","Chetwynd","Lillooet","Bella Coola","Fort St. James","Invermere","Hazelton","Valemount","Revelstoke","Houston",
              "Squamish","Fernie","Burns Lake","Queen Charlotte City","Dease Lake","Sparwood","Creston","Delta","Coquitlam","Sardis","Port Moody",
              "Castlegar","Parksville","Pitt Meadows","Sidney","White Rock","Clearwater","Sooke","Agassiz","Summerland","Kimberly","Aldergrove"),
  closure_year = c(2006,2004,2004,2004,2004,2004,2004,2004,2004,2004,2004,2005,2004,2007,2007,2003,2014,2005,2011,2011,2008,2007,2004,2004,2004,2004,
                   2003,2003,2003,2003,2003,2003,1999)
)

per_capita <-

#Create agg_yearly to aggregate the values from raw datasets
agg_yearly <- Data_new %>%
  group_by(csdname, year) %>%
  summarize(
    cases = sum(cases, na.rm = TRUE),
    recips = sum(recips, na.rm = TRUE),
    depchld = sum(depchld, na.rm = TRUE),
    .groups = "drop"
  )

#add closure data into agg_yearly
agg_yearly <- agg_yearly %>%
  left_join(closure_data, by = "csdname") %>%
  left_join(combined_pop, by = c("csdname" = "Name","year"))

# Creates a lists of CSD codes for split
csd_list <- split(agg_yearly, agg_yearly$csdname)

#Adds post closure and event time
agg_yearly <- agg_yearly %>%
  mutate(post_closure = ifelse(year >= closure_year, 1, 0),
         event_time = year - closure_year
  )

#filter for event time range
agg_trimmed <- agg_yearly %>%
  filter(event_time >= -4 & event_time <= 4)

















#Regression: # of cases on office closure 
cases <- feols(cases ~ sunab(closure_year, year) | csdname + year, data = agg_trimmed)

iplot(cases)

#Event closure plots raw
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
summary(cases)

