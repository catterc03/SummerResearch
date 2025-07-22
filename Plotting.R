# Name: Christopher Catterick
# Version: 1.0
# Desc: R Script to create plots unrelated to Regressions 
#


library(dplyr)
library(ggplot2)
library(tidyverse)
library(MatchIt)
library(fixest)

d <- OfficesPerYear

d <- d %>%
  slice(-c(1,2,3,4,5,6,7,25,26,27,28,29),)

p <- ggplot(d, aes(x = Year, y = Number.of.Offices)) +
  labs(y = "Number of Offices", x = "Year", title = "Number of Field Offices in Operation by Year") +
  geom_line(color = "black", linewidth = 0.75) + 
  scale_y_continuous(breaks = seq(0,160, by = 20), limits = c(0,160)) +
  scale_x_continuous(breaks = seq(1998,2014, by = 2)) +
  theme_classic()

p

