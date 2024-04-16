library(readr)
library(tidyverse)
library(ggplot2)
library(sf)

crime <- read.csv("Final Project/crime.csv")
pop <- read.csv("Final Project/pop.csv")
unemp <- read.csv("Final Project/unemp.csv")
wealth <- read.csv("Final Project/wealth.csv")
ym <- read.csv("Final Project/ym.csv")
colnames(pop) <- c("Block", "Population")
colnames(unemp) <- c("Block", "Unemployment")
colnames(wealth) <- c("Block", "Wealth")
colnames(ym) <- c("Block", "YoungMales")
colnames(crime) <- c("Block", colnames(crime)[2:ncol(crime)])

# https://www.weather.gov/lot/ord_rfd_monthly_yearly_normals
temp <- rep(c(25.2,	28.8,	39.0,	49.7,	60.6,	70.6,	75.4,	73.8,	66.3,	54.0,	41.3,	30.5), 6)

data <- pop %>% 
  left_join(ym, by="Block") %>% 
  left_join(wealth, by="Block") %>% 
  left_join(unemp, by="Block") %>% 
  left_join(crime, by="Block") 


