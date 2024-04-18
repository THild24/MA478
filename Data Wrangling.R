library(readr)
library(tidyverse)
library(ggplot2)
library(sf)
library(lubridate)

crime <- read.csv("crime.csv")
pop <- read.csv("pop.csv")
unemp <- read.csv("unemp.csv")
wealth <- read.csv("wealth.csv")
ym <- read.csv("ym.csv")
colnames(pop) <- c("Block", "Population")
colnames(unemp) <- c("Block", "Unemployment")
colnames(wealth) <- c("Block", "Wealth")
colnames(ym) <- c("Block", "YoungMales")
colnames(crime) <- c("Block", colnames(crime)[2:ncol(crime)])


data <- pop %>% 
  left_join(ym, by="Block") %>% 
  left_join(wealth, by="Block") %>% 
  left_join(unemp, by="Block") %>% 
  left_join(crime, by="Block") 


data_long <- data %>%
  pivot_longer(cols = starts_with("count"),  # Assuming date columns start with "date_"
               names_to = "date",
               values_to = "observation") 
data_long$date <- parse_date(data_long$date, format = "count.%Y%m")

data_long <- data_long %>% 
  mutate(year = year(date)) %>% 
  mutate(month_number = (year - 2010) * 12 + month(date) - 1) %>%
  mutate(month_name = month(date, label = TRUE, abbr = TRUE))

# https://www.weather.gov/lot/ord_rfd_monthly_yearly_normals
temp <- c(25.2,	28.8,	39.0,	49.7,	60.6,	70.6,	75.4,	73.8,	66.3,	54.0,	41.3,	30.5)
month_name <- unique(data_long$month_name)
temp_df <- data.frame(
  month_name,
  temp
)

data_long <- data_long %>% 
  left_join(temp_df, by="month_name") %>% 
  mutate(wealth_percapita = Wealth / Population) %>% 
  mutate(ym_prop = YoungMales / Population)

