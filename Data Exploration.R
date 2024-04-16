library(readr)
library(tidyverse)
library(ggplot2)
library(sf)
library(broom)

crime <- read.csv("Final Project/crime.csv") %>% 
  select(-c(X))

pop <- read.csv("Final Project/pop.csv")
unemp <- read.csv("Final Project/unemp.csv")
wealth <- read.csv("Final Project/wealth.csv")
ym <- read.csv("Final Project/ym.csv")
colnames(pop) <- c("X", "Population")
colnames(unemp) <- c("X", "Unemployment")
colnames(wealth) <- c("X", "Wealth")
colnames(ym) <- c("X", "YoungMales")

total_burglaries <- data.frame(
  time = colnames(crime),  # Assuming column names represent time
  total = colSums(crime)   # Summing up burglaries for each time period
)

# Convert 'time' column to Date format if needed
total_burglaries$time <- parse_date(total_burglaries$time, format = "count.%Y%m")

# Plot using ggplot
ggplot(total_burglaries, aes(x = time, y = total)) +
  geom_point() +
  labs(x = "Time", y = "Total Burglaries", title = "Total Burglaries Over Time in Chicago") +
  theme_classic()


shape <- read_sf("Final Project/chipocsouth.shp")

area_count <- shape %>% 
  select(count)

area_count %>% 
  ggplot(aes(fill = count)) +
  geom_sf(aes(geometry = geometry)) +
  labs(fill = "Aggregated Burglary Counts", title = "Burglary Counts by Census Block") +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  theme_void()

hist(area_count$count)
hist(pop$x)

area_count$X <- 1:nrow(area_count)

area_data <- area_count %>% 
  left_join(pop, by="X") %>% 
  left_join(unemp, by="X") %>% 
  left_join(wealth, by="X") %>% 
  left_join(ym, by="X")

area_data2 <- area_data %>% 
  select(-c(X))
area_data2$geometry <- NULL

cor_matrix <- cor(area_data2) #Change rating to a numeric
cor_matrix_long <- as.data.frame(as.table(cor_matrix))
colnames(cor_matrix_long) <- c("Var1", "Var2", "Correlation")
ggplot(data = cor_matrix_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "darkblue", midpoint = 0, limits = c(-1, 1)) + 
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
