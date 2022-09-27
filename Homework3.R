library(xlsx)
library(tidyverse)
library(lubridate)



# reading in the data
data = read.xlsx("BSB_tagging_data.xlsx", sheetIndex = 1)

# removing NA values from the data
data <- data %>% filter(!is.na(Date_at_recapture)) %>% 
  filter(!is.na(Date_at_capture)) %>% 
  filter(!is.na(Length_at_capture))

data <- data %>% mutate(month = month(Date_at_recapture))

# filter for data taken after July
lateSeason <- data %>% filter(month > 7)

# make a column of true/false which says whether a female change sex
lateSeason <- lateSeason %>% mutate(changed = (Sex_at_capture == "F" & Sex_at_recapture != "F"))

#shape 1 = k+1, number of successes (9)
k = sum(lateSeason$changed)
n = nrow(lateSeason)

shape1 = k +1

#shape 2 = n-k+1, where n is total number of observations
shape2 = n-k+1

# make a framework to plot the density against
quantFish = seq(0, 1, 0.01)

# calculating the density, mainly based on n and k
densityFish = dbeta(quantFish, shape1 , shape2)

# plot the density function
plot(quantFish, densityFish)

