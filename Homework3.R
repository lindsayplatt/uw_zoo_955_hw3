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

lateSeason <- data %>% filter(month > 7)

lateSeason <- lateSeason %>% mutate(changed = (Sex_at_capture == "F" & Sex_at_recapture != "F"))



