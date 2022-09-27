library(xlsx)
library(tidyverse)

data = read.xlsx("BSB_tagging_data.xlsx", sheetIndex = 1)

data <- data %>% filter(!is.na(Date_at_recapture)) %>% 
  filter(!is.na(Date_at_capture)) %>% 
  filter(!is.na(Length_at_capture))



