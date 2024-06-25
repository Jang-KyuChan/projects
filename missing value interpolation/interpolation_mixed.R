library(tidyverse)
rough <- read.csv('result_11.csv')
smooth <- read.csv('result_12.csv')

colnames(rough) <- c('id', 'Rough_Value')
colnames(smooth) <- c('id', 'Smooth_Value')

rough %>%
  left_join(smooth) %>%
  mutate(Value = (Rough_Value + Smooth_Value) / 2) %>%
  select(-c('Rough_Value', 'Smooth_Value')) %>%
  ungroup() %>%
  write.csv('result_mixed.csv', row.names = FALSE)

