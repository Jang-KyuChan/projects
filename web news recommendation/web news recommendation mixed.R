library(tidyverse)

a <- read.csv('web_news_recommendation/result/logP_bias_model_1000_0.01.csv') # 1등
b <- read.csv('web_news_recommendation/result/logP_3_bias_lang_model_1000_0.02.csv')
c <- read.csv('web_news_recommendation/result/logP(-1)_basic_model_1200_0.01_0.2.csv')

a <- a %>%
  mutate(from = 'a')
b <- b %>%
  mutate(from = 'b')
c <- c %>%
  mutate(from = 'c')

data = a %>%
  bind_rows(b) %>% bind_rows(c) %>%
  group_by(userID, articleID) %>%
  mutate(n = n()) %>%
  slice(1) %>%
  arrange(userID, desc(n), from) %>%
  group_by(userID) %>% slice_head(n=5) %>% select(userID, articleID)

data %>% 
  write.csv('web_news_recommendation/result/Ensemble_model_2.csv', row.names = FALSE)

data %>% group_by(userID) %>% summarize(n = n()) %>% filter(n != 5)



