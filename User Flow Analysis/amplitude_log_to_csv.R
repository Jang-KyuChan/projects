library(dplyr)
library(jsonlite)
library(lubridate)
library(tidyr)
library(purrr)

file_name = '240101-240229'

filenames <- list.files(file_name, pattern="*.json.gz")
filenames = paste(file_name, filenames, sep="/")

is_first_file = TRUE

for (i in 1:length(filenames)){
  print(filenames[i])
  string = filenames[i]
  
  instance <- stream_in(file(string))
  instance = instance[,!(unname(sapply(instance, class)) %in% c('data.frame', 'list'))]
  
  if (is_first_file == TRUE){
    data <- instance
    is_first_file = FALSE
  }
  else{
    data <- dplyr::bind_rows(data, instance)
  }
}

data <- data %>%
  mutate(event_time = as.POSIXct(event_time)) %>%
  arrange(event_time)

write_name = paste(file_name, ".csv", sep = '')

write.csv(data, write_name)

# Validation
data %>%
  filter(event_type == 'CHECK_EVENT') %>%
  mutate(event_time = as.POSIXct(event_time) + hours(9)) %>% # UTC to KST
  mutate(date = as.Date(event_time)) %>%
  group_by(date) %>%
  dplyr::summarize(n = n()) %>%
  View()
