library(tidyverse)

data <- read.csv('open/data.csv')

data <- data %>%
  mutate(numeric_id = as.integer(substr(id, 8, 12)))

interpolated_data <- data %>%
  mutate(weight = 0, weight_sum = 0)

max_confidence = 10

find_weight <- function(dist, min_dist){
  # return(1 / (1 + (abs(dist) / abs(min_dist))^2.5))
  return (1 / (1 + abs(dist)^3))
  # return (1 - (dist / max_dist)^2) # Epanechnikov
}

confidence <- function(dist){
  if (abs(dist) < 10){
    return(1)
  }
  if (abs(dist) < 20){
    return(1)
  }
  else{return(1)}
}


find_value <- function(id, max_confidence, left_mask, right_mask){
  weight = 0
  weight_sum = 0
  left_number = c()
  right_number = c()
  
  # left
  left_confidence = 0
  left_pointer = ifelse(id - left_mask <= 0, id - 1, id - left_mask)
  while (left_confidence < max_confidence){
    if (left_pointer < 0){break}
    if (!is.na(data$Value[left_pointer + 1])){
      left_confidence <- left_confidence + confidence(id - left_pointer)
      #weight <- weight + find_weight(id - left_pointer)
      #weight_sum <- weight_sum + find_weight(id - left_pointer) * data$Value[left_pointer + 1]
      left_number = append(left_number, id - left_pointer)
    }
    left_pointer <- left_pointer - 1
    }
  
  # right
  right_confidence = 0
  right_pointer = ifelse(id + right_mask >= length(data$id) - 1, id + 1, id + right_mask)
  while (right_confidence < max_confidence){
    if (right_pointer > length(data$id) - 1){break}
    if (!is.na(data$Value[right_pointer + 1])){
      right_confidence <- right_confidence + confidence(right_pointer - id)
      #weight <- weight + find_weight(right_pointer - id)
      #weight_sum <- weight_sum + find_weight(right_pointer - id) * data$Value[right_pointer + 1]
      right_number = append(right_number, right_pointer - id)
    }
    right_pointer <- right_pointer + 1
  }
  
  # calculate weight
  min_dist = min(min(left_number), min(right_number), na.rm = TRUE)
  if (length(left_number) >= 1){
    for (i in 1:length(left_number)){
      idx = left_number[i]
      weight <- weight + find_weight(idx, min_dist)
      weight_sum <- weight_sum + find_weight(idx, min_dist) * data$Value[id - idx + 1]
    }
  }
  if (length(right_number) >= 1){
    for (i in 1:length(right_number)){
      idx = right_number[i]
      weight <- weight + find_weight(idx, min_dist)
      weight_sum <- weight_sum + find_weight(idx, min_dist) * data$Value[id + idx + 1]
    }
  }
  print(paste0(id + 1, "/", length(data$id)))
  return(c(weight_sum, weight))
}

# mask_data <- read.csv('mask_data.csv')

for (i in 0:(length(data$id)-1)){ 
  #a = find_value(i, max_confidence, mask_data$left_mask[i + 1], mask_data$right_mask[i + 1])
  a = find_value(i, max_confidence, 1, 1)
  interpolated_data$weight_sum[i + 1] <- a[1]
  interpolated_data$weight[i + 1] <- a[2]
  }

interpolated_data <- interpolated_data %>%
  mutate(estimated_value = weight_sum / weight) %>%
  mutate(integrated_value = ifelse(!is.na(Value), Value, estimated_value))

# MSE (train)
interpolated_data %>%
  filter(!is.na(Value)) %>%
  summarize(SSE = sum((Value - estimated_value)^2))

interpolated_data %>%
  filter(!is.na(Value)) %>%
  summarize(RMSE = sqrt(mean((Value - estimated_value)^2)))

# submit
interpolated_data %>%
  select(id, integrated_value) %>%
  rename(Value = integrated_value) %>%
  write.csv('result_13.csv', row.names = FALSE)
