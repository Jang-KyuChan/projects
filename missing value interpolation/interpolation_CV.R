library(tidyverse)

data <- read.csv('open/data.csv')

data <- data %>%
  mutate(numeric_id = as.integer(substr(id, 8, 12)))

interpolated_data <- data %>%
  mutate(weight = 0, weight_sum = 0, min_dist = 0)

max_confidence = 5

find_weight <- function(dist){
  return(1 / abs(dist)^1.6)
}

confidence <- function(dist){
  if (abs(dist) < 10){
    return(1)
  }
  if (abs(dist) < 20){
    return(0.5)
  }
  else{return(0.5)}
}


find_value <- function(id, max_confidence){
  weight = 0
  weight_sum = 0
  min_dist = 100000
  # left
  left_confidence = 0
  left_pointer = id - 1
  while (left_confidence < max_confidence){
    if (left_pointer <= 0){break}
    if (!is.na(data$Value[left_pointer + 1])){
      left_confidence <- left_confidence + confidence(id - left_pointer)
      weight <- weight + find_weight(id - left_pointer)
      weight_sum <- weight_sum + find_weight(id - left_pointer) * data$Value[left_pointer + 1]
      min_dist = min(min_dist, id - left_pointer)
    }
    left_pointer <- left_pointer - 1
  }
  
  # right
  right_confidence = 0
  right_pointer = id + 1
  while (right_confidence < max_confidence){
    if (right_pointer >= length(data$id)){break}
    if (!is.na(data$Value[right_pointer + 1])){
      right_confidence <- right_confidence + confidence(right_pointer - id)
      weight <- weight + find_weight(right_pointer - id)
      weight_sum <- weight_sum + find_weight(right_pointer - id) * data$Value[right_pointer + 1]
      min_dist <- min(min_dist, right_pointer - id)
    }
    right_pointer <- right_pointer + 1
  }
  
  print(paste0(id + 1, "/", length(data$id)))
  return(c(weight_sum, weight, min_dist))
  
}

for (i in 0:(length(data$id)-1)){
  a = find_value(i, max_confidence)
  interpolated_data$weight_sum[i + 1] <- a[1]
  interpolated_data$weight[i + 1] <- a[2]
  interpolated_data$min_dist[i + 1] <- a[3]
}

interpolated_data <- interpolated_data %>%
  mutate(estimated_value = weight_sum / weight) %>%
  mutate(integrated_value = ifelse(!is.na(Value), Value, estimated_value))

# MSE (train)
a = interpolated_data %>%
  filter(!is.na(Value)) %>%
  summarize(MSE = sum((Value - estimated_value)^2))



print(c(a, b, c))

interpolated_data


