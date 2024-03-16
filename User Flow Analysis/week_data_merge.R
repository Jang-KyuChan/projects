library(tidyverse)
filenames <- list.files('week_data', pattern="*.csv")
filenames = sort(filenames)

is_first_file = 1

for (i in 1:length(filenames)){
  print(paste('week_data/', filenames[i], sep = ''))
  read_file = read.csv(paste('week_data/', filenames[i], sep = ''))
  
  if (is.character(read_file$os_version) == FALSE){
    read_file$os_version = as.character(read_file$os_version)
  }
  
  if (is_first_file == 1){
    data <- read_file
    is_first_file = 0
  }
  else{
    data <- dplyr::bind_rows(data, read_file) # already sorted
  }
}

write.csv(data, "big_data/240101-240229.csv") # update

