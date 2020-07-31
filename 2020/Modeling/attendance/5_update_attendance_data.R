library(tidyverse)

count <- 0
for (file_name in list.files('Modeling/attendance/data/Models/', full.names = TRUE)) {
  model <- readRDS(file_name)
  count <- count + 1
  print(count)
} 


