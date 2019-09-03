# Enrollment file to identify students with multiple enrollments in the absenteeism file

library(tidyverse)

dupes <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/student_chronic_absenteeism_Jun17.csv") %>% 
  filter(str_length(student_id) == 7) %>%
  # Remove duplicates on all columns
  distinct()
