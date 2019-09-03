library(tidyverse)
library(janitor)
library(readxl)
library(readstata13)


student_absenteeism <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/student_chronic_absenteeism_AM_Jul11.csv")

k_2_schools <- student_absenteeism %>% 
  mutate(grade = if_else(grade == 'K', '00', grade)) %>% 
  group_by(system, school) %>% 
  summarise(
    max_grade = max(grade, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  filter(max_grade < '03')


