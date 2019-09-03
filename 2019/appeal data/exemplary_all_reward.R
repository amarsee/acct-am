library(tidyverse)
library(janitor)
library(readstata13)

school_grades <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades.csv")

district_designation <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_designations.csv")

reward_group <- school_grades %>% 
  group_by(system, system_name) %>% 
  summarise(
    n_schools = n(),
    reward = sum(reward)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_reward = round(reward / n_schools * 100 + 1e-10, 1)
  ) %>% 
  left_join(district_designation %>% select(system, final_determination), by = 'system') %>% 
  arrange(-pct_reward)

