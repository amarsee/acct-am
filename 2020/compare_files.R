# compare files

library(tidyverse)


# ====== Compare Assessment =================
state_am <- read_csv('N:/ORP_accountability/data/2020_final_accountability_files/state_assessment_file.csv')
state_sm <- read_csv('N:/ORP_accountability/data/2020_final_accountability_files/state_assessment_file_SM.csv')

compare_state <- bind_rows(
  setdiff(state_am, state_sm) %>% mutate(person = 'AM'),
  setdiff(state_sm, state_am) %>% mutate(person = 'SM')
) %>% 
  arrange(subject, grade, subgroup, person)

district_am <- read_csv('N:/ORP_accountability/data/2020_final_accountability_files/district_assessment_file.csv')
district_sm <- read_csv('N:/ORP_accountability/data/2020_final_accountability_files/district_assessment_file_SM.csv')

compare_district <- bind_rows(
  setdiff(district_am, district_sm) %>% mutate(person = 'AM'),
  setdiff(district_sm, district_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, subject, grade, subgroup, person)

school_am <- read_csv('N:/ORP_accountability/data/2020_final_accountability_files/school_assessment_file.csv')
school_sm <- read_csv('N:/ORP_accountability/data/2020_final_accountability_files/school_assessment_file_SM.csv')

compare_school <- bind_rows(
  setdiff(school_am, school_sm) %>% mutate(person = 'AM'),
  setdiff(school_sm, school_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, school, subject, grade, subgroup, person)




