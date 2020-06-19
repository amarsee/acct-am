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

# =============== Compare absenteeism =================================
student_absenteeism_am <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/student_chronic_absenteeism_Jun16.csv")
student_absenteeism_sm <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/student_chronic_absenteeism_Jun18_SM.csv")

compare_student <- bind_rows(
  setdiff(student_absenteeism_am, student_absenteeism_sm) %>% mutate(person = 'AM'),
  setdiff(student_absenteeism_sm, student_absenteeism_am) %>% mutate(person = 'SM')
) %>% 
  arrange(student_id, system, school, person)

state_absenteeism_am <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/state_chronic_absenteeism_Jun16.csv")
state_absenteeism_sm <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/state_chronic_absenteeism_Jun18_SM.csv")

compare_state <- bind_rows(
  setdiff(state_absenteeism_am, state_absenteeism_sm) %>% mutate(person = 'AM'),
  setdiff(state_absenteeism_sm, state_absenteeism_am) %>% mutate(person = 'SM')
) %>% 
  arrange(subgroup, grade_band, person)

dist_absenteeism_am <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/district_chronic_absenteeism_Jun16.csv")
dist_absenteeism_sm <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/district_chronic_absenteeism_Jun18_SM.csv")

compare_district <- bind_rows(
  setdiff(dist_absenteeism_am, dist_absenteeism_sm) %>% mutate(person = 'AM'),
  setdiff(dist_absenteeism_sm, dist_absenteeism_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, subgroup, grade_band, person)

school_absenteeism_am <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/school_chronic_absenteeism_Jun16.csv")
school_absenteeism_sm <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/school_chronic_absenteeism_Jun18_SM.csv")

compare_school <- bind_rows(
  setdiff(school_absenteeism_am, school_absenteeism_sm) %>% mutate(person = 'AM'),
  setdiff(school_absenteeism_sm, school_absenteeism_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, school, subgroup, grade_band, person)














