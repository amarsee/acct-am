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


# =============== Compare Ready Grad =================================
student_ready_grad_am <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_student_level.csv",
                                  col_types = 'icccciccciiciiiiiiiiiiiiiiiiiiic')
student_ready_grad_sm <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_grad_student_sm.csv",
                                  col_types = 'icccciccciiciiiiiiiiiiiiiiiiiiic')

compare_student <- bind_rows(
  setdiff(student_ready_grad_am, student_ready_grad_sm) %>% mutate(person = 'AM'),
  setdiff(student_ready_grad_sm, student_ready_grad_am) %>% mutate(person = 'SM')
) %>% 
  arrange(student_key, district_no, school_no, person)

state_ready_grad_am <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_state.csv")
state_ready_grad_sm <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_grad_state_sm.csv")

compare_state <- bind_rows(
  setdiff(state_ready_grad_am, state_ready_grad_sm) %>% mutate(person = 'AM'),
  setdiff(state_ready_grad_sm, state_ready_grad_am) %>% mutate(person = 'SM')
) %>% 
  arrange(subgroup, person)

dist_ready_grad_am <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_district.csv")
dist_ready_grad_sm <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_grad_district_sm.csv")

compare_district <- bind_rows(
  setdiff(dist_ready_grad_am, dist_ready_grad_sm) %>% mutate(person = 'AM'),
  setdiff(dist_ready_grad_sm, dist_ready_grad_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, subgroup, person)

school_ready_grad_am <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_school.csv")
school_ready_grad_sm <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_grad_school_sm.csv")

compare_school <- bind_rows(
  setdiff(school_ready_grad_am, school_ready_grad_sm) %>% mutate(person = 'AM'),
  setdiff(school_ready_grad_sm, school_ready_grad_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, school, subgroup, person)

school_targets_ready_grad_am <- read_csv("N:/ORP_accountability/projects/2021_amo/ready_grad_targets_school.csv")
school_targets_ready_grad_sm <- read_csv("N:/ORP_accountability/projects/Sophie/schooltargets2020.csv")

compare_targets <- bind_rows(
  setdiff(school_targets_ready_grad_am, school_targets_ready_grad_sm) %>% mutate(person = 'AM'),
  setdiff(school_targets_ready_grad_sm, school_targets_ready_grad_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, school, subgroup, person)








