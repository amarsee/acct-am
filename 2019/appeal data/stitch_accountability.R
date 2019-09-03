library(tidyverse)
library(janitor)
library(readxl)
library(readstata13)
library(acct)

schools_to_update <- read_excel("N:/ORP_accountability/projects/Andrew/Accountability/2019/appeal data/score_changes.xlsm") %>% 
  filter(!is.na(system))

# ============= ACCT file ==========================

old_acct <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv")

new_acct <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file_AM_Aug14.csv")

old_remaining <- old_acct %>% 
  anti_join(schools_to_update, by = c('system', 'school'))

new_to_merge <- new_acct %>% 
  inner_join(schools_to_update %>% select(system, school), by = c('system', 'school'))

stitched_output <- bind_rows(old_remaining, new_to_merge) %>% 
  arrange(system, school, indicator, subgroup) %>% 
  distinct()

write_csv(stitched_output, "N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file_AM_Aug16_stitched.csv", na = '')


# ============= school grading grades file ==========================

old_grades <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades_Jul29.csv")

# new_grades <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades_AM_Aug14.csv")
new_grades <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades_aug14.csv")

old_remaining_grades <- old_grades %>% 
  anti_join(schools_to_update, by = c('system', 'school'))

new_to_merge_grades <- new_grades %>% 
  inner_join(schools_to_update %>% select(system, school), by = c('system', 'school'))

stitched_output_grades <- bind_rows(old_remaining_grades, new_to_merge_grades) %>% 
  arrange(system, school) %>% 
  distinct()

write_csv(stitched_output_grades, "N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades_AM_Aug16_stitched.csv", na = '')


# ============= school grading metrics file ==========================

old_metrics <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_metrics_Jul29.csv")

# new_metrics <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_metrics_AM_Aug14.csv")
new_metrics <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_metrics_aug14.csv")

old_remaining_metrics <- old_metrics %>% 
  anti_join(schools_to_update, by = c('system', 'school'))

new_to_merge_metrics <- new_metrics %>% 
  inner_join(schools_to_update %>% select(system, school), by = c('system', 'school'))

stitched_output_metrics <- bind_rows(old_remaining_metrics, new_to_merge_metrics) %>% 
  arrange(system, school) %>% 
  distinct()

write_csv(stitched_output_metrics, "N:/ORP_accountability/projects/2019_school_accountability/school_grading_metrics_AM_Aug16_stitched.csv", na = '')










