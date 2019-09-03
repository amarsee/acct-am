library(readxl)
library(tidyverse)
library(readstata13)

hs_crosswalk <- read_csv("N:/ORP_accountability/projects/Jessica/Data Returns/Helpful Documents/ACT_xwalkAug2018rev.csv")

# act_highest_read <- read.dta13("N:/Assessment_Data Returns/ACT/2017-18/2018 Spring/Final Spring Files/20180717_ACT_JuniorDayResults_SY2017-18_Whalen_v1.dta") %>%
#   filter(!is.na(state_stud_id), test_location != "M", grade == 11) %>%
#   inner_join(hs_crosswalk, by = "acthscode") %>%
#   filter(system != 99855) %>%
#   group_by(state_stud_id) %>%
#   mutate(highest_reading = max(act_read)) %>%
#   ungroup() %>%
#   filter(act_read == highest_reading) %>%
#   transmute(system, school, first_name, last_name, grade, subject = "ACT Reading", state_student_id = state_stud_id, act_subscore = act_read)

act_highest_math <- read_csv("N:/Assessment_Data Returns/ACT/2018-19/2019 Spring/Spring 2019 Final File/20190620_ACT_JuniorDayResults_SY2018-19.csv") %>% 
  filter(!is.na(state_stud_id), test_location != "M", grade == 11) %>%
  inner_join(hs_crosswalk, by = "acthscode") %>%
  filter(system != 99855) %>% 
  group_by(state_stud_id) %>% 
  mutate(highest_math = max(act_math)) %>% 
  ungroup() %>% 
  filter(act_math == highest_math) %>% 
  transmute(system, school, first_name, last_name, grade, subject = "ACT Math", state_student_id = state_stud_id, act_subscore = act_math)

student_level <- read_csv("N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv",
                          col_types = "iciccccccciiiiiccicciiiiiiiiiicciiinii")  

substitution_math <- student_level %>%
  filter(original_subject %in% c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")) %>%
  anti_join(act_highest_math, ., by = "state_student_id")

# substitution_read <- student_level %>%
#   filter(original_subject %in% c("English I", "English II")) %>%
#   anti_join(act_highest_read, ., by = "state_student_id")

substitution_student <- substitution_math %>%
  arrange(system, school, state_student_id, subject)

# write csv student level
write_csv(substitution_student, "N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_student_AM.csv")

school_names <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv")

# ======================= School Level ============================================

substitution_school <- substitution_student %>% 
  mutate(
    valid_test = 1,
    met_benchmark = if_else(act_subscore >= 22, 1, 0),
    not_met_benchmark = if_else(act_subscore < 22, 1, 0)
  ) %>% 
  group_by(system, school, subject) %>% 
  summarise(
    valid_tests = sum(valid_test, na.rm = TRUE),
    n_met_benchmark = sum(met_benchmark, na.rm = TRUE),
    n_not_met_benchmark = sum(not_met_benchmark, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_met_benchmark = round(n_met_benchmark/valid_tests*100+1e-5, 1),
    pct_not_met_benchmark = round(n_not_met_benchmark/valid_tests*100+1e-5, 1)
  ) %>% 
  left_join(school_names, by = c('system', 'school')) %>% 
  filter(!is.na(school)) %>% 
  select(system, system_name, school, school_name, everything())

# write csv for school level
write_csv(substitution_school, "N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_school_AM.csv")

# ================================ District Level ======================

substitution_district <- substitution_student %>% 
  mutate(
    valid_test = 1,
    met_benchmark = if_else(act_subscore >= 22, 1, 0),
    not_met_benchmark = if_else(act_subscore < 22, 1, 0)
  ) %>% 
  group_by(system, subject) %>% 
  summarise(
    valid_tests = sum(valid_test, na.rm = TRUE),
    n_met_benchmark = sum(met_benchmark, na.rm = TRUE),
    n_not_met_benchmark = sum(not_met_benchmark, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_met_benchmark = round(n_met_benchmark/valid_tests*100+1e-5, 1),
    pct_not_met_benchmark = round(n_not_met_benchmark/valid_tests*100+1e-5, 1)
  ) %>% 
  left_join(school_names %>% select(system, system_name) %>% distinct(), by = c('system')) %>% 
  select(system, system_name, everything())

# write district csv
write_csv(substitution_district, "N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_district_AM.csv")

# ====================== State Level =======================================
substitution_state <- substitution_student %>% 
  mutate(
    valid_test = 1,
    met_benchmark = if_else(act_subscore >= 22, 1, 0),
    not_met_benchmark = if_else(act_subscore < 22, 1, 0)
  ) %>% 
  group_by(subject) %>% 
  summarise(
    valid_tests = sum(valid_test, na.rm = TRUE),
    n_met_benchmark = sum(met_benchmark, na.rm = TRUE),
    n_not_met_benchmark = sum(not_met_benchmark, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_met_benchmark = round(n_met_benchmark/valid_tests*100+1e-5, 1),
    pct_not_met_benchmark = round(n_not_met_benchmark/valid_tests*100+1e-5, 1),
    system = 0,
    system_name = 'State of Tennessee'
  ) %>% 
  select(system, system_name, everything())

# Write state level csv
write_csv(substitution_district, "N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_state_AM.csv")


# ====================== Compare ACT substitution ===============================
# Student level
student_alex <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_student.csv")
substitution_student <- substitution_student %>% 
  mutate(act_subscore = as.double(act_subscore))

diff_student <- setdiff(substitution_student, student_alex) %>% 
  bind_rows(setdiff(student_alex, substitution_student)) %>% 
  arrange(system, school, grade, state_student_id)

# School level
school_alex <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_school.csv")


diff_school <- setdiff(substitution_school, school_alex) %>% 
  bind_rows(setdiff(school_alex, substitution_school)) %>% 
  arrange(system, school)

# District level
district_alex <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_district.csv")


diff_district <- setdiff(substitution_district, district_alex) %>% 
  bind_rows(setdiff(district_alex, substitution_district)) %>% 
  arrange(system)

# District level
state_alex <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_state.csv")


diff_state <- setdiff(substitution_state, state_alex) %>% 
  bind_rows(setdiff(state_alex, substitution_state)) %>% 
  arrange(system)






