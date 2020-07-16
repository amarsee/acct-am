library(tidyverse)
library(janitor)
library(acct)

# ============ School and District Names =================
school_names <- read_csv("N:/ORP_accountability/data/2020_final_accountability_files/names.csv")
dist_names <- school_names %>% 
  select(system, system_name) %>% 
  distinct()

# ============= School Level ==========
# Read in last 3 Years of AMOs
absenteeism_2018_school <- read_csv("N:/ORP_accountability/projects/2018_amo/school_chronic_absenteeism.csv") %>% 
  transmute(
    system, school, 
    subgroup = if_else(subgroup == 'English Learners', 'English Learners with Transitional 1-4', subgroup),
    n_students_2018 = n_students, n_chronically_absent_2018 = n_chronically_absent,  
    pct_chronically_absent_2018 = pct_chronically_absent
  )
absenteeism_2019_school <- read_csv("N:/ORP_accountability/projects/2019_amo/absenteeism_targets_school.csv") %>% 
  transmute(
    system, school, subgroup,
    n_students_2019 = n_students, n_chronically_absent_2019 = n_chronically_absent,  
    pct_chronically_absent_2019 = pct_chronically_absent
  )
absenteeism_2020_school <- read_csv("N:/ORP_accountability/projects/2020_amo/absenteeism_targets_school.csv") %>% 
  transmute(
    system, school, subgroup,
    n_students_2020 = n_students, n_chronically_absent_2020 = n_chronically_absent,  
    pct_chronically_absent_2020 = pct_chronically_absent
  )

# Create New AMOs
school_absenteeism_3_year_amo <- absenteeism_2020_school %>% 
  left_join(absenteeism_2019_school, by = c('system', 'school', 'subgroup')) %>% 
  left_join(absenteeism_2018_school, by = c('system', 'school', 'subgroup')) %>% 
  rowwise() %>% 
  mutate(
    pct_chronically_absent_3_year_avg = round(mean(c(pct_chronically_absent_2018,
                                             pct_chronically_absent_2019,
                                             pct_chronically_absent_2020), na.rm = TRUE) + 1e-5, 1)
  ) %>% 
  ungroup() %>% 
  mutate(
    AMO_target = if_else(
      n_students_2020 >= 30,
      round(pct_chronically_absent_3_year_avg - (pct_chronically_absent_3_year_avg / 16) + 1e-5, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      n_students_2020 >= 30,
      round(pct_chronically_absent_3_year_avg - (pct_chronically_absent_3_year_avg / 8) + 1e-5, 1),
      NA_real_
    )
  ) %>% 
  inner_join(school_names, by = c('system', 'school')) %>% 
  select(system, system_name, school, school_name, everything())

write_csv(school_absenteeism_3_year_amo, 'N:/ORP_accountability/projects/2021_amo/absenteeism_targets_school_3_year.csv', na = '')

# ============= District Level ==========
# Read in last 3 Years of AMOs
absenteeism_2018_district <- read_csv("N:/ORP_accountability/projects/2018_amo/system_chronic_absenteeism.csv") %>% 
  transmute(
    system, 
    subgroup = if_else(subgroup == 'English Learners', 'English Learners with Transitional 1-4', subgroup),
    grade_band,
    n_students_2018 = n_students, n_chronically_absent_2018 = n_chronically_absent,  
    pct_chronically_absent_2018 = pct_chronically_absent
  )
absenteeism_2019_district <- read_csv("N:/ORP_accountability/projects/2019_amo/absenteeism_targets_district.csv") %>% 
  transmute(
    system, subgroup, grade_band,
    n_students_2019 = n_students, n_chronically_absent_2019 = n_chronically_absent,  
    pct_chronically_absent_2019 = pct_chronically_absent
  ) %>% 
  filter(grade_band == 'All Grades')
absenteeism_2020_district <- read_csv("N:/ORP_accountability/projects/2020_amo/absenteeism_targets_district.csv") %>% 
  transmute(
    system, subgroup, grade_band,
    n_students_2020 = n_students, n_chronically_absent_2020 = n_chronically_absent,  
    pct_chronically_absent_2020 = pct_chronically_absent
  )

# Create New AMOs
district_absenteeism_3_year_amo <- absenteeism_2020_district %>% 
  left_join(absenteeism_2019_district, by = c('system', 'subgroup', 'grade_band')) %>% 
  left_join(absenteeism_2018_district, by = c('system', 'subgroup', 'grade_band')) %>% 
  rowwise() %>% 
  mutate(
    pct_chronically_absent_3_year_avg = round(mean(c(pct_chronically_absent_2018,
                                                     pct_chronically_absent_2019,
                                                     pct_chronically_absent_2020), na.rm = TRUE) + 1e-5, 1)
  ) %>% 
  ungroup() %>% 
  mutate(
    AMO_target = if_else(
      n_students_2020 >= 30,
      round(pct_chronically_absent_3_year_avg - (pct_chronically_absent_3_year_avg / 16) + 1e-5, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      n_students_2020 >= 30,
      round(pct_chronically_absent_3_year_avg - (pct_chronically_absent_3_year_avg / 8) + 1e-5, 1),
      NA_real_
    )
  ) %>% 
  inner_join(dist_names, by = c('system')) %>% 
  select(system, system_name, everything())

write_csv(district_absenteeism_3_year_amo, 'N:/ORP_accountability/projects/2021_amo/absenteeism_targets_district_3_year.csv', na = '')










