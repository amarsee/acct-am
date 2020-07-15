library(tidyverse)
library(lubridate)

# ======================== School Level ============================
school_success <- read_csv("N:/ORP_accountability/projects/2020_amo/success_rate_targets_school.csv") %>% 
  transmute(system, system_name, school, school_name, 
            indicator = 'Success Rate', subgroup,
            grade = 'All Grades', n_students = n_count, prior_metric = metric, 
            amo_target = AMO_target, amo_target_double = AMO_target_double)

school_grad_rate <- read_csv("N:/ORP_accountability/projects/2021_amo/grad_targets_school.csv") %>% 
  transmute(system, system_name, school, school_name, indicator = 'Graduation Rate', subgroup,
            grade = '9th through 12th', n_students = grad_cohort_prior, prior_metric = grad_rate_prior, 
            amo_target = AMO_target, amo_target_double = AMO_target_double)

school_ready_grad <- read_csv("N:/ORP_accountability/projects/2021_amo/ready_grad_targets_school.csv") %>% 
  transmute(system, system_name, school, school_name, indicator = 'Ready Graduate', subgroup,
            grade = '9th through 12th', n_students = grad_count_prior, prior_metric = pct_ready_grad_prior, 
            amo_target = AMO_target, amo_target_double = AMO_target_double)

school_abs <- read_csv("N:/ORP_accountability/projects/2020_amo/absenteeism_targets_school.csv") %>% 
  transmute(system, system_name, school, school_name, indicator = 'Chronic Absenteeism', subgroup,
            grade = 'All Grades', n_students, prior_metric = pct_chronically_absent, 
            amo_target = AMO_target, amo_target_double = AMO_target_double)

school_combined <- bind_rows(school_success, school_grad_rate, school_ready_grad, school_abs) %>% 
  arrange(system, school, indicator)

district_numbers <- sort(unique(school_combined$system))
# Split School Level
school_combined %>%
  split(., .$system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/projects/", year(now())+1, "_amo/split/", .y,
      "_SchoolLevelAMO_", sprintf("%02d", day(now())), month(now(), label = T), year(now()),
      ".csv"
    ), na = "")
  )

# ================================ District Level ================================
district_abs <- read_csv("N:/ORP_accountability/projects/2020_amo/absenteeism_targets_district.csv") %>% 
  transmute(system, system_name, indicator = 'Chronic Absenteeism', subgroup,
            grade = 'All Grades', n_students, prior_metric = pct_chronically_absent, 
            amo_target = AMO_target, amo_target_double = AMO_target_double)

district_elpa <- read_csv("N:/ORP_accountability/projects/2020_amo/elpa_targets_district.csv") %>% 
  transmute(system, system_name, indicator = 'ELPA', subgroup,
            grade = 'All Grades', n_students = growth_standard_denom, prior_metric = pct_met_growth_standard, 
            amo_target = AMO_target, amo_target_double = AMO_target_double)

district_grad_rate <- read_csv("N:/ORP_accountability/projects/2021_amo/grad_targets_district.csv") %>% 
  transmute(system, system_name, indicator = 'Graduation Rate', subgroup,
            grade = '9th through 12th', n_students = grad_cohort_prior, prior_metric = grad_rate_prior, 
            amo_target = AMO_target, amo_target_double = AMO_target_double)

district_success <- read_csv("N:/ORP_accountability/projects/2020_amo/success_rate_targets_district.csv") %>% 
  transmute(system, system_name, indicator = 'Success Rate', subgroup,
            grade, n_students = n_count, prior_metric = metric, 
            amo_target = AMO_target, amo_target_double = AMO_target_double)

district_combined <- bind_rows(district_abs, district_elpa, district_grad_rate, district_success) %>% 
  arrange(system, indicator)

# Split District Level
district_combined %>%
  split(., .$system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/projects/", year(now())+1, "_amo/split/", .y,
      "_DistrictLevelAMO_", sprintf("%02d", day(now())), month(now(), label = T), year(now()),
      ".csv"
    ), na = "")
  )











