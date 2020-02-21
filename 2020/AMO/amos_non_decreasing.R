library(tidyverse)
library(janitor)

# ============================== Success Rate ===================================
district_current_ach <- read_csv("N:/ORP_accountability/projects/2020_amo/success_rate_targets_district_AM.csv")
district_prior_ach <- read_csv("N:/ORP_accountability/projects/2019_amo/success_rate_targets_district.csv") %>% 
  select(system:subgroup, AMO_target_prior = AMO_target, AMO_target_double_prior = AMO_target_double)

# Keep highest between the two (no decreasing AMOs)
district_amo_ach <- district_current_ach %>% 
  left_join(district_prior_ach, by = c('system', 'grade', 'subgroup')) %>% 
  mutate(
    AMO_target_new = case_when(
      !is.na(AMO_target) & !is.na(AMO_target_prior) ~ pmax(AMO_target, AMO_target_prior),
      !is.na(AMO_target) & is.na(AMO_target_prior) ~ AMO_target,
      is.na(AMO_target) & !is.na(AMO_target_prior) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    AMO_target_double_new = case_when(
      !is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ pmax(AMO_target_double, AMO_target_double_prior),
      !is.na(AMO_target_double) & is.na(AMO_target_double_prior) ~ AMO_target_double,
      is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(system:metric, AMO_target = AMO_target_new, AMO_target_double = AMO_target_double_new)


write_csv(district_amo_ach, "N:/ORP_accountability/projects/2020_amo/success_rate_targets_district_v2_AM.csv", na = "")


school_current_ach <- read_csv("N:/ORP_accountability/projects/2020_amo/success_rate_targets_school_AM.csv")
school_prior_ach <- read_csv("N:/ORP_accountability/projects/2019_amo/success_rate_targets_school.csv") %>% 
  select(system:subgroup, AMO_target_prior = AMO_target, AMO_target_double_prior = AMO_target_double)

# Keep highest between the two (no decreasing AMOs)
school_amo_ach <- school_current_ach %>% 
  left_join(school_prior_ach, by = c('system', 'school', 'subgroup')) %>% 
  mutate(
    AMO_target_new = case_when(
      !is.na(AMO_target) & !is.na(AMO_target_prior) ~ pmax(AMO_target, AMO_target_prior),
      !is.na(AMO_target) & is.na(AMO_target_prior) ~ AMO_target,
      is.na(AMO_target) & !is.na(AMO_target_prior) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    AMO_target_double_new = case_when(
      !is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ pmax(AMO_target_double, AMO_target_double_prior),
      !is.na(AMO_target_double) & is.na(AMO_target_double_prior) ~ AMO_target_double,
      is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(system:metric, AMO_target = AMO_target_new, AMO_target_double = AMO_target_double_new)


write_csv(school_amo_ach, "N:/ORP_accountability/projects/2020_amo/success_rate_targets_school_v2_AM.csv", na = "")

# ============================== Absenteeism ===================================
district_current_absenteeism <- read_csv("N:/ORP_accountability/projects/2020_amo/absenteeism_targets_district_AM.csv")
district_prior_absenteeism <- read_csv("N:/ORP_accountability/projects/2019_amo/absenteeism_targets_district_primary_enrollment.csv") %>% 
  filter(grade_band == 'All Grades') %>% 
  select(system:grade_band, AMO_target_prior = AMO_target, AMO_target_double_prior = AMO_target_double, -system_name)

# Keep highest between the two (no decreasing AMOs)
district_amo_absenteeism <- district_current_absenteeism %>% 
  left_join(district_prior_absenteeism, by = c('system', 'subgroup', 'grade_band')) %>% 
  mutate(
    AMO_target_new = case_when(
      !is.na(AMO_target) & !is.na(AMO_target_prior) ~ pmin(AMO_target, AMO_target_prior),
      !is.na(AMO_target) & is.na(AMO_target_prior) ~ AMO_target,
      is.na(AMO_target) & !is.na(AMO_target_prior) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    AMO_target_double_new = case_when(
      !is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ pmin(AMO_target_double, AMO_target_double_prior),
      !is.na(AMO_target_double) & is.na(AMO_target_double_prior) ~ AMO_target_double,
      is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(system:pct_chronically_absent, AMO_target = AMO_target_new, AMO_target_double = AMO_target_double_new)


write_csv(district_amo_absenteeism, "N:/ORP_accountability/projects/2020_amo/absenteeism_targets_district_v2_AM.csv", na = "")


school_current_absenteeism <- read_csv("N:/ORP_accountability/projects/2020_amo/absenteeism_targets_school_AM.csv")
school_prior_absenteeism <- read_csv("N:/ORP_accountability/projects/2019_amo/absenteeism_targets_school_primary_enrollment.csv") %>% 
  select(system, school, subgroup, grade_band, AMO_target_prior = AMO_target, AMO_target_double_prior = AMO_target_double)

# Keep highest between the two (no decreasing AMOs)
school_amo_absenteeism <- school_current_absenteeism %>% 
  left_join(school_prior_absenteeism, by = c('system', 'school', 'subgroup', 'grade_band')) %>% 
  mutate(
    AMO_target_new = case_when(
      !is.na(AMO_target) & !is.na(AMO_target_prior) ~ pmin(AMO_target, AMO_target_prior),
      !is.na(AMO_target) & is.na(AMO_target_prior) ~ AMO_target,
      is.na(AMO_target) & !is.na(AMO_target_prior) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    AMO_target_double_new = case_when(
      !is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ pmin(AMO_target_double, AMO_target_double_prior),
      !is.na(AMO_target_double) & is.na(AMO_target_double_prior) ~ AMO_target_double,
      is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(system:pct_chronically_absent, AMO_target = AMO_target_new, AMO_target_double = AMO_target_double_new)


write_csv(school_amo_absenteeism, "N:/ORP_accountability/projects/2020_amo/absenteeism_targets_school_v2_AM.csv", na = "")

# ============================== Grad Rate ===================================
district_current_grad <- read_csv("N:/ORP_accountability/projects/2020_amo/grad_targets_district_AM.csv")
district_prior_grad <- read_csv("N:/ORP_accountability/projects/2019_amo/grad_district.csv") %>% 
  mutate(
    subgroup = case_when(
      subgroup == 'English Learners' ~ 'English Learners with Transitional 1-4',
      TRUE ~ subgroup
    )
  ) %>% 
  select(system, subgroup, AMO_target_prior = AMO_target, AMO_target_double_prior = AMO_target_double)

# Keep highest between the two (no decreasing AMOs)
district_amo_grad <- district_current_grad %>% 
  left_join(district_prior_grad, by = c('system', 'subgroup')) %>% 
  mutate(
    AMO_target_new = case_when(
      !is.na(AMO_target) & !is.na(AMO_target_prior) ~ pmax(AMO_target, AMO_target_prior),
      !is.na(AMO_target) & is.na(AMO_target_prior) ~ AMO_target,
      is.na(AMO_target) & !is.na(AMO_target_prior) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    AMO_target_double_new = case_when(
      !is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ pmax(AMO_target_double, AMO_target_double_prior),
      !is.na(AMO_target_double) & is.na(AMO_target_double_prior) ~ AMO_target_double,
      is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(system:grad_rate, AMO_target = AMO_target_new, AMO_target_double = AMO_target_double_new)


write_csv(district_amo_grad, "N:/ORP_accountability/projects/2020_amo/grad_targets_district_v2_AM.csv", na = "")


school_current_grad <- read_csv("N:/ORP_accountability/projects/2020_amo/grad_targets_school_AM.csv")
school_prior_grad <- read_csv("N:/ORP_accountability/projects/2019_amo/grad_school.csv") %>% 
  mutate(
    subgroup = case_when(
      subgroup == 'English Learners' ~ 'English Learners with Transitional 1-4',
      TRUE ~ subgroup
    )
  ) %>% 
  select(system, school, subgroup, AMO_target_prior = AMO_target, AMO_target_double_prior = AMO_target_double)

# Keep highest between the two (no decreasing AMOs)
school_amo_grad <- school_current_grad %>% 
  left_join(school_prior_grad, by = c('system', 'school', 'subgroup')) %>% 
  mutate(
    AMO_target_new = case_when(
      !is.na(AMO_target) & !is.na(AMO_target_prior) ~ pmax(AMO_target, AMO_target_prior),
      !is.na(AMO_target) & is.na(AMO_target_prior) ~ AMO_target,
      is.na(AMO_target) & !is.na(AMO_target_prior) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    AMO_target_double_new = case_when(
      !is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ pmax(AMO_target_double, AMO_target_double_prior),
      !is.na(AMO_target_double) & is.na(AMO_target_double_prior) ~ AMO_target_double,
      is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(system:grad_rate, AMO_target = AMO_target_new, AMO_target_double = AMO_target_double_new)


write_csv(school_amo_grad, "N:/ORP_accountability/projects/2020_amo/grad_targets_school_v2_AM.csv", na = "")

# ============================== Ready Grad ===================================
# School Level Only
school_current_ready_grad <- read_csv("N:/ORP_accountability/projects/2020_amo/ready_grad_targets_school_AM.csv")
school_prior_ready_grad <- read_csv("N:/ORP_accountability/projects/2019_amo/ready_grad_school.csv") %>% 
  mutate(
    subgroup = case_when(
      subgroup == 'English Learners' ~ 'English Learners with Transitional 1-4',
      TRUE ~ subgroup
    )
  ) %>% 
  select(system, school, subgroup, AMO_target_prior = AMO_target, AMO_target_double_prior = AMO_target_double)

# Keep highest between the two (no decreasing AMOs)
school_amo_ready_grad <- school_current_ready_grad %>% 
  left_join(school_prior_ready_grad, by = c('system', 'school', 'subgroup')) %>% 
  mutate(
    AMO_target_new = case_when(
      !is.na(AMO_target) & !is.na(AMO_target_prior) ~ pmax(AMO_target, AMO_target_prior),
      !is.na(AMO_target) & is.na(AMO_target_prior) ~ AMO_target,
      is.na(AMO_target) & !is.na(AMO_target_prior) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    AMO_target_double_new = case_when(
      !is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ pmax(AMO_target_double, AMO_target_double_prior),
      !is.na(AMO_target_double) & is.na(AMO_target_double_prior) ~ AMO_target_double,
      is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(system:pct_ready_grad, AMO_target = AMO_target_new, AMO_target_double = AMO_target_double_new)


write_csv(school_amo_ready_grad, "N:/ORP_accountability/projects/2020_amo/ready_grad_targets_school_v2_AM.csv", na = "")

# ============================== ELPA ===================================
district_current_elpa <- read_csv("N:/ORP_accountability/projects/2020_amo/elpa_targets_district_AM.csv")
district_prior_elpa <- read_csv("N:/ORP_accountability/projects/2019_amo/elpa_district.csv") %>% 
  select(system, subgroup, AMO_target_prior = AMO_target, AMO_target_double_prior = AMO_target_double)

# Keep highest between the two (no decreasing AMOs)
district_amo_elpa <- district_current_elpa %>% 
  left_join(district_prior_elpa, by = c('system', 'subgroup')) %>% 
  mutate(
    AMO_target_new = case_when(
      !is.na(AMO_target) & !is.na(AMO_target_prior) ~ pmax(AMO_target, AMO_target_prior),
      !is.na(AMO_target) & is.na(AMO_target_prior) ~ AMO_target,
      is.na(AMO_target) & !is.na(AMO_target_prior) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    AMO_target_double_new = case_when(
      !is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ pmax(AMO_target_double, AMO_target_double_prior),
      !is.na(AMO_target_double) & is.na(AMO_target_double_prior) ~ AMO_target_double,
      is.na(AMO_target_double) & !is.na(AMO_target_double_prior) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(system:pct_met_growth_standard, AMO_target = AMO_target_new, AMO_target_double = AMO_target_double_new)


write_csv(district_amo_elpa, "N:/ORP_accountability/projects/2020_amo/elpa_targets_district_v2_AM.csv", na = "")






