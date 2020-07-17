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
school_success_rate_2018 <- read_csv("N:/ORP_accountability/projects/2018_amo/school_success_rate_no_science.csv") %>% 
  transmute(
    system, school, subgroup,
    n_count_2018 = valid_tests_prior, metric_2018 = pct_on_mastered_prior
  )
school_success_rate_2019 <- read_csv("N:/ORP_accountability/projects/2019_amo/success_rate_targets_school.csv") %>% 
  transmute(
    system, school, subgroup,
    n_count_2019 = valid_tests, metric_2019 = success_rate_prior
  )
school_success_rate_2020 <- read_csv("N:/ORP_accountability/projects/2020_amo/success_rate_targets_school.csv") %>% 
  transmute(
    system, school, subgroup,
    n_count_2020 = n_count, metric_2020 = metric
  )

school_success_rate_3_year_amo <- school_success_rate_2020 %>% 
  left_join(school_success_rate_2019, by = c('system', 'school', 'subgroup')) %>% 
  left_join(school_success_rate_2018, by = c('system', 'school', 'subgroup')) %>% 
  rowwise() %>% 
  mutate(
    metric_3_year_avg = round(mean(c(metric_2018,
                                                      metric_2019,
                                                      metric_2020), na.rm = TRUE) + 1e-5, 1)
  ) %>% 
  ungroup() %>% 
  mutate(
    metric_3_year_avg = if_else(is.na(metric_3_year_avg), NA_real_, metric_3_year_avg),
    AMO_target = if_else(
      n_count_2020 >= 30,
      round(((100 - metric_3_year_avg) / 16) + metric_3_year_avg + 1e-5, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      n_count_2020 >= 30,
      round(((100 - metric_3_year_avg) / 8) + metric_3_year_avg + 1e-5, 1),
      NA_real_
    )
  ) %>% 
  inner_join(school_names, by = c('system', 'school')) %>% 
  select(system, system_name, school, school_name, everything())

write_csv(school_success_rate_3_year_amo, 'N:/ORP_accountability/projects/2021_amo/success_rate_targets_school_3_year.csv', na = '')


# ============= District Level ==========
# Read in last 3 Years of AMOs
district_success_rate_2018 <- read_csv("N:/ORP_accountability/projects/2018_amo/district_success_rate_no_science.csv") %>% 
  transmute(
    system, grade, subgroup,
    n_count_2018 = valid_tests_prior, metric_2018 = pct_on_mastered_prior
  )
district_success_rate_2019 <- read_csv("N:/ORP_accountability/projects/2019_amo/success_rate_targets_district.csv") %>% 
  transmute(
    system, grade, subgroup,
    n_count_2019 = valid_tests, metric_2019 = success_rate_prior
  )
district_success_rate_2020 <- read_csv("N:/ORP_accountability/projects/2020_amo/success_rate_targets_district.csv") %>% 
  transmute(
    system, grade, subgroup,
    n_count_2020 = n_count, metric_2020 = metric
  )

district_success_rate_3_year_amo <- district_success_rate_2020 %>% 
  left_join(district_success_rate_2019, by = c('system', 'grade', 'subgroup')) %>% 
  left_join(district_success_rate_2018, by = c('system', 'grade', 'subgroup')) %>% 
  rowwise() %>% 
  mutate(
    metric_3_year_avg = round(mean(c(metric_2018,
                                     metric_2019,
                                     metric_2020), na.rm = TRUE) + 1e-5, 1)
  ) %>% 
  ungroup() %>% 
  mutate(
    metric_3_year_avg = if_else(is.na(metric_3_year_avg), NA_real_, metric_3_year_avg),
    AMO_target = if_else(
      n_count_2020 >= 30,
      round(((100 - metric_3_year_avg) / 16) + metric_3_year_avg + 1e-5, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      n_count_2020 >= 30,
      round(((100 - metric_3_year_avg) / 8) + metric_3_year_avg + 1e-5, 1),
      NA_real_
    )
  ) %>% 
  inner_join(dist_names, by = c('system')) %>% 
  select(system, system_name, everything())

write_csv(district_success_rate_3_year_amo, 'N:/ORP_accountability/projects/2021_amo/success_rate_targets_district_3_year.csv', na = '')





