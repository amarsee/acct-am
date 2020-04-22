# Compare 2020 data through March 2 to last year
# Andrew Marsee
# 3/31/20

library(acct)
library(tidyverse)
library(janitor)
library(lubridate)

# ============= School Level ====================
school_2018 <- read_csv("N:/ORP_accountability/data/2018_chronic_absenteeism/school_chronic_absenteeism_primary_enrollment_only.csv")

school_2019 <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/school_chronic_absenteeism_Jul11.csv")

school_2020 <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/school_chronic_absenteeism_through_Mar2_Mar30.csv")

abs_diff <- school_2019 %>% 
  rename_at(
    .vars = vars(n_students:pct_chronically_absent),
    .funs = ~ paste0(., '_2019')
  ) %>% 
  left_join(
    school_2020 %>% 
      select(system, school, subgroup, n_students:pct_chronically_absent) %>% 
      rename_at(
        .vars = vars(n_students:pct_chronically_absent),
        .funs = ~ paste0(., '_2020')
      ),
    by = c('system', 'school', 'subgroup')
  ) %>% 
  mutate(
    pct_chronically_absent_diff = pct_chronically_absent_2020 - pct_chronically_absent_2019
  ) %>% 
  filter(
    n_students_2019 >= 30,
    n_students_2020 >= 30
  )

dist_pct_improved <- abs_diff %>% 
  group_by(system, system_name, subgroup) %>% 
  summarise(
    n_schools = n(),
    n_improved = sum(pct_chronically_absent_diff <= 0),
    n_worsened = sum(pct_chronically_absent_diff > 0)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_improved = round(n_improved / n_schools *100 + 1e-10, 1),
    pct_worsened = round(n_worsened / n_schools *100 + 1e-10, 1)
  )

abs_diff_prior <- school_2018 %>% 
  rename_at(
    .vars = vars(n_students:pct_chronically_absent),
    .funs = ~ paste0(., '_2018')
  ) %>% 
  left_join(
    school_2019 %>% 
      select(system, school, subgroup, n_students:pct_chronically_absent) %>% 
      rename_at(
        .vars = vars(n_students:pct_chronically_absent),
        .funs = ~ paste0(., '_2019')
      ),
    by = c('system', 'school', 'subgroup')
  ) %>% 
  mutate(
    pct_chronically_absent_diff = pct_chronically_absent_2019 - pct_chronically_absent_2018
  ) %>% 
  filter(
    n_students_2018 >= 30,
    n_students_2019 >= 30
  )

dist_pct_improved_prior <- abs_diff_prior %>% 
  group_by(system, system_name, subgroup) %>% 
  summarise(
    n_schools = n(),
    n_improved = sum(pct_chronically_absent_diff <= 0),
    n_worsened = sum(pct_chronically_absent_diff > 0)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_improved = round(n_improved / n_schools *100 + 1e-10, 1),
    pct_worsened = round(n_worsened / n_schools *100 + 1e-10, 1)
  )

all_student_comp <- abs_diff %>% 
  filter(subgroup == 'All Students') %>% 
  left_join(
    abs_diff_prior %>% 
      filter(subgroup == 'All Students') %>% 
      select(system, school, ca_diff_18_19 = pct_chronically_absent_diff),
    by = c('system', 'school')
  ) %>%
  rename(ca_diff_19_20 = pct_chronically_absent_diff) %>% 
  filter(!is.na(ca_diff_19_20), !is.na(ca_diff_18_19)) %>% 
  pivot_longer(
    cols = c(ca_diff_19_20, ca_diff_18_19),
    names_to = 'year',
    values_to = 'yoy_diff'
  ) %>% 
  mutate(
    year = if_else(year == 'ca_diff_19_20', '2019 to 2020', '2018 to 2019'),
    direction = if_else(yoy_diff <= 0, 'Improved', 'Worsened')
  ) %>%
  group_by(year, direction) %>% 
  summarise(
    n_schools = n()
  ) %>% 
  group_by(year) %>% 
  mutate(
    denom = sum(n_schools)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_schools = round(n_schools / denom * 100 +1e-10, 1)
  )

# ========================== District Level =====================
district_2018 <- read_csv("N:/ORP_accountability/data/2018_chronic_absenteeism/district_chronic_absenteeism_primary_enrollment_only.csv")

district_2019 <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/district_chronic_absenteeism_Jul11.csv")

district_2020 <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/district_chronic_absenteeism_through_Mar2_Mar30.csv")

yoy_comparison_district <- district_2020 %>% 
  rename_at(.vars = vars(n_students:pct_chronically_absent),
            .funs = ~paste0(.,'_2020')) %>% 
  left_join(
    district_2019 %>% 
      select(system, subgroup, grade_band, n_students:pct_chronically_absent) %>% 
      rename_at(.vars = vars(n_students:pct_chronically_absent),
                .funs = ~paste0(.,'_2019')),
    by = c('system', 'subgroup', 'grade_band')
  ) %>% 
  left_join(
    district_2018 %>% 
      select(system, subgroup, grade_band, n_students:pct_chronically_absent) %>% 
      rename_at(.vars = vars(n_students:pct_chronically_absent),
                .funs = ~paste0(.,'_2018')),
    by = c('system', 'subgroup', 'grade_band')
  ) %>% 
  filter_at(c('n_students_2020', 'n_students_2019', 'n_students_2018'),
            all_vars(. >= 30)) %>% 
  mutate(
    
  )



