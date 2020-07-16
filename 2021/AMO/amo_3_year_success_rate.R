library(tidyverse)
library(janitor)
library(acct)

# ============= School Level ==========
# Read in last 3 Years of AMOs
success_rate_2018 <- read_csv("N:/ORP_accountability/projects/2018_amo/school_success_rate.csv") %>% 
  transmute(
    system, school, subgroup,
    n_count_2018 = valid_tests_prior, metric_2018 = pct_on_mastered_prior
  )
success_rate_2019 <- read_csv("N:/ORP_accountability/projects/2019_amo/success_rate_targets_school.csv") %>% 
  transmute(
    system, school, subgroup,
    n_count_2019 = valid_tests, metric_2019 = success_rate_prior
  )
success_rate_2020 <- read_csv("N:/ORP_accountability/projects/2020_amo/success_rate_targets_school.csv") %>% 
  transmute(
    system, school, subgroup,
    n_count_2020 = n_count, metric_2020 = metric
  )

success_rate_3_year_amo <- success_rate_2020 %>% 
  left_join(success_rate_2019, by = c('system', 'school', 'subgroup')) %>% 
  left_join(success_rate_2018, by = c('system', 'school', 'subgroup'))








