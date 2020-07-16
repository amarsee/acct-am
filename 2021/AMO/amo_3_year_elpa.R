
library(tidyverse)
library(janitor)
library(acct)

# ============ School and District Names =================
school_names <- read_csv("N:/ORP_accountability/data/2020_final_accountability_files/names.csv")
dist_names <- school_names %>% 
  select(system, system_name) %>% 
  distinct()

# ============= District Level ===============
# Read in last 3 Years of AMOs
elpa_2018_district <- read_csv("N:/ORP_accountability/projects/2018_amo/system_elpa_AMO_targets2018_JW.csv") %>% 
  transmute(
    system, 
    subgroup = if_else(subgroup == 'English Language Learners', 'English Learners', subgroup),
    growth_standard_denom_2018 = n_validtests_growth, pct_met_growth_standard_2018 = pct_met_growth_standard
  )
elpa_2019_district <- read_csv("N:/ORP_accountability/projects/2019_amo/elpa_district.csv") %>% 
  transmute(
    system, subgroup,
    growth_standard_denom_2019 = growth_standard_denom, pct_met_growth_standard_2019 = pct_met_growth_standard
  )
elpa_2020_district <- read_csv("N:/ORP_accountability/projects/2020_amo/elpa_targets_district.csv") %>% 
  transmute(
    system, subgroup,
    growth_standard_denom_2020 = growth_standard_denom, pct_met_growth_standard_2020 = pct_met_growth_standard
  )

# Create New AMOs
district_elpa_3_year_amo <- elpa_2020_district %>% 
  left_join(elpa_2019_district, by = c('system', 'subgroup')) %>% 
  left_join(elpa_2018_district, by = c('system', 'subgroup')) %>% 
  rowwise() %>% 
  mutate(
    pct_met_growth_standard_3_year_avg = round(mean(c(pct_met_growth_standard_2018,
                                                     pct_met_growth_standard_2019,
                                                     pct_met_growth_standard_2020), na.rm = TRUE) + 1e-5, 1)
  ) %>% 
  ungroup() %>% 
  mutate(
    AMO_target = if_else(
      growth_standard_denom_2020 >= 30,
      round(((100 - pct_met_growth_standard_3_year_avg) / 16) + pct_met_growth_standard_3_year_avg + 1e-5, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      growth_standard_denom_2020 >= 30,
      round(((100 - pct_met_growth_standard_3_year_avg) / 8) + pct_met_growth_standard_3_year_avg + 1e-5, 1),
      NA_real_
    )
  ) %>% 
  inner_join(dist_names, by = c('system')) %>% 
  select(system, system_name, everything())

write_csv(district_elpa_3_year_amo, 'N:/ORP_accountability/projects/2021_amo/elpa_targets_district_3_year.csv', na = '')
