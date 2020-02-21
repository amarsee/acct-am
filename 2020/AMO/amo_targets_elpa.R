library(tidyverse)

subgroups <- c(  "All Students", "Black/Hispanic/Native American", "Economically Disadvantaged", 
                 "English Learners with Transitional 1-4", "English Learners", "Students with Disabilities")

district_targets <- read_csv('N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_district.csv') %>%
  filter(
    subgroup %in% subgroups
  ) %>%
  mutate(
    AMO_target = if_else(
      growth_standard_denom >= 30,
      round(((100 - pct_met_growth_standard) / 16) + pct_met_growth_standard + 1e-10, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      growth_standard_denom >= 30,
      round(((100 - pct_met_growth_standard) / 8) + pct_met_growth_standard + 1e-10, 1),
      NA_real_
    )
  ) %>% 
  transmute(
    system, subgroup, growth_standard_denom,
    pct_met_growth_standard, AMO_target, AMO_target_double
  )

write_csv(district_targets, "N:/ORP_accountability/projects/2020_amo/elpa_targets_district_AM.csv", na = "")

school_targets <- read_csv('N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_school.csv') %>%
  filter(
    !str_detect(subgroup, 'Non'), !subgroup %in% c('Female', 'Migrant', 'Homeless', 'Male')
  ) %>%
  mutate(
    AMO_target = if_else(
      growth_standard_denom >= 10,
      round(((100 - pct_met_growth_standard) / 16) + pct_met_growth_standard + 1e-10, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      growth_standard_denom >= 10,
      round(((100 - pct_met_growth_standard) / 8) + pct_met_growth_standard + 1e-10, 1),
      NA_real_
    )
  ) %>% 
  transmute(
    system, system_name, school, school_name, subgroup, growth_standard_denom,
    pct_met_growth_standard, AMO_target, AMO_target_double
  )

write_csv(school_targets, "N:/ORP_accountability/projects/2020_amo/elpa_targets_school_AM.csv", na = "")


