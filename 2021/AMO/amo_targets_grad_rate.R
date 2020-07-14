library(tidyverse)

subgroups <- c(  "All Students", "Black/Hispanic/Native American", "Economically Disadvantaged", 
                 "English Learners with Transitional 1-4", "Students with Disabilities")

district_targets <- read_csv("N:/ORP_accountability/data/2018_graduation_rate/district_grad_rate.csv") %>%
  filter(
    subgroup %in% subgroups
  ) %>%
  mutate(
    AMO_target = if_else(
      grad_cohort >= 30,
      round(((100 - grad_rate) / 16) + grad_rate + 1e-10, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      grad_cohort >= 30,
      round(((100 - grad_rate) / 8) + grad_rate + 1e-10, 1),
      NA_real_
    )
  ) %>% 
  transmute(
    system, system_name, subgroup, grad_cohort_prior = grad_cohort,
    grad_rate = if_else(grad_cohort_prior < 30, NA_real_, grad_rate), AMO_target, AMO_target_double
  )

write_csv(district_targets, "N:/ORP_accountability/projects/2020_amo/grad_targets_district_AM.csv", na = "")

school_targets <- read_csv("N:/ORP_accountability/data/2018_graduation_rate/school_grad_rate.csv") %>%
  filter(!str_detect(subgroup, 'Non'), !subgroup %in% c('Female', 'Migrant', 'Homeless', 'Male')) %>% 
  mutate(
    AMO_target = if_else(
      grad_cohort >= 30,
      round(((100 - grad_rate) / 16) + grad_rate + 1e-10, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      grad_cohort >= 30,
      round(((100 - grad_rate) / 8) + grad_rate + 1e-10, 1),
      NA_real_
    )
  ) %>% 
  transmute(
    system, system_name, school, school_name, subgroup, grad_cohort_prior = grad_cohort,
    grad_rate = if_else(grad_cohort_prior < 30, NA_real_, grad_rate), AMO_target, AMO_target_double
  )

write_csv(school_targets, "N:/ORP_accountability/projects/2020_amo/grad_targets_school_AM.csv", na = "")
