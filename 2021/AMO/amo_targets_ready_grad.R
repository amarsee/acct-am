library(tidyverse)

subgroups <- c(  "All Students", "Black/Hispanic/Native American", "Economically Disadvantaged", 
                 "English Learners with Transitional 1-4", "Students with Disabilities")

school_targets <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_school.csv") %>%
  filter(!str_detect(subgroup, 'Non'), !subgroup %in% c('Female', 'Migrant', 'Homeless', 'Male')) %>% 
  mutate(
    AMO_target = if_else(
      n_count >= 30,
      round(((100 - pct_ready_grad) / 16) + pct_ready_grad + 1e-10, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      n_count >= 30,
      round(((100 - pct_ready_grad) / 8) + pct_ready_grad + 1e-10, 1),
      NA_real_
    )
  ) %>% 
  transmute(
    system, system_name, school, school_name, subgroup, grad_count_prior = n_count,
    pct_ready_grad = if_else(grad_count_prior < 30, NA_real_, pct_ready_grad), AMO_target, AMO_target_double
  ) %>% 
  arrange(system, school, subgroup)

write_csv(school_targets, "N:/ORP_accountability/projects/2021_amo/ready_grad_targets_school.csv", na = "")
