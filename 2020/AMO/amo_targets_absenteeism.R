library(tidyverse)

subgroups <- c(  "All Students", "Black/Hispanic/Native American", "Economically Disadvantaged", 
                 "English Learners with Transitional 1-4", "Students with Disabilities")

district_targets <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/district_chronic_absenteeism_Jul11.csv") %>%
  filter(
    grade_band == "All Grades", subgroup %in% subgroups
  ) %>%
  mutate(
    AMO_target = if_else(
      n_students >= 30,
      round(pct_chronically_absent - (pct_chronically_absent / 16) + 1e-10, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      n_students >= 30,
      round(pct_chronically_absent - (pct_chronically_absent / 8) + 1e-10, 1),
      NA_real_
    )
  )

write_csv(district_targets, "N:/ORP_accountability/projects/2020_amo/absenteeism_targets_district_AM.csv", na = "")

school_targets <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/school_chronic_absenteeism_Jul11.csv") %>%
  mutate(
    AMO_target = if_else(
      n_students >= 30,
      round(pct_chronically_absent - (pct_chronically_absent / 16) + 1e-10, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      n_students >= 30,
      round(pct_chronically_absent - (pct_chronically_absent / 8) + 1e-10, 1),
      NA_real_
    )
  )

write_csv(school_targets, "N:/ORP_accountability/projects/2020_amo/absenteeism_targets_school_AM.csv", na = "")

