# school subject grade AMOs
# Andrew Marsee
# 11/20/2019

library(tidyverse)
library(janitor)
library(acct)
library(readxl)
library(lubridate)


# ======================== read in assessment files ===================================
district_assessment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_assessment_file.csv")

school_assessment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_assessment_file.csv")

# =============================== District Level ===========================
district_subject_targets <- district_assessment %>% 
  filter(year == 2019) %>% 
  mutate(
    AMO_target = if_else(
      valid_tests >= 30,
      round(((100 - pct_on_mastered) / 16) + pct_on_mastered + 1e-10, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      valid_tests >= 30,
      round(((100 - pct_on_mastered) / 8) + pct_on_mastered + 1e-10, 1),
      NA_real_
    ),
    year = 2020
  )

# =============================== School Level ===========================
school_subject_targets <- school_assessment %>% 
  filter(year == 2019) %>%
  mutate(
    AMO_target = if_else(
      valid_tests >= 30,
      round(((100 - pct_on_mastered) / 16) + pct_on_mastered + 1e-10, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      valid_tests >= 30,
      round(((100 - pct_on_mastered) / 8) + pct_on_mastered + 1e-10, 1),
      NA_real_
    ),
    year = 2020
  )

# ===================================== Split Files ============================
district_numbers <- sort(unique(school_subject_targets$system))

# Split District 
district_subject_targets %>%
  split(., .$system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/projects/", year(now())+1, "_amo/Grade Subject Level Split/", .y,
      "_district_subject_grade_targets_", sprintf("%02d", day(now())), month(now(), label = T), year(now()),
      ".csv"
    ), na = "")
  )

# Split School
school_subject_targets %>%
  split(., .$system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/projects/", year(now())+1, "_amo/Grade Subject Level Split/", .y,
      "_school_subject_grade_targets_", sprintf("%02d", day(now())), month(now(), label = T), year(now()),
      ".csv"
    ), na = "")
  )










