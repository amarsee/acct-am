# school subject grade AMOs
# Andrew Marsee
# 11/20/2019

library(tidyverse)
library(janitor)
library(acct)
library(readxl)
library(lubridate)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")
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


district_subject_targets_grade_band <- district_assessment %>% 
  filter(year == 2019, !grade %in% c("Missing Grade", "All Grades")) %>% 
  mutate(
    grade = case_when(
      grade %in% 3:5 ~ "3rd through 5th",
      grade %in% 6:8 ~ "6th through 8th",
      grade %in% 9:12 | is.na(grade) ~ "9th through 12th"
    ),
    subject = case_when(
      subject %in% math_eoc | (grade == "9th through 12th" & subject == "Math") ~ "HS Math",
      subject %in% english_eoc | (grade == "9th through 12th" & subject == "ELA") ~ "HS ELA",
      TRUE ~ subject
    )
  ) %>% 
  filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
                         "English Learners with Transitional 1-4", "Students with Disabilities", "Super Subgroup")) %>% 
  group_by(year, system, system_name, subject, grade, subgroup) %>% 
  summarise(
    valid_tests_prior = sum(valid_tests),
    pct_below_prior = round(sum(n_below) / sum(valid_tests) * 100 + 1e-10, 1),
    pct_on_mastered_prior = round( ( sum(n_on_track) + sum(n_mastered) )  / sum(valid_tests) * 100 + 1e-10, 1)
  ) %>% 
  ungroup() %>% 
  mutate(
    AMO_target = if_else(
      valid_tests_prior >= 30,
      round(((100 - pct_on_mastered_prior) / 16) + pct_on_mastered_prior + 1e-10, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      valid_tests_prior >= 30,
      round(((100 - pct_on_mastered_prior) / 8) + pct_on_mastered_prior + 1e-10, 1),
      NA_real_
    ),
    year = 2020
  ) %>% 
  arrange(system, subject, subgroup, grade)
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
district_numbers <- sort(unique(district_subject_targets_grade_band$system))

# Split District 
district_subject_targets %>%
  split(., .$system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, file = paste0(
      "N:/ORP_accountability/projects/", year(now())+1, "_amo/Grade Subject Level Split/", .y,
      "_district_subject_grade_targets_", sprintf("%02d", day(now())), month(now(), label = T), year(now()),
      ".csv"
    ), na = "")
  )

district_subject_targets_grade_band %>%
  split(., .$system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, file = paste0(
      "N:/ORP_accountability/projects/", 2020, "_amo/Grade Subject Level Split/", .y,
      "_district_subject_grade_band_targets_", sprintf("%02d", day(now())), month(now(), label = T), year(now()),
      ".csv"
    ), na = "")
  )

# Split School
school_subject_targets %>%
  split(., .$system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, file = paste0(
      "N:/ORP_accountability/projects/", year(now())+1, "_amo/Grade Subject Level Split/", .y,
      "_school_subject_grade_targets_", sprintf("%02d", day(now())), month(now(), label = T), year(now()),
      ".csv"
    ), na = "")
  )










