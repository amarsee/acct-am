library(tidyverse)

school_names <- read_csv('N:/ORP_accountability/data/2020_final_accountability_files/names.csv')
dist_names <- school_names %>% 
  select(system, system_name) %>% 
  distinct()

# ================= School Success Rate ====================
# 2019
success_2019 <- read_csv("N:/ORP_accountability/projects/2020_amo/success_rate_targets_school.csv")

# 3 year
success_3_year <- read_csv("N:/ORP_accountability/projects/2021_amo/3 Year Avg/success_rate_targets_school_3_year.csv")

# Compare
compare_success <- success_3_year %>% 
  select(system:subgroup, AMO_target_3_year = AMO_target, AMO_target_double_3_year = AMO_target_double) %>% 
  left_join(
    success_2019 %>% select(system, school, subgroup, AMO_target_2019 = AMO_target, AMO_target_double_2019 = AMO_target_double),
    by = c('system', 'school', 'subgroup')
  ) %>% 
  mutate(
    difference = AMO_target_3_year - AMO_target_2019
  )

difference_success_amo <- compare_success %>% 
  filter(!is.na(difference)) %>% 
  mutate(
    movement = case_when(
      difference > 0 ~ 'Increase',
      difference == 0 ~ 'Equal',
      difference < 0 ~ 'Decrease',
      TRUE ~ NA_character_
    )
  )
mean(difference_success_amo$difference) # On average, -1.3 points
group_by(difference_success_amo, subgroup) %>% summarise(mean_diff = mean(difference))
# subgroup                               mean_diff
# <chr>                                      <dbl>
# 1 All Students                              -1.26 
# 2 Asian                                     -1.46 
# 3 Black or African American                 -1.29 
# 4 Black/Hispanic/Native American            -1.51 
# 5 Economically Disadvantaged                -1.34 
# 6 English Learners with Transitional 1-4    -1.83 
# 7 Hispanic                                  -1.91 
# 8 Students with Disabilities                -0.676
# 9 Super Subgroup                            -1.39 
# 10 White                                    -1.31 

pct_school_success_change <- group_by(difference_success_amo, subgroup, movement) %>% 
  summarise(n_schools = n()) %>% 
  group_by(subgroup) %>% 
  mutate(
    denom = sum(n_schools)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_schools = round(n_schools / denom * 100 + 1e-5, 1)
  )

group_by(difference_success_amo %>% filter(difference < 0), subgroup) %>% summarise(mean_diff = mean(difference))

# ================= District Success Rate ====================
# 2019
district_success_2019 <- read_csv("N:/ORP_accountability/projects/2020_amo/success_rate_targets_district.csv")

# 3 year
district_success_3_year <- read_csv("N:/ORP_accountability/projects/2021_amo/3 Year Avg/success_rate_targets_district_3_year.csv")

# Compare
district_compare_success <- district_success_3_year %>% 
  select(system:subgroup, AMO_target_3_year = AMO_target, AMO_target_double_3_year = AMO_target_double) %>% 
  left_join(
    district_success_2019 %>% select(system, grade, subgroup, AMO_target_2019 = AMO_target, AMO_target_double_2019 = AMO_target_double),
    by = c('system', 'grade', 'subgroup')
  ) %>% 
  mutate(
    difference = AMO_target_3_year - AMO_target_2019
  )

district_difference_success_amo <- district_compare_success %>% 
  filter(!is.na(difference)) %>% 
  mutate(
    movement = case_when(
      difference > 0 ~ 'Increase',
      difference == 0 ~ 'Equal',
      difference < 0 ~ 'Decrease',
      TRUE ~ NA_character_
    )
  )
mean(district_difference_success_amo$difference) # On average, -1.2 points
group_by(district_difference_success_amo, grade, subgroup) %>% summarise(mean_diff = mean(difference))
# grade            subgroup                               mean_diff
# <chr>            <chr>                                      <dbl>
# 1 3rd through 5th  All Students                              -1.42 
# 2 3rd through 5th  Black/Hispanic/Native American            -1.55 
# 3 3rd through 5th  Economically Disadvantaged                -1.36 
# 4 3rd through 5th  English Learners with Transitional 1-4    -2.13 
# 5 3rd through 5th  Students with Disabilities                -0.156
# 6 6th through 8th  All Students                              -0.428
# 7 6th through 8th  Black/Hispanic/Native American            -1.14 
# 8 6th through 8th  Economically Disadvantaged                -0.674
# 9 6th through 8th  English Learners with Transitional 1-4    -1.42 
# 10 6th through 8th  Students with Disabilities                -0.387
# 11 9th through 12th All Students                              -2.27 
# 12 9th through 12th Black/Hispanic/Native American            -2.46 
# 13 9th through 12th Economically Disadvantaged                -2.29 
# 14 9th through 12th English Learners with Transitional 1-4    -1.08 
# 15 9th through 12th Students with Disabilities                -0.430

pct_district_success_change <- group_by(district_difference_success_amo, subgroup, grade, movement) %>% 
  summarise(n_districts = n()) %>% 
  group_by(subgroup, grade) %>% 
  mutate(
    denom = sum(n_districts)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_districts = round(n_districts / denom * 100 + 1e-5, 1)
  )

# ================= School Absenteeism ====================
# 2019
school_abs_2019 <- read_csv("N:/ORP_accountability/projects/2020_amo/absenteeism_targets_school.csv")

# 3 year
school_abs_3_year <- read_csv("N:/ORP_accountability/projects/2021_amo/3 Year Avg/absenteeism_targets_school_3_year.csv")

# Compare
compare_abs <- school_abs_3_year %>% 
  select(system:subgroup, AMO_target_3_year = AMO_target, AMO_target_double_3_year = AMO_target_double) %>% 
  left_join(
    school_abs_2019 %>% select(system, school, subgroup, AMO_target_2019 = AMO_target, AMO_target_double_2019 = AMO_target_double),
    by = c('system', 'school', 'subgroup')
  ) %>% 
  mutate(
    difference = AMO_target_3_year - AMO_target_2019
  )

difference_abs_amo <- compare_abs %>% 
  filter(!is.na(difference)) %>% 
  mutate(
    movement = case_when(
      difference > 0 ~ 'Increase',
      difference == 0 ~ 'Equal',
      difference < 0 ~ 'Decrease',
      TRUE ~ NA_character_
    )
  )
mean((difference_abs_amo %>% filter(subgroup == 'All Students'))$difference) # On average, +0.17 points
group_by(difference_abs_amo, subgroup) %>% summarise(mean_diff = mean(difference))

# subgroup                               mean_diff
# <chr>                                      <dbl>
# 1 All Students                               0.167
# 2 Asian                                      0.333
# 3 Black or African American                  0.123
# 4 Black/Hispanic/Native American             0.417
# 5 Economically Disadvantaged                 0.954
# 6 English Learners with Transitional 1-4     0.579
# 7 Hispanic                                   0.599
# 8 Students with Disabilities                 0.670
# 9 White                                      0.948


# ================= District Absenteeism ====================
# 2019
district_abs_2019 <- read_csv("N:/ORP_accountability/projects/2020_amo/absenteeism_targets_district.csv")

# 3 year
district_abs_3_year <- read_csv("N:/ORP_accountability/projects/2021_amo/3 Year Avg/absenteeism_targets_district_3_year.csv")

# Compare
compare_abs_district <- district_abs_3_year %>% 
  select(system:subgroup, AMO_target_3_year = AMO_target, AMO_target_double_3_year = AMO_target_double) %>% 
  left_join(
    district_abs_2019 %>% select(system, subgroup, AMO_target_2019 = AMO_target, AMO_target_double_2019 = AMO_target_double),
    by = c('system', 'subgroup')
  ) %>% 
  mutate(
    difference = AMO_target_3_year - AMO_target_2019
  )

difference_district_abs_amo <- compare_abs_district %>% 
  filter(!is.na(difference)) %>% 
  mutate(
    movement = case_when(
      difference > 0 ~ 'Increase',
      difference == 0 ~ 'Equal',
      difference < 0 ~ 'Decrease',
      TRUE ~ NA_character_
    )
  )

mean((difference_district_abs_amo %>% filter(subgroup == 'All Students'))$difference) # On average, +0.7 points
group_by(difference_district_abs_amo, subgroup) %>% summarise(mean_diff = mean(difference))

# subgroup                               mean_diff
# <chr>                                      <dbl>
# 1 All Students                               0.703
# 2 Black/Hispanic/Native American             0.858
# 3 Economically Disadvantaged                 1.60 
# 4 English Learners with Transitional 1-4     0.421
# 5 Students with Disabilities                 0.961


# ================= District ELPA ====================
# 2019
district_elpa_2019 <- read_csv("N:/ORP_accountability/projects/2020_amo/elpa_targets_district.csv")

# 3 year
district_elpa_3_year <- read_csv("N:/ORP_accountability/projects/2021_amo/3 Year Avg/elpa_targets_district_3_year.csv")

# Compare
compare_elpa_district <- district_elpa_3_year %>% 
  select(system:subgroup, AMO_target_3_year = AMO_target, AMO_target_double_3_year = AMO_target_double) %>% 
  left_join(
    district_elpa_2019 %>% select(system, subgroup, AMO_target_2019 = AMO_target, AMO_target_double_2019 = AMO_target_double),
    by = c('system', 'subgroup')
  ) %>% 
  mutate(
    difference = AMO_target_3_year - AMO_target_2019
  )

difference_district_elpa_amo <- compare_elpa_district %>% 
  filter(!is.na(difference)) %>% 
  mutate(
    movement = case_when(
      difference > 0 ~ 'Increase',
      difference == 0 ~ 'Equal',
      difference < 0 ~ 'Decrease',
      TRUE ~ NA_character_
    )
  )

mean((difference_district_elpa_amo %>% filter(subgroup == 'All Students'))$difference) # On average, -3.6 points
group_by(difference_district_elpa_amo, subgroup) %>% summarise(mean_diff = mean(difference))
group_by(difference_district_elpa_amo, subgroup, movement) %>% summarise(n_schools = n())

# subgroup                       mean_diff
# <chr>                              <dbl>
# 1 All Students                      -3.60 
# 2 Black/Hispanic/Native American    -3.09 
# 3 Economically Disadvantaged        -2.25 
# 4 English Learners                  -3.60 
# 5 Students with Disabilities         0.371







