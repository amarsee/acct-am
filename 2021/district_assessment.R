library(readxl)
library(tidyverse)

student_level <- read_csv("N://ORP_accountability/projects/2020_student_level_file/2020_student_level_file.csv")
# sl <- student_level
# sl$grade[sl$grade == 0] <- "Missing Grade"

names <- read_csv("N:\\ORP_accountability\\data\\2020_final_accountability_files\\names.csv") %>% 
  select(system, system_name) %>% 
  distinct()

sl <- student_level %>% 
  # select(-system, - school) %>% 
  # rename(system = acct_system, school = acct_school) %>% 
  mutate(
    grade = as.character(grade),
    below = case_when(
      performance_level == "Below" | performance_level == "Below Basic" ~ 1,
      TRUE                      ~ 0
    ),
    approaching = case_when(
      performance_level == "Approaching" | performance_level == "Basic" ~ 1,
      TRUE                      ~ 0
    ),
    on_track = case_when(
      performance_level == "Proficient" | performance_level == "On Track" ~ 1,
      TRUE                      ~ 0
    ),
    mastered = case_when(
      performance_level == "Mastered" | performance_level == "Advanced" ~ 1,
      TRUE                      ~ 0
    ),
    grade = if_else(is.na(grade) | grade == '0', "Missing Grade", grade)
  ) %>% 
  filter(residential_facility == 0 | is.na(residential_facility), grade != 2) %>% 
  # fill(system_name) %>% 
  rename(subgroup = reported_race)

total_by_subgroup <- function(df) {
  out_df <- df %>%
    group_by(system, system_name, test, original_subject, grade, subgroup) %>%
    summarise(
      enrolled = sum(enrolled),
      tested = sum(tested),
      valid_tests = sum( valid_test),
      n_below = sum(below),
      n_approaching = sum(approaching),
      n_on_track = sum(on_track),
      n_mastered = sum(mastered)
    ) %>%
    ungroup()
  return(out_df)
}

state_demos <- bind_rows(
    sl,
    sl %>% filter(bhn_group > 0) %>% mutate(subgroup = "Black/Hispanic/Native American"),
    sl %>% filter(bhn_group == 0 | is.na(bhn_group)) %>% mutate(subgroup = "Non-Black/Hispanic/Native American"),
    sl %>% filter(economically_disadvantaged > 0) %>% mutate(subgroup = "Economically Disadvantaged"),
    sl %>% filter(economically_disadvantaged == 0 | is.na(economically_disadvantaged)) %>% mutate(subgroup = "Non-Economically Disadvantaged"),
    sl %>% filter(el > 0) %>% mutate(subgroup = "English Learners"),
    sl %>% filter(t1234 > 0) %>% mutate(subgroup = "English Learner Transitional 1-4"),
    sl %>% filter(t1234 > 0 | el > 0) %>% mutate(subgroup = "English Learners with Transitional 1-4"),
    sl %>% filter((t1234 == 0 | is.na(t1234)) & (el == 0 | is.na(el))) %>% mutate(subgroup = "Non-English Learners/Transitional 1-4"),
    sl %>% filter(gender == 'F') %>% mutate(subgroup = "Female"),
    sl %>% filter(gender == 'M') %>% mutate(subgroup = "Male"),
    sl %>% filter(gifted == 1) %>% mutate(subgroup = "Gifted"),
    sl %>% filter(migrant == 1) %>% mutate(subgroup = "Migrant"),
    sl %>% filter(special_ed > 0) %>% mutate(subgroup = "Students with Disabilities"),
    sl %>% filter(special_ed == 0 | is.na(special_ed)) %>% mutate(subgroup = "Non-Students with Disabilities"),
    sl %>% mutate(subgroup = "All Students"),
    # sl %>% mutate(subgroup = "All Students", grade = "All Grades"),
    sl %>% filter(bhn_group > 0 | economically_disadvantaged > 0 | t1234 > 0 | el > 0 | special_ed > 0) %>% mutate(subgroup = "Super Subgroup")
  )

state_totals <- bind_rows(
    state_demos %>% total_by_subgroup(),
    state_demos %>% mutate(grade = 'All Grades') %>% total_by_subgroup()
  ) %>% 
  filter(subgroup != "Unknown") %>% 
  arrange(system, original_subject, grade, subgroup)
  

district_assessment <- state_totals %>% 
  select(-system_name) %>% 
  left_join(names, by = c('system')) %>% 
  mutate(
    pct_mastered = round((n_mastered/valid_tests) * 100 + 1e-10, 1),
    pct_on_mastered = round(((n_on_track + n_mastered)/valid_tests) * 100 + 1e-10, 1),
    pct_on_track = ifelse(n_approaching == 0 & n_below == 0, 100 - pct_mastered, round((n_on_track/valid_tests) * 100 + 1e-10, 1)),
    pct_approaching = ifelse(n_below == 0, 100 - (pct_on_track + pct_mastered), round((n_approaching/valid_tests) * 100 + 1e-10, 1)),
    pct_below = 100 - (pct_mastered + pct_on_track + pct_approaching),
    year = 2020) %>% 
  select(system, system_name, everything()) %>% 
  rename(subject = original_subject)

district_assessment_previous <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_assessment_file.csv")

district_assessment_final <- district_assessment %>% 
  mutate(
    subgroup = case_when(
      subgroup == "American Indian/Alaska Native" ~ "American Indian or Alaska Native",
      subgroup == "Hispanic/Latino" ~ "Hispanic",
      subgroup == "Native Hawaiian/Pac. Islander" ~ "Native Hawaiian or Other Pacific Islander",
      TRUE ~ subgroup
    ),
    test = case_when(
      test == "Alt-Science/Social Studies" | test == "MSAA" ~ "MSAA/Alt-Social Studies",
      TRUE ~ test
    )
  ) %>% 
  bind_rows(district_assessment_previous %>% filter(year %in% c(2019, 2018), subject %in% unique(district_assessment$subject))) %>% 
  filter(!(grade %in% c(3,4,5) & subject == 'Social Studies')) %>% 
  select(year, system:n_mastered, pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>% 
  mutate_at(
    .vars = vars(pct_below:pct_on_mastered),
    .funs = ~if_else(is.na(.), NA_real_, .)
  ) %>% 
  arrange(system, subject, grade, subgroup, test, -year)
  # arrange(system, test, subject, grade, subgroup, -year)

# Write csv
write_csv(district_assessment_final, "N:/ORP_accountability/data/2020_final_accountability_files/district_assessment_file.csv", na = '')

# ================= Comparing to Alex's output file =======================

dist_assessment_comp <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_assessment_file.csv")

diff_df <- district_assessment_final %>% 
  setdiff(dist_assessment_comp) %>% 
  bind_rows(setdiff(dist_assessment_comp, district_assessment_final)) %>% 
  arrange(system, test, subject, grade, subgroup, -year)


diff_non_pcts <- district_assessment_final %>% 
  select(year:n_mastered) %>% 
  setdiff(dist_assessment_comp %>% select(year:n_mastered)) %>% 
  bind_rows(setdiff(dist_assessment_comp %>% select(year:n_mastered), district_assessment_final %>% select(year:n_mastered))) %>% 
  arrange(system, test, subject, grade, subgroup, -year)

# # Comparing to last year's output file
# district_assessment_source <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/district_assessment_file.csv")
# dist_source <- district_assessment_source %>% 
#   filter(year == 2018)
# 
# district_assessment_non_pct <- district_assessment[,1:14]
# district_assessment_source_non_pct <- dist_source[,1:14]
# 
# assess_diffs_non_pct <- setdiff(district_assessment_non_pct, district_assessment_source_non_pct)
# 
# joined <- inner_join(district_assessment, dist_source, by = c("system", "system_name", "subgroup", "subject", "grade", "year", "test")) %>% 
#   mutate(diff = pct_below.x - pct_below.y)
# 




