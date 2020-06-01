library(readxl)
library(tidyverse)

student_level <- read_csv("N:/ORP_accountability/projects/2020_student_level_file/2020_student_level_file.csv")
# sl <- student_level
# sl$grade[sl$grade == 0] <- "Missing Grade"

names <- read_csv("N:/ORP_accountability/data/2020_final_accountability_files/names.csv")

sl <- student_level %>% 
  # select(-system, - school) %>% 
  # rename(system = acct_system, school = acct_school) %>% 
  mutate(
    grade = if_else(grade == 0, "Missing Grade", as.character(grade)),
    # grade = as.character(grade),
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
  filter(residential_facility == 0 | is.na(residential_facility), grade != 2) %>% # , homebound == 0) %>% 
  # fill(system_name) %>% 
  rename(subgroup = reported_race)

total_by_subgroup <- function(df) {
  out_df <- df %>% 
    group_by(system, school, test, original_subject, grade, subgroup) %>% 
    summarise(
      enrolled = sum(enrolled),
      tested = sum(tested),
      valid_tests = sum(valid_test),
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


school_assessment <- state_totals %>% 
  # select(-school_name, -system_name) %>% 
  left_join(names, by = c('system', 'school')) %>% 
  mutate(
    pct_mastered = round((n_mastered/valid_tests) * 100 + 1e-10, 1),
    pct_on_mastered = round(((n_on_track + n_mastered)/valid_tests) * 100 + 1e-10, 1),
    pct_on_track = ifelse(n_approaching == 0 & n_below == 0, 100 - pct_mastered, round((n_on_track/valid_tests) * 100 + 1e-10, 1)),
    pct_approaching = ifelse(n_below == 0, 100 - (pct_on_track + pct_mastered), round((n_approaching/valid_tests) * 100 + 1e-10, 1)),
    pct_below = 100 - (pct_mastered + pct_on_track + pct_approaching),
    year = 2020) %>% 
  rename(subject = original_subject) %>% 
  select(system, system_name, school, school_name, everything())# %>% 
  # filter(!school_name %in% cte_alt_adult$SCHOOL, !school_name %in% closed_schools$SCHOOL_NAME)

school_assessment_previous <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_assessment_file.csv")

school_assessment_final <- school_assessment %>% 
  mutate(
    system_name = case_when(
      system == 964 & school == 964 ~ "Tennessee School for the Deaf",
      system == 970 & school == 970 ~ "Department Of Children's Services Education Division",
      TRUE ~ system_name
    ),
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
  bind_rows(school_assessment_previous %>% filter(year %in% c(2019, 2018), subject %in% unique(school_assessment$subject))) %>% 
  filter(!(grade %in% c(3,4,5) & subject == 'Social Studies')) %>% 
  group_by(system, school) %>% 
  filter(max(year) == 2020) %>% 
  ungroup() %>% 
  select(year, system:n_mastered, pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>% 
  mutate_at(
    .vars = vars(pct_below:pct_on_mastered),
    .funs = ~if_else(is.na(.), NA_real_, .)
  ) %>% 
  arrange(system, school, test, subject, grade, subgroup, -year)

# Write csv
write_csv(school_assessment_final, "N:/ORP_accountability/data/2020_final_accountability_files/school_assessment_file.csv", na = '')

# ================= Comparing to Alex's output file =======================

school_assessment_comp <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_assessment_file.csv")

diff_df <- school_assessment_final %>% 
  setdiff(school_assessment_comp) %>% 
  bind_rows(setdiff(school_assessment_comp, school_assessment_final)) %>% 
  arrange(system, school, test, subject, grade, subgroup, -year)


diff_non_pcts <- school_assessment_final %>% 
  select(year:n_mastered) %>% 
  setdiff(school_assessment_comp %>% select(year:n_mastered)) %>% 
  bind_rows(setdiff(school_assessment_comp %>% select(year:n_mastered), school_assessment_final %>% select(year:n_mastered))) %>% 
  arrange(system, school, test, subject, grade, subgroup, -year)


# Checking Participation Rates

school_assessment_check_participation <- school_assessment_final %>% 
  mutate(
    participation_rate = round(tested / enrolled * 100 + 1e-10, 0)
         ) %>% 
  filter(year == 2019, test == 'TNReady')











