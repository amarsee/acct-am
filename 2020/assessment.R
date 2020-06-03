# Assessment Files

library(tidyverse)
library(lubridate)

# ============= Functions ================
total_by_subgroup <- function(grouped_df) {
  out_df <- grouped_df %>% 
    # group_by(system, school, test, original_subject, grade, subgroup) %>% 
    summarise_at(
      .vars = c('enrolled', 'tested', 'valid_test', 'below', 'approaching', 'on_track', 'mastered'),
      .funs = sum
    ) %>% 
    rename(valid_tests = valid_test) %>% 
    rename_at(.vars = vars(below:mastered), .funs = ~ paste0('n_', .)) %>% 
    ungroup()
  return(out_df)
}

calc_pl_pcts <- function(df, acct_year){
  out_df <- df %>% 
    mutate(
      pct_mastered = round((n_mastered/valid_tests) * 100 + 1e-10, 1),
      pct_on_mastered = round(((n_on_track + n_mastered)/valid_tests) * 100 + 1e-10, 1),
      pct_on_track = if_else(n_approaching == 0 & n_below == 0, 100 - pct_mastered, round((n_on_track/valid_tests) * 100 + 1e-10, 1)),
      pct_approaching = if_else(n_below == 0, 100 - (pct_on_track + pct_mastered), round((n_approaching/valid_tests) * 100 + 1e-10, 1)),
      pct_below = 100 - (pct_mastered + pct_on_track + pct_approaching),
      year = acct_year) %>% 
    rename(subject = original_subject)
  
  return(out_df)
}


# ======== Student Level ===============
student <- read_csv('N:/ORP_accountability/projects/2020_student_level_file/2020_student_level_file.csv') %>% 
  mutate(
    grade = if_else(grade == 0 | is.na(grade), "Missing Grade", as.character(grade)),
    below = if_else(!is.na(performance_level) & (performance_level == "Below" | performance_level == "Below Basic"), 1, 0),
    approaching = if_else(!is.na(performance_level) & (performance_level == "Approaching" | performance_level == "Basic"), 1, 0),
    on_track = if_else(!is.na(performance_level) & (performance_level == "On Track" | performance_level == "Proficient" | performance_level == "On-Track"), 1, 0),
    mastered = if_else(!is.na(performance_level) & (performance_level == "Mastered" | performance_level == "Advanced"), 1, 0),
    subgroup = case_when(
      reported_race == "American Indian/Alaska Native" ~ "American Indian or Alaska Native",
      reported_race == "Hispanic/Latino" ~ "Hispanic",
      reported_race == "Native Hawaiian/Pac. Islander" ~ "Native Hawaiian or Other Pacific Islander",
      TRUE ~ reported_race
    ),
    test = case_when(
      test == "Alt-Science/Social Studies" | test == "MSAA" | test == 'Alt-Social Studies' ~ "MSAA/Alt-Social Studies",
      TRUE ~ test
    )
  ) %>% 
  filter(residential_facility == 0 | is.na(residential_facility), grade != 2) # , homebound == 0)

demos <- bind_rows(
  student, # Race/ethnicity subgroups
  student %>% mutate(subgroup = "All Students"),
  student %>% filter(bhn_group > 0) %>% mutate(subgroup = "Black/Hispanic/Native American"),
  student %>% filter(bhn_group == 0 | is.na(bhn_group)) %>% mutate(subgroup = "Non-Black/Hispanic/Native American"),
  student %>% filter(economically_disadvantaged > 0) %>% mutate(subgroup = "Economically Disadvantaged"),
  student %>% filter(economically_disadvantaged == 0 | is.na(economically_disadvantaged)) %>% mutate(subgroup = "Non-Economically Disadvantaged"),
  student %>% filter(el > 0) %>% mutate(subgroup = "English Learners"),
  student %>% filter(t1234 > 0) %>% mutate(subgroup = "English Learner Transitional 1-4"),
  student %>% filter(t1234 > 0 | el > 0) %>% mutate(subgroup = "English Learners with Transitional 1-4"),
  student %>% filter((t1234 == 0 | is.na(t1234)) & (el == 0 | is.na(el))) %>% mutate(subgroup = "Non-English Learners/Transitional 1-4"),
  student %>% filter(gender == 'F') %>% mutate(subgroup = "Female"),
  student %>% filter(gender == 'M') %>% mutate(subgroup = "Male"),
  student %>% filter(gifted == 1) %>% mutate(subgroup = "Gifted"),
  student %>% filter(migrant == 1) %>% mutate(subgroup = "Migrant"),
  student %>% filter(special_ed > 0) %>% mutate(subgroup = "Students with Disabilities"),
  student %>% filter(special_ed == 0 | is.na(special_ed)) %>% mutate(subgroup = "Non-Students with Disabilities"),
  student %>% filter(bhn_group > 0 | economically_disadvantaged > 0 | t1234 > 0 | el > 0 | special_ed > 0) %>% mutate(subgroup = "Super Subgroup")
)



# =================== School Level =========================
sch_totals <- bind_rows(
  demos %>% 
    group_by(system, system_name, school, school_name, test, original_subject, grade, subgroup) %>% 
    total_by_subgroup(),
  demos %>% 
    mutate(grade = 'All Grades') %>% 
    group_by(system, system_name, school, school_name, test, original_subject, grade, subgroup) %>% 
    total_by_subgroup()
) %>% 
  filter(subgroup != "Unknown") %>% 
  calc_pl_pcts(acct_year = 2020) %>%
  select(year, system:n_mastered, pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>% 
  arrange(system, school, subject, grade, subgroup)

school_assessment_previous <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_assessment_file.csv")

school_assessment_final <- sch_totals %>%
  bind_rows(school_assessment_previous %>% filter(year %in% c(2019, 2018), subject %in% unique(sch_totals$subject))) %>%
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

write_csv(school_assessment_final, "N:/ORP_accountability/data/2020_final_accountability_files/school_assessment_file.csv", na = '')
# ======================== District Level ========================
dist_totals <- bind_rows(
  demos %>% 
    group_by(system, system_name, test, original_subject, grade, subgroup) %>% 
    total_by_subgroup(),
  demos %>% 
    mutate(grade = 'All Grades') %>% 
    group_by(system, system_name, test, original_subject, grade, subgroup) %>% 
    total_by_subgroup()
) %>% 
  filter(subgroup != "Unknown") %>% 
  calc_pl_pcts(acct_year = 2020) %>%
  select(year, system:n_mastered, pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>% 
  arrange(system, subject, grade, subgroup)

district_assessment_previous <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_assessment_file.csv")

district_assessment_final <- dist_totals %>% 
  bind_rows(district_assessment_previous %>% filter(year %in% c(2019, 2018), subject %in% unique(dist_totals$subject))) %>% 
  filter(!(grade %in% c(3,4,5) & subject == 'Social Studies')) %>% 
  group_by(system) %>%
  filter(max(year) == 2020) %>%
  ungroup() %>%
  select(year, system:n_mastered, pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>% 
  mutate_at(
    .vars = vars(pct_below:pct_on_mastered),
    .funs = ~if_else(is.na(.), NA_real_, .)
  ) %>% 
  arrange(system, subject, grade, subgroup, test, -year)

# Write csv
write_csv(district_assessment_final, "N:/ORP_accountability/data/2020_final_accountability_files/district_assessment_file.csv", na = '')

# ========================== State Level ======================
state_totals <- bind_rows(
  demos %>% 
    group_by(test, original_subject, grade, subgroup) %>% 
    total_by_subgroup(),
  demos %>% 
    mutate(grade = 'All Grades') %>% 
    group_by(test, original_subject, grade, subgroup) %>% 
    total_by_subgroup()
) %>% 
  filter(subgroup != "Unknown") %>% 
  mutate(
    system = 0,
    system_name = "State of Tennessee"
  ) %>% 
  calc_pl_pcts(acct_year = 2020) %>%
  select(year, system, system_name, test:n_mastered, pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>% 
  arrange(system, subject, grade, subgroup)

state_assessment_prior <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/state_assessment_file.csv")

state_assessment_final <- state_totals %>% 
  bind_rows(state_assessment_prior %>% filter(year %in% c(2019, 2018), subject %in% unique(state_totals$subject))) %>% 
  filter(!(grade %in% c(3,4,5) & subject == 'Social Studies')) %>% 
  select(year, system:n_mastered, pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>% 
  mutate_at(
    .vars = vars(pct_below:pct_on_mastered),
    .funs = ~if_else(is.na(.), NA_real_, .)
  ) %>% 
  arrange(system, subject, grade, subgroup, test, -year)

# Write csv
write_csv(state_assessment_final, "N:/ORP_accountability/data/2020_final_accountability_files/state_assessment_file.csv", na = '')

# =============== Split Files ==================
# Split district file
district_numbers <- sort(unique(student$system))

district_assessment_final %>%
  group_split(system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/data/2020_final_accountability_files/split/", .y,
      "_2020_DistrictAssessmentFile_", format(Sys.Date(), "%d%b%Y"), ".csv"
    ), na = "")
  )


# Split school file
school_assessment_final %>%
  group_split(system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/data/2020_final_accountability_files/split/", .y,
      "_2020_SchoolAssessmentFile_", format(Sys.Date(), "%d%b%Y"), ".csv"
    ), na = "")
  )

student_split <- read_csv('N:/ORP_accountability/projects/2020_student_level_file/2020_student_level_file.csv')
# Split student level file
student_split %>%
  group_split(system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/data/2020_final_accountability_files/split/", .y,
      "_2020_StudentLevelFiles_", format(Sys.Date(), "%d%b%Y"), ".csv"
    ), na = "")
  )



