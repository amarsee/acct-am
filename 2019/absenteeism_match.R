library(tidyverse)
library(janitor)
library(readstata13)
library(readxl)
library(haven)

school_names <- read_csv("N:\\ORP_accountability\\data\\2019_final_accountability_files\\names.csv")

dist_names <- school_names %>% 
  select(system, system_name) %>% 
  unique()

# =================================== Student Level and filtering duplicates ==========================================

ca_student_level <- read_csv("N:\\ORP_accountability\\data\\2019_chronic_absenteeism\\student_chronic_absenteeism_AM_Jul11.csv") %>% 
  mutate(
    ca_indicator = if_else(absentee_rate >= 10, 1, 0),
    grade_band = case_when(
      grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08") ~ 'K through 8th',
      grade %in% c("09", "10", "11", "12") ~ '9th through 12th'
    )
  )

ca_student_level <- ca_student_level %>% 
  bind_rows(ca_student_level %>% mutate(grade_band = 'All Grades'))


# ================================== School Level ===========================================================
out_df_ca <- bind_rows(
  ca_student_level %>% mutate(subgroup = 'All Students'),
  ca_student_level %>% filter(Native == 1) %>% mutate(subgroup = 'American Indian or Alaska Native'),
  ca_student_level %>% filter(Asian == 1) %>% mutate(subgroup = 'Asian'),
  ca_student_level %>% filter(Black == 1) %>% mutate(subgroup = 'Black or African American'),
  ca_student_level %>% filter(Black == 1 | Hispanic == 1 | Native == 1) %>% 
    mutate(subgroup = 'Black/Hispanic/Native American'),
  ca_student_level %>% filter(ED == 1) %>% mutate(subgroup = 'Economically Disadvantaged'),
  ca_student_level %>% filter(EL == 1) %>% mutate(subgroup = 'English Learners with Transitional 1-4'),
  ca_student_level %>% filter(Hispanic == 1) %>% mutate(subgroup = 'Hispanic'),
  ca_student_level %>% filter(HPI == 1) %>% mutate(subgroup = 'Native Hawaiian or Other Pacific Islander'),
  ca_student_level %>% filter(White == 1) %>% mutate(subgroup = 'White'),
  ca_student_level %>% filter(SWD == 1) %>% mutate(subgroup = 'Students with Disabilities')
)

school_level_ca <- out_df_ca %>% 
  filter(isp_days/instructional_calendar_days >= 0.5, grade_band == 'All Grades') %>% 
  group_by(system, school, subgroup, grade_band) %>% 
  summarise(
    n_students = n(),
    n_chronically_absent = sum(ca_indicator, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_chronically_absent = round(n_chronically_absent / n_students * 100 + 1e-5, 1)
  ) %>% 
  left_join(school_names, by = c("system", "school"))%>% 
  transmute(
    # year = 2018,
    system, system_name, school, school_name,
    subgroup, grade_band, n_students, n_chronically_absent, pct_chronically_absent
  ) %>% 
  arrange(system, school, subgroup)

write_csv(school_level_ca, "N:/ORP_accountability/data/2019_chronic_absenteeism/school_chronic_absenteeism_Jul11_AM.csv")

# check school level
school_level_comp <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/school_chronic_absenteeism_Jul11.csv")

diff_df_school <- dplyr::setdiff(school_level_ca, school_level_comp) %>% 
  bind_rows(dplyr::setdiff(school_level_comp, school_level_ca)) %>% 
  arrange(system, subgroup, grade_band)

# ===================================== District Level =====================================

district_level_student <- read_csv("N:\\ORP_accountability\\data\\2019_chronic_absenteeism\\student_chronic_absenteeism_AM_Jul11.csv") %>% 
  mutate(
    ca_indicator = if_else(absentee_rate >= 10, 1, 0),
    grade_band = case_when(
      grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08") ~ 'K through 8th',
      grade %in% c("09", "10", "11", "12") ~ '9th through 12th'
    )
  ) %>% 
  # Add up absences and ISP days across every enrollment
  group_by(student_id, system, system_name, grade_band) %>%
  summarise(
    n_absences = sum(n_absences, na.rm = TRUE),
    isp_days = sum(isp_days, na.rm = TRUE),
    instructional_days = max(instructional_calendar_days),
    ED = max(ED, na.rm = TRUE),
    SWD = max(SWD, na.rm = TRUE),
    EL = max(EL, na.rm = TRUE),
    Black = max(Black, na.rm = TRUE),
    Hispanic = max(Hispanic, na.rm = TRUE),
    Native = max(Native, na.rm = TRUE),
    HPI = max(HPI, na.rm = TRUE),
    Asian = max(Asian, na.rm = TRUE),
    White = max(White, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(isp_days/instructional_days >= 0.5) %>%
  mutate(chronic_absence = as.integer(n_absences/isp_days >= 0.1))

district_level_student <- district_level_student %>% 
  bind_rows(district_level_student %>% mutate(grade_band = 'All Grades'))

out_df_district_ca <- bind_rows(
  district_level_student %>% mutate(subgroup = 'All Students'),
  district_level_student %>% filter(Native == 1) %>% mutate(subgroup = 'American Indian or Alaska Native'),
  district_level_student %>% filter(Asian == 1) %>% mutate(subgroup = 'Asian'),
  district_level_student %>% filter(Black == 1) %>% mutate(subgroup = 'Black or African American'),
  district_level_student %>% filter(Black == 1 | Hispanic == 1 | Native == 1) %>% 
    mutate(subgroup = 'Black/Hispanic/Native American'),
  district_level_student %>% filter(ED == 1) %>% mutate(subgroup = 'Economically Disadvantaged'),
  district_level_student %>% filter(EL == 1) %>% mutate(subgroup = 'English Learners with Transitional 1-4'),
  district_level_student %>% filter(Hispanic == 1) %>% mutate(subgroup = 'Hispanic'),
  district_level_student %>% filter(HPI == 1) %>% mutate(subgroup = 'Native Hawaiian or Other Pacific Islander'),
  district_level_student %>% filter(White == 1) %>% mutate(subgroup = 'White'),
  district_level_student %>% filter(SWD == 1) %>% mutate(subgroup = 'Students with Disabilities')
)

district_level_ca <- out_df_district_ca %>% 
  group_by(system, subgroup, grade_band) %>% 
  summarise(
    n_students = n(),
    n_chronically_absent = sum(chronic_absence, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_chronically_absent = round(n_chronically_absent / n_students * 100 + 1e-5, 1)
  ) %>% 
  left_join(dist_names, by = "system") %>% 
  transmute(
    system, system_name,
    subgroup, grade_band, n_students, n_chronically_absent, pct_chronically_absent
  ) %>% 
  arrange(system, subgroup)

write_csv(district_level_ca, "N:/ORP_accountability/data/2019_chronic_absenteeism/district_chronic_absenteeism_Jul11_AM.csv")

# check district
district_level_comp <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/district_chronic_absenteeism_Jul11.csv")

diff_df <- dplyr::setdiff(district_level_ca, district_level_comp) %>% 
  bind_rows(dplyr::setdiff(district_level_comp, district_level_ca)) %>% 
  arrange(system, subgroup, grade_band)


# diff_non_pcts <- school_assessment_final %>% 
#   select(year:n_mastered) %>% 
#   setdiff(school_assessment_comp %>% select(year:n_mastered)) %>% 
#   bind_rows(setdiff(school_assessment_comp %>% select(year:n_mastered), school_assessment_final %>% select(year:n_mastered))) %>% 
#   arrange(system, school, test, subject, grade, subgroup, -year)


# =============================== State Level ==========================================

state_level_student <- read_csv("N:\\ORP_accountability\\data\\2019_chronic_absenteeism\\student_chronic_absenteeism_AM_Jul11.csv") %>% 
  mutate(
    ca_indicator = if_else(absentee_rate >= 10, 1, 0),
    grade_band = case_when(
      grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08") ~ 'K through 8th',
      grade %in% c("09", "10", "11", "12") ~ '9th through 12th'
    )
  ) %>% 
  # Add up absences and ISP days across every enrollment
  group_by(student_id, grade_band) %>%
  summarise(
    n_absences = sum(n_absences, na.rm = TRUE),
    isp_days = sum(isp_days, na.rm = TRUE),
    instructional_days = max(instructional_calendar_days),
    ED = max(ED, na.rm = TRUE),
    SWD = max(SWD, na.rm = TRUE),
    EL = max(EL, na.rm = TRUE),
    Black = max(Black, na.rm = TRUE),
    Hispanic = max(Hispanic, na.rm = TRUE),
    Native = max(Native, na.rm = TRUE),
    HPI = max(HPI, na.rm = TRUE),
    Asian = max(Asian, na.rm = TRUE),
    White = max(White, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(isp_days >= 45) %>%
  mutate(chronic_absence = as.integer(n_absences/isp_days >= 0.1))

state_level_student <- state_level_student %>% 
  bind_rows(state_level_student %>% mutate(grade_band = 'All Grades'))

out_df_state_ca <- bind_rows(
  state_level_student %>% mutate(subgroup = 'All Students'),
  state_level_student %>% filter(Native == 1) %>% mutate(subgroup = 'American Indian or Alaska Native'),
  state_level_student %>% filter(Asian == 1) %>% mutate(subgroup = 'Asian'),
  state_level_student %>% filter(Black == 1) %>% mutate(subgroup = 'Black or African American'),
  state_level_student %>% filter(Black == 1 | Hispanic == 1 | Native == 1) %>% 
    mutate(subgroup = 'Black/Hispanic/Native American'),
  state_level_student %>% filter(ED == 1) %>% mutate(subgroup = 'Economically Disadvantaged'),
  state_level_student %>% filter(EL == 1) %>% mutate(subgroup = 'English Learners with Transitional 1-4'),
  state_level_student %>% filter(Hispanic == 1) %>% mutate(subgroup = 'Hispanic'),
  state_level_student %>% filter(HPI == 1) %>% mutate(subgroup = 'Native Hawaiian or Other Pacific Islander'),
  state_level_student %>% filter(White == 1) %>% mutate(subgroup = 'White'),
  state_level_student %>% filter(SWD == 1) %>% mutate(subgroup = 'Students with Disabilities')
)

state_level_ca <- out_df_state_ca %>% 
  group_by(subgroup, grade_band) %>% 
  summarise(
    n_students = n(),
    n_chronically_absent = sum(chronic_absence, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_chronically_absent = round(n_chronically_absent / n_students * 100 + 1e-5, 1)
  ) %>% 
  mutate(
    system = 0,
    system_name = 'State of Tennessee'
  )%>% 
  transmute(
    system, system_name, 
    subgroup, grade_band, n_students, n_chronically_absent, pct_chronically_absent
  ) %>% 
  arrange(system, subgroup)

write_csv(state_level_ca, "N:/ORP_accountability/data/2019_chronic_absenteeism/state_chronic_absenteeism_Jul11_AM.csv")

# check district
state_level_comp <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/state_chronic_absenteeism_Jul11.csv")

diff_df_state <- dplyr::setdiff(state_level_ca, state_level_comp) %>% 
  bind_rows(dplyr::setdiff(state_level_comp, state_level_ca)) %>% 
  arrange(system, subgroup, grade_band)


