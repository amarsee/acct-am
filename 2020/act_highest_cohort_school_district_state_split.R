library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)

school_names <- read_csv("N:/ORP_accountability/data/2020_final_accountability_files/names.csv") %>% 
  bind_rows(
    tribble(
      ~system, ~system_name, ~school, ~school_name,
      330, "Hamilton County", 95, "Hamilton County High School",
      520, "Lincoln County", 25, "Lincoln County Ninth Grade Academy",
      61, "Cleveland", 40, "F.I. Denning Center of Technology and Careers",
      # 570, "Madison County", 40, "Jackson Central-Merry Academy of Medical Technology High School",
      792, "Shelby County", 2085, "Carver High School",
      792, "Shelby County", 2315, "Hamilton Middle",
      792, "Shelby County", 2378, "Hamilton Middle",
      792, "Shelby County", 2535, "Northside High School",
      792, "Shelby County", 8125, "DuBois High School of Arts Technology",
      792, "Shelby County", 8130, "DuBois High of Leadership Public Policy",
      792, "Shelby County", 8295, "Gateway University",
      794, "Bartlett", 170, "Bartlett 9th Grade Academy",
      985, "Achievement School District", 35, "GRAD Academy Memphis"
    )
  )

system_names <- school_names %>% 
  select(system, system_name) %>% 
  distinct()

act_student <- read_csv("N:/ORP_accountability/data/2020_ACT/ACT_student_pre_appeals.csv",
                        col_types = 'icccccccciccccccciciiiciiccccciccccccicccccccciiiiiiillllllllllllllllllllllllll')

# ============================== Functions ===============================
filter_subgroup <- function(df, column_name, subgroup_label){
  return(df %>% filter(column_name == TRUE) %>% mutate(subgroup = subgroup_label))
}

# Subgroups for ACT
subgroups_act <- function(student_df){
  out_df <- bind_rows(
    student_df %>% mutate(subgroup = 'All Students'),
    student_df %>% filter(Native == TRUE) %>% mutate(subgroup = 'American Indian or Alaska Native'),
    student_df %>% filter(Asian == TRUE) %>% mutate(subgroup = 'Asian'),
    student_df %>% filter(Black == TRUE) %>% mutate(subgroup = 'Black or African American'),
    student_df %>% filter(BHN == TRUE) %>% mutate(subgroup = 'Black/Hispanic/Native American'),
    student_df %>% filter(ED == TRUE) %>% mutate(subgroup = 'Economically Disadvantaged'),
    student_df %>% filter(EL == TRUE) %>% mutate(subgroup = 'English Learners'),
    student_df %>% filter(Hispanic == TRUE) %>% mutate(subgroup = 'Hispanic'),
    student_df %>% filter(HPI == TRUE) %>% mutate(subgroup = 'Native Hawaiian or Other Pacific Islander'),
    student_df %>% filter(Non_ED == TRUE) %>% mutate(subgroup = 'Non-Economically Disadvantaged'),
    student_df %>% filter(Non_EL == TRUE) %>% mutate(subgroup = 'Non-English Learners'),
    student_df %>% filter(Non_SWD == TRUE) %>% mutate(subgroup = 'Non-Students with Disabilities'),
    student_df %>% filter(SWD == TRUE) %>% mutate(subgroup = 'Students with Disabilities'),
    student_df %>% filter(BHN == TRUE | ED == TRUE | EL == TRUE | SWD == TRUE) %>% mutate(subgroup = 'Super Subgroup'),
    student_df %>% filter(White == TRUE) %>% mutate(subgroup = 'White')
  )
  return(out_df)
}


collapse_act <- function(df, ...){
  sum_cols <- df %>% 
    group_by(...) %>% 
    summarise_at(
      .vars = c('enrolled', 'tested', 'valid_test', 'n_21_or_higher', 'n_below_19', 'met_CRB_english',
                'met_CRB_math', 'met_CRB_reading', 'met_CRB_science', 'met_CRB_all'),
      .funs = ~sum(., na.rm = TRUE)
    ) %>% 
    ungroup()
  mean_cols <- df %>% 
    group_by(...) %>% 
    summarise_at(
      .vars = vars(english:composite),
      .funs = ~round(mean(., na.rm = TRUE) + 1e-10, 1)
    ) %>% 
    ungroup()
  return(sum_cols %>% 
           left_join(mean_cols, by = )  %>% 
           mutate(participation_rate = round(tested / enrolled * 100 + 1e-10, 0)) %>% 
           select(..., participation_rate, everything()))
}

pcts_act <- function(df){
  out_df <- df %>% 
    mutate_at(
      .vars = vars(n_21_or_higher:met_CRB_all),
      .funs = list(pct = ~ round(. / valid_test * 100 + 1e-10, 1))
    ) %>% 
    rename_at( vars( contains( "_pct") ), list( ~paste("pct", gsub("_pct|n_", "", .), sep = "_") ) )
  return(out_df)
}
# ========================== Student level with subgroups =====================
student_w_subgroups <- subgroups_act(act_student)

# ============================= School Level ===================================
school_level_act <- student_w_subgroups %>% 
  collapse_act(system, school, subgroup) %>% 
  pcts_act() %>% 
  mutate_at(
    .vars = vars(english:pct_met_CRB_all),
    .funs = ~ if_else(is.na(.), NA_real_, .)
  ) %>% 
  left_join(school_names, by = c('system', 'school')) %>% 
  select(system, system_name, school, school_name, everything()) %>% 
  arrange(system, school, subgroup)

write_csv(school_level_act, "N:/ORP_accountability/data/2020_ACT/ACT_school_pre_appeals.csv", na = '')

# ======================== District Level ==================================
district_level_act <- student_w_subgroups %>% 
  collapse_act(system, subgroup) %>% 
  pcts_act() %>% 
  mutate_at(
    .vars = vars(english:pct_met_CRB_all),
    .funs = ~ if_else(is.na(.), NA_real_, .)
  ) %>% 
  left_join(system_names, by = c('system')) %>% 
  select(system, system_name, everything()) %>% 
  arrange(system, subgroup) %>% 
  distinct()

write_csv(district_level_act, "N:/ORP_accountability/data/2020_ACT/ACT_district_pre_appeals.csv", na = '')

# District Level for the tracker
appeals_tracker_district <- read_excel("N:/ORP_accountability/appeals/2020/ACT/Coding/ACT Appeals Master Tracker.xlsm", sheet = 2) %>% 
  clean_names() %>% 
  select(system = district_number) %>% 
  left_join(district_level_act %>% filter(subgroup == 'All Students') %>% 
              select(system, participation_rate, avg_composite = composite, pct_21_or_higher), 
            by = 'system')

write_csv(appeals_tracker_district, 'N:/ORP_accountability/appeals/2021/ACT/Coding/act_district_stats.csv', na = '')

# ======================== State Level ==================================
state_level_act <- student_w_subgroups %>% 
  collapse_act(subgroup) %>% 
  pcts_act() %>% 
  mutate_at(
    .vars = vars(english:pct_met_CRB_all),
    .funs = ~ if_else(is.na(.), NA_real_, .)
  ) %>% 
  mutate(
    system = 0, system_name = 'State of Tennessee'
  ) %>% 
  select(system, system_name, everything())

write_csv(state_level_act, "N:/ORP_accountability/data/2020_ACT/ACT_state_pre_appeals.csv", na = '')



# =========================== Split Files ================================
student_level_to_split <- act_student %>% 
  left_join(school_names, by = c('system', 'school')) %>% 
  select(
    system, system_name, school, school_name, student_key, first_name, middle_name, last_name,
    act_composite_highest = composite, act_english_highest = english, act_math_highest = math,
    act_reading_highest = reading, act_science_highest = science, el, ed, swd, valid_test,
    met_CRB_english, met_CRB_math, met_CRB_reading, met_CRB_science, met_CRB_all,
    sat_total, suffix,
    date_of_birth, gender, immigrant, date_1st_enrolled_us_school, year_entered_grade9,
    native_language, ethnicity, race_i, race_a, race_p, race_b, race_w, cohortyear,
    calc_from, assignment, eoy_action, withdrawal_reason, completion_type, completion_period,
    completion_date, year_withdrawn, included_in_cohort, race_ethnicity, manual_intervention, homeless, cte,
    migrant, isp_id, save_as_filename, upload_date, user_id, reviewer_user_id, comments, status,
    reviewed_date, revised_included_in_cohort
  ) %>% 
  mutate_at(
    .vars = vars(valid_test:met_CRB_all),
    .funs = as.integer
  )

district_numbers <- sort(unique(student_level_to_split$system))

student_level_to_split %>%
  split(., .$system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/data/", year(now()), "_ACT/split/", .y,
      "_2020_StudentLevelACT_", format(Sys.Date(), "%d%b%Y"),
      ".csv"
    ), na = "")
  )

# Split School Level
school_level_act %>%
  split(., .$system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/data/", year(now()), "_ACT/split/", .y,
      "_2020_SchoolLevelACT_", format(Sys.Date(), "%d%b%Y"),
      ".csv"
    ), na = "")
  )

# Split District Level
district_level_act %>%
  split(., .$system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/data/", year(now()), "_ACT/split/", .y,
      "_2020_DistrictLevelACT_", format(Sys.Date(), "%d%b%Y"),
      ".csv"
    ), na = "")
  )

