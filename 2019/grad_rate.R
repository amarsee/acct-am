library(tidyverse)
library(janitor)
library(readxl)
library(readstata13)

# demo_combined <- read_csv('N:/TNReady/2018-19/spring/demographics/spring_2019_assessment_demographics_combined_pull_20190610.csv')
# 
# demos_filtered_2019 <- demo_combined %>% 
#   transmute(
#     system = district_id,
#     school = school_id,
#     unique_student_id = student_key, 
#     #grade,
#     gender, 
#     hispanic = if_else(ethnicity == 'H', 'Y', 'N'),
#     economically_disadvantaged = case_when(
#       codeab == 1 ~ 'Y',
#       codeab == 2 ~ 'N',
#       TRUE ~ NA_character_
#     ),
#     reported_race = reportedrace,
#     title_1 = title1,
#     gifted = isgifted,
#     functionally_delayed = isfunctionallydelayed,
#     migrant = ismigrant,
#     el_arrived_year_1 = elrecentlyarrivedyearone,
#     el_arrived_year_2 = elrecentlyarrivedyeartwo,
#     el = isel,
#     el_t1234 = t1t2,
#     special_ed = specialeducation,
#     ed = if_else(economically_disadvantaged == 'Y', 1, 0),
#     enrolled_50_pct_district = district50percent,
#     enrolled_50_pct_school = school50percent
#   ) %>% 
#   mutate(
#     native_american = if_else(reported_race == 1, 1, 0),
#     asian = if_else(reported_race == 2, 1, 0),
#     black = if_else(reported_race == 3, 1, 0),
#     hawaiian_pi = if_else(reported_race == 5, 1, 0),
#     white = if_else(reported_race == 6, 1, 0),
#     race = case_when(
#       reported_race == 1 ~ 'American Indian/Alaska Native',
#       reported_race == 2 ~ 'Asian',
#       reported_race == 3 ~ 'Black or African American',
#       reported_race == 4 ~ 'Hispanic/Latino',
#       reported_race == 5 ~ 'Native Hawaiian/Pac. Islander',
#       reported_race == 6 ~ 'White',
#       TRUE ~ 'Unknown'
#     ),
#     bhn_group = if_else(reported_race %in% c(1,3,4), 1 ,0)
#   )

grad_student_level <- read_csv("N:/ORP_accountability/projects/2019_graduation_rate/Data/studentcohortdata_20190809.csv") %>% 
  clean_names()

grad_included <- grad_student_level %>% 
  filter(included_in_cohort == 'Y') %>% 
  select(student_key, gender, ethnicity:migrant, t1_t2)

# ======================== State Level =============================================

# # subgroups <- c("All Students", "American Indian or Alaska Native", "Asian", "Black or African American", "Black/Hispanic/Native American",
#                "Economically Disadvantaged", "English Learners with Transitional 1-4", "Female", "Hispanic", "Homeless", "Male", 
#                "Migrant", "Native Hawaiian or Other Pacific Islander", "Non-Black/Hispanic/Native American", 
#                "Non-Economically Disadvantaged", "Non-English Learners/Transitional 1-4", "Non-Homeless", "Non-Migrant",
#                "Non-Students with Disabilities", "Students with Disabilities", "White")
subgroups <- c("American Indian or Alaska Native", "Asian", "Black or African American", "Black/Hispanic/Native American",
  "Economically Disadvantaged", "English Learners with Transitional 1-4", "Female", "Hispanic", "Homeless", 
  "Migrant", "Native Hawaiian or Other Pacific Islander", "Students with Disabilities", "White")

out_df <- grad_included %>% 
  mutate(subgroup = 'All Students')

for (subgroup in subgroups){
  student_df <- grad_included
  if (subgroup == "American Indian or Alaska Native"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'I') %>% 
      mutate(subgroup = "American Indian or Alaska Native")
    non_hist_df <- 0
  }else if (subgroup == "Asian"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'A') %>% 
      mutate(subgroup = "Asian")
    non_hist_df <- 0
  }else if (subgroup == "Black or African American"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'B') %>% 
      mutate(subgroup = "Black or African American")
    non_hist_df <- 0
  }else if (subgroup == "Black/Hispanic/Native American"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'B' | race_ethnicity == 'H' | race_ethnicity == 'I') %>% 
      mutate(subgroup = "Black/Hispanic/Native American")
    non_hist_df <- student_df %>% 
      filter(race_ethnicity != 'B' & race_ethnicity != 'H' & race_ethnicity != 'I')%>% 
      mutate(subgroup = "Non-Black/Hispanic/Native American")
  } else if (subgroup == "Economically Disadvantaged") {
    hist_df <- student_df %>% 
      filter(econ_dis == 'Y') %>% 
      mutate(subgroup = "Economically Disadvantaged")
    non_hist_df <- student_df %>% 
      filter(econ_dis != 'Y') %>% 
      mutate(subgroup = "Non-Economically Disadvantaged")
  }else if (subgroup == "English Learners with Transitional 1-4") {
    hist_df <- student_df %>% 
      filter(ell == 'Y') %>% 
      mutate(subgroup = "English Learners with Transitional 1-4")
    non_hist_df <- student_df %>% 
      filter(ell == 'N')%>% 
      mutate(subgroup = "Non-English Learners/Transitional 1-4")
  }else if (subgroup == "Female"){
    hist_df <- student_df %>% 
      filter(gender == 'F') %>% 
      mutate(subgroup = "Female")
    non_hist_df <- student_df %>% 
      filter(gender == 'M') %>% 
      mutate(subgroup = 'Male')
  }else if (subgroup == "Hispanic"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'H') %>% 
      mutate(subgroup = "Hispanic")
    non_hist_df <- 0
  }else if (subgroup == "Homeless") {
    hist_df <- student_df %>% 
      filter(homeless == 'Y') %>% 
      mutate(subgroup = "Homeless")
    non_hist_df <- student_df %>% 
      filter(homeless != 'Y')%>% 
      mutate(subgroup = "Non-Homeless")
  }else if (subgroup == "Migrant") {
    hist_df <- student_df %>% 
      filter(migrant == 'Y') %>% 
      mutate(subgroup = "Migrant")
    non_hist_df <- student_df %>% 
      filter(migrant != 'Y')%>% 
      mutate(subgroup = "Non-Migrant")
  }else if (subgroup == "Native Hawaiian or Other Pacific Islander"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'P') %>% 
      mutate(subgroup = "Native Hawaiian or Other Pacific Islander")
    non_hist_df <- 0
  }else if (subgroup == "White"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'W') %>% 
      mutate(subgroup = "White")
    non_hist_df <- 0
  }else {
    hist_df <- student_df %>% 
      filter(sped == 'Y') %>% 
      mutate(subgroup = "Students with Disabilities")
    non_hist_df <- student_df %>% 
      filter(sped != 'Y')%>% 
      mutate(subgroup = "Non-Students with Disabilities")
  }
  # hist_grouped <- total_by_subgroup(hist_df)
  out_df <- rbind(out_df, hist_df)
  if (is.data.frame(non_hist_df)){
    #non_hist_grouped <- total_by_subgroup(non_hist_df)
    out_df <- rbind(out_df, non_hist_df)
  }
}

out_df <- out_df %>% 
  mutate(
    completed = if_else(completion_type %in% c(1,11,12,13), 1, 0)
  )

state_df <- out_df %>% 
  group_by(subgroup) %>% 
  summarise(
    grad_cohort = n(),
    grad_count = sum(completed)
  ) %>% 
  ungroup() %>% 
  mutate(
    grad_rate = round(grad_count / grad_cohort * 100 + 1e-10, 0)
  ) %>% 
  transmute(
    year = 2019,
    system = 0,
    system_name = 'State of Tennessee',
    subgroup, grad_cohort, grad_count, grad_rate
  )
# write_csv
write_csv(state_df, "N:/ORP_accountability/data/2019_graduation_rate/state_grad_rate.csv")


# ======================================== District Level ==============================================
district_df <- out_df %>% 
  group_by(subgroup, district_no) %>% 
  summarise(
    grad_cohort = n(),
    grad_count = sum(completed, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    grad_rate = round(grad_count / grad_cohort * 100 + 1e-10, 0)
  ) %>% 
  transmute(
    year = 2019,
    system = district_no,
    subgroup, grad_cohort, grad_count, grad_rate
  ) %>% 
  arrange(system, subgroup)
# write_csv
write_csv(district_df, "N:/ORP_accountability/data/2019_graduation_rate/district_grad_rate.csv")

# ========================================== School Level =====================================================


school_df <- out_df %>% 
  group_by(subgroup, district_no, school_no) %>% 
  summarise(
    grad_cohort = n(),
    grad_count = sum(completed)
  ) %>% 
  ungroup() %>% 
  mutate(
    grad_rate = round(grad_count / grad_cohort * 100 + 1e-10, 0)
  ) %>% 
  transmute(
    year = 2019,
    system = district_no,
    school = school_no,
    subgroup, grad_cohort, grad_count, grad_rate
  ) %>% 
  arrange(system, school, subgroup)

# write_csv
write_csv(school_df, "N:/ORP_accountability/data/2019_graduation_rate/school_grad_rate.csv")

