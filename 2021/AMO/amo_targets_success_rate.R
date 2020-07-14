library(tidyverse)
library(janitor)
library(acct)
library(readxl)

subgroups <- c(  "All Students", "Black/Hispanic/Native American", "Economically Disadvantaged", 
                 "English Learners with Transitional 1-4", "Students with Disabilities")

# ======================== Prepare District Data ======================
math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

subgroups <- c('All Students', 'Black/Hispanic/Native American', 'Economically Disadvantaged', 
               "English Learners with Transitional 1-4", 'Students with Disabilities')

student_level <- read_csv("N://ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv") 

sl <- student_level %>% 
  # filter(!(system == 964 & school == 964 | system == 970 & school == 970)) %>%
  mutate_at("residential_facility", ~ if_else(is.na(.), 0, .)) %>%
  mutate_at("enrolled_50_pct_district", ~ if_else(is.na(.), "Y", .)) %>% 
  mutate(
    original_subject = case_when(
      grade < 9 & original_subject %in% c('Algebra I', 'Algebra II', "Geometry", "Integrated Math I", "Integrated Math II", 
                                          "Integrated Math III", 'English I', 'English II') ~ subject,
      TRUE ~ original_subject
    )
  ) %>% 
  filter(residential_facility == 0, enrolled_50_pct_district == 'Y' | system != acct_system, # !is.na(state_student_id), grade %in% 3:12,
         !original_subject %in% c("US History", "Social Studies")) %>% 
  select(-system, -school) %>% 
  rename(system = acct_system, school = acct_school)

system_df <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv") %>% # student_level %>% 
  select(system, system_name) %>% 
  distinct() %>% 
  filter(!is.na(system_name))

integrated_math <- student_level %>% 
  filter(original_subject %in% c("Algebra I", "Integrated Math I")) %>% 
  count(system, original_subject) %>% 
  group_by(system) %>% 
  mutate(temp = max(n)) %>% 
  # Systems where Integrated Math is the max between that and Algebra I
  filter(n == temp, original_subject == "Integrated Math I")
# Vector with the sytems where that is the case
int_math_vec <- integrated_math[['system']]

act_sub <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_district.csv") %>% 
  transmute(system,
            original_subject = 'HS Math', subject = 'HS Math', grade = "9th through 12th", subgroup = "All Students", 
            valid_test = valid_tests, on_track = n_met_benchmark, mastered = 0
  ) 

sl <- sl %>% 
  filter(original_subject %in% c('Math', 'ELA', math_eoc, english_eoc)) %>% 
  mutate(
    on_track = case_when(
      performance_level == "Proficient" | performance_level == "On Track" ~ 1,
      TRUE                      ~ 0
    ),
    mastered = case_when(
      performance_level == "Mastered" | performance_level == "Advanced" ~ 1,
      TRUE                      ~ 0
    ),
    original_subject = case_when(
      grade >= 9 & original_subject %in% math_eoc ~ 'HS Math',
      grade >= 9 & original_subject %in% english_eoc ~ 'HS ELA',
      TRUE ~ original_subject
    ),
    subject = case_when(
      subject %in% c('HS Math', math_eoc) ~ 'HS Math',
      subject %in% english_eoc ~ 'HS ELA',
      TRUE ~ subject
    ),
    grade = case_when(
      grade >= 9 | is.na(grade) ~ '9th through 12th',
      grade >= 6 ~ '6th through 8th',
      grade >= 3 ~ '3rd through 5th'
    )
  ) %>% 
  # fill(system_name) %>% 
  rename(subgroup = reported_race)

total_by_subgroup_dist <- function(df) {
  out_df <- df %>% 
    group_by(system, subject, grade, subgroup) %>% 
    summarise(
      enrolled = sum(enrolled, na.rm = TRUE),
      tested = sum(tested, na.rm = TRUE),
      valid_tests = sum( valid_test),
      n_on_track = sum(on_track),
      n_mastered = sum(mastered)
    ) %>% 
    ungroup()
  return(out_df)
}

all_students <- sl %>% 
  bind_rows(act_sub) %>% 
  mutate(subgroup = "All Students") %>% 
  total_by_subgroup_dist()

cat_subgroups <- function(student_df, students_grouped ){
  base_df = students_grouped
  subgroups <- c("Black/Hispanic/Native American", "Economically Disadvantaged", "English Learners with Transitional 1-4", "Students with Disabilities")
  for (subgroup in subgroups){
    if (subgroup == "Black/Hispanic/Native American"){
      hist_df <- student_df %>% 
        filter(bhn_group > 0) %>% 
        mutate(subgroup = "Black/Hispanic/Native American")
    } else if (subgroup == "Economically Disadvantaged") {
      hist_df <- student_df %>% 
        filter(economically_disadvantaged > 0) %>% 
        mutate(subgroup = "Economically Disadvantaged")
    }else if (subgroup == "English Learners with Transitional 1-4") {
      hist_df <- student_df %>% 
        filter(t1234 > 0 | el > 0) %>% 
        mutate(subgroup = "English Learners with Transitional 1-4")
    }else {
      hist_df <- student_df %>% 
        filter(special_ed > 0) %>% 
        mutate(subgroup = "Students with Disabilities")
    }
    hist_grouped <- total_by_subgroup_dist(hist_df)
    base_df <- rbind(base_df, hist_grouped)
  }
  return(base_df)
}


# ======================================= Success Rate ================================================
dist_totals <- cat_subgroups(sl, all_students) %>%
  arrange(system, subject, subgroup) %>% 
  rename(subject = subject)


dist_achievement <- dist_totals %>% 
  mutate(
    tested = if_else(valid_tests >= 30, tested, 0),
    enrolled = if_else(valid_tests >= 30, enrolled, 0),
    valid_tests = if_else(valid_tests >= 30, valid_tests, 0),
    n_on_track = if_else(valid_tests >= 30, n_on_track, NA_real_),
    n_mastered = if_else(valid_tests >= 30, n_mastered, NA_real_)
  ) %>% 
  group_by(system, subgroup, grade)  %>%
  summarise(
    participation_rate = round(100 * sum(tested)/sum(enrolled) + 1e-10, 0),
    n_count = sum(valid_tests),
    success_rate = round(((sum(n_on_track, na.rm = TRUE) + sum(n_mastered, na.rm = TRUE))/sum(valid_tests)) * 100 + 1e-10, 1)
  ) %>%
  ungroup() %>% 
  mutate(success_rate = ifelse(is.na(success_rate), NA, success_rate))

# ======================= District AMO Targets =======================

district_targets <- dist_achievement %>%
  filter(
    subgroup %in% subgroups
  ) %>%
  mutate(
    AMO_target = if_else(
      n_count >= 30,
      round(((100 - success_rate) / 16) + success_rate + 1e-10, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      n_count >= 30,
      round(((100 - success_rate) / 8) + success_rate + 1e-10, 1),
      NA_real_
    )
  ) %>% 
  left_join(system_df, by = 'system') %>% 
  transmute(
    system, system_name, grade, subgroup, n_count,
    metric = success_rate, AMO_target, AMO_target_double
  )

write_csv(district_targets, "N:/ORP_accountability/projects/2020_amo/success_rate_targets_district_AM.csv", na = "")

# ================ Prepare data for school targets =========================================

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

grade_pools <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv") %>% 
  select(system, school, pool, designation_ineligible) # %>% 
# mutate(pool = if_else(system == 600 & school == 40, 'HS', pool))

cte_alt_adult <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/cte_alt_adult_schools.csv") %>% 
  clean_names() %>% 
  mutate_at(.vars = c('district_number', 'school_number'),
            .funs = as.numeric) %>% 
  rename(school_name = school) %>% 
  rename(system = district_number, school = school_number)

student_level <- read_csv("N://ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv")

sl <- student_level %>%
  filter(!(system == 964 & school == 964 | system == 970 & school == 970)) %>%
  mutate_at("residential_facility", ~ if_else(is.na(.), 0, .)) %>%
  mutate_at("enrolled_50_pct_school", ~ if_else(is.na(.), "Y", .)) %>% 
  mutate(
    original_subject = case_when(
      grade < 9 & original_subject %in% c('Algebra I', 'Algebra II', "Geometry", "Integrated Math I", "Integrated Math II",
                                          "Integrated Math III", 'English I', 'English II', 'Biology I', 'Chemistry') ~ subject,
      TRUE ~ original_subject
    )
  )

# school_df <- student_level %>% 
#   select(system, system_name, school, school_name) %>% 
#   distinct()

school_df <- read_csv("N:\\ORP_accountability\\data\\2019_final_accountability_files\\names.csv")

integrated_math <- student_level %>% 
  filter(original_subject %in% c("Algebra I", "Integrated Math I")) %>% 
  count(system, original_subject) %>% 
  group_by(system) %>% 
  mutate(temp = max(n)) %>% 
  # Systems where Integrated Math is the max between that and Algebra I
  filter(n == temp, original_subject == "Integrated Math I")
# Vector with the sytems where that is the case
int_math_vec <- integrated_math[['system']]

# ACT score substitution
act_sub <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_school.csv") %>% 
  #left_join(school_df, by = c('system', 'school')) %>% 
  transmute(system, system_name, school, school_name,
            subject = case_when(
              subject == "ACT Math" & system %in% int_math_vec ~ "Integrated Math III",
              TRUE  ~ "Algebra II"
            ), grade = 11, subgroup = "All Students", valid_test = valid_tests, on_track = n_met_benchmark, mastered = 0
  ) 

# ========================================== Prep for Achievement =======================================
sl <- sl %>% 
  mutate(
    on_track = case_when(
      performance_level == "Proficient" | performance_level == "On Track" ~ 1,
      TRUE                      ~ 0
    ),
    mastered = case_when(
      performance_level == "Mastered" | performance_level == "Advanced" ~ 1,
      TRUE                      ~ 0
    )
  ) %>% 
  filter(residential_facility == 0, (enrolled_50_pct_school == 'Y' | (acct_system != system | school != acct_school)),  # homebound == 0, !is.na(state_student_id),grade %in% 3:12, 
         original_subject %in% c("Math", "ELA", math_eoc, english_eoc)) %>% 
  
  # fill(system_name) %>% 
  rename(subgroup = reported_race)

total_by_subgroup <- function(df) {
  out_df <- df %>% 
    group_by(acct_system, acct_school, subject, subgroup) %>% 
    summarise(
      enrolled = sum(enrolled, na.rm = TRUE),
      tested = sum(tested, na.rm = TRUE),
      valid_tests = sum( valid_test),
      n_on_track = sum(on_track),
      n_mastered = sum(mastered)
    ) %>% 
    ungroup() %>% 
    rename(system = acct_system, school = acct_school)
  return(out_df)
}

grouped_by_race <- total_by_subgroup(sl)

all_students <- sl %>% 
  bind_rows(act_sub %>% rename(acct_system = system, acct_school = school)) %>% 
  mutate(subgroup = "All Students") %>% 
  total_by_subgroup()

super_subgroup <- sl %>% 
  filter(bhn_group > 0 | economically_disadvantaged > 0 | t1234 > 0 | el > 0 | special_ed > 0) %>% 
  mutate(subgroup = "Super Subgroup") %>% 
  total_by_subgroup()

cat_subgroups <- function(student_df, students_grouped ){
  base_df = students_grouped
  subgroups <- c("Black/Hispanic/Native American", "Economically Disadvantaged", "English Learners with Transitional 1-4", "Students with Disabilities")
  for (subgroup in subgroups){
    if (subgroup == "Black/Hispanic/Native American"){
      hist_df <- student_df %>% 
        filter(bhn_group > 0) %>% 
        mutate(subgroup = "Black/Hispanic/Native American")
    } else if (subgroup == "Economically Disadvantaged") {
      hist_df <- student_df %>% 
        filter(economically_disadvantaged > 0) %>% 
        mutate(subgroup = "Economically Disadvantaged")
    }else if (subgroup == "English Learners with Transitional 1-4") {
      hist_df <- student_df %>% 
        filter(t1234 > 0 | el > 0) %>% 
        mutate(subgroup = "English Learners with Transitional 1-4")
    }else {
      hist_df <- student_df %>% 
        filter(special_ed > 0) %>% 
        mutate(subgroup = "Students with Disabilities")
    }
    hist_grouped <- total_by_subgroup(hist_df)
    base_df <- rbind(base_df, hist_grouped)
  }
  return(base_df)
}

state_totals <- cat_subgroups(sl, grouped_by_race) %>% 
  filter(subgroup != "Unknown") %>% 
  #rename(system = acct_system, school = acct_school) %>% 
  rbind(all_students, super_subgroup) %>% 
  arrange(system, school, subject, subgroup) %>% 
  # rename(subject = original_subject) %>% 
  mutate(
    subgroup = case_when(
      subgroup == "American Indian/Alaska Native" ~ "American Indian or Alaska Native",
      subgroup == "Hispanic/Latino" ~ "Hispanic",
      subgroup == "Native Hawaiian/Pac. Islander" ~ "Native Hawaiian or Other Pacific Islander",
      TRUE ~ subgroup
    )
  )


# ============================= Achievement =========================

school_achievement <- state_totals %>% 
  mutate(
    subject = case_when(
      subject %in% english_eoc ~ "HS English",
      subject %in% c(math_eoc, "HS Math") ~ "HS Math",
      TRUE ~ subject
    )
  ) %>% 
  group_by(system, school, subject, subgroup)  %>%
  summarise(
    enrolled = sum(enrolled, na.rm = TRUE),
    tested = sum(tested, na.rm = TRUE),
    valid_tests = sum(valid_tests, na.rm = TRUE),
    n_on_track = sum(n_on_track, na.rm = TRUE),
    n_mastered = sum(n_mastered, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    valid_tests = if_else(valid_tests >= 30, valid_tests, 0),
    n_on_track = if_else(valid_tests >= 30, n_on_track, 0),
    n_mastered = if_else(valid_tests >= 30, n_mastered, 0)
  ) %>% 
  group_by(system, school, subgroup)  %>%
  summarise(
    participation_rate = round5(100 * sum(tested)/sum(enrolled)),
    n_count = sum(valid_tests),
    success_rate = round(((sum(n_on_track) + sum(n_mastered))/sum(valid_tests)) * 100 + 1e-10, 1)
  ) %>% 
  ungroup() %>% 
  mutate(success_rate = ifelse(is.na(success_rate), NA, success_rate))

# ================= School targets ===========================

school_targets <- school_achievement %>%
  mutate(
    AMO_target = if_else(
      n_count >= 10,
      round(((100 - success_rate) / 16) + success_rate + 1e-10, 1),
      NA_real_
    ),
    AMO_target_double = if_else(
      n_count >= 10,
      round(((100 - success_rate) / 8) + success_rate + 1e-10, 1),
      NA_real_
    )
  ) %>% 
  left_join(school_df, by = c('system', 'school')) %>% 
  transmute(
    system, system_name, school, school_name, subgroup, n_count,
    metric = success_rate, AMO_target, AMO_target_double
  )

write_csv(school_targets, "N:/ORP_accountability/projects/2020_amo/success_rate_targets_school_AM.csv", na = "")
