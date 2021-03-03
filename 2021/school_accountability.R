library(tidyverse)
library(janitor)
library(readxl)
library(readstata13)
library(acct)

# Functions ====================
# Calculate ach totals by subgroup
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

ci_upper_bound <- function(df) {
  out_df <- df %>% 
    mutate(
      ci_bound = round(100 * (n_count/(n_count + (qnorm(0.975)^2)))*((metric/100) + ((qnorm(0.975)^2)/(2*n_count))  +   
                                                                       qnorm(0.975)* sqrt( (((metric/100) * (1 - (metric/100)))/ n_count) + ((qnorm(0.975)^2) / (4* n_count^2)))) + 1e-10,1)
    )
  return(out_df)
}

ci_lower_bound <- function(df) {
  out_df <- df %>% 
    mutate(
      ci_bound = round(100 * (n_count/(n_count + (qnorm(0.975)^2)))*((metric/100) + ((qnorm(0.975)^2)/(2*n_count))  -   
                                                                       qnorm(0.975)* sqrt( (((metric/100) * (1 - (metric/100)))/ n_count) + ((qnorm(0.975)^2) / (4* n_count^2)))) + 1e-10,1)
    )
  return(out_df)
}


# School Information =====================
school_df <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv")

grade_pools <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv") %>% 
  select(system, school, pool, designation_ineligible) %>% 
  mutate(pool = if_else(system == 600 & school == 40, 'HS', pool))

cte_alt_adult <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/cte_alt_adult_schools.csv") %>% 
  clean_names() %>% 
  mutate_at(.vars = c('district_number', 'school_number'),
            .funs = as.numeric) %>% 
  rename(school_name = school) %>% 
  rename(system = district_number, school = school_number)

# Subjects =======================
math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

# Student Level ==============
student_level <- read_csv("N://ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv")

integrated_math <- student_level %>% 
  filter(original_subject %in% c("Algebra I", "Integrated Math I")) %>% 
  count(system, original_subject) %>% 
  group_by(system) %>% 
  mutate(temp = max(n)) %>% 
  # Systems where Integrated Math is the max between that and Algebra I
  filter(n == temp, original_subject == "Integrated Math I")

# Vector with the sytems where that is the case
int_math_vec <- integrated_math[['system']]

# Previous AMO data
amo_achievement <- read_csv("N:/ORP_accountability/projects/2019_amo/success_rate_targets_school.csv") %>% 
  transmute(system, school, subgroup, metric_prior = success_rate_prior, AMO_target, AMO_target_double)

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
  ) %>% 
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
  filter(residential_facility == 0, 
         (enrolled_50_pct_school == 'Y' | (acct_system != system | school != acct_school)),  # homebound == 0, !is.na(state_student_id),grade %in% 3:12, 
         original_subject %in% c("Math", "ELA", math_eoc, english_eoc)) %>% 
  
  # fill(system_name) %>% 
  rename(subgroup = reported_race)

state_totals <- bind_rows(
  sl %>% filter(subgroup != "Unknown"), # Race/ethnicity subgroups
  sl %>% filter(bhn_group > 0) %>% mutate(subgroup = "Black/Hispanic/Native American"),
  sl %>% filter(economically_disadvantaged > 0) %>% mutate(subgroup = "Economically Disadvantaged"),
  sl %>% filter(t1234 > 0 | el > 0) %>% mutate(subgroup = "English Learners with Transitional 1-4"),
  sl %>% filter(special_ed > 0) %>% mutate(subgroup = "Students with Disabilities"),
  sl %>% 
    filter(bhn_group > 0 | economically_disadvantaged > 0 | t1234 > 0 | el > 0 | special_ed > 0) %>% 
    mutate(subgroup = "Super Subgroup"),
  sl %>% 
    bind_rows(act_sub %>% rename(acct_system = system, acct_school = school)) %>% 
    mutate(subgroup = "All Students")
) %>% 
  total_by_subgroup() %>% 
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


# ====================== TCAP Participation Rate =======================
school_assessment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_assessment_file.csv")

tcap_participation <- school_assessment %>% 
  filter(year == 2019, grade == 'All Grades', subject %in% c("Math", "ELA", math_eoc, english_eoc)) %>% 
  group_by(system, system_name, school, school_name, subgroup) %>%
  summarise(
    n_enrolled = sum(enrolled),
    n_tested = sum(tested)
    ) %>% 
  ungroup() %>% 
  mutate(
    participation_rate = round(n_tested/n_enrolled * 100 + 1e-5, 0)
  )

# saveRDS(tcap_participation, "data/tcap_participation_2018.rds")

# ============================= Achievement =========================

school_achievement <- state_totals %>% 
  mutate(
    subject = case_when(
      subject %in% english_eoc ~ "HS English",
      subject %in% c(math_eoc, "HS Math") ~ "HS Math",
      TRUE ~ subject
      )
    ) %>% 
  # Need to have 30 tests in subject (e.g HS Math or Math)
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
    valid_tests = if_else(valid_tests > 29, valid_tests, 0),
    n_on_track = if_else(valid_tests > 29, n_on_track, 0),
    n_mastered = if_else(valid_tests > 29, n_mastered, 0),
    enrolled_pr = if_else(valid_tests > 29, enrolled, 0)
  ) %>% 
  group_by(system, school, subgroup)  %>%
   summarise(
     participation_rate = round5(100 * sum(tested)/sum(enrolled)),
      n_count = sum(valid_tests),
      success_rate = if_else(participation_rate >= 95,
                             round(((sum(n_on_track) + sum(n_mastered))/sum(valid_tests)) * 100 + 1e-10, 1),
                             round(((sum(n_on_track) + sum(n_mastered))/(round(0.95* sum(enrolled_pr)+1e-10)) ) * 100 + 1e-10, 1))
   ) %>% 
  ungroup() %>% 
  mutate(success_rate = ifelse(is.na(success_rate), NA, success_rate)) %>% 
  left_join(grade_pools, by = c('system', 'school')) %>% 
  left_join(school_df, by = c('system', 'school')) %>% 
  # Filtered out if No pool or school name
  # filter(!is.na(pool), !is.na(school_name)) %>% 
  transmute(system, system_name, school, school_name, pool, designation_ineligible, indicator = 'Achievement', 
            subgroup,participation_rate, n_count, metric = success_rate) %>% 
  # Upper bound confidence interval function
  ci_upper_bound() %>% 
  # Join amo target df
  left_join(amo_achievement, by = c('system', 'school', 'subgroup')) %>% 
  mutate(
    score_abs = case_when(
      metric >= 45 ~ 4,
      metric >= 35 ~ 3,
      metric >= 27.5 ~ 2,
      metric >= 20 ~ 1,
      metric >= 0 ~ 0
    ),
    score_target = case_when(
      metric >= AMO_target_double ~ 4,
      metric >= AMO_target ~ 3,
      ci_bound >= AMO_target ~ 2,
      ci_bound > metric_prior ~ 1,
      ci_bound <= metric_prior ~ 0
    ),
    score = pmax(score_abs, score_target)
  ) %>% 
  # Join tnready participation and set to 0 if below 95
  # left_join(tcap_participation %>% select(system:subgroup, participation_rate), by = c('system','system_name', 'school', 'school_name', 'subgroup')) %>% 
  mutate(
    score_abs = if_else(participation_rate < 95 & n_count > 0 & !is.na(score_abs), 0, score_abs),
    score_target = if_else(participation_rate < 95 & n_count > 0 & !is.na(score_target), 0, score_target),
    score = if_else(participation_rate < 95 & n_count > 0 & !is.na(score), 0, score),
    participation_rate = if_else(n_count == 0, NA_real_, participation_rate),
    grade = case_when(
      score == 4 ~ 'A',
      score == 3 ~ 'B',
      score == 2 ~ 'C',
      score == 1 ~ 'D',
      score == 0 ~ 'F',
      TRUE ~ NA_character_
    )
  ) %>% 
  # Select desired columns
  select(system:subgroup, participation_rate, n_count:grade)


# =================================== Growth =================================================

tvaas <- read_excel("N:/ORP_accountability/data/2019_tvaas/2019-School-Level-Accountability-Results-EOC-TCAP.xlsx") %>%
  clean_names() %>%
  rename(system_name = system, system = system_number, school = school_number) %>%
  # Filter out school or system == 0
  filter(school !=0, system != 0) %>% 
  mutate(system = as.integer(system), school = as.integer(school),
         subgroup = case_when(
           subgroup == "American Indian/Alaska Native" ~ "American Indian or Alaska Native",
           subgroup == "Hispanic/Latino" ~ "Hispanic",
           subgroup == "Native Hawaiian/Pac. Islander" ~ "Native Hawaiian or Other Pacific Islander",
           subgroup == "Black" ~ "Black or African American",
           subgroup == "English Learners (includes EL and T1-4)" ~ "English Learners with Transitional 1-4",
           subgroup == "Hawaiian Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
           subgroup == "Native American" ~ "American Indian or Alaska Native",
           TRUE ~ subgroup
         )
  ) %>% 
  group_by(system, school, subgroup) %>%
  # Select best score between All Grades and All Grades, No Grade 3
  mutate(best = max(index)) %>%
  ungroup() %>% 
  filter(index == best) %>% 
  select(system, school, subgroup, number_of_students, index, level, best) %>% 
  group_by(system, school, subgroup) %>%
  # Select best score between All Grades and All Grades, No Grade 3
  mutate(most_students = max(number_of_students)) %>%
  ungroup() %>% 
  filter(number_of_students == most_students) %>% 
  distinct() %>% 
  left_join(grade_pools, by = c('system', 'school')) %>%
  left_join(school_df, by = c('system', 'school'))  %>%
  # filter(!is.na(pool), !is.na(school_name)) %>% 
  transmute(system, system_name, school, school_name, pool, designation_ineligible, indicator = "Growth", subgroup,
            n_count = number_of_students,
            metric = level,
            # ci_bound = NA_real_, metric_prior = NA_real_, AMO_target = NA_real_, AMO_target_double = NA_real_, score_abs = NA_real_, score_target = NA_real_,
            score = case_when(
              metric == 5 ~ 4,
              metric == 4 ~ 3,
              metric == 3 ~ 2,
              metric == 2 ~ 1,
              metric == 1 ~ 0
            ),
            grade = case_when(
              score == 4 ~ 'A',
              score == 3 ~ 'B',
              score == 2 ~ 'C',
              score == 1 ~ 'D',
              score == 0 ~ 'F',
              TRUE ~ NA_character_
            )
  ) %>% 
  distinct()

# Compare to previous files
  
# tvaas_comp <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2018_school_accountability_file.csv") %>% 
#   filter(indicator == "Growth")
# 
# comp_tvaas <- setdiff(tvaas, tvaas_comp)

# ==================================== Graduation Rate ========================================

amo_grad <- read_csv("N:/ORP_accountability/projects/2019_amo/grad_school.csv") %>% 
  filter(!grepl("Non-", subgroup)) %>% 
  transmute(
    system, school, 
    subgroup = case_when(
      subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
      TRUE ~ subgroup
    ),
    metric_prior = if_else(grad_cohort >= 30, grad_rate, NA_real_), 
    AMO_target, AMO_target_double
  ) # %>% 
  # filter(!(subgroup == "Native Hawaiian or Other Pacific Islander" & n_count == 0))

grad_2019 <- read_csv("N:/ORP_accountability/data/2018_graduation_rate/school_grad_rate.csv") %>% 
  filter(school !=0, system != 0, !grepl("Non-", subgroup), !subgroup %in% c('Male', 'Female', 'Homeless', 'Migrant'), 
         !(system == 90 & school == 7)) %>% 
  transmute(
    system, system_name, school, school_name, indicator = "Graduation Rate",
    subgroup = case_when(
      subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
      TRUE ~ subgroup
    ),
    n_count = if_else(grad_cohort >= 30, grad_cohort, 0), 
    # n_count = if_else(grad_cohort >= 20, grad_cohort, 0), 
    metric = if_else(n_count > 0, grad_rate, NA_real_)
  ) %>% 
  ci_upper_bound() %>% 
  left_join(amo_grad, by = c('system', 'school', 'subgroup')) %>% 
  mutate(
    score_abs = case_when(
      metric >= 95 ~ 4,
      metric >= 90 ~ 3,
      metric >= 80 ~ 2,
      metric >= 67 ~ 1,
      metric >= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    score_target = case_when(
      metric >= AMO_target_double ~ 4,
      metric >= AMO_target ~ 3,
      ci_bound >= AMO_target ~ 2,
      ci_bound > metric_prior ~ 1,
      ci_bound <= metric_prior ~ 0,
      TRUE ~ NA_real_
    ),
    score = pmax(score_abs, score_target),
    grade = case_when(
      score == 4 ~ 'A',
      score == 3 ~ 'B',
      score == 2 ~ 'C',
      score == 1 ~ 'D',
      score == 0 ~ 'F',
      TRUE ~ NA_character_
    )
  ) %>% 
  left_join(grade_pools, by = c("system", "school")) %>% 
  select(system, school, indicator, subgroup:designation_ineligible) %>% 
  left_join(school_df, by = c('system', 'school')) %>% 
  transmute(system, system_name, school, school_name, pool, designation_ineligible, indicator, subgroup, n_count, metric,
            ci_bound, metric_prior, AMO_target, AMO_target_double, score_abs, score_target, score, grade)#  %>% 
  # filter(!is.na(system_name), !is.na(pool))

# Compare to previous files

# grad_comp <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2018_school_accountability_file.csv") %>% 
#   filter(indicator == "Graduation Rate")
# 
# comp_grad <- setdiff(grad_2018, grad_comp)

# ========================== ACT/SAT Participation =============================================
act_partic_concat <- function(df){
  out_df <- bind_rows(
    df %>% mutate(subgroup = "All Students"),
    df %>% filter(race_ethnicity == 'B' | race_ethnicity == 'H' | race_ethnicity == 'I') %>% mutate(subgroup = "Black/Hispanic/Native American"),
    df %>% filter(econ_dis == 'Y') %>% mutate(subgroup = "Economically Disadvantaged"),
    df %>% filter(elb == 'Y') %>% mutate(subgroup = "English Learners with Transitional 1-4"),
    df %>% filter(race_ethnicity == 'B' | race_ethnicity == 'H' | race_ethnicity == 'I' | elb == 'Y' | econ_dis == 'Y' | swd == 'Y') %>% mutate(subgroup = "Super Subgroup"),
    df %>% filter(race_ethnicity == 'I') %>% mutate(subgroup = "American Indian or Alaska Native"),
    df %>% filter(race_ethnicity == 'A') %>% mutate(subgroup = "Asian"),
    df %>% filter(race_ethnicity == 'B') %>% mutate(subgroup = "Black or African American"),
    df %>% filter(race_ethnicity == 'H') %>% mutate(subgroup = "Hispanic"),
    df %>% filter(race_ethnicity == 'W') %>% mutate(subgroup = "White"),
    df %>% filter(swd == 'Y') %>% mutate(subgroup = "Students with Disabilities")
  )
  return(out_df)
}

ready_grad_participation_rate_n <- read_csv('N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_student_level_06182019.csv',
                                     col_types = 'icccciccciiciiiiiiiiiiiiiiiiiiic') %>%
  rename(system = district_no, school = school_no) %>%
  mutate(
    cohort_indicator = if_else(included_in_cohort == 'Y', 1, 0),
    ready_grad_indicator = if_else(ready_graduate == 'Y', 1, 0),
    completed_act_or_sat = if_else((sat_total > 0 | act_composite > 0) & included_in_cohort == 'Y' & completion_type %in% c(1, 11, 12, 13), 1, 0),
    on_time_grad = if_else(included_in_cohort == 'Y' & completion_type %in% c(1, 11, 12, 13), 1, 0)
  ) %>% 
  act_partic_concat() %>%
  group_by(system, school, subgroup) %>%
  summarise(
    n_on_time_grads = sum(on_time_grad, na.rm = TRUE),
    n_completed_act_or_sat = sum(completed_act_or_sat, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    participation_rate = round(n_completed_act_or_sat/n_on_time_grads * 100 + 1e-5, 0)
  ) %>%
  arrange(system, school, subgroup) %>%
  mutate(participation_rate = if_else(n_on_time_grads < 20, NA_real_, participation_rate))
  # mutate(participation_rate = if_else(is.na(participation_rate), NA_real_, participation_rate))


# ====================== Ready Grad ========================

amo_ready_grad <- read_csv("N:/ORP_accountability/projects/2019_amo/ready_grad_school.csv") %>% 
  filter(!grepl("Non-", subgroup)) %>% 
  transmute(
    system, school, 
    subgroup = case_when(
      subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
      TRUE ~ subgroup
    ),
    metric_prior = if_else(grad_cohort >= 30, pct_ready_grad, NA_real_), 
    AMO_target, AMO_target_double
  )

ready_grad <- read_csv("N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_school.csv") %>% 
  rename(participation_rate = act_participation_rate) %>% 
  filter(school !=0, system != 0, !grepl("Non-", subgroup)) %>% #
  transmute(
    system, school, indicator = 'Ready Graduates',
    subgroup = case_when(
      subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
      TRUE ~ subgroup
    ), participation_rate,
    n_count = ifelse(n_count >= 30, n_count, 0), 
    # n_count = ifelse(n_count >= 20, n_count, 0),
    metric = ifelse(n_count > 0, pct_ready_grad, NA_real_)
  ) %>% 
  ci_upper_bound() %>% 
  left_join(amo_ready_grad, by = c('system', 'school', 'subgroup')) %>% 
  mutate(
    score_abs = case_when(
      metric >= 40 ~ 4,
      metric >= 30 ~ 3,
      metric >= 25 ~ 2,
      metric >= 16 ~ 1,
      metric >= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    score_target = case_when(
      metric >= AMO_target_double ~ 4,
      metric >= AMO_target ~ 3,
      ci_bound >= AMO_target ~ 2,
      ci_bound > metric_prior ~ 1,
      ci_bound <= metric_prior ~ 0,
      TRUE ~ NA_real_
    ),
    score = pmax(score_abs, score_target)
  ) %>% 
  left_join(grade_pools, by = c("system", "school")) %>% 
  select(system, school, indicator, subgroup:designation_ineligible) %>% 
  left_join(school_df, by = c('system', 'school')) %>% 
  transmute(system, system_name, school, school_name, pool, designation_ineligible, indicator, subgroup, participation_rate, n_count, metric,
            ci_bound, metric_prior, AMO_target, AMO_target_double, score_abs, score_target, score) %>% 
  # filter(!is.na(system_name), !is.na(pool)) %>% 
  # left_join(ready_grad_participation_rate %>% select(system, school, subgroup, participation_rate), by = c('system', 'school', 'subgroup')) %>% 
  mutate(
    participation_rate = if_else(n_count == 0, NA_real_, participation_rate), 
    score_abs = if_else(!is.na(participation_rate) & participation_rate < 95 & !is.na(score_abs) , 0, score_abs),
    score_target = if_else(!is.na(participation_rate) & participation_rate < 95 & !is.na(score_target), 0, score_target),
    score = if_else(!is.na(participation_rate) & participation_rate < 95 & !is.na(score), 0, score),
    grade = case_when(
      score == 4 ~ 'A',
      score == 3 ~ 'B',
      score == 2 ~ 'C',
      score == 1 ~ 'D',
      score == 0 ~ 'F',
      TRUE ~ NA_character_
    )
  ) %>% 
  select(system, system_name, school, school_name, pool, designation_ineligible, indicator, subgroup, participation_rate,
         n_count, metric, ci_bound, metric_prior, AMO_target, AMO_target_double, score_abs, score_target, score, grade)

# Compare to previous files

# ready_comp <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2018_school_accountability_file.csv") %>% 
#   filter(indicator == "Ready Graduates")
# 
# comp_ready <- setdiff(ready_grad, ready_comp)

# ======================= Chronic Absenteeism ===============================

amo_absenteeism <- read_csv("N:/ORP_accountability/projects/2019_amo/absenteeism_targets_school_primary_enrollment.csv",
                            col_types = "iicicccninnn") %>% 
  transmute(
    system, school, 
    subgroup = case_when(
      subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
      TRUE ~ subgroup
    ),
    metric_prior = if_else(n_students >= 30, pct_chronically_absent, NA_real_), 
    AMO_target = AMO_target, AMO_target_double = AMO_target_double
  )

absenteeism <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/school_chronic_absenteeism_Jul11.csv",
                        col_types = "icicccnnn") %>% 
  filter(school !=0, system != 0) %>% 
  transmute(
    system, school, indicator = 'Chronic Absenteeism',
    subgroup = case_when(
      subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
      TRUE ~ subgroup
    ),
    n_count = ifelse(n_students >= 30, n_students, 0), 
    metric = ifelse(n_count > 0, pct_chronically_absent, NA_real_)
  ) %>% 
  ci_lower_bound() %>% 
  left_join(amo_absenteeism, by = c('system', 'school', 'subgroup')) %>% 
  left_join(grade_pools, by = c("system", "school"))  %>% 
  mutate(
    score_abs = case_when(
      pool == "K8" & metric <= 6 ~ 4,
      pool == "K8" & metric <= 9 ~ 3,
      pool == "K8" & metric <= 13 ~ 2,
      pool == "K8" & metric <= 20 ~ 1,
      pool == "K8" & metric > 20 ~ 0,
      pool == "HS" & metric <= 10 ~ 4,
      pool == "HS" & metric <= 14 ~ 3,
      pool == "HS" & metric <= 20 ~ 2,
      pool == "HS" & metric <= 30 ~ 1,
      pool == "HS" & metric > 30 ~ 0,
      TRUE ~ NA_real_
    ),
    score_target = case_when(
      metric <= AMO_target_double ~ 4,
      metric <= AMO_target ~ 3,
      ci_bound <= AMO_target ~ 2,
      ci_bound < metric_prior ~ 1,
      ci_bound >= metric_prior ~ 0,
      TRUE ~ NA_real_
    ),
    score = pmax(score_abs, score_target),
    grade = case_when(
      score == 4 ~ 'A',
      score == 3 ~ 'B',
      score == 2 ~ 'C',
      score == 1 ~ 'D',
      score == 0 ~ 'F',
      TRUE ~ NA_character_
    )
  ) %>% 
  left_join(school_df, by = c('system', 'school')) %>% 
  transmute(system, system_name, school, school_name, pool, designation_ineligible, indicator, subgroup, n_count, metric,
            ci_bound, metric_prior, AMO_target, AMO_target_double, score_abs, score_target, score, grade) # %>% 
  # filter(!is.na(system_name), !is.na(pool))

# Compare to previous files

# absent_comp <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2018_school_accountability_file.csv") %>% 
#   filter(indicator == "Chronic Absenteeism")
# 
# comp_absent <- setdiff(absenteeism, absent_comp)

# ===================================== ELPA ========================

elpa <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_school.csv") %>% 
  filter(school !=0, system != 0, !grepl("Non-", subgroup))  %>% 
  mutate(subgroup = if_else(subgroup == 'English Learners', "English Learners with Transitional 1-4", subgroup)) %>% 
  transmute(
    system, school, indicator = 'ELPA Growth Standard',
    subgroup,
    n_count = ifelse(growth_standard_denom >= 10, growth_standard_denom, 0), 
    metric = ifelse(n_count > 0, pct_met_growth_standard, NA_real_),
    # ci_bound = NA_real_, metric_prior = NA_real_, AMO_target = NA_real_, AMO_target_double = NA_real_,
    score_abs = NA_real_, score_target = NA_real_,
    score = case_when(
      metric >= 60 ~ 4,
      metric >= 50 ~ 3,
      metric >= 40 ~ 2,
      metric >= 25 ~ 1,
      metric < 25 ~ 0
    ), 
    #score = pmax(score_abs, score_target), #score_target = NA_real_, score = NA_real_,
    grade = case_when(
      score == 4 ~ 'A',
      score == 3 ~ 'B',
      score == 2 ~ 'C',
      score == 1 ~ 'D',
      score == 0 ~ 'F',
      TRUE ~ NA_character_
    )
  ) %>% 
  left_join(grade_pools, by = c("system", "school")) %>% 
  left_join(school_df, by = c('system', 'school')) %>% 
  transmute(system, system_name, school, school_name, pool, designation_ineligible, indicator, subgroup, n_count, metric,
            score_abs, score_target, score, grade) # %>% # ci_bound, metric_prior, AMO_target, AMO_target_double, score_target, score
  # filter(!is.na(system_name), !is.na(pool))

# elpa_comp <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2018_school_accountability_file.csv") %>% 
#   filter(indicator == "ELPA Growth Standard")
# 
# comp_elpa <- setdiff(elpa, elpa_comp)

total_accountability <- bind_rows(school_achievement, tvaas, grad_2019, absenteeism, elpa, ready_grad) %>% 
  # anti_join(cte_alt_adult, by = c('system', 'school')) %>% 
  filter(!is.na(system_name)) %>%  # , designation_ineligible != 1 , !is.na(pool)
  select(system:participation_rate, n_count:grade) %>% 
  select(-grade) %>% 
  arrange(system, school, indicator, subgroup) %>% 
  distinct()
# total_comp <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2018_school_accountability_file.csv")
# 
# comp_total <- setdiff(total_accountability, total_comp)

# historic_underserved_subgroups <- c('All Students', 'Black/Hispanic/Native American', 'Economically Disadvantaged', 
#                "English Learners with Transitional 1-4", 'Students with Disabilities', 'Super Subgroup')
# 
# write_csv(total_accountability, "N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file_AM_Aug14.csv", na = "")
# write_csv(total_accountability %>% filter(system == 600, school == 40), "N:/ORP_accountability/projects/Andrew/Accountability/2019/appeal data/maury_appeal_hs_Aug20_n_20.csv", na = "")
# write_csv(total_accountability, "N:/ORP_accountability/projects/Andrew/Accountability/2019/appeal data/maury_appeal_full_data_hs_n_20.csv", na = "")
# saveRDS(total_accountability, "N:/ORP_accountability/projects/Andrew/Pre-Coding-2019/school_rds.rds")

alex_comp <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv")

am_comp <- total_accountability %>% 
  select(-designation_ineligible) # %>% 
  # filter(indicator != 'Growth')
# 
comp_total <- setdiff(am_comp, alex_comp) %>% 
  bind_rows(setdiff(alex_comp, am_comp)) %>% 
  arrange(system, school, subgroup, indicator)











