# Code to suppress files for data downloads

library(tidyverse)

# Supression Rules
# Any pct > 95 or < 5 at school level - **
# Any pct > 99 or < 1 at district/state level - **
# N count < 10 - *

# ================== Assessment =====================
suppress_assessment <- function(file, threshold = 1) {
  
  file %>%
    mutate_at(
      vars(pct_below, pct_approaching, pct_on_track, pct_mastered),
      ~ round(. + 1e-10, 1)
    ) %>% 
    rowwise() %>%
    mutate(
      temp = any(
        pct_below < threshold, pct_below > (100 - threshold),
        pct_approaching < threshold, pct_approaching > (100 - threshold),
        pct_on_track < threshold, pct_on_track > (100 - threshold),
        pct_mastered < threshold, pct_mastered > (100 - threshold))
    ) %>%
    ungroup() %>%
    mutate_at(
      vars(n_below, n_approaching, n_on_track, n_mastered,
           pct_below, pct_approaching, pct_on_track, pct_mastered),
      ~ if_else(temp, "**", as.character(.))
    ) %>%
    select(-temp) %>%
    # Pct On Track/Mastered suppressed separately
    mutate(pct_on_mastered = if_else(pct_on_mastered < threshold | pct_on_mastered > (100 - threshold), "**", as.character(pct_on_mastered))) %>%
    mutate_at(
      vars(n_below, n_approaching, n_on_track, n_mastered,
           pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered),
      ~ if_else(valid_tests < 10, "*", as.character(.)))
  
}
# Supress assessment files
assessment_files <- map(
  grep('.+_assessment_file.csv', list.files('N:/ORP_accountability/data/2020_final_accountability_files', full.names = TRUE), value = TRUE),
  .f = ~ read_csv(.) %>% filter(year == 2020)
)

state <- assessment_files[[3]] %>% 
  suppress_assessment()
district <- assessment_files[[1]] %>% 
  suppress_assessment()
school <- assessment_files[[2]] %>% 
  suppress_assessment(threshold = 5)

write_csv(state, 'N:/ORP_accountability/data/2020_final_accountability_files/state_assessment_file_suppressed.csv', na = '')
write_csv(district, 'N:/ORP_accountability/data/2020_final_accountability_files/district_assessment_file_suppressed.csv', na = '')
write_csv(school, 'N:/ORP_accountability/data/2020_final_accountability_files/school_assessment_file_suppressed.csv', na = '')

# ==================== Suppress Absenteeism ========================
suppress_abs <- function(file, threshold = 1) {
  
  file %>%
    mutate_at(
      .vars = c('n_chronically_absent', 'pct_chronically_absent'),
      .funs = ~ if_else(pct_chronically_absent < threshold, '**', as.character(.))
    ) %>% 
    mutate_at(
      .vars = c('n_chronically_absent', 'pct_chronically_absent'),
      .funs = ~ if_else(n_students < 10, '*', as.character(.))
    )
  
}

state_abs <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/state_chronic_absenteeism_Aug14.csv")
district_abs <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/district_chronic_absenteeism_Aug14.csv")
school_abs <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/school_chronic_absenteeism_Aug14.csv")

write_csv(state_abs %>% suppress_abs(), "N:/ORP_accountability/data/2020_chronic_absenteeism/state_chronic_absenteeism_suppressed.csv", na = '')
write_csv(district_abs %>% suppress_abs(), "N:/ORP_accountability/data/2020_chronic_absenteeism/district_chronic_absenteeism_suppressed.csv", na = '')
write_csv(school_abs %>% suppress_abs(threshold = 5), "N:/ORP_accountability/data/2020_chronic_absenteeism/school_chronic_absenteeism_suppressed.csv", na = '')


# ================== Ready Grad =====================
suppress_rg <- function(file, threshold = 1) {
  
  file %>%
    mutate_at(
      .vars = vars(n_ready_grad, pct_ready_grad),
      .funs = ~ if_else(pct_ready_grad < threshold | pct_ready_grad > (100 - threshold), "**", as.character(.))
    ) %>%
    mutate_at(
      .vars = vars(n_ready_grad, pct_ready_grad),
      .funs = ~ if_else(n_count < 10, "*", as.character(.))
    )
  
}

state <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_state.csv") %>% 
  select(-act_participation_rate) %>%
  suppress_rg()
district <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_district.csv") %>% 
  select(-act_participation_rate) %>%
  suppress_rg()
school <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_school.csv") %>% 
  select(-act_participation_rate) %>%
  suppress_rg(threshold = 5)

write_csv(state, "N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_state_suppressed.csv", na = '')
write_csv(district, "N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_district_suppressed.csv", na = '')
write_csv(school, "N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_school_suppressed.csv", na = '')

# ============== ELPA =====================
suppress_elpa <- function(file, threshold = 1) {
  
  file %>%
    mutate_at(
      .vars = vars(n_exit, pct_exit),
      # .vars = vars(n_met_exit_criteria, pct_met_exit_criteria),
      .funs = ~ if_else(pct_exit < threshold | pct_exit > (100 - threshold), "**", as.character(.))
      # .funs = ~ if_else(pct_met_exit_criteria < threshold | pct_met_exit_criteria > (100 - threshold), "**", as.character(.))
    ) %>%
    mutate_at(
      .vars = vars(n_exit, pct_exit, composite_average, literacy_average),
      # .vars = vars(n_met_exit_criteria, pct_met_exit_criteria, composite_average, literacy_average),
      .funs = ~ if_else(exit_denom < 10, "*", as.character(.))
      # .funs = ~ if_else(valid_tests < 10, "*", as.character(.))
    ) %>%
    mutate_at(
      .vars = vars(n_met_growth_standard, pct_met_growth_standard),
      .funs = ~ if_else(pct_met_growth_standard < threshold | pct_met_growth_standard > (100 - threshold), "**", as.character(.))
    ) %>%
    mutate_at(
      .vars = vars(n_met_growth_standard, pct_met_growth_standard),
      .funs = ~ if_else(growth_standard_denom < 10, "*", as.character(.))
    )
  
}


state <- read_csv("N:/ORP_accountability/data/2020_ELPA/wida_growth_standard_state.csv") %>% 
  suppress_elpa()
district <- read_csv("N:/ORP_accountability/data/2020_ELPA/wida_growth_standard_district.csv") %>% 
  suppress_elpa()
school <- read_csv("N:/ORP_accountability/data/2020_ELPA/wida_growth_standard_school.csv") %>% 
  suppress_elpa(threshold = 5)

write_csv(state, "N:/ORP_accountability/data/2020_ELPA/wida_growth_standard_state_suppressed.csv", na = '')
write_csv(district, "N:/ORP_accountability/data/2020_ELPA/wida_growth_standard_district_suppressed.csv", na = '')
write_csv(school, "N:/ORP_accountability/data/2020_ELPA/wida_growth_standard_school_suppressed.csv", na = '')


# ===================== Graduation Rate ====================
suppress_grad <- function(file, bottom_threshold = 1, top_threshold = 1) {
  
  file %>%
    mutate_at(
      .vars = vars(grad_count, grad_rate),
      .funs = ~ if_else(grad_rate < bottom_threshold | grad_rate > (100 - top_threshold), "**", as.character(.))
      # .funs = ~ if_else(grad_rate < threshold | (grad_rate > (100 - threshold) & grad_rate != 100), "**", as.character(.))
    ) %>%
    mutate_at(
      .vars = vars(grad_count, grad_rate),
      .funs = ~ if_else(grad_cohort < 10, "*", as.character(.))
    )
  
}

state <- read_csv("N:/ORP_accountability/data/2020_graduation_rate/state_grad_rate.csv") %>% 
  suppress_grad()
district <- read_csv("N:/ORP_accountability/data/2020_graduation_rate/district_grad_rate.csv") %>% 
  suppress_grad()
school <- read_csv("N:/ORP_accountability/data/2020_graduation_rate/school_grad_rate.csv") %>% 
  suppress_grad(top_threshold = 1, bottom_threshold = 5)

write_csv(state, "N:/ORP_accountability/data/2020_graduation_rate/state_grad_rate_suppressed.csv", na = '')
write_csv(district, "N:/ORP_accountability/data/2020_graduation_rate/district_grad_rate_suppressed.csv", na = '')
write_csv(school, "N:/ORP_accountability/data/2020_graduation_rate/school_grad_rate_suppressed.csv", na = '')

# ==================== ACT Highest Cohort ====================
district_current <- read_csv("N:/ORP_accountability/data/2020_ACT/ACT_district_post_appeals_AM.csv") %>%
  filter(
    subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
                    "English Learners", "Students with Disabilities")
  ) %>%
  select(
    District = system,
    `District Name` = system_name,
    Subgroup = subgroup,
    `Valid Tests` = valid_test,
    `Participation Rate` = participation_rate,
    `Average English Score` = english,
    `Average Math Score` = math,
    `Average Reading Score` = reading,
    `Average Science Score` = science,
    `Average Composite Score` = composite,
    `Number Scoring 21 or Higher` = n_21_or_higher,
    `Percent Scoring 21 or Higher` = pct_21_or_higher,
    `Number Scoring Below 19` = n_below_19,
    `Percent Scoring Below 19` = pct_below_19
  ) %>%
  mutate_at(vars(`Participation Rate`:`Percent Scoring Below 19`), ~ if_else(`Valid Tests` > 0 & `Valid Tests` < 10, "*", as.character(.))) %>%
  mutate(
    flag = `Valid Tests` >= 10 & 
      as.numeric(`Percent Scoring 21 or Higher`) < 1 | as.numeric(`Percent Scoring 21 or Higher`) > 99 |
      as.numeric(`Percent Scoring Below 19`) < 1 | as.numeric(`Percent Scoring Below 19`) > 99
  ) %>%
  mutate_at(vars(`Number Scoring 21 or Higher`:`Percent Scoring Below 19`), ~ if_else(not_na(flag) & flag, "**", as.character(.))) %>%
  select(-flag)

school_current <- read_csv("N:/ORP_accountability/data/2020_ACT/ACT_school_post_appeals_AM.csv") %>%
  filter(
    subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
                    "English Learners", "Students with Disabilities")
  ) %>%
  select(
    District = system,
    `District Name` = system_name,
    School = school,
    `School Name` = school_name,
    Subgroup = subgroup,
    `Valid Tests` = valid_test,
    `Participation Rate` = participation_rate,
    `Average English Score` = english,
    `Average Math Score` = math,
    `Average Reading Score` = reading,
    `Average Science Score` = science,
    `Average Composite Score` = composite,
    `Number Scoring 21 or Higher` = n_21_or_higher,
    `Percent Scoring 21 or Higher` = pct_21_or_higher,
    `Number Scoring Below 19` = n_below_19,
    `Percent Scoring Below 19` = pct_below_19
  ) %>%
  mutate_at(vars(`Participation Rate`:`Percent Scoring Below 19`), ~ if_else(`Valid Tests` > 0 & `Valid Tests` < 10, "*", as.character(.))) %>%
  mutate(
    flag = `Valid Tests` >= 10 & 
      as.numeric(`Percent Scoring 21 or Higher`) < 5 | as.numeric(`Percent Scoring 21 or Higher`) > 95 |
      as.numeric(`Percent Scoring Below 19`) < 5 | as.numeric(`Percent Scoring Below 19`) > 95
  ) %>%
  mutate_at(vars(`Number Scoring 21 or Higher`:`Percent Scoring Below 19`), ~ if_else(not_na(flag) & flag, "**", as.character(.))) %>%
  select(-flag)

state_act <- read_csv("N:/ORP_accountability/data/2020_ACT/ACT_state_post_appeals_AM.csv") %>%
  filter(
    subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
                    "English Learners", "Students with Disabilities")
  ) %>%
  select(
    District = system,
    `District Name` = system_name,
    Subgroup = subgroup,
    `Valid Tests` = valid_test,
    `Participation Rate` = participation_rate,
    `Average English Score` = english,
    `Average Math Score` = math,
    `Average Reading Score` = reading,
    `Average Science Score` = science,
    `Average Composite Score` = composite,
    `Number Scoring 21 or Higher` = n_21_or_higher,
    `Percent Scoring 21 or Higher` = pct_21_or_higher,
    `Number Scoring Below 19` = n_below_19,
    `Percent Scoring Below 19` = pct_below_19
  )
  

write_csv(school_current, "N:/ORP_accountability/data/2020_ACT/ACT_school_suppressed.csv", na = "")
write_csv(state_act, "N:/ORP_accountability/data/2020_ACT/ACT_state_suppressed.csv", na = '')
write_csv(district_current, "N:/ORP_accountability/data/2020_ACT/ACT_district_suppressed.csv", na = "")







