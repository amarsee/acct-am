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

state_abs <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/state_chronic_absenteeism_Jul7.csv")
district_abs <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/district_chronic_absenteeism_Jul7.csv")
school_abs <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/school_chronic_absenteeism_Jul7.csv")

write_csv(state_abs %>% suppress_abs(), "N:/ORP_accountability/data/2020_chronic_absenteeism/state_chronic_absenteeism_suppressed.csv", na = '')
write_csv(district_abs %>% suppress_abs(), "N:/ORP_accountability/data/2020_chronic_absenteeism/district_chronic_absenteeism_suppressed.csv", na = '')
write_csv(school_abs %>% suppress_abs(threshold = 5), "N:/ORP_accountability/data/2020_chronic_absenteeism/school_chronic_absenteeism_suppressed.csv", na = '')












