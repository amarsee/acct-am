library(tidyverse)
library(janitor)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

school_assessment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_assessment_file.csv") %>% 
  filter(grade %in% '3':'12' | grade == 'Missing Grade', year == 2019)

school_release <- school_assessment %>% 
  mutate(
    test = case_when(
      grade %in% '3':'8' & subject %in% c('ELA', english_eoc) & test %in% c('EOC') ~ 'TNReady',
      grade %in% '3':'8' & subject %in% c('Math', math_eoc) & test %in% c('EOC') ~ 'TNReady',
      grade %in% '3':'8' & subject %in% c('US History') & test %in% c('EOC') ~ 'TNReady',
      TRUE ~ test
    ),
    subject = case_when(
      grade %in% '3':'5' & subject == 'ELA' ~ '3-5 ELA',
      grade %in% '6':'8' & subject %in% c('ELA', english_eoc) ~ '6-8 ELA',
      grade %in% '3':'5' & subject %in% c('Math', math_eoc) ~ '3-5 Math',
      grade %in% '6':'8' & subject %in% c('Math', math_eoc) ~ '6-8 Math',
      grade %in% '6':'8' & subject %in% c('US History', 'Social Studies') ~ '6-8 Social Studies',
      (grade %in% '9':'12' | grade == 'Missing Grade') & subject %in% c(math_eoc) ~ 'HS Math',
      (grade %in% '9':'12' | grade == 'Missing Grade') & subject %in% c(english_eoc) ~ 'HS English',
      (grade %in% '9':'12' | grade == 'Missing Grade') & subject %in% c('Math') ~ 'Math',
      (grade %in% '9':'12' | grade == 'Missing Grade') & subject %in% c('ELA') ~ 'ELA',
      (grade %in% '9':'12' | grade == 'Missing Grade') & subject == 'US History' ~ 'US History'
    )

  ) %>% 
  bind_rows(school_assessment %>% filter(grade %in% '3':'8') %>% 
              mutate(
                test = case_when(
                  grade %in% '3':'8' & subject %in% c('ELA', english_eoc) & test %in% c('EOC') ~ 'TNReady',
                  grade %in% '3':'8' & subject %in% c('Math', math_eoc) & test %in% c('EOC') ~ 'TNReady',
                  grade %in% '3':'8' & subject %in% c('US History') & test %in% c('EOC') ~ 'TNReady',
                  TRUE ~ test
                ),
                subject = case_when(
                  grade %in% '3':'8' & subject %in% c('ELA', english_eoc) ~ '3-8 ELA',
                  grade %in% '3':'8' & subject %in% c('Math', math_eoc) ~ '3-8 Math',
                  grade %in% '3':'8' & subject %in% c('US History', 'Social Studies') ~ '3-8 Social Studies'
                )
              )) %>% 
  # bind_rows(school_assessment %>% filter(subject %in% c('Math', 'ELA'))) %>% 
  filter(subgroup != "English Learner Transitional 1-4", !is.na(subject)) %>% 
  group_by(year, system, system_name, school, school_name, test, subject, subgroup) %>% 
  summarise_at(
    .vars = vars(valid_tests:n_mastered),
    .funs = ~ sum(.)
  ) %>% 
  mutate(
    pct_below = round(n_below / valid_tests * 100 + 1e-10, 1),
    pct_approaching = round(n_approaching / valid_tests * 100 + 1e-10, 1),
    pct_on_track = round(n_on_track / valid_tests * 100 + 1e-10, 1),
    pct_mastered = round(n_mastered / valid_tests * 100 + 1e-10, 1),
    pct_on_mastered = round((n_on_track + n_mastered) / valid_tests * 100 + 1e-10, 1)
  ) %>% 
  mutate_at(
    .vars = vars(pct_below:pct_on_mastered),
    .funs = ~ if_else(is.na(.), NA_real_, .)
  ) %>% 
  arrange(subject, subgroup, system, school, test)

write_csv(school_release, "N:/ORP_accountability/data/2019_final_accountability_files/school_release_file_AM.csv", na = '')  

















