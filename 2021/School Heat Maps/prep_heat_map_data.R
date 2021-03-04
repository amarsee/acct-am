library(tidyverse)
library(janitor)
library(readxl)
library(readstata13)
library(broman)
library(knitr)
library(kableExtra)
library(pander)

# school_designations <- read_rds('N:/ORP_accountability/projects/Andrew/Pre-Coding-2019/school_rds.rds')
school_designations <- read_csv("N:/ORP_accountability/projects/Andrew/Accountability/2019/school_heat_map_data/school_designations_Aug15.csv")
total_acct <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv")

school_indicator_metrics <- total_acct %>% 
  filter(subgroup %in% c('All Students', 'Black/Hispanic/Native American', 'Economically Disadvantaged',
                         'English Learners with Transitional 1-4', 'Students with Disabilities', 'Super Subgroup')) %>% 
  mutate(
    pathway = case_when(
      score_abs >= score_target ~ 'Abs',
      score_target > score_abs ~ 'AMO',
      !is.na(score) & is.na(score_target) & is.na(score_abs) ~ 'Abs',
      TRUE ~ NA_character_
    )
  ) %>% 
  select(system, school, indicator, subgroup, metric, pathway) %>% 
  mutate(
    subgroup = case_when(
      subgroup == "Black/Hispanic/Native American" ~ "BHN",
      subgroup == "Economically Disadvantaged" ~ "ED",
      subgroup == "Students with Disabilities" ~ "SWD",
      subgroup == "English Learners with Transitional 1-4" ~ "EL",
      TRUE ~ subgroup
    ),
    indicator = case_when(
      indicator == 'ELPA Growth Standard' ~ 'ELPA',
      indicator == 'Ready Graduates' ~ 'Ready Graduate',
      TRUE ~ indicator
    )
  ) %>% 
  pivot_wider(names_from = 'subgroup', values_from = c('metric', 'pathway')) %>% 
  clean_names() %>% 
  rename(Indicator = indicator)

indicator_score <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades.csv") %>% 
  select(system, school, score_achievement:score_elpa) %>% 
  gather(indicator, indicator_avg, score_achievement:score_elpa) %>% 
  mutate(
    indicator = case_when(
      indicator == 'score_achievement' ~ 'Achievement',
      indicator == 'score_growth' ~ 'Growth',
      indicator == 'score_absenteeism' ~ 'Chronic Absenteeism',
      indicator == 'score_elpa' ~ 'ELPA',
      indicator == 'score_grad' ~ 'Graduation Rate',
      indicator == 'score_ready_grad' ~ 'Ready Graduate',
      indicator == 'final_score' ~ 'Final Score (Weighted Average)',
      TRUE ~ indicator
    ),
    indicator_avg = round(indicator_avg, 1)
  ) %>% 
  arrange(system, school, indicator)

heat_map_overall_table <- school_designations %>% 
  select(system:achievement_score, growth_score, chronic_absenteeism_score, elpa_growth_standard_score, graduation_rate_score,
         ready_graduates_score, final_score) %>% 
  mutate(
    subgroup = case_when(
      subgroup == "Black/Hispanic/Native American" ~ "BHN",
      subgroup == "Economically Disadvantaged" ~ "ED",
      subgroup == "Students with Disabilities" ~ "SWD",
      subgroup == "English Learners with Transitional 1-4" ~ "EL",
      TRUE ~ subgroup
    )
  ) %>% 
  gather(key = 'indicator', value = 'score', achievement_score:final_score) %>% 
  spread(subgroup, score) %>% 
  mutate(
    indicator = case_when(
      indicator == 'achievement_score' ~ 'Achievement',
      indicator == 'growth_score' ~ 'Growth',
      indicator == 'chronic_absenteeism_score' ~ 'Chronic Absenteeism',
      indicator == 'elpa_growth_standard_score' ~ 'ELPA',
      indicator == 'graduation_rate_score' ~ 'Graduation Rate',
      indicator == 'ready_graduates_score' ~ 'Ready Graduate',
      indicator == 'final_score' ~ 'Weighted Subgroup Score',
      TRUE ~ indicator
    )
  ) %>% 
  # Use the indicator score because it incorporates the 60-40 Split
  left_join(indicator_score, by = c('system', 'school', 'indicator')) %>% 
  rename(`All Subgroups` = indicator_avg) %>% 
  # rowwise() %>% 
  # mutate(
  #   `All Subgroups` = case_when(
  #     is.na(BHN) & is.na(EL) & is.na(ED) & is.na(SWD) ~ round(mean(c(`All Students`, `Super Subgroup`), na.rm = TRUE), 1),
  #     TRUE ~ round(mean(c(`All Students`, BHN, ED, EL, SWD), na.rm = TRUE), 1)
  #   ) 
  # ) %>% 
  # ungroup() %>% 
  replace_na(list('All Subgroups' = NA_real_)) %>% 
  rename(Indicator = indicator) %>% 
  arrange(system, school) %>% 
  mutate(
    `All Subgroups` = as.character(`All Subgroups`),
    `All Subgroups` = if_else(Indicator == 'Weighted Subgroup Score' | is.na(`All Subgroups`), ' ', `All Subgroups`)
  ) %>% 
  rename(`Indicator Score (60-40)` = `All Subgroups`) %>% 
  left_join(school_indicator_metrics, by = c('system', 'school', 'Indicator'))

# write_csv(heat_map_overall_table, "N:/ORP_accountability/projects/Andrew/Accountability/2020/School Heat Maps/heat_map_data/school_report_scores.csv")

# ========================== Weightings ================================================
count_df <- total_acct %>% 
  select(system, school, indicator, subgroup, score_abs, score_target, n_count) %>% 
  rename('Indicator' = indicator) %>% 
  mutate(Indicator = case_when(
    Indicator == 'Ready Graduates' ~ 'Ready Graduate',
    Indicator == 'ELPA Growth Standard' ~ 'ELPA',
    TRUE ~ Indicator
    ),
    subgroup = case_when(
      subgroup == "Black/Hispanic/Native American" ~ "BHN",
      subgroup == "Economically Disadvantaged" ~ "ED",
      subgroup == "Students with Disabilities" ~ "SWD",
      subgroup == "English Learners with Transitional 1-4" ~ "EL",
      TRUE ~ subgroup
    )
  )

heat_map_weightings <- school_designations %>% 
  select(system:subgroup, achievement_score, achievement_base_weighting, growth_score, growth_base_weighting, chronic_absenteeism_score,
         chronic_absenteeism_base_weighting, elpa_growth_standard_score, elpa_growth_standard_base_weighting, graduation_rate_score,
         graduation_rate_base_weighting, ready_graduates_score, ready_graduates_base_weighting, f_elpa_weight:final_score) %>% 
  mutate(
    subgroup = case_when(
      subgroup == "Black/Hispanic/Native American" ~ "BHN",
      subgroup == "Economically Disadvantaged" ~ "ED",
      subgroup == "Students with Disabilities" ~ "SWD",
      subgroup == "English Learners with Transitional 1-4" ~ "EL",
      TRUE ~ subgroup
    )
  ) %>% 
  select(-total_weight) %>% 
  gather(key = 'Indicator', value = 'metric', achievement_score:final_score) %>% 
  mutate(category = case_when(
    Indicator %in% c('achievement_score', 'achievement_base_weighting', 'f_achievement_weight') ~ 'Achievement',
    Indicator %in% c('growth_score', 'growth_base_weighting', 'f_growth_weight') ~ 'Growth',
    Indicator %in% c('chronic_absenteeism_score', 'chronic_absenteeism_base_weighting', 'f_chronic_absent_weight') ~ 'Chronic Absenteeism',
    Indicator %in% c('elpa_growth_standard_score', 'elpa_growth_standard_base_weighting', 'f_elpa_weight') ~ 'ELPA',
    Indicator %in% c('graduation_rate_score', 'graduation_rate_base_weighting', 'f_grad_weight') ~ 'Graduation Rate',
    Indicator %in% c('ready_graduates_score', 'ready_graduates_base_weighting', 'f_ready_grad_weight') ~ 'Ready Graduate',
    TRUE ~ 'Final Score'
  ),
  Indicator = case_when(
    Indicator %in% c('achievement_score', 'growth_score', 'chronic_absenteeism_score', 'elpa_growth_standard_score',
                     'graduation_rate_score', 'ready_graduates_score') ~ 'Score',
    Indicator %in% c('achievement_base_weighting', 'growth_base_weighting', 'chronic_absenteeism_base_weighting', 
                     'elpa_growth_standard_base_weighting', 'graduation_rate_base_weighting', 'ready_graduates_base_weighting') ~ 'Base Weight',
    Indicator %in% c('f_achievement_weight', 'f_growth_weight', 'f_chronic_absent_weight', 
                     'f_elpa_weight', 'f_grad_weight', 'f_ready_grad_weight') ~ 'Adjusted Weight',
    TRUE ~ 'Final Score'
    )
  ) %>% 
  spread(Indicator, metric) %>% 
  rename(Indicator = category) %>% 
  select(system:Indicator, Score, `Base Weight`, `Adjusted Weight`, `Final Score`) %>% 
  left_join(count_df, by = c('system', 'school', 'subgroup', 'Indicator')) %>% 
  rename('N Count' = n_count, 'Absolute' = score_abs, 'Target' = score_target) %>% 
  select(system:Indicator, `Absolute`, `Target`, Score:`Final Score`, `N Count`) %>% 
  arrange(system, school, subgroup)
  
# write_csv(heat_map_weightings, "N:/ORP_accountability/projects/Andrew/Accountability/2019/school_heat_map_data/heat_map_weightings_Aug15.csv")


# ======================== Categorizing Scores ===================================
# school_heat_map <- school_designations %>% 
#   mutate(
#     abs_grade = case_when(
#       score_abs > 3 ~ 'A',
#       score_abs > 2 ~ 'B',
#       score_abs > 1 ~ 'C',
#       score_abs > 0 ~ 'D',
#       score_abs == 0 ~ 'F',
#       TRUE ~ NA_character_
#     ),
#     amo_grade = case_when(
#       score_target > 3 ~ 'A',
#       score_target > 2 ~ 'B',
#       score_target > 1 ~ 'C',
#       score_target > 0 ~ 'D',
#       score_target == 0 ~ 'F',
#       TRUE ~ NA_character_
#     ),
#     best_grade = case_when(
#       score > 3 ~ 'A',
#       score > 2 ~ 'B',
#       score > 1 ~ 'C',
#       score > 0 ~ 'D',
#       score == 0 ~ 'F',
#       TRUE ~ NA_character_
#     )
#   ) %>% 
#   select(-(n_count:AMO_target_double))
# 
subs_to_remove <- c("American Indian or Alaska Native", "Asian", "Black or African American",
                    "English Learners", "Hispanic", "White", "Native Hawaiian or Other Pacific Islander", "English Learner Transitional 1-4")

# system_scores <- school_heat_map %>%
#   select(-designation_ineligible) %>%
#   mutate(
#     subgroup = case_when(
#       subgroup == "Black/Hispanic/Native American" ~ "BHN",
#       subgroup == "Economically Disadvantaged" ~ "ED",
#       subgroup == "Students with Disabilities" ~ "SWD",
#       subgroup == "English Learners with Transitional 1-4" ~ "ELL",
#       TRUE ~ subgroup
#     )
#   ) %>% 
#   filter(!subgroup %in% subs_to_remove) %>% 
#   group_by(system, system_name, school, school_name, pool, subgroup) %>% 
#   mutate(subgroup_result = round(mean(score, na.rm=TRUE), 2)) %>% 
#   ungroup() %>% 
#   mutate(subgroup_result = if_else(is.na(subgroup_result), NA_real_, subgroup_result)) %>% 
#   group_by(system, system_name, school, school_name, pool, indicator) %>% 
#   mutate(indicator_score = round(mean(score, na.rm=TRUE), 2)) %>% 
#   ungroup() %>% 
#   mutate(indicator_score = if_else(is.na(indicator_score), NA_real_, indicator_score)) %>% 
#   # filter(system == 10, school == 2) %>% 
#   gather(score_abs:subgroup_result, key = 'metric', value = "value") %>%
#   group_by(system, system_name, school, school_name, pool, indicator) %>% 
#   mutate(temp = ifelse(subgroup == 'All Students', value, NA)) %>% 
#   ungroup() %>% 
#   arrange(system, school, metric, indicator, subgroup) %>% 
#   fill(temp) %>% 
#   mutate(indicator_score = if_else(metric == 'subgroup_result', temp, as.character(indicator_score))) %>% 
#   select(-temp) %>% 
#   spread(key = subgroup, value = value) %>% 
#   select(system:ELL, SWD, `Super Subgroup`) %>% 
#   filter(!metric %in% c('score', 'score_abs', 'score_target')) %>% 
#   select(system:indicator, metric:`Super Subgroup`, indicator_score) %>% 
#   filter(!(indicator == 'Growth' & metric %in% c('abs_grade', 'amo_grade', 'subgroup_result')), 
#          !(indicator == 'ELPA Growth Standard' & metric %in% c('abs_grade', 'amo_grade', 'subgroup_result')),
#          !(indicator == 'Chronic Absenteeism' & metric == 'subgroup_result')) %>% 
#   mutate(
#     indicator = case_when(
#       metric == 'subgroup_result' ~ 'Subgroup Result (All Indicators)',
#       TRUE ~ indicator
#     )
#   ) %>%
#   unique() %>% 
#   mutate(
#     metric = case_when(
#       metric == 'abs_grade' ~ "Abs",
#       metric == 'amo_grade' ~ "AMO",
#       TRUE ~ "Best"
#     )
#   ) %>% 
#   filter(metric == 'Best') %>% 
#   select(-metric) %>% 
#   rename(Indicator = indicator, "All Subgroups" = indicator_score) %>% 
#   replace_na(list(`All Students`='', BHN='', ED='', ELL = '', SWD = '', `Super Subgroup` = '')) %>% 
#   arrange(system, school, Indicator)

# saveRDS(system_scores, file = "data/school_report_scores.rds")

# =============================== TCAP Participation Rate ==============================

tcap_rate <- read_rds('N:/ORP_accountability/projects/Andrew/Pre-Coding-2019/data/tcap_participation_2018.rds') %>% 
  filter(! subgroup %in% subs_to_remove, !str_detect(subgroup, 'Non')) %>% 
  mutate(
    subgroup = case_when(
      subgroup == "Black/Hispanic/Native American" ~ "BHN",
      subgroup == "Economically Disadvantaged" ~ "ED",
      subgroup == "Students with Disabilities" ~ "SWD",
      subgroup == "English Learners with Transitional 1-4" ~ "EL",
      TRUE ~ subgroup
    ),
    participation_rate = if_else(n_tested >= 30, participation_rate, NA_real_)
  ) %>% 
  select(-n_enrolled, -n_tested) %>% 
  spread(subgroup, participation_rate)
# saveRDS(tcap_rate, file = "data/school_tcap_rates.rds")

# =============================== ACT/SAT Participation ===========================================
act_sat_rate <- read_rds('N:/ORP_accountability/projects/Andrew/Pre-Coding-2019/data/act_sat_participation_2018.rds') %>% 
  filter(! subgroup %in% subs_to_remove, !str_detect(subgroup, 'Non')) %>% 
  mutate(
    subgroup = case_when(
      subgroup == "Black/Hispanic/Native American" ~ "BHN",
      subgroup == "Economically Disadvantaged" ~ "ED",
      subgroup == "Students with Disabilities" ~ "SWD",
      subgroup == "English Learners with Transitional 1-4" ~ "EL",
      TRUE ~ subgroup
    ),
    participation_rate = if_else(n_on_time_grads >= 30, participation_rate, NA_real_)
  ) %>% 
  select(-n_on_time_grads, -n_completed_act_or_sat) %>% 
  spread(subgroup, participation_rate)
# saveRDS(act_sat_rate, file = "data/school_act_sat_rates.rds")

# ========================================== Prep for Heat Map ======================================

school_acct_2018 <- read_rds('N:/ORP_accountability/projects/Andrew/Pre-Coding-2019/data/school_report_scores.rds')

heat_map_df_list <- function(base_df, system_no) {
  dist_acct <- base_df %>% 
    filter(system == system_no)
  dist_schools <- unique(dist_acct$school)
  df_list <- list()
  
  for (school_no in dist_schools) {
    out_df <- dist_acct %>%
      filter(school == school_no)
    df_list <- c(df_list, list(out_df))
  }
  return(df_list)
}

dist_10_dfs <- heat_map_df_list(school_acct_2018, 792)

dist_10_pool_school <- school_acct_2018 %>% 
  filter(system == 792) %>% 
  group_by(school, school_name, pool) %>% 
  summarise(temp = max(`Indicator Result (All Subgroups)`)) %>% 
  arrange(desc(pool), school_name)
dist_10_school_list <- dist_10_pool_school$school_name
dist_school_nos <- dist_10_pool_school$school
dist_pools <- dist_10_pool_school$pool



# Function to create multiple tabs
make.tabs <- function(school_list, dist_school_nos, dist_pools){
  res <- NULL
  k8_flag = 1
  hs_flag = 1
  for(i in seq_along(school_list)){
    n_pool <- dist_pools[i]
    if(n_pool == 'K8' & k8_flag == 1) {
      res <- c(res, '## Pool: ', dist_pools[i], '\n')
      k8_flag <- k8_flag + 1
    } else if (n_pool == 'HS' & hs_flag == 1) {
      res <- c(res, '## Pool: ', dist_pools[i], '\n')
      hs_flag <- hs_flag + 1
    }
    res <- c(res, '### ', school_list[i], ' (', dist_school_nos[i], ')', '\n',
             "```{r echo = FALSE}", '\n',
             "oldw <- getOption('warn')", '\n',
'options(warn = -1)', '\n',
  'dist_dfs[[', i,']]', " %>% ", '\n',
  "select(Indicator:`Indicator Result (All Subgroups)`)"," %>% ", '\n',
"mutate_at(", '\n',
  '.vars = c("All Students","Black/Hispanic/Native American","Economically Disadvantaged",', '\n',
            '"English Learners with Transitional 1-4", "Students with Disabilities", "Super Subgroup"),', '\n', 
  ".f = ~ case_when(", '\n',
    ". == 'A' ~ color_tile( 'green4', 'green4')(.),", '\n',
    " . == 'B' ~ color_tile('lightgreen', 'lightgreen')(.),", '\n',
    " . == 'C' ~ color_tile('yellow', 'yellow')(.),", '\n',
    " . == 'D' ~ color_tile('tomato', 'tomato')(.),", '\n',
    " . == 'F' ~ color_tile('red', 'red')(.),", '\n',
    "TRUE ~ ." , '\n',
  ")) %>% ", '\n',
  "kable(format = 'latex', escape = F) %>% ", '\n',
  "kable_styling(c('striped', 'bordered')) %>%", '\n', 
  'add_header_above(c(" " = 2, "Report Card" = 6))', '\n',
'options(warn = oldw)', '\n',
             '```', '\n\n',
'<div style="margin-bottom:100px;">', '\n',
'</div>', '\n',
"```{r echo=FALSE}", '\n',
"kable(grade_tbl) %>% ", '\n',
'  kable_styling(bootstrap_options = "striped", full_width = F, position = "float_right")', '\n',
"```", '\n',
'<div style="margin-bottom:150px;">', '\n',
'</div>', '\n',

'&emsp;&emsp;&emsp;&emsp;&emsp; **Final Score**:  &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;', '\n',
'            **Grade**:  &emsp;&emsp;', '\n',

'<div style="margin-bottom:100px;">', '\n',
'</div>', '\n\n')
  }
  return(res)
}

cat('
---
title: "Heat Maps"
author: "TDOE"
date: "April 5, 2019"
output: 
  pdf_document:
    toc: true
    toc_depth: 2
---

<style type="text/css">

body{ /* Normal  */
font-size: 18px;
}
td {  /* Table  */
font-size: 12px;
}
<!-- h1.title { -->
<!--   font-size: 38px; -->
<!--   color: DarkRed; -->
<!-- } -->
<!-- h1 { /* Header 1 */ -->
<!--   font-size: 28px; -->
<!--   color: DarkBlue; -->
<!-- } -->
<!-- h2 { /* Header 2 */ -->
<!--     font-size: 22px; -->
<!--   color: DarkBlue; -->
<!-- } -->
<!-- h3 { /* Header 3 */ -->
<!--   font-size: 18px; -->
<!--   font-family: "Times New Roman", Times, serif; -->
<!--   color: DarkBlue; -->
<!-- } -->
<!-- code.r{ /* Code block */ -->
<!--     font-size: 12px; -->
<!-- } -->
<!-- pre { /* Code block - determines code spacing between lines */ -->
<!--     font-size: 14px; -->
<!-- } -->
</style>

<div style="margin-bottom:100px;">
</div>
```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(janitor)
library(readxl)
library(readstata13)
library(broman)
library(knitr)
library(kableExtra)
library(pander)
library(formattable)

final_score = c("3.1-4.0", "2.1-3.0", "1.1-2.0", "0-1.0", "Priority School")
grade = c("A", "B", "C", "D", "F")
grade_tbl <- data.frame(`Final Score` = final_score, "Grade" = grade)

school_acct_2018 <- read_rds("N:/ORP_accountability/projects/Andrew/Pre-Coding-2019/data/school_report_scores.rds")
district <- 10
school_10_2 <- school_acct_2018 %>% 
filter(system == district, school == 2) %>% 
select(Indicator:`Indicator Result (All Subgroups)`)

heat_map_df_list <- function(base_df, system_no) {
dist_acct <- base_df %>% 
filter(system == system_no)
dist_schools <- unique(dist_acct$school)
df_list <- list()

for (school_no in dist_schools) {
out_df <- dist_acct %>%
filter(school == school_no)
df_list <- c(df_list, list(out_df))
}
return(df_list)
}

dist_dfs <- heat_map_df_list(school_acct_2018, 792)

dist_10_pool_school <- school_acct_2018 %>% 
  filter(system == 792) %>% 
  group_by(school, school_name, pool) %>% 
  summarise(temp = max(`Indicator Result (All Subgroups)`)) %>% 
  arrange(desc(pool), school_name)
dist_10_school_list <- dist_10_pool_school$school_name
dist_school_nos <- dist_10_pool_school$school
dist_pools <- dist_10_pool_school$pool


# cell_color <- function(x) (cell_spec(x, "html", background = case_when(
#   Metric == "Best" & x == "A" ~ "green",
#   TRUE ~ "blue"
# )))   select(Indicator:`Indicator Result (All Subgroups)`)
```
<!-- ### District 792 {.tabset .tabset-fade .tabset-pills} -->

',
  make.tabs(dist_10_school_list, dist_school_nos, dist_pools),
  sep = "",
  file = "filetoknit.Rmd")


















