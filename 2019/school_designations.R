# Designation File
# Andrew Marsee
# 7/24/2019

library(tidyverse)
library(janitor)
library(readxl)
library(readstata13)

# Read in final ACCT file
#total_accountability <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv")
total_accountability <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file_AM_Aug16_stitched.csv")

# Historically Underserved groups
historic_underserved_subgroups <- c('All Students', 'Black/Hispanic/Native American', 'Economically Disadvantaged', 
                                    "English Learners with Transitional 1-4", 'Students with Disabilities', 'Super Subgroup')

# ======================= School Designations (Historically Underserved Subgroups) ==================================
school_designations <- total_accountability %>% 
  filter(subgroup %in% historic_underserved_subgroups) %>% # , indicator != 'Growth' designation_ineligible == 0, , !is.na(pool)
  select(system, system_name, school, school_name, pool, indicator, subgroup, score) %>% 
  # spread(indicator, score) %>%
  # clean_names() %>% 
  mutate(
    base_weighting = case_when(
      indicator == 'ELPA Growth Standard' ~ 0.10,
      indicator == 'Achievement' & pool == 'HS' ~ 0.30,
      indicator == 'Achievement' & pool == 'K8' ~ 0.45,
      indicator == 'Growth' & pool == 'HS' ~ 0.25,
      indicator == 'Growth' & pool == 'K8' ~ 0.35,
      indicator == 'Ready Graduates' ~ 0.20,
      indicator == 'Graduation Rate' ~ 0.05,
      indicator == 'Chronic Absenteeism' ~ 0.10,
      TRUE ~ NA_real_
    )
  ) %>% 
  nest(score, base_weighting, .key = 'value_col') %>%
  spread(key = indicator, value = value_col) %>%
  replace_na(
    list(Achievement = list(data.frame(score = NA, base_weighting = NA)),
         `Chronic Absenteeism` = list(data.frame(score = NA, base_weighting = 0.10)),
         `ELPA Growth Standard` = list(data.frame(score = NA, base_weighting = 0.10)),
         `Graduation Rate` = list(data.frame(score = NA, base_weighting = NA)),
         `Growth` = list(data.frame(score = NA, base_weighting = NA)),
         `Ready Graduates` = list(data.frame(score = NA, base_weighting = NA))
    )
  ) %>% 
  unnest(Achievement, `Chronic Absenteeism`, `ELPA Growth Standard`, `Graduation Rate`, `Growth`, `Ready Graduates`, .sep = '_') %>% 
  clean_names() %>% 
  mutate(
    achievement_base_weighting = if_else(pool == 'K8', 0.45, 0.30),
    graduation_rate_base_weighting = if_else(pool == 'K8', NA_real_, 0.05),
    growth_base_weighting = if_else(pool == 'K8', 0.35, 0.25),
    ready_graduates_base_weighting = if_else(pool == 'K8', NA_real_, 0.20),
    f_elpa_weight = if_else(is.na(elpa_growth_standard_score), 0, elpa_growth_standard_base_weighting),
    f_achievement_weight = if_else(f_elpa_weight == 0, achievement_base_weighting + .05, achievement_base_weighting),
    f_growth_weight = if_else(f_elpa_weight == 0, growth_base_weighting + .05, growth_base_weighting), # Takes care of ELPA missing
    f_ready_grad_weight = if_else((pool == 'HS' & is.na(ready_graduates_score)), 0, ready_graduates_base_weighting),
    f_grad_weight = if_else((pool == 'HS' & is.na(graduation_rate_score)), 0, graduation_rate_base_weighting),
    f_chronic_absent_weight = if_else(is.na(chronic_absenteeism_score), 0, chronic_absenteeism_base_weighting),
    f_achievement_weight = if_else(is.na(achievement_score), 0, f_achievement_weight),
    f_growth_weight = if_else(is.na(growth_score), 0, f_growth_weight)
  ) %>% 
  rowwise() %>% 
  mutate(
    total_weight = sum(f_elpa_weight, f_achievement_weight, f_growth_weight, f_ready_grad_weight, 
                       f_grad_weight, f_chronic_absent_weight, na.rm = TRUE)
  ) %>% 
  mutate_at(.vars = c ('f_elpa_weight', 'f_achievement_weight', 'f_growth_weight', 'f_ready_grad_weight', 
                       'f_grad_weight', 'f_chronic_absent_weight'), .funs = ~(round(./total_weight, 2))) %>% 
  mutate(
    final_score = if_else(is.na(elpa_growth_standard_score * f_elpa_weight),0,elpa_growth_standard_score * f_elpa_weight) + 
      if_else(is.na(achievement_score * f_achievement_weight),0,achievement_score * f_achievement_weight) + 
      if_else(is.na(chronic_absenteeism_score * f_chronic_absent_weight),0,chronic_absenteeism_score * f_chronic_absent_weight) +
      if_else(is.na(graduation_rate_score * f_grad_weight),0,graduation_rate_score * f_grad_weight) +
      if_else(is.na(growth_score * f_growth_weight),0,growth_score * f_growth_weight) +
      if_else(is.na(ready_graduates_score * f_ready_grad_weight),0,ready_graduates_score * f_ready_grad_weight),
    final_score = case_when(
      is.na(elpa_growth_standard_score) & is.na(achievement_score) & is.na(chronic_absenteeism_score) & 
        is.na(graduation_rate_score) & is.na(growth_score) & is.na(ready_graduates_score) ~ NA_real_,
      TRUE ~ round(final_score + 1e-5, 1)
    )
  ) %>% 
  replace_na(list(f_elpa_weight = NA_real_, f_achievement_weight = NA_real_, f_growth_weight = NA_real_,
                  f_ready_grad_weight = NA_real_, f_grad_weight = NA_real_, f_chronic_absent_weight = NA_real_)) %>% 
  ungroup()

# write_csv(school_designations, "N:/ORP_accountability/projects/Andrew/Accountability/2019/school_heat_map_data/school_designations_Aug15.csv")

school_final_scores <- school_designations %>% 
  filter(!is.na(pool)) %>% 
  select(system:subgroup, achievement_score, achievement_base_weighting, growth_score, growth_base_weighting, chronic_absenteeism_score,
         chronic_absenteeism_base_weighting, elpa_growth_standard_score, elpa_growth_standard_base_weighting, graduation_rate_score,
         graduation_rate_base_weighting, ready_graduates_score, ready_graduates_base_weighting, f_elpa_weight:final_score) %>% 
  mutate(
    subgroup = case_when(
      subgroup %in% c('Black/Hispanic/Native American', 'Economically Disadvantaged',
                      'English Learners with Transitional 1-4', 'Students with Disabilities') ~ 'Historically Underserved',
      TRUE ~ subgroup
    )
  ) %>% 
  group_by(system, system_name, school, school_name, pool, subgroup) %>% 
  # Even weight average across indicators
  summarise_at(.vars = c('achievement_score', 'growth_score', 'chronic_absenteeism_score', 'elpa_growth_standard_score', 'graduation_rate_score',
                         'ready_graduates_score', 'achievement_base_weighting', 'growth_base_weighting', 'chronic_absenteeism_base_weighting',
                         'elpa_growth_standard_base_weighting', 'graduation_rate_base_weighting', 'ready_graduates_base_weighting'),
               .funs = ~ mean(., na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter((!is.na(achievement_score) | !is.na(growth_score) | !is.na(chronic_absenteeism_score) | 
            !is.na(elpa_growth_standard_score) | !is.na(graduation_rate_score) | !is.na(ready_graduates_score))) %>% 
  mutate(
    at_least_one_score = case_when(
      (!is.na(achievement_score) | !is.na(growth_score) | !is.na(chronic_absenteeism_score) | 
         !is.na(elpa_growth_standard_score) | !is.na(graduation_rate_score) | !is.na(ready_graduates_score)) & subgroup != 'All Students' ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  group_by(system, school) %>% 
  mutate(
    sub_super_subgroup = sum(at_least_one_score)
  ) %>% 
  ungroup() %>% 
  mutate(
    subgroup = if_else(sub_super_subgroup < 2 & subgroup != 'All Students' & 
                         (!is.na(achievement_score) | !is.na(growth_score) | !is.na(chronic_absenteeism_score) | 
                            !is.na(elpa_growth_standard_score) | !is.na(graduation_rate_score) | !is.na(ready_graduates_score)), 'Historically Underserved', subgroup)
  ) %>% 
  filter(subgroup != 'Super Subgroup') %>% 
  select(-sub_super_subgroup, -at_least_one_score) %>% 
  gather(metric, value, achievement_score:ready_graduates_score) %>% 
  mutate(
    subgroup_metric = paste0(subgroup, '_', metric)
  ) %>% 
  select(-subgroup, -metric) %>% 
  spread(subgroup_metric, value) %>% 
  clean_names() %>% 
  mutate(
    score_achievement = case_when(
      !is.na(all_students_achievement_score) & !is.na(historically_underserved_achievement_score) ~ 0.6*all_students_achievement_score + 0.4*historically_underserved_achievement_score,
      !is.na(all_students_achievement_score) ~ all_students_achievement_score,
      TRUE ~ NA_real_
    ),
    score_growth = case_when(
      !is.na(all_students_growth_score) & !is.na(historically_underserved_growth_score) ~ 0.6*all_students_growth_score + 0.4*historically_underserved_growth_score,
      !is.na(all_students_growth_score) ~ all_students_growth_score,
      TRUE ~ NA_real_
    ),
    score_absenteeism = case_when(
      !is.na(all_students_chronic_absenteeism_score) & !is.na(historically_underserved_chronic_absenteeism_score) ~ 0.6*all_students_chronic_absenteeism_score + 0.4*historically_underserved_chronic_absenteeism_score,
      !is.na(all_students_chronic_absenteeism_score) ~ all_students_chronic_absenteeism_score,
      TRUE ~ NA_real_
    ),
    score_grad = case_when(
      !is.na(all_students_graduation_rate_score) & !is.na(historically_underserved_graduation_rate_score) ~ 0.6*all_students_graduation_rate_score + 0.4*historically_underserved_graduation_rate_score,
      !is.na(all_students_graduation_rate_score) ~ all_students_graduation_rate_score,
      TRUE ~ NA_real_
    ),
    score_ready_grad = case_when(
      !is.na(all_students_ready_graduates_score) & !is.na(historically_underserved_ready_graduates_score) ~ 0.6*all_students_ready_graduates_score + 0.4*historically_underserved_ready_graduates_score,
      !is.na(all_students_ready_graduates_score) ~ all_students_ready_graduates_score,
      TRUE ~ NA_real_
    ),
    score_elpa = case_when(
      !is.na(all_students_elpa_growth_standard_score) & !is.na(historically_underserved_elpa_growth_standard_score) ~ 0.6*all_students_elpa_growth_standard_score + 0.4*historically_underserved_elpa_growth_standard_score,
      !is.na(all_students_elpa_growth_standard_score) ~ all_students_elpa_growth_standard_score,
      TRUE ~ NA_real_
    )
  ) %>% 
  select(-(all_students_achievement_score:historically_underserved_ready_graduates_score)) %>% 
  # Applying same redistribution of weights after a mean of scores
  mutate(
    f_elpa_weight = if_else(is.na(score_elpa), 0, elpa_growth_standard_base_weighting),
    f_achievement_weight = if_else(f_elpa_weight == 0, achievement_base_weighting + .05, achievement_base_weighting),
    f_growth_weight = if_else(f_elpa_weight == 0, growth_base_weighting + .05, growth_base_weighting), # Takes care of ELPA missing
    f_ready_grad_weight = if_else((pool == 'HS' & is.na(score_ready_grad)), 0, ready_graduates_base_weighting),
    f_grad_weight = if_else((pool == 'HS' & is.na(score_grad)), 0, graduation_rate_base_weighting),
    f_chronic_absent_weight = if_else(is.na(score_absenteeism), 0, chronic_absenteeism_base_weighting),
    f_achievement_weight = if_else(is.na(score_achievement), 0, f_achievement_weight),
    f_growth_weight = if_else(is.na(score_growth), 0, f_growth_weight)
  ) %>% 
  rowwise() %>% 
  mutate(
    total_weight = sum(f_elpa_weight, f_achievement_weight, f_growth_weight, f_ready_grad_weight, 
                       f_grad_weight, f_chronic_absent_weight, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  # Divide by total weight to redistribute weightings
  mutate_at(vars(f_elpa_weight, f_achievement_weight, f_growth_weight, f_ready_grad_weight,
                 f_grad_weight, f_chronic_absent_weight),
            .funs = ~ ./total_weight) %>%
  ungroup() %>%
  # Add everything together not using rowwise
  mutate(
    final_score = if_else(is.na(score_elpa * f_elpa_weight),0,score_elpa * f_elpa_weight) + 
      if_else(is.na(score_achievement * f_achievement_weight),0,score_achievement * f_achievement_weight) + 
      if_else(is.na(score_absenteeism * f_chronic_absent_weight),0,score_absenteeism * f_chronic_absent_weight) +
      if_else(is.na(score_grad * f_grad_weight),0,score_grad * f_grad_weight) +
      if_else(is.na(score_growth * f_growth_weight),0,score_growth * f_growth_weight) +
      if_else(is.na(score_ready_grad * f_ready_grad_weight),0,score_ready_grad * f_ready_grad_weight),
    # If a school is missing everything, make it NA instead of 0. Otherwise round to one decimal place
    final_score = case_when(
      is.na(score_elpa) & is.na(score_achievement) & is.na(score_absenteeism) & 
        is.na(score_grad) & is.na(score_growth) & is.na(score_ready_grad) ~ NA_real_,
      TRUE ~ round(final_score + 1e-10, 1)
    )
  )

school_final_weighted <- school_final_scores %>% 
  select(system:pool, score_achievement:score_elpa, final_score)

# write_csv(school_final_weighted, "N:/ORP_accountability/projects/Andrew/Accountability/2019/school_heat_map_data/school_final_grades_Aug15.csv")

# ============================== Designations with all Subgroups =====================================

school_designations_all_subgroups <- total_accountability %>% 
  filter(!is.na(pool)) %>% # designation_ineligible == 0,  | (is.na(pool) & !is.na(designation_ineligible))
  select(system, system_name, school, school_name, pool, designation_ineligible, indicator, subgroup, score) %>% 
  mutate(
    base_weighting = case_when(
      indicator == 'ELPA Growth Standard' ~ 0.10,
      indicator == 'Achievement' & pool == 'HS' ~ 0.30,
      indicator == 'Achievement' & pool == 'K8' ~ 0.45,
      indicator == 'Growth' & pool == 'HS' ~ 0.25,
      indicator == 'Growth' & pool == 'K8' ~ 0.35,
      indicator == 'Ready Graduates' ~ 0.20,
      indicator == 'Graduation Rate' ~ 0.05,
      indicator == 'Chronic Absenteeism' ~ 0.10,
      TRUE ~ NA_real_
    )
  ) %>% 
  nest(score, base_weighting, .key = 'value_col') %>%
  spread(key = indicator, value = value_col) %>%
  replace_na(
    list(Achievement = list(data.frame(score = NA, base_weighting = NA)),
         `Chronic Absenteeism` = list(data.frame(score = NA, base_weighting = 0.10)),
         `ELPA Growth Standard` = list(data.frame(score = NA, base_weighting = 0.10)),
         `Graduation Rate` = list(data.frame(score = NA, base_weighting = NA)),
         `Growth` = list(data.frame(score = NA, base_weighting = NA)),
         `Ready Graduates` = list(data.frame(score = NA, base_weighting = NA))
    )
  ) %>% 
  unnest(Achievement, `Chronic Absenteeism`, `ELPA Growth Standard`, `Graduation Rate`, `Growth`, `Ready Graduates`, .sep = '_') %>% 
  clean_names() %>% 
  mutate(
    achievement_base_weighting = if_else(pool == 'K8', 0.45, 0.30),
    graduation_rate_base_weighting = if_else(pool == 'K8', NA_real_, 0.05),
    growth_base_weighting = if_else(pool == 'K8', 0.35, 0.25),
    ready_graduates_base_weighting = if_else(pool == 'K8', NA_real_, 0.20),
    f_elpa_weight = if_else(is.na(elpa_growth_standard_score), 0, elpa_growth_standard_base_weighting),
    f_achievement_weight = if_else(f_elpa_weight == 0, achievement_base_weighting + .05, achievement_base_weighting),
    f_growth_weight = if_else(f_elpa_weight == 0, growth_base_weighting + .05, growth_base_weighting), # Takes care of ELPA missing
    f_ready_grad_weight = if_else((pool == 'HS' & is.na(ready_graduates_score)), 0, ready_graduates_base_weighting),
    f_grad_weight = if_else((pool == 'HS' & is.na(graduation_rate_score)), 0, graduation_rate_base_weighting),
    f_chronic_absent_weight = if_else(is.na(chronic_absenteeism_score), 0, chronic_absenteeism_base_weighting),
    f_achievement_weight = if_else(is.na(achievement_score), 0, f_achievement_weight),
    f_growth_weight = if_else(is.na(growth_score), 0, f_growth_weight)
  ) %>% 
  rowwise() %>% 
  mutate(
    total_weight = sum(f_elpa_weight, f_achievement_weight, f_growth_weight, f_ready_grad_weight, 
                       f_grad_weight, f_chronic_absent_weight, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate_at(.vars = c('f_elpa_weight', 'f_achievement_weight', 'f_growth_weight', 'f_ready_grad_weight', 
                      'f_grad_weight', 'f_chronic_absent_weight'), 
            .funs = ~ (round(./total_weight + 1e-10, 4))) %>% 
  mutate(
    final_score = if_else(is.na(elpa_growth_standard_score * f_elpa_weight),0,elpa_growth_standard_score * f_elpa_weight) + 
      if_else(is.na(achievement_score * f_achievement_weight),0,achievement_score * f_achievement_weight) + 
      if_else(is.na(chronic_absenteeism_score * f_chronic_absent_weight),0,chronic_absenteeism_score * f_chronic_absent_weight) +
      if_else(is.na(graduation_rate_score * f_grad_weight),0,graduation_rate_score * f_grad_weight) +
      if_else(is.na(growth_score * f_growth_weight),0,growth_score * f_growth_weight) +
      if_else(is.na(ready_graduates_score * f_ready_grad_weight),0,ready_graduates_score * f_ready_grad_weight),
    final_score = case_when(
      is.na(elpa_growth_standard_score) & is.na(achievement_score) & is.na(chronic_absenteeism_score) & 
        is.na(graduation_rate_score) & is.na(growth_score) & is.na(ready_graduates_score) ~ NA_real_,
      TRUE ~ round(final_score + 1e-10, 1)
    )
  ) %>% 
  mutate_at(.vars = c('f_elpa_weight', 'f_achievement_weight', 'f_growth_weight', 'f_ready_grad_weight', 
                      'f_grad_weight', 'f_chronic_absent_weight'), 
            .funs = ~ (round(./total_weight, 2))) %>% 
  replace_na(list(f_elpa_weight = NA_real_, f_achievement_weight = NA_real_, f_growth_weight = NA_real_,
                  f_ready_grad_weight = NA_real_, f_grad_weight = NA_real_, f_chronic_absent_weight = NA_real_)) %>% 
  ungroup() %>% 
  clean_names() %>% 
  mutate(
    final_grade = case_when(
      final_score >= 3.1 ~ 'A',
      final_score >= 2.1 ~ 'B',
      final_score >= 1.1 ~ 'C',
      final_score >= 0 ~ 'D',
      TRUE ~ NA_character_
    )
  )

school_metric_write_out <- school_designations_all_subgroups %>% 
  transmute(system, school, pool, designation_ineligible, subgroup, score_achievement = achievement_score, score_growth = growth_score,
            score_grad = graduation_rate_score, score_ready_grad = ready_graduates_score, 
            score_absenteeism = chronic_absenteeism_score, score_elpa = elpa_growth_standard_score,
            weight_achievement = f_achievement_weight, weight_growth = f_growth_weight,
            weight_grad = f_grad_weight, weight_ready_grad = f_ready_grad_weight, weight_absenteeism = f_chronic_absent_weight,
            weight_elpa = f_elpa_weight, total_weight, subgroup_average = final_score)

# write_csv(school_metric_write_out, "N:/ORP_accountability/projects/Andrew/Accountability/2019/appeal data/school_grading_metrics_maury_hs_bhn.csv", na='')
#write_csv(school_metric_write_out, "N:/ORP_accountability/projects/2019_school_accountability/school_grading_metrics_AM_Aug14.csv", na ='')

# ====================================== Identifying TSI Schools ====================================================
# Read in Priority because they are not eligible for TSI
priority_current_tsi <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/priority_exit.csv") %>% 
  filter(priority_csi == 1, priority_exit == 0 | is.na(priority_exit)) %>% 
  select(system, school, priority_csi)

tsi_df <- school_designations_all_subgroups %>% 
  left_join(priority_current_tsi, by = c('system', 'school')) %>% 
  mutate(
    tsi_eligible = case_when(
      priority_csi == 1 ~ 0,
      pool== 'HS' & !is.na(achievement_score) & !is.na(chronic_absenteeism_score) & !is.na(graduation_rate_score) & !is.na(growth_score) & 
        !is.na(ready_graduates_score) & !subgroup %in% c('All Students', 'Super Subgroup')  ~ 1,
      pool== 'K8' & !is.na(achievement_score) & !is.na(chronic_absenteeism_score) & !is.na(growth_score) & 
        !subgroup %in% c('All Students', 'Super Subgroup')  ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  mutate(
    final_score = if_else(tsi_eligible == 0, NA_real_, final_score)
  ) %>% 
  select(system:subgroup, final_score, tsi_eligible) %>% 
  group_by(system, system_name, school, school_name, pool) %>% 
  mutate(tsi_eligible = max(tsi_eligible)) %>% 
  ungroup() %>% 
  spread(subgroup, final_score) %>% 
  clean_names() %>% 
  rename(Native = american_indian_or_alaska_native, BHN = black_hispanic_native_american, ED = economically_disadvantaged,
         SWD = students_with_disabilities, EL = english_learners_with_transitional_1_4, Asian = asian, Black = black_or_african_american,
         Hispanic = hispanic, HPI = native_hawaiian_or_other_pacific_islander, White = white) %>% 
  mutate_at(
    vars(all_students:White),
    .funs = list(
      rank = ~ if_else(!is.na(.) & designation_ineligible == 0, rank(., ties.method = "min"), NA_integer_),
      denom = ~ sum(!is.na(.))
    )
  ) %>% 
  mutate_at(
    vars(Native_rank:White_denom),
    .funs = ~ if_else(designation_ineligible == 1, NA_real_, as.numeric(.))
  ) %>% 
  mutate(
    all_students_percentile = round(100 * all_students_rank/all_students_denom + 1e-10, 1),
    Native_percentile = round(100 * Native_rank/Native_denom + 1e-10, 1),
    Asian_percentile = round(100 * Asian_rank/Asian_denom + 1e-10, 1),
    Black_percentile = round(100 * Black_rank/Black_denom + 1e-10, 1),
    BHN_percentile = round(100 * BHN_rank/BHN_denom + 1e-10, 1),
    ED_percentile = round(100 * ED_rank/ED_denom + 1e-10, 1),
    EL_percentile = round(100 *  EL_rank/ EL_denom + 1e-10, 1),
    Hispanic_percentile = round(100 * Hispanic_rank/Hispanic_denom + 1e-10, 1),
    HPI_percentile = round(100 * HPI_rank/HPI_denom + 1e-10, 1),
    SWD_percentile = round(100 * SWD_rank/SWD_denom + 1e-10, 1),
    super_subgroup_percentile = round(100 * super_subgroup_rank/super_subgroup_denom + 1e-10, 1),
    White_percentile = round(100 * White_rank/White_denom + 1e-10, 1)
  ) %>% 
  mutate_at(vars(all_students_percentile:White_percentile),
            .funs = list(targeted_support = ~case_when(
              is.na(.) ~ NA_real_,
              . <= 5 ~ 1, # TSI if in bottom 5 percent for subgroup and less than 1.1
              TRUE ~ 0
            )
            )
  ) %>% 
  rename_at( vars( contains( "_targeted_support") ), list( ~paste("targeted_support", gsub("_percentile_targeted_support", "", .), sep = "_") ) ) #%>% 
# Uncomment to see change if 1.1 safe harbor is used 
# mutate(
#   targeted_support_all_students = if_else(all_students >= 1.1, 0, targeted_support_all_students),
#   targeted_support_Native = if_else(Native >= 1.1, 0, targeted_support_Native),
#   targeted_support_Aisan = if_else(Asian >= 1.1, 0, targeted_support_Asian),
#   targeted_support_Black = if_else(Black >= 1.1, 0, targeted_support_Black),
#   targeted_support_BHN = if_else(BHN >= 1.1, 0, targeted_support_BHN),
#   targeted_support_ED = if_else(ED >= 1.1, 0, targeted_support_ED),
#   targeted_support_EL = if_else(EL >= 1.1, 0, targeted_support_EL),
#   targeted_support_Hispanic = if_else(Hispanic >= 1.1, 0, targeted_support_Hispanic),
#   targeted_support_HPI = if_else(HPI >= 1.1, 0, targeted_support_HPI),
#   targeted_support_SWD = if_else(SWD >= 1.1, 0, targeted_support_SWD),
#   targeted_support_super_subgroup = if_else(super_subgroup >= 1.1, 0, targeted_support_super_subgroup),
#   targeted_support_White = if_else(White >= 1.1, 0, targeted_support_White)
# ) %>%
tsi_cut_scores <- tsi_df %>% 
  select(system:White, all_students_percentile:White_percentile, targeted_support_all_students:targeted_support_White, 
         -all_students, -super_subgroup,-all_students_percentile, -super_subgroup_percentile, -targeted_support_all_students, -targeted_support_super_subgroup) %>% 
  gather(subgroup, targeted_support, targeted_support_Native:targeted_support_White) %>% 
  select(system:tsi_eligible, subgroup, targeted_support) %>% 
  filter(!is.na(targeted_support)) %>% 
  mutate(
    subgroup = case_when(
      str_detect(subgroup, 'Asian') ~ 'Asian',# subgroup == 'targeted_support_Asian' ~ 'Asian',
      str_detect(subgroup, 'Black') ~ 'Black or African American',
      str_detect(subgroup, 'BHN') ~ 'Black/Hispanic/Native American',
      str_detect(subgroup, 'EL') ~ 'English Learners with Transitional 1-4',
      str_detect(subgroup, 'Hispanic') ~ 'Hispanic',
      str_detect(subgroup, 'White') ~ 'White',
      str_detect(subgroup, 'Native') ~ 'American Indian or Alaska Native',
      str_detect(subgroup, 'ED') ~ 'Economically Disadvantaged',
      str_detect(subgroup, 'SWD') ~ 'Students with Disabilities',
      TRUE ~ NA_character_
    )
  ) %>% 
  left_join(
    tsi_df %>% 
      select(system:White, all_students_percentile:White_percentile, targeted_support_all_students:targeted_support_White, 
             -all_students, -super_subgroup,-all_students_percentile, -super_subgroup_percentile, -targeted_support_all_students, -targeted_support_super_subgroup) %>% 
      gather(subgroup, percentile, Native_percentile:White_percentile) %>% 
      select(system, school, subgroup, percentile) %>% 
      mutate(
        subgroup = case_when(
          str_detect(subgroup, 'Asian') ~ 'Asian',# subgroup == 'targeted_support_Asian' ~ 'Asian',
          str_detect(subgroup, 'Black') ~ 'Black or African American',
          str_detect(subgroup, 'BHN') ~ 'Black/Hispanic/Native American',
          str_detect(subgroup, 'EL') ~ 'English Learners with Transitional 1-4',
          str_detect(subgroup, 'Hispanic') ~ 'Hispanic',
          str_detect(subgroup, 'White') ~ 'White',
          str_detect(subgroup, 'Native') ~ 'American Indian or Alaska Native',
          str_detect(subgroup, 'ED') ~ 'Economically Disadvantaged',
          str_detect(subgroup, 'SWD') ~ 'Students with Disabilities',
          TRUE ~ NA_character_
        )
      ),
    by = c('system', 'school', 'subgroup')
  ) %>% 
  left_join(
    tsi_df %>% 
      select(system:White, all_students_percentile:White_percentile, targeted_support_all_students:targeted_support_White, 
             -all_students, -super_subgroup,-all_students_percentile, -super_subgroup_percentile, -targeted_support_all_students, -targeted_support_super_subgroup) %>% 
      gather(subgroup, subgroup_score, Native:White) %>% 
      select(system, school, subgroup, subgroup_score) %>% 
      mutate(
        subgroup = case_when(
          str_detect(subgroup, 'Asian') ~ 'Asian',# subgroup == 'targeted_support_Asian' ~ 'Asian',
          str_detect(subgroup, 'Black') ~ 'Black or African American',
          str_detect(subgroup, 'BHN') ~ 'Black/Hispanic/Native American',
          str_detect(subgroup, 'EL') ~ 'English Learners with Transitional 1-4',
          str_detect(subgroup, 'Hispanic') ~ 'Hispanic',
          str_detect(subgroup, 'White') ~ 'White',
          str_detect(subgroup, 'Native') ~ 'American Indian or Alaska Native',
          str_detect(subgroup, 'ED') ~ 'Economically Disadvantaged',
          str_detect(subgroup, 'SWD') ~ 'Students with Disabilities',
          TRUE ~ NA_character_
        )
      ),
    by = c('system', 'school', 'subgroup')
  ) %>% 
  filter(targeted_support == 1) %>% 
  group_by(subgroup) %>% 
  summarise(
    tsi_cut_score = max(subgroup_score)
  )

# write_csv(tsi_cut_scores, 'N:/ORP_accountability/projects/2019_school_accountability/tsi_cut_scores_2019.csv')


tsi_df <- tsi_df %>% 
  select(system:tsi_eligible, contains( "targeted_support_"), -targeted_support_all_students, 
         -targeted_support_super_subgroup) %>% 
  rowwise() %>% 
  mutate(
    targeted_support_subgroups = sum(targeted_support_Native,targeted_support_Asian, targeted_support_Black, targeted_support_BHN,
                                     targeted_support_ED, targeted_support_EL, targeted_support_Hispanic,
                                     targeted_support_HPI, targeted_support_SWD, targeted_support_White, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(targeted_support = if_else(targeted_support_subgroups > 0, 1, 0))

school_grading_grades <- tsi_df %>% 
  select(-system_name, - school_name) %>% 
  left_join(school_final_weighted %>% select(system, school, score_achievement:score_elpa, final_average = final_score), by = c('system', 'school')) %>% 
  select(system:designation_ineligible, score_achievement:score_elpa, targeted_support_Native:targeted_support, final_average)

# write_csv(school_grading_grades, "N:/ORP_accountability/projects/Andrew/Accountability/2019/appeal data/school_grading_grades_maury_hs_n_20.csv", na='')
# write_csv(school_grading_grades, "N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades_AM_Aug14.csv", na='')

# ========================================= Priority Exit ============================================================

priority_prior <- read_excel("N:\\ORP_accountability\\data\\2018_final_accountability_files\\school_designations_file.xlsx", sheet = 'Priority') %>% 
  select(system:pool, priority, grad_less_than_67, comprehensive_support, percentile)

priority_pool_maxes_prior <- read_excel("N:\\ORP_accountability\\data\\2018_final_accountability_files\\school_designations_file.xlsx", sheet = 'Priority') %>% 
  group_by(pool) %>% 
  summarise(
    pool_priority_max_success_rate = max(pct_on_mastered, na.rm=TRUE)
  )

# Most recent year for priority exit
priority_exit_success_current <- read_csv("N:\\ORP_accountability\\data\\2019_final_accountability_files\\school_accountability_file_AM.csv") %>% 
  filter(designation_ineligible == 0, indicator %in% c("Achievement", 'Graduation Rate'), subgroup == 'All Students') %>% 
  select(system:subgroup,metric) %>% 
  spread(indicator, metric) %>% 
  clean_names() %>% 
  mutate(
    rank = if_else(!is.na(achievement), rank(achievement, ties = "max"), NA_integer_),
    denom = sum(!is.na(achievement)),
    percentile = round(100 * rank/denom + 1e-10, 1),
    success_10th_percentile_current = if_else(percentile > 10, 1, 0),
    success_15th_percentile_current = if_else(percentile > 15, 1, 0),
    # tvaas_4_5_2018 = if_else(growth >= 4, 1, 0),
    grad_rate_67_current = if_else(graduation_rate >= 67, 1, 0)
  )

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

priority_exit_tvaas_prior <- read_excel("N:/ORP_accountability/data/2018_tvaas/SAS-TDOE-School-VA-by-Subject.xlsx") %>% 
  clean_names() %>% 
  rename(system = district_number, school_name = school, school = school_number) %>% 
  mutate(subject = if_else(subject == 'English Language Arts', 'ELA', subject),
         system = as.numeric(system), school = as.numeric(school)) %>% 
  filter(number_of_students >= 30, subject %in% c(math_eoc, english_eoc, 'Math', 'ELA'), 
         (is.na(grade) | grade=='Cumulative Grades')) %>% #, subgroup == 'All Students'
  mutate(
    score = as.numeric(str_extract(level, "(\\d)")),
    tvaas_4_5_current = if_else(score >= 4, 1, 0)
  ) %>% 
  group_by(system, school) %>% 
  mutate(
    tvaas_total_4_5 = sum(tvaas_4_5_current, na.rm = TRUE),
    tvaas_total_subgroups = sum(!is.na(tvaas_4_5_current))
  ) %>% 
  ungroup() %>% 
  mutate(
    tvaas_exit_prior = if_else(tvaas_total_4_5 == tvaas_total_subgroups, 1, 0)
  ) %>% 
  select(system, school, tvaas_exit_prior) %>% 
  distinct()

priority_exit_success_prior <- read_csv("N:\\ORP_accountability\\data\\2018_final_accountability_files\\2018_school_accountability_file.csv") %>% 
  filter(designation_ineligible == 0, indicator %in% c("Achievement", 'Graduation Rate'), subgroup == 'All Students') %>% 
  select(system:subgroup,metric) %>% 
  spread(indicator, metric) %>% 
  clean_names() %>% 
  mutate(
    rank = if_else(!is.na(achievement), rank(achievement, ties = "max"), NA_integer_),
    denom = sum(!is.na(achievement)),
    percentile = round(100 * rank/denom + 1e-10, 1),
    success_10th_percentile_prior = if_else(percentile > 10, 1, 0),
    grad_rate_67_prior = if_else(graduation_rate >= 67, 1, 0)
  )

priority_exit <- priority_prior %>% 
  left_join(priority_exit_success_current %>% select(system, school, success_10th_percentile_current, success_15th_percentile_current, grad_rate_67_current),
            by = c('system', 'school')) %>% 
  left_join(priority_exit_success_prior %>% select(system, school, success_10th_percentile_prior, grad_rate_67_prior),
            by = c('system', 'school')) %>% 
  left_join(priority_exit_tvaas_prior, by = c('system', 'school')) %>% 
  mutate(
    priority_exit = case_when(
      success_10th_percentile_current == 1 & success_10th_percentile_prior == 1 ~ 1,
      success_15th_percentile_current == 1 ~ 1,
      grad_less_than_67 == 1 & grad_rate_67_current == 1 & grad_rate_67_prior == 1 ~ 1,
      TRUE ~ 0
    )
  )

# Two years ago for priority exit criteria
# priority_exit_2017 <- read_csv("N:\\ORP_accountability\\data\\2018_final_accountability_files\\2018_school_accountability_file.csv") %>% 
#   filter(designation_ineligible == 0, indicator %in% c("Achievement", 'Growth', 'Graduation Rate')) %>% 
#   select(system:subgroup,metric) %>% 
#   spread(indicator, metric) %>% 
#   clean_names() %>% 
#   mutate(
#     success_10th_percentile_2018 = if_else(achievement > quantile(achievement, probs = 0.10, na.rm = TRUE), 1, 0),
#     success_15th_percentile_2018 = if_else(achievement > quantile(achievement, probs = 0.15, na.rm = TRUE), 1, 0),
#     tvaas_4_5_2018 = if_else(growth >= 4, 1, 0),
#     grad_rate_67_2018 = if_else(graduation_rate >= 67, 1, 0)
#   )

# ========================================= ATSI Schools ===========================================================
acct_2017 <- read_csv("N:/ORP_accountability/data/2017_final_accountability_files/school_numeric_2017_JW_10242017.csv") %>% 
  select(-system_name, -school_name) %>% 
  filter(year == 2017, !subject %in% c('ACT', 'Graduation Rate', 'Science'), !str_detect(subgroup, 'Non-')) %>% 
  mutate(
    subgroup = if_else(subgroup == 'English Language Learners with T1/T2', 'English Learners with Transitional 1-4', subgroup)
  ) %>% 
  filter(valid_tests >= 30) %>% 
  group_by(system, school, subgroup) %>% 
  summarise(
    n_count_2017 = sum(valid_tests),
    n_on_track_mastered_2017 = sum(n_ontrack_prof) + sum(n_mastered_adv)
  ) %>% 
  ungroup() %>% 
  filter(n_count_2017 >= 30)

acct_2018 <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2018_school_accountability_file.csv") %>% 
  filter(indicator == 'Achievement', designation_ineligible == 0) %>% 
  mutate(
    n_on_track_mastered_2018 = round(metric * n_count / 100, 0)
  ) %>% 
  filter(!is.na(n_on_track_mastered_2018)) %>% 
  select(system, school, subgroup, n_count_2018 = n_count, n_on_track_mastered_2018)

acct_2019 <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file_AM.csv") %>% 
  filter(indicator == 'Achievement', designation_ineligible == 0) %>% 
  mutate(
    n_on_track_mastered_2019 = round(metric * n_count / 100, 0)
  ) %>% 
  select(system:designation_ineligible,subgroup, n_on_track_mastered_2019, n_count_2019 = n_count)

grad_2018 <- read_csv("N:/ORP_accountability/data/2018_graduation_rate/school_grad_rate.csv") %>% 
  select(-system_name, -school_name) %>% 
  filter(!str_detect(subgroup, 'Non-'), !subgroup %in% c('Female', 'Homeless', 'Hispanic', 'Male', 'Migrant')) %>% 
  select(system, school, subgroup, grad_count_2018 = grad_count)

grad_2017 <- read_csv("N:/ORP_accountability/data/2017_graduation_rate/grad_rate_base_EK.csv") %>% 
  select(-system_name, -school_name, -subject) %>% 
  filter(school != 0, !str_detect(subgroup, 'Non-'), !subgroup %in% c('Female', 'Homeless', 'Hispanic', 'Male', 'Migrant')) %>% 
  mutate(subgroup = if_else(subgroup == 'English Learners', 'English Learners with Transitional 1-4', subgroup)) %>% 
  select(system, school, subgroup, grad_count_2017 = grad_count)

grad_2016 <- read_csv("N:/ORP_accountability/data/2016/2016_graduation_rate/grad_rate_base_EK.csv") %>% 
  select(-system_name, -school_name, -subject) %>% 
  filter(school != 0, !str_detect(subgroup, 'Non-'), !subgroup %in% c('Female', 'Homeless', 'Hispanic', 'Male', 'Migrant')) %>% 
  mutate(subgroup = if_else(subgroup == 'English Learners', 'English Learners with Transitional 1-4', subgroup)) %>% 
  select(system, school, subgroup, grad_count_2016 = grad_count)

act_2018 <- read.dta13("N:/ORP_accountability/data/2018_ACT/Post-Appeals/ACT_school2019_appeals.dta") %>% 
  select(system, school, subgroup, n_21_or_higher_2018 = n_21_orhigher) %>% 
  filter(!str_detect(subgroup, 'Non-'), !subgroup %in% c('Female', 'Homeless', 'Hispanic', 'Male', 'Migrant')) %>% 
  mutate(
    subgroup = case_when(
      subgroup == 'HPI' ~ 'Native Hawaiian or Other Pacific Islander',
      subgroup == 'English Language Learners with T1/T2' ~ 'English Learners with Transitional 1-4',
      subgroup == 'Native American' ~ "American Indian or Alaska Native",
      TRUE ~ subgroup
    )
  )

act_2017 <- read_csv("N:/ORP_accountability/data/2017_ACT/act_base_EK_post_appeals.csv") %>% 
  select(system, school, subgroup, n_21_or_higher_2017 = n_21_or_higher) %>% 
  filter(school != 0, !str_detect(subgroup, 'Non-'), !subgroup %in% c('Female', 'Homeless', 'Hispanic', 'Male', 'Migrant')) %>% 
  mutate(
    subgroup = case_when(
      subgroup == 'Hawaiian or Pacific Islander' ~ 'Native Hawaiian or Other Pacific Islander',
      subgroup == 'English Learners' ~ 'English Learners with Transitional 1-4',
      subgroup == 'Hispanic or Latino' ~ 'Hispanic',
      subgroup == 'Native American' ~ "American Indian or Alaska Native",
      TRUE ~ subgroup
    )
  )

act_2016 <- read.dta13("N:/ORP_accountability/data/2016/2016_ACT/ACT_school2017_individualsubgroups.dta") %>% 
  select(system, school, subgroup, n_21_orhigher_2016 = n_21_orhigher) %>% 
  filter(school != 0, !str_detect(subgroup, 'Non-'), !subgroup %in% c('Female', 'Homeless', 'Hispanic', 'Male', 'Migrant')) %>% 
  mutate(
    subgroup = case_when(
      subgroup == 'HPI' ~ 'Native Hawaiian or Other Pacific Islander',
      subgroup == 'English Language Learners with T1/T2' ~ 'English Learners with Transitional 1-4',
      subgroup == 'Hispanic or Latino' ~ 'Hispanic',
      subgroup == 'Native American' ~ "American Indian or Alaska Native",
      TRUE ~ subgroup
    )
  )

total_three_year_success_rate <- acct_2019 %>% 
  left_join(acct_2018, by = c('system', 'school', 'subgroup')) %>% 
  left_join(acct_2017, by = c('system', 'school', 'subgroup')) %>% 
  left_join(grad_2018, by = c('system', 'school', 'subgroup')) %>% 
  left_join(grad_2017, by = c('system', 'school', 'subgroup')) %>% 
  left_join(grad_2016, by = c('system', 'school', 'subgroup')) %>% 
  left_join(act_2018, by = c('system', 'school', 'subgroup')) %>% 
  left_join(act_2017, by = c('system', 'school', 'subgroup')) %>% 
  left_join(act_2016, by = c('system', 'school', 'subgroup')) %>% 
  mutate(
    n_21_or_higher_2018 = if_else(grad_count_2018 < 30, 0L, as.integer(n_21_or_higher_2018)),
    n_21_or_higher_2017 = if_else(grad_count_2017 < 30, 0L, as.integer(n_21_or_higher_2017)),
    n_21_orhigher_2016 = if_else(grad_count_2016 < 30, 0L, as.integer(n_21_orhigher_2016)),
    grad_count_2018 = if_else(grad_count_2018 < 30, 0L, as.integer(grad_count_2018)),
    grad_count_2017 = if_else(grad_count_2017 < 30, 0L, as.integer(grad_count_2017)),
    grad_count_2016 = if_else(grad_count_2016 < 30, 0L, as.integer(grad_count_2016)),
    three_year_numerator = n_on_track_mastered_2019 + n_on_track_mastered_2018 + n_on_track_mastered_2017 +
      n_21_or_higher_2018 + n_21_or_higher_2017 + n_21_orhigher_2016,
    three_year_denominator = n_count_2019 + n_count_2018 + n_count_2017 + 
      grad_count_2018 + grad_count_2017 + grad_count_2016,
    success_rate = round(three_year_numerator/three_year_denominator * 100 + 1e-10, 1)
  )

atsi_df <- total_accountability %>% 
  filter(designation_ineligible == 0, indicator %in% c('Achievement', 'Graduation Rate')) %>% 
  select(system:pool,indicator,subgroup, metric) %>% 
  spread(indicator, metric) %>% 
  rename(success_rate = Achievement, grad_rate = `Graduation Rate`) %>% 
  left_join(priority_pool_maxes, by = 'pool') %>% 
  left_join(school_designations_all_subgroups %>% select(system, school, subgroup, final_grade), by = c('system', 'school', 'subgroup')) %>% 
  left_join(tsi_df %>% select(system, school, targeted_support), by = c('system', 'school')) %>% 
  mutate(
    additional_targeted_support = case_when(
      targeted_support == 1 & success_rate <= pool_priority_max_success_rate & !final_grade %in% c('A', 'B') ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  arrange(system, school)


# ======================================== Reward school identification ==================================================
reward_df <- school_final_weighted %>% 
  select(system:pool, all_student_score = `All Students`, subgroup_score = `Historically Underserved`, final_score) %>% 
  mutate(
    reward = if_else(final_score >=3.1, 1, 0)
  ) %>% 
  left_join(priority_prior, by = c('system', 'system_name', 'school', 'school_name', 'pool')) %>% 
  replace_na(list(priority = 0)) %>% 
  select(system:pool, priority, all_student_score, subgroup_score, overall_score = final_score, reward)




# re-assign grades with a minus sign if focus school

grade_with_minus_sign <- school_final_grades %>% 
  left_join(tsi_df %>% select(system, school, targeted_support), by = c('system', 'school')) %>% 
  mutate(
    final_grade = case_when(
      targeted_support == 1 & final_grade == 'A' ~ 'B-',
      targeted_support == 1 & final_grade != 'D' ~ paste0(final_grade, '-'),
      TRUE ~ final_grade
    )
  ) %>% 
  select(-targeted_support)














