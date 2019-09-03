library(tidyverse)
library(janitor)
library(readstata13)

# demographics_2019 <- read_csv("N:/Assessment_Data Returns/TCAP_End-of-Course/2018-19/Demographic Files/fall_eoc_demographics_snapshot_20181208.csv")

# demographics_2019_spring <- read_csv('N:\\TNReady\\2018-19\\spring\\demographics\\spring_2019_assessment_demographics_20190510.csv')

# demographics_latest <- read_csv('N:\\TNReady\\2018-19\\spring\\demographics\\student_demographics_20190610.csv')

demo_combined <- read_csv('N:/TNReady/2018-19/spring/demographics/spring_2019_assessment_demographics_combined_pull_20190610.csv')

demos_filtered_2019 <- demo_combined %>% 
  transmute(
    system = district_id,
    school = school_id,
    unique_student_id = student_key, 
    #grade,
    gender, 
    hispanic = if_else(ethnicity == 'H', 'Y', 'N'),
    economically_disadvantaged = case_when(
      codeab == 1 ~ 'Y',
      codeab == 2 ~ 'N',
      TRUE ~ NA_character_
    ),
    reported_race = reportedrace,
    title_1 = title1,
    gifted = isgifted,
    functionally_delayed = isfunctionallydelayed,
    migrant = ismigrant,
    el_arrived_year_1 = elrecentlyarrivedyearone,
    el_arrived_year_2 = elrecentlyarrivedyeartwo,
    el = isel,
    el_t1234 = t1t2,
    special_ed = specialeducation,
    ed = if_else(economically_disadvantaged == 'Y', 1, 0),
    enrolled_50_pct_district = district50percent,
    enrolled_50_pct_school = school50percent
  ) %>% 
  mutate(
    native_american = if_else(reported_race == 1, 1, 0),
    asian = if_else(reported_race == 2, 1, 0),
    black = if_else(reported_race == 3, 1, 0),
    hawaiian_pi = if_else(reported_race == 5, 1, 0),
    white = if_else(reported_race == 6, 1, 0),
    race = case_when(
      reported_race == 1 ~ 'American Indian/Alaska Native',
      reported_race == 2 ~ 'Asian',
      reported_race == 3 ~ 'Black or African American',
      reported_race == 4 ~ 'Hispanic/Latino',
      reported_race == 5 ~ 'Native Hawaiian/Pac. Islander',
      reported_race == 6 ~ 'White',
      TRUE ~ 'Unknown'
    ),
    bhn_group = if_else(reported_race %in% c(1,3,4), 1 ,0)
  )

school_names <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv")

dist_names <- school_names %>% 
  select(system, system_name) %>% 
  distinct()

prior <- read_csv("N:\\ORP_accountability\\data\\2018_ELPA\\wida_growth_standard_student_level.csv",
                  col_types = cols(.default = "n")) %>% #,
  clean_names() %>%
  select(student_id = student_id,
         prof_composite_prior = prof_composite,
         prof_composite_two_years_prior = prof_composite_17)


# elpa <- read.dta13('N:/Assessment_Data Returns/ACCESS for ELs and ALT/2017-18/WIDA/20180706_WIDA_AccessResults_SY2017-18_Whalen_v3.dta') %>% 
elpa <- read_csv("N:\\Assessment_Data Returns\\ACCESS for ELs and ALT\\2018-19\\TN_Summative_StudRR_File_2019-06-07.csv",
                 col_types = cols(.default = "c")) %>% 
  clean_names() %>% 
  filter(reported_mode != 'Mixed') %>% 
  filter(
    reported_mode != "Mixed",
    !(grade == 0 & cluster_listening != 0 & !is.na(cluster_listening)),
    !(grade == 1 & cluster_listening != 1 & !is.na(cluster_listening)),
    !(grade == 2 & reported_mode == "Paper" & cluster_listening != 2 & !is.na(cluster_listening)),
    !(grade == 3 & reported_mode == "Paper" & cluster_listening != 3 & !is.na(cluster_listening)),
    !(grade %in% 2:3 & reported_mode == "Online" & cluster_listening != 2 & !is.na(cluster_listening)),
    !(grade %in% 4:5 & cluster_listening != 4 & !is.na(cluster_listening)),
    !(grade %in% 6:8 & cluster_listening != 6 & !is.na(cluster_listening)),
    !(grade %in% 9:12 & cluster_listening != 9 & !is.na(cluster_listening))
  ) %>%
  transmute(
    student_id = as.numeric(state_student_id),
    system = as.integer(str_sub(district_number, 3, 5)),
    school = school_number,
    grade = as.integer(grade),
    scale_score_listening = listening_scale_score,
    scale_score_reading = reading_scale_score,
    scale_score_speaking = speaking_scale_score,
    scale_score_writing = writing_scale_score,
    scale_score_comprehension = comprehension_scale_score,
    scale_score_oral = oral_scale_score,
    scale_score_literacy = literacy_scale_score,
    scale_score_composite = composite_overall_scale_score,
    prof_listening = listening_proficiency_level,
    prof_reading = reading_proficiency_level,
    prof_speaking = speaking_proficiency_level,
    prof_writing = writing_proficiency_level,
    prof_comprehension = comprehension_proficiency_level,
    prof_oral = oral_proficiency_level,
    prof_literacy = literacy_proficiency_level,
    prof_composite = composite_overall_proficiency_level
  ) %>%
  mutate_at(vars(starts_with("scale_score_"), starts_with("prof_"), school, system), as.numeric) %>%
  filter(!is.na(student_id), !is.na(scale_score_composite)) %>% 
  mutate(
    school = if_else(system == 792 & school == 2596, 2598, school)
  )

subgroups <- c("American Indian or Alaska Native", "Asian", "Black or African American", "Black/Hispanic/Native American",
               "Economically Disadvantaged", "English Learners", "Hispanic", 
               "Native Hawaiian or Other Pacific Islander", "Students with Disabilities", "White")

elpa_total <- elpa %>% 
  group_by(student_id) %>% 
  mutate(student_max_comp = max(scale_score_composite, na.rm = TRUE)) %>% 
  filter(scale_score_composite == student_max_comp) %>% 
  mutate(student_max_lit = max(scale_score_literacy, na.rm=TRUE)) %>% 
  filter(scale_score_literacy == student_max_lit) %>% 
  ungroup() %>% 
  left_join(prior, by = 'student_id') %>% 
  mutate(
    exit_denom = if_else(!is.na(prof_composite) & !is.na(prof_literacy), 1, 0),
    # TN WIDA exit criteria
    exit = if_else(prof_literacy >= 4 & prof_composite >= 4.2, 1, 0),
    growth_standard_denom = if_else(!is.na(prof_composite) & !is.na(prof_composite_prior), 1,0),
    growth_standard_1yr = case_when(
      is.na(prof_composite_prior) ~ NA_real_,
      prof_composite_prior <= 1.4 ~ 1.3,
      prof_composite_prior <= 1.9 ~ 0.7,
      prof_composite_prior <= 2.4 ~ 0.8,
      prof_composite_prior <= 2.9 ~ 0.7,
      prof_composite_prior <= 3.4 ~ 0.4,
      prof_composite_prior <= 3.9 ~ 0.5,
      prof_composite_prior <= 4.4 ~ 0.4,
      prof_composite_prior <= 4.9 ~ 0.2
    ),
    growth_standard_2yr = case_when(
      is.na(prof_composite_two_years_prior) ~ NA_real_,
      prof_composite_two_years_prior <= 1.4 ~ round(1.3 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 1.9 ~ round(0.7 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 2.4 ~ round(0.8 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 2.9 ~ round(0.7 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 3.4 ~ round(0.4 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 3.9 ~ round(0.5 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 4.4 ~ round(0.4 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 4.9 ~ round(0.2 + prof_composite_two_years_prior + 1e-5, 1)
    ),
    growth_standard_2yr = case_when(
      is.na(growth_standard_2yr) ~ NA_real_,
      growth_standard_2yr <= 1.4 ~ round(1.3 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 1.9 ~ round(0.7 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 2.4 ~ round(0.8 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 2.9 ~ round(0.7 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 3.4 ~ round(0.4 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 3.9 ~ round(0.5 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 4.4 ~ round(0.4 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 4.9 ~ round(0.2 + growth_standard_2yr + 1e-5, 1)
    ),
    met_growth_standard = case_when(
      growth_standard_denom == 0 ~ NA_integer_,
      round(prof_composite - prof_composite_prior + 1e-5, 1) >= growth_standard_1yr | prof_composite >= growth_standard_2yr ~ 1L,
      # round(prof_composite - prof_composite_prior + 1e-5, 1) >= growth_standard_1yr | round(prof_composite - prof_composite_two_years_prior + 1e-5, 1) >= growth_standard_2yr ~ 1L,
      TRUE ~ 0L
    ),
    met_growth_standard = if_else(growth_standard_denom == 1L & exit == 1L, 1L, met_growth_standard)
  )

elpa_with_demo <- elpa_total %>% 
  mutate(school = as.numeric(school)) %>% 
  left_join(demos_filtered_2019 %>% rename(student_id = unique_student_id), by = c('student_id', 'system', 'school')) %>% 
  mutate(el = 1) # %>% 
  # replace_na(list(ed=0, swd=0))

out_df <- elpa_with_demo %>% 
  mutate(subgroup = 'All Students')
# super_sub <- elpa_with_demo %>% 
#   mutate(subgroup = 'Super Subgroup')
# out_df <- out_df %>% 
#   rbind(super_sub)

for (subgroup in subgroups){
  student_df <- elpa_with_demo
  if (subgroup == "American Indian or Alaska Native"){
    hist_df <- student_df %>% 
      filter(native_american == 1) %>% 
      mutate(subgroup = "American Indian or Alaska Native")
    non_hist_df <- 0
  }else if (subgroup == "Asian"){
    hist_df <- student_df %>% 
      filter(asian == 1) %>% 
      mutate(subgroup = "Asian")
    non_hist_df <- 0
  }else if (subgroup == "Black or African American"){
    hist_df <- student_df %>% 
      filter(black == 1) %>% 
      mutate(subgroup = "Black or African American")
    non_hist_df <- 0
  }else if (subgroup == "Black/Hispanic/Native American"){
    hist_df <- student_df %>% 
      filter(bhn_group == 1) %>% 
      mutate(subgroup = "Black/Hispanic/Native American")
    non_hist_df <- 0
  } else if (subgroup == "Economically Disadvantaged") {
    hist_df <- student_df %>% 
      filter(ed == 1) %>% 
      mutate(subgroup = "Economically Disadvantaged")
    non_hist_df <- student_df %>% 
      filter(ed == 0 | is.na(ed)) %>% 
      mutate(subgroup = "Non-Economically Disadvantaged")
  }else if (subgroup == "English Learners") {
    hist_df <- student_df %>% 
      filter(el == 1) %>% 
      mutate(subgroup = "English Learners")
      # mutate(subgroup = "English Learners with Transitional 1-4")
    non_hist_df <- 0
  }else if (subgroup == "Hispanic"){
    hist_df <- student_df %>% 
      filter(hispanic == 'Y') %>% 
      mutate(subgroup = "Hispanic")
    non_hist_df <- 0
  }else if (subgroup == "Native Hawaiian or Other Pacific Islander"){
    hist_df <- student_df %>% 
      filter(hawaiian_pi == 1) %>% 
      mutate(subgroup = "Native Hawaiian or Other Pacific Islander")
    non_hist_df <- 0
  }else if (subgroup == "White"){
    hist_df <- student_df %>% 
      filter(white == 1) %>% 
      mutate(subgroup = "White")
    non_hist_df <- 0
  }else {
    hist_df <- student_df %>% 
      filter(special_ed == 1) %>% 
      mutate(subgroup = "Students with Disabilities")
    non_hist_df <- student_df %>% 
      filter(special_ed == 0 | is.na(special_ed))%>% 
      mutate(subgroup = "Non-Students with Disabilities")
  }
  # hist_grouped <- total_by_subgroup(hist_df)
  out_df <- rbind(out_df, hist_df)
  if (is.data.frame(non_hist_df)){
    #non_hist_grouped <- total_by_subgroup(non_hist_df)
    out_df <- rbind(out_df, non_hist_df)
  }
}


# ================================= State Level ========================================

state_level <- out_df %>% 
  group_by(subgroup) %>% 
  summarise(
    exit_denom = sum(exit_denom, na.rm = TRUE),
    n_exit = sum(exit, na.rm = TRUE),
    growth_standard_denom = sum(growth_standard_denom, na.rm=TRUE), 
    n_met_growth_standard = sum(met_growth_standard, na.rm=TRUE),
    composite_avg = round(mean(prof_composite, na.rm=TRUE)+1e-5,1),
    literacy_avg = round(mean(prof_literacy, na.rm=TRUE)+1e-5,1)# ,
    # avg_composite_growth = round(mean(growth_standard_1yr, na.rm=TRUE)+1e-5,1)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_exit = round(n_exit / exit_denom * 100 + 1e-5, 1),
    pct_met_growth_standard = round(n_met_growth_standard / growth_standard_denom * 100 + 1e-5, 1)
  ) %>% 
  transmute(
    # year = 2019,
    # system = 0,
    # system_name = 'State of Tennessee',
    subgroup, exit_denom, n_exit, pct_exit, growth_standard_denom, 
    n_met_growth_standard,pct_met_growth_standard, literacy_avg, composite_avg # , avg_composite_growth
  )

# write state level csv
write_csv(state_level, "N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_state_AM.csv")

# ===================================== District Level =====================================

district_level <- out_df %>% 
  group_by(system, subgroup) %>% 
  summarise(
    exit_denom = sum(exit_denom, na.rm = TRUE),
    n_exit = sum(exit, na.rm = TRUE),
    growth_standard_denom = sum(growth_standard_denom, na.rm=TRUE), 
    n_met_growth_standard = sum(met_growth_standard, na.rm=TRUE),
    composite_avg = round(mean(prof_composite, na.rm=TRUE)+1e-5,1),
    literacy_avg = round(mean(prof_literacy, na.rm=TRUE)+1e-5,1)# ,
    # avg_composite_growth = round(mean(growth_standard_1yr, na.rm=TRUE)+1e-5,1)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_exit = round(n_exit / exit_denom * 100 + 1e-5, 1),
    pct_met_growth_standard = round(n_met_growth_standard / growth_standard_denom * 100 + 1e-5, 1)
  ) %>% 
  left_join(dist_names, by = 'system') %>% 
  transmute(
    # year = 2019,
    system, system_name,
    subgroup, exit_denom, n_exit, pct_exit, growth_standard_denom, 
    n_met_growth_standard,
    pct_met_growth_standard = if_else(is.na(pct_met_growth_standard), NA_real_, pct_met_growth_standard), 
    literacy_avg, composite_avg# , avg_composite_growth
  ) %>% 
  arrange(system, subgroup)

# write state level csv
write_csv(district_level, "N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_district_AM.csv")

# ===================================== School Level ===============================================

school_level <- out_df %>% 
  group_by(system, school, subgroup) %>% 
  summarise(
    exit_denom = sum(exit_denom, na.rm = TRUE),
    n_exit = sum(exit, na.rm = TRUE),
    growth_standard_denom = sum(growth_standard_denom, na.rm=TRUE), 
    n_met_growth_standard = sum(met_growth_standard, na.rm=TRUE),
    composite_avg = round(mean(prof_composite, na.rm=TRUE)+1e-5,1),
    literacy_avg = round(mean(prof_literacy, na.rm=TRUE)+1e-5,1)# ,
    # avg_composite_growth = round(mean(growth_standard_1yr, na.rm=TRUE)+1e-5,1)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_exit = round(n_exit / exit_denom * 100 + 1e-5, 1),
    pct_met_growth_standard = round(n_met_growth_standard / growth_standard_denom * 100 + 1e-5, 1)
  ) %>% 
  left_join(school_names, by = c('system', 'school')) %>% 
  transmute(
    # year = 2019,
    system, system_name, school, school_name,
    subgroup, exit_denom, n_exit, pct_exit, growth_standard_denom, 
    n_met_growth_standard,
    pct_met_growth_standard = if_else(is.na(pct_met_growth_standard), NA_real_, pct_met_growth_standard), 
    literacy_avg, composite_avg# , avg_composite_growth
  ) %>% 
  arrange(system, school, subgroup)

# write school level csv
write_csv(school_level, "N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_school_AM.csv")

# ====================================== Student Level ==========================================

elpa_student_level <- elpa_with_demo %>% 
  left_join(school_names, by = c('system', 'school')) %>% 
  transmute(system, system_name, school, school_name, student_id, grade, scale_score_listening, scale_score_reading, scale_score_speaking,
            scale_score_writing, scale_score_comprehension, scale_score_oral, scale_score_literacy, scale_score_composite,
            prof_listening, prof_reading, prof_speaking, prof_writing, prof_comprehension, prof_oral, prof_literacy, prof_composite,
            BHN = bhn_group, 
            Hispanic = if_else(hispanic == 'Y', 1, 0),
            Black = black,
            Native = native_american,
            HPI = hawaiian_pi,
            Asian = asian,
            White = white,
            SWD = special_ed,
            ED = if_else(economically_disadvantaged == 'Y', 1, 0),
            EL = 1,
            prof_composite_18 = prof_composite_prior,
            prof_composite_17 = prof_composite_two_years_prior,
            exit_denom, exit, growth_standard_denom, growth_standard_1yr, growth_standard_2yr,
            met_growth_standard
            )

# write csv
write_csv(elpa_student_level, "N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_student_AM.csv")


# elpa %>% 
#   filter(system %in% c(10, 61, 531, 792, 860)) %>% 
#   group_by(system) %>% 
#   summarise(
#     el_count = n()
#   ) %>% 
#   View()

# demo_latest_ids_from_first <- demos_filtered_2019 %>% filter(unique_student_id %in% demos_filtered_spring$unique_student_id)
# 
# demo_diff <- setdiff(demos_filtered_spring, demo_latest_ids_from_first)
# demo_diff_latest <- setdiff(demo_latest_ids_from_first, demos_filtered_spring)
# 
# colnames(demo_diff)[4:ncol(demo_diff)] <- paste(colnames(demo_diff)[4:ncol(demo_diff)], 'spring', sep = "_")
# colnames(demo_diff_latest)[4:ncol(demo_diff_latest)] <- paste(colnames(demo_diff_latest)[4:ncol(demo_diff_latest)], 'latest', sep = "_")
# 
# total_demo_diff <- demo_diff %>% 
#   left_join(demo_diff_latest %>% select(unique_student_id:bhn_group_latest), by = 'unique_student_id') %>% 
#   filter(!is.na(race_latest))
# 
# iter_cols <- colnames(demo_latest_ids_from_first)[4:(ncol(demo_latest_ids_from_first))]
# 
# for(col_id in iter_cols){
#   col_spring = paste(col_id, 'spring', sep = '_')
#   col_latest = paste(col_id, 'latest', sep = '_')
#   print(col_id)
#   print(table(total_demo_diff[[col_spring]], total_demo_diff[[col_latest]]))
# }
# 
# demographics_total <- demographics_2019_spring %>% 
#   bind_rows(demographics_latest %>% 
#               rename(district_id = district_no, school_id = school_no, instructionalavailability = instructionalavailabilty) %>% 
#               filter(!student_key %in% demographics_2019_spring$student_key)) %>% 
#   select(-iselexcluded)
# 
# write_csv(demographics_total, 'N:/TNReady/2018-19/spring/demographics/spring_2019_assessment_demographics_combined_pull_20190610.csv')
# 
# n_demo <- read_csv('N:/TNReady/2018-19/spring/demographics/spring_2019_assessment_demographics_combined_pull_20190610.csv')

















