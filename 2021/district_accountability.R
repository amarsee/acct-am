library(tidyverse)
library(janitor)
library(readxl)
library(readstata13)
library(writexl)

grade_pools <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv") %>% 
  select(system, school, pool, designation_ineligible)

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

amo_dist_success <- read_csv("N:/ORP_accountability/projects/2019_amo/success_rate_targets_district.csv") %>% 
  transmute(system, grade, subgroup, metric_prior = success_rate_prior, AMO_target, AMO_target_double)

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


# all_students <- sl %>% 
#   bind_rows(act_sub) %>% 
#   mutate(subgroup = "All Students") %>% 
#   total_by_subgroup_dist()
# 
# 
# cat_subgroups <- function(student_df, students_grouped ){
#   base_df = students_grouped
#   subgroups <- c("Black/Hispanic/Native American", "Economically Disadvantaged", "English Learners with Transitional 1-4", "Students with Disabilities")
#   for (subgroup in subgroups){
#     if (subgroup == "Black/Hispanic/Native American"){
#       hist_df <- student_df %>% 
#         filter(bhn_group > 0) %>% 
#         mutate(subgroup = "Black/Hispanic/Native American")
#     } else if (subgroup == "Economically Disadvantaged") {
#       hist_df <- student_df %>% 
#         filter(economically_disadvantaged > 0) %>% 
#         mutate(subgroup = "Economically Disadvantaged")
#     }else if (subgroup == "English Learners with Transitional 1-4") {
#       hist_df <- student_df %>% 
#         filter(t1234 > 0 | el > 0) %>% 
#         mutate(subgroup = "English Learners with Transitional 1-4")
#     }else {
#       hist_df <- student_df %>% 
#         filter(special_ed > 0) %>% 
#         mutate(subgroup = "Students with Disabilities")
#     }
#     hist_grouped <- total_by_subgroup_dist(hist_df)
#     base_df <- rbind(base_df, hist_grouped)
#   }
#   return(base_df)
# }

  # ======================================= Success Rate ================================================

success_value_added_dist <- read_excel("N:/ORP_accountability/data/2019_tvaas/2019-District-Level-Accountability-Results-EOC-TCAP.xlsx") %>%
  clean_names() %>%
  rename(system = system_number, system_name = system) %>% 
  filter(subgroup != "Super Subgroup") %>% # grade != 'Grades 4-5', 
  mutate(system = as.integer(system),
         subgroup = case_when(
           subgroup == "English Learners (includes EL and T1-4)" ~ "English Learners with Transitional 1-4",
           TRUE ~ subgroup
         ),
         grade = case_when(
           grade %in% c("Grades 3-5", "Grades 4-5") ~ "3rd through 5th",
           grade == "Grades 6-8" ~ "6th through 8th",
           TRUE ~ "9th through 12th"
         )
  ) %>%
  group_by(system, subgroup, grade) %>% 
  mutate(temp = max(index)) %>%
  ungroup() %>%
  filter(temp == index) %>%
  select(system, subgroup, grade, number_of_students, level) %>% 
  distinct() %>% 
  transmute(system, subgroup, grade, indicator = "Achievement",
            value_add_metric = level,
            value_add_pathway = case_when(
              value_add_metric == 5 ~ 4,
              value_add_metric == 4 ~ 3,
              value_add_metric == 3 ~ 2,
              value_add_metric == 2 ~ 1,
              value_add_metric == 1 ~ 0
            )
  )

dist_totals <- bind_rows(
    sl %>% bind_rows(act_sub) %>% mutate(subgroup = "All Students"),
    sl %>% filter(bhn_group > 0) %>% mutate(subgroup = "Black/Hispanic/Native American"),
    sl %>% filter(economically_disadvantaged > 0) %>% mutate(subgroup = "Economically Disadvantaged"),
    sl %>% filter(t1234 > 0 | el > 0) %>% mutate(subgroup = "English Learners with Transitional 1-4"),
    sl %>% filter(special_ed > 0) %>% mutate(subgroup = "Students with Disabilities")
  ) %>% 
  total_by_subgroup_dist() %>% 
  arrange(system, subject, subgroup)

# dist_totals <- cat_subgroups(sl, all_students) %>%
#   # rbind(all_students) %>% 
#   #rbind(all_students, super_subgroup) %>% 
#   arrange(system, subject, subgroup) %>% 
#   rename(subject = subject)
#   

dist_achievement <- dist_totals %>% 
  mutate(
    tested = if_else(valid_tests > 29, tested, 0),
    enrolled = if_else(valid_tests > 29, enrolled, 0),
    valid_tests = if_else(valid_tests > 29, valid_tests, 0),
    n_on_track = if_else(valid_tests > 29, n_on_track, NA_real_),
    n_mastered = if_else(valid_tests > 29, n_mastered, NA_real_)
  ) %>% 
  group_by(system, subgroup, grade)  %>%
  summarise(
    participation_rate = round(100 * sum(tested)/sum(enrolled) + 1e-10, 0),
    n_count = sum(valid_tests),
    success_rate = round(((sum(n_on_track, na.rm = TRUE) + sum(n_mastered, na.rm = TRUE))/sum(valid_tests)) * 100 + 1e-10, 1)
  ) %>%
  ungroup() %>% 
  mutate(success_rate = ifelse(is.na(success_rate), NA, success_rate)) %>% 
  transmute(system, subgroup, grade, indicator = 'Achievement', 
            participation_rate, n_count, metric = success_rate) %>% 
  ci_upper_bound() %>% 
  left_join(amo_dist_success, by = c('system', 'subgroup', 'grade')) %>% 
  mutate(
    absolute_pathway = case_when(
      metric >= 45 ~ 4,
      metric >= 35 ~ 3,
      metric >= 27.5 ~ 2,
      metric >= 20 ~ 1,
      metric >= 0 ~ 0
    ),
    AMO_pathway = case_when(
      metric >= AMO_target_double ~ 4,
      metric >= AMO_target ~ 3,
      ci_bound >= AMO_target ~ 2,
      ci_bound > metric_prior ~ 1,
      ci_bound <= metric_prior ~ 0
    )#,
    #AMO_absolute_pathway = pmax(score_abs, score_target)
  ) # %>% 
  # filter(!is.na(system_name))

dist_success_amo_absolute_value <- dist_achievement %>% 
  left_join(success_value_added_dist, by = c('system', 'subgroup', 'grade', 'indicator')) %>% 
  mutate(
    participation_rate = if_else(n_count == 0, NA_real_, participation_rate)
  ) %>% 
  mutate_at(
    .vars = c('absolute_pathway', 'AMO_pathway', 'value_add_pathway'),
    .f = ~ case_when(
      participation_rate < 95 & !is.na(.) ~ 0,
      TRUE ~ .
    )
  ) %>% 
  left_join(system_df, by = c('system')) %>% 
  filter(!is.na(system_name)) %>% 
  select(system, system_name, subgroup:indicator, participation_rate, n_count:value_add_pathway)

# ========================== ACT/SAT Participation =============================================
subgroups <- c("Black/Hispanic/Native American", "Economically Disadvantaged", "English Learners with Transitional 1-4",
               "Students with Disabilities", "American Indian or Alaska Native", "Asian", "Black or African American",
               "Hispanic", "White")


dist_act_participation <- read_csv("N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_district.csv") %>% 
  rename(participation_rate = act_participation_rate) 
  

# =================================== Graduation Rate =====================================
value_add_grad <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_grad_va.csv")

amo_dist_grad <- read_csv("N:/ORP_accountability/projects/2019_amo/grad_district.csv") %>% 
  #filter(!grepl("Non-", subgroup)) %>% 
  transmute(
    system, 
    subgroup = case_when(
      subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
      TRUE ~ subgroup
    ),
    metric_prior = if_else(grad_cohort >= 30, grad_rate, NA_real_), 
    AMO_target, AMO_target_double
  )

dist_grad <- read_csv("N:/ORP_accountability/data/2018_graduation_rate/district_grad_rate.csv") %>% 
  # filter(system != 0, !grepl("Non-", subgroup)) %>% # school !=0, 
  mutate(
    subgroup = case_when(
          subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
           TRUE ~ subgroup
        )
  ) %>% 
  filter(subgroup %in% c('All Students',subgroups)) %>% 
  transmute(
    system,
    subgroup = case_when(
      subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
      TRUE ~ subgroup
    ), grade = 'All Grades', indicator = "Graduation Rate",
    n_count = if_else(grad_cohort >= 30, grad_cohort, 0),
    metric = if_else(n_count > 0, grad_rate, NA_real_)
  ) %>%
  ci_upper_bound() %>%
  left_join(amo_dist_grad, by = c('system', 'subgroup')) %>%
  mutate(
    absolute_pathway = case_when(
      metric >= 95 ~ 4,
      metric >= 90 ~ 3,
      metric >= 80 ~ 2,
      metric >= 67 ~ 1,
      metric >= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    AMO_pathway = case_when(
      metric >= AMO_target_double ~ 4,
      metric >= AMO_target ~ 3,
      ci_bound >= AMO_target ~ 2,
      ci_bound > metric_prior ~ 1,
      ci_bound <= metric_prior ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  left_join(value_add_grad %>% select(system, subgroup, value_add_metric, value_add_pathway), by = c('system', 'subgroup')) %>% 
  left_join(system_df,by = 'system') %>% 
  select(system, system_name, everything()) %>% 
  mutate(system_name = if_else(system == 90, 'Carroll County', system_name)) %>% 
  left_join(dist_act_participation %>% select(system, subgroup, participation_rate), by = c('system', 'subgroup')) %>% 
  mutate_at(
    .vars = c('absolute_pathway', 'AMO_pathway', 'value_add_pathway'),
    .f = ~ case_when(
      participation_rate < 95 & !is.na(.) ~ 0,
      TRUE ~ .
    )
  ) %>% 
  mutate(
    participation_rate = if_else(n_count == 0, NA_real_, participation_rate)
  ) %>% 
  filter(system != 90) %>% 
  select(system:indicator, participation_rate, n_count:value_add_pathway)


# ================================= ELPA =================================================

# ELPA AMO targets
elpa_amo <- read_csv('N:/ORP_accountability/projects/2019_amo/elpa_district.csv')  %>% 
  mutate(subgroup = if_else(subgroup == 'English Learners', "English Learners with Transitional 1-4", subgroup)) %>% 
  filter(subgroup %in% c(subgroups, 'All Students')) %>% 
  transmute(system, subgroup, metric_prior = if_else(growth_standard_denom >= 30, pct_met_growth_standard, NA_real_) , 
            AMO_target, AMO_target_double)

# ELPA Value Add
value_add_elpa <- read_csv('N:\\ORP_accountability\\data\\2019_final_accountability_files\\district_elpa_va_AM.csv') %>% 
  mutate(subgroup = if_else(subgroup == "English Learners", "English Learners with Transitional 1-4", subgroup))

# ELPA 
elpa <- read_csv('N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_district.csv') %>%
  filter(!is.na(system_name)) %>% 
  mutate(subgroup = if_else(subgroup == 'English Learners', "English Learners with Transitional 1-4", subgroup)) %>% 
  filter(subgroup %in% c('All Students', subgroups)) %>% 
  transmute(
    system, 
    subgroup, grade = 'All Grades', indicator = "ELPA Growth Standard",
    n_count = if_else(growth_standard_denom >= 30, growth_standard_denom, 0),
    metric = if_else(n_count > 0, pct_met_growth_standard, NA_real_)
  ) %>% 
  ci_upper_bound() %>% 
  left_join(elpa_amo, by = c('system', 'subgroup')) %>% 
  mutate(
    absolute_pathway = case_when(
      metric >= 60 ~ 4,
      metric >= 50 ~ 3,
      metric >= 40 ~ 2,
      metric >= 25 ~ 1,
      metric >= 0 ~ 0,
      TRUE ~ NA_real_
    ),
    AMO_pathway = case_when(
      metric >= AMO_target_double ~ 4,
      metric >= AMO_target ~ 3,
      ci_bound >= AMO_target ~ 2,
      ci_bound > metric_prior ~ 1,
      ci_bound <= metric_prior ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% 
  # Join value add df
  left_join(value_add_elpa, by = c('system', 'subgroup')) %>% 
  left_join(system_df, by = 'system') %>% 
  select(system, system_name, everything()) 


# =================================== Chronic Absenteeism ==================================================

amo_dist_absenteeism <- read_csv("N:/ORP_accountability/projects/2019_amo/absenteeism_targets_district_primary_enrollment.csv") %>% 
  filter(grade_band == 'All Grades') %>% 
  transmute(
    system, system_name, 
    subgroup = case_when(
      subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
      TRUE ~ subgroup
    ),
    metric_prior = if_else(n_students >= 30, pct_chronically_absent, NA_real_), 
    AMO_target, AMO_target_double
  )

dist_absenteeism <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/district_chronic_absenteeism_Jul11.csv") %>%
  filter(grade_band == 'All Grades', subgroup %in% c('All Students', subgroups)) %>% 
  transmute(system, system_name, subgroup, grade = grade_band,
            indicator = 'Chronic Absenteeism',
            n_count = if_else(n_students >= 30, n_students, 0),
            metric = if_else(n_count > 0, pct_chronically_absent, NA_real_)) %>%
  ci_lower_bound() %>%
  left_join(amo_dist_absenteeism, by = c('system', 'system_name', 'subgroup')) %>%
  mutate(
    absolute_pathway = case_when(
      metric <= 8 ~ 4,
      metric <= 11.5 ~ 3,
      metric <= 16.5 ~ 2,
      
      metric <= 25 ~ 1,
      metric > 25 ~ 0,
      TRUE ~ NA_real_
    ),
    AMO_pathway = case_when(
      metric <= AMO_target_double ~ 4,
      metric <= AMO_target ~ 3,
      ci_bound <= AMO_target ~ 2,
      ci_bound < metric_prior ~ 1,
      ci_bound >= metric_prior ~ 0,
      TRUE ~ NA_real_
    )#,
    #AMO_absolute_pathway = pmax(score_abs, score_target)
  ) %>% 
  mutate(
    system_name = if_else(system == 970, "Department Of Children's Services Education Division", system_name)
  )

ca_value_add <- read_csv('N:\\ORP_accountability\\data\\2019_final_accountability_files\\district_absenteeism_va_AM.csv') %>% 
  mutate(subgroup = if_else(subgroup == 'English Learners', "English Learners with Transitional 1-4", subgroup))

chronic_absent_with_value <- dist_absenteeism %>% 
  left_join(ca_value_add, by = c('system', 'subgroup'))

# ================================== Total Accountability =======================================================

total_dist_accountability <- bind_rows(dist_success_amo_absolute_value, chronic_absent_with_value, elpa, dist_grad) %>% 
  filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged", "English Learners with Transitional 1-4",
                         "Students with Disabilities", "Super Subgroup")) %>% 
  select(system, system_name, indicator, grade, subgroup, participation_rate, everything()) %>% 
  arrange(system, indicator, grade, subgroup) %>% 
  fill(system_name) %>% 
  mutate(
    overall_score = case_when(
      (!is.na(absolute_pathway) & !is.na(AMO_pathway) & !is.na(value_add_pathway)) ~ (pmax(absolute_pathway, AMO_pathway) + value_add_pathway)/2,
      (!is.na(absolute_pathway) & !is.na(AMO_pathway)) ~ pmax(absolute_pathway, AMO_pathway),
      TRUE ~ NA_real_
    )
  ) %>% 
  distinct()

# Write out file
# write_xlsx(total_dist_accountability, path = "N:\\ORP_accountability\\projects\\Andrew\\Pre-Coding-2019\\data\\district_accountability_2019_example.xlsx")
write_csv(total_dist_accountability, path = "N:\\ORP_accountability\\data\\2019_final_accountability_files\\district_accountability_file_AM.csv", na = '')
  

# Compare Files
alex_comp <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_accountability_file.csv")

am_comp <- total_dist_accountability 
# 
comp_total <- setdiff(am_comp, alex_comp) %>% 
  bind_rows(setdiff(alex_comp, am_comp)) %>% 
  arrange(system, subgroup, indicator, grade)

comp_no_va <- setdiff(am_comp %>% select(-(value_add_metric:overall_score)), alex_comp %>% select(-(value_add_metric:overall_score))) %>% 
  bind_rows(setdiff(alex_comp %>% select(-(value_add_metric:overall_score)), am_comp %>% select(-(value_add_metric:overall_score)))) %>% 
  arrange(system, subgroup, indicator, grade)

district_acct <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_accountability_file_AM.csv")

district_scores <- district_acct %>% 
  transmute(
    system, system_name, subgroup,
    grade_indicator = paste(grade, indicator, sep = ' '),
    overall_score
  ) %>% 
  spread(grade_indicator, overall_score) %>% 
  rowwise() %>% 
  mutate(
    subgroup_average = mean(c(`3rd through 5th Achievement`,`6th through 8th Achievement`, `9th through 12th Achievement`,
                              `All Grades Graduation Rate`, `All Grades Chronic Absenteeism`, `All Grades ELPA Growth Standard`), na.rm = TRUE),#+1e-10,#,2),
    subgroup_average = ifelse(!is.na(subgroup_average), subgroup_average, NA)
  ) %>% 
  mutate(
    determination = case_when(
      subgroup_average >= 3.1 ~ 'Exemplary',
      subgroup_average >= 2.1 ~ 'Advancing',
      subgroup_average >= 1.1 ~ 'Satisfactory',
      subgroup_average >= 0 ~ 'Marginal',
      TRUE ~ NA_character_
    )
  )

# student_absent_2018_npe <- read_csv('N:/ORP_accountability/data/2018_chronic_absenteeism/student_chronic_absenteeism.csv')


system_final_determination <- district_scores %>% 
  select(system, system_name, subgroup, subgroup_average) %>% 
  spread(subgroup, subgroup_average) %>% 
  clean_names() %>% 
  rowwise() %>%
  mutate(
    # Only use historically underserved populations for subgroup average
    subgroup_avg = mean(c(black_hispanic_native_american, economically_disadvantaged, 
                                english_learners_with_transitional_1_4, students_with_disabilities), na.rm = TRUE),#, 2),
    # NA if not a historically underserved subgroup
    subgroup_avg = if_else(is.na(subgroup_avg), NA_real_, subgroup_avg)
  ) %>% 
  ungroup() %>% 
  # Filter NA because that means it's not a historically underserved subgroup
  filter(!is.na(subgroup_avg)) %>% 
  mutate(
    all_students_determination =  case_when(
      all_students >= 3.1 ~ 'Exemplary',
      all_students >= 2.1 ~ 'Advancing',
      all_students >= 1.1 ~ 'Satisfactory',
      all_students < 1.1 ~ 'Marginal'
    ), 
    subgroup_determination =  case_when(
      subgroup_avg >= 3.1 ~ 'Exemplary',
      subgroup_avg >= 2.1 ~ 'Advancing',
      subgroup_avg >= 1.1 ~ 'Satisfactory',
      subgroup_avg < 1.1 ~ 'Marginal'
    ), 
    overall_avg = round((all_students *0.6) + (subgroup_avg * 0.4) + 1e-5, 1),
    final_determination = case_when(
      overall_avg >= 3.1 ~ 'Exemplary',
      overall_avg >= 2.1 ~ 'Advancing',
      overall_avg >= 1.1 ~ 'Satisfactory',
      overall_avg < 1.1 ~ 'Marginal'
    ), 
    overall_g = 1
  ) %>% 
  group_by(overall_g) %>%
  mutate(
    rank = if_else(!is.na(overall_avg), rank(overall_avg, ties.method = "min"), NA_integer_),
    denom = sum(!is.na(overall_avg)),
    percentile = round(100 * rank/denom + 1e-10, 1),
    met_minimum_performance = if_else(percentile <= 5, 0, 1)
    #bottom_5_val = quantile(overall_avg, probs = 0.05, na.rm = TRUE, names = FALSE)
  ) %>% 
  ungroup() %>% 
  mutate(
    final_determination = case_when(
      # overall_avg < bottom_5_val ~ 'In Need of Improvement',
      met_minimum_performance == 0 ~ 'In Need of Improvement',
      TRUE ~ final_determination
    )
  ) %>% 
  transmute(system, system_name, achievement_average = all_students, achievement_determination = all_students_determination, 
            subgroup_average = subgroup_avg, subgroup_determination,
            overall_average = overall_avg, rank, denom, percentile, met_minimum_performance, final_determination)


write_csv(system_final_determination, "N:/ORP_accountability/data/2019_final_accountability_files/district_designations_AM.csv", na= '')

# ========================= Compare Designations =============================
alex_comp <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_designations.csv")

am_comp <- system_final_determination %>% 
  select(-system_name)
# 
comp_total <- setdiff(am_comp, alex_comp) %>% #  %>% select(system:overall_average)
  bind_rows(setdiff(alex_comp, am_comp)) %>% 
  arrange(system)


# ==================== Write Designations =========================
district_designations <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_designations.csv") %>% 
  filter(!is.na(final_determination)) %>% 
  left_join(system_df, by = 'system') %>% 
  select(system, system_name, final_determination)

write_csv(district_designations, "N:/ORP_accountability/projects/2019_district_accountability/district_final_designations.csv")




