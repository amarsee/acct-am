library(tidyverse)

# Functions
total_by_subgroup_dist <- function(df) {
  out_df <- df %>% 
    group_by(system, system_name, original_subject, grade, subgroup) %>% 
    summarise(
      valid_tests = sum( valid_test),
      n_on_track = sum(on_track),
      n_mastered = sum(mastered)
    ) %>% 
    ungroup()
  return(out_df)
}

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

student_level <- read_csv("N://ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv")

sl_current <- student_level %>% 
  mutate_at("residential_facility", ~ if_else(is.na(.), 0, .)) %>%
  mutate_at("enrolled_50_pct_district", ~ if_else(is.na(.), "Y", .)) %>%
  mutate(
    original_subject = case_when(
      grade < 9 & original_subject %in% c('Algebra I', 'Algebra II', "Geometry", "Integrated Math I", "Integrated Math II", 
                                          "Integrated Math III", 'English I', 'English II', 'Biology I', 'Chemistry') ~ subject,
      TRUE ~ original_subject
    )
  ) %>% 
  filter(residential_facility == 0, grade %in% 3:12, enrolled_50_pct_district == 'Y', !is.na(state_student_id),
         !original_subject %in% c("US History", "Social Studies")) %>% 
  filter(!(grade >= 9 & original_subject %in% c("Biology I", "Chemistry", "Science")))

sl_current <- sl_current %>% 
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
      grade >= 9 & original_subject %in% c('Algebra I', 'Algebra II', "Geometry", "Integrated Math I", "Integrated Math II", 
                                           "Integrated Math III") ~ 'Math',
      grade >= 9 & original_subject %in% c('English I', 'English II', 'English III') ~ 'ELA',
      TRUE ~ original_subject
    ),
    grade = case_when(
      grade >= 9 ~ '9th through 12th',
      grade >= 6 ~ '6th through 8th',
      grade >= 3 ~ '3rd through 5th'
    )
  ) %>% 
  # fill(system_name) %>% 
  rename(subgroup = reported_race)


# Transitional ELA On track or mastered current 
current_student_level_t14 <- sl_current %>% 
  filter(t1234 == 1) %>% 
  mutate(
    grade = 'All Grades'
  )

transition_all_students <- current_student_level_t14 %>% 
  # bind_rows(act_sub) %>% 
  mutate(subgroup = "All Students") %>% 
  total_by_subgroup_dist()

# transition_super_sub <- current_student_level_t14 %>% 
#   filter(bhn_group > 0 | economically_disadvantaged > 0 | t1234 > 0 | el > 0 | special_ed > 0) %>% 
#   mutate(subgroup = "Super Subgroup") %>% 
#   total_by_subgroup_dist()
# 
# transition_all_students <- transition_all_students %>% 
#   rbind(transition_super_sub)

dist_totals_transition_current <- cat_subgroups(current_student_level_t14, transition_all_students) %>% 
  filter(subgroup != "Unknown") %>% 
  #rbind(all_students, super_subgroup) %>% 
  arrange(system, original_subject, subgroup) %>% 
  rename(subject = original_subject) %>% 
  filter(subject == 'ELA') %>% 
  mutate(
    pct_on_mastered_current = if_else(valid_tests >= 30, round((n_on_track + n_mastered)/valid_tests * 100 + 1e-10, 1), NA_real_)
  )

# Transitional ELA On track or mastered prior 
student_level_prior <- read_csv("N:\\ORP_accountability\\projects\\2018_student_level_file\\2018_student_level_file.csv")

sl_prior <- student_level_prior %>% 
  rename(t1234 = el_t1234) %>% 
  mutate(
    original_subject = case_when(
      grade < 9 & original_subject %in% c('Algebra I', 'Algebra II', "Geometry", "Integrated Math I", "Integrated Math II", 
                                          "Integrated Math III", 'English I', 'English II', 'Biology I', 'Chemistry') ~ subject,
      TRUE ~ original_subject
    )
  ) %>% 
  filter(residential_facility == 0, grade %in% 3:12, enrolled_50_pct_district == 'Y', !is.na(state_student_id),
         !original_subject %in% c("US History", "Social Studies")) %>% 
  filter(!(grade >= 9 & original_subject %in% c("Biology I", "Chemistry", "Science")))

system_df_prior <- student_level_prior %>% 
  select(system, system_name) %>% 
  distinct() %>% 
  filter(!is.na(system_name))

sl_prior <- sl_prior %>% 
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
      grade >= 9 & original_subject %in% c('Algebra I', 'Algebra II', "Geometry", "Integrated Math I", "Integrated Math II", 
                                           "Integrated Math III") ~ 'Math',
      grade >= 9 & original_subject %in% c('English I', 'English II', 'English III') ~ 'ELA',
      TRUE ~ original_subject
    ),
    grade = case_when(
      grade >= 9 ~ '9th through 12th',
      grade >= 6 ~ '6th through 8th',
      grade >= 3 ~ '3rd through 5th'
    )
  ) %>% 
  # fill(system_name) %>% 
  mutate(grade = 'All Grades') %>% 
  rename(subgroup = race)

prior_student_level_t14 <- sl_prior %>% 
  filter(t1234 == 1)

transition_all_students_prior <- prior_student_level_t14 %>% 
  #   bind_rows(act_sub) %>% 
  mutate(subgroup = "All Students") %>% 
  total_by_subgroup_dist()

# transition_super_sub_prior <- prior_student_level_t14 %>% 
#   filter(bhn_group > 0 | economically_disadvantaged > 0 | t1234 > 0 | el > 0 | special_ed > 0) %>% 
#   mutate(subgroup = "Super Subgroup") %>% 
#   total_by_subgroup_dist()
# 
# transition_all_students_prior <- transition_all_students_prior %>% 
#   rbind(transition_super_sub_prior)

# Create subgroups
dist_totals_transition_prior <- cat_subgroups(prior_student_level_t14, transition_all_students_prior) %>% 
  filter(subgroup != "Unknown") %>% 
  #rbind(all_students, super_subgroup) %>% 
  arrange(system, original_subject, subgroup) %>% 
  rename(subject = original_subject) %>% 
  filter(subject == 'ELA') %>% 
  mutate(
    pct_on_mastered_prior = if_else(valid_tests >= 30, round((n_on_track + n_mastered)/valid_tests * 100 + 1e-10, 1), NA_real_)
  )

# ELPA Value Add Metric
value_add_elpa <- dist_totals_transition_current %>% 
  left_join(dist_totals_transition_prior %>% select(system, grade, subgroup, pct_on_mastered_prior), by = c('system', 'grade', 'subgroup')) %>% 
  mutate(
    value_add_metric = pct_on_mastered_current - pct_on_mastered_prior
  ) %>% 
  group_by(subgroup) %>% 
  mutate(
    rank = if_else(!is.na(value_add_metric), rank(value_add_metric, ties.method = "max"), NA_integer_),
    denom = sum(!is.na(value_add_metric)),
    # percentile = round(percent_rank(value_add_metric) + 1e-10, 2),
    # quintile_1 = quantile(value_add_metric, probs = 0.20, na.rm = TRUE, names = FALSE),
    # quintile_2 = quantile(value_add_metric, probs = 0.40, na.rm = TRUE, names = FALSE),
    # quintile_3 = quantile(value_add_metric, probs = 0.60, na.rm = TRUE, names = FALSE),
    # quintile_4 = quantile(value_add_metric, probs = 0.80, na.rm = TRUE, names = FALSE),
    # value_add_pathway = case_when(
    #   percentile >= 0.8 ~ 4,
    #   percentile >= 0.6 ~ 3,
    #   percentile >= 0.4 ~ 2,
    #   percentile >= 0.2 ~ 1,
    #   percentile < 0.2 ~ 0,
    #   TRUE ~ NA_real_
    # )
    # value_add_pathway = case_when(
    #   value_add_metric >= quintile_4 ~ 4,
    #   value_add_metric >= quintile_3 ~ 3,
    #   value_add_metric >= quintile_2 ~ 2,
    #   value_add_metric >= quintile_1 ~ 1,
    #   value_add_metric < quintile_1 ~ 0,
    #   TRUE ~ NA_real_
    # )
    value_add_pathway = case_when(
      rank/denom >= 0.8 ~ 4L,
      rank/denom >= 0.6 ~ 3L,
      rank/denom >= 0.4 ~ 2L,
      rank/denom >= 0.2 ~ 1L,
      rank/denom < 0.2 ~ 0L
    )
  ) %>% 
  ungroup() %>% 
  # select(-c(quintile_1:quintile_4)) %>% 
  # select(-percentile) %>% 
  transmute(system, subgroup, value_add_metric, value_add_pathway) %>% # grade, indicator = 'ELPA',
  arrange(subgroup, system)


# =========================== Write csv ================================
write_csv(value_add_elpa, "N:/ORP_accountability/data/2019_final_accountability_files/district_elpa_va_AM.csv")


# ========================== Compare Files ============================
alex_elpa_va <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_elpa_va.csv")

elpa_diff_df <- setdiff(value_add_elpa, alex_elpa_va) %>% 
  bind_rows(setdiff(alex_elpa_va, value_add_elpa)) %>% 
  arrange(system, subgroup)




















