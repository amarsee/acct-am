library(readxl)
library(tidyverse)

student_level <- read_csv("N://ORP_accountability/projects/2020_student_level_file/2020_student_level_file.csv")
# student_level$grade[student_level$grade == 0 | ] <- "Missing Grade"
student_level <- student_level %>% 
  mutate(
    grade = as.character(grade),
    below = case_when(
      performance_level == "Below" | performance_level == "Below Basic" ~ 1,
      TRUE                      ~ 0
    ),
    approaching = case_when(
      performance_level == "Approaching" | performance_level == "Basic" ~ 1,
      TRUE                      ~ 0
    ),
    on_track = case_when(
      performance_level == "Proficient" | performance_level == "On Track" ~ 1,
      TRUE                      ~ 0
    ),
    mastered = case_when(
      performance_level == "Mastered" | performance_level == "Advanced" ~ 1,
      TRUE                      ~ 0
    ),
    grade = if_else(is.na(grade) | grade == '0', "Missing Grade", grade)
  ) %>% 
  filter(residential_facility == 0 | is.na(residential_facility), grade != 2) %>% 
  rename(subgroup = reported_race)

total_by_subgroup <- function(df) {
  out_df <- df %>% 
    group_by(test, original_subject, grade, subgroup) %>% 
    summarise(
      enrolled = sum(enrolled),
      tested = sum(tested),
      valid_tests = sum( valid_test),
      n_below = sum(below),
      n_approaching = sum(approaching),
      n_on_track = sum(on_track),
      n_mastered = sum(mastered)
    ) %>% 
    ungroup()
  return(out_df)
}

grouped_by_race <- total_by_subgroup(student_level)

all_students <- student_level %>% 
  mutate(subgroup = "All Students") %>% 
  total_by_subgroup()
all_students_all_grades <- student_level %>% 
  mutate(subgroup = "All Students", grade = "All Grades") %>%
  total_by_subgroup()

super_subgroup <- student_level %>% 
  filter(bhn_group > 0 | economically_disadvantaged > 0 | t1234 > 0 | el > 0 | special_ed > 0) %>% 
  mutate(subgroup = "Super Subgroup") %>% 
  total_by_subgroup()


cat_subgroups <- function(student_df, students_grouped ){
  base_df = students_grouped
  subgroups <- c("Black/Hispanic/Native American", "Economically Disadvantaged", "English Learners", "English Learner Transitional 1-4", 
                 "English Learners with Transitional 1-4", "Students with Disabilities", 'Female', 'Migrant', 'Gifted')
  for (subgroup in subgroups){
    if (subgroup == "Black/Hispanic/Native American"){
      hist_df <- student_df %>% 
        filter(bhn_group > 0) %>% 
        mutate(subgroup = "Black/Hispanic/Native American")
      non_hist_df <- student_df %>% 
        filter(bhn_group == 0 | is.na(bhn_group))%>% 
        mutate(subgroup = "Non-Black/Hispanic/Native American")
    } else if (subgroup == "Economically Disadvantaged") {
      hist_df <- student_df %>% 
        filter(economically_disadvantaged > 0) %>% 
        mutate(subgroup = "Economically Disadvantaged")
      non_hist_df <- student_df %>% 
        filter(economically_disadvantaged == 0 | is.na(economically_disadvantaged)) %>% 
        mutate(subgroup = "Non-Economically Disadvantaged")
    }else if (subgroup == "English Learners") {
      hist_df <- student_df %>% 
        filter(el > 0) %>% 
        mutate(subgroup = "English Learners")
      non_hist_df <- 0
    }else if (subgroup == "English Learner Transitional 1-4") {
      hist_df <- student_df %>% 
        filter(t1234 > 0) %>% 
        mutate(subgroup = "English Learner Transitional 1-4")
      non_hist_df <- 0
    }else if (subgroup == "English Learners with Transitional 1-4") {
      hist_df <- student_df %>% 
        filter(t1234 > 0 | el > 0) %>% 
        mutate(subgroup = "English Learners with Transitional 1-4")
      non_hist_df <- student_df %>% 
        filter((t1234 == 0 | is.na(t1234)) & (el == 0 | is.na(el)))%>% 
        mutate(subgroup = "Non-English Learners/Transitional 1-4")
    }else if (subgroup == "Female") {
      hist_df <- student_df %>% 
        filter(gender == 'F') %>% 
        mutate(subgroup = "Female")
      non_hist_df <- student_df %>% 
        filter(gender == 'M')%>% 
        mutate(subgroup = "Male")
    }else if (subgroup == "Gifted") {
      hist_df <- student_df %>% 
        filter(gifted == 1) %>% 
        mutate(subgroup = "Gifted")
      non_hist_df <- 0
    }else if (subgroup == "Migrant") {
      hist_df <- student_df %>% 
        filter(migrant == 1) %>% 
        mutate(subgroup = "Migrant")
      non_hist_df <- 0
    }else {
      hist_df <- student_df %>% 
        filter(special_ed > 0) %>% 
        mutate(subgroup = "Students with Disabilities")
      non_hist_df <- student_df %>% 
        filter(special_ed == 0 | is.na(special_ed))%>% 
        mutate(subgroup = "Non-Students with Disabilities")
    }
    hist_grouped <- total_by_subgroup(hist_df)
    base_df <- rbind(base_df, hist_grouped)
    if (is.data.frame(non_hist_df)){
      non_hist_grouped <- total_by_subgroup(non_hist_df)
      base_df <- rbind(base_df, non_hist_grouped)
    }
  }
  return(base_df)
}

all_grades <- cat_subgroups(student_level, grouped_by_race) %>% 
  rbind(super_subgroup) %>%
  mutate(grade = "All Grades") %>% 
  group_by(test, original_subject, grade, subgroup) %>% 
  summarise(
    enrolled = sum(enrolled),
    tested = sum(tested),
    valid_tests = sum( valid_tests),
    n_below = sum(n_below),
    n_approaching = sum(n_approaching),
    n_on_track = sum(n_on_track),
    n_mastered = sum(n_mastered)
  ) %>% 
  ungroup() %>% 
  filter(subgroup != "Unknown")

state_totals <- cat_subgroups(student_level, grouped_by_race) %>% 
  filter(subgroup != "Unknown") %>% 
  rbind(all_students, super_subgroup, all_grades, all_students_all_grades) %>% 
  arrange(original_subject, grade, subgroup)

state_assessment_prior <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/state_assessment_file.csv")

state_assessment <- state_totals %>% 
  mutate(
    pct_mastered = round((n_mastered/valid_tests) * 100 + 1e-10, 1),
    pct_on_mastered = round(((n_on_track + n_mastered)/valid_tests) * 100 + 1e-10, 1),
    pct_on_track = ifelse(n_approaching == 0 & n_below == 0, 100 - pct_mastered, round((n_on_track/valid_tests) * 100 + 1e-10, 1)),
    pct_approaching = ifelse(n_below == 0, 100 - (pct_on_track + pct_mastered), round((n_approaching/valid_tests) * 100 + 1e-10, 1)),
    pct_below = 100 - (pct_mastered + pct_on_track + pct_approaching),
    year = 2020,
    system = 0,
    system_name = "State of Tennessee",
    subgroup = case_when(
      subgroup == 'American Indian/Alaska Native' ~ "American Indian or Alaska Native",
      subgroup == "Hispanic/Latino" ~ "Hispanic",
      subgroup == "Native Hawaiian/Pac. Islander" ~ "Native Hawaiian or Other Pacific Islander",
      TRUE ~ subgroup
    ),
    test = if_else(test == "Alt-Science/Social Studies" | test == "MSAA", "MSAA/Alt-Social Studies", test)
  ) %>% 
  rename(subject = original_subject) %>% 
  bind_rows(state_assessment_prior %>% filter(year %in% c(2019, 2018), subject %in% unique(state_totals$original_subject))) %>% 
  filter(!(grade %in% c(3,4,5) & subject == 'Social Studies')) %>% 
  select(year:system_name, test:n_mastered, pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>% 
  arrange(system, test, subject, grade, subgroup, -year)

# Write csv
write_csv(state_assessment, "N:/ORP_accountability/data/2020_final_accountability_files/state_assessment_file.csv")

# ========================= Compare Files ====================================
state_assessment_comp <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/state_assessment_file.csv")

diff_df <- state_assessment %>% 
  setdiff(state_assessment_comp) %>% 
  bind_rows(setdiff(state_assessment_comp, state_assessment)) %>% 
  arrange(system, test, subject, grade, subgroup, -year)

diff_non_pcts <- state_assessment %>% 
  select(year:n_mastered) %>% 
  setdiff(state_assessment_comp %>% select(year:n_mastered)) %>% 
  bind_rows(setdiff(state_assessment_comp %>% select(year:n_mastered), state_assessment %>% select(year:n_mastered))) %>% 
  arrange(system, test, subject, grade, subgroup, -year)




