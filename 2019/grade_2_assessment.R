library(readxl)
library(tidyverse)

# cte_alt_adult <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/cte_alt_adult_schools.csv")
# closed_schools <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/closed_schools.csv")
# sped_schools <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/sped_schools.csv")
# new_schools <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/new_schools.csv")

# ================================= School Level ========================================================

student_level <- read_csv("N://ORP_accountability/projects/2019_grade_2_assessment/grade_2_student_level_AM.csv") %>% 
  mutate(
    grade = if_else(grade == 0, "Missing Grade", as.character(grade))
  )

names <- read_csv("N:\\ORP_accountability\\data\\2019_final_accountability_files\\names.csv")

sl <- student_level %>% 
  # select(-system, - school) %>% 
  # rename(system = acct_system, school = acct_school) %>% 
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
  filter(residential_facility == 0 | is.na(residential_facility)) %>% # , homebound == 0) %>% 
  # fill(system_name) %>% 
  rename(subgroup = reported_race)

total_by_subgroup <- function(df) {
  out_df <- df %>% 
    group_by(system, school, test, original_subject, grade, subgroup) %>% 
    summarise(
      enrolled = sum(enrolled),
      tested = sum(tested),
      valid_tests = sum(valid_test),
      n_below = sum(below),
      n_approaching = sum(approaching),
      n_on_track = sum(on_track),
      n_mastered = sum(mastered)
    ) %>% 
    ungroup()
  return(out_df)
}

cat_subgroups <- function(student_df, students_grouped ){
  base_df = students_grouped
  subgroups <- c("Black/Hispanic/Native American", "Economically Disadvantaged","English Learners", "English Learner Transitional 1-4", 
                 "English Learners with Transitional 1-4", "Students with Disabilities", 'Female', 'Gifted', 'Migrant')
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

state_totals <- cat_subgroups(sl, total_by_subgroup(sl)) %>% 
  filter(subgroup != "Unknown") %>% 
  rbind(sl %>% 
          mutate(subgroup = "All Students") %>% 
          total_by_subgroup(), 
        sl %>% 
          filter(bhn_group > 0 | economically_disadvantaged > 0 | t1234 > 0 | el > 0 | special_ed > 0) %>% 
          mutate(subgroup = "Super Subgroup") %>% 
          total_by_subgroup()) %>%  
        # cat_subgroups(sl, total_by_subgroup(sl)) %>% 
        #   rbind(super_subgroup) %>%
        #   mutate(grade = "All Grades") %>% 
        #   group_by(system, school, test, original_subject, grade, subgroup) %>% 
        #   summarise(
        #     enrolled = sum(enrolled),
        #     tested = sum(tested),
        #     valid_tests = sum( valid_tests),
        #     n_below = sum(n_below),
        #     n_approaching = sum(n_approaching),
        #     n_on_track = sum(n_on_track),
        #     n_mastered = sum(n_mastered)
        #   ) %>% 
        #   ungroup() %>% 
        #   filter(subgroup != "Unknown"), 
        # sl %>% 
        #   mutate(subgroup = "All Students", grade = "All Grades") %>%
        #   total_by_subgroup()) %>% 
  arrange(system, original_subject, grade, subgroup)


school_assessment <- state_totals %>% 
  # select(-school_name, -system_name) %>% 
  left_join(names, by = c('system', 'school')) %>% 
  mutate(
    pct_mastered = round((n_mastered/valid_tests) * 100 + 1e-10, 1),
    pct_on_mastered = round(((n_on_track + n_mastered)/valid_tests) * 100 + 1e-10, 1),
    pct_on_track = ifelse(n_approaching == 0 & n_below == 0, 100 - pct_mastered, round((n_on_track/valid_tests) * 100 + 1e-10, 1)),
    pct_approaching = ifelse(n_below == 0, 100 - (pct_on_track + pct_mastered), round((n_approaching/valid_tests) * 100 + 1e-10, 1)),
    pct_below = 100 - (pct_mastered + pct_on_track + pct_approaching),
    year = 2019) %>% 
  rename(subject = original_subject) %>% 
  select(system, system_name, school, school_name, everything())# %>% 
# filter(!school_name %in% cte_alt_adult$SCHOOL, !school_name %in% closed_schools$SCHOOL_NAME)

school_assessment_previous <- read_csv("N:/ORP_accountability/projects/2018_grade_2_assessment/2018_grade_2_school_level_file.csv") %>% 
  mutate(
    test = 'Grade 2',
    grade = '2'
  )

school_assessment_final <- school_assessment %>% 
  mutate(
    system_name = case_when(
      system == 964 & school == 964 ~ "Tennessee School for the Deaf",
      system == 970 & school == 970 ~ "Department Of Children's Services Education Division",
      TRUE ~ system_name
    ),
    subgroup = case_when(
      subgroup == "American Indian/Alaska Native" ~ "American Indian or Alaska Native",
      subgroup == "Hispanic/Latino" ~ "Hispanic",
      subgroup == "Native Hawaiian/Pac. Islander" ~ "Native Hawaiian or Other Pacific Islander",
      TRUE ~ subgroup
    ),
    test = case_when(
      test == "Alt-Science/Social Studies" | test == "MSAA" ~ "MSAA/Alt-Social Studies",
      TRUE ~ test
    )
  ) %>% 
  bind_rows(school_assessment_previous %>% filter(year %in% c(2018, 2017), subject %in% unique(school_assessment$subject))) %>% 
  select(year, system:n_mastered, pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>% 
  arrange(system, school, test, subject, grade, subgroup, -year)

# Write csv
write_csv(school_assessment_final, "N:/ORP_accountability/projects/2019_grade_2_assessment/grade_2_school_level_file_AM.csv", na = '')



# ====================================  District Level =======================================
names <- read_csv("N:\\ORP_accountability\\data\\2019_final_accountability_files\\names.csv") %>% 
  select(system, system_name) %>% 
  distinct()

sl <- student_level %>% 
  # select(-system, - school) %>% 
  # rename(system = acct_system, school = acct_school) %>% 
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
  filter(residential_facility == 0 | is.na(residential_facility)) %>% 
  # fill(system_name) %>% 
  rename(subgroup = reported_race)

total_by_subgroup <- function(df) {
  out_df <- df %>%
    group_by(system, system_name, test, original_subject, grade, subgroup) %>%
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

# Function to iterate over desired subgroups and outputs a df with all subgroups
cat_subgroups <- function(student_df, students_grouped ){
  base_df = students_grouped
  # subgroups to be created
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

state_totals <- cat_subgroups(sl, total_by_subgroup(sl)) %>% 
  filter(subgroup != "Unknown") %>% 
  rbind(sl %>% 
          mutate(subgroup = "All Students") %>% 
          total_by_subgroup(), 
        sl %>% 
          filter(bhn_group > 0 | economically_disadvantaged > 0 | t1234 > 0 | el > 0 | special_ed > 0) %>% 
          mutate(subgroup = "Super Subgroup") %>% 
          total_by_subgroup()
  ) %>% 
  arrange(system, original_subject, grade, subgroup)

district_assessment <- state_totals %>% 
  select(-system_name) %>% 
  left_join(names, by = c('system')) %>% 
  mutate(
    pct_mastered = round((n_mastered/valid_tests) * 100 + 1e-10, 1),
    pct_on_mastered = round(((n_on_track + n_mastered)/valid_tests) * 100 + 1e-10, 1),
    pct_on_track = ifelse(n_approaching == 0 & n_below == 0, 100 - pct_mastered, round((n_on_track/valid_tests) * 100 + 1e-10, 1)),
    pct_approaching = ifelse(n_below == 0, 100 - (pct_on_track + pct_mastered), round((n_approaching/valid_tests) * 100 + 1e-10, 1)),
    pct_below = 100 - (pct_mastered + pct_on_track + pct_approaching),
    year = 2019) %>% 
  select(system, system_name, everything()) %>% 
  rename(subject = original_subject)

district_assessment_previous <- read_csv("N:/ORP_accountability/projects/2018_grade_2_assessment/2018_grade_2_district_level_file.csv") %>% 
  mutate(
    test = 'Grade 2',
    grade = '2'
  )

district_assessment_final <- district_assessment %>% 
  mutate(
    subgroup = case_when(
      subgroup == "American Indian/Alaska Native" ~ "American Indian or Alaska Native",
      subgroup == "Hispanic/Latino" ~ "Hispanic",
      subgroup == "Native Hawaiian/Pac. Islander" ~ "Native Hawaiian or Other Pacific Islander",
      TRUE ~ subgroup
    )
  ) %>% 
  bind_rows(district_assessment_previous %>% filter(year %in% c(2018, 2017), subject %in% unique(district_assessment$subject))) %>% 
  select(year, system:n_mastered, pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>% 
  arrange(system, test, subject, grade, subgroup, -year)

# Write csv
write_csv(district_assessment_final, "N:/ORP_accountability/projects/2019_grade_2_assessment/grade_2_district_level_file_AM.csv", na = '')








