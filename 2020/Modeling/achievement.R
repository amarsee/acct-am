# Achievement Modeling
# 3 Methods for the Achievement Indicator
# 1. Current method (AMO and Absolute Pathway)
# 2. 2 Year average with same rules applied as now
# 3. 3 Year average with same rules applied as now
# 4. Weighted Proficiency Rate and ranking schools, assigning score by percentile

library(tidyverse)
library(acct)
library(andrewacct)
library(ggplot2)
library(scales)
library(lemon)
library(networkD3)

# ============== Current Method =======================
ach_current <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv") %>% 
  filter(indicator == 'Achievement')


# =================== 2 year average ====================
ach_2_year_avg <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv") %>% 
  filter(indicator == 'Achievement') %>% 
  mutate(
    metric_2_year_avg = case_when(
      !is.na(metric) & !is.na(metric_prior) ~ round((metric + metric_prior)/2 + 1e-10, 1),
      !is.na(metric) ~ metric,
      TRUE ~ NA_real_
    ),
    score = case_when(
      metric_2_year_avg >= 45 ~ 4,
      metric_2_year_avg >= 35 ~ 3,
      metric_2_year_avg >= 27.5 ~ 2,
      metric_2_year_avg >= 20 ~ 1,
      metric_2_year_avg >= 0 ~ 0,
      TRUE ~ NA_real_
    )
  )


# =================== 3 year average ====================
ach_3_year_avg <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv") %>% 
  filter(indicator == 'Achievement') %>% 
  left_join(
    read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2018_school_accountability_file.csv") %>% 
      select(system, school, indicator, subgroup, metric_2_years_ago = metric_prior),
    by = c('system', 'school', 'indicator', 'subgroup')
  ) %>% 
  mutate(
    metric_3_year_avg = case_when(
      !is.na(metric) & !is.na(metric_prior) & !is.na(metric_2_years_ago) ~ round((metric + metric_prior + metric_2_years_ago)/3 + 1e-10, 1),
      !is.na(metric) & !is.na(metric_prior) ~ round((metric + metric_prior)/2 + 1e-10, 1),
      !is.na(metric) ~ metric,
      TRUE ~ NA_real_
    ),
    score = case_when(
      metric_3_year_avg >= 45 ~ 4,
      metric_3_year_avg >= 35 ~ 3,
      metric_3_year_avg >= 27.5 ~ 2,
      metric_3_year_avg >= 20 ~ 1,
      metric_3_year_avg >= 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

# ============= Weighted Profiency Rate ================
math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

student_level <- read_csv("N://ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv")

sl <- student_level %>%
  filter(!(system == 964 & school == 964 | system == 970 & school == 970)) %>%
  mutate_at("residential_facility", ~ if_else(is.na(.), 0, .)) %>%
  mutate_at("enrolled_50_pct_school", ~ if_else(is.na(.), "Y", .)) %>% 
  mutate(
    original_subject = case_when(
      grade < 9 & original_subject %in% c('Algebra I', 'Algebra II', "Geometry", "Integrated Math I", "Integrated Math II",
                                          "Integrated Math III", 'English I', 'English II', 'Biology I', 'Chemistry') ~ subject,
      TRUE ~ original_subject
    )
  )

integrated_math <- student_level %>% 
  filter(original_subject %in% c("Algebra I", "Integrated Math I")) %>% 
  count(system, original_subject) %>% 
  group_by(system) %>% 
  mutate(temp = max(n)) %>% 
  # Systems where Integrated Math is the max between that and Algebra I
  filter(n == temp, original_subject == "Integrated Math I")
# Vector with the sytems where that is the case
int_math_vec <- integrated_math[['system']]

act_sub <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_school.csv") %>% 
  #left_join(school_df, by = c('system', 'school')) %>% 
  transmute(system, system_name, school, school_name,
            subject = case_when(
              subject == "ACT Math" & system %in% int_math_vec ~ "Integrated Math III",
              TRUE  ~ "Algebra II"
            ), grade = 11, subgroup = "All Students", valid_test = valid_tests, on_track = n_met_benchmark, mastered = 0
  ) 

sl <- sl %>% 
  mutate(
    approaching = case_when(
      performance_level == "Approaching" ~ 1,
      TRUE                      ~ 0
    ),
    on_track = case_when(
      performance_level == "Proficient" | performance_level == "On Track" ~ 1,
      TRUE                      ~ 0
    ),
    mastered = case_when(
      performance_level == "Mastered" | performance_level == "Advanced" ~ 1,
      TRUE                      ~ 0
    )
  ) %>% 
  filter(residential_facility == 0, (enrolled_50_pct_school == 'Y' | (acct_system != system | school != acct_school)),  # homebound == 0, !is.na(state_student_id),grade %in% 3:12, 
         original_subject %in% c("Math", "ELA", math_eoc, english_eoc)) %>% 
  
  # fill(system_name) %>% 
  rename(subgroup = reported_race)

total_by_subgroup <- function(df) {
  out_df <- df %>% 
    group_by(acct_system, acct_school, subject, subgroup) %>% 
    summarise(
      enrolled = sum(enrolled, na.rm = TRUE),
      tested = sum(tested, na.rm = TRUE),
      valid_tests = sum( valid_test),
      n_approaching = sum(approaching),
      n_on_track = sum(on_track),
      n_mastered = sum(mastered)
    ) %>% 
    ungroup() %>% 
    rename(system = acct_system, school = acct_school)
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

grouped_by_race <- total_by_subgroup(sl)

all_students <- sl %>% 
  bind_rows(act_sub %>% rename(acct_system = system, acct_school = school)) %>% 
  mutate(subgroup = "All Students") %>% 
  total_by_subgroup()

super_subgroup <- sl %>% 
  filter(bhn_group > 0 | economically_disadvantaged > 0 | t1234 > 0 | el > 0 | special_ed > 0) %>% 
  mutate(subgroup = "Super Subgroup") %>% 
  total_by_subgroup()

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
    hist_grouped <- total_by_subgroup(hist_df)
    base_df <- rbind(base_df, hist_grouped)
  }
  return(base_df)
}

state_totals <- cat_subgroups(sl, grouped_by_race) %>% 
  filter(subgroup != "Unknown") %>% 
  #rename(system = acct_system, school = acct_school) %>% 
  rbind(all_students, super_subgroup) %>% 
  arrange(system, school, subject, subgroup) %>% 
  # rename(subject = original_subject) %>% 
  mutate(
    subgroup = case_when(
      subgroup == "American Indian/Alaska Native" ~ "American Indian or Alaska Native",
      subgroup == "Hispanic/Latino" ~ "Hispanic",
      subgroup == "Native Hawaiian/Pac. Islander" ~ "Native Hawaiian or Other Pacific Islander",
      TRUE ~ subgroup
    )
  )


school_achievement <- state_totals %>% 
  mutate(
    subject = case_when(
      subject %in% english_eoc ~ "HS English",
      subject %in% c(math_eoc, "HS Math") ~ "HS Math",
      TRUE ~ subject
    )
  ) %>% 
  group_by(system, school, subject, subgroup)  %>%
  summarise(
    enrolled = sum(enrolled, na.rm = TRUE),
    tested = sum(tested, na.rm = TRUE),
    valid_tests = sum(valid_tests, na.rm = TRUE),
    n_approaching = sum(n_approaching, na.rm = TRUE),
    n_on_track = sum(n_on_track, na.rm = TRUE),
    n_mastered = sum(n_mastered, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    valid_tests = if_else(valid_tests > 29, valid_tests, 0),
    n_approaching = if_else(valid_tests > 29, n_approaching, 0),
    n_on_track = if_else(valid_tests > 29, n_on_track, 0),
    n_mastered = if_else(valid_tests > 29, n_mastered, 0)
  ) %>% 
  group_by(system, school, subgroup)  %>%
  summarise(
    participation_rate = round5(100 * sum(tested)/sum(enrolled)),
    n_count = sum(valid_tests),
    success_rate = round(((sum(n_approaching) + 2*sum(n_on_track) + 2.5*sum(n_mastered))/sum(valid_tests)) * 100 + 1e-10, 1)
  ) %>% 
  ungroup() %>% 
  mutate(success_rate = ifelse(is.na(success_rate), NA, success_rate))

ach_weighted_avg <- school_achievement %>% 
  filter(!is.na(success_rate)) %>% 
  group_by(subgroup) %>% 
  mutate(
    denom = n(),
    rank = rank(success_rate, ties.method = 'max'),
    ach_percentile = round(rank/denom * 100 + 1e-10, 1)
  ) %>% 
  ungroup() %>% 
  mutate(
    score = case_when(
      ach_percentile >= 85 ~ 4,
      ach_percentile >= 60 ~ 3,
      ach_percentile >= 25 ~ 2,
      ach_percentile >= 5 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  left_join(
    ach_current %>% select(system:indicator) %>% distinct(),
    by = c('system', 'school')
  )

# ================== Overall Ach =================
ach_ovr_score <- bind_rows(
  ach_current %>% filter(!is.na(score)) %>% mutate(method = 'Current'),
  ach_2_year_avg %>% filter(!is.na(score)) %>% mutate(method = '2 Year Avg.'),
  ach_3_year_avg %>% filter(!is.na(score)) %>% mutate(method = '3 Year Avg.'),
  ach_weighted_avg %>% filter(!is.na(score)) %>% mutate(method = 'Weighted Average')
) %>% 
  filter(subgroup %in% c('All Students', 'Black/Hispanic/Native American', 'Economically Disadvantaged',
                         'English Learners with Transitional 1-4', 'Students with Disabilities')) %>% 
  mutate(
    subgroup = case_when(
      subgroup %in% c('Black/Hispanic/Native American', 'Economically Disadvantaged',
                      'English Learners with Transitional 1-4', 'Students with Disabilities') ~ 'Historically Underserved',
      TRUE ~ subgroup
    )
  ) %>% 
  group_by(system, system_name, school, school_name, pool, designation_ineligible, indicator, method, subgroup) %>% 
  summarise(
    subgroup_score = mean(score)
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    names_from = subgroup,
    values_from = subgroup_score
  ) %>% 
  janitor::clean_names() %>% 
  mutate(
    ovr_ach_score = if_else(!is.na(historically_underserved), round((all_students * 0.6) + (historically_underserved * 0.4) + 1e-10, 1), all_students),
    grade = case_when(
      ovr_ach_score >= 3.1 ~ 'A',
      ovr_ach_score >= 2.1 ~ 'B',
      ovr_ach_score >= 1.1 ~ 'C',
      ovr_ach_score >= 0 ~ 'D',
      # ovr_ach_score == 0 ~ 'F'
      TRUE ~ NA_character_
    )
  )

ovr_score_dist <- ach_ovr_score %>% 
  group_by(method, grade) %>% 
  summarise(n_schools = n()) %>% 
  ungroup() %>% 
  group_by(method) %>% 
  mutate(denom = sum(n_schools)) %>% 
  ungroup() %>% 
  mutate(
    pct_schools = round(n_schools / denom * 100 + 1e-10, 0)
  )
# ============= Change of grade from current =================
grade_change <- ach_ovr_score %>% filter(method == 'Current') %>% select(system:indicator, grade_current = grade) %>% 
  left_join(
    ach_ovr_score %>% filter(method == '2 Year Avg.') %>% select(system, school, grade_2_year_avg = grade),
    by = c('system', 'school')
  ) %>% 
  left_join(
    ach_ovr_score %>% filter(method == '3 Year Avg.') %>% select(system, school, grade_3_year_avg = grade),
    by = c('system', 'school')
  ) %>% 
  left_join(
    ach_ovr_score %>% filter(method == 'Weighted Average') %>% select(system, school, grade_weighted_avg = grade),
    by = c('system', 'school')
  )

grade_pct_move <- grade_change %>% 
  group_by(indicator, grade_current, grade_2_year_avg) %>% 
  summarise(
    n_count = n()
  ) %>% 
  ungroup() %>% 
  transmute(indicator, grade_current, method = '2 Year Avg', 
            new_grade = grade_2_year_avg, n_count) %>% 
  bind_rows(
    grade_change %>% 
      group_by(indicator, grade_current, grade_3_year_avg) %>% 
      summarise(
        n_count = n()
      ) %>% 
      ungroup() %>% 
      transmute(indicator, grade_current, method = '3 Year Avg', 
                new_grade = grade_3_year_avg, n_count),
    grade_change %>% 
      group_by(indicator, grade_current, grade_weighted_avg) %>% 
      summarise(
        n_count = n()
      ) %>% 
      ungroup() %>% 
      transmute(indicator, grade_current, method = 'Weighted Avg', 
                new_grade = grade_weighted_avg, n_count)
  ) %>% 
  group_by(grade_current, method) %>% 
  mutate(
    grade_current_count = sum(n_count)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_schools = round(n_count / grade_current_count * 100 + 1e-10, 1)
  )

grade_move_sankey <- grade_pct_move %>% 
  mutate(
    method_starting = paste0('Starting ', method, ' - ', grade_current),
    method_final = paste0('Final ', method, ' - ', new_grade)
  )
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(grade_move_sankey$method_starting), as.character(grade_move_sankey$method_final)) %>% unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
grade_move_sankey$IDsource=match(grade_move_sankey$method_starting, nodes$name)-1 
grade_move_sankey$IDtarget=match(grade_move_sankey$method_final, nodes$name)-1

# Make the Network
sankeyNetwork(Links = grade_move_sankey, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "n_count", NodeID = "name", 
              sinksRight=FALSE, nodeWidth=40, fontSize=13, nodePadding=20)
# =============== Plots =======================
score_dist <- bind_rows(
  ach_current %>% filter(!is.na(score)) %>% mutate(method = 'Current'),
  ach_2_year_avg %>% filter(!is.na(score)) %>% mutate(method = '2 Year Avg.'),
  ach_3_year_avg %>% filter(!is.na(score)) %>% mutate(method = '3 Year Avg.'),
  ach_weighted_avg %>% filter(!is.na(score)) %>% mutate(method = 'Weighted Average')
) %>% 
  mutate(
    grade = case_when(
      score == 4 ~ 'A',
      score == 3 ~ 'B',
      score == 2 ~ 'C',
      score == 1 ~ 'D',
      score == 0 ~ 'F'
    )
  ) %>% 
  group_by(method, subgroup, grade) %>% 
  summarise(
    n_schools = n()
  ) %>% 
  group_by(method, subgroup) %>% 
  mutate(denom = sum(n_schools)) %>% 
  ungroup() %>% 
  mutate(
      pct_schools = round(n_schools / denom * 100 + 1e-10)
  ) %>% 
  mutate(
    subgroup = case_when(
      subgroup == 'All Students' ~ 'All',
      subgroup == 'Black/Hispanic/Native American' ~ 'BHN',
      subgroup == 'Economically Disadvantaged' ~ 'ED',
      subgroup == 'English Learners with Transitional 1-4' ~ 'EL',
      subgroup == 'Students with Disabilities' ~ 'SWD',
      TRUE ~ subgroup
    )
  )



score_dist %>%
  filter(subgroup %in% c('All', 'BHN', 'ED', 'EL', 'SWD')) %>% 
  ggplot(aes(x = factor(grade), y = pct_schools, 
             label = paste0(pct_schools) ,fill = factor(method, levels = c('Current', '2 Year Avg.', '3 Year Avg.', 'Weighted Average')))) +
  geom_bar(stat='identity', position = position_dodge(width=0.9), width = 0.85) +
  geom_text(size = 3, position = position_dodge(width = 0.90), aes(vjust=-0.25)) +
  labs(x = "Grade", y = "Percentage of Eligible Schools", title = "Achievement Indicator Comparison",
       fill = "") +
  scale_y_continuous(breaks = c(0,20,40, 60, 80, 100), limits=c(0, 100)) +
  scale_fill_tdoe() +
  theme_bw() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = 'gray'),
        text = element_text(size=12), legend.position="bottom") +
  # theme(text = element_text(size = 12)) +
  facet_rep_wrap(~ subgroup, nrow = 2, repeat.tick.labels = 'x')



ovr_score_dist %>%
  ggplot(aes(x = factor(grade), y = pct_schools, 
             label = paste0(pct_schools) ,fill = factor(method, levels = c('Current', '2 Year Avg.', '3 Year Avg.', 'Weighted Average')))) +
  geom_bar(stat='identity', position = position_dodge(width=0.9), width = 0.85) +
  geom_text(size = 3, position = position_dodge(width = 0.90), aes(vjust=-0.25)) +
  labs(x = "Grade", y = "Percentage of Eligible Schools", title = "Achievement Indicator Comparison",
       fill = "") +
  scale_y_continuous(breaks = c(0,20,40, 60, 80, 100), limits=c(0, 100)) +
  scale_fill_tdoe() +
  theme_bw() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = 'gray'),
        text = element_text(size=12), legend.position="bottom")
  # theme(text = element_text(size = 12)) +

ovr_score_dist %>%
  ggplot(aes(x = factor(method, levels = c('Current', '2 Year Avg.', '3 Year Avg.', 'Weighted Average')), y = pct_schools, 
             label = paste0(pct_schools, '%') ,fill =  factor(grade))) +
  geom_bar(stat='identity', position = position_dodge(width=0.9), width = 0.85) +
  geom_text(size = 3.5, position = position_dodge(width = 0.90), aes(vjust=-0.25)) +
  labs(x = "Method", y = "Percentage of Eligible Schools", title = "Achievement Indicator Comparison",
       fill = "") +
  scale_y_continuous(breaks = c(0,20,40, 60, 80, 100), limits=c(0, 100)) +
  # scale_fill_tdoe() +
  scale_fill_manual(values = c("A" = "#3FD222", "B" = "#ADF591", "C" = "#F3FC2F", "D" = "#FF9F9F")) +
  theme_bw() +
  theme(panel.grid.major.y = element_line(size = 0.5, color = 'gray'),
        text = element_text(size=12), legend.position="bottom")


