library(tidyverse)
library(janitor)
library(readstata13)
library(readxl)
# library(lubridate)
library(haven)
# library(RJDBC)
# setwd(str_c("N:/ORP_accountability/projects/", year(today()), "_ready_graduate/"))
# 
# # ============================== Pull In Data =============================
# # Data
# con = dbConnect(
#   JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA20593/Downloads/ojdbc6.jar"), 
#   readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
#   "EIS_MGR",
#   readRegistry("Environment", "HCU")$EIS_MGR_PWD
# ) 
# # SQL Pull
# rg = as.tbl(
#   dbGetQuery(
#     con, 
#     "select * 
#     from student_ready_grads"
#   )
#   ) %>% 
#   janitor::clean_names() 
# 
# ready_grad_student <- rg %>% 
#   select(
#     student_key:ncrc_work_keys, participate_clg_lvl_pgm, n_cambridge,
#     n_ap = n_adv_placement, n_ib = n_inter_baccalaureate, n_sdc = n_statewide_dual_credit,
#     n_ldc = n_local_dual_credit, n_de = n_dual_enrollment, ready_graduate
#   ) %>% 
#   mutate_at(
#     .vars = c('student_key', 'cohortyear', 'district_no', 'school_no', 'completion_type', 'sat_math', 'sat_critical_reading',
#               'sat_total', 'act_english', 'act_math', 'act_reading', 'act_science', 'act_composite', 'industry_cert_earned',
#               'asvab', 'ncrc_work_keys', 'participate_clg_lvl_pgm', 'n_cambridge', 'n_ap', 'n_ib', 'n_sdc', 'n_ldc', 'n_de'),
#     .funs = as.integer
#   )
# 
# write_csv(ready_grad_student , 'N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_student_level_06182019.csv')

school_names <- read_csv("N:\\ORP_accountability\\data\\2019_final_accountability_files\\names.csv")

# school_names <- read_excel("N:/ORP_accountability/projects/Andrew/Accountability/2019/2018-19 EDFacts School Master File_4-10-19.xlsx",
#                            sheet = '1819 Sch and Grade Info') %>% 
#   clean_names() %>% 
#   transmute(system = as.integer(dg_4_lea_id_state),
#             school = as.integer(dg_5_school_id_state),
#             system_name = extra_item_lea_name,
#             school_name = dg_7_school_name)

dist_names <- school_names %>% 
  select(system, system_name) %>% 
  unique()

subgroups <- c("American Indian or Alaska Native", "Asian", "Black or African American", "Black/Hispanic/Native American",
               "Economically Disadvantaged", "English Language Learners", "Hispanic", 
               "Native Hawaiian or Other Pacific Islander", "Students with Disabilities", "White")

# =================================== Student Level and filtering duplicates ==========================================

ca_student_level <- read_csv("N:\\ORP_accountability\\data\\2019_chronic_absenteeism\\student_chronic_absenteeism_AM_Jul11.csv") %>% 
  mutate(
    ca_indicator = if_else(absentee_rate >= 10, 1, 0),
    grade_band = case_when(
      grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08") ~ 'K through 8th',
      grade %in% c("09", "10", "11", "12") ~ '9th through 12th'
    )
  )

ca_student_level <- ca_student_level %>% 
  bind_rows(ca_student_level %>% mutate(grade_band = 'All Grades'))

# 
# ready_grad_student_level_total <- ready_grad_student_level_school_act %>% 
#   full_join(ready_grad_student_level_school_sat, by=c('student_key','first_name', 'middle_name', 'last_name', 'race_ethnicity', 'cohortyear',
#                                                       'elb', 'swd', 'econ_dis', 'system', 'school', 'included_in_cohort', 'completion_type', 
#                                                       'sat_math', 'sat_critical_reading', 'sat_total', 'act_english', 'act_math', 'act_reading',
#                                                       'act_science', 'act_composite', 'industry_cert_earned', 'asvab', 'ncrc_work_keys',
#                                                       'n_cambridge', 'n_ap', 'n_ib', 'n_sdc', 'n_ldc', 'n_de', 'participate_clg_lvl_pgm', 'ready_graduate')) %>% 
#   bind_rows(ready_grad_student_level_neither) %>% 
#   mutate(
#     cohort_indicator = if_else(included_in_cohort == 'Y', 1, 0),
#     ready_grad_indicator = if_else(ready_graduate == 'Y', 1, 0),
#     on_time_grad = if_else(completion_type %in% c(1,11,12,13), 1, 0)
#   ) %>% 
#   replace_na(list(completed_act = 0, completed_sat = 0)) %>% 
#   mutate(completed_act_sat = if_else(completed_act > 0 | completed_sat > 0, 1, 0))


# ================================== Ready Grad Subgroups ===========================================================

out_df_ca <- ca_student_level %>% 
  mutate(subgroup = 'All Students')


for (subgroup in subgroups){
  student_df <- ca_student_level
  if (subgroup == "American Indian or Alaska Native"){
    hist_df <- student_df %>% 
      filter(Native == 1) %>% 
      mutate(subgroup = "American Indian or Alaska Native")
    non_hist_df <- 0
  }else if (subgroup == "Asian"){
    hist_df <- student_df %>% 
      filter(Asian == 1) %>% 
      mutate(subgroup = "Asian")
    non_hist_df <- 0
  }else if (subgroup == "Black or African American"){
    hist_df <- student_df %>% 
      filter(Black == 1) %>% 
      mutate(subgroup = "Black or African American")
    non_hist_df <- 0
  }else if (subgroup == "Black/Hispanic/Native American"){
    hist_df <- student_df %>% 
      filter(Black == 1 | Hispanic == 1 | Native == 1) %>% 
      mutate(subgroup = "Black/Hispanic/Native American")
    non_hist_df <- 0
  } else if (subgroup == "Economically Disadvantaged") {
    hist_df <- student_df %>% 
      filter(ED == 1) %>% 
      mutate(subgroup = "Economically Disadvantaged")
    non_hist_df <- 0
  }else if (subgroup == "English Language Learners") {
    hist_df <- student_df %>% 
      filter(EL == 1) %>% 
      mutate(subgroup = "English Learners with Transitional 1-4")
    non_hist_df <- 0
  }else if (subgroup == "Hispanic"){
    hist_df <- student_df %>% 
      filter(Hispanic == 1) %>% 
      mutate(subgroup = "Hispanic")
    non_hist_df <- 0
  }else if (subgroup == "Native Hawaiian or Other Pacific Islander"){
    hist_df <- student_df %>% 
      filter(HPI == 1) %>% 
      mutate(subgroup = "Native Hawaiian or Other Pacific Islander")
    non_hist_df <- 0
  }else if (subgroup == "White"){
    hist_df <- student_df %>% 
      filter(White == 1) %>% 
      mutate(subgroup = "White")
    non_hist_df <- 0
  }else {
    hist_df <- student_df %>% 
      filter(SWD == 1) %>% 
      mutate(subgroup = "Students with Disabilities")
    non_hist_df <- 0
  }
  # hist_grouped <- total_by_subgroup(hist_df)
  out_df_ca <- rbind(out_df_ca, hist_df)
  if (is.data.frame(non_hist_df)){
    #non_hist_grouped <- total_by_subgroup(non_hist_df)
    out_df_ca <- rbind(out_df_ca, non_hist_df)
  }
}


# ===================================== District Level =====================================

district_level_student <- read_csv("N:\\ORP_accountability\\data\\2019_chronic_absenteeism\\student_chronic_absenteeism_AM_Jul11.csv") %>% 
  mutate(
    ca_indicator = if_else(absentee_rate >= 10, 1, 0),
    grade_band = case_when(
      grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08") ~ 'K through 8th',
      grade %in% c("09", "10", "11", "12") ~ '9th through 12th'
    )
  ) %>% 
  # Add up absences and ISP days across every enrollment
  group_by(student_id, system, system_name, grade_band) %>%
  summarise(
    n_absences = sum(n_absences, na.rm = TRUE),
    isp_days = sum(isp_days, na.rm = TRUE),
    instructional_days = max(instructional_calendar_days),
    # All = any(All),
    # BHN = any(BHN, na.rm = TRUE),
    ED = max(ED, na.rm = TRUE),
    SWD = max(SWD, na.rm = TRUE),
    EL = max(EL, na.rm = TRUE),
    Black = max(Black, na.rm = TRUE),
    Hispanic = max(Hispanic, na.rm = TRUE),
    Native = max(Native, na.rm = TRUE),
    HPI = max(HPI, na.rm = TRUE),
    Asian = max(Asian, na.rm = TRUE),
    White = max(White, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(isp_days/instructional_days >= 0.5) %>%
  mutate(chronic_absence = as.integer(n_absences/isp_days >= 0.1))

district_level_student <- district_level_student %>% 
  bind_rows(district_level_student %>% mutate(grade_band = 'All Grades'))

out_df_district_ca <- district_level_student %>% 
  mutate(subgroup = 'All Students')


for (subgroup in subgroups){
  student_df <- district_level_student
  if (subgroup == "American Indian or Alaska Native"){
    hist_df <- student_df %>% 
      filter(Native == 1) %>% 
      mutate(subgroup = "American Indian or Alaska Native")
    non_hist_df <- 0
  }else if (subgroup == "Asian"){
    hist_df <- student_df %>% 
      filter(Asian == 1) %>% 
      mutate(subgroup = "Asian")
    non_hist_df <- 0
  }else if (subgroup == "Black or African American"){
    hist_df <- student_df %>% 
      filter(Black == 1) %>% 
      mutate(subgroup = "Black or African American")
    non_hist_df <- 0
  }else if (subgroup == "Black/Hispanic/Native American"){
    hist_df <- student_df %>% 
      filter(Black == 1 | Hispanic == 1 | Native == 1) %>% 
      mutate(subgroup = "Black/Hispanic/Native American")
    non_hist_df <- 0
  } else if (subgroup == "Economically Disadvantaged") {
    hist_df <- student_df %>% 
      filter(ED == 1) %>% 
      mutate(subgroup = "Economically Disadvantaged")
    non_hist_df <- 0
  }else if (subgroup == "English Language Learners") {
    hist_df <- student_df %>% 
      filter(EL == 1) %>% 
      mutate(subgroup = "English Learners with Transitional 1-4")
    non_hist_df <- 0
  }else if (subgroup == "Hispanic"){
    hist_df <- student_df %>% 
      filter(Hispanic == 1) %>% 
      mutate(subgroup = "Hispanic")
    non_hist_df <- 0
  }else if (subgroup == "Native Hawaiian or Other Pacific Islander"){
    hist_df <- student_df %>% 
      filter(HPI == 1) %>% 
      mutate(subgroup = "Native Hawaiian or Other Pacific Islander")
    non_hist_df <- 0
  }else if (subgroup == "White"){
    hist_df <- student_df %>% 
      filter(White == 1) %>% 
      mutate(subgroup = "White")
    non_hist_df <- 0
  }else {
    hist_df <- student_df %>% 
      filter(SWD == 1) %>% 
      mutate(subgroup = "Students with Disabilities")
    non_hist_df <- 0
  }
  # hist_grouped <- total_by_subgroup(hist_df)
  out_df_district_ca <- rbind(out_df_district_ca, hist_df)
  if (is.data.frame(non_hist_df)){
    #non_hist_grouped <- total_by_subgroup(non_hist_df)
    out_df_district_ca <- rbind(out_df_district_ca, non_hist_df)
  }
}


district_level_ca <- out_df_district_ca %>% 
  # group_by(student_id, system, system_name, subgroup, grade_band) %>%
  # summarise(
  #   n_absences = sum(n_absences, na.rm = TRUE),
  #   isp_days = sum(isp_days, na.rm = TRUE),
  #   instructional_days = max(instructional_calendar_days)
  # ) %>%
  # ungroup() %>%
  # filter(isp_days/instructional_days >= 0.5) %>%
  # mutate(chronic_absence = as.integer(n_absences/isp_days >= 0.1)) %>% 
  group_by(system, subgroup, grade_band) %>% 
  summarise(
    n_students = n(),
    n_chronically_absent = sum(chronic_absence, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_chronically_absent = round(n_chronically_absent / n_students * 100 + 1e-5, 1)
  ) %>% 
  left_join(dist_names, by = "system")%>% 
  transmute(
    # year = 2018,
    system, system_name,
    subgroup, grade_band, n_students, n_chronically_absent, pct_chronically_absent
  ) %>% 
  arrange(system, subgroup)

write_csv(district_level_ca, "N:/ORP_accountability/data/2019_chronic_absenteeism/district_chronic_absenteeism_Jul11_AM.csv")

# check district
district_level_comp <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/district_chronic_absenteeism_Jul11.csv")

diff_df <- dplyr::setdiff(district_level_ca, district_level_comp) %>% 
  bind_rows(dplyr::setdiff(district_level_comp, district_level_ca)) %>% 
  arrange(system, subgroup, grade_band)


# diff_non_pcts <- school_assessment_final %>% 
#   select(year:n_mastered) %>% 
#   setdiff(school_assessment_comp %>% select(year:n_mastered)) %>% 
#   bind_rows(setdiff(school_assessment_comp %>% select(year:n_mastered), school_assessment_final %>% select(year:n_mastered))) %>% 
#   arrange(system, school, test, subject, grade, subgroup, -year)


# ===================================== School Level =====================================

school_level_ca <- out_df_ca %>% 
  # group_by(student_id, system, system_name, subgroup, grade_band) %>%
  # summarise(
  #   n_absences = sum(n_absences, na.rm = TRUE),
  #   isp_days = sum(isp_days, na.rm = TRUE),
  #   instructional_days = max(instructional_calendar_days)
  # ) %>%
  # ungroup() %>%
  # filter(isp_days/instructional_days >= 0.5) %>%
  # mutate(chronic_absence = as.integer(n_absences/isp_days >= 0.1)) %>% 
  filter(isp_days/instructional_calendar_days >= 0.5, grade_band == 'All Grades') %>% 
  group_by(system, school, subgroup, grade_band) %>% 
  summarise(
    n_students = n(),
    n_chronically_absent = sum(ca_indicator, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_chronically_absent = round(n_chronically_absent / n_students * 100 + 1e-5, 1)
  ) %>% 
  left_join(school_names, by = c("system", "school"))%>% 
  transmute(
    # year = 2018,
    system, system_name, school, school_name,
    subgroup, grade_band, n_students, n_chronically_absent, pct_chronically_absent
  ) %>% 
  arrange(system, school, subgroup)

write_csv(school_level_ca, "N:/ORP_accountability/data/2019_chronic_absenteeism/school_chronic_absenteeism_Jul11_AM.csv")

# check district
school_level_comp <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/school_chronic_absenteeism_Jul11.csv")

diff_df_school <- dplyr::setdiff(school_level_ca, school_level_comp) %>% 
  bind_rows(dplyr::setdiff(school_level_comp, school_level_ca)) %>% 
  arrange(system, subgroup, grade_band)

# =============================== State Level ==========================================

state_level_student <- read_csv("N:\\ORP_accountability\\data\\2019_chronic_absenteeism\\student_chronic_absenteeism_AM_Jul11.csv") %>% 
  mutate(
    ca_indicator = if_else(absentee_rate >= 10, 1, 0),
    grade_band = case_when(
      grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08") ~ 'K through 8th',
      grade %in% c("09", "10", "11", "12") ~ '9th through 12th'
    )
  ) %>% 
  # Add up absences and ISP days across every enrollment
  group_by(student_id, grade_band) %>%
  summarise(
    n_absences = sum(n_absences, na.rm = TRUE),
    isp_days = sum(isp_days, na.rm = TRUE),
    instructional_days = max(instructional_calendar_days),
    # All = any(All),
    # BHN = any(BHN, na.rm = TRUE),
    ED = max(ED, na.rm = TRUE),
    SWD = max(SWD, na.rm = TRUE),
    EL = max(EL, na.rm = TRUE),
    Black = max(Black, na.rm = TRUE),
    Hispanic = max(Hispanic, na.rm = TRUE),
    Native = max(Native, na.rm = TRUE),
    HPI = max(HPI, na.rm = TRUE),
    Asian = max(Asian, na.rm = TRUE),
    White = max(White, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(isp_days >= 45) %>%
  mutate(chronic_absence = as.integer(n_absences/isp_days >= 0.1))

state_level_student <- state_level_student %>% 
  bind_rows(state_level_student %>% mutate(grade_band = 'All Grades'))

out_df_state_ca <- state_level_student %>% 
  mutate(subgroup = 'All Students')


for (subgroup in subgroups){
  student_df <- state_level_student
  if (subgroup == "American Indian or Alaska Native"){
    hist_df <- student_df %>% 
      filter(Native == 1) %>% 
      mutate(subgroup = "American Indian or Alaska Native")
    non_hist_df <- 0
  }else if (subgroup == "Asian"){
    hist_df <- student_df %>% 
      filter(Asian == 1) %>% 
      mutate(subgroup = "Asian")
    non_hist_df <- 0
  }else if (subgroup == "Black or African American"){
    hist_df <- student_df %>% 
      filter(Black == 1) %>% 
      mutate(subgroup = "Black or African American")
    non_hist_df <- 0
  }else if (subgroup == "Black/Hispanic/Native American"){
    hist_df <- student_df %>% 
      filter(Black == 1 | Hispanic == 1 | Native == 1) %>% 
      mutate(subgroup = "Black/Hispanic/Native American")
    non_hist_df <- 0
  } else if (subgroup == "Economically Disadvantaged") {
    hist_df <- student_df %>% 
      filter(ED == 1) %>% 
      mutate(subgroup = "Economically Disadvantaged")
    non_hist_df <- 0
  }else if (subgroup == "English Language Learners") {
    hist_df <- student_df %>% 
      filter(EL == 1) %>% 
      mutate(subgroup = "English Learners with Transitional 1-4")
    non_hist_df <- 0
  }else if (subgroup == "Hispanic"){
    hist_df <- student_df %>% 
      filter(Hispanic == 1) %>% 
      mutate(subgroup = "Hispanic")
    non_hist_df <- 0
  }else if (subgroup == "Native Hawaiian or Other Pacific Islander"){
    hist_df <- student_df %>% 
      filter(HPI == 1) %>% 
      mutate(subgroup = "Native Hawaiian or Other Pacific Islander")
    non_hist_df <- 0
  }else if (subgroup == "White"){
    hist_df <- student_df %>% 
      filter(White == 1) %>% 
      mutate(subgroup = "White")
    non_hist_df <- 0
  }else {
    hist_df <- student_df %>% 
      filter(SWD == 1) %>% 
      mutate(subgroup = "Students with Disabilities")
    non_hist_df <- 0
  }
  # hist_grouped <- total_by_subgroup(hist_df)
  out_df_state_ca <- rbind(out_df_state_ca, hist_df)
  if (is.data.frame(non_hist_df)){
    #non_hist_grouped <- total_by_subgroup(non_hist_df)
    out_df_state_ca <- rbind(out_df_state_ca, non_hist_df)
  }
}


state_level_ca <- out_df_state_ca %>% 
  # group_by(student_id, subgroup, grade_band) %>%
  # summarise(
  #   n_absences = sum(n_absences, na.rm = TRUE),
  #   isp_days = sum(isp_days, na.rm = TRUE),
  #   instructional_days = max(instructional_calendar_days)
  # ) %>%
  # ungroup() %>%
  # filter(isp_days >= 45) %>%
  # mutate(chronic_absence = as.integer(n_absences/isp_days >= 0.1)) %>% 
  group_by(subgroup, grade_band) %>% 
  summarise(
    n_students = n(),
    n_chronically_absent = sum(chronic_absence, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_chronically_absent = round(n_chronically_absent / n_students * 100 + 1e-5, 1)
  ) %>% 
  mutate(
    system = 0,
    system_name = 'State of Tennessee'
  )%>% 
  transmute(
    # year = 2018,
    system, system_name, 
    subgroup, grade_band, n_students, n_chronically_absent, pct_chronically_absent
  ) %>% 
  arrange(system, subgroup)

write_csv(state_level_ca, "N:/ORP_accountability/data/2019_chronic_absenteeism/state_chronic_absenteeism_Jul11_AM.csv")

# check district
state_level_comp <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/state_chronic_absenteeism_Jul11.csv")

diff_df_state <- dplyr::setdiff(state_level_ca, state_level_comp) %>% 
  bind_rows(dplyr::setdiff(state_level_comp, state_level_ca)) %>% 
  arrange(system, subgroup, grade_band)


