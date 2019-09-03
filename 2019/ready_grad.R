# options(java.parameters = "-Xmx16G")
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
               "Native Hawaiian or Other Pacific Islander", "Students with Disabilities", "Super Subgroup", "White")

# =================================== Student Level and filtering duplicates ==========================================

ready_grad_student_level_total <- read_csv('N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_student_level_06182019.csv',
                                     col_types = 'icccciccciiciiiiiiiiiiiiiiiiiiic') %>% 
  filter(included_in_cohort == 'Y') %>% 
  rename(system = district_no, school = school_no) %>% 
  mutate(
    cohort_indicator = if_else(included_in_cohort == 'Y' & completion_type %in% c(1, 11, 12, 13), 1, 0),
    ready_grad_indicator = if_else(ready_graduate == 'Y' & completion_type %in% c(1, 11, 12, 13), 1, 0), 
    # Indicate that the student took a test if they have a valid composite score
    completed_act_sat = if_else((sat_total > 0 | act_composite > 0) & included_in_cohort == 'Y' & completion_type %in% c(1, 11, 12, 13), 1, 0),
    on_time_grad = if_else(included_in_cohort == 'Y' & completion_type %in% c(1, 11, 12, 13), 1, 0)
    )
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

out_df_ready_grad <- ready_grad_student_level_total %>% 
  mutate(subgroup = 'All Students')


for (subgroup in subgroups){
  student_df <- ready_grad_student_level_total
  if (subgroup == "American Indian or Alaska Native"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'I') %>% 
      mutate(subgroup = "American Indian or Alaska Native")
    non_hist_df <- 0
  }else if (subgroup == "Asian"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'A') %>% 
      mutate(subgroup = "Asian")
    non_hist_df <- 0
  }else if (subgroup == "Black or African American"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'B') %>% 
      mutate(subgroup = "Black or African American")
    non_hist_df <- 0
  }else if (subgroup == "Black/Hispanic/Native American"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'B' | race_ethnicity == 'H' | race_ethnicity == 'I') %>% 
      mutate(subgroup = "Black/Hispanic/Native American")
    non_hist_df <- 0
  } else if (subgroup == "Economically Disadvantaged") {
    hist_df <- student_df %>% 
      filter(econ_dis == 'Y') %>% 
      mutate(subgroup = "Economically Disadvantaged")
    non_hist_df <- student_df %>% 
      filter(econ_dis == 'N') %>% 
      mutate(subgroup = "Non-Economically Disadvantaged")
  }else if (subgroup == "English Language Learners") {
    hist_df <- student_df %>% 
      filter(elb == 'Y') %>% 
      mutate(subgroup = "English Learners with Transitional 1-4")
    non_hist_df <- student_df %>% 
      filter(elb == 'N') %>% 
      mutate(subgroup = "Non-English Learners")
  }else if (subgroup == "Hispanic"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'H') %>% 
      mutate(subgroup = "Hispanic")
    non_hist_df <- 0
  }else if (subgroup == "Native Hawaiian or Other Pacific Islander"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'P') %>% 
      mutate(subgroup = "Native Hawaiian or Other Pacific Islander")
    non_hist_df <- 0
  }else if (subgroup == "White"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'W') %>% 
      mutate(subgroup = "White")
    non_hist_df <- 0
  }else if (subgroup == "Super Subgroup"){
    hist_df <- student_df %>% 
      filter(race_ethnicity == 'B' | race_ethnicity == 'H' | race_ethnicity == 'I' | elb == 'Y' | econ_dis == 'Y' | swd == 'Y') %>% 
      mutate(subgroup = "Super Subgroup")
    non_hist_df <- 0
  }else {
    hist_df <- student_df %>% 
      filter(swd == 'Y') %>% 
      mutate(subgroup = "Students with Disabilities")
    non_hist_df <- student_df %>% 
      filter(swd == 'N')%>% 
      mutate(subgroup = "Non-Students with Disabilities")
  }
  # hist_grouped <- total_by_subgroup(hist_df)
  out_df_ready_grad <- rbind(out_df_ready_grad, hist_df)
  if (is.data.frame(non_hist_df)){
    #non_hist_grouped <- total_by_subgroup(non_hist_df)
    out_df_ready_grad <- rbind(out_df_ready_grad, non_hist_df)
  }
}


# ===================================== District Level =====================================

district_level_ready_grad <- out_df_ready_grad %>% 
  group_by(system, subgroup) %>% 
  summarise(
    grad_cohort = sum(cohort_indicator, na.rm = TRUE),
    n_ready_grad = sum(ready_grad_indicator, na.rm = TRUE),
    n_completed_act_sat = sum(completed_act_sat, na.rm = TRUE),
    n_count = n()
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_ready_grad = round(n_ready_grad / n_count * 100 + 1e-5, 1),
    act_participation_rate = round(n_completed_act_sat / grad_cohort * 100 + 1e-5, 0)
  ) %>% 
  left_join(dist_names, by = "system")%>% 
  transmute(
    # year = 2018,
    system, system_name,
    subgroup, act_participation_rate, n_count, n_ready_grad, pct_ready_grad
  ) %>% 
  arrange(subgroup, system)

write_csv(district_level_ready_grad, "N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_district_AM.csv")
# ===================================== School Level =====================================

school_level_ready_grad <- out_df_ready_grad %>% 
  group_by(system, school, subgroup) %>% 
  summarise(
    grad_cohort = sum(cohort_indicator, na.rm = TRUE),
    n_ready_grad = sum(ready_grad_indicator, na.rm = TRUE),
    n_completed_act_sat = sum(completed_act_sat, na.rm = TRUE),
    n_count = n()
  ) %>% 
  ungroup() %>% 
  filter(grad_cohort > 0) %>% 
  mutate(
    pct_ready_grad = round(n_ready_grad / n_count * 100 + 1e-5, 1),
    act_participation_rate = round(n_completed_act_sat / grad_cohort * 100 + 1e-5, 0)
  ) %>% 
  left_join(school_names, by = c("system", 'school')) %>% 
  transmute(
    # year = 2018,
    system, system_name, school, school_name,
    subgroup, act_participation_rate, n_count, n_ready_grad, pct_ready_grad
  ) %>% 
  arrange(subgroup, system, school)

write_csv(school_level_ready_grad, "N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_school_AM.csv")

# =============================== State Level ==========================================
state_level_ready_grad <- out_df_ready_grad %>% 
  group_by(subgroup) %>% 
  summarise(
    grad_cohort = sum(cohort_indicator, na.rm = TRUE),
    n_ready_grad = sum(ready_grad_indicator, na.rm = TRUE),
    n_completed_act_sat = sum(completed_act_sat, na.rm = TRUE),
    n_count = n()
  ) %>% 
  ungroup() %>% 
  filter(grad_cohort > 0) %>% 
  mutate(
    pct_ready_grad = round(n_ready_grad / n_count * 100 + 1e-5, 1),
    act_participation_rate = round(n_completed_act_sat / grad_cohort * 100 + 1e-5, 0)
  ) %>% 
  # left_join(school_names, by = c("system", 'school')) %>% 
  transmute(
    # year = 2018,
    subgroup, act_participation_rate, n_count, n_ready_grad, pct_ready_grad
  ) %>% 
  arrange(subgroup)

write_csv(state_level_ready_grad, "N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_state_AM.csv")






