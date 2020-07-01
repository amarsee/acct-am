options(java.parameters = "-Xmx16G")
library(tidyverse)
library(janitor)
library(readstata13)
library(readxl)
library(haven)
library(RJDBC)

# ============ Function to calculate rates ===========
calc_counts <- function(grouped_df){
  grouped_df %>% 
    summarise(
      grad_cohort = sum(grad_indicator, na.rm = TRUE),
      n_ready_grad = sum(ready_grad_indicator, na.rm = TRUE),
      n_completed_act_sat = sum(completed_act_sat, na.rm = TRUE),
      n_count = n()
    ) %>% 
    ungroup()
}

calc_pcts <- function(df){
  df %>% 
    mutate(
      pct_ready_grad = round(n_ready_grad / n_count * 100 + 1e-5, 1),
      act_participation_rate = round(n_completed_act_sat / grad_cohort * 100 + 1e-5, 0)
    )
}

# ================== Pull Data ===============
# Data
con = dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="N:/ORP_accountability/ojdbc6.jar"),
  readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
  "EIS_MGR",
  readRegistry("Environment", "HCU")$EIS_MGR_PWD
)
# SQL Pull
rg = as.tbl(
  dbGetQuery(
    con,
    "select *
    from student_ready_grads"
  )
  ) %>%
  janitor::clean_names()

ready_grad_student <- rg %>%
  select(
    student_key:ncrc_work_keys, participate_clg_lvl_pgm, n_cambridge,
    n_ap = n_adv_placement, n_ib = n_inter_baccalaureate, n_sdc = n_statewide_dual_credit,
    n_ldc = n_local_dual_credit, n_de = n_dual_enrollment, ready_graduate
  ) %>%
  mutate_at(
    .vars = c('student_key', 'cohortyear', 'district_no', 'school_no', 'completion_type', 'sat_math', 'sat_critical_reading',
              'sat_total', 'act_english', 'act_math', 'act_reading', 'act_science', 'act_composite', 'industry_cert_earned',
              'asvab', 'ncrc_work_keys', 'participate_clg_lvl_pgm', 'n_cambridge', 'n_ap', 'n_ib', 'n_sdc', 'n_ldc', 'n_de'),
    .funs = as.integer
  )

# write_csv(ready_grad_student , 'N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_student_level_06182019.csv')


school_names <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv")

dist_names <- school_names %>% 
  select(system, system_name) %>% 
  distinct()

# =================================== Student Level and filtering duplicates ==========================================

ready_grad_student_level_total <- read_csv('N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_student_level.csv',
                                     col_types = 'icccciccciiciiiiiiiiiiiiiiiiiiic') %>% 
  filter(included_in_cohort == 'Y') %>% 
  rename(system = district_no, school = school_no) %>%
  mutate(
    grad_indicator = if_else(included_in_cohort == 'Y' & completion_type %in% c(1, 11, 12, 13), 1, 0),
    ready_grad_indicator = if_else(ready_graduate == 'Y' & completion_type %in% c(1, 11, 12, 13), 1, 0), 
    # Indicate that the student took a test if they have a valid composite score
    completed_act_sat = if_else((sat_total > 0 | act_composite > 0) & included_in_cohort == 'Y' & completion_type %in% c(1, 11, 12, 13), 1, 0)
  )

# ================================== Ready Grad Subgroups ===========================================================

out_df_ready_grad <- bind_rows(
    ready_grad_student_level_total %>% mutate(subgroup = 'All Students'),
    ready_grad_student_level_total %>% filter(race_ethnicity == 'I') %>% mutate(subgroup = "American Indian or Alaska Native"),
    ready_grad_student_level_total %>% filter(race_ethnicity == 'A') %>% mutate(subgroup = "Asian"),
    ready_grad_student_level_total %>% filter(race_ethnicity == 'B') %>% mutate(subgroup = "Black or African American"),
    ready_grad_student_level_total %>% filter(race_ethnicity == 'B' | race_ethnicity == 'H' | race_ethnicity == 'I') %>% mutate(subgroup = "Black/Hispanic/Native American"),
    ready_grad_student_level_total %>% filter(ed == 'Y') %>% mutate(subgroup = "Economically Disadvantaged"),
    ready_grad_student_level_total %>% filter(ed == 'N') %>% mutate(subgroup = "Non-Economically Disadvantaged"),
    ready_grad_student_level_total %>% filter(el == 'Y') %>% mutate(subgroup = "English Learners with Transitional 1-4"),
    ready_grad_student_level_total %>% filter(el == 'N') %>% mutate(subgroup = "Non-English Learners"),
    ready_grad_student_level_total %>% filter(race_ethnicity == 'H') %>% mutate(subgroup = "Hispanic"),
    ready_grad_student_level_total %>% filter(race_ethnicity == 'P') %>% mutate(subgroup = "Native Hawaiian or Other Pacific Islander"),
    ready_grad_student_level_total %>% filter(race_ethnicity == 'W') %>% mutate(subgroup = "White"),
    ready_grad_student_level_total %>% 
      filter(race_ethnicity == 'B' | race_ethnicity == 'H' | race_ethnicity == 'I' | el == 'Y' | ed == 'Y' | swd == 'Y') %>% 
      mutate(subgroup = "Super Subgroup"),
    ready_grad_student_level_total %>% filter(swd == 'Y') %>% mutate(subgroup = "Students with Disabilities"),
    ready_grad_student_level_total %>% filter(swd == 'N')%>% mutate(subgroup = "Non-Students with Disabilities")
  )


# ===================================== District Level =====================================

district_level_ready_grad <- out_df_ready_grad %>% 
  group_by(system, subgroup) %>% 
  calc_counts() %>% 
  calc_pcts() %>% 
  left_join(dist_names, by = "system")%>% 
  transmute(
    # year = 2018,
    system, system_name,
    subgroup, act_participation_rate, n_count, n_ready_grad, pct_ready_grad
  ) %>% 
  arrange(system, subgroup)

write_csv(district_level_ready_grad, "N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_district_AM.csv")
# ===================================== School Level =====================================

school_level_ready_grad <- out_df_ready_grad %>% 
  group_by(system, school, subgroup) %>% 
  calc_counts() %>%
  filter(grad_cohort > 0) %>% 
  calc_pcts() %>% 
  left_join(school_names, by = c("system", 'school')) %>% 
  transmute(
    # year = 2018,
    system, system_name, school, school_name,
    subgroup, act_participation_rate, n_count, n_ready_grad, pct_ready_grad
  ) %>% 
  arrange(system, school, subgroup)

write_csv(school_level_ready_grad, "N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_school_AM.csv")

# =============================== State Level ==========================================
state_level_ready_grad <- out_df_ready_grad %>% 
  group_by(subgroup) %>% 
  calc_counts() %>%
  # filter(grad_cohort > 0) %>% 
  calc_pcts() %>% 
  transmute(
    # year = 2018,
    subgroup, act_participation_rate, n_count, n_ready_grad, pct_ready_grad
  ) %>% 
  arrange(subgroup)

write_csv(state_level_ready_grad, "N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_state_AM.csv")

# =================== Split Files ======================
# Split district file
district_numbers <- sort(unique(ready_grad_student_level_total$system))

district_level_ready_grad %>%
  group_split(system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/projects/2020_ready_graduate/Data/split/", .y,
      "_2020_ReadyGraduate_District_Level_", format(Sys.Date(), "%d%b%Y"),
      ".csv"
    ), na = "")
  )


# Split school file
school_level_ready_grad %>%
  group_split(system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/projects/2020_ready_graduate/Data/split/", .y,
      "_2020_ReadyGraduate_School_Level_", format(Sys.Date(), "%d%b%Y"), ".csv"
    ), na = "")
  )

# Split student level file
ready_grad_student_level_total %>%
  group_split(system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/projects/2020_ready_graduate/Data/split/", .y,
      "_2020_ReadyGraduate_Student_Level_", format(Sys.Date(), "%d%b%Y"), ".csv"
    ), na = "")
  )



# ================================== Analysis ============================
# EPSO Participation
epso_2020 <- ready_grad_student_level_total %>% 
  filter(included_in_cohort == 'Y') %>% 
  mutate(
    epso_particpated = as.numeric(industry_cert > 0 | n_cambridge > 0 | n_ap > 0 | 
                                      n_ib > 0 | n_sdc > 0 | n_ldc > 0 | n_de > 0 | clep > 0)
  ) %>% 
  summarise(
    n_cohort = n(),
    n_participated_epso = sum(epso_particpated),
    pct_participated_epso = round(n_participated_epso / n_cohort * 100, 1)
  )

epso_2019 <- read_csv('N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_student_level_06182019.csv',
                      col_types = 'icccciccciiciiiiiiiiiiiiiiiiiiic') %>% 
  filter(included_in_cohort == 'Y') %>% 
  replace_na(list(industry_cert_earned = 0, n_de = 0)) %>% 
  mutate(
    epso_particpated = as.numeric(industry_cert_earned > 0 | n_cambridge > 0 | n_ap > 0 | 
                                    n_ib > 0 | n_sdc > 0 | n_ldc > 0 | n_de > 0 | participate_clg_lvl_pgm > 0)
  ) %>% 
  summarise(
    n_cohort = n(),
    n_participated_epso = sum(epso_particpated),
    pct_participated_epso = round(n_participated_epso / n_cohort * 100, 1)
  )


















