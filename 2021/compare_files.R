# compare files

library(tidyverse)


# ====== Compare Assessment =================
state_am <- read_csv('N:/ORP_accountability/data/2020_final_accountability_files/state_assessment_file.csv')
state_sm <- read_csv('N:/ORP_accountability/data/2020_final_accountability_files/state_assessment_file_SM.csv')

compare_state <- bind_rows(
  dplyr::setdiff(state_am, state_sm) %>% mutate(person = 'AM'),
  dplyr::setdiff(state_sm, state_am) %>% mutate(person = 'SM')
) %>% 
  arrange(subject, grade, subgroup, person)

district_am <- read_csv('N:/ORP_accountability/data/2020_final_accountability_files/district_assessment_file.csv')
district_sm <- read_csv('N:/ORP_accountability/data/2020_final_accountability_files/district_assessment_file_SM.csv')

compare_district <- bind_rows(
  dplyr::setdiff(district_am, district_sm) %>% mutate(person = 'AM'),
  dplyr::setdiff(district_sm, district_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, subject, grade, subgroup, person)

school_am <- read_csv('N:/ORP_accountability/data/2020_final_accountability_files/school_assessment_file.csv')
school_sm <- read_csv('N:/ORP_accountability/data/2020_final_accountability_files/school_assessment_file_SM.csv')

compare_school <- bind_rows(
  setdiff(school_am, school_sm) %>% mutate(person = 'AM'),
  setdiff(school_sm, school_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, school, subject, grade, subgroup, person)

# =============== Compare Absenteeism =================================
student_absenteeism_am <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/student_chronic_absenteeism_Aug14.csv")
student_absenteeism_sm <- read_csv("N:/ORP_accountability/projects/Sophie/student_absenteeism_Aug14.csv")
student_absenteeism_old <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/Previous Versions/student_chronic_absenteeism_Aug13.csv")

compare_student <- bind_rows(
  dplyr::setdiff(student_absenteeism_am %>% select(-grade), student_absenteeism_sm %>% select(-grade)) %>% mutate(person = 'AM'),
  dplyr::setdiff(student_absenteeism_sm %>% select(-grade), student_absenteeism_am %>% select(-grade)) %>% mutate(person = 'SM')
) %>% 
  arrange(student_id, system, school, person)

compare_student_new <- bind_rows(
  dplyr::setdiff(student_absenteeism_am, student_absenteeism_old) %>% mutate(person = 'New'),
  dplyr::setdiff(student_absenteeism_old, student_absenteeism_am) %>% mutate(person = 'Old')
) %>% 
  filter(!(system == 792 & school == 8275)) %>% 
  arrange(student_id, system, school, person)

state_absenteeism_am <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/state_chronic_absenteeism_Aug14.csv")
state_absenteeism_sm <- read_csv("N:/ORP_accountability/projects/Sophie/state_absenteeism_Aug14.csv")
state_absenteeism_old <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/Previous Versions/state_chronic_absenteeism_Aug13.csv")

compare_state <- bind_rows(
  dplyr::setdiff(state_absenteeism_am, state_absenteeism_sm) %>% mutate(person = 'AM'),
  dplyr::setdiff(state_absenteeism_sm, state_absenteeism_am) %>% mutate(person = 'SM')
) %>% 
  arrange(subgroup, grade_band, person)

compare_state_new <- bind_rows(
  dplyr::setdiff(state_absenteeism_am, state_absenteeism_old) %>% mutate(person = 'New'),
  dplyr::setdiff(state_absenteeism_old, state_absenteeism_am) %>% mutate(person = 'Old')
) %>% 
  arrange(subgroup, grade_band, person)

dist_absenteeism_am <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/district_chronic_absenteeism_Aug14.csv")
dist_absenteeism_sm <- read_csv("N:/ORP_accountability/projects/Sophie/district_absenteeism_Aug14.csv")
dist_absenteeism_old <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/Previous Versions/district_chronic_absenteeism_Aug13.csv")

compare_district <- bind_rows(
  dplyr::setdiff(dist_absenteeism_am, dist_absenteeism_sm) %>% mutate(person = 'AM'),
  dplyr::setdiff(dist_absenteeism_sm, dist_absenteeism_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, subgroup, grade_band, person)

compare_district_new <- bind_rows(
  dplyr::setdiff(dist_absenteeism_am, dist_absenteeism_old) %>% mutate(person = 'New'),
  dplyr::setdiff(dist_absenteeism_old, dist_absenteeism_am) %>% mutate(person = 'Old')
) %>% 
  arrange(system, subgroup, grade_band, person)

school_absenteeism_am <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/school_chronic_absenteeism_Aug14.csv")
school_absenteeism_sm <- read_csv("N:/ORP_accountability/projects/Sophie/school_absenteeism_Aug14.csv")
school_absenteeism_old <- read_csv("N:/ORP_accountability/data/2020_chronic_absenteeism/Previous Versions/school_chronic_absenteeism_Aug13.csv")

compare_school <- bind_rows(
  dplyr::setdiff(school_absenteeism_am, school_absenteeism_sm) %>% mutate(person = 'AM'),
  dplyr::setdiff(school_absenteeism_sm, school_absenteeism_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, school, subgroup, grade_band, person)

compare_school_new <- bind_rows(
  dplyr::setdiff(school_absenteeism_am, school_absenteeism_old) %>% mutate(person = 'New'),
  dplyr::setdiff(school_absenteeism_old, school_absenteeism_am) %>% mutate(person = 'Old')
) %>% 
  arrange(system, school, subgroup, grade_band, person)


# =============== Compare Ready Grad =================================
student_ready_grad_am <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_student_level.csv",
                                  col_types = 'icccciccciiciiiiiiiiiiiiiiiiiiic')
student_ready_grad_sm <- read_csv("N:/ORP_accountability/projects/Sophie/Ready Grad 2020/ready_grad_student.csv",
                                  col_types = 'icccciccciiciiiiiiiiiiiiiiiiiiic')
student_original <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/Previous/Ready Grad 07242020 Release/ready_graduate_student_level_07242020.csv",
                                  col_types = 'icccciccciiciiiiiiiiiiiiiiiiiiic')
compare_student <- bind_rows(
  setdiff(student_ready_grad_am, student_ready_grad_sm) %>% mutate(person = 'AM'),
  setdiff(student_ready_grad_sm, student_ready_grad_am) %>% mutate(person = 'SM')
) %>% 
  arrange(student_key, district_no, school_no, person)

compare_student_new <- bind_rows(
  setdiff(student_ready_grad_am, student_original) %>% mutate(person = 'New'),
  setdiff(student_original, student_ready_grad_am) %>% mutate(person = 'Original')
) %>% 
  arrange(student_key, district_no, school_no, person)

state_ready_grad_am <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_state.csv")
state_ready_grad_sm <- read_csv("N:/ORP_accountability/projects/Sophie/Ready Grad 2020/ready_grad_state.csv")

compare_state <- bind_rows(
  setdiff(state_ready_grad_am, state_ready_grad_sm) %>% mutate(person = 'AM'),
  setdiff(state_ready_grad_sm, state_ready_grad_am) %>% mutate(person = 'SM')
) %>% 
  arrange(subgroup, person)

dist_ready_grad_am <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_district.csv")
dist_ready_grad_sm <- read_csv("N:/ORP_accountability/projects/Sophie/Ready Grad 2020/ready_grad_district.csv")

compare_district <- bind_rows(
  setdiff(dist_ready_grad_am, dist_ready_grad_sm) %>% mutate(person = 'AM'),
  setdiff(dist_ready_grad_sm, dist_ready_grad_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, subgroup, person)

school_ready_grad_am <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_school.csv")
school_ready_grad_sm <- read_csv("N:/ORP_accountability/projects/Sophie/Ready Grad 2020/ready_grad_school.csv")

compare_school <- bind_rows(
  setdiff(school_ready_grad_am, school_ready_grad_sm) %>% mutate(person = 'AM'),
  setdiff(school_ready_grad_sm, school_ready_grad_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, school, subgroup, person)

school_targets_ready_grad_am <- read_csv("N:/ORP_accountability/projects/2021_amo/ready_grad_targets_school.csv")
school_targets_ready_grad_sm <- read_csv("N:/ORP_accountability/projects/Sophie/Ready Grad 2020/readygradtargets.csv")

compare_targets <- bind_rows(
  setdiff(school_targets_ready_grad_am, school_targets_ready_grad_sm) %>% mutate(person = 'AM'),
  setdiff(school_targets_ready_grad_sm, school_targets_ready_grad_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, school, subgroup, person)

# ===================== Compare RG to Grad Rate ====================
school_rg <- read_csv("N:/ORP_accountability/projects/2020_ready_graduate/Data/ready_graduate_school.csv") %>% 
  select(system, school, subgroup, n_count)
school_grad_rate_2019 <- read_csv("N:/ORP_accountability/data/2019_graduation_rate/school_grad_rate.csv") %>% 
  select(system, school, subgroup, n_count = grad_cohort) %>% 
  filter(subgroup %in% unique(school_rg$subgroup))

compare_rg_and_grad <- bind_rows(
  setdiff(school_rg, school_grad_rate_2019) %>% mutate(person = 'RG'),
  setdiff(school_grad_rate_2019, school_rg) %>% mutate(person = 'Grad')
) %>% 
  arrange(system, school, subgroup, person)

# ============ Compare Grad Rate AMOs =================
school_targets_grad_am <- read_csv("N:/ORP_accountability/projects/2021_amo/grad_targets_school.csv")
school_targets_grad_sm <- read_csv("N:/ORP_accountability/projects/Sophie/Grad Rate 2020/gradrates_school_SM.csv")

compare_targets_school <- bind_rows(
  setdiff(school_targets_grad_am, school_targets_grad_sm) %>% mutate(person = 'AM'),
  setdiff(school_targets_grad_sm, school_targets_grad_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, school, subgroup, person)

district_targets_grad_am <- read_csv("N:/ORP_accountability/projects/2021_amo/grad_targets_district.csv")
district_targets_grad_sm <- read_csv("N:/ORP_accountability/projects/Sophie/Grad Rate 2020/gradrates_district_SM.csv")

compare_targets_district <- bind_rows(
  setdiff(district_targets_grad_am, district_targets_grad_sm) %>% mutate(person = 'AM'),
  setdiff(district_targets_grad_sm, district_targets_grad_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, subgroup, person)

# ====================== Compare ELPA =======================
student_elpa_am <- read_csv("N:/ORP_accountability/data/2020_ELPA/wida_growth_standard_student.csv")
student_elpa_sm <- read_csv("N:/ORP_accountability/projects/Sophie/ELPA 2020/wida_student.csv")

compare_student <- bind_rows(
  setdiff(student_elpa_am, student_elpa_sm) %>% mutate(person = 'AM'),
  setdiff(student_elpa_sm, student_elpa_am) %>% mutate(person = 'SM')
) %>% 
  arrange(student_id, system, school, person)

state_elpa_am <- read_csv("N:/ORP_accountability/data/2020_ELPA/wida_growth_standard_state.csv")
state_elpa_sm <- read_csv("N:/ORP_accountability/projects/Sophie/ELPA 2020/wida_state.csv")

compare_state <- bind_rows(
  setdiff(state_elpa_am, state_elpa_sm) %>% mutate(person = 'AM'),
  setdiff(state_elpa_sm, state_elpa_am) %>% mutate(person = 'SM')
) %>% 
  arrange(subgroup, person)

dist_elpa_am <- read_csv("N:/ORP_accountability/data/2020_ELPA/wida_growth_standard_district.csv")
dist_elpa_sm <- read_csv("N:/ORP_accountability/projects/Sophie/ELPA 2020/wida_district.csv")

compare_district <- bind_rows(
  setdiff(dist_elpa_am, dist_elpa_sm) %>% mutate(person = 'AM'),
  setdiff(dist_elpa_sm, dist_elpa_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, subgroup, person)

school_elpa_am <- read_csv("N:/ORP_accountability/data/2020_ELPA/wida_growth_standard_school.csv")
school_elpa_sm <- read_csv("N:/ORP_accountability/projects/Sophie/ELPA 2020/wida_school.csv")

compare_school <- bind_rows(
  setdiff(school_elpa_am, school_elpa_sm) %>% mutate(person = 'AM'),
  setdiff(school_elpa_sm, school_elpa_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, school, subgroup, person)



# ==================== Shelby County Appeal ================================
multiple_grades <- absenteeism %>% 
  group_by(system, school, student_key, n_absences, isp_days) %>% 
  mutate(n = n(),
         min_grade = min(grade),
         max_grade = max(grade)) %>% 
  filter(n > 1)

# 5023192 - K and 5
write_csv(multiple_grades %>% arrange(system, school, student_id, grade), "N:/ORP_accountability/projects/Andrew/Data Requests/2020/Data/multiple_grades_absenteeism.csv", na = "")


absenteeism %>% 
  group_by(system, school, student_key, n_absences, isp_days) %>% 
  mutate(n = n()) %>% 
  filter(n > 1) %>% View()

school_comp <- school %>% 
  left_join(
    school_absenteeism_am %>% select(system, school, subgroup, grade_band,
                                    n_students_old = n_students,
                                   n_chronically_absent_old = n_chronically_absent,
                                    pct_chronically_absent_old = pct_chronically_absent),
    by = c('system', 'school', 'subgroup', 'grade_band')
  ) %>% 
  mutate(
    pct_diff = pct_chronically_absent - pct_chronically_absent_old
  )


school_metrics <- school_comp %>% 
  mutate(
    improved = if_else(pct_diff < 0, 1, 0),
    declined = if_else(pct_diff > 0, 1, 0)
  )

# ================= Compare ACT ================
student_act_am <- read_csv("N:/ORP_accountability/data/2020_ACT/ACT_student_post_appeals.csv",
                           col_types = 'icccccccciccccccciciiiciiccccciccccccicccccccciiiiiiillllllllllllllllllllllllll')
student_act_sm <- read_csv("N:/ORP_accountability/projects/Sophie/ACT 2020/ACT_student_postappeal.csv",
                           col_types = 'icccccccciccccccciciiiciiccccciccccccicccccccciiiiiiillllllllllllllllllllllllll')
student_pre_appeals_am <- read_csv("N:/ORP_accountability/data/2020_ACT/Pre Appeals/ACT_student_pre_appeals.csv",
                                   col_types = 'icccccccciccccccciciiiciiccccciccccccicccccccciiiiiiillllllllllllllllllllllllll')

compare_student <- bind_rows(
  setdiff(student_act_am, student_act_sm) %>% mutate(person = 'AM'),
  setdiff(student_act_sm, student_act_am) %>% mutate(person = 'SM')
) %>% 
  arrange(student_key, system, school, person)

compare_student_pre_post <- bind_rows(
  setdiff(student_act_am %>% select(-cte), student_pre_appeals_am %>% select(-cte)) %>% mutate(person = 'Post'),
  setdiff(student_pre_appeals_am %>% select(-cte), student_act_am %>% select(-cte)) %>% mutate(person = 'Pre')
) %>% 
  arrange(student_key, system, school, person)

state_act_am <- read_csv("N:/ORP_accountability/data/2020_ACT/ACT_state_post_appeals.csv")
state_act_sm <- read_csv("N:/ORP_accountability/projects/Sophie/ACT 2020/ACT_state_postappeal.csv")

compare_state <- bind_rows(
  setdiff(state_act_am, state_act_sm) %>% mutate(person = 'AM'),
  setdiff(state_act_sm, state_act_am) %>% mutate(person = 'SM')
) %>% 
  arrange(subgroup, person)

dist_act_am <- read_csv("N:/ORP_accountability/data/2020_ACT/ACT_district_post_appeals.csv")
dist_act_sm <- read_csv("N:/ORP_accountability/projects/Sophie/ACT 2020/ACT_district_postappeal.csv")

compare_district <- bind_rows(
  setdiff(dist_act_am, dist_act_sm) %>% mutate(person = 'AM'),
  setdiff(dist_act_sm, dist_act_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, subgroup, person)

school_act_am <- read_csv("N:/ORP_accountability/data/2020_ACT/ACT_school_post_appeals.csv")
school_act_sm <- read_csv("N:/ORP_accountability/projects/Sophie/ACT 2020/ACT_school_postappeal.csv")

compare_school <- bind_rows(
  setdiff(school_act_am, school_act_sm) %>% mutate(person = 'AM'),
  setdiff(school_act_sm, school_act_am) %>% mutate(person = 'SM')
) %>% 
  arrange(system, school, subgroup, person)














