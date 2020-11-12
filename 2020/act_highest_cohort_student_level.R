library(tidyverse)
library(janitor)
library(readstata13)
library(lubridate)
library(readxl)

# hs_crosswalk <- read_csv("N:/ORP_accountability/projects/Jessica/Data Returns/Helpful Documents/ACT_xwalkAug2018rev.csv")

grad_rate_student_level <- read_csv("N:/ORP_accountability/data/2020_graduation_rate/student_level.csv") %>% 
  filter(included_in_cohort == 'Y', completion_type %in% c(1, 11, 12, 13))

closed <- read_csv("N:/ORP_accountability/data/2020_tdoe_provided_files/closed_schools.csv") %>%
  transmute(system = as.integer(SYSTEM), school = as.integer(SCHOOL))

cte_alt_adult <- read_csv("N:/ORP_accountability/data/2020_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
  transmute(system = as.integer(DISTRICT_NUMBER), school = as.integer(SCHOOL_NUMBER))

# ============= Junior Day 2 Years Ago ====================
junior_2_prior <- read.dta13("N:/Assessment_Data Returns/ACT/2017-18/2018 Spring/Final Spring Files/20180716_ACT_JuniorDayResults_SY2017-18_Whalen_v1.dta") %>% 
  filter(test_location != "M") %>% 
  # dedup by highest composite
  group_by(state_stud_id) %>% 
  mutate(temp = max(act_composite)) %>% 
  filter(act_composite == temp) %>% 
  # dedup by max math
  mutate(temp = max(act_math)) %>% 
  filter(act_math == temp) %>% 
  # dedup by max reading
  mutate(temp = max(act_read)) %>% 
  filter(act_read == temp) %>% 
  # dedup by max english
  mutate(temp = max(act_eng)) %>% 
  filter(act_eng == temp) %>% 
  # dedup by max science
  mutate(temp = max(act_sci)) %>% 
  filter(act_sci == temp) %>% 
  ungroup() %>% 
  mutate(cr_all = if_else(cr_eng == 1 & cr_math == 1 & cr_read == 1 & cr_sci == 1, 1, 0)) %>% 
  select(unique_student_id = state_stud_id, act_eng:act_composite, cr_eng:cr_sci, cr_all) %>% 
  mutate(test_date = '2017-18', pref = 4)

# ============= Junior Day Prior ====================
junior_prior <- read_csv("N:/Assessment_Data Returns/ACT/2018-19/2019 Spring/Spring 2019 Final File/20190620_ACT_JuniorDayResults_SY2018-19.csv") %>% 
  filter(test_location != "M") %>% 
  # dedup by highest composite
  group_by(state_stud_id) %>% 
  mutate(temp = max(act_composite)) %>% 
  filter(act_composite == temp) %>% 
  # dedup by max math
  mutate(temp = max(act_math)) %>% 
  filter(act_math == temp) %>% 
  # dedup by max reading
  mutate(temp = max(act_read)) %>% 
  filter(act_read == temp) %>% 
  # dedup by max english
  mutate(temp = max(act_eng)) %>% 
  filter(act_eng == temp) %>% 
  # dedup by max science
  mutate(temp = max(act_sci)) %>% 
  filter(act_sci == temp) %>% 
  ungroup() %>% 
  mutate(cr_all = if_else(cr_eng == 1 & cr_math == 1 & cr_read == 1 & cr_sci == 1, 1, 0)) %>% 
  select(unique_student_id = state_stud_id, act_eng:act_composite, cr_eng:cr_sci, cr_all) %>% 
  mutate_at(
    .vars = vars(act_eng:act_composite),
    .funs = as.integer
  ) %>% 
  mutate(test_date = '2018-19', pref = 3)

# ============= Junior Day Current ====================
junior_current <- read_csv("N:/Assessment_Data Returns/ACT/2019-2020/2020 Spring Testing/Final/20200924_ACT_JuniorDayResults_SY2019-20.csv",
                           col_types = 'icccccicciiiiiiciiciiiiiciiciiii') %>% 
  # filter(test_location != "M") %>% 
  # dedup by highest composite
  group_by(state_stud_id) %>% 
  mutate(temp = max(act_composite)) %>% 
  filter(act_composite == temp) %>% 
  # dedup by max math
  mutate(temp = max(act_math)) %>% 
  filter(act_math == temp) %>% 
  # dedup by max reading
  mutate(temp = max(act_read)) %>% 
  filter(act_read == temp) %>% 
  # dedup by max english
  mutate(temp = max(act_eng)) %>% 
  filter(act_eng == temp) %>% 
  # dedup by max science
  mutate(temp = max(act_sci)) %>% 
  filter(act_sci == temp) %>% 
  ungroup() %>% 
  mutate(cr_all = if_else(cr_eng == 1 & cr_math == 1 & cr_read == 1 & cr_sci == 1, 1, 0)) %>% 
  select(unique_student_id = state_stud_id, act_eng:act_composite, cr_eng:cr_sci, cr_all) %>% 
  mutate_at(
    .vars = vars(act_eng:act_composite),
    .funs = as.integer
  ) %>% 
  mutate(test_date = '2019-20', pref = 2)

# ============= Cohort Highest Score File ====================
cohort_highest_score <- read_csv("N:/Assessment_Data Returns/ACT/2019-2020/2020 Graduation Cohort Final/20200915_ACT_GraduatingCohortHighestScore_SY2019-20.csv") %>% 
  # dedup by highest composite
  group_by(unique_student_id) %>% 
  mutate(temp = max(act_composite_highest)) %>% 
  filter(act_composite_highest == temp) %>% 
  # dedup by max math
  mutate(temp = max(act_math_highest)) %>% 
  filter(act_math_highest == temp) %>% 
  # dedup by max reading
  mutate(temp = max(act_reading_highest)) %>% 
  filter(act_reading_highest == temp) %>% 
  # dedup by max english
  mutate(temp = max(act_english_highest)) %>% 
  filter(act_english_highest == temp) %>% 
  # dedup by max science
  mutate(temp = max(act_science_highest)) %>% 
  filter(act_science_highest == temp) %>% 
  ungroup() %>% 
  rename_at(.vars = vars(cr_eng_highest:cr_all_highest),
            .funs = ~str_replace(., '_highest', '')) %>% 
  select(unique_student_id, act_eng = act_english_highest, act_math = act_math_highest, act_read = act_reading_highest,
         act_sci = act_science_highest, act_composite = act_composite_highest, cr_eng:cr_sci, cr_all) %>% 
  mutate(test_date = 'Cohort', pref = 1)


# ============== Combine and dedup again =======================
act_student_level <- bind_rows(junior_2_prior, junior_prior, junior_current, cohort_highest_score) %>% 
  # dedup by highest composite
  group_by(unique_student_id) %>% 
  mutate(temp = max(act_composite)) %>% 
  filter(act_composite == temp) %>% 
  # dedup by max math
  mutate(temp = max(act_math)) %>% 
  filter(act_math == temp) %>% 
  # dedup by max reading
  mutate(temp = max(act_read)) %>% 
  filter(act_read == temp) %>% 
  # dedup by max english
  mutate(temp = max(act_eng)) %>% 
  filter(act_eng == temp) %>% 
  # dedup by max science
  mutate(temp = max(act_sci)) %>% 
  filter(act_sci == temp) %>% 
  # dedup by test_date
  mutate(temp = min(pref, na.rm = TRUE)) %>% 
  filter(pref == temp) %>% 
  ungroup() %>% 
  select(-temp, -test_date, - pref) %>% 
  distinct() %>% 
  rename(student_key = unique_student_id, composite = act_composite, math = act_math, reading = act_read,
         english = act_eng, science = act_sci) %>% 
  left_join(grad_rate_student_level, ., by = c('student_key')) 

# ===================== Update Student Level with Appeals ======================================
appeals_tracker <- read_excel("N:/ORP_accountability/appeals/2021/ACT/Coding/ACT Appeals Master Tracker.xlsm", sheet = 3) %>% 
  clean_names() %>% 
  filter(status == 'Approved') %>% # approved appeals
  mutate(student_key = as.numeric(student_id)) %>% 
  mutate_at(vars(sat_total:sat_reading),
            .funs = ~if_else(. == 'n/a', '0', .)) %>% # sometimes people put n/a 
  mutate_at(c(vars(act_english_highest:act_composite_highest), vars(sat_total:sat_reading)),
            .funs = ~as.numeric(.)) %>% 
  mutate_at(.vars = c(vars(act_english_highest:act_composite_highest), vars(sat_total:sat_reading)),
            .funs = ~ if_else(. == 0, NA_real_, .)) %>% 
  mutate(
    cr_eng_app = if_else(act_english_highest >= 18, 1, 0),
    cr_math_app = if_else(act_math_highest >= 22, 1, 0),
    cr_read_app = if_else(act_reading_highest >= 22, 1, 0),
    cr_sci_app = if_else(act_science_highest >= 23, 1, 0),
    cr_all_app = if_else(cr_eng_app == 1 & cr_math_app == 1 & cr_read_app == 1 & cr_sci_app == 1, 1, 0)
  )

act_student_w_appeals <- act_student_level %>% 
  # mutate(sat_total = NA) %>% 
  left_join(appeals_tracker %>% select(student_key, act_english_highest:act_composite_highest, cr_eng_app:cr_all_app, sat_total),
            by = 'student_key') %>% 
  mutate(
    english = if_else(!is.na(act_english_highest), act_english_highest, english),
    math = if_else(!is.na(act_math_highest), act_math_highest, math),
    reading = if_else(!is.na(act_reading_highest), act_reading_highest, reading),
    science = if_else(!is.na(act_science_highest), act_science_highest, science),
    composite = if_else(!is.na(act_composite_highest), act_composite_highest, composite),
    cr_eng = if_else(!is.na(cr_eng_app), cr_eng_app, cr_eng),
    cr_math = if_else(!is.na(cr_math_app), cr_math_app, cr_math),
    cr_read = if_else(!is.na(cr_read_app), cr_read_app, cr_read),
    cr_sci = if_else(!is.na(cr_sci_app), cr_sci_app, cr_sci),
    cr_all = if_else(!is.na(cr_all_app), cr_all_app, cr_all)
  ) %>% 
  # mutate_at(
  #   .vars = c(vars(english:science), vars(cr_eng:cr_all)),
  #   .funs = ~if_else(!is.na(sat_total), NA_real_, .)
  # ) %>% 
  select(-(act_english_highest:cr_all_app))

act_student_level_post_appeals <- act_student_w_appeals %>% 
  mutate(
    # sat_total = NA, # Comment out for post appeals
    enrolled = 1,
    tested = !is.na(composite) | !is.na(sat_total),
    valid_test = !is.na(composite) | !is.na(sat_total),
    n_21_or_higher = (!is.na(composite) & composite >= 21) | (!is.na(sat_total) & sat_total >= 1060),
    n_below_19 = (!is.na(composite) & composite < 19) | (!is.na(sat_total) & sat_total < 980),
    n_21_or_higher_male = ((!is.na(composite) & composite >= 21) | (!is.na(sat_total) & sat_total >= 1060)) & gender == 'M',
    n_below_19_male = ((!is.na(composite) & composite < 19) | (!is.na(sat_total) & sat_total < 980)) & gender == 'M',
    n_21_or_higher_female = ((!is.na(composite) & composite >= 21) | (!is.na(sat_total) & sat_total >= 1060)) & gender == 'F',
    n_below_19_female = ((!is.na(composite) & composite < 19) | (!is.na(sat_total) & sat_total < 980)) & gender == 'F',
    met_CRB_english = cr_eng == 1,
    met_CRB_math = cr_math == 1,
    met_CRB_reading = cr_read == 1,
    met_CRB_science = cr_sci == 1,
    met_CRB_all = cr_all == 1,
    ED = ed == 'Y',
    BHN = ethnicity == 'H' | race_b == 'Y' | race_i == 'Y',
    EL = el == 'Y',
    SWD = swd =='Y',
    Asian = race_a == 'Y' & ethnicity != 'H' & race_b != 'Y' & race_i != 'Y' & race_p != 'Y',
    Black = race_b == 'Y' & ethnicity != 'H',
    Hispanic = ethnicity == 'H',
    Native = race_i == 'Y' & ethnicity != 'H' & race_b != 'Y',
    HPI = race_p == 'Y' & ethnicity != 'H' & race_b != 'Y' & race_i != 'Y',
    White = race_w == 'Y' & ethnicity != 'H' & race_b != 'Y' & race_i != 'Y' & race_p != 'Y' & race_a != 'Y',
    Non_ED = ed =='N',
    Non_EL = el == 'N',
    Non_SWD = swd == 'N'
  ) %>% 
  select(-(cr_eng:cr_all))

write_csv(act_student_level_post_appeals, "N:/ORP_accountability/data/2020_ACT/ACT_student_post_appeals_v1.csv", na = '')



