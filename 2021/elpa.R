# ELPA Files
# 2021

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(RJDBC)
library(janitor)
library(readstata13)

con <-   dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="N:/ORP_accountability/ojdbc6.jar"),
  readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
  "EIS_MGR",
  readRegistry("Environment", "HCU")$EIS_MGR_PWD
)


# Function ===========================
agg_elpa <- function(df, ...) {
  df %>% 
    group_by(...) %>% 
    summarise(
      participation_denom = sum(participation_denom),
      n_participated = sum(participated),
      participation_rate = round(100 * sum(participated) / sum(participation_denom) + 1e-10, 0),
      exit_denom = sum(exit_denom, na.rm = TRUE),
      n_exit = sum(exit, na.rm = TRUE),
      growth_standard_denom = sum(growth_standard_denom, na.rm=TRUE), 
      n_met_growth_standard = sum(met_growth_standard, na.rm=TRUE),
      composite_average = round(mean(prof_composite, na.rm=TRUE)+1e-5,1),
      literacy_average = round(mean(prof_literacy, na.rm=TRUE)+1e-5,1)# ,
      # avg_composite_growth = round(mean(growth_standard_1yr, na.rm=TRUE)+1e-5,1)
    ) %>% 
    ungroup() %>% 
    mutate(
      pct_exit = round(n_exit / exit_denom * 100 + 1e-5, 1),
      pct_met_growth_standard = round(n_met_growth_standard / growth_standard_denom * 100 + 1e-5, 1)
    )
}

# EIS EL Pull ==========================
# Pull EL enrollments from EIS 
# Enrolled during the whole testing window
els <- dbGetQuery(
  con,
  "
  SELECT isp.school_year,
  isp.student_key as student_id,
  s.district_no as system,
  s.school_no as school,
  isp.first_name,
  isp.last_name,
  --sn.gender,
  isp.begin_date,
  isp.end_date,
  isp.withdrawal_reason,
  isp.english_language_background,
  EIS_MGR.FN_GET_IG(isp.ISP_ID) as grade
  FROM isp
  LEFT JOIN school s on isp.school_bu_id = s.school_bu_id
  LEFT JOIN (
    SELECT DISTINCT student_key,
            gender
    FROM  student_new
  ) sn ON sn.student_key = isp.student_key
  WHERE school_year = 2020
    AND english_language_background IN ('L', 'W')
    AND begin_date <= DATE '2021-02-01'
    AND (end_date IS NULL OR end_date > begin_date)
    AND (end_date IS NULL OR end_date >= DATE '2021-04-15')
    AND type_of_service = 'P'
    AND EIS_MGR.FN_GET_IG(isp.ISP_ID) NOT IN ('P3', 'P4')
  "
) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(
    last_name = str_to_upper(last_name),
    first_name = str_to_upper(first_name)
  ) %>% 
  arrange(system, school)

# Code ===============================

demo_combined <- read_csv("N:/ORP_accountability/projects/Andrew/Demographics/Data/student_demographics_06082021.csv")

demos_filtered <- demo_combined %>% 
  transmute(
    system = district_no,
    school = school_no,
    unique_student_id = student_key, 
    #grade,
    gender,
    hispanic = if_else(ethnicity == 'H', 'Y', 'N'),
    economically_disadvantaged = case_when(
      codeab == 1 ~ 'Y',
      codeab == 2 ~ 'N',
      TRUE ~ NA_character_
    ),
    reported_race = reportedrace,
    title_1 = title1,
    gifted = isgifted,
    functionally_delayed = isfunctionallydelayed,
    migrant = ismigrant,
    foster = isfostercare,
    el_arrived_year_1 = elrecentlyarrivedyearone,
    el_arrived_year_2 = elrecentlyarrivedyeartwo,
    el = isel,
    el_t1234 = t1t2,
    special_ed = specialeducation,
    ed = if_else(economically_disadvantaged == 'Y', 1, 0),
    enrolled_50_pct_district = district50percent,
    enrolled_50_pct_school = school50percent
  ) %>% 
  mutate(
    native_american = if_else(reported_race == 1, 1, 0),
    asian = if_else(reported_race == 2, 1, 0),
    black = if_else(reported_race == 3, 1, 0),
    hawaiian_pi = if_else(reported_race == 5, 1, 0),
    white = if_else(reported_race == 6, 1, 0),
    race = case_when(
      reported_race == 1 ~ 'American Indian/Alaska Native',
      reported_race == 2 ~ 'Asian',
      reported_race == 3 ~ 'Black or African American',
      reported_race == 4 ~ 'Hispanic/Latino',
      reported_race == 5 ~ 'Native Hawaiian/Pac. Islander',
      reported_race == 6 ~ 'White',
      TRUE ~ 'Unknown'
    ),
    bhn_group = if_else(reported_race %in% c(1,3,4), 1 ,0)
  )

school_names <- read_csv("N:/ORP_accountability/data/2021_final_accountability_files/names.csv")

dist_names <- school_names %>% 
  select(system, system_name) %>% 
  distinct()

# Need the prior year of ELPA data for one and two year growth standard
prior_year <- read_csv("N:/ORP_accountability/data/2020_ELPA/wida_growth_standard_student.csv",
                  col_types = cols(.default = "c")) %>% #,
  clean_names() %>%
  transmute(student_id = as.numeric(student_id),
         prof_composite_prior = as.numeric(prof_composite),
         prof_composite_two_years_prior = as.numeric(prof_composite_19))

two_prior <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_student.csv",
                  col_types = cols(.default = "c")) %>% #,
  clean_names() %>%
  transmute(student_id = as.numeric(student_id),
            prof_composite_two_years_prior = as.numeric(prof_composite))

prior <- bind_rows(
  prior_year,
  two_prior %>% 
    filter(!student_id %in% prior_year$student_id)
)

# Alt ============
wida_alt <- read_csv("N:/Assessment_Data Returns/ACCESS for ELs and ALT/2020-21/TN_Alternate_StudRR_File_2021-04-29.csv",
                     col_types = cols(.default = "c")) %>% 
  clean_names() %>% 
  transmute(
    student_id = as.numeric(state_student_id), # NAs introduced due to 3 students having characters in the id
    student_last_name = str_to_upper(student_last_name),
    student_first_name = str_to_upper(student_first_name),
    # gender,
    system = as.integer(str_sub(district_number, 3, 5)),
    school = school_number,
    grade = as.integer(grade),
    test = "WIDA Alternate ACCESS",
    listening_status,
    reading_status,
    speaking_status,
    writing_status,
    scale_score_listening = listening_scale_score,
    scale_score_reading = reading_scale_score,
    scale_score_speaking = speaking_scale_score,
    scale_score_writing = writing_scale_score,
    scale_score_comprehension = comprehension_score,
    scale_score_oral = oral_scale_score,
    scale_score_literacy = literacy_scale_score,
    scale_score_composite = composite_overall_scale_score
    # prof_listening = listening_proficiency_level,
    # prof_reading = reading_proficiency_level,
    # prof_speaking = speaking_proficiency_level,
    # prof_writing = writing_proficiency_level,
    # prof_comprehension = comprehension_proficiency_level,
    # prof_oral = oral_proficiency_level,
    # prof_literacy = literacy_proficiency_level,
    # prof_composite = composite_overall_proficiency_level
  )


# elpa <- read.dta13('N:/Assessment_Data Returns/ACCESS for ELs and ALT/2017-18/WIDA/20180706_WIDA_AccessResults_SY2017-18_Whalen_v3.dta') %>% 
elpa <- read_csv("N:/Assessment_Data Returns/ACCESS for ELs and ALT/2020-21/TN_Summative_StudRR_File_2021-04-29.csv",
                 col_types = cols(.default = "c")) %>% 
  clean_names() %>% 
  # 12 records with WWWW school number
  # 49,364
  # filter(
  #   reported_mode != "Mixed",
  #   !(grade == 0 & cluster_listening != 0 & !is.na(cluster_listening)),
  #   !(grade == 1 & cluster_listening != 1 & !is.na(cluster_listening)),
  #   !(grade == 2 & reported_mode == "Paper" & cluster_listening != 2 & !is.na(cluster_listening)),
  #   !(grade == 3 & reported_mode == "Paper" & cluster_listening != 3 & !is.na(cluster_listening)),
  #   !(grade %in% 2:3 & reported_mode == "Online" & cluster_listening != 2 & !is.na(cluster_listening)),
  #   !(grade %in% 4:5 & cluster_listening != 4 & !is.na(cluster_listening)),
  #   !(grade %in% 6:8 & cluster_listening != 6 & !is.na(cluster_listening)),
  #   !(grade %in% 9:12 & cluster_listening != 9 & !is.na(cluster_listening))
  # ) %>%
  # 49,361 if left in. Currently commenting out above filter
  # filter(!is.na(state_student_id), !is.na(composite_overall_scale_score)) %>%
  transmute(
    student_id = as.numeric(state_student_id), # NAs introduced due to 3 students having characters in the id
    student_last_name = str_to_upper(student_last_name),
    student_first_name = str_to_upper(student_first_name),
    # gender,
    system = as.integer(str_sub(district_number, 3, 5)),
    school = school_number,
    grade = as.integer(grade),
    test = "WIDA ACCESS",
    listening_status,
    reading_status,
    speaking_status,
    writing_status,
    scale_score_listening = listening_scale_score,
    scale_score_reading = reading_scale_score,
    scale_score_speaking = speaking_scale_score,
    scale_score_writing = writing_scale_score,
    scale_score_comprehension = comprehension_scale_score,
    scale_score_oral = oral_scale_score,
    scale_score_literacy = literacy_scale_score,
    scale_score_composite = composite_overall_scale_score,
    prof_listening = listening_proficiency_level,
    prof_reading = reading_proficiency_level,
    prof_speaking = speaking_proficiency_level,
    prof_writing = writing_proficiency_level,
    prof_comprehension = comprehension_proficiency_level,
    prof_oral = oral_proficiency_level,
    prof_literacy = literacy_proficiency_level,
    prof_composite = composite_overall_proficiency_level
  ) %>%
  bind_rows(wida_alt) %>%
  mutate_at(vars(starts_with("scale_score_"), starts_with("prof_"), school, system), as.numeric) %>%
  full_join(
    els %>% 
      select(student_id, system, school),
    by = c("system", "school", "student_id")
  ) %>% 
  # Getting names added in for ELs in EIS, but not WIDA
  left_join(
    els %>% 
      mutate(
        grade = case_when(
          grade == "K" ~ "0",
          str_detect(grade, "T") ~ gsub("T", "0", grade),
          TRUE ~ grade
        )
      ) %>% 
      select(student_id, system, school, first_name_eis = first_name, last_name_eis = last_name, 
             grade_eis = grade), # gender_eis = gender, 
    by = c("system", "school", "student_id")
  ) %>% 
  mutate(
    test = if_else(is.na(test), "WIDA ACCESS", test), # If missing, WIDA ACCESS. Missing if EL in EIS but not in CDF
    source = case_when(
      is.na(student_first_name) ~ "EIS",
      TRUE ~ test
    ),
    student_first_name = if_else(is.na(student_first_name), first_name_eis, student_first_name),
    student_last_name = if_else(is.na(student_last_name), last_name_eis, student_last_name),
    # gender = if_else(is.na(gender), gender_eis, gender),
    grade = if_else(is.na(grade), as.integer(grade_eis), grade)
  ) %>% 
  select(-(first_name_eis:grade_eis)) %>% 
  # 6 have bad IDs, all missing
  # Yuli Juan Pedro in Bedford has 2 records, one with missing ID and one not
  # 11 in system 40 and 1 in 390 are missing school numbers they have WWWW for the school
  filter(!is.na(student_id), !is.na(school)) # %>%  # , !is.na(scale_score_composite),
  # mutate(
  #   school = if_else(system == 792 & school == 2596, 2598, school)
  # )

# subgroups <- c("American Indian or Alaska Native", "Asian", "Black or African American", "Black/Hispanic/Native American",
#                "Economically Disadvantaged", "English Learners", "Hispanic", 
#                "Native Hawaiian or Other Pacific Islander", "Students with Disabilities", "White")

elpa_total <- elpa %>% 
  # deduplicate - drops all duplicates 
  # Keep Alt if there is one
  arrange(student_id, desc(test)) %>% 
  group_by(student_id) %>% 
  mutate(temp = first(test)) %>% 
  filter(test == temp) %>% 
  ungroup() %>% 
  # keep max composite first
  arrange(student_id, -scale_score_composite) %>% 
  group_by(student_id) %>%
  mutate(student_max_comp = first(scale_score_composite)) %>% 
  ungroup() %>% 
  filter(scale_score_composite == student_max_comp | is.na(student_max_comp)) %>% 
  # next keep max literacy
  arrange(student_id, -scale_score_literacy) %>% 
  group_by(student_id) %>% 
  mutate(student_max_lit = first(scale_score_literacy)) %>% 
  filter(scale_score_literacy == student_max_lit | is.na(student_max_lit)) %>% 
  ungroup() %>% 
  # next keep max reading
  # Added in this to resolve few remaining duplicates
  arrange(student_id, -scale_score_reading) %>%
  group_by(student_id) %>%
  mutate(student_max_reading = first(scale_score_reading)) %>%
  filter(scale_score_reading == student_max_reading | is.na(student_max_reading)) %>%
  ungroup() %>%
  left_join(prior, by = 'student_id') %>% 
  mutate(
    exit_denom = if_else(!is.na(prof_composite) & !is.na(prof_literacy), 1, 0),
    # valid_test = if_else(!is.na(prof_composite) & !is.na(prof_literacy), 1, 0),
    # TN WIDA exit criteria
    exit = if_else(prof_literacy >= 4.2 & prof_composite >= 4.4, 1, 0), # 2020-21 exit criteria
    # met_exit_criteria = if_else(prof_literacy >= 4.2 & prof_composite >= 4.4, 1, 0), # 2020-21 exit criteria
    growth_standard_denom = if_else((!is.na(prof_composite) & !is.na(prof_composite_prior)) |
                                      (!is.na(prof_composite) & !is.na(prof_composite_two_years_prior)), 1,0),
    growth_standard_1yr = case_when(
      is.na(prof_composite_prior) ~ NA_real_,
      prof_composite_prior <= 1.4 ~ 1.3,
      prof_composite_prior <= 1.9 ~ 0.7,
      prof_composite_prior <= 2.4 ~ 0.8,
      prof_composite_prior <= 2.9 ~ 0.7,
      prof_composite_prior <= 3.4 ~ 0.4,
      prof_composite_prior <= 3.9 ~ 0.5,
      prof_composite_prior <= 4.4 ~ 0.4,
      prof_composite_prior <= 4.9 ~ 0.2
    ),
    growth_standard_2yr = case_when(
      is.na(prof_composite_two_years_prior) ~ NA_real_,
      prof_composite_two_years_prior <= 1.4 ~ round(1.3 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 1.9 ~ round(0.7 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 2.4 ~ round(0.8 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 2.9 ~ round(0.7 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 3.4 ~ round(0.4 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 3.9 ~ round(0.5 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 4.4 ~ round(0.4 + prof_composite_two_years_prior + 1e-5, 1),
      prof_composite_two_years_prior <= 4.9 ~ round(0.2 + prof_composite_two_years_prior + 1e-5, 1)
    ),
    growth_standard_2yr = case_when(
      is.na(growth_standard_2yr) ~ NA_real_,
      growth_standard_2yr <= 1.4 ~ round(1.3 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 1.9 ~ round(0.7 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 2.4 ~ round(0.8 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 2.9 ~ round(0.7 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 3.4 ~ round(0.4 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 3.9 ~ round(0.5 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 4.4 ~ round(0.4 + growth_standard_2yr + 1e-5, 1),
      growth_standard_2yr <= 4.9 ~ round(0.2 + growth_standard_2yr + 1e-5, 1)
    ),
    met_growth_standard = case_when(
      growth_standard_denom == 0 ~ NA_integer_,
      round(prof_composite - prof_composite_prior + 1e-5, 1) >= growth_standard_1yr ~ 1L,
      prof_composite >= growth_standard_2yr ~ 1L,
      # round(prof_composite - prof_composite_prior + 1e-5, 1) >= growth_standard_1yr | round(prof_composite - prof_composite_two_years_prior + 1e-5, 1) >= growth_standard_2yr ~ 1L,
      TRUE ~ 0L
    ),
    met_growth_standard = if_else(growth_standard_denom == 1L & exit == 1L, 1L, met_growth_standard),
    participation_denom = if_else(grade >= 3, 1, 0),
    participated = case_when(
      grade < 3 ~ 0,
      # Started on any domain is counted as participated
      listening_status %in% c("C", "P") ~ 1,
      reading_status %in% c("C", "P") ~ 1,
      speaking_status %in% c("C", "P") ~ 1,
      writing_status %in% c("C", "P") ~ 1,
      TRUE ~ 0
    )
  )

elpa_with_demo <- elpa_total %>% 
  mutate(school = as.numeric(school)) %>% 
  left_join(demos_filtered %>% rename(student_id = unique_student_id), 
            by = c('student_id', 'system', 'school')) %>% 
  mutate(el = 1) # %>% 
# replace_na(list(ed=0, swd=0))

# 169 records missing demographics (0.3%)

out_df <- bind_rows(
  elpa_with_demo %>% mutate(subgroup = 'All Students'),
  elpa_with_demo %>% filter(native_american == 1) %>% mutate(subgroup = "American Indian or Alaska Native"),
  elpa_with_demo %>% filter(asian == 1) %>% mutate(subgroup = "Asian"),
  elpa_with_demo %>% filter(black == 1) %>% mutate(subgroup = "Black or African American"),
  elpa_with_demo %>% filter(bhn_group == 1) %>% mutate(subgroup = "Black/Hispanic/Native American"),
  elpa_with_demo %>% filter(ed == 1) %>% mutate(subgroup = "Economically Disadvantaged"),
  elpa_with_demo %>% filter(ed == 0 | is.na(ed)) %>% mutate(subgroup = "Non-Economically Disadvantaged"),
  elpa_with_demo %>% filter(el == 1) %>% mutate(subgroup = "English Learners"),
  elpa_with_demo %>% filter(hispanic == 'Y') %>% mutate(subgroup = "Hispanic"),
  elpa_with_demo %>% filter(hawaiian_pi == 1) %>% mutate(subgroup = "Native Hawaiian or Other Pacific Islander"),
  elpa_with_demo %>% filter(white == 1) %>% mutate(subgroup = "White"),
  elpa_with_demo %>% filter(special_ed == 1) %>% mutate(subgroup = "Students with Disabilities"),
  elpa_with_demo %>% filter(special_ed == 0 | is.na(special_ed)) %>% mutate(subgroup = "Non-Students with Disabilities")
)


# ================================= State Level ========================================

state_level <- out_df %>% 
  # filter(grade >= 3) %>% 
  agg_elpa(subgroup) %>% 
  transmute(
    subgroup, 
    participation_denom,
    n_participated,
    participation_rate,
    exit_denom, 
    n_exit, 
    pct_exit, 
    growth_standard_denom, 
    n_met_growth_standard,pct_met_growth_standard, literacy_average, composite_average # , avg_composite_growth
  ) %>% 
  arrange(subgroup)

# write state level csv
write_csv(state_level, "N:/ORP_accountability/data/2021_ELPA/wida_growth_standard_state.csv", na = '')

# ===================================== District Level =====================================

district_level <- out_df %>% 
  agg_elpa(system, subgroup) %>% 
  left_join(dist_names, by = 'system') %>% 
  transmute(
    system, system_name,
    subgroup, 
    participation_denom,
    n_participated,
    participation_rate,
    exit_denom, 
    n_exit, 
    pct_exit, 
    growth_standard_denom, 
    n_met_growth_standard,
    pct_met_growth_standard = if_else(is.na(pct_met_growth_standard), NA_real_, pct_met_growth_standard), 
    literacy_average, composite_average# , avg_composite_growth
  ) %>% 
  arrange(system, subgroup)

# write state level csv
write_csv(district_level, "N:/ORP_accountability/data/2021_ELPA/wida_growth_standard_district.csv", na = '')

# ===================================== School Level ===============================================

school_level <- out_df %>% 
  agg_elpa(system, school, subgroup) %>% 
  left_join(school_names, by = c('system', 'school')) %>% 
  transmute(
    system, system_name, school, school_name,
    subgroup, 
    participation_denom,
    n_participated,
    participation_rate,
    exit_denom,
    n_exit,
    pct_exit, 
    growth_standard_denom, 
    n_met_growth_standard,
    pct_met_growth_standard = if_else(is.na(pct_met_growth_standard), NA_real_, pct_met_growth_standard), 
    literacy_average, composite_average# , avg_composite_growth
  ) %>% 
  arrange(system, school, subgroup)

# write school level csv
write_csv(school_level, "N:/ORP_accountability/data/2021_ELPA/wida_growth_standard_school.csv", na = '')

# ====================================== Student Level ==========================================
# Add in status and participation fields
elpa_student_level <- elpa_with_demo %>% 
  left_join(school_names, by = c('system', 'school')) %>% 
  transmute(system, system_name, school, school_name, student_id, 
            student_last_name, student_first_name,
            grade, gender, 
            test, source,
            listening_status,
            reading_status,
            speaking_status,
            writing_status,
            scale_score_listening, scale_score_reading, scale_score_speaking,
            scale_score_writing, scale_score_comprehension, scale_score_oral, scale_score_literacy, scale_score_composite,
            prof_listening, prof_reading, prof_speaking, prof_writing, prof_comprehension, prof_oral, prof_literacy, prof_composite,
            BHN = bhn_group, 
            Hispanic = if_else(hispanic == 'Y', 1, 0),
            Black = black,
            Native = native_american,
            HPI = hawaiian_pi,
            Asian = asian,
            White = white,
            SWD = special_ed,
            ED = if_else(economically_disadvantaged == 'Y', 1, 0),
            EL = 1,
            gifted,
            migrant,
            foster,
            prof_composite_20 = prof_composite_prior,
            prof_composite_19 = prof_composite_two_years_prior,
            participation_denom,
            participated,
            exit_denom, 
            exit, 
            growth_standard_denom, growth_standard_1yr, growth_standard_2yr,
            met_growth_standard
  ) %>% 
  arrange(system, school, student_id)

# write csv
write_csv(elpa_student_level, "N:/ORP_accountability/data/2021_ELPA/wida_growth_standard_student.csv", na = '')


# =================== Split Files ======================
# Split district file
district_numbers <- sort(unique(elpa_student_level$system))

district_level %>%
  group_split(system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/data/2021_ELPA/split/", .y,
      "_2021_ACCESSDistrictLevelFile_", format(Sys.Date(), "%d%b%Y"),
      ".csv"
    ), na = "")
  )


# Split school file
school_level %>%
  group_split(system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/data/2021_ELPA/split/", .y,
      "_2021_ACCESSSchoolLevelFile_", format(Sys.Date(), "%d%b%Y"), ".csv"
    ), na = "")
  )

# Split student level file
elpa_student_level %>%
  group_split(system) %>%
  walk2(
    .x = .,
    .y = district_numbers,
    .f = ~ write_csv(.x, path = paste0(
      "N:/ORP_accountability/data/2021_ELPA/split/", .y,
      "_2021_ACCESSStudentLevelFile_", format(Sys.Date(), "%d%b%Y"), ".csv"
    ), na = "")
  )






elpa %>%
# wida_alt %>%
  anti_join(
    school_names,
    by = c("system", "school")
  ) %>% 
  View()

