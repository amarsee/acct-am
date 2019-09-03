library(tidyverse)
library(janitor)
library(readxl)
library(readstata13)

# demographics <- read_csv("N:/Assessment_Data Returns/TCAP_End-of-Course/2018-19/Demographic Files/fall_eoc_demographics_snapshot_20181208.csv")

# demographics_2019_spring <- read_csv('N:\\TNReady\\2018-19\\spring\\demographics\\spring_2019_assessment_demographics_20190510.csv')
demographics_2019_spring <- read_csv('N:/TNReady/2018-19/spring/demographics/spring_2019_assessment_demographics_combined_pull_20190610.csv')

demos_filtered <- demographics_2019_spring %>% 
  filter(str_length(student_key) == 7) %>% 
  transmute(
    unique_student_id = student_key,
    system = district_id,
    school = school_id,
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
    el_arrived_year_1 = elrecentlyarrivedyearone,
    el_arrived_year_2 = elrecentlyarrivedyeartwo,
    el = isel,
    t1234 = t1t2,
    special_ed = specialeducation,
    enrolled_50_pct_district = district50percent,
    enrolled_50_pct_school = school50percent
  ) %>% 
  mutate(
    native_american = if_else(reported_race == 1, 'Y', 'N'),
    asian = if_else(reported_race == 2, 'Y', 'N'),
    black = if_else(reported_race == 3, 'Y', 'N'),
    hawaiian_pi = if_else(reported_race == 5, 'Y', 'N'),
    white = if_else(reported_race == 6, 'Y', 'N'),
    reported_race = case_when(
      reported_race == 1 ~ 'American Indian/Alaska Native',
      reported_race == 2 ~ 'Asian',
      reported_race == 3 ~ 'Black or African American',
      reported_race == 4 ~ 'Hispanic/Latino',
      reported_race == 5 ~ 'Native Hawaiian/Pac. Islander',
      reported_race == 6 ~ 'White',
      TRUE ~ 'Unknown'
    ),
    bhn_group = if_else(!reported_race %in% c('American Indian/Alaska Native','Black or African American','Hispanic/Latino') | is.na(reported_race), 0, 1)
  )

# =============================================== Grade 2 ========================================================================
grade_2 <- read_fwf("N:/Assessment_Data Returns/TCAP Grades 2/SY2018-19/2018-2019 TN 2019 Grade 2 CDF Final Scores - 20190812.Txt",
                           col_types = 'icicccciicccciiiiiciic', 
                           #n_max = 993639,
                           fwf_cols(system = c(7, 11), system_name = c(12, 86), school = c(87, 90),
                                    school_name = c(91, 165), last_name = c(166, 200), first_name = c(201, 235),
                                    middle_initial = c(236, 236), unique_student_id = c(245, 253), grade = c(276, 277),
                                    content_area_code = c(278, 280), attempted = c(378, 378), modified_format = c(379, 380),
                                    school_type = c(597, 597),teacher_of_record_tln = c(390, 409), reason_not_tested = c(592, 593), ri_status = c(594, 595),
                                    raw_score = c(702, 704), scale_score= c(708, 711), performance_level = c(712, 726),
                                    scale_score_lb_ci = c(730,732), scale_score_ub_ci = c(727,729), item_response_array=c(747,876)))

grade_2_total <- grade_2 %>%
  # mutate(
  #   grade = case_when(
  #     grade =='K' ~ '0',
  #     grade == 'T7' ~ '07',
  #     grade == 'T8' ~ '08',
  #     TRUE ~ grade
  #     ),
  #   grade = as.numeric(grade)
  # ) %>% 
  mutate(
    school = case_when(
      system == 961L & school == 961L ~ 5L,
      system == 963L & school == 963L ~ 5L,
      TRUE ~ school
    ),
    ri_status = if_else(ri_status == 6L & reason_not_tested == 1L, 0L, ri_status)
  ) %>% 
  left_join(demos_filtered, by = c("system", "school", "unique_student_id")) %>%
  select(system:attempted, gender, reported_race, bhn_group, hispanic, native_american:white, economically_disadvantaged, title_1, gifted, functionally_delayed,
         migrant, el, el_arrived_year_1:special_ed,  modified_format, enrolled_50_pct_district, enrolled_50_pct_school,
         teacher_of_record_tln:item_response_array) %>% 
  replace_na(list(bhn_group = 0)) %>% # race = 'Unknown', 
  # mutate(grade = if_else(grade %in% 1:2, NA_integer_, grade)) %>% 
  filter(
    system <= 986,  # Private School districts
    school != 981,  # Homeschool
    grade %in% 1:12 | is.na(grade)  # Grade 13
  ) %>%
  select(-(hispanic:white)) %>% 
  mutate(
    test= 'Grade 2'#,
    # semester = 'Spring'
  )

# ================================= Grade 2 ALT =====================================
grade_2_alt <- read_fwf("N:/Assessment_Data Returns/TCAP Grades 2 Alt/2018-2019 TN Grade 2 Alt CDF Final Scores - 20190729.txt",
                           col_types = 'icicccciccccciiiiiciic',
                           fwf_cols(system = c(7, 11), system_name = c(12, 86), school = c(87, 90),
                                    school_name = c(91, 165), last_name = c(166, 200), first_name = c(201, 235),
                                    middle_initial = c(236, 236), unique_student_id = c(245, 253), grade = c(276, 277),
                                    content_area_code = c(278, 280), attempted = c(378, 378), modified_format = c(379, 380),
                                    school_type = c(597, 597),teacher_of_record_tln = c(390, 409), reason_not_tested = c(592, 593), ri_status = c(594, 595),
                                    raw_score = c(702, 704), scale_score= c(708, 711), performance_level = c(712, 726),
                                    scale_score_lb_ci = c(730,732), scale_score_ub_ci = c(727,729), item_response_array=c(747,876))) %>%
  mutate(
    test = "Grade 2 Alt",
    grade = if_else(grade == "HS", "11", grade),
    # semester = "Spring",
    performance_level = case_when(
      performance_level == "Level 3" ~ "Mastered",
      performance_level == "Level 2" ~ "On Track",
      performance_level == "Level 1" ~ "Approaching"
    ),
    absent = if_else(reason_not_tested == 1, 1,0),
    not_enrolled = if_else(reason_not_tested == 2, 1,0),
    not_scheduled = if_else(reason_not_tested == 3, 1 ,0),
    medically_exempt = if_else(reason_not_tested == 4, 1,0),
    residential_facility = if_else(reason_not_tested == 5, 1,0),
    did_not_submit = if_else(reason_not_tested == 7, 1,0),
    breach_adult = if_else(ri_status == 1, 1,0),
    breach_student = if_else(ri_status == 2, 1,0),
    irregular_admin = if_else(ri_status == 3, 1,0),
    incorrect_grade_subject = if_else(ri_status == 4, 1,0),
    refused_to_test = if_else(ri_status == 5, 1,0),
    failed_attemptedness = if_else(ri_status == 6, 1,0),
    original_subject = case_when(
      content_area_code == "EN" | content_area_code == "ENG" ~ "ELA",
      content_area_code == "MA" | content_area_code == "MAT" ~ "Math",
      # content_area_code == "SCI" ~ "Science",
      content_area_code == "SS" | content_area_code == "SOC" | content_area_code == "SCI" ~ "Social Studies",
      content_area_code == "A1" ~ "Algebra I",
      content_area_code == "A2" ~ "Algebra II",
      content_area_code == "B1" ~ "Biology I",
      content_area_code == "C1" ~ "Chemistry",
      content_area_code == "E1" ~ "English I",
      content_area_code == "E2" ~ "English II",
      content_area_code == "E3" ~ "English III",
      content_area_code == "G1" ~ "Geometry",
      content_area_code == "M1" ~ "Integrated Math I",
      content_area_code == "M2" ~ "Integrated Math II",
      content_area_code == "M3" ~ "Integrated Math III",
      content_area_code == "U1" ~ "US History"
    )
  ) %>%
  left_join(demos_filtered, by = c("unique_student_id", "system", "school")) %>%
  select(system:attempted, gender, reported_race, bhn_group, economically_disadvantaged, title_1:migrant, el,
         el_arrived_year_1, el_arrived_year_2, t1234, special_ed,modified_format, enrolled_50_pct_district,
         enrolled_50_pct_school, teacher_of_record_tln:item_response_array, test, absent:failed_attemptedness, original_subject) %>%
  replace_na(list(bhn_group = 0)) %>%
  mutate(grade = as.numeric(grade))


grade_2_alt_total <- grade_2_alt %>% 
  mutate(
    special_ed = 1L
  ) %>% 
  mutate(
    economically_disadvantaged = if_else(economically_disadvantaged == 1, 'Y', 'N'),
    bhn_group = if_else(reported_race %in% c("Black or African American", "Hispanic/Latino", "American Indian/Alaska Native"), 1, 0),
    ri_status = if_else(ri_status == 6L & reason_not_tested == 1L, 0L, as.integer(ri_status))
  ) %>% 
  filter(!(system == 750 & school == 0))

# ====================================== CDF ==============================
total_cdf <- bind_rows(grade_2_total) %>% 
  filter(content_area_code != 'E3') %>% 
  filter(!(unique_student_id == 4244992 & content_area_code == 'ENG')) %>% 
  mutate(
    performance_level = if_else(performance_level == "On track", "On Track", performance_level),
    absent = if_else(reason_not_tested == 1, 1,0),
    not_enrolled = if_else(reason_not_tested == 2, 1,0),
    not_scheduled = if_else(reason_not_tested == 3, 1 ,0),
    medically_exempt = if_else(reason_not_tested == 4, 1,0),
    residential_facility = if_else(reason_not_tested == 5, 1,0),
    did_not_submit = if_else(reason_not_tested == 7, 1,0),
    breach_adult = if_else(ri_status == 1, 1,0),
    breach_student = if_else(ri_status == 2, 1,0),
    irregular_admin = if_else(ri_status == 3, 1,0),
    incorrect_grade_subject = if_else(ri_status == 4, 1,0),
    refused_to_test = if_else(ri_status == 5, 1,0),
    failed_attemptedness = if_else(ri_status == 6, 1,0),
    original_subject = case_when(
      content_area_code == "EN" | content_area_code == "ENG" ~ "ELA",
      content_area_code == "MA" | content_area_code == "MAT" ~ "Math",
      # content_area_code == "SCI" ~ "Science",
      content_area_code == "SS" | content_area_code == "SOC" | content_area_code == "SCI" ~ "Social Studies",
      content_area_code == "A1" ~ "Algebra I",
      content_area_code == "A2" ~ "Algebra II",
      content_area_code == "B1" ~ "Biology I",
      content_area_code == "C1" ~ "Chemistry",
      content_area_code == "E1" ~ "English I",
      content_area_code == "E2" ~ "English II",
      content_area_code == "E3" ~ "English III",
      content_area_code == "G1" ~ "Geometry",
      content_area_code == "M1" ~ "Integrated Math I",
      content_area_code == "M2" ~ "Integrated Math II",
      content_area_code == "M3" ~ "Integrated Math III",
      content_area_code == "U1" ~ "US History",
      TRUE ~ NA_character_
    )
  ) 


student_level <- bind_rows(total_cdf %>% mutate(economically_disadvantaged=if_else(economically_disadvantaged=='Y', 1, 0)), 
                           grade_2_alt %>% mutate(economically_disadvantaged=if_else(economically_disadvantaged=='Y', 1, 0))) %>% # , alt_science_ss
  mutate(
    enrolled = 1,
    # tested = 1, # MSAA already has a tested field
    tested = if_else(!is.na(performance_level), 1, 0),
    valid_test = NA_integer_, # initialize valid tests and assign it later
    # economically_disadvantaged = if_else(economically_disadvantaged == 'Y', 1, 0),
    el = if_else(el == 1, 1, 0), 
    el_recently_arrived = if_else(el_arrived_year_1 == 1 | el_arrived_year_2 == 1, 1, 0),
    t1234 = if_else(t1234 %in% 1:4, 1, 0), # Transitional given a 0 or 1 instead of 0-4
    special_ed = if_else(special_ed == 1, 1, 0),
    functionally_delayed = if_else(functionally_delayed == 1, 1,0),
    # homebound = homebound == "Y",
    original_performance_level = performance_level,
    subject = original_subject
  ) %>%
  select(system, school, test, original_subject, subject, 
         original_performance_level, performance_level, scale_score,
         enrolled, tested, valid_test, state_student_id = unique_student_id,
         last_name, first_name, grade, gender, reported_race, bhn_group, gifted, functionally_delayed, special_ed,
         economically_disadvantaged, migrant, el, t1234, el_recently_arrived,
         enrolled_50_pct_district, enrolled_50_pct_school, absent, not_enrolled, not_scheduled,
         breach_adult, breach_student, irregular_admin, incorrect_grade_subject,
         refused_to_test, failed_attemptedness, residential_facility, did_not_submit,
         ri_status, medically_exempt, teacher_of_record_tln) %>%
  # Drop excluded records
  filter(!is.na(system),
         grade != 13 | is.na(grade),
         !(school %in% c(981,982) | system >= 990)#, # 981 is homeschool  residential_facility != 1 | is.na(residential_facility),
         # Drop medically exempt?
  ) %>%
  # Apply testing flag hierarchy
  # Absent (reason_not_tested 1) students have a missing proficiency and are not tested
  # EL Recently Arrived students with missing proficiency are not considered tested
  # EL Recently Arrived students performance level are converted to missing
  # Proficiency modified to missing if refused to test or failed attemptedness
  # Any record with an RI status of 0 or 3 (Irregular Administration) is enrolled and tested, but do not have performance levels
  # Any record with an RI status other than 0 or 3 is neither enrolled nor tested
  mutate(
    enrolled = case_when(
      breach_adult == 1 | breach_student == 1 | irregular_admin==1 | incorrect_grade_subject == 1 | refused_to_test == 1 | failed_attemptedness == 1 ~ 0,
      not_enrolled == 1 | not_scheduled == 1 ~ 0,
      TRUE ~ 1
    ),
    # tested = case_when(
    #   breach_adult == 1 | breach_student ==1 | irregular_admin == 1 | incorrect_grade_subject == 1| refused_to_test == 1 | failed_attemptedness == 1 ~ 0,
    #   absent == 1 | not_enrolled == 1 | not_scheduled == 1 ~ 0,
    #   el_recently_arrived == 1L & is.na(original_performance_level) ~ 0,
    #   TRUE ~ 1
    # ),
    performance_level = case_when(
      # Invalid performance level for values below, used to denote valid tests
      breach_adult == 1 | breach_student == 1 | irregular_admin==1 | incorrect_grade_subject == 1 | refused_to_test == 1 | failed_attemptedness == 1 ~ NA_character_,
      not_enrolled == 1 | not_scheduled == 1 | absent == 1 | medically_exempt == 1 | residential_facility == 1 | did_not_submit == 1~ NA_character_,
      el_recently_arrived == 1 ~ NA_character_,
      TRUE ~ performance_level
    )
  )

# Records from Alternative, CTE, Adult HS are dropped from student level
alt_cte_adult <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
  transmute(system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER), cte_alt_adult = 1)

dedup <- student_level %>%
  anti_join(alt_cte_adult, by = c("system", "school")) %>%
  mutate(
    # For students with multiple records across test types, MSAA has priority, then EOC, then 3-8
    test_priority = case_when(
      test == "Grade 2 Alt" ~ 2,
      test == "Grade 2" ~ 1
    )
  ) %>%
  group_by(state_student_id, subject) %>%
  mutate(temp = max(test_priority, na.rm = TRUE)) %>%
  filter(test_priority == temp | temp == -Inf) %>%
  select(-test_priority, -temp) %>%
  ungroup() %>%
  # For students with multiple records within the same test, take highest proficiency level
  mutate(
    prof_priority = case_when(
      performance_level %in% c("Below", "Below Basic") ~ 1,
      performance_level %in% c("Approaching", "Basic") ~ 2,
      performance_level %in% c("On Track", "Proficient") ~ 3,
      performance_level %in% c("Mastered", "Advanced") ~ 4
    )
  ) %>%
  group_by(state_student_id, original_subject, test) %>%
  mutate(temp = max(prof_priority, na.rm = TRUE)) %>%
  filter(prof_priority == temp | temp == -Inf) %>% # | (is.na(state_student_id) & test == "Alt-Social Studies")) %>%
  select(-prof_priority, -temp) %>%
  ungroup() %>%
  # For students with multiple records within the same performance level, take highest scale score
  group_by(state_student_id, original_subject, test, performance_level) %>%
  mutate(temp = max(scale_score, na.rm = TRUE)) %>%
  filter(scale_score == temp | temp == -Inf) %>%
  select(-temp) %>%
  ungroup() %>%
  # Deduplicate by missing demographic, grade
  # demographic
  mutate(
    demo_priority = case_when(
      reported_race %in% c("American Indian/Alaska Native", "Asian", "Black or African American", "Native Hawaiian/Pac. Islander",
                           "Hispanic/Latino", "White") ~ 2,
      reported_race == 'Unknown' | is.na(reported_race) ~ 1
    )
  ) %>%
  group_by(state_student_id, original_subject, test, performance_level) %>% 
  mutate(temp = max(demo_priority, na.rm = TRUE)) %>%
  filter(demo_priority == temp | temp == -Inf) %>%
  select(-demo_priority, -temp) %>%
  ungroup() %>% 
  # grade
  mutate(
    grade_priority = case_when(
      !is.na(grade) ~ 2,
      is.na(grade) ~ 1
    )
  ) %>%
  group_by(state_student_id, original_subject, test, performance_level) %>% 
  mutate(temp = max(grade_priority, na.rm = TRUE)) %>%
  filter(grade_priority == temp | temp == -Inf) %>%
  select(-grade_priority, -temp) %>%
  ungroup() %>% 
  # Valid test if there is a performance level
  mutate(valid_test = as.numeric(!is.na(performance_level)))

school_names <- read_csv("N:\\ORP_accountability\\data\\2019_final_accountability_files\\names.csv") %>% 
  bind_rows(
    tribble(
      ~system, ~system_name, ~school, ~school_name,
      970, "Department of Children's Services", 25, "Gateway to Independence",
      # 970, "Department of Children's Services", 45, "Wilder Youth Development Center",
      970, "Department of Children's Services", 65, "Mountain View Youth Development Center"# ,
      # 970, "Department of Children's Services", 140, "DCS Affiliated Schools"
    )
  )

# read in WIDA ACCESS file
wida_current <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_student.csv")

# add percentiles
output <- dedup %>%
  filter(!(original_subject == "Science" & grade %in% c("3", "4"))) %>%
  left_join(school_names, by = c("system", "school")) %>%
  mutate(
    system_name = case_when(
      system == 970 & !is.na(system_name) ~ "Department Of Children's Services Education Division",
      TRUE ~ system_name
    ),
    school_name = case_when(
      system == 792 & school == 8228 ~ "Southern Avenue Charter School Of Academic Excellence  Creative Arts",
      system == 330 & school == 58 ~ "Dupont Elementary",
      system == 330 & school == 8002 ~ "Ivy Academy, Inc.",
      TRUE ~ school_name
    )
  ) %>% 
  select(system, system_name, school, school_name, test, original_subject, subject, 
         original_performance_level, performance_level, scale_score, enrolled, tested, valid_test,
         state_student_id, last_name, first_name, grade, gender, reported_race, bhn_group, teacher_of_record_tln,
         functionally_delayed, special_ed, economically_disadvantaged, gifted, migrant, el, t1234, el_recently_arrived,
         enrolled_50_pct_district, enrolled_50_pct_school, absent, refused_to_test, residential_facility) %>%
  mutate(
    el = if_else(state_student_id %in% wida_current$student_id, 1, el) # If student appears in WIDA file, assign el to 1
  ) %>% 
  group_by(test, original_subject, grade) %>%
  # Percentiles by grade and original subject for 3-8
  mutate(
    rank = if_else(!is.na(scale_score), rank(scale_score, ties = "max"), NA_integer_),
    denom = sum(!is.na(scale_score)),
    percentile = if_else(test == "Grade 2", round(100 * rank/denom + 1e-10, 1), NA_real_)
  ) %>% 
  ungroup() %>% 
  mutate(
    enrolled = 1
  ) %>% 
  select(-rank, -denom) %>% 
  arrange(system, school, state_student_id)

write_csv(output, "N:/ORP_accountability/projects/2019_grade_2_assessment/grade_2_student_level_AM.csv", na = '')







