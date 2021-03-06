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

# Enrollment variables for alt and MSAA
# enrollment <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/enrollment.csv")

  

# EOCs
# ======================================= Fall EOC ===========================================================
fall_eoc <- read_fwf("N:/Assessment_Data Returns/TCAP_End-of-Course/2018-19/Fall EOC 2018/2018-2019 TN 2019 Fall EOC CDF Final Scores-20190211.txt",
                     col_types = 'icicccciicccciiiiiciic',
                       fwf_cols(system = c(7, 11), system_name = c(12, 86), school = c(87, 90),
                                school_name = c(91, 165), last_name = c(166, 200), first_name = c(201, 235),
                                middle_initial = c(236, 236), unique_student_id = c(245, 253), grade = c(274, 275),
                                content_area_code = c(278, 280), attempted = c(378, 378), modified_format = c(379, 380),
                                school_type = c(597, 597),teacher_of_record_tln = c(390, 409), reason_not_tested = c(592, 593), ri_status = c(594, 595),
                                raw_score = c(702, 704), scale_score= c(708, 711), performance_level = c(712, 726),
                                scale_score_lb_ci = c(730,732), scale_score_ub_ci = c(727,729), item_response_array=c(747,876)))

fall_eoc_total <- fall_eoc %>%
  mutate(
    school = case_when(
      system == 961L & school == 961L ~ 5L,
      system == 963L & school == 963L ~ 5L,
      TRUE ~ school
    ),
    ri_status = if_else(ri_status == 6L & reason_not_tested == 1L, 0L, ri_status)
  ) %>% 
  left_join(demos_filtered, by = c( "system", "school", "unique_student_id")) %>%
  select(system:attempted, gender, reported_race, bhn_group, hispanic, native_american:white, economically_disadvantaged, title_1, gifted, functionally_delayed,
         migrant, el, el_arrived_year_1:special_ed,  modified_format, enrolled_50_pct_district, enrolled_50_pct_school,
         teacher_of_record_tln:item_response_array) %>% 
  filter(
    system <= 986,  # Private School districts
    school != 981,  # Homeschool
    grade %in% 1:12 | is.na(grade)  # Grade 13
  ) %>%
  mutate(grade = if_else(grade %in% 1:2, NA_integer_, grade)) %>% 
  replace_na(list(bhn_group = 0)) %>% # race = 'Unknown', 
  select(-(hispanic:white)) %>% 
  mutate(
    test= 'EOC',
    semester = 'Fall'
  )

# ========================================== TCAP 3-8 ====================================================================
grade_3_8_TCAP <- read_fwf("N:\\Assessment_Data Returns\\TCAP_Grades 3-8\\2018-19\\2018-2019 TN 2019 Spring 3-8 CDF Final Scores-20190730_updated2019-08-01.Txt",
                           col_types = 'icicccciicccciiiiiciic', 
                           #n_max = 993639,
                           fwf_cols(system = c(7, 11), system_name = c(12, 86), school = c(87, 90),
                                    school_name = c(91, 165), last_name = c(166, 200), first_name = c(201, 235),
                                    middle_initial = c(236, 236), unique_student_id = c(245, 253), grade = c(276, 277),
                                    content_area_code = c(278, 280), attempted = c(378, 378), modified_format = c(379, 380),
                                    school_type = c(597, 597),teacher_of_record_tln = c(390, 409), reason_not_tested = c(592, 593), ri_status = c(594, 595),
                                    raw_score = c(702, 704), scale_score= c(708, 711), performance_level = c(712, 726),
                                    scale_score_lb_ci = c(730,732), scale_score_ub_ci = c(727,729), item_response_array=c(747,876)))

grade_3_8_total <- grade_3_8_TCAP %>%
  mutate(
    school = case_when(
      system == 961L & school == 961L ~ 5L, # Edit schools mislabeled in CDF
      system == 963L & school == 963L ~ 5L,
      TRUE ~ school
    ),
    # Change RI status for Failed Attemptedness because most had RI of 1 (Absent)
    ri_status = if_else(ri_status == 6L & reason_not_tested == 1L, 0L, ri_status) 
  ) %>% 
  left_join(demos_filtered, by = c("system", "school", "unique_student_id")) %>%
  select(system:attempted, gender, reported_race, bhn_group, hispanic, native_american:white, economically_disadvantaged, title_1, gifted, functionally_delayed,
         migrant, el, el_arrived_year_1:special_ed,  modified_format, enrolled_50_pct_district, enrolled_50_pct_school,
         teacher_of_record_tln:item_response_array) %>% 
  replace_na(list(bhn_group = 0)) %>% # race = 'Unknown', 
  mutate(grade = if_else(grade %in% 1:2, NA_integer_, grade)) %>% 
  filter(
    system <= 986,  # Private School districts
    school != 981,  # Homeschool
    grade %in% 1:12 | is.na(grade)  # Grade 13
  ) %>%
  select(-(hispanic:white)) %>% 
  mutate(
    test= 'TNReady',
    semester = 'Spring'
  )

# =================================== Spring EOC ==================================================
spring_eoc <- read_fwf("N:\\Assessment_Data Returns\\TCAP_End-of-Course\\2018-19\\Spring EOC 2019\\2018-2019 TN 2019 Spring EOC CDF Final Scores-20190629.txt",
                       col_types = 'icicccciicccciiiiiciic',
                        fwf_cols(system = c(7, 11), system_name = c(12, 86), school = c(87, 90),
                                 school_name = c(91, 165), last_name = c(166, 200), first_name = c(201, 235),
                                 middle_initial = c(236, 236), unique_student_id = c(245, 253), grade = c(274, 275),
                                 content_area_code = c(278, 280), attempted = c(378, 378), modified_format = c(379, 380),
                                 school_type = c(597, 597),teacher_of_record_tln = c(390, 409), reason_not_tested = c(592, 593), ri_status = c(594, 595),
                                 raw_score = c(702, 704), scale_score= c(708, 711), performance_level = c(712, 726),
                                 scale_score_lb_ci = c(730,732), scale_score_ub_ci = c(727,729), item_response_array=c(747,876)))


spring_eoc_total <- spring_eoc %>%
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
  replace_na(list( bhn_group = 0)) %>% # race = 'Unknown',
  filter(
    system <= 986,  # Private School districts
    school != 981,  # Homeschool
    grade %in% 1:12 | is.na(grade)  # Grade 13
  ) %>%
  mutate(grade = if_else(grade %in% 1:2, NA_integer_, grade)) %>% 
  select(-(hispanic:white)) %>%
  mutate(
    test= 'EOC',
    semester = 'Spring'
  )

# spring_eoc <- read_csv('N:/Assessment_Data Returns/TCAP_End-of-Course/2018-19/Spring EOC 2019/2018-2019 TN 2019 Spring EOC CDF Final Scores-20190613.csv')

# ================================= ALT Social Studies =====================================
alt_science_ss <- read_csv("N:/ORP_accountability/data/2019_cdf/2019_alt_ss_cdf.csv") %>% 
  mutate(
    test = "Alt-Social Studies",
    semester = "Spring",
    special_ed = 1L,
    performance_level = case_when(
      performance_level == "Level 3" ~ "Mastered",
      performance_level == "Level 2" ~ "On Track",
      performance_level == "Level 1" ~ "Approaching"
    )
  ) %>% 
  mutate(
    economically_disadvantaged = if_else(economically_disadvantaged == 1, 'Y', 'N'),
    bhn_group = if_else(reported_race %in% c("Black or African American", "Hispanic/Latino", "American Indian/Alaska Native"), 1, 0),
    ri_status = if_else(ri_status == 6L & reason_not_tested == 1L, 0L, as.integer(ri_status))
  ) %>% 
  filter(!(system == 750 & school == 0))


# =================================== Total TCAP/EOC ================================================
total_cdf <- bind_rows(fall_eoc_total, spring_eoc_total, grade_3_8_total, alt_science_ss) %>% 
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


math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")
science_eoc <- c("Biology I", "Chemistry")

# Integrated Math districts for reassigning MSAA subjects
int_math_systems <- total_cdf %>%
  filter(content_area_code %in% c("A1", "M1")) %>%
  count(system, content_area_code) %>%
  group_by(system) %>%
  mutate(temp = max(n)) %>%
  filter(n == temp, content_area_code == "M1") %>%
  magrittr::extract2("system") %>%
  as.integer()



# ========================================= MSAA ========================================
# MSAA
msaa <- read_csv("N:\\ORP_accountability\\data\\2019_cdf\\2019_msaa_cdf.csv") %>%
  mutate(
    school = case_when(
      system == 961 & school == 961 ~ 5,
      system == 963 & school == 963 ~ 5,
      TRUE ~ school
    )
  ) %>% 
  filter(!reporting_status %in% c("WDR", "NLE")) %>%
  mutate(
    test = "MSAA",
    semester = "Spring",
    special_ed = 1,# = 1
    performance_level = if_else(reporting_status != "TES", NA_character_, performance_level),
    # absent = 0,
    # enrolled = 1,
    tested = if_else(reporting_status == "DNT", 0, 1)
  ) %>%
  left_join(demos_filtered %>% select(system, school, unique_student_id, bhn_group), by = c("system", "school", "unique_student_id")) %>%
  replace_na(list(bhn_group = 0)) # %>%reported_race = 'Unknown', 



# ================================================ Student Level =====================================
student_level <- bind_rows(total_cdf %>% mutate(economically_disadvantaged=if_else(economically_disadvantaged=='Y', 1, 0)), msaa) %>% # , alt_science_ss
  mutate(
    enrolled = 1,
    tested = if_else(test != "MSAA", 1, tested), # MSAA already has a tested field
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
         semester, ri_status, medically_exempt, teacher_of_record_tln, reporting_status) %>%
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
    tested = case_when(
      test == "MSAA" & reporting_status == "DNT" ~ 0,
      breach_adult == 1 | breach_student ==1 | irregular_admin == 1 | incorrect_grade_subject == 1| refused_to_test == 1 | failed_attemptedness == 1 ~ 0,
      absent == 1 | not_enrolled == 1 | not_scheduled == 1 ~ 0,
      el_recently_arrived == 1L & is.na(original_performance_level) ~ 0,
      TRUE ~ 1
    ),
    performance_level = case_when(
      # Invalid performance level for values below, used to denote valid tests
      breach_adult == 1 | breach_student == 1 | irregular_admin==1 | incorrect_grade_subject == 1 | refused_to_test == 1 | failed_attemptedness == 1 ~ NA_character_,
      not_enrolled == 1 | not_scheduled == 1 | absent == 1 | medically_exempt == 1 | residential_facility == 1 | did_not_submit == 1~ NA_character_,
      el_recently_arrived == 1 ~ NA_character_,
      TRUE ~ performance_level
    ),
    # Modify subject for MSAA tests in grades >= 9 (6.8)
    subject = case_when(
      original_subject == "Math" & test == "MSAA" & grade >= 9 & system %in% int_math_systems ~ "Integrated Math I",
      original_subject == "Math" & test == "MSAA" & grade >= 9 & !(system %in% int_math_systems) ~ "Algebra I",
      original_subject == "ELA" & test == "MSAA" & grade >= 9 ~ "English II",
      TRUE ~ subject
    ),
    # Convert subjects per accountability rules
    subject = case_when(
      grade %in% 3:8 & original_subject %in% math_eoc ~ "Math",
      grade %in% 3:8 & original_subject %in% english_eoc ~ "ELA",
      grade %in% 3:8 & original_subject == "US History" ~ "Social Studies",
      TRUE ~ subject
    )
  ) %>% 
  select(-reporting_status)

# Records from Alternative, CTE, Adult HS are dropped from student level
alt_cte_adult <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
  transmute(system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER), cte_alt_adult = 1)

# Using enrollments to assign school and system for accountability
acct_system_school <- read_csv("N:\\ORP_accountability\\data\\2019_chronic_absenteeism\\student_chronic_absenteeism_Jul11.csv") %>%
  distinct() %>% 
  group_by(student_id, system, school) %>% 
  mutate(
    # Sum days across enrollments in the same system and school
    isp_days = sum(isp_days)
  ) %>% 
  ungroup() %>% 
  mutate(
    # Calculate the percent of the year student was enrolled
    enrolled_pct = round(isp_days/instructional_calendar_days * 100 + 1e-10, 2)
  ) %>% 
  # only keep students enrolled at least 50% of the year in the school
  filter(enrolled_pct >= 50) %>% 
  # Drop enrollments in alt, cte, adult schools
  anti_join(alt_cte_adult, by = c('system', 'school')) %>% 
  group_by(student_id) %>% 
  # finding the max pct_enrolled and max days enrolled for each student
  mutate(
    max_pct_school = max(enrolled_pct, na.rm = TRUE),
    max_days_school = max(isp_days, na.rm = TRUE),
    myorder_school = 1:n()
  ) %>% 
  ungroup() %>% 
  # keep enrollments with most days enrolled
  filter(max_days_school == isp_days) %>% 
  mutate(
    # assign acct_system and acct_school
    acct_system = case_when(
      isp_days == max_days_school & max_pct_school >= 50 ~ system,
      TRUE ~ NA_real_
    ),
    acct_school = case_when(
      isp_days == max_days_school & max_pct_school >= 50 ~ school,
      TRUE ~ NA_real_
    )
  ) %>% 
  # Filter out enrollments that don't receive an acct_system
  filter(!is.na(acct_system)) %>% 
  group_by(student_id) %>% 
  # Consolidating Enrollments
  mutate(
    max_days = max(isp_days, na.rm = TRUE),
    min_days = min(isp_days, na.rm = TRUE),
    max_system = max(system, na.rm = TRUE),
    min_system = min(system, na.rm = TRUE),
    max_school = max(school, na.rm = TRUE),
    min_school = min(school, na.rm = TRUE),
    max_count = n()
  ) %>% 
  ungroup() %>% 
  filter(!(max_days == min_days & max_count > 1 & max_system != min_system), 
         !(max_days == min_days & max_count > 1 & max_school != min_school)) %>% 
  group_by(student_id, acct_system) %>% 
  # First enrollment
  mutate(min_count = min(myorder_school, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # if multiple enrollments still exist, take the first one
  filter(myorder_school == min_count) %>% 
  mutate(
    acct_school = case_when(
      acct_system == 961 & acct_school == 961 ~ 5,
      acct_system == 963 & acct_school == 963 ~ 5,
      TRUE ~ school
    )
  ) %>% 
  select(unique_student_id = student_id, acct_system, acct_school)

# write_csv(acct_system_school %>% rename(state_student_id = unique_student_id), "N:/ORP_accountability/data/2019_final_accountability_files/enrollment_AM.csv")

dedup <- student_level %>%
  anti_join(alt_cte_adult, by = c("system", "school")) %>%
  # 1,559,451
  mutate(
    # For students with multiple records across test types, MSAA has priority, then EOC, then 3-8
    test_priority = case_when(
      test %in% c("MSAA", "Alt-Social Studies") ~ 3,
      test == "EOC" ~ 2,
      test == "TNReady" ~ 1
    )
  ) %>%
  group_by(state_student_id, subject) %>%
  mutate(temp = max(test_priority, na.rm = TRUE)) %>%
  filter(test_priority == temp | temp == -Inf) %>%
  select(-test_priority, -temp) %>%
  ungroup() %>%
  # 1,548,767
  # Lost 10,684 records
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
  # 1,548,046
  # Lost 721 records
  # For students with multiple records within the same performance level, take highest scale score
  group_by(state_student_id, original_subject, test, performance_level) %>%
  mutate(temp = max(scale_score, na.rm = TRUE)) %>%
  filter(scale_score == temp | temp == -Inf) %>%
  select(-temp) %>%
  ungroup() %>%
  # 1,547,348
  # Lost 698 records
  # For students with multiple test records with the same proficiency across administrations, take the most recent
  mutate(
    semester_priority = case_when(
      test %in% c("MSAA", "Alt-Social Studies", "Achievement") | (test == "EOC" & semester == "Spring") ~ 2,
      test == "EOC" & semester == "Fall" ~ 1
    )
  ) %>%
  group_by(state_student_id, subject, test) %>%
  mutate(temp = max(semester_priority, na.rm = TRUE)) %>%
  filter(semester_priority == temp | temp == -Inf | (is.na(state_student_id) & test == "Alt-Social Studies")) %>%
  select(-semester_priority, -temp) %>%
  ungroup() %>%
  # 1,547,259
  # Lost 89 records
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
  # 1,547,259
  # Lost 0 records
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
  #1,547,259


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
  select(system, system_name, school, school_name, test, original_subject, subject, semester,
         original_performance_level, performance_level, scale_score, enrolled, tested, valid_test,
         state_student_id, last_name, first_name, grade, gender, reported_race, bhn_group, teacher_of_record_tln,
         functionally_delayed, special_ed, economically_disadvantaged, gifted, migrant, el, t1234, el_recently_arrived,
         enrolled_50_pct_district, enrolled_50_pct_school, absent, 
         medically_exempt, 
         refused_to_test, residential_facility) %>%
  mutate(
    el = if_else(state_student_id %in% wida_current$student_id, 1, el) # If student appears in WIDA file, assign el to 1
  ) %>% 
  group_by(test, original_subject, grade) %>%
  # Percentiles by grade and original subject for 3-8
  mutate(
    rank = if_else(!is.na(scale_score), rank(scale_score, ties = "max"), NA_integer_),
    denom = sum(!is.na(scale_score)),
    percentile = if_else(test == "TNReady", round(100 * rank/denom + 1e-10, 1), NA_real_)
  ) %>% 
  ungroup() %>% 
  group_by(test, original_subject) %>%
  # Percentiles by original subject for EOC
  mutate(
    rank = if_else(!is.na(scale_score), rank(scale_score, ties = "max"), NA_integer_),
    denom = sum(!is.na(scale_score)),
    percentile = if_else(test == 'EOC', round(100 * rank/denom + 1e-10, 1), percentile)
  ) %>% 
  ungroup() %>% 
  select(-rank, -denom) %>% 
  left_join(acct_system_school %>% rename(state_student_id = unique_student_id), by = c('state_student_id')) %>% 
  mutate(
    acct_system = if_else(is.na(acct_system), system, acct_system),
    acct_school = if_else(is.na(acct_school), school, acct_school)
  ) %>% 
  arrange(system, school, state_student_id)
  


# Write out student level
write_csv(output, 'N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_medically_exempt_included.csv')

# compare student level files
alex_comp <- read_csv("N:\\ORP_accountability\\projects\\2019_student_level_file\\2019_student_level_file.csv")

diff_df <- setdiff(alex_comp %>% select(-percentile), output %>% select(-teacher_of_record_tln, -percentile)) %>% # , -percentile  %>% select(-percentile)      %>% select(-acct_system, -acct_school)
  bind_rows(setdiff(output %>% select(-teacher_of_record_tln, -percentile), alex_comp %>% select(-percentile))) %>% # %>% select(-percentile)   , -percentile                 %>% select(-acct_system, -acct_school)
  arrange(system, school, state_student_id, original_subject)


