# Pull to update attendance

# ATTENDANCE CODES
# A	Absent
# T	Present for Transportation
# P	Present
# U	Unexcused Absence
# X	Unexecused Absence, present for transportation
# D	Distance Learning / Virtual Classrooms

options(java.parameters = "-Xmx16G")

library(acct)
library(tidyverse)
library(janitor)
library(lubridate)
library(RJDBC)

con <- dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="N:/ORP_accountability/ojdbc6.jar"),
  readRegistry("Environment", hive = "HCU")$EIS_MGR_CXN_STR[1],
  "EIS_MGR",
  readRegistry("Environment", hive = "HCU")$EIS_MGR_PWD[1]
)

# =============== Data Pulls ====================
student_daily_virtual <- dbGetQuery(con,
                                    str_c("
                                          SELECT 
                                          TO_CHAR(SCAL.SCHOOL_YEAR) || '-' || TO_CHAR(SCAL.SCHOOL_YEAR+1) AS SCHOOL_YEAR,
                                          S.district_no as system,
                                          S.school_no as school,
                                          SCAL.SCHOOL_BU_ID,
                                          SCAL.ID_DATE,
                                          stu_absent.student_key,
                                          stu_absent.first_name,
                                          stu_absent.middle_name,
                                          stu_absent.last_name,
                                          stu_absent.grade,
                                          stu_absent.attendance_type
                                          
                                          
                                          FROM (SELECT DISTINCT 
                                          SCAL_ID_DAYS.school_bu_id,
                                          SCAL_ID_DAYS.school_year,
                                          SCAL_ID_DAYS.id_date
                                          FROM SCAL_ID_DAYS
                                          WHERE SCAL_ID_DAYS.school_year = ",2020,") SCAL
                                          LEFT JOIN (
                                          SELECT DISTINCT
                                          ISP.SCHOOL_BU_ID,
                                          isp.primary_district_id as system,
                                          isp.primary_school_id as school,
                                          isp.first_name,
                                          isp.middle_name,
                                          isp.last_name,
                                          ig.grade,
                                          student_absences.attendance_date,
                                          student_absences.attendance_type,
                                          isp.student_key
                                          FROM ISP
                                          LEFT JOIN (
                                          SELECT *
                                          FROM (SELECT ig.isp_id,
                                          ig.assignment as grade,
                                          dense_rank() over (partition by student_key order by ig_begin_date desc) rnk
                                          FROM instructional_grade ig)
                                          WHERE rnk = 1
                                          AND grade NOT IN ('P3', 'P4')
                                          ) ig ON ig.isp_id = isp.isp_id
                                          LEFT JOIN student_absences ON isp.isp_id = student_absences.isp_id
                                          WHERE isp.school_year = ", 2020,"
                                          AND isp.type_of_service = 'P'
                                          --AND attendance_type NOT IN ('P')
                                          --AND isp.student_key = 3194068
                                          ) stu_absent ON scal.school_bu_id = stu_absent.school_bu_id
                                          AND scal.id_date = stu_absent.attendance_date
                                          LEFT JOIN school S ON scal.school_bu_id = S.school_bu_id
                                          WHERE SCAL.id_date <= TRUNC(SYSDATE, 'DD')-1
                                          
                                          ")) %>%
  as_tibble() %>% 
  clean_names() %>% 
  filter(!is.na(student_key), grade %in% c('K', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10':'12')) %>% 
  arrange(system, school, student_key, id_date)


enrollment_2020 <- dbGetQuery(con,
                              str_c("
                                    SELECT DISTINCT isp.school_year,
                                    isp.student_key,
                                    S.district_no as system,
                                    S.school_no as school,
                                    isp.student_pin,
                                    isp.begin_date,
                                    isp.end_date,
                                    isp.enrollment_reason,
                                    isp.withdrawal_reason,
                                    isp.primary_district_id,
                                    isp.primary_school_id,
                                    ig.grade
                                    FROM ISP
                                    LEFT JOIN (
                                    SELECT *
                                    FROM (SELECT ig.isp_id,
                                    ig.assignment as grade,
                                    dense_rank() over (partition by student_key order by ig_begin_date desc) rnk
                                    FROM instructional_grade ig)
                                    WHERE rnk = 1
                                    AND grade NOT IN ('P3', 'P4')
                                    ) ig ON ig.isp_id = isp.isp_id
                                    LEFT JOIN school S ON isp.school_bu_id = S.school_bu_id
                                    WHERE school_year = 2020
                                    AND type_of_service = 'P'
                                    ")) %>%
  as_tibble() %>% 
  clean_names()

# ================= Assessment Files ===================

fall_eoc_2019 <- read_csv('N:/ORP_accountability/projects/2020_student_level_file/2020_student_level_file.csv')
district_assmt <- read_csv("N:/ORP_accountability/data/2020_final_accountability_files/district_assessment_file.csv") %>% 
  filter(year == 2020,
         subgroup == "All Students",
         grade == "All Grades",
         enrolled >= 10)

school_assmt <- read_csv("N:/ORP_accountability/data/2020_final_accountability_files/school_assessment_file.csv") %>% 
  filter(year == 2020,
         subgroup == "All Students",
         grade == "All Grades",
         enrolled >= 5)

# ================= Fall EOC Registrations =====================
eoc_list <- readxl::read_excel("C:/Users/ca20593/Downloads/FallEOCSchoolsDistricts.xlsx") %>% 
  rename(
    system = DistrictID,
    school = SchoolID
  )

eoc_student_list <- readxl::read_excel("C:/Users/ca20593/Downloads/FallEOCstudentTests.xlsx") %>% 
  transmute(
    system = DistrictID,
    school = SchoolID,
    student_key = USID
  )

# ==================== Analysis ==================
school_hs_enr <- enrollment_2020 %>% 
  mutate(begin_date = as.Date(begin_date), end_date = as.Date(end_date)) %>% 
  filter(grade %in% c('09', '10', '11', '12'),
         begin_date <= as.Date("2020-10-01") & (is.na(end_date) | end_date >=as.Date("2020-10-02"))) %>% 
  group_by(system, school) %>% 
  summarise(n_enrolled_hs_20_21 = n())

school_total_enr <- enrollment_2020 %>% 
  mutate(begin_date = as.Date(begin_date), end_date = as.Date(end_date)) %>% 
  filter(student_key %in% eoc_student_list$student_key,
         begin_date <= as.Date("2020-10-01") & (is.na(end_date) | end_date >=as.Date("2020-10-02"))) %>% 
  group_by(system, school) %>% 
  summarise(n_enrolled_20_21 = n())

all_codes_sch_id_date <- student_daily_virtual %>% 
  filter(student_key %in% eoc_student_list$student_key) %>% 
  # filter(grade %in% c('09', '10', '11', '12')) %>%
  # filter(attendance_type == "D") %>% # attendance_type == "D", system %in% system_list, 
  group_by(system, school, id_date) %>% 
  summarise(
    n_students = n()
  ) %>% 
  ungroup() %>% 
  # mutate(id_date = as.Date(id_date)) %>% 
  # select(-attendance_type) %>% 
  arrange(id_date) %>% 
  pivot_wider(names_from = "id_date", values_from = "n_students") %>% 
  clean_names() # %>% 
  # mutate(diff = x2020_09_08_00_00_00_0 - x2020_09_03_00_00_00_0 ) # x2020_08_28_00_00_00_0 - x2020_08_31_00_00_00_0

d_code_sch_id_date <- student_daily_virtual %>% 
  filter(student_key %in% eoc_student_list$student_key) %>% 
  # filter(grade %in% c('09', '10', '11', '12')) %>%
  filter(attendance_type == "D") %>% # attendance_type == "D", system %in% system_list, 
  group_by(system, school, id_date) %>% 
  summarise(
    n_students = n()
  ) %>% 
  ungroup() %>% 
  # mutate(id_date = as.Date(id_date)) %>% 
  # select(-attendance_type) %>% 
  arrange(id_date) %>% 
  pivot_wider(names_from = "id_date", values_from = "n_students") %>% 
  clean_names() #%>% 
  # mutate(diff = x2020_09_08_00_00_00_0 - x2020_09_03_00_00_00_0 ) # x2020_08_28_00_00_00_0 - x2020_08_31_00_00_00_0

school_assmt_wide <- school_assmt %>% 
  select(system:school_name, subject, tested) %>% 
  pivot_wider(names_from = "subject", values_from = "tested")

sch_dist_data <- school_assmt_wide %>% 
  rowwise() %>% 
  mutate(
    Math = sum(`Algebra I`, `Algebra II`, `Geometry`, `Integrated Math I`, `Integrated Math II`, `Integrated Math III`, na.rm = TRUE),
    English = sum(`English I`, `English II`)
  ) %>% 
  ungroup() %>% 
  # left_join(
  #   school_hs_enr, by = c("system", "school")
  # ) %>% 
  left_join(school_total_enr, by = c("system", "school")) %>% 
  left_join(
    all_codes_sch_id_date %>% 
      select(system, school, all_att_codes_Sep_29 = x2020_09_29_00_00_00_0),
    by = c("system", "school")
  ) %>% 
  left_join(
    d_code_sch_id_date %>% 
      select(system, school, d_code_Sep_29 = x2020_09_29_00_00_00_0),
    by = c("system", "school")
  ) %>% 
  arrange(system, school)

write_csv(sch_dist_data, "N:/ORP_accountability/projects/Andrew/acct-am/2020/Modeling/attendance/data/tested_fall_eoc_2019_with_20-21_enrollment_attendance.csv", na = "")


sch_eoc_data <- distinct(eoc_student_list, system, school) %>%  
  # left_join(
  #   school_hs_enr, by = c("system", "school")
  # ) %>% 
  left_join(school_total_enr, by = c("system", "school")) %>% 
  left_join(
    all_codes_sch_id_date %>% 
      select(system, school, all_att_codes_Sep_28 = x2020_09_28_00_00_00_0, all_att_codes_Sep_29 = x2020_09_29_00_00_00_0,
             all_att_codes_Sep_30 = x2020_09_30_00_00_00_0, all_att_codes_Oct_01 = x2020_10_01_00_00_00_0,
             all_att_codes_Oct_02 = x2020_10_02_00_00_00_0),
    by = c("system", "school")
  ) %>% 
  left_join(
    d_code_sch_id_date %>% 
      select(system, school, d_code_Sep_28 = x2020_09_28_00_00_00_0, d_code__Sep_29 = x2020_09_29_00_00_00_0,
             d_code__Sep_30 = x2020_09_30_00_00_00_0, d_code__Oct_01 = x2020_10_01_00_00_00_0,
             d_code__Oct_02 = x2020_10_02_00_00_00_0),
    by = c("system", "school")
  ) %>% 
  arrange(system, school)

write_csv(sch_eoc_data, "N:/ORP_accountability/projects/Andrew/acct-am/2020/Modeling/attendance/data/school_specific_students_multiple_days_20-21_enrollment_attendance.csv", na = "")

specific_student_attendance <- student_daily_virtual %>% 
  filter(student_key %in% eoc_student_list$student_key) %>% 
  mutate(id_date = as.Date(id_date)) %>% 
  filter(
    id_date >= as.Date("2020-09-16")
  ) %>% 
  group_by(system, school, student_key) %>% 
  mutate(
    all_code_total = n(),
    d_code_total = sum(attendance_type == "D"),
    absence_code_total = sum(attendance_type %in% c("A", "U", "X", "T"))
  ) %>% 
  ungroup() %>% 
  # select(-attendance_type) %>% 
  arrange(id_date) %>% 
  pivot_wider(names_from = "id_date", values_from = "attendance_type") %>% 
  clean_names() %>% 
  # rename_at(.vars = vars(x2020_07_22_00_00_00_0:x2020_10_13_00_00_00_0),
  #           .funs = ~gsub("_00_00_00_0", "", .)) %>% 
  # select(school_year:grade, x2020_09_16:x2020_10_13) %>% 
  arrange(system, school, student_key) #%>% 
  # rowwise() %>% 
  # mutate(
  #   total_att_codes = select(., x2020_09_16:x2020_10_13) %>% apply(1, sum, na.rm=TRUE)
  # ) #%>% 
  # ungroup()

stu_eoc_out <- eoc_student_list %>% 
  left_join(specific_student_attendance, by = c("system", "school", "student_key")) %>% 
  arrange(system, school, student_key)

write_csv(stu_eoc_out, "N:/ORP_accountability/projects/Andrew/acct-am/2020/Modeling/attendance/data/student_fall_eoc_attendance.csv", na = "")





