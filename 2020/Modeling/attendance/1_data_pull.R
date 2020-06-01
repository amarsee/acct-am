# Pull attendance data for the last 5 years
# Daily attendance rate

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

test <- read_csv("N:/ORP_accountability/projects/Andrew/Data Requests/2020/Loss of Instruction Effect 03302020/data/consecutive_absence_pull.csv")

test %>% 
  group_by(system, school) %>% 
  summarise(
    cal_days = max(isp_days),
    att_days = n_distinct(scal_date)
  ) %>% 
  View()


test_query <- dbGetQuery(con,
                         "SELECT 
                          S.DISTRICT_NO as system,
                              S.SCHOOL_NO as school,
                              COUNT(SCAL.ID_DATE) as n_days
                          --S.SCHOOL_BU_ID,
                          --SCAL.SCHOOL_YEAR,
                          --SCAL.ID_DATE
                         FROM EIS_MGR.SCAL_ID_DAYS SCAL
                          LEFT JOIN EIS_MGR.SCHOOL S ON SCAL.SCHOOL_BU_ID = S.SCHOOL_BU_ID
                         WHERE  school_year = 2019
                         GROUP BY S.DISTRICT_NO, S.SCHOOL_NO
                        
                         ") %>%
  as.tbl()

test_query_2 <- dbGetQuery(con,
                         "SELECT  *
                          FROM school
                          WHERE  rownum < 1000
                         ") %>%
  as.tbl()

test_2 <- dbGetQuery(con,
                         str_c("SELECT 
                          --isp.isp_id,
                          --isp.student_key,
                          isp.school_year,
                          --isp.school_bu_id,
                          --isp.begin_date,
                          --nvl(isp.end_date,  DATE '", 2005,"-06-10' )   as end_date,
                          isp.primary_district_id as system,
                          isp.primary_school_id as school,
                          scal.id_date,
                          COUNT(isp.isp_id) as n_enrolled
                          FROM isp
                          LEFT JOIN (SELECT DISTINCT 
                                  SCAL_ID_DAYS.school_bu_id,
                                  SCAL_ID_DAYS.school_year,
                                  SCAL_ID_DAYS.id_date
                                  FROM SCAL_ID_DAYS
                                  WHERE SCAL_ID_DAYS.school_year = ",2004,") SCAL ON ISP.school_bu_id = SCAL.school_bu_id
                                                                AND SCAL.school_year = ISP.school_year
                          WHERE isp.type_of_service = 'P'
                            AND scal.id_date BETWEEN ISP.begin_date AND nvl(isp.end_date,  DATE '", 2005,"-06-15' )
                            
                          GROUP BY isp.school_year, isp.primary_district_id, isp.primary_school_id, scal.id_date
                            -- AND rownum < 100000
                            --AND attendance_type = 'P'
                            --AND isp_id = 10298312
                          -- AND district_no = 12 AND school_no = 40
                         ") ) %>%
  as.tbl()


test_3 <- dbGetQuery(con,
                         str_c("SELECT 
                          TO_CHAR(SCAL.SCHOOL_YEAR) || '-' || TO_CHAR(SCAL.SCHOOL_YEAR+1) AS SCHOOL_YEAR,
                          S.DISTRICT_NO as system,
                              S.SCHOOL_NO as school,
                              S.SCHOOL_NAME,
                              --COUNT(SCAL.ID_DATE) as n_days
                          SCAL.SCHOOL_BU_ID,
                          SCAL.ID_DATE,
                          stu_absent.attendance_type,
                          stu_absent.student_key
                          
                         FROM (SELECT DISTINCT 
                                  SCAL_ID_DAYS.school_bu_id,
                                  SCAL_ID_DAYS.school_year,
                                  SCAL_ID_DAYS.id_date
                                  FROM SCAL_ID_DAYS
                                  WHERE SCAL_ID_DAYS.school_year = ",2018,") SCAL
                        LEFT JOIN (
                          SELECT DISTINCT
                          ISP.SCHOOL_BU_ID,
                          student_absences.attendance_date,
                          student_absences.attendance_type,
                          isp.student_key
                          FROM ISP
                          LEFT JOIN EIS_MGR.INSTRUCTIONAL_GRADE IG ON ISP.ISP_ID = IG.ISP_ID
                          LEFT JOIN student_absences ON isp.isp_id = student_absences.isp_id
                          WHERE isp.school_year = ", 2018,"
                            AND isp.type_of_service = 'P'
                            AND IG.ASSIGNMENT NOT IN ('P3', 'P4')
                            AND attendance_type <> 'P'
                        ) stu_absent ON scal.school_bu_id = stu_absent.school_bu_id
                              AND scal.id_date = stu_absent.attendance_date
                          LEFT JOIN EIS_MGR.SCHOOL S ON SCAL.SCHOOL_BU_ID = S.SCHOOL_BU_ID
                         --GROUP BY S.DISTRICT_NO, S.SCHOOL_NO, S.SCHOOL_NAME
                        -- WHERE rownum < 1000
                         ")) %>%
  as.tbl()

daily_present <- test_3 %>% 
  janitor::clean_names() %>% 
  group_by(system, school, id_date) %>% 
  summarise(
    n_present = sum(!is.na(attendance_type))
  ) %>% 
  group_by(system, school) %>% 
  mutate(max_present = max(n_present)) %>% 
  ungroup() %>% 
  filter(max_present != 0)

daily_abs_df <- tibble()

for (year in 2012:2019) {
  abs_pull <- dbGetQuery(con,
                       str_c("SELECT 
                             TO_CHAR(SCAL.SCHOOL_YEAR) || '-' || TO_CHAR(SCAL.SCHOOL_YEAR+1) AS SCHOOL_YEAR,
                             S.DISTRICT_NO as system,
                             S.SCHOOL_NO as school,
                             S.SCHOOL_NAME,
                             --COUNT(SCAL.ID_DATE) as n_days
                             SCAL.SCHOOL_BU_ID,
                             SCAL.ID_DATE,
                             stu_absent.attendance_type,
                             stu_absent.student_key
                             
                             FROM (SELECT DISTINCT 
                             SCAL_ID_DAYS.school_bu_id,
                             SCAL_ID_DAYS.school_year,
                             SCAL_ID_DAYS.id_date
                             FROM SCAL_ID_DAYS
                             WHERE SCAL_ID_DAYS.school_year = ",year,") SCAL
                             LEFT JOIN (
                             SELECT DISTINCT
                             ISP.SCHOOL_BU_ID,
                             student_absences.attendance_date,
                             student_absences.attendance_type,
                             isp.student_key
                             FROM ISP
                             LEFT JOIN EIS_MGR.INSTRUCTIONAL_GRADE IG ON ISP.ISP_ID = IG.ISP_ID
                             LEFT JOIN student_absences ON isp.isp_id = student_absences.isp_id
                             WHERE isp.school_year = ", year,"
                             AND isp.type_of_service = 'P'
                             AND IG.ASSIGNMENT NOT IN ('P3', 'P4')
                             AND attendance_type <> 'P'
                             ) stu_absent ON scal.school_bu_id = stu_absent.school_bu_id
                             AND scal.id_date = stu_absent.attendance_date
                             LEFT JOIN EIS_MGR.SCHOOL S ON SCAL.SCHOOL_BU_ID = S.SCHOOL_BU_ID
                             --GROUP BY S.DISTRICT_NO, S.SCHOOL_NO, S.SCHOOL_NAME
                             -- WHERE rownum < 1000
                             ")) %>%
  as.tbl()

  daily_absence <- abs_pull %>% 
    janitor::clean_names() %>% 
    group_by(school_year, system, school, id_date) %>% 
    summarise(
      n_absent = sum(!is.na(attendance_type))
    ) %>% 
    group_by(school_year, system, school) %>% 
    mutate(max_abs = max(n_absent)) %>% 
    ungroup() %>% 
    filter(max_abs != 0)
  
  daily_abs_df <- bind_rows(daily_abs_df, daily_absence)
}

write_csv(daily_abs_df, 'N:/ORP_accountability/projects/Andrew/acct-am/2020/Modeling/attendance/data/daily_absences_2012-19.csv', na = '')


daily_enrollment_df <- tibble()

for (year in 2012:2019) {
  enr_pull <- dbGetQuery(con,
                         str_c("SELECT 
                               --isp.isp_id,
                               --isp.student_key,
                               isp.school_year,
                               --isp.school_bu_id,
                               --isp.begin_date,
                               --nvl(isp.end_date,  DATE '", year + 1,"-06-10' )   as end_date,
                                --isp.primary_district_id as system,
                                --isp.primary_school_id as school,
                               s.district_no as system,
                               s.school_no as school,
                               scal.id_date,
                               COUNT(isp.isp_id) as n_enrolled
                               FROM isp
                              JOIN EIS_MGR.SCHOOL S ON ISP.SCHOOL_BU_ID = S.SCHOOL_BU_ID
                              LEFT JOIN EIS_MGR.INSTRUCTIONAL_GRADE IG ON ISP.ISP_ID = IG.ISP_ID
                               LEFT JOIN (SELECT DISTINCT 
                               SCAL_ID_DAYS.school_bu_id,
                               SCAL_ID_DAYS.school_year,
                               SCAL_ID_DAYS.id_date
                               FROM SCAL_ID_DAYS
                               WHERE SCAL_ID_DAYS.school_year = ",year,") SCAL ON ISP.school_bu_id = SCAL.school_bu_id
                                                      AND SCAL.school_year = ISP.school_year
                               WHERE isp.type_of_service = 'P' 
                                AND IG.ASSIGNMENT NOT IN ('P3', 'P4')
                               AND scal.id_date BETWEEN ISP.begin_date AND nvl(isp.end_date,  DATE '", year + 1,"-06-15' )
                               
                               GROUP BY isp.school_year, s.district_no, s.school_no, scal.id_date
                                --GROUP BY isp.school_year, isp.primary_district_id, isp.primary_school_id, scal.id_date
                               -- AND rownum < 100000
                               --AND attendance_type = 'P'
                               --AND isp_id = 10298312
                               -- AND district_no = 12 AND school_no = 40
                               ") ) %>%
  as.tbl() %>% 
    janitor::clean_names()

  daily_enrollment_df <- bind_rows(daily_enrollment_df, enr_pull) %>% 
    arrange(system, school, school_year, id_date)
}

write_csv(daily_enrollment_df, 'N:/ORP_accountability/projects/Andrew/acct-am/2020/Modeling/attendance/data/daily_enrollment_2012-19.csv', na = '')



