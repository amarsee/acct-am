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

# Some schools have enrollments but no grade assignments
daily_pull <- dbGetQuery(con,
                       str_c("
                          SELECT --TO_CHAR(ABS_AGG.SCHOOL_YEAR) || '-' || TO_CHAR(ABS_AGG.SCHOOL_YEAR+1) AS SCHOOL_YEAR,
                                ABS_AGG.school_year, 
                                ABS_AGG.system, 
                             ABS_AGG.school, 
                             ABS_AGG.id_date,
                             ABS_AGG.n_absent,
                              ENR.n_enrolled,
                              (ENR.n_enrolled - ABS_AGG.n_absent) AS n_present,
                              ROUND(((ENR.n_enrolled - ABS_AGG.n_absent) / ENR.n_enrolled) * 100, 1) AS attendance_rate
                          FROM ( SELECT 
                                ABS_PULL.school_year, 
                                ABS_PULL.system, 
                                ABS_PULL.school, 
                                ABS_PULL.id_date,
                                COUNT(ALL ABS_PULL.attendance_type) AS n_absent
                          FROM (
                            SELECT 
                             TO_CHAR(SCAL.SCHOOL_YEAR) || '-' || TO_CHAR(SCAL.SCHOOL_YEAR+1) AS SCHOOL_YEAR,
                             S.DISTRICT_NO as system,
                             S.SCHOOL_NO as school,
                             S.SCHOOL_NAME,
                             SCAL.SCHOOL_BU_ID,
                             SCAL.ID_DATE,
                             stu_absent.attendance_type,
                             stu_absent.student_key
                             
                             FROM (SELECT DISTINCT 
                             SCAL_ID_DAYS.school_bu_id,
                             SCAL_ID_DAYS.school_year,
                             SCAL_ID_DAYS.id_date
                             FROM SCAL_ID_DAYS
                             WHERE SCAL_ID_DAYS.school_year = ",2020,") SCAL
                             LEFT JOIN (
                             SELECT DISTINCT
                             ISP.SCHOOL_BU_ID,
                             student_absences.attendance_date,
                             student_absences.attendance_type,
                             isp.student_key
                             FROM ISP
                             --LEFT JOIN EIS_MGR.INSTRUCTIONAL_GRADE IG ON ISP.ISP_ID = IG.ISP_ID
                             LEFT JOIN student_absences ON isp.isp_id = student_absences.isp_id
                             WHERE isp.school_year = ", 2020,"
                             AND isp.type_of_service = 'P'
                             ---AND IG.ASSIGNMENT NOT IN ('P3', 'P4')
                             AND attendance_type NOT IN ('D', 'P')
                             ) stu_absent ON scal.school_bu_id = stu_absent.school_bu_id
                             AND scal.id_date = stu_absent.attendance_date
                             LEFT JOIN EIS_MGR.SCHOOL S ON SCAL.SCHOOL_BU_ID = S.SCHOOL_BU_ID
                            ) ABS_PULL
                            GROUP BY ABS_PULL.school_year, ABS_PULL.system, ABS_PULL.school, ABS_PULL.id_date 
                          ) ABS_AGG
                            LEFT JOIN (
                                SELECT 
                             TO_CHAR(ISP.SCHOOL_YEAR) || '-' || TO_CHAR(ISP.SCHOOL_YEAR+1) AS SCHOOL_YEAR,
                             s.district_no as system,
                             s.school_no as school,
                             scal.id_date,
                             COUNT(isp.isp_id) as n_enrolled
                             FROM isp
                              JOIN EIS_MGR.SCHOOL S ON ISP.SCHOOL_BU_ID = S.SCHOOL_BU_ID
                            --LEFT JOIN EIS_MGR.INSTRUCTIONAL_GRADE IG ON ISP.ISP_ID = IG.ISP_ID
                            LEFT JOIN (SELECT DISTINCT 
                              SCAL_ID_DAYS.school_bu_id,
                              SCAL_ID_DAYS.school_year,
                              SCAL_ID_DAYS.id_date
                               FROM SCAL_ID_DAYS
                              WHERE SCAL_ID_DAYS.school_year = ",2020,") SCAL ON ISP.school_bu_id = SCAL.school_bu_id
                              AND SCAL.school_year = ISP.school_year
                             WHERE isp.type_of_service = 'P' 
                              --AND IG.ASSIGNMENT NOT IN ('P3', 'P4')
                              AND scal.id_date BETWEEN ISP.begin_date AND nvl(isp.end_date,  SYSDATE) -- DATE '2021-06-15'
                             
                             GROUP BY isp.school_year, s.district_no, s.school_no, scal.id_date
                            ) ENR
                          ON ABS_AGG.school_year = ENR.school_year and 
                              ABS_AGG.system = ENR.system and
                              ABS_AGG.school = ENR.school and
                              ABS_AGG.id_date =  ENR.id_date
                          WHERE ABS_AGG.id_date < TRUNC(SYSDATE, 'DD')
                             ")) %>%
  as_tibble() %>% 
  clean_names() %>% 
  arrange(system, school, id_date)


daily_attendance_w_features <- daily_pull %>% 
  mutate(id_date = as.Date(id_date)) %>% 
  group_by(school_year, system, school) %>% 
  mutate(
    school_day = row_number(),
    att_rate_previous_day = data.table::shift(attendance_rate),
    att_rate_5_day = round(data.table::frollmean(attendance_rate, n = 5, fill = NA), 1),
    att_rate_10_day = round(data.table::frollmean(attendance_rate, n = 10, fill = NA), 1),
    att_rate_20_day = round(data.table::frollmean(attendance_rate, n = 20, fill = NA), 1),
    att_rate_previous_day = if_else(
      is.na(att_rate_previous_day),
      attendance_rate,
      att_rate_previous_day
    ),
    att_rate_5_day = if_else(
      is.na(att_rate_5_day),
      round(cumsum(attendance_rate)/school_day, 1),
      att_rate_5_day
    ),
    att_rate_10_day = if_else(
      is.na(att_rate_10_day),
      round(cumsum(attendance_rate)/school_day, 1),
      att_rate_10_day
    ),
    att_rate_20_day = if_else(
      is.na(att_rate_20_day),
      round(cumsum(attendance_rate)/school_day, 1),
      att_rate_20_day
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    day_of_week = weekdays(id_date),
    cal_month = months(id_date)
  ) 

enrollment_2020 <- dbGetQuery(con,
                              str_c("
                                    SELECT *
                                    FROM ISP
                                    WHERE school_year = 2020
                                      AND begin_date < NVL(end_date, SYSDATE + 40)
                                    ")) %>%
  as_tibble() %>% 
  clean_names()

