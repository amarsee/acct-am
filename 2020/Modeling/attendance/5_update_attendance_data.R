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

# ======================= Student Daily Absences ==========================
student_daily_absences <- dbGetQuery(con,
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
                              ig.assignment as grade,
                               student_absences.attendance_date,
                               student_absences.attendance_type,
                               isp.student_key
                               FROM ISP
                               LEFT JOIN EIS_MGR.INSTRUCTIONAL_GRADE IG ON ISP.ISP_ID = IG.ISP_ID
                               LEFT JOIN student_absences ON isp.isp_id = student_absences.isp_id
                               WHERE isp.school_year = ", 2020,"
                               AND isp.type_of_service = 'P'
                               AND IG.ASSIGNMENT NOT IN ('P3', 'P4')
                               AND attendance_type NOT IN ('D', 'P')
                                --AND isp.student_key = 3194068
                               ) stu_absent ON scal.school_bu_id = stu_absent.school_bu_id
                                              AND scal.id_date = stu_absent.attendance_date
                                LEFT JOIN school S ON scal.school_bu_id = S.school_bu_id
                                WHERE SCAL.id_date < TRUNC(SYSDATE, 'DD')
                                    
                               ")) %>%
  as_tibble() %>% 
  clean_names() %>% 
  filter(!is.na(student_key), grade %in% c('K', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10':'12')) %>% 
  arrange(system, school, student_key, id_date)

write_csv(
  student_daily_absences,
  str_c("N:/ORP_accountability/projects/Andrew/acct-am/2020/Modeling/attendance/data/student_daily_absences_", format(Sys.Date(), "%m%d%y"), ".csv"),
  na = ''
)

# ======== Power BI =================
student_power_bi <- dbGetQuery(con,
                   str_c("
                    SELECT scal.school_year,
                        enr.isp_id, 
                        scal.id_date,
                        scal.school_bu_id,
                        enr.primary_district_id as district_no,
                        enr.primary_school_id as school_no, 
                        enr.instructional_program_num,enr.student_key, 
                        enr.first_name, enr.middle_name, enr.last_name, enr.grade,
                        enr.english_language_background, enr.begin_date, enr.end_date,
                        stu_abs.attendance_type,
                        CASE WHEN stu_abs.attendance_type = 'D' OR stu_abs.attendance_type IS NULL THEN 1
                              ELSE 0 END as present,
                        CASE WHEN stu_abs.attendance_type = 'D' THEN 1
                              ELSE 0 END as present_virtual,
                        CASE WHEN stu_abs.attendance_type IN ('A', 'U', 'X', 'T') THEN 1
                              ELSE 0 END as absent
                    FROM (SELECT school_bu_id,
                            school_year,
                            instructional_program_num,
                            id_date,
                            1 as temp
                          FROM scal_id_days
                          WHERE school_year = ", 2020, ") SCAL
                    LEFT JOIN (
                        SELECT isp.isp_id, isp.school_year, isp.school_bu_id, isp.primary_district_id,
                            isp.primary_school_id, isp.instructional_program_num,isp.student_key, 
                            isp.first_name, isp.middle_name, isp.last_name, ig.grade,
                            isp.english_language_background, isp.begin_date, isp.end_date, 1 as temp
                        FROM isp
                        LEFT JOIN (
                          SELECT *
                          FROM (SELECT ig.isp_id,
                            ig.assignment as grade,
                            dense_rank() over (partition by student_key order by ig_begin_date desc) rnk
                            FROM instructional_grade ig)
                          WHERE rnk = 1
                            AND grade NOT IN ('P3', 'P4')
                        ) ig ON ig.isp_id = isp.isp_id
                        WHERE school_year = " , 2020, "
                          AND type_of_service = 'P'
                          AND primary_district_id <> 0
                          AND primary_school_id <> 0
                          AND begin_date < NVL(end_date, SYSDATE)
                          AND ig.grade IS NOT NULL
                    ) enr on scal.school_bu_id = enr.school_bu_id AND scal.school_year = enr.school_year
                                AND scal.instructional_program_num = enr.instructional_program_num
                                AND scal.temp = enr.temp
                    LEFT JOIN (SELECT *
                            FROM student_absences
                            WHERE attendance_type NOT IN ('P')
                          ) stu_abs ON stu_abs.isp_id = enr.isp_id 
                                      AND stu_abs.attendance_date = scal.id_date
                    WHERE scal.id_date >= enr.begin_date
                        AND scal.id_date <= NVL(enr.end_date, TRUNC(SYSDATE, 'DD')-1)
                    ORDER BY primary_district_id, primary_school_id, student_key, id_date
                         
                         ")) %>%
  as_tibble() %>% 
  clean_names() #%>% 
  # filter(!is.na(student_key), grade %in% c('K', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10':'12')) %>% 
  # arrange(system, school, student_key, id_date)

student_power_bi_test <- dbGetQuery(con,
                               str_c("
                                     SELECT count(*)
                                     FROM (SELECT school_bu_id,
                                     school_year,
                                     instructional_program_num,
                                     id_date,
                                     1 as temp
                                     FROM scal_id_days
                                     WHERE school_year = ", 2020, ") SCAL
                                     LEFT JOIN (
                                     SELECT isp.isp_id, isp.school_year, isp.school_bu_id, isp.primary_district_id,
                                     isp.primary_school_id, isp.instructional_program_num,isp.student_key, 
                                     isp.first_name, isp.middle_name, isp.last_name,
                                     isp.english_language_background, 1 as temp
                                     FROM isp
                                     WHERE school_year = " , 2020, "
                                     ) enr on scal.school_bu_id = enr.school_bu_id AND scal.school_year = enr.school_year
                                     AND scal.instructional_program_num = enr.instructional_program_num
                                     AND scal.temp = enr.temp
                                     
                                     ")) %>%
  as_tibble() %>% 
  clean_names()
# ============= Virtual Attendance =====================
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
                                           ig.assignment as grade,
                                           student_absences.attendance_date,
                                           student_absences.attendance_type,
                                           isp.student_key
                                           FROM ISP
                                           LEFT JOIN EIS_MGR.INSTRUCTIONAL_GRADE IG ON ISP.ISP_ID = IG.ISP_ID
                                           LEFT JOIN student_absences ON isp.isp_id = student_absences.isp_id
                                           WHERE isp.school_year = ", 2020,"
                                           AND isp.type_of_service = 'P'
                                           AND IG.ASSIGNMENT NOT IN ('P3', 'P4')
                                           AND attendance_type IN ('D')
                                           --AND isp.student_key = 3194068
                                           ) stu_absent ON scal.school_bu_id = stu_absent.school_bu_id
                                           AND scal.id_date = stu_absent.attendance_date
                                           LEFT JOIN school S ON scal.school_bu_id = S.school_bu_id
                                           WHERE SCAL.id_date < TRUNC(SYSDATE, 'DD')
                                           
                                           ")) %>%
  as_tibble() %>% 
  clean_names() %>% 
  filter(!is.na(student_key), grade %in% c('K', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10':'12')) %>% 
  arrange(system, school, student_key, id_date)

enrollment_2020 <- dbGetQuery(con,
                              str_c("
                                    SELECT DISTINCT isp.school_year,
                                      isp.student_key,
                                      isp.student_pin,
                                      isp.begin_date,
                                      isp.end_date,
                                      isp.enrollment_reason,
                                      isp.withdrawal_reason,
                                      isp.primary_district_id,
                                      isp.primary_school_id,
                                      ig.assignment as grade
                                    FROM ISP
                                    LEFT JOIN instructional_grade ig ON isp.isp_id = ig.isp_id
                                    WHERE school_year = 2020
                                      AND type_of_service = 'P'
                                    ")) %>%
  as_tibble() %>% 
  clean_names()

enrollment_2019 <- dbGetQuery(con,
                              str_c("
                                    SELECT 
                                      isp.student_key,
                                      isp.primary_district_id as system,
                                      isp.primary_school_id as school,
                                      isp.begin_date,
                                      isp.end_date,
                                      ig.assignment as grade
                                    FROM ISP
                                    LEFT JOIN instructional_grade ig ON isp.isp_id = ig.isp_id
                                    WHERE school_year = 2019
                                      AND isp.type_of_service = 'P'
                                      AND DATE '2020-05-15' < NVL(end_date, Date '2020-05-30')
                                      AND ig.ig_begin_date >= isp.begin_date
                                    ")) %>%
  as_tibble() %>% 
  clean_names() %>% 
  arrange(system, school, student_key) %>% 
  distinct()

write_csv(enrollment_2019, "N:/ORP_accountability/projects/Data Requests/Enrollment/enrollment")

# ===================== Student Level =================================
school_year <- if_else(month(today()) >= 7, year(today()), year(today()) - 1)

instructional_days <- dbGetQuery(con,
                                 str_c("SELECT
                                       SCAL.SCHOOL_BU_ID,
                                       SCAL.SCHOOL_YEAR AS YEAR,
                                       D.DISTRICT_NAME AS SYSTEM_NAME,
                                       D.DISTRICT_NO AS SYSTEM,
                                       S.SCHOOL_NAME,
                                       S.SCHOOL_NO AS SCHOOL,
                                       COUNT(DISTINCT SCAL.ID_DATE) AS INSTRUCTIONAL_DAYS
                                       FROM EIS_MGR.SCAL_ID_DAYS SCAL
                                       JOIN EIS_MGR.SCHOOL S ON SCAL.SCHOOL_BU_ID = S.SCHOOL_BU_ID
                                       JOIN EIS_MGR.DISTRICT D ON S.DISTRICT_NO = D.DISTRICT_NO
                                       WHERE SCHOOL_YEAR = ", school_year,"
                                       AND SCAL.ID_DATE <= SYSDATE
                                       GROUP BY SCAL.SCHOOL_BU_ID, SCAL.SCHOOL_YEAR, D.DISTRICT_NAME, D.DISTRICT_NO, S.SCHOOL_NAME, S.SCHOOL_NO
                                       ORDER BY SCAL.SCHOOL_BU_ID")
                                 ) %>%
  as_tibble() %>%
  clean_names()

ytd_student_abs <- dbGetQuery(con,
                         str_c("SELECT
                         TO_CHAR(ISP.SCHOOL_YEAR) || '-' || TO_CHAR(ISP.SCHOOL_YEAR+1) AS SCHOOL_YEAR,
                         S.DISTRICT_NO,
                         S.SCHOOL_NO,
                         ISP.INSTRUCTIONAL_PROGRAM_NUM,
                         ISP.ISP_ID,
                         ISP.STUDENT_KEY,
                         ISP.FIRST_NAME,
                         ISP.MIDDLE_NAME,
                         ISP.LAST_NAME,
                         ISP.BEGIN_DATE,
                         ISP.END_DATE,
                         ISP.ENROLLMENT_REASON,
                         ISP.WITHDRAWAL_REASON,
                         ISP.TYPE_OF_SERVICE,
                         STU.DATE_OF_BIRTH,
                         STU.ETHNIC_ORIGIN,
                         STU.GENDER,
                         S.SCHOOL_BU_ID,
                         D.DISTRICT_BU_ID,
                         NVL (IG.ASSIGNMENT, ' ') AS GRADE,
                        --ig.ig_begin_date,
                         DECODE(STU.ETHNICITY,'H','Hispanic','Non Hispanic') ETHNICITY,
                         STU.RACE_I,
                         STU.RACE_A,
                         STU.RACE_P,
                         STU.RACE_B,
                         STU.RACE_W,
                         (CASE WHEN TRUANTS.CNT_UNEXCUSED = 0 THEN NULL ELSE TRUANTS.CNT_UNEXCUSED END) AS CNT_UNEXCUSED,
                         (CASE WHEN TRUANTS.CNT_UNEXCUSED_TRANS = 0 THEN NULL ELSE TRUANTS.CNT_UNEXCUSED_TRANS END) AS CNT_UNEXCUSED_TRANS,
                         (CASE WHEN TRUANTS.CNT_EXCUSED = 0 THEN NULL ELSE TRUANTS.CNT_EXCUSED END) AS CNT_EXCUSED,
                         (CASE WHEN TRUANTS.CNT_EXCUSED_TRANS = 0 THEN NULL ELSE TRUANTS.CNT_EXCUSED_TRANS END) AS CNT_EXCUSED_TRANS,
                         TRUANTS.CNT_TOTAL,
                         (SELECT COUNT(SCAL.ID_DATE) AS ISP_DAYS_1
                         FROM EIS_MGR.SCAL_ID_DAYS SCAL
                         WHERE SCAL.SCHOOL_BU_ID = ISP.SCHOOL_BU_ID
                         AND SCAL.SCHOOL_YEAR = ISP.SCHOOL_YEAR
                         AND SCAL.INSTRUCTIONAL_PROGRAM_NUM = ISP.INSTRUCTIONAL_PROGRAM_NUM
                         AND SCAL.ID_DATE >= ISP.BEGIN_DATE
                         AND SCAL.ID_DATE <= LEAST(NVL(ISP.END_DATE, SYSDATE), SYSDATE)
                         ) AS ISP_DAYS
                         FROM ISP
                         JOIN EIS_MGR.STUDENT_NEW STU ON STU.STUDENT_KEY = ISP.STUDENT_KEY
                         JOIN EIS_MGR.SCHOOL S ON ISP.SCHOOL_BU_ID = S.SCHOOL_BU_ID
                         LEFT JOIN EIS_MGR.INSTRUCTIONAL_GRADE IG ON ISP.ISP_ID = IG.ISP_ID
                         JOIN EIS_MGR.DISTRICT D ON S.DISTRICT_NO = D.DISTRICT_NO
                         LEFT JOIN (
                         SELECT ISP.ISP_ID,
                         COUNT(CASE WHEN ATTENDANCE_TYPE = 'U' THEN 1 END) AS CNT_UNEXCUSED,
                         COUNT(CASE WHEN ATTENDANCE_TYPE = 'X' THEN 1 END) AS CNT_UNEXCUSED_TRANS,
                         COUNT(CASE WHEN ATTENDANCE_TYPE = 'A' THEN 1 END) AS CNT_EXCUSED,
                         COUNT(CASE WHEN ATTENDANCE_TYPE = 'T' THEN 1 END) AS CNT_EXCUSED_TRANS,
                         COUNT(CASE WHEN ATTENDANCE_TYPE <> 'P' THEN 1 END) AS CNT_TOTAL
                         FROM EIS_MGR.INSTRUCTIONAL_SERVICE_PERIOD ISP
                         JOIN EIS_MGR.STUDENT_ABSENCES SA  ON ISP.ISP_ID = SA.ISP_ID
                         WHERE ISP.SCHOOL_YEAR = ", school_year, "
                         AND SA.ATTENDANCE_DATE >= ISP.BEGIN_DATE
                         AND (SA.ATTENDANCE_DATE <= LEAST(NVL(ISP.END_DATE, SYSDATE), SYSDATE) ) --DATE '2020-03-02'
                         GROUP BY ISP.ISP_ID
                         ) TRUANTS ON ISP.ISP_ID = TRUANTS.ISP_ID
                         WHERE ISP.SCHOOL_YEAR = ", school_year,"
                         AND IG.ASSIGNMENT NOT IN ('P3', 'P4')
                         AND ISP.TYPE_OF_SERVICE = 'P'
                         ORDER BY S.DISTRICT_NO, S.SCHOOL_NO, ISP.LAST_NAME, ISP.FIRST_NAME")
                         ) %>%
  as_tibble() %>%
  clean_names() %>%
  mutate_at(
    .vars = c("instructional_program_num", "district_no", "school_no", "student_key", "isp_days", "cnt_total"),
    .f = as.numeric
  ) %>%
  filter(grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) %>%
  mutate(n_absences = if_else(is.na(cnt_total), 0, cnt_total)) %>%
  distinct() %>% 
  select(
    instructional_program_num, system = district_no, school = school_no, grade, #ig_begin_date
    student_key,
    first_name, middle_name, last_name,
    gender, ethnicity, race_i, race_a,
    race_p, race_b, race_w,
    begin_date, end_date, isp_days, n_absences
  ) %>% 
  # group_by(system, school, grade, student_key) %>%
  # summarise(
  #   first_name = first(first_name),
  #   middle_name = first(middle_name),
  #   last_name = first(last_name),
  #   n_absences = sum(n_absences, na.rm = TRUE), 
  #   isp_days = sum(isp_days, na.rm = TRUE)
  # ) %>%
  # ungroup() %>% 
  inner_join(instructional_days, by = c("system", "school")) %>% 
  mutate(
    grade_band = case_when(
      grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08") ~ "K through 8th",
      grade %in% c("09", "10", "11", "12") ~ "9th through 12th"
    ),
    chronic_absence = as.integer(n_absences/isp_days >= 0.1)#,
    # All = TRUE
  )

write_csv(
  ytd_student_abs %>% 
    select(year, system, system_name, school, school_name, school_bu_id, student_key:last_name, grade, grade_band,
           gender:race_w, begin_date:n_absences, instructional_days, chronic_absence),
  str_c("N:/ORP_accountability/projects/Andrew/acct-am/2020/Modeling/attendance/data/student_absenteeism_", format(Sys.Date(), "%m%d%y"), ".csv"),
  na = ''
)


cohort <- dbGetQuery(con,
                               str_c("
                                     select *
                                    FROM studentcohortdata scd
                                    left join studentcohortdocs docs on scd.student_key = docs.student_key
                                     WHERE cohortyear = 2016
                                     ")) %>%
  as_tibble() %>% 
  clean_names()


