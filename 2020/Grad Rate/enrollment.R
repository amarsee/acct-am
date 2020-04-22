# Pulling enrollment for withdrawn students
# Last updated 4/21/2020

options(java.parameters = "-Xmx16G")

library(acct)
library(tidyverse)
library(janitor)
library(lubridate)
library(RJDBC)

# con <- dbConnect(
#     JDBC("oracle.jdbc.OracleDriver", classPath = Sys.getenv("JAVA_PATH")),
#     Sys.getenv("CONNECTION_STRING"),
#     Sys.getenv("SCHEMA_NAME"),
#     Sys.getenv("DB_PASSWORD")
# )

con <- dbConnect(
    JDBC("oracle.jdbc.OracleDriver", classPath="N:/ORP_accountability/ojdbc6.jar"),
    readRegistry("Environment", hive = "HCU")$EIS_MGR_CXN_STR[1],
    "EIS_MGR",
    readRegistry("Environment", hive = "HCU")$EIS_MGR_PWD[1]
)
# 
# # Pull attendance from database
attendance <- dbGetQuery(con,
    "SELECT
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
        AND SCAL.ID_DATE <= NVL(ISP.END_DATE,
          (SELECT MAX(SCAL_ID_DAYS.ID_DATE)
              FROM EIS_MGR.SCAL_ID_DAYS
              WHERE SCAL_ID_DAYS.SCHOOL_BU_ID = ISP.SCHOOL_BU_ID
              AND SCAL.SCHOOL_YEAR = ISP.SCHOOL_YEAR
              AND SCAL.INSTRUCTIONAL_PROGRAM_NUM = ISP.INSTRUCTIONAL_PROGRAM_NUM)
          )
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
      WHERE ISP.SCHOOL_YEAR = EXTRACT(YEAR FROM SYSDATE) - 1
      AND SA.ATTENDANCE_DATE >= ISP.BEGIN_DATE
      AND (SA.ATTENDANCE_DATE < ISP.END_DATE OR ISP.END_DATE IS NULL)
      GROUP BY ISP.ISP_ID
      ) TRUANTS ON ISP.ISP_ID = TRUANTS.ISP_ID
    WHERE ISP.SCHOOL_YEAR = EXTRACT(YEAR FROM SYSDATE) - 1
    AND IG.ASSIGNMENT NOT IN ('P3', 'P4') 
    AND ISP.TYPE_OF_SERVICE = 'P'
    ORDER BY S.DISTRICT_NO, S.SCHOOL_NO, ISP.LAST_NAME, ISP.FIRST_NAME") %>%
    as.tbl() %>%
    clean_names() %>%
    mutate_at(
        .vars = c("instructional_program_num", "district_no", "school_no", "student_key", "isp_days", "cnt_total",
                  "withdrawal_reason"),
        .f = as.numeric
    ) # %>%
    # select(
    #     instructional_program_num, system = district_no, school = school_no, grade, student_key,
    #     first_name, middle_name, last_name, begin_date, end_date, isp_days, cnt_total
    # )

# Export pull so we don't have to run it each time
write_csv(attendance, 
          str_c("N:/ORP_accountability/data/2020_graduation_rate/enrollment",format(Sys.Date(), "%Y%m%d"),".csv"), 
          na = '')

