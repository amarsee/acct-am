# ECON 6110 - Assignment 3
# Evan Kramer
# 5/6/2019

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
# library(odbc)
library(RJDBC)
# setwd(str_c("N:/TNReady/", year(today()) - 1, "-", str_sub(year(today()), -2, -1), "/spring/demographics/"))

school_year <- year(today())-1
# From EIS
demo = dbGetQuery(
  dbConnect(
    # JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), # Evan's classPath
    JDBC("oracle.jdbc.OracleDriver", classPath="N:/ORP_accountability/ojdbc6.jar"), # Andrew's classPath
    readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
    "EIS_MGR",
    readRegistry("Environment", "HCU")$EIS_MGR_PWD
  ),
  str_c("SELECT 
    STUDENT_KEY,
    SCHOOL_BU_ID,
    DISTRICT_NO,
    SCHOOL_NO,
    SCHOOL_YEAR,
    INSTRUCTIONAL_PROGRAM_NUM,
    ETHNICITY,
    IsAmericanIndian,
    IsAsian,
    IsBlack,
    IsPacificIslander,
    IsWhite,
    ReportedRace,
    GENDER,
    Title1,
    CodeAB,
    IsGifted,
    IsFunctionallyDelayed,
    IsMigrant,
    IsElExcluded,
    IsEl,
    T1T2,
    SpecialEducation,
    IEPExists,
    HasAutism,
    HasDeaf_Blindness,
    HasDevelopmentalDelay,
    HasEmotionalDisturbance,
    HasHearingImpairment,
    HasIntellectualDisability,
    HasMultipleDisabilities,
    HasOrthopedicImpairment,
    HasOtherHealthImpairment,
    HasSpecificLearningDisability,
    HasSpeechOrLanguageImpairment,
    HasTraumaticBrainInjury,
    HasVisualImpairment,
    Grade,
    IsMobile,
    District50Percent,
    School50Percent,
    HomeLanguage,
    LimitedEnglishProficiency,
    InstructionalAvailabilty,
    IsHomeless,	
    IsRunaway,
    ElRecentlyArrivedYearOne,
    ElRecentlyArrivedYearTwo
  FROM (SELECT ROW_NUMBER() OVER(PARTITION BY isp.STUDENT_KEY,
    isp.SCHOOL_BU_ID,
    isp.SCHOOL_YEAR
    ORDER BY isp.BEGIN_DATE DESC) Priority,
  isp.STUDENT_KEY,
  isp.SCHOOL_BU_ID,
  sch.DISTRICT_NO,
  sch.SCHOOL_NO,
  isp.SCHOOL_YEAR,
  isp.INSTRUCTIONAL_PROGRAM_NUM,
  (CASE WHEN ((sn.ETHNICITY = 'H') OR (sn.ETHNICITY = 'N')) THEN sn.ETHNICITY
    ELSE 'U'
    END) ETHNICITY,
  DECODE(sn.RACE_I, 'Y', 1, 'N', 0, NULL, NULL) IsAmericanIndian,
  DECODE(sn.RACE_A, 'Y', 1, 'N', 0, NULL, NULL) IsAsian,
  DECODE(sn.RACE_B, 'Y', 1, 'N', 0, NULL, NULL) IsBlack,
  DECODE(sn.RACE_P, 'Y', 1, 'N', 0, NULL, NULL) IsPacificIslander,
  DECODE(sn.RACE_W, 'Y', 1, 'N', 0, NULL, NULL) IsWhite,
  (CASE WHEN sn.ETHNICITY = 'H' THEN 4
      WHEN sn.RACE_B = 'Y' THEN 3
      WHEN sn.RACE_I = 'Y' THEN 1
      WHEN sn.RACE_P = 'Y' THEN 5
      WHEN sn.RACE_A = 'Y' THEN 2
      WHEN sn.RACE_W = 'Y' THEN 6
      ELSE 0
    END) ReportedRace,
  sn.GENDER,
  (CASE
  WHEN schT1.SCHOOL_PROGRAM_TYPE_ID = '001' THEN 1
  WHEN ((schT1.SCHOOL_PROGRAM_TYPE_ID = '009') AND (stuT1.ISP_ID IS NOT NULL)) THEN 2
    ELSE 0
    END) Title1,
  DECODE(typeAB.ISP_ID, NULL, 2, 1)  CodeAB,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'GFT', 1, 0)  IsGifted,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'FD', 1, 0)  IsFunctionallyDelayed,
  DECODE(EIS_MGR.migrantstudent(isp.isp_id, isp.SCHOOL_YEAR), 'Y', 1, 0) IsMigrant,
  0  IsElExcluded, --Logic removed, column retained
  (CASE
  WHEN isp.ENGLISH_LANGUAGE_BACKGROUND IN ('L','W') THEN 1
    ELSE 0
    END) IsEl,
  (CASE
  WHEN isp.ENGLISH_LANGUAGE_BACKGROUND IN ('1','2','3','4') THEN
  TO_NUMBER(isp.ENGLISH_LANGUAGE_BACKGROUND)
    ELSE 0
    END) T1T2,
  (CASE -- this is a duplicate of below but not removed to preserve column
    WHEN IEPExists.ISP_ID IS NOT NULL THEN 1
    ELSE 0
  END)  SpecialEducation,
  (CASE WHEN IEPExists.ISP_ID IS NOT NULL THEN 1
    ELSE 0
    END)  IEPExists,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'AUT', 'Y', 'N')  HasAutism,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'DB', 'Y', 'N')  HasDeaf_Blindness,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'DD', 'Y', 'N')  HasDevelopmentalDelay,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'EMN', 'Y', 'N')  HasEmotionalDisturbance,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'HI', 'Y', 'N')  HasHearingImpairment,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'ID', 'Y', 'N')  HasIntellectualDisability,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'MD', 'Y', 'N')  HasMultipleDisabilities,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'OI', 'Y', 'N')  HasOrthopedicImpairment,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'OHI', 'Y', 'N')  HasOtherHealthImpairment,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'SLD', 'Y', 'N')  HasSpecificLearningDisability,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'SLI', 'Y', 'N')  HasSpeechOrLanguageImpairment,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'TBI', 'Y', 'N')  HasTraumaticBrainInjury,
  DECODE(EIS_MGR.FN_GET_Primary_Disability(isp.ISP_ID), 'VI', 'Y', 'N')  HasVisualImpairment,
  EIS_MGR.FN_GET_IG(isp.ISP_ID)  Grade,
  (CASE WHEN ismob.ISP_ID IS NOT NULL THEN 'Y'
    ELSE 'N'
    END) IsMobile,
  (CASE WHEN EIS_MGR.FN_ASSESS_DIST_DAY_CT(sch.DISTRICT_NO, isp.SCHOOL_YEAR, isp.STUDENT_KEY) = 0 THEN 'N'
    WHEN EIS_MGR.FN_ASSESS_STU_DIST_DAY_CT(sch.DISTRICT_NO,
      isp.SCHOOL_YEAR,
      isp.STUDENT_KEY) / EIS_MGR.FN_ASSESS_DIST_DAY_CT(sch.DISTRICT_NO,
      isp.SCHOOL_YEAR,
      isp.STUDENT_KEY) >= .5
    THEN 'Y'
    ELSE 'N'
  END) District50Percent,
  (CASE WHEN EIS_MGR.FN_ASSESS_SCH_DAY_CT(isp.SCHOOL_BU_ID,
        isp.SCHOOL_YEAR,
        isp.STUDENT_KEY) = 0
        THEN 'N' 
    WHEN  EIS_MGR.FN_ASSESS_STU_SCH_DAY_CT(isp.SCHOOL_BU_ID,
        isp.SCHOOL_YEAR,
        isp.STUDENT_KEY) / EIS_MGR.FN_ASSESS_SCH_DAY_CT(isp.SCHOOL_BU_ID,
        isp.SCHOOL_YEAR,
        isp.STUDENT_KEY) >= .5
    THEN 'Y'
    ELSE 'N'
  END)  School50Percent,
  99  HomeLanguage,
  (CASE WHEN ((isp.ENGLISH_LANGUAGE_BACKGROUND IS NULL) OR (isp.ENGLISH_LANGUAGE_BACKGROUND = 'E')) THEN 'N'
      WHEN isp.ENGLISH_LANGUAGE_BACKGROUND IN ('L', 'W') THEN 'Y'
      WHEN isp.ENGLISH_LANGUAGE_BACKGROUND IN ('1','2','3','4','F') THEN 'F'
      WHEN isp.ENGLISH_LANGUAGE_BACKGROUND = 'N' THEN 'S'
    ELSE NULL
    END)  LimitedEnglishProficiency,
  EIS_MGR.FN_ASSESS_INSTRUCT_AVAIL(isp.ISP_ID, isp.BEGIN_DATE,
    isp.END_DATE, isp.INSTRUCTIONAL_PROGRAM_NUM,
    isp.SCHOOL_BU_ID, isp.SCHOOL_YEAR)  InstructionalAvailabilty,
  (CASE WHEN ishl.ISP_ID IS NOT NULL THEN 1
    ELSE 0
    END) IsHomeless,
  (CASE WHEN isrw.ISP_ID IS NOT NULL THEN 1
    ELSE 0
    END) IsRunaway,
  (CASE
      WHEN ((isp.ENGLISH_LANGUAGE_BACKGROUND NOT IN ('L','W')) OR (sn.DATE_1ST_ENROLLED_US_SCHOOL IS NULL)) THEN 0
      WHEN sn.DATE_1ST_ENROLLED_US_SCHOOL >= TRUNC(SYSDATE - 365) THEN 1
      ELSE 0
    END) ElRecentlyArrivedYearOne,
  (CASE
      WHEN ((isp.ENGLISH_LANGUAGE_BACKGROUND NOT IN ('L','W')) OR (sn.DATE_1ST_ENROLLED_US_SCHOOL IS NULL)) THEN 0
      WHEN sn.DATE_1ST_ENROLLED_US_SCHOOL BETWEEN TRUNC(SYSDATE - 730) AND TRUNC(SYSDATE - 366) THEN 1
    ELSE 0
    END)  ElRecentlyArrivedYearTwo
  FROM (SELECT * 
          FROM EIS_MGR.INSTRUCTIONAL_SERVICE_PERIOD
          WHERE school_year = ", school_year,")  isp
  INNER JOIN EIS_MGR.SCHOOL sch
  ON isp.SCHOOL_BU_ID = sch.SCHOOL_BU_ID
  INNER JOIN EIS_MGR.STUDENT_NEW  sn
  ON isp.STUDENT_KEY = sn.STUDENT_KEY
  LEFT JOIN (SELECT DISTINCT ISP_ID
    FROM EIS_MGR.STUDENT_CLASSIFICATION
    WHERE TRUNC(SYSDATE) BETWEEN SC_BEGIN_DATE
    AND COALESCE(SC_END_DATE, TRUNC(SYSDATE + 1))
    AND STUDENT_CLASSIFICATION_TYPE IN ('H','I','J','U','FOS01'))  typeAB
  ON isp.ISP_ID = typeAB.ISP_ID
  LEFT JOIN (SELECT DISTINCT ISP_ID
    FROM EIS_MGR.STUDENT_CLASSIFICATION
    WHERE TRUNC(SYSDATE) BETWEEN SC_BEGIN_DATE
    AND COALESCE(SC_END_DATE, TRUNC(SYSDATE + 1))
    AND STUDENT_CLASSIFICATION_TYPE = 'T')  stuT1
  ON isp.ISP_ID = stuT1.ISP_ID
  LEFT JOIN (SELECT DISTINCT bu_id,
    school_program_type_id
    FROM SDE_DIR.SCHOOL_PROGRAM@sde_dir_link.world
    WHERE school_program_type_id IN ('001', '009')
    AND TRUNC(SYSDATE) BETWEEN sp_begin_date
    AND COALESCE(sp_end_date, TRUNC(SYSDATE) + 1))  schT1
  ON isp.SCHOOL_BU_ID = schT1.bu_id
  LEFT JOIN (SELECT DISTINCT ISP_ID 
    FROM EIS_MGR.SPED_DISABILITIES
    WHERE TRUNC(SYSDATE) BETWEEN DIS_BEGIN_DATE
    AND COALESCE(DIS_END_DATE, TRUNC(SYSDATE + 1))
    AND DISABILITY_TYPE NOT IN (3,16)
    AND DISABILITY_LEVEL = 'P')  IEPExists
  ON isp.ISP_ID = IEPExists.ISP_ID
  LEFT JOIN (SELECT DISTINCT ISP_ID
    FROM EIS_MGR.STUDENT_CLASSIFICATION
    WHERE TRUNC(SYSDATE) BETWEEN SC_BEGIN_DATE
    AND COALESCE(SC_END_DATE, TRUNC(SYSDATE + 1))
    AND STUDENT_CLASSIFICATION_TYPE IN ('H','I','U'))  ismob
    ON isp.ISP_ID = ismob.ISP_ID
  LEFT JOIN (SELECT DISTINCT ISP_ID
    FROM EIS_MGR.STUDENT_CLASSIFICATION
    WHERE TRUNC(SYSDATE) BETWEEN SC_BEGIN_DATE
    AND COALESCE(SC_END_DATE, TRUNC(SYSDATE + 1))
    AND STUDENT_CLASSIFICATION_TYPE = 'H')  ishl
    ON isp.ISP_ID = ishl.ISP_ID
  LEFT JOIN (SELECT DISTINCT ISP_ID
    FROM EIS_MGR.STUDENT_CLASSIFICATION
    WHERE TRUNC(SYSDATE) BETWEEN SC_BEGIN_DATE
    AND COALESCE(SC_END_DATE, TRUNC(SYSDATE + 1))
    AND STUDENT_CLASSIFICATION_TYPE = 'U')  isrw
  ON isp.ISP_ID = isrw.ISP_ID
  WHERE isp.School_Year = extract(year from sysdate) - 1
  AND COALESCE(isp.BEGIN_DATE, TRUNC(SYSDATE + 1)) <= TRUNC(SYSDATE)
  AND COALESCE(isp.BEGIN_DATE, TRUNC(SYSDATE + 1)) <> COALESCE(isp.END_DATE, TRUNC(SYSDATE + 1))
  -- AND COALESCE(isp.END_DATE, TRUNC(SYSDATE + 1)) > TRUNC(SYSDATE)
  AND isp.TYPE_OF_SERVICE = 'P') dummy
  WHERE Priority = 1")
) %>% 
  janitor::clean_names() %>% 
  as_tibble()

# Input and output
col_names = c(
  "STUDENT_KEY",
  "SCHOOL_BU_ID",
  "DISTRICT_ID",
  "SCHOOL_ID",
  "SCHOOL_YEAR",
  "INSTRUCTIONAL_PROGRAM_NUM",
  "ETHNICITY",
  "ISAMERICANINDIAN",
  "ISASIAN",
  "ISBLACK",
  "ISPACIFICISLANDER",
  "ISWHITE",
  "REPORTEDRACE",
  "GENDER",
  "TITLE1",
  "CODEAB",
  "ISGIFTED",
  "ISFUNCTIONALLYDELAYED",
  "ISMIGRANT",
  "ISELEXCLUDED",
  "ISEL",
  "T1T2",
  "SPECIALEDUCATION",
  "IEPEXISTS",
  "HASAUTISM",
  "HASDEAF_BLINDNESS",
  "HASDEVELOPMENTALDELAY",
  "HASEMOTIONALDISTURBANCE",
  "HASHEARINGIMPAIRMENT",
  "HASINTELLECTUALDISABILITY",
  "HASMULTIPLEDISABILITIES",
  "HASORTHOPEDICIMPAIRMENT",
  "HASOTHERHEALTHIMPAIRMENT",
  "HASSPECIFICLEARNINGDISABILITY",
  "HASSPEECHORLANGUAGEIMPAIRMENT",
  "HASTRAUMATICBRAININJURY",
  "HASVISUALIMPAIRMENT",
  "GRADE",
  "ISMOBILE",
  "DISTRICT50PERCENT",
  "SCHOOL50PERCENT",
  "HOMELANGUAGE",
  "LIMITEDENGLISHPROFICIENCY",
  "INSTRUCTIONALAVAILABILITY",
  "ISHOMELESS",
  "ISRUNAWAY",
  "ELRECENTLYARRIVEDYEARONE",
  "ELRECENTLYARRIVEDYEARTWO"
)
file = read_csv("N:/TNReady/2018-19/spring/demographics/spring_2019_assessment_demographics_20190510.csv", col_names = F)
if(length(col_names) == ncol(demo)) {
  #names(file) = col_names
  write_csv(demo, "N:/ORP_accountability/projects/Andrew/Demographics/Data/student_demographics_03182021.csv", na = "")
} else {
  print("File not saved; different number of variables and column names")
}
