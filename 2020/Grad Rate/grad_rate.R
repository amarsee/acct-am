options(java.parameters = "-Xmx16G")
library(tidyverse)
library(janitor)
library(readstata13)
library(readxl)
library(haven)
library(RJDBC)

# ============ Function to calculate rates ===========
calc_counts <- function(grouped_df){
  grouped_df %>% 
    summarise(
      grad_cohort = sum(grad_indicator, na.rm = TRUE),
      n_ready_grad = sum(ready_grad_indicator, na.rm = TRUE),
      n_completed_act_sat = sum(completed_act_sat, na.rm = TRUE),
      n_count = n()
    ) %>% 
    ungroup()
}

calc_pcts <- function(df){
  df %>% 
    mutate(
      pct_ready_grad = round(n_ready_grad / n_count * 100 + 1e-5, 1),
      act_participation_rate = round(n_completed_act_sat / grad_cohort * 100 + 1e-5, 0)
    )
}

# ================== Pull Data ===============
# Data
con = dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="N:/ORP_accountability/ojdbc6.jar"),
  readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
  "EIS_MGR",
  readRegistry("Environment", "HCU")$EIS_MGR_PWD
)
# SQL Pull
cohort = as_tibble(
  dbGetQuery(
    con,
    str_c("SELECT scd.student_key,
                scd.first_name,
                scd.middle_name,
                scd.last_name,
                scd.suffix,
                scd.date_of_birth,
                scd.gender,
                scd.immigrant,
                scd.date_1st_enrolled_us_school,
                scd.year_entered_grade9,
                scd.native_language,
                scd.ethnicity,
                scd.race_i,
                scd.race_a,
                scd.race_p,
                scd.race_b,
                scd.race_w,
                scd.cohortyear,
                scd.calc_from,
                scd.district_no as system,
                scd.school_no as school,
                scd.assignment,
                scd.eoy_action,
                scd.withdrawal_reason,
                scd.completion_type,
                scd.completion_period,
                scd.completion_date,
                scd.ell as el,
                scd.econ_dis as ed,
                scd.sped as swd,
                scd.year_withdrawn,
                scd.included_in_cohort,
                scd.race_ethnicity,
                scd.manual_intervention,
                scd.homeless,
                scd.cte,
                scd.migrant,
                scd.isp_id,
                docs.save_as_filename,
                docs.upload_date,
                docs.user_id,
                docs.reviewer_user_id,
                docs.comments,
                docs.status,
                docs.reviewed_date,
                docs.revised_included_in_cohort
    FROM studentcohortdata scd
    LEFT JOIN (
          SELECT student_key,
                save_as_filename,
                modified_date as upload_date,
                user_id,
                reviewer_user_id,
                comments,
                status,
                reviewed_date,
                revised_included_in_cohort
          FROM studentcohortdocs) docs on scd.student_key = docs.student_key
    WHERE cohortyear = extract(year from SYSDATE) - 4"
    )
  )
  ) %>%
  janitor::clean_names()

write_csv(ready_grad_student , 'N:/ORP_accountability/data/2020_ready_graduate/Data/ready_graduate_student_level.csv', na = '')


