# Connect to Database Using Local Environment Variables
# Andrew Marsee
# 4/23/2019
# Pull conducted on 6/5/2019

# Set up, more here: https://changhsinlee.com/pyderpuffgirls-ep2/
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd("N:/")
eis_con = dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="N:/ORP_accountability/ojdbc6.jar"),
  readRegistry("Environment", hive = "HCU")$EIS_MGR_CXN_STR[1],
  "EIS_MGR",
  readRegistry("Environment", hive = "HCU")$EIS_MGR_PWD[1]
) 
sde_con = dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="N:/ORP_accountability/ojdbc6.jar"),
  readRegistry("Environment", hive = "HCU")$SDE_DIR_CXN_STR[1],
  "SDE_DIR",
  readRegistry("Environment", hive = "HCU")$SDE_DIR_PWD[1]
)

# ----------------------- Closed Schools -------------------------------------------
# 2.  List of Closed schools that closed prior to 2018-19: 
#   a.	Schools that were closed prior to the start of or during the 2018-19 school year (many will have end date 6/30/2018). 
#   b.	Closed Dates: 5/31/18 to 8/31/18.
closed_schools <- as.tbl(
  dbGetQuery(
    eis_con,
    "select  distinct
    S.DISTRICT_NUMBER as system, 
    D.DISTRICT_NAME as system_name,
    S.SCHOOL_NUMBER as school, 
    BU.BU_NAME as school_name, 
    BU.STATUS as status,
    max(OP.OP_BEGIN_DATE) over (partition by OP.BU_ID) as OP_BEGIN_DATE,   
    max(OP.OP_END_DATE) over (partition by OP.BU_ID) as OP_END_DATE,
    SCA.enrollment        
    
    from SCHOOL@SDE_DIR_LINK.WORLD S 
    join INSTRUCTIONAL_TYPES@SDE_DIR_LINK.WORLD IT 
    on S.INSTRUCTIONAL_TYPE_ID = IT.INSTRUCTIONAL_TYPE_ID 
    join OPERATIONAL_PERIOD@SDE_DIR_LINK.WORLD OP
    on S.BU_ID = OP.BU_ID 
    join BUSINESS_UNIT@SDE_DIR_LINK.WORLD BU
    on S.BU_ID = BU.BU_ID  
    join EIS_MGR.DISTRICT D
    on S.DISTRICT_NUMBER = D.DISTRICT_NO 
    left join(select school_bu_id, count(distinct student_key) as enrollment
    from eis_mgr.instructional_service_period
    where school_year = extract(year from sysdate) - 2 and (begin_date > end_date or end_date is null)
    group by school_bu_id) SCA  on S.BU_ID = SCA.school_bu_id 
    
    where OP.OP_END_DATE between  '31-MAY-' || to_char(extract(year from sysdate) - 1) AND '31-Aug-' || to_char(extract(year from sysdate))
    and S.SCHOOL_TYPE_ID in ('000', '002', '003')
    
    order by D.DISTRICT_NAME, BU.BU_NAME
    "
  )
)
# write csv for closed schools
write_csv(closed_schools, "N:\\ORP_accountability\\data\\2019_tdoe_provided_files\\closed_schools.csv", na = "")

# ------------------ Alt, CTE, Adult Schools ----------------------------
# 5.    List of CTE schools: 
#   a.    List of CTE schools active during the 2017-18 school year. 
# 6.    List of adult high schools: 
#   a.    List of Adult schools active during the 2017-18 school year.
# 7.    List of Alternative schools: 
#   a.	List of alternative schools active during the 2017-18 school year
# Instructional type of 6 (CTE), 8 (ALT), or 9 (Adult)
cte_alt_adult_schools <- as.tbl(
  dbGetQuery(
    eis_con,
    "select
    S.DISTRICT_NUMBER as District_Number, 
    D.DISTRICT_NAME as District,
    S.SCHOOL_NUMBER as School_Number, 
    BU.BU_NAME as School, 
    S.SCHOOL_TYPE_ID, 
    S.INSTRUCTIONAL_TYPE_ID,
    IT.IT_DESCRIPTION as SchoolType,
    BU.STATUS as Status,
    max(OP.OP_BEGIN_DATE) over (partition by OP.BU_ID) as OP_BEGIN_DATE,   
    max(OP.OP_END_DATE) over (partition by OP.BU_ID) as OP_END_DATE,
    SCA.enrollment      
    
    from SCHOOL@SDE_DIR_LINK.WORLD S 
    join INSTRUCTIONAL_TYPES@SDE_DIR_LINK.WORLD IT 
    on S.INSTRUCTIONAL_TYPE_ID = IT.INSTRUCTIONAL_TYPE_ID 
    join OPERATIONAL_PERIOD@SDE_DIR_LINK.WORLD OP
    on S.BU_ID = OP.BU_ID 
    join BUSINESS_UNIT@SDE_DIR_LINK.WORLD BU
    on S.BU_ID = BU.BU_ID  
    join EIS_MGR.DISTRICT D
    on S.DISTRICT_NUMBER = D.DISTRICT_NO    
    left join(select school_bu_id, count(distinct student_key) as enrollment
    from eis_mgr.instructional_service_period
    where school_year = extract(year from sysdate) - 2 and (begin_date > end_date or end_date is null)
    group by school_bu_id) SCA  on S.BU_ID = SCA.school_bu_id 
    
    where S.SCHOOL_TYPE_ID in ('000', '002', '003')
    and S.INSTRUCTIONAL_TYPE_ID in ('006','008','009')
    and BU.STATUS = 'A'   
    and (OP_END_DATE is null OR OP_END_DATE >'01-JUN-' || to_char(extract(year from sysdate)))  
    
    order by SchoolType, D.DISTRICT_NAME, BU.BU_NAME
    "
  )
)
# write csv for cte, alt, and adult schools
write_csv(cte_alt_adult_schools, "N:\\ORP_accountability\\data\\2019_tdoe_provided_files\\cte_alt_adult_schools.csv", na = "")

# ----------------- New Schools -----------------------------------
 # 1.  List of New schools: List of schools that opened prior to or during the 2017-18 school year but after the 2016-17 school year. 
 #     Open Dates: 5/31/18 to 8/31/18
new_schools <- as.tbl(
  dbGetQuery(
    eis_con,
    "select  
    S.DISTRICT_NUMBER as system, 
    D.DISTRICT_NAME as system_name,
    S.SCHOOL_NUMBER as school, 
    BU.BU_NAME as school_name, 
    BU.STATUS as status,
    max(OP.OP_BEGIN_DATE) over (partition by OP.BU_ID) as op_begin_date,   
    max(OP.OP_END_DATE) over (partition by OP.BU_ID) as op_end_date,
    SCA.enrollment   
    
    from 
    SCHOOL@SDE_DIR_LINK.WORLD S 
    join INSTRUCTIONAL_TYPES@SDE_DIR_LINK.WORLD IT 
    on S.INSTRUCTIONAL_TYPE_ID = IT.INSTRUCTIONAL_TYPE_ID 
    join OPERATIONAL_PERIOD@SDE_DIR_LINK.WORLD OP
    on S.BU_ID = OP.BU_ID 
    join BUSINESS_UNIT@SDE_DIR_LINK.WORLD BU
    on S.BU_ID = BU.BU_ID  
    join EIS_MGR.DISTRICT D
    on S.DISTRICT_NUMBER = D.DISTRICT_NO    
    left join(select school_bu_id, count(distinct student_key) as enrollment
    from eis_mgr.instructional_service_period
    where school_year = extract(year from sysdate) - 2 and (begin_date > end_date or end_date is null)
    group by school_bu_id) SCA  on S.BU_ID = SCA.school_bu_id     
    
    where 
    S.SCHOOL_TYPE_ID in ('000', '002', '003') and 
    OP.OP_BEGIN_DATE between  '31-MAY-' || to_char(extract(year from sysdate) - 1) AND '31-Aug-' || to_char(extract(year from sysdate) - 1) and 
    OP.OP_END_DATE is null
    
    order by D.DISTRICT_NAME, BU.BU_NAME
    "
  )
)
# write csv for new schools
write_csv(new_schools, "N:\\ORP_accountability\\data\\2019_tdoe_provided_files\\new_schools.csv", na = "")

# ----------------- Prior Closed Schools -----------------------------------
# 3.	List of closed schools 2018: 
#   a.	schools that were open at any point or all of the 2018-19 school year but closed by Jul 1, 2019. 
# b.	Closed Dates: 12/1/2018 to 7/1/2019
prior_closed_schools <- as.tbl(
  dbGetQuery(
    eis_con,
    "select S.DISTRICT_NUMBER as system, 
    D.DISTRICT_NAME as system_name,
    S.SCHOOL_NUMBER as school, 
    BU.BU_NAME as school_name, 
    BU.STATUS as status,
    max(OP.OP_BEGIN_DATE) over (partition by OP.BU_ID) as OP_BEGIN_DATE,   
    max(OP.OP_END_DATE) over (partition by OP.BU_ID) as OP_END_DATE,
    SCA.enrollment     
    
    from SCHOOL@SDE_DIR_LINK.WORLD S 
    join INSTRUCTIONAL_TYPES@SDE_DIR_LINK.WORLD IT 
    on S.INSTRUCTIONAL_TYPE_ID = IT.INSTRUCTIONAL_TYPE_ID 
    join OPERATIONAL_PERIOD@SDE_DIR_LINK.WORLD OP
    on S.BU_ID = OP.BU_ID 
    join BUSINESS_UNIT@SDE_DIR_LINK.WORLD BU
    on S.BU_ID = BU.BU_ID  
    join EIS_MGR.DISTRICT D
    on S.DISTRICT_NUMBER = D.DISTRICT_NO   
    left join(select school_bu_id, count(distinct student_key) as enrollment
    from eis_mgr.instructional_service_period
    where school_year = extract(year from sysdate) - 2 and (begin_date > end_date or end_date is null)
    group by school_bu_id) SCA  on S.BU_ID = SCA.school_bu_id 
    
    where 
    OP.OP_END_DATE between  '01-DEC-' || to_char(extract(year from sysdate) - 1) AND '01-JUL-' || to_char(extract(year from sysdate))
    and S.SCHOOL_TYPE_ID in ('000', '002', '003')
    
    order by D.DISTRICT_NAME, BU.BU_NAME
    "
  )
  )
# write csv for prior closed schools
write_csv(prior_closed_schools, "N:\\ORP_accountability\\data\\2019_tdoe_provided_files\\prior_closed_schools.csv", na = "")

# ----------------- SPED Schools -----------------------------------
# 4.  SPED schools: List of Special Education schools active during the 2017-18 school year.
#        Instructional Type ID = '007' - Special Education
sped_schools <- as.tbl(
  dbGetQuery(
    eis_con,
    "select S.DISTRICT_NUMBER as system, 
    D.DISTRICT_NAME as system_name,
    S.SCHOOL_NUMBER as school, 
    BU.BU_NAME as school_name, 
    S.SCHOOL_TYPE_ID, 
    S.INSTRUCTIONAL_TYPE_ID,
    IT.IT_DESCRIPTION as SchoolType,
    BU.STATUS as Status,
    max(OP.OP_BEGIN_DATE) over (partition by OP.BU_ID) as OP_BEGIN_DATE,   
    max(OP.OP_END_DATE) over (partition by OP.BU_ID) as OP_END_DATE,
    SCA.enrollment      
    
    from SCHOOL@SDE_DIR_LINK.WORLD S 
    join INSTRUCTIONAL_TYPES@SDE_DIR_LINK.WORLD IT 
    on S.INSTRUCTIONAL_TYPE_ID = IT.INSTRUCTIONAL_TYPE_ID 
    join OPERATIONAL_PERIOD@SDE_DIR_LINK.WORLD OP
    on S.BU_ID = OP.BU_ID 
    join BUSINESS_UNIT@SDE_DIR_LINK.WORLD BU
    on S.BU_ID = BU.BU_ID  
    join EIS_MGR.DISTRICT D
    on S.DISTRICT_NUMBER = D.DISTRICT_NO    
    left join(select school_bu_id, count(distinct student_key) as enrollment
    from eis_mgr.instructional_service_period
    where school_year = extract(year from sysdate) - 1 and (begin_date > end_date or end_date is null)
    group by school_bu_id) SCA  on S.BU_ID = SCA.school_bu_id 
    
    where S.SCHOOL_TYPE_ID in ('000', '002', '003')
    and S.INSTRUCTIONAL_TYPE_ID = '007'
    and BU.STATUS = 'A'   
    -- and (OP_END_DATE is null or  (OP_END_DATE between '01-JUN-2017' and '31-JUL-2017'))  --usually closing school will be inputted in the system in Jun-July
    and (OP_END_DATE is null OR OP_END_DATE >'01-JUN-2018')  
    order by D.DISTRICT_NAME, BU.BU_NAME
    "
  )
  )

# write csv for SPED schools
write_csv(sped_schools, "N:\\ORP_accountability\\data\\2019_tdoe_provided_files\\sped_schools.csv", na = "")




esl_inspection <- as.tbl(
  dbGetQuery(
    eis_con,
    "SELECT
    PRIMARY_DISTRICT_ID,
    COUNT(*) as number_els
    
    FROM eis_mgr.instructional_service_period

    where PRIMARY_DISTRICT_ID in ('531', '061', '860', '792')
    and ENGLISH_LANGUAGE_BACKGROUND in ('L', 'W')
    and school_year = extract(year from sysdate) - 1 and (begin_date > end_date or end_date is null)
    and begin_date < '01-MAR-2019'
    GROUP BY PRIMARY_DISTRICT_ID
    "
  )
)

esl_inspection_primary <- as.tbl(
  dbGetQuery(
    eis_con,
    "SELECT 
    *
    
    FROM eis_mgr.instructional_service_period

    where PRIMARY_DISTRICT_ID in ('531', '061', '860', '10')
    and ENGLISH_LANGUAGE_BACKGROUND in ('L', 'W') 
    and school_year = extract(year from sysdate) - 1 and (begin_date > end_date or end_date is null)
    "
  )
)


