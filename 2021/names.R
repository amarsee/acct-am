

options(java.parameters = "-Xmx16G")
library(acct)
library(tidyverse)
library(RJDBC)
eis_con = dbConnect(
  JDBC("oracle.jdbc.OracleDriver", classPath="N:/ORP_accountability/ojdbc6.jar"),
  readRegistry("Environment", hive = "HCU")$EIS_MGR_CXN_STR[1],
  "EIS_MGR",
  readRegistry("Environment", hive = "HCU")$EIS_MGR_PWD[1]
) 

fall_eoc <- read_csv("N:/ORP_accountability/data/2020_cdf/2020_fall_eoc_cdf.csv",
    col_types = "iciccccdiccccdiiiiciiciiciiciiiiiicc")

# spring_eoc <- read_csv("N:/ORP_accountability/data/2019_cdf/2019_spring_eoc_cdf.csv",
#     col_types = "iciccccdiccccdiiiiciiciiciiciiiiiicc")
# 
# tn_ready <- read_csv("N:/ORP_accountability/data/2019_cdf/2019_3_8_cdf.csv",
#     col_types = "iciccccdiccccdiiiiciiciiciiciiiiiicc")

districts <- bind_rows(fall_eoc) %>% # , spring_eoc, tn_ready
    transmute(system, system_name = str_to_title(system_name)) %>%
    distinct()

schools <- bind_rows(fall_eoc) %>% # , spring_eoc, tn_ready
    transmute(system, school, school_name = str_to_title(school_name)) %>%
    distinct()

# Pull active schools from EIS
schools <- as_tibble(
  dbGetQuery(
    eis_con,
    "
      SELECT s.district_no, d.district_name,
              s.school_no, s.school_name
      FROM school s
      LEFT JOIN district d ON s.district_no = d.district_no
      WHERE s.operational_status = 'A'
        AND s.instructional_type NOT IN ('010')
    "
  )
  ) %>% 
  janitor::clean_names() %>% 
  transmute(
    system = as.numeric(district_no),
    system_name = district_name,
    school = as.numeric(school_no),
    school_name = school_name
  ) %>% 
  arrange(system, school)
# Master school file for EDFacts from TNShare
master <- readxl::read_excel("N:/ORP_accountability/projects/Andrew/EDFacts/2020-2021/2020-21 EDFacts School Master FIle_3-9-21.xlsx", sheet = 2) %>%
    janitor::clean_names() %>%
    transmute(
        system = as.integer(dg_4_lea_id_state),
        system_name = lea_name,
        school = as.integer(dg_5_school_id_state),
        school_name = dg_7_school_name
    ) 

schools_missing <- master %>%
    anti_join(schools, by = c("system", "school")) %>%
    select(system, school, school_name)

schools_present <- master %>% 
  inner_join(schools %>% select(system, school), by = c('system', 'school')) %>% 
  select(system, school, school_name)

# districts_present <- bind_rows(fall_eoc) %>% # , spring_eoc, tn_ready
#   transmute(system) %>%
#   distinct() %>% 
#   inner_join(
#     master %>% 
#       select(system, system_name) %>% 
#       distinct(),
#     by = c('system')
#   )

districts <- schools %>% 
  select(system, system_name) %>% 
  distinct()

test <- bind_rows(schools_present, schools_missing) %>%
    left_join(districts, by = "system") %>%
    select(system, system_name, school, school_name) %>%
    bind_rows(
      tribble(
        ~system, ~system_name, ~school, ~school_name,
        970, "Department Of Children's Services Education Division", 25, "Gateway to Independence",
        # Wilder included in EDFacts starting 2020-21
        # 970, "Department of Children's Services", 45, "Wilder Youth Development Center",
        970, "Department Of Children's Services Education Division", 65, "Mountain View Youth Development Center",
        970, "Department Of Children's Services Education Division", 140, "DCS Affiliated Schools"
      )
    ) %>%
    arrange(system, school) %>%
    write_csv("N:/ORP_accountability/data/2021_final_accountability_files/names.csv", na = "")

# master %>%
#   distinct() %>% 
#   bind_rows(
#     tribble(
#       ~system, ~system_name, ~school, ~school_name,
#       970, "Department of Children's Services", 25, "Gateway to Independence",
#       970, "Department of Children's Services", 45, "Wilder Youth Development Center",
#       970, "Department of Children's Services", 65, "Mountain View Youth Development Center",
#       970, "Department of Children's Services", 140, "DCS Affiliated Schools"
#     )
#   ) %>%
#   arrange(system, school) %>%
#   write_csv("N:/ORP_accountability/data/2020_final_accountability_files/names.csv", na = "")

