library(acct)
library(tidyverse)

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

master <- readxl::read_excel("H:/EDEN Data/EDEN 19-20/LEA and School Master Files/2019-20 EDFacts School Master File_1-27-20.xls", sheet = 2) %>%
    janitor::clean_names() %>%
    transmute(
        system = as.integer(dg_4_lea_id_state),
        system_name = extra_item_lea_name,
        school = as.integer(dg_5_school_id_state),
        school_name = dg_7_school_name
    ) 

schools_missing <- master %>%
    anti_join(schools, by = c("system", "school")) %>%
    select(system, school, school_name)

schools_present <- master %>% 
  inner_join(schools %>% select(system, school), by = c('system', 'school')) %>% 
  select(system, school, school_name)

districts_present <- bind_rows(fall_eoc) %>% # , spring_eoc, tn_ready
  transmute(system) %>%
  distinct() %>% 
  inner_join(
    master %>% 
      select(system, system_name) %>% 
      distinct(),
    by = c('system')
  )

bind_rows(schools_present, schools_missing) %>%
    left_join(districts_present, by = "system") %>%
    select(system, system_name, school, school_name) %>%
    arrange(system, school) %>%
    write_csv("N:/ORP_accountability/data/2020_final_accountability_files/names.csv", na = "")

master %>%
  distinct() %>% 
  bind_rows(
    tribble(
      ~system, ~system_name, ~school, ~school_name,
      970, "Department of Children's Services", 25, "Gateway to Independence",
      970, "Department of Children's Services", 45, "Wilder Youth Development Center",
      970, "Department of Children's Services", 65, "Mountain View Youth Development Center",
      970, "Department of Children's Services", 140, "DCS Affiliated Schools"
    )
  ) %>%
  arrange(system, school) %>%
  write_csv("N:/ORP_accountability/data/2020_final_accountability_files/names.csv", na = "")

