# Graduation Cohort Appeals
# Evan Kramer
# 10/19/2018
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)

setwd("N:/ORP_accountability")

data = T
analysis = T
format = T
output = T
press = F
dropouts = F

# =================== Data ======================
if(data == T) {
  # Connect to database
  con <- dbConnect(
    JDBC("oracle.jdbc.OracleDriver", classPath="N:/ORP_accountability/ojdbc6.jar"),
    readRegistry("Environment", hive = "HCU")$EIS_MGR_CXN_STR[1],
    "EIS_MGR",
    readRegistry("Environment", hive = "HCU")$EIS_MGR_PWD[1]
  )
  
  # Pull data
  scd = as.tbl(dbGetQuery(con,
                          "SELECT scd.student_key, first_name, middle_name, last_name, suffix, date_of_birth,
    gender, immigrant, date_1st_enrolled_us_school, year_entered_grade9, native_language,
    ethnicity, race_i, race_a, race_p, race_b, race_w, cohortyear, calc_from, district_no AS system,
    school_no AS school, assignment, eoy_action, withdrawal_reason, completion_type, completion_period,
    completion_date, ell AS el, econ_dis AS ed, sped AS swd, year_withdrawn, included_in_cohort, 
    race_ethnicity, manual_intervention, homeless, cte, migrant, isp_id, save_as_filename, 
    modified_date AS upload_date, user_id, reviewer_user_id, comments, status, reviewed_date, revised_included_in_cohort
  FROM studentcohortdata scd
  LEFT OUTER JOIN studentcohortdocs doc ON scd.student_key = doc.student_key
  WHERE cohortyear = 2015")) %>% 
    janitor::clean_names()
  
#  Output file
  path = "N:/ORP_accountability/data/2019_graduation_rate/"
  file = "student_level.csv"
  if(file %in% list.files(path)) {
    if(!dir.exists(str_c(path, "Previous"))) {
      dir.create(str_c(path, "Previous"))
      dir.create(str_c(path, "Previous/", str_replace_all(today(), "-", "")))
    }
    if(!dir.exists(str_c(path, "Previous/", str_replace_all(today(), "-", "")))) {
      dir.create(str_c(path, "Previous/", str_replace_all(today(), "-", "")))
    }
    file.rename(str_c(path, file),
                str_c(path, "Previous/", str_replace_all(today(), "-", ""), "/", file))
  }
  write_csv(scd, str_c(path, file), na = "")
  
} else {
  rm(data)
}

# ================== Analysis ======================
if(analysis == T) {
  scd = read_csv("N:/ORP_accountability/data/2019_graduation_rate/student_level.csv", 
                 col_types = "dccccTccTdcccccccdcdddcddcTcccdccccccdcTcccdTc") %>% 
    # Add subgroups as variables for easy looping
    mutate(All = T, BHN = race_ethnicity %in% c("B", "H", "I"), ED = ed == "Y", EL = el == "Y",
           SWD = swd == "Y", Black = race_ethnicity == "B", Hispanic = race_ethnicity == "H",
           Native_Amer = race_ethnicity == "I", HPI = race_ethnicity == "P", Asian = race_ethnicity == "A",
           White = race_ethnicity == "W", Homeless = homeless == "Y", Male = gender == "M", Female = gender == "F",
           Migrant = migrant == "Y", Non_BHN = !race_ethnicity %in% c("B", "H", "I"), Non_ED = ed == "N", 
           Non_EL = el == "N", Non_Homeless = homeless == "N", Non_Migrant = migrant == "N", Non_SWD = swd == "N") %>% 
    filter(!is.na(system) & !is.na(school))
  
  # Initialize empty data frame for loop
  state_grad = as.tbl(data.frame())
  district_grad = as.tbl(data.frame())
  school_grad = as.tbl(data.frame())
  
  # Loop for all subgroups
  for (s in c("All", "BHN", "ED", "SWD", "EL", "Black", "Hispanic", "Native_Amer", "HPI", "Asian", "White",
              "Homeless", "Male", "Female", "Migrant", "Non_BHN", "Non_ED", "Non_EL", "Non_Homeless",
              "Non_Migrant", "Non_SWD")) {
    # State
    # Filter for relevant subgroup
    temp = filter(scd, get(s) == TRUE) %>% 
      # Summarize variables
      summarize(year = year(today()), subgroup = s,
                system = 0, system_name = "State of Tennessee",
                school = 0, school_name = "All Schools",
                grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                grad_count = sum(included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13), na.rm = T),
                grad_rate = ifelse(sum(included_in_cohort == "Y", na.rm = T) == 0,
                                   NA, round(100 * sum(included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13), na.rm = T) / 
                                               sum(included_in_cohort == "Y", na.rm = T) + 1e-9, 1))) 
    state_grad = bind_rows(state_grad, temp) 
    
    # District
    # Filter for relevant subgroup
    temp = filter(scd, get(s) == T) %>% 
      # Summarize variables
      group_by(system) %>%
      summarize(year = year(today()), subgroup = s,
                school = 0, school_name = "All Schools",
                grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                grad_count = sum(included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13), na.rm = T),
                grad_rate = ifelse(sum(included_in_cohort == "Y", na.rm = T) == 0,
                                   NA, round(100 * sum(included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13), na.rm = T) / 
                                               sum(included_in_cohort == "Y", na.rm = T) + 1e-9, 1))) %>% 
      ungroup()
    district_grad = bind_rows(district_grad, temp) 
    
    # School
    # Filter for relevant subgroup
    temp = filter(scd, get(s) == TRUE ) %>% 
      # Summarize variables
      group_by(system, school) %>%
      summarize(year = year(today()), subgroup = s,
                grad_cohort = sum(included_in_cohort == "Y", na.rm = T),
                grad_count = sum(included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13), na.rm = T),
                grad_rate = ifelse(sum(included_in_cohort == "Y", na.rm = T) == 0,
                                   NA, round(100 * sum(included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13), na.rm = T) / 
                                               sum(included_in_cohort == "Y", na.rm = T) + 1e-9, 1))) %>% 
      ungroup()
    school_grad = bind_rows(school_grad, temp) 
    
    # Remove temporary data frame
    rm(temp)
  } 
  rm(s)
} else {
  rm(analysis)
}

# ======== Format files ==================
if(format == T) {
  # State
  state_output = transmute(state_grad, year, system, system_name, subgroup = case_when(
    subgroup == "All" ~ "All Students",
    subgroup == "BHN" ~ "Black/Hispanic/Native American",
    subgroup == "ED" ~ "Economically Disadvantaged",
    subgroup == "EL" ~ "English Learners with Transitional 1-4",
    subgroup == "Black" ~ "Black or African American",
    subgroup == "Native_Amer" ~ "American Indian or Alaska Native",
    subgroup == "HPI" ~ "Native Hawaiian or Other Pacific Islander",
    subgroup == "SWD" ~ "Students with Disabilities",
    subgroup == "Non_BHN" ~ "Non-Black/Hispanic/Native American",
    subgroup == "Non_ED" ~ "Non-Economically Disadvantaged",
    subgroup == "Non_EL" ~ "Non-English Learners/Transitional 1-4",
    subgroup == "Non_Homeless" ~ "Non-Homeless",
    subgroup == "Non_Migrant" ~ "Non-Migrant",
    subgroup == "Non_SWD" ~ "Non-Students with Disabilities",
    T ~ subgroup
  ), 
  grad_cohort, grad_count, grad_rate) %>% 
    arrange(system, subgroup)  
  
  # District
  district_output = transmute(district_grad, year, system, subgroup = case_when(
    subgroup == "All" ~ "All Students",
    subgroup == "BHN" ~ "Black/Hispanic/Native American",
    subgroup == "ED" ~ "Economically Disadvantaged",
    subgroup == "EL" ~ "English Learners with Transitional 1-4",
    subgroup == "Black" ~ "Black or African American",
    subgroup == "Native_Amer" ~ "American Indian or Alaska Native",
    subgroup == "HPI" ~ "Native Hawaiian or Other Pacific Islander",
    subgroup == "SWD" ~ "Students with Disabilities",
    subgroup == "Non_BHN" ~ "Non-Black/Hispanic/Native American",
    subgroup == "Non_ED" ~ "Non-Economically Disadvantaged",
    subgroup == "Non_EL" ~ "Non-English Learners/Transitional 1-4",
    subgroup == "Non_Homeless" ~ "Non-Homeless",
    subgroup == "Non_Migrant" ~ "Non-Migrant",
    subgroup == "Non_SWD" ~ "Non-Students with Disabilities",
    T ~ subgroup
  ),
  grad_cohort, grad_count, grad_rate) %>% 
    # Add district names
    left_join(read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv") %>% 
                filter(school != 0) %>%
                group_by(system) %>% 
                summarize(system_name = first(system_name)), by = "system") %>%
    select(year, starts_with("system"), everything()) %>%
    arrange(system, subgroup)  
  
  # School
  school_output = transmute(school_grad, year, system, school, subgroup = case_when(
    subgroup == "All" ~ "All Students",
    subgroup == "BHN" ~ "Black/Hispanic/Native American",
    subgroup == "ED" ~ "Economically Disadvantaged",
    subgroup == "EL" ~ "English Learners with Transitional 1-4",
    subgroup == "Black" ~ "Black or African American",
    subgroup == "Native_Amer" ~ "American Indian or Alaska Native",
    subgroup == "HPI" ~ "Native Hawaiian or Other Pacific Islander",
    subgroup == "SWD" ~ "Students with Disabilities",
    subgroup == "Non_BHN" ~ "Non-Black/Hispanic/Native American",
    subgroup == "Non_ED" ~ "Non-Economically Disadvantaged",
    subgroup == "Non_EL" ~ "Non-English Learners/Transitional 1-4",
    subgroup == "Non_Homeless" ~ "Non-Homeless",
    subgroup == "Non_Migrant" ~ "Non-Migrant",
    subgroup == "Non_SWD" ~ "Non-Students with Disabilities",
    T ~ subgroup
  ),
  grad_cohort, grad_count, grad_rate) %>% 
    # Add school names
    left_join(read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv") %>% 
                bind_rows(
                  tribble(
                    ~system, ~system_name, ~school, ~school_name,
                    970, "Department of Children's Services", 25, "Gateway to Independence",
                    970, "Department of Children's Services", 65, "Mountain View Youth Development Center",
                    61, "Cleveland", 40, "F.I. Denning Center of Technology and Careers",
                    570, "Madison County", 40, "Jackson Central-Merry Academy of Medical Technology High School",
                    792, "Shelby County", 2085, "Carver High School",
                    792, "Shelby County", 2535, "Northside High School",
                    792, "Shelby County", 8125, "DuBois High School of Arts Technology",
                    792, "Shelby County", 8130, "DuBois High of Leadership Public Policy",
                    985, "Achievement School District", 35, "GRAD Academy Memphis"
                  )
                ), 
              by = c("system", "school")) %>%
    select(year, starts_with("system"), starts_with("school"), everything()) %>%
    arrange(system, school, subgroup) 
} else {
  rm(format)
}

# ======================= Output files ============================
if(output == T) {
  path = "N:/ORP_accountability/data/2019_graduation_rate/"
  for(f in c("state", "district", "school")) {
    file = str_c(f, "_grad_rate.csv")
    if(file %in% list.files(path)) {
      if(!dir.exists(str_c(path, "Previous"))) {
        dir.create(str_c(path, "Previous"))
        dir.create(str_c(path, "Previous/", str_replace_all(now(), "[-:]", "")))
      }
      if(!dir.exists(str_c(path, "Previous/", str_replace_all(now(), "[-:]", "")))) {
        dir.create(str_c(path, "Previous/", str_replace_all(now(), "[-:]", "")))
      }
      file.rename(str_c(path, file),
                  str_c(path, "Previous/", str_replace_all(now(), "[-:]", ""), "/", file))
    }
    if(f == "state") {
      write_csv(state_output, str_c(path, file), na = "")
    } else if(f == "district") {
      write_csv(district_output, str_c(path, file), na = "")
    } else if(f == "school") {
      write_csv(school_output, str_c(path, file), na = "")
    }
  }
} else {
  rm(output)
}

# =================== Press release =============================
if(press == T) {
  gd = read_csv("N:/ORP_accountability/data/2019_graduation_rate/district_grad_rate.csv") 
  gs = read_csv("N:/ORP_accountability/data/2019_graduation_rate/school_grad_rate.csv")
  gs2 = read_csv("N:/ORP_accountability/data/2019_graduation_rate/state_grad_rate.csv")
  s2018 = read_csv("N:/ORP_accountability/data/2019_graduation_rate/student_level.csv", 
                   col_types = "dccccTccTdcccccccdcdddcddcTcccdccccccdcTcccdTc")
  s2017 = read_csv("N:/ORP_accountability/data/2018_graduation_rate/student_level.csv")
  
  # How many more students graduated compared to last year?
  summarise(s2018, sum(included_in_cohort == "Y" & 
                         completion_type %in% c(1, 11, 12, 13), na.rm = T)) %>% 
    as.integer() - sum(s2017$grad_count, na.rm = T)
  
  # How many schools graduated 100% of their students?
  filter(gd, subgroup == "All Students" & grad_cohort >= 10 & grad_rate >= 95) %>% 
    left_join(filter(gs, system != 0 & school == 0 & subgroup == "All Students"),
              by = "system") %>%
    filter(grad_count)
  
  # How many districts improved compared to last year?
  filter(gd, grad_cohort >= 10 & subgroup == "All Students") %>% 
    left_join(read_csv("N:/ORP_accountability/data/2018_graduation_rate/district_grad_rate.csv") %>% 
                filter(grad_cohort >= 10 & school == 0 & system != 0 & subgroup == "All Students") %>% 
                transmute(system, grad_cohort, grad_count, grad_rate),
              by = "system") %>%
    #filter(grad_rate.x - grad_rate.y >= 5) %>% 
    # Who were the biggest improvers?
    arrange(desc(grad_cohort.x, grad_rate.x - grad_rate.y)) %>% 
    transmute(system, system_name, grad_rate.x, grad_rate.y, diff = grad_rate.x - grad_rate.y)
  
  # Did we close graduation gaps?
  read_csv("N:/ORP_accountability/data/2019_graduation_rate/district_grad_rate.csv") %>% 
    filter(grad_cohort >= 10) %>%
    mutate(subgroup = ifelse(subgroup == "English Learners with Transitional 1-4",
                             "English Learners", subgroup)) %>%
    right_join(read_csv("N:/ORP_accountability/data/2018_graduation_rate/district_grad_rate.csv") %>% 
                 filter(grad_cohort >= 10 & system != 0 & school == 0), 
               by = c("system", "subgroup")) %>% 
    transmute(system, system_name.y, subgroup, grad_cohort.x, grad_rate.x, grad_rate.y, diff = grad_rate.x - grad_rate.y) %>% 
    arrange(desc(diff)) %>% 
    filter(subgroup == "Students with Disabilities")
} else {
  rm(press)
}

# ========================== Dropouts =================================
if(dropouts == T) {
  scd = read_csv("data/2019_graduation_rate/student_level.csv",
                 col_types = "dccccTccTdcccccccdcdddcddcTcccdccccccdcTcccdTc")
  enr = read_csv("data/2019_graduation_rate/enrollment20190913.csv")
  
  a = anti_join(
    # Cohort data
    filter(scd, included_in_cohort == "Y" & is.na(completion_type) &
                         (is.na(completion_type) | completion_type == 5)),
    # Next year enrollments
    janitor::clean_names(enr) %>% 
      filter(type_of_service == "P") %>% 
      arrange(student_key, desc(begin_date), desc(is.na(end_date))) %>% 
      group_by(student_key) %>%
      summarize(withdrawal_reason = first(withdrawal_reason), 
                begin_date = first(begin_date), end_date = first(end_date)) %>%
      ungroup() %>% 
      filter(!withdrawal_reason %in% c(0, 1, 3, 4)), by = "student_key"
    ) %>% 
    transmute(student_key, dropout_count = T) %>% 
    right_join(scd, by = "student_key") %>% 
    mutate(grad_cohort = included_in_cohort == "Y", 
           grad_count = included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13),
           dropout_count = ifelse(included_in_cohort == "Y" & withdrawal_reason %in% c(0, 1, 3, 4) & 
                                    (is.na(completion_type) | !completion_type %in% c(1, 11, 12, 13)), 
                                             T, dropout_count),
           All = 1, Bhn = race_ethnicity %in% c("B", "H", "I"), Ed = ed == "Y", 
           El = el == "Y", Swd = swd == "Y", super = Bhn == T | Ed == T | El == T | Swd == T,
           non_bhn = !Bhn, non_ed = ed == "N", non_el = el == "N", non_swd = swd == "N",
           A = race_ethnicity == "A", B = race_ethnicity == "B", H = race_ethnicity == "H",
           I = race_ethnicity == "I", P = race_ethnicity == "P", W = race_ethnicity == "W", 
           MIG = migrant == 'Y',
           M = gender == "M", `F` = gender == "F") %>% 
    mutate_at(vars(grad_cohort:`F`), funs(as.integer(.)))
  
  # Summarize to school, district, and state level files
  collapse = tibble()
  
  for(s in c("All", "A", "B", "H", "P", "I", "W", "Bhn", "Ed", "Swd", "El", 
              "non_bhn", "non_ed", "non_swd", "non_el", "super", 'MIG', "M", "F")) {
    temp = filter(a, eval(parse(text = paste(s, "== 1")))) %>%
      group_by(system, school) %>%
      summarize_at(vars(grad_cohort, grad_count, dropout_count), funs(sum(., na.rm = T))) %>%
      mutate(subgroup = s) 
    collapse = bind_rows(collapse, temp)
  }
  
  # School
  mutate(collapse, subgroup = case_when(
    subgroup == "A" ~ "Asian",
    subgroup == "All" ~ "All Students",
    subgroup == "Bhn" ~ "Black/Hispanic/Native American",
    subgroup == "B" ~ "Black or African American", 
    subgroup == "Ed" ~ "Economically Disadvantaged",
    subgroup == "El" ~ "English Learners",
    subgroup == "F" ~ "Female",
    subgroup == "H" ~ "Hispanic",
    subgroup == "I" ~ "American Indian or Alaska Native",
    subgroup == "M" ~ "Male",
    subgroup == "MIG" ~ "Migrant",
    subgroup == "P" ~ "Native Hawaiian or Other Pacific Islander",
    subgroup == "non_bhn" ~ "Non-Black/Hispanic/Native American",
    subgroup == "non_ed" ~ "Non-Economically Disadvantaged",
    subgroup == "non_el" ~ "Non-English Learners",
    subgroup == "non_swd" ~ "Non-Students with Disabilities",
    subgroup == "Swd" ~ "Students with Disabilities",
    subgroup == "W" ~ "White")
  ) %>% 
    filter(!is.na(subgroup) & !is.na(system)) %>% 
    arrange(system, school, subgroup) %>% 
    transmute(system, school, subgroup, grad_cohort, grad_count, dropout_count, 
              grad_rate = ifelse(grad_cohort == 0, NA, round(100 * grad_count / grad_cohort, 1)),
              dropout_rate = ifelse(grad_cohort == 0, NA, round(100 * dropout_count / grad_cohort, 1))) %>% 
    left_join(read_csv("data/2019_final_accountability_files/names.csv"),
              by = c("system", "school")) %>%
    select(starts_with("system"), starts_with("school"), everything()) %>% 
    write_csv("data/2019_graduation_rate/school_dropout_rate.csv", na = "") %>%
    
    # District 
    group_by(system, subgroup) %>% 
    summarize_at(vars(grad_cohort, grad_count, dropout_count), funs(sum(., na.rm = T))) %>% 
    ungroup() %>%
    transmute(system, school = 0, school_name = "All Schools", subgroup, grad_cohort, grad_count, dropout_count, 
              grad_rate = ifelse(grad_cohort == 0, NA, round(100 * grad_count / grad_cohort, 1)),
              dropout_rate = ifelse(grad_cohort == 0, NA, round(100 * dropout_count / grad_cohort, 1))) %>% 
    left_join(read_csv("data/2019_final_accountability_files/names.csv") %>% 
                filter(school != 0) %>% 
                group_by(system) %>% 
                summarize(system_name = first(system_name)), by = "system") %>% 
    select(starts_with("system"), everything()) %>%
    write_csv("data/2019_graduation_rate/district_dropout_rate.csv", na = "") %>%
    
    # State
    group_by(subgroup) %>% 
    summarize_at(vars(grad_cohort, grad_count, dropout_count), funs(sum(., na.rm = T))) %>% 
    ungroup() %>%
    transmute(system = 0, system_name = "State of Tennessee", school = 0, school_name = "All Schools", 
              subgroup, grad_cohort, grad_count, dropout_count, 
              grad_rate = ifelse(grad_cohort == 0, NA, round(100 * grad_count / grad_cohort, 1)),
              dropout_rate = ifelse(grad_cohort == 0, NA, round(100 * dropout_count / grad_cohort, 1))) %>% 
    write_csv("data/2019_graduation_rate/state_dropout_rate.csv", na = "") 
} else {
  rm(dropouts)
}
