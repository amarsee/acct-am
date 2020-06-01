# Daily Attendance Rate

library(tidyverse)

daily_absences <- read_csv("N:/ORP_accountability/projects/Andrew/acct-am/2020/Modeling/attendance/data/daily_absences_2012-19.csv")

daily_enrollment <- read_csv("N:/ORP_accountability/projects/Andrew/acct-am/2020/Modeling/attendance/data/daily_enrollment_2012-19.csv")

daily_attendance_rate <- (daily_enrollment %>% mutate(school_year = paste0(school_year, '-', school_year+1))) %>% 
  left_join(daily_absences, by = c('school_year', 'system', 'school', 'id_date')) %>% 
  filter(!is.na(n_absent), n_absent <= n_enrolled) %>% # 3 cases where absences were one higher than enrollment
  mutate(
    attendance_rate = round((n_enrolled - n_absent) / n_enrolled * 100 + 1e-10, 1)
  )
  
write_csv(daily_attendance_rate %>% select(-max_abs), "N:/ORP_accountability/projects/Andrew/acct-am/2020/Modeling/attendance/data/daily_attendance_2012-19.csv", na = '')













