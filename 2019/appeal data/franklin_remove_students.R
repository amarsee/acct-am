library(tidyverse)
library(janitor)
library(readxl)
library(readstata13)
library(acct)

# ================ Read CSV =========================
fssd_student_level <- read_csv("N:/ORP_accountability/projects/NCLBAppeals/2018-19 Archive Accountability Web Files/20190812/941_StudentLevelFiles_19Jul2019.csv")

# ============ Remove IDs given ================
fssd_ids_removed <- fssd_student_level %>% 
  filter(!state_student_id %in% c(4615320, 4527412, 4840680, 4592556))

# ==================== Write csv ====================
write_csv(fssd_ids_removed, "N:/ORP_accountability/projects/Andrew/Accountability/2019/appeal data/941_StudentLevelFiles_14Aug2019.csv", na = '')
