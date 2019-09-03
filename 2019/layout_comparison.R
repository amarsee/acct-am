library(readxl)
library(tidyverse)

# =================== Assessment Files ===========================

school_assessment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_assessment_file.csv")

district_assessment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_assessment_file.csv")

student_level <- read_csv("N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv")


# ============================ ELPA Files ==================================

elpa_student_level <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_student.csv")

elpa_school_level <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_school.csv")
