library(tidyverse)
library(janitor)

district_list <- read_csv("N:\\ORP_accountability\\data\\2019_final_accountability_files\\names.csv") %>% 
  distinct(system)

for(district in district_list){
# ====================== CA Split Files =================================
  
  ca_base_student <- '_ChronicAbsenteeismStudentFile_17Jun2019'
  ca_base_school <- '_ChronicAbsenteeismSchoolFile_17Jun2019'
  ca_base_dist <- '_ChronicAbsenteeismDistrictFile_17Jun2019'
  
  ca_directory <- 'N:/ORP_accountability/data/2019_chronic_absenteeism/split/'
  
  ca_abs_student <- paste0(ca_directory, as.character(district), ca_base_student, '.csv')
  ca_abs_school <- paste0(ca_directory, as.character(district), ca_base_school, '.csv')
  ca_abs_dist <- paste0(ca_directory, as.character(district), ca_base_dist, '.csv')
  
  ca_file_vec <- c()
  if(file.exists(ca_abs_student)){
    ca_file_vec <- append(ca_file_vec, ca_abs_student)
  }
  if(file.exists(ca_abs_school)){
    ca_file_vec <- append(ca_file_vec, ca_abs_school)
  }
  if(file.exists(ca_abs_dist)){
    ca_file_vec <- append(ca_file_vec, ca_abs_dist)
  }
  

# ====================== ELPA Split =================
  elpa_base_student <- '_ACCESSStudentLevelFile_13Jun2019'
  elpa_base_school <- '_ACCESSSchoolLevelFile_13Jun2019'
  elpa_base_dist <- '_ACCESSDistrictLevelFile_13Jun2019'
  
  elpa_directory <- 'N:/ORP_accountability/data/2019_ELPA/split/'
  
  elpa_abs_student <- paste0(elpa_directory, as.character(district), elpa_base_student, '.csv')
  elpa_abs_school <- paste0(elpa_directory, as.character(district), elpa_base_school, '.csv')
  elpa_abs_dist <- paste0(elpa_directory, as.character(district), elpa_base_dist, '.csv')
  
  elpa_file_vec <- c()
  if(file.exists(elpa_abs_student)){
    elpa_file_vec <- append(elpa_file_vec, elpa_abs_student)
  }
  if(file.exists(elpa_abs_school)){
    elpa_file_vec <- append(elpa_file_vec, elpa_abs_school)
  }
  if(file.exists(elpa_abs_dist)){
    elpa_file_vec <- append(elpa_file_vec, elpa_abs_dist)
  }
  # ================================ Ready Graduate Split =======================================
  rg_base_student <- '_ReadyGraduate_Student_Level_20Jun2019'
  rg_base_school <- '_ReadyGraduate_School_Level_20Jun2019'
  rg_base_dist <- '_ReadyGraduate_District_Level_20Jun2019'
  
  rg_directory <- 'N:/ORP_accountability/projects/2019_ready_graduate/Data/split/'
  
  rg_abs_student <- paste0(rg_directory, as.character(district), rg_base_student, '.csv')
  rg_abs_school <- paste0(rg_directory, as.character(district), rg_base_school, '.csv')
  rg_abs_dist <- paste0(rg_directory, as.character(district), rg_base_dist, '.csv')
  
  rg_file_vec <- c()
  if(file.exists(rg_abs_student)){
    rg_file_vec <- append(rg_file_vec, rg_abs_student)
  }
  if(file.exists(rg_abs_school)){
    rg_file_vec <- append(rg_file_vec, rg_abs_school)
  }
  if(file.exists(elpa_abs_dist)){
    rg_file_vec <- append(rg_file_vec, rg_abs_dist)
  }
  
  # ===================================== Graduation Rate ==========================================
  
  # ========================================= Assessment =====================================
  # ach_base_student <- 'ReadyGraduate_Student_Level_20Jun2019'
  # ach_base_school <- 'ReadyGraduate_School_Level_20Jun2019'
  # ach_base_dist <- 'ReadyGraduate_District_Level_20Jun2019'
  # 
  # ach_directory <- 'N:/ORP_accountability/projects/2019_ready_graduate/Data/split/'
  # 
  # ach_abs_student <- paste0(ach_directory, as.character(district), ach_base_student, '.csv')
  # ach_abs_school <- paste0(ach_directory, as.character(district), ach_base_student, '.csv')
  # ach_abs_dist <- paste0(ach_directory, as.character(district), ach_base_dist, '.csv')
  # 
  # rg_file_vec <- c()
  # if(file.exists(ach_abs_student)){
  #   rg_file_vec <- append(rg_file_vec, ach_abs_student)
  # }
  # if(file.exists(ach_abs_school)){
  #   rg_file_vec <- append(rg_file_vec, ach_abs_school)
  # }
  # if(file.exists(elpa_abs_dist)){
  #   rg_file_vec <- append(rg_file_vec, ach_abs_dist)
  # }
  
  # ================================= Combine and zip =============================
  total_vec <- c(ca_file_vec, elpa_file_vec, rg_file_vec) # ach_file_vec
  
  zip_base <- 'N:/ORP_accountability/projects/Andrew/Accountability/2019/test_zipped/'
  file_suffix <- '_first_release_jul8'
  final_zip_path <- paste0(zip_base, as.character(district), file_suffix)
  
  if(length(total_vec>0)){
   zip(final_zip_path, total_vec, flags = " a -tzip", zip = "C:\\Program Files\\7-Zip\\7Z") # Had to download the 7-Zip program and point the function to it
  }
  
}







