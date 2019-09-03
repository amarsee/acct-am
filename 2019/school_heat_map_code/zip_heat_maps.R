library(tidyverse)
library(janitor)
library(lubridate)

setwd(str_c("N:/ORP_accountability/projects/", year(now()), "_school_accountability/heat maps"))

total_acct <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv")

school_acct_current <- read_csv("N:/ORP_accountability/projects/Andrew/Accountability/2019/school_heat_map_data/school_report_scores.csv") %>% 
  filter(!is.na(pool)) %>% 
  left_join(total_acct %>% select(system, school, designation_ineligible) %>% distinct(), by = c('system', 'school')) %>% 
  filter(designation_ineligible == 0) %>% 
  select(-designation_ineligible)

# Creating a list of districts
# district_list <- unique(school_acct_current$system)
# 
# file_list <- list.files("N:/ORP_accountability/projects/2019_school_accountability/heat maps")
# file_list <- paste0("N:/ORP_accountability/projects/2019_school_accountability/heat maps/", file_list)

zip_base <- "N:/ORP_accountability/projects/2019_school_accountability/heat maps/Zipped/"

# List of files
file_table = tibble(
  filename = list.files(), 
  system = str_sub(filename, 1, 3) %>% 
    str_replace_all("_", "") %>% 
    as.numeric()
) %>% 
  filter(!is.na(system))

for(d in sort(unique(file_table$system))) {
  # file_vector = file_table[startsWith(file_list, paste0(as.character(d), '_SchoolHeatMapsFile'))]
  file_vector = file_table$filename[file_table$system == d]
  
  if(length(file_vector > 0)) {
    zip(
      zipfile = paste0(zip_base, as.character(d), "_SchoolHeatMapsFile_",as.character(month(Sys.Date(), label = TRUE)), 
                       as.character(day(Sys.Date()))),
      files = file_vector,
      flags = " a -tzip", 
      zip = "C:\\Program Files\\7-Zip\\7Z") # Had to download the 7-Zip program and point the function to it
  }
}









