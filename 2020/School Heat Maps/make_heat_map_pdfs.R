library(tidyverse)
library(janitor)
library(readxl)
library(readstata13)
library(knitr)
library(filesstrings)
library(lubridate)

schools_to_update <- read_excel("N:/ORP_accountability/projects/Andrew/Accountability/2019/appeal data/score_changes.xlsm") %>% 
  filter(!is.na(system)) %>% 
  mutate(system_school = paste0(as.character(system), '/', as.character(school)))

total_acct <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file_aug14.csv")

school_acct_current <- read_csv("N:/ORP_accountability/projects/Andrew/Accountability/2019/school_heat_map_data/school_report_scores_Aug15.csv") %>% 
  filter(!is.na(pool)) %>% 
  left_join(total_acct %>% select(system, school, designation_ineligible) %>% distinct(), by = c('system', 'school')) %>% 
  filter(designation_ineligible == 0) %>% 
  select(-designation_ineligible)

# Creating a list of districts
# district_list <- unique(school_acct_current$system)
# district_list <- c(12, 52, 60, 190, 301, 330, 470, 490, 570, 630, 792, 850, 890, 901, 940, 951)
# district_list <- c(260, 470, 350, 275, 940, 821, 190, 330, 620, 830, 950)
district_list <- sort(unique(schools_to_update$system))

for (dist in district_list) {
  # Filter to just the district
  dist_df <- school_acct_current %>% 
    filter(system == dist) %>% 
    group_by(school, school_name, pool) %>% 
    summarise(temp = max(`Indicator Score (60-40)`, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(desc(pool), school_name)
  # list of school names
  dist_school_list <- dist_df$school_name
  # list of school numbers 
  dist_school_nos <- dist_df$school
  # list of pools
  dist_pools <- dist_df$pool
  
  # Index list
  ind_list <- c(1:nrow(dist_df))
  # Base Directory to put the heat maps in
  dist_dir_str <- paste0("N:/ORP_accountability/projects/2019_school_accountability/heat maps")#/",
                         #"district_", dist)
  # Check if directory exists for pdfs. If it doesn't, create it
  if(!dir.exists(dist_dir_str)) {
    dir.create(dist_dir_str)
  }
  for(school_index in ind_list) {
    school_num <- dist_school_nos[[school_index]]
    pasted_dist_school <- paste0(as.character(dist), '/', as.character(school_num))
    
    if(pasted_dist_school %in% schools_to_update$system_school){
      # file name
      school_file_base <- unlist(str_split(dist_school_list[[school_index]], pattern = "(\\W+)"))
      school_filename_joined <- paste(school_file_base, collapse = '_')

      school_filename <- paste0(dist, '_', 'SchoolHeatMapsFile',
                                '_', school_filename_joined, '_', dist_school_nos[[school_index]], '_',
                                as.character(month(Sys.Date(), label = TRUE)), format(Sys.Date(), "%d"), '.pdf')
      absolute_school_path <- paste0(dist_dir_str, "/", school_filename)
      # Render Rmd with the parameters passed in
      rmarkdown::render("N:/ORP_accountability/projects/Andrew/Accountability/2019/school_heat_map_code/school_heat_map_rmd.Rmd", params = list(
        district_id = dist,
        school_id = school_num,
        set_title = dist_school_list[[school_index]]
      ),
      output_file = absolute_school_path)
    }

    # file.rename("heat_maps_pre_code_2019.pdf", school_filename)
    # file.move(school_filename, dist_dir_str, overwrite = TRUE)
  }
  
  # print(ind_list)
}
