library(tidyverse)
library(readxl)
library(readstata13)

designations <- read_excel("N:/ORP_accountability/data/2019_final_accountability_files/school_designations_file_Jul29.xlsx")


core_regions <- read.dta13("N:/ORP_accountability/projects/Andrew/Crosswalks/core_region_crosswalk.dta")

reward_with_core <- designations %>% 
  filter(str_detect(designation, 'Reward')) %>% 
  left_join(core_regions %>% select(system, region), by = 'system') %>% 
  arrange(region)

write_csv(reward_with_core, "N:/ORP_accountability/data/2019_final_accountability_files/reward_schools_with_core_region.csv", na= '')
