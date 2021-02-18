# Add CORE Region to data files

library(tidyverse)
library(readstata13)

add_core <- function(file_path) {
  core_xwalk <- read.dta13("N:/ORP_accountability/projects/Andrew/Crosswalks/core_region_crosswalk.dta")
  file_base <- gsub('.csv', '', file_path)
  file_data <- read_csv(file_path)
  data_w_region <- file_data %>% 
    left_join(core_xwalk %>% select(system, region), by = 'system')
  
  write_csv(data_w_region, str_c(file_base, '_with_region.csv'), na = '')
}

# ======================= ELPA ==============================
# School
add_core("N:/ORP_accountability/data/2020_ELPA/wida_growth_standard_school.csv")

# District()
add_core("N:/ORP_accountability/data/2020_ELPA/wida_growth_standard_district.csv")

# ===================== Absenteeism =========================
# School
add_core("N:/ORP_accountability/data/2020_chronic_absenteeism/school_chronic_absenteeism_Aug14.csv")

# District
add_core("N:/ORP_accountability/data/2020_chronic_absenteeism/district_chronic_absenteeism_Aug14.csv")


# ================== ACT =========================
# School
add_core("N:/ORP_accountability/data/2020_ACT/ACT_school_post_appeals.csv")

# District
add_core("N:/ORP_accountability/data/2020_ACT/ACT_district_post_appeals.csv")








