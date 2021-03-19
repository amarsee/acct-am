# Process CDF
# Fall EOC

library(tidyverse)

# CDF is a fixed width file - create a tibble with start position, end position, and name for each variable
layout <- tribble(
  ~start, ~end, ~colnames,
  33, 37, "system",
  38, 112, "system_name",
  113, 116, "school",
  117, 191, "school_name",
  192, 241, "last_name",
  242, 291, "first_name",
  292, 292, "middle_initial",
  302, 310, "unique_student_id",
  346, 347, "grade",  # Student grade
  # 348, 349, "tested_grade",
  350, 352, "content_area_code",
  428, 428, "test_mode",
  429, 429, "attempted",
  430, 431, "modified_format",
  441, 460, "teacher_of_record_tln",
  543, 543, "reason_not_tested",
  544, 544, "ri_status",
  
  733, 735, "raw_score",
  742, 745, "scale_score",
  746, 760, "performance_level",
  764, 766, "scale_score_lb_ci",
  761, 763, "scale_score_ub_ci",
  781, 910, "item_response_array"
)

# Read in cdf according to layout
cdf <- read_fwf("N:/Assessment_Data Returns/TCAP_End-of-Course/2020-2021/Fall2020_EOC/2020_TN_Fall_2020_EOC_CDF_20210204.txt",
                col_types = "iciccccdiccccdiiiiciic",
                col_positions = fwf_positions(
                  start = layout$start,
                  end = layout$end,
                  col_names = layout$colnames
                )
)

# Demographic file
demographics <- read_csv("N:/ORP_accountability/projects/Andrew/Demographics/Data/student_demographics_03182021.csv") %>%
  # Student IDs should be 7 digits
  filter(str_length(student_key) == 7) %>%
  transmute(
    unique_student_id = student_key,
    system = district_no,
    school = school_no,
    el = isel,
    t1234 = t1t2,
    reported_race = case_when(
      ethnicity == "H" ~ "Hispanic/Latino",
      isblack == 1 ~ "Black or African American",
      isamericanindian == 1 ~ "American Indian/Alaska Native",
      ispacificislander == 1 ~ "Native Hawaiian/Pac. Islander",
      isasian == 1 ~ "Asian",
      iswhite == 1 ~ "White",
      TRUE ~ "Unknown"
    ),
    el_arrived_year_1 = elrecentlyarrivedyearone,
    el_arrived_year_2 = elrecentlyarrivedyeartwo,
    gender,
    gifted = isgifted,
    migrant = ismigrant,
    title_1 = title1,
    special_ed = specialeducation,
    functionally_delayed = isfunctionallydelayed,
    economically_disadvantaged = case_when(
      codeab == 1 ~ 1,
      codeab == 2 ~ 0
    ),
    enrolled_50_pct_district = district50percent,
    enrolled_50_pct_school = school50percent
  )

cdf_with_demographics <- left_join(cdf, demographics, by = c("unique_student_id", "system", "school")) %>%
  filter(
    system <= 986,  # Private School districts
    school != 981,  # Homeschool
    grade %in% 1:12 | is.na(grade)  # Grade 13
  ) %>%
  mutate(grade = if_else(grade %in% 1:2, NA_integer_, grade))

write_csv(cdf_with_demographics, path = "N:/ORP_accountability/data/2021_cdf/2021_fall_eoc_cdf.csv", na = '')

# Content Area Codes ==========
# A1 = Algebra I
# A2 = Algebra II
# B1 = Biology
# E1 = English I
# E2 = English II
# G1 = Geometry
# M1 = Integrated Math I
# M2 = Integrated Math II
# M3 = Integrated Math III
# U1 = U.S. History

