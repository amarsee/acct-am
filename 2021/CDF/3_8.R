library(tidyverse)

# CDF is a fixed width file - create a tibble with start position, end position, and name for each variable
layout <- tribble(
    ~start, ~end, ~colnames,
    7, 11, "system",
    12, 86, "system_name",
    87, 90, "school",
    91, 165, "school_name",
    166, 200, "last_name",
    201, 235, "first_name",
    236, 236, "middle_initial",
    245, 253, "unique_student_id",
    276, 277, "grade",  # Tested grade
    278, 280, "content_area_code",
    377, 377, "test_mode",
    378, 378, "attempted",
    379, 380, "modified_format",
    390, 409, "teacher_of_record_tln",
    592, 593, "reason_not_tested",
    594, 595, "ri_status",

    702, 704, "raw_score",
    708, 711, "scale_score",
    712, 726, "performance_level",
    727, 729, "scale_score_lb_ci",
    730, 732, "scale_score_ub_ci",
    747, 876, "item_response_array"
)

# Read in cdf according to layout
cdf <- read_fwf(file = "N:/Assessment_Data Returns/TCAP_Grades 3-8/2018-19/archive/2018-2019 TN 2019 Spring 3-8 CDF Final Scores-20190704.txt",
    col_positions = fwf_positions(
        start = layout$start,
        end = layout$end,
        col_names = layout$colnames
    ), 
    col_types = c("iciccccdiccccdiiddcddc")
) %>%
    filter(!(content_area_code == "ENG" & unique_student_id == 4244992)) %>%
    mutate(ri_status = if_else(ri_status == 6 & reason_not_tested == 1, 0L, ri_status))

# Demographic file
demographics <- read_csv("N:/TNReady/2018-19/spring/demographics/spring_2019_assessment_demographics_combined_pull_20190610.csv") %>%
    # Student IDs should be 7 digits
    filter(str_length(student_key) == 7) %>%
    transmute(
        unique_student_id = student_key,
        system = district_id,
        school = school_id,
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

write_csv(cdf_with_demographics, path = "N:/ORP_accountability/data/2019_cdf/2019_3_8_cdf.csv")
