library(tidyverse)
library(janitor)
library(readxl)
library(readstata13)
library(ggplot2)

# ======================= Crosswalks =========================
core_region_crosswalk <- read.dta13("N:/ORP_accountability/projects/Andrew/Crosswalks/core_region_crosswalk.dta")
grades_offered <- read_csv("N:/ORP_accountability/projects/Andrew/Crosswalks/TNSCHGRADESOFF190717.csv")

middle_schools <- grades_offered %>% 
  transmute(system = as.numeric(state_leaid),
            school = as.numeric(state_school_id),
            grades_offered) %>% 
  mutate(grades_offered = case_when(
      grades_offered == 'PK' ~ '-1',
      grades_offered == 'KG' ~ '0',
      TRUE ~ grades_offered
    )
  ) %>% 
  group_by(system, school) %>% 
  summarise(
    max_grade = max(grades_offered),
    min_grade = min(grades_offered)
  ) %>% 
  ungroup() %>% 
  filter(min_grade %in% c('05', '06'),
         max_grade == '08')

# =========================== TVAAS ===============================
# Additionally, xx school districts – x percent – met or exceeded student growth expectations 
# with xx percent of all Tennessee schools earning a level 4 or 5, a xx percent (increase or decrease) from last year.

# TVASS 4 or 5 current
tvaas_district_current <- read_excel("N:/ORP_accountability/data/2019_tvaas/SAS-NIET District-Wide.xlsx") %>% 
  clean_names() %>% 
  summarise(
    n_count = n(),
    total_4_5 = sum(system_wide_composite %in% c(4,5))
  ) %>% 
  ungroup() %>% 
  mutate(
    percent_met_exceeded = round(total_4_5 / n_count * 100, 1)
  )

tvaas_school_current <- read_excel("N:/ORP_accountability/data/2019_tvaas/SAS-NIET School-Wide.xlsx") %>% 
  clean_names() %>% 
  summarise(
    n_count = n(),
    total_4_5 = sum(school_wide_composite %in% c(4,5))
  ) %>% 
  ungroup() %>% 
  mutate(
    percent_met_exceeded = round(total_4_5 / n_count * 100, 1)
  )

tvaas_school_prior <- read_excel("N:/ORP_accountability/data/2018_tvaas/School Composite Level.xlsx") %>% 
  clean_names() %>% 
  summarise(
    n_count = n(),
    total_4_5 = sum(school_wide_composite %in% c(4,5))
  ) %>% 
  ungroup() %>% 
  mutate(
    percent_met_exceeded = round(total_4_5 / n_count * 100, 1)
  )

# ============= Grade 3 Reading Three Year Trend ==================
# Insert Third grade reading data trend summary, stressing key indicator of student success;
grade_3_ela <- read_csv('N:/ORP_accountability/data/2019_final_accountability_files/state_assessment_file.csv') %>% 
  filter(grade == '3', subject == 'ELA', subgroup == 'All Students') %>% 
  group_by(year, subject, grade) %>% 
  summarise(
    n_on_track_mastered = sum(n_on_track, na.rm = TRUE) + sum(n_mastered, na.rm = TRUE),
    n_valid_tests = sum(valid_tests, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_on_track_mastered = round(n_on_track_mastered/ n_valid_tests * 100 + 1e-5, 1)
  )

# Plot it 
grade_3_ela %>% 
  ggplot(aes(x = factor(year), y = pct_on_track_mastered, label = pct_on_track_mastered)) +
  geom_bar(stat= 'identity',position = position_dodge(), fill = '#decade') +
  geom_text(size = 6, position = position_dodge(width = 1), aes(hjust=.5, vjust = -1)) +
  # scale_fill_discrete(name="",breaks=c("n_exited", "n_not_met_criteria"), labels=c("Met Criteria", "Did Not Meet Criteria")) +
  labs(x = "Year", y = "Proficiency Rate", title = "",
       fill = "") +
  # ylim(0,100) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0, 100)) +
  theme_minimal() +
  theme(text = element_text(size=22))


# ================== ELA Data Trend Summary =====================
# Insert ELA data trend summary, Elementary and Middle:

grade_3_ela <- read_csv('N:/ORP_accountability/data/2019_final_accountability_files/state_assessment_file.csv') %>% 
  filter(grade == '3', subject == 'ELA', subgroup == 'All Students') %>% 
  group_by(year, subject, grade) %>% 
  summarise(
    n_on_track_mastered = sum(n_on_track, na.rm = TRUE) + sum(n_mastered, na.rm = TRUE),
    n_valid_tests = sum(valid_tests, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_on_track_mastered = round(n_on_track_mastered/ n_valid_tests * 100 + 1e-5, 1)
  )







