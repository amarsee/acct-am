library(tidyverse)

# Absenteeism value add calculation
student_absent_prior <- read_csv('N:/ORP_accountability/data/2018_chronic_absenteeism/student_chronic_absenteeism_primary_enrollment_only.csv')
student_absent_current <- read_csv('N:/ORP_accountability/data/2019_chronic_absenteeism/student_chronic_absenteeism_Jul11.csv')

# Students not chronically absent in current year
non_chron_absent_current <- student_absent_current %>% 
  filter(str_length(student_id) == 7) %>%
  group_by(system,system_name, student_id) %>% 
  summarise(
    instructional_calendar_days = max(instructional_calendar_days, na.rm = TRUE),
    n_absences = sum(n_absences, na.rm = TRUE),
    isp_days = sum(isp_days, na.rm = TRUE),
    # system_name = first(system_name), 
    bhn = max(Black, Hispanic, Native, na.rm = TRUE),
    ed = max(ED, na.rm = TRUE),
    el = max(EL, na.rm = TRUE),
    swd = max(SWD, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  filter(isp_days/instructional_calendar_days >= 0.5) %>% 
  mutate(absentee_rate_current = round(n_absences/isp_days*100 + 1e-10,1)) # %>% 
  # filter(absentee_rate_current < 10)

# Students chronically absent in Previous Year
chron_absent_prior <- student_absent_prior %>%
  filter(str_length(student_id) == 7) %>%
  group_by(student_id) %>% 
  summarise(
    instructional_calendar_days = max(instructional_calendar_days, na.rm = TRUE),
    n_absences = sum(n_absences, na.rm = TRUE),
    isp_days = sum(isp_days, na.rm = TRUE),
    #  system_name = first(system_name), 
    bhn = max(Black, Hispanic, Native, na.rm = TRUE),
    ed = max(ED, na.rm = TRUE),
    el = max(EL, na.rm = TRUE),
    swd = max(SWD, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  filter(isp_days/instructional_calendar_days >= 0.5) %>%
  mutate(absentee_rate_prior = round(n_absences/isp_days*100 + 1e-10,1)) %>% 
  filter(absentee_rate_prior >= 10)

# District Chronic Absenteeism Indicator
total_chron_value <- non_chron_absent_current %>% 
  inner_join(chron_absent_prior, by = 'student_id') %>% 
  # select(-c(3:5,12:15)) %>% 
  transmute(system, system_name, bhn_current = bhn.x, ed_current = ed.x, el_current = el.x, swd_current = swd.x, absentee_rate_current,
         bhn_prior = bhn.y, ed_prior = ed.y, el_prior = el.y, swd_prior = swd.y, absentee_rate_prior) %>%
  mutate(
    all_chron_absent_prior = 1,
    all_non_chron_absent_current = if_else(absentee_rate_current < 10, 1, 0),
    bhn_non_chron_absent_current = if_else((absentee_rate_current < 10) & (bhn_current > 0), 1, 0),
    ed_non_chron_absent_current = if_else((absentee_rate_current < 10) & (ed_current > 0), 1, 0),
    el_non_chron_absent_current = if_else(absentee_rate_current < 10 & (el_current > 0), 1, 0),
    swd_non_chron_absent_current = if_else(absentee_rate_current < 10 & (swd_current > 0), 1, 0)
  )  %>%
  group_by(system) %>%
  summarise(
    n_ca_prior = sum(all_chron_absent_prior, na.rm = TRUE),
    n_not_ca_current = sum(all_non_chron_absent_current, na.rm = TRUE),
    n_bhn_prior_absent = sum(bhn_current, na.rm = TRUE),
    n_bhn_current_now_non_absent = sum(bhn_non_chron_absent_current, na.rm = TRUE),
    n_el_prior_absent = sum(el_current, na.rm = TRUE),
    n_el_current_now_non_absent = sum(el_non_chron_absent_current, na.rm = TRUE),
    n_ed_prior_absent = sum(ed_current, na.rm = TRUE),
    n_ed_current_now_non_absent = sum(ed_non_chron_absent_current, na.rm = TRUE),
    n_swd_prior_absent = sum(swd_current, na.rm = TRUE),
    n_swd_current_now_non_absent = sum(swd_non_chron_absent_current, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    all_pct = round(n_not_ca_current/n_ca_prior * 100 + 1e-10, 1),
    bhn_pct = round(n_bhn_current_now_non_absent/n_bhn_prior_absent * 100 + 1e-10, 1),
    el_pct = round(n_el_current_now_non_absent/n_el_prior_absent * 100 + 1e-10, 1),
    ed_pct = round(n_ed_current_now_non_absent/n_ed_prior_absent * 100 + 1e-10, 1),
    swd_pct = round(n_swd_current_now_non_absent/n_swd_prior_absent * 100 + 1e-10, 1)
  ) %>% 
  mutate(
    all_pct = if_else(n_ca_prior >= 30, all_pct, NA_real_),
    bhn_pct = if_else(n_bhn_prior_absent >= 30, bhn_pct, NA_real_),
    el_pct = if_else(n_el_prior_absent >= 30, el_pct, NA_real_),
    ed_pct = if_else(n_ed_prior_absent >= 30, ed_pct, NA_real_),
    swd_pct = if_else(n_swd_prior_absent >= 30, swd_pct, NA_real_)
  ) %>%
  select(c(system, all_pct:swd_pct)) %>%
  gather(subgroup, value_add_metric, -system) %>%
  mutate(
    subgroup = case_when(
      subgroup == 'all_pct' ~ 'All Students',
      subgroup == 'bhn_pct' ~ 'Black/Hispanic/Native American',
      subgroup == 'ed_pct' ~ 'Economically Disadvantaged',
      subgroup == 'el_pct' ~ 'English Learners with Transitional 1-4',
      subgroup == 'swd_pct' ~ 'Students with Disabilities'
    )
  ) %>%
  arrange(system, subgroup) %>%
  group_by(subgroup) %>%
  mutate(
    # quintile_1 = quantile(value_add_metric, probs = 0.20, na.rm = TRUE, names = FALSE),
    # quintile_2 = quantile(value_add_metric, probs = 0.40, na.rm = TRUE, names = FALSE),
    # quintile_3 = quantile(value_add_metric, probs = 0.60, na.rm = TRUE, names = FALSE),
    # quintile_4 = quantile(value_add_metric, probs = 0.80, na.rm = TRUE, names = FALSE),
    # value_added_pathway = case_when(
    #   value_add_metric >= quintile_4 ~ 4,
    #   value_add_metric >= quintile_3 ~ 3,
    #   value_add_metric >= quintile_2 ~ 2,
    #   value_add_metric >= quintile_1 ~ 1,
    #   value_add_metric < quintile_1 ~ 0,
    #   TRUE ~ NA_real_
    # )
    rank = if_else(!is.na(value_add_metric), rank(value_add_metric, ties.method = "max"), NA_integer_),
    denom = sum(!is.na(value_add_metric)),
    value_add_pathway = case_when(
      rank/denom >= 0.8 ~ 4L,
      rank/denom >= 0.6 ~ 3L,
      rank/denom >= 0.4 ~ 2L,
      rank/denom >= 0.2 ~ 1L,
      rank/denom < 0.2 ~ 0L
    )
  ) %>%
  ungroup() %>%
  # select(-c(quintile_1:quintile_4))
  transmute(system, subgroup, value_add_metric, value_add_pathway) %>% # grade, indicator = 'ELPA',
  arrange(subgroup, system)


# =========================== Write csv ================================
write_csv(total_chron_value, "N:/ORP_accountability/data/2019_final_accountability_files/district_absenteeism_va_AM.csv")

# ========================== Compare Files ============================
alex_absenteeism_va <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_absenteeism_va.csv")

absenteeism_diff_df <- setdiff(total_chron_value, alex_absenteeism_va) %>% 
  bind_rows(setdiff(alex_absenteeism_va, total_chron_value)) %>% 
  arrange(subgroup, system)











