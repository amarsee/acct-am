library(tidyverse)
library(readstata13)

# Value add metric based on change in percent of graduates who are considered ready graduates

subgroups <- c("All Students", "Black/Hispanic/Native American",  "Economically Disadvantaged", "English Learners with Transitional 1-4", 
               "Students with Disabilities")

ready_grad_prior <- read_csv('N:\\ORP_accountability\\data\\2018_final_accountability_files\\district_ready_grad.csv') %>% 
  mutate(
    pct_ready_grad = if_else(grad_cohort < 30, NA_real_, pct_ready_grad),
    subgroup = if_else(subgroup == 'English Learners', "English Learners with Transitional 1-4", subgroup)
  )

ready_grad_current <- read_csv('N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_district.csv') %>% 
  mutate(
    pct_ready_grad = if_else(n_count < 30, NA_real_, pct_ready_grad),
    subgroup = if_else(subgroup == 'English Learners', "English Learners with Transitional 1-4", subgroup)
  )

ready_grad_value_add <- ready_grad_current %>% 
  select(system, subgroup, pct_ready_grad) %>% 
  left_join(ready_grad_prior %>% select(system, subgroup, pct_ready_grad_prior = pct_ready_grad), by = c('system', 'subgroup')) %>% 
  mutate(
    value_add_metric = pct_ready_grad - pct_ready_grad_prior
  ) %>% 
  group_by(subgroup) %>% 
  mutate(
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
  # select(-c(quintile_1:quintile_4)) %>% 
  # select(-percentile) %>% 
  filter(subgroup %in% subgroups) %>% 
  transmute(system, subgroup, indicator = 'Graduation Rate', value_add_metric, value_add_pathway) %>% # grade, indicator = 'ELPA',
  arrange(subgroup, system)



# =========================== Write csv ================================
write_csv(ready_grad_value_add, "N:/ORP_accountability/data/2019_final_accountability_files/district_grad_va_AM.csv")

# ========================== Compare Files ============================
alex_grad_va <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_grad_va.csv")

grad_rate_diff_df <- setdiff(ready_grad_value_add, alex_grad_va) %>% 
  bind_rows(setdiff(alex_grad_va, ready_grad_value_add)) %>% 
  arrange(subgroup, system)












