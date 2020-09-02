library(tidyverse)
library(janitor)
library(lubridate)

# ====================== State =======================
state_grad <- read_csv(paste0("N:/ORP_accountability/data/", as.character(year(Sys.Date())), "_graduation_rate/state_grad_rate.csv"),
                       col_types = cols(.default = "c"))

state_suppressed <- state_grad %>% 
  mutate(
    grad_count = if_else(as.numeric(grad_rate) < 1 | as.numeric(grad_rate) > 99, '**', grad_count),
    grad_rate = if_else(as.numeric(grad_rate) < 1 | as.numeric(grad_rate) > 99, '**', grad_rate)
  ) %>% 
  mutate_at(
    .vars = c('grad_rate', 'grad_count'),
    .funs = ~ case_when(
      as.numeric(grad_cohort) < 10 ~ '*',
      TRUE ~ .
    )
  ) 

write_csv(state_suppressed, paste0("N:/ORP_accountability/data/", as.character(year(Sys.Date())), "_graduation_rate/state_grad_rate_suppressed.csv"))

# ======================= District =======================
district_grad <- read_csv(paste0("N:/ORP_accountability/data/", as.character(year(Sys.Date())), "_graduation_rate/district_grad_rate.csv"),
                       col_types = cols(.default = "c"))

district_suppressed <- district_grad %>% 
  mutate(
    grad_count = if_else(as.numeric(grad_rate) < 1 | as.numeric(grad_rate) > 99, '**', grad_count),
    grad_rate = if_else(as.numeric(grad_rate) < 1 | as.numeric(grad_rate) > 99, '**', grad_rate)
  ) %>% 
  mutate_at(
    .vars = c('grad_rate', 'grad_count'),
    .funs = ~ case_when(
      as.numeric(grad_cohort) < 10 ~ '*',
      TRUE ~ .
    )
  ) 

write_csv(district_suppressed, paste0("N:/ORP_accountability/data/", as.character(year(Sys.Date())), "_graduation_rate/district_grad_rate_suppressed.csv"))

# ======================= School =======================
school_grad <- read_csv(paste0("N:/ORP_accountability/data/", as.character(year(Sys.Date())), "_graduation_rate/school_grad_rate.csv"),
                          col_types = cols(.default = "c"))

school_suppressed <- school_grad %>% 
  mutate(
    grad_count = if_else(as.numeric(grad_rate) < 1 | as.numeric(grad_rate) > 99, '**', grad_count),
    grad_rate = if_else(as.numeric(grad_rate) < 1 | as.numeric(grad_rate) > 99, '**', grad_rate)
  ) %>% 
  mutate_at(
    .vars = c('grad_rate', 'grad_count'),
    .funs = ~ case_when(
      as.numeric(grad_cohort) < 10 ~ '*',
      TRUE ~ .
    )
  ) 

write_csv(school_suppressed, paste0("N:/ORP_accountability/data/", as.character(year(Sys.Date())), "_graduation_rate/school_grad_rate_suppressed.csv"))







