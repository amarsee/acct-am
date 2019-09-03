library(tidyverse)
library(janitor)
library(readstata13)

state_ready_grad_current <- read_csv("N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_state.csv")

state_ready_grad_prior <- read.dta13("N:/ORP_accountability/data/2018_final_accountability_files/ready_grad_state2018_JW.dta")


