library(tidyverse)


student_2019 <- read_csv("N:/ORP_accountability/data/2019_ACT/ACT_student_post_appeals_AM.csv",
                         col_types = 'icccccccciccccccciciiiciiccccciccccccicccccccciiiiiiillllllllllllllllllllllllll')

student_2020 <- read_csv("N:/ORP_accountability/data/2020_ACT/ACT_student_post_appeals.csv",
                         col_types = 'icccccccciccccccciciiiciiccccciccccccicccccccciiiiiiillllllllllllllllllllllllll')

shapiro.test((student_2019 %>% filter(!is.na(composite)))$composite)

wilcox.test((student_2019 %>% filter(!is.na(composite)))$composite, 
            (student_2020 %>% filter(!is.na(composite)))$composite)

t.test((student_2019 %>% filter(!is.na(composite)))$composite, 
            (student_2020 %>% filter(!is.na(composite)))$composite)

t.test((student_2019 %>% filter(!is.na(english)))$english, 
       (student_2020 %>% filter(!is.na(english)))$english)

t.test((student_2019 %>% filter(!is.na(math)))$math, 
       (student_2020 %>% filter(!is.na(math)))$math)

t.test((student_2019 %>% filter(!is.na(reading)))$reading, 
       (student_2020 %>% filter(!is.na(reading)))$reading)

t.test((student_2019 %>% filter(!is.na(science)))$science, 
       (student_2020 %>% filter(!is.na(science)))$science)
