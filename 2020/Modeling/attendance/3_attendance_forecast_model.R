# Model to forecast attendance each day

library(tidyverse)
library(ggplot2)
library(stats)
library(zoo)
library(tidyquant)
library(timetk)
library(sweep)
library(tseries)
library(forecast)
library(xts)

# ============= Read in daily attendance from 2012-2019 ================
daily_attendance <- read_csv("N:/ORP_accountability/projects/Andrew/acct-am/2020/Modeling/attendance/data/daily_attendance_2012-19.csv")


daily_attendance %>% 
  filter(system == 10, school == 20) %>% 
  ggplot(aes(x = id_date, y = attendance_rate)) +
  geom_line()

daily_attendance %>% stl(s.window='periodic') %>% seasadj() -> eeadj
autoplot(eeadj)

daily_att_nest <- daily_attendance %>% 
  filter(id_date >= as.Date('2012-08-15'),
         id_date <= as.Date('2020-03-02')) %>% 
  select(system, school, id_date, attendance_rate) %>% 
  group_by(system, school) %>% 
  nest()

daily_att_nest_ts <- daily_att_nest %>%
  mutate(data_ts = map(.x       = data, 
                       .f       = tk_ts, 
                       select   = -id_date, 
                       start    = as.Date('2012-08-15'),
                       freq     = 5))



test_ts <- daily_att_nest_ts[[1,4]]

fit_basic1<- auto.arima(test_ts)

futurVal <- forecast(fit_basic1,h=30, level=c(99.5))
plot(futurVal)

acf(test_ts)
pacf(test_ts)

# =============== testing functions ====================
anderson_high <- daily_attendance %>% 
  filter(system == 10, school == 2, id_date <= as.Date('2020-03-02')) %>% 
  select(id_date, attendance_rate)

z <- read.zoo(anderson_high)
fit <- auto.arima(z)
fore <- forecast(fit, h = 150)
fore_df <- as_tibble(fore, rownames = 'epoch')
fore_df$epoch <- as.POSIXct(as.numeric(fore_df$epoch), origin = '1970-01-01')

ts_and <- as.ts(z)

xts(df[,-1])









