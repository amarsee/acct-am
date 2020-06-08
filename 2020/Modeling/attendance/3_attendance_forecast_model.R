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

daily_attendance_w_features <- daily_attendance %>% 
  group_by(school_year, system, school) %>% 
  mutate(
    school_day = row_number(),
    att_rate_previous_day = data.table::shift(attendance_rate),
    att_rate_5_day = round(data.table::frollmean(attendance_rate, n = 5, fill = NA), 1),
    att_rate_10_day = round(data.table::frollmean(attendance_rate, n = 10, fill = NA), 1),
    att_rate_20_day = round(data.table::frollmean(attendance_rate, n = 20, fill = NA), 1),
    att_rate_previous_day = if_else(
      is.na(att_rate_previous_day),
      attendance_rate,
      att_rate_previous_day
    ),
    att_rate_5_day = if_else(
      is.na(att_rate_5_day),
      round(cumsum(attendance_rate)/school_day, 1),
      att_rate_5_day
    ),
    att_rate_10_day = if_else(
      is.na(att_rate_10_day),
      round(cumsum(attendance_rate)/school_day, 1),
      att_rate_10_day
    ),
    att_rate_20_day = if_else(
      is.na(att_rate_20_day),
      round(cumsum(attendance_rate)/school_day, 1),
      att_rate_20_day
    )
  ) %>% 
  ungroup() %>% 
  mutate(
    day_of_week = weekdays(id_date),
    cal_month = months(id_date)
  )

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
anderson_high <- daily_attendance_w_features %>% 
  filter(system == 10, school == 2, id_date <= as.Date('2020-03-02')) %>% 
  select(id_date, attendance_rate)

z <- read.zoo(anderson_high)
fit <- auto.arima(z)
fore <- forecast(fit, h = 150)
fore_df <- as_tibble(fore, rownames = 'epoch')
fore_df$epoch <- as.POSIXct(as.numeric(fore_df$epoch), origin = '1970-01-01')

ts_and <- as.ts(z)

xts(df[,-1])




# ==== Rolling averages, day of week, school day number? ====
library(randomForest)
library(rsample)
library(caret)
set.seed(123)

att_model <- function(df) {
  caret::train(attendance_rate ~ .,
        data = df,
        method = "ranger")
}

anderson_high <- daily_attendance_w_features %>% 
  filter(system == 10, school == 2, id_date <= as.Date('2020-03-02')) %>% 
  mutate_at(
    .vars = c('day_of_week', 'cal_month'),
    .funs = ~ as.factor(.)
  ) %>% 
  select(attendance_rate:cal_month)

and_split <- initial_split(anderson_high, prop = .7)
and_train <- training(and_split)
and_test  <- testing(and_split)

rf_and <- randomForest(
  formula = attendance_rate ~ .,
  data    = and_train
)

rf_and

varImpPlot(rf_and)

test_prediction <- predict(rf_and, and_test[,-1])

model <- train(attendance_rate ~ .,
               data = and_train,
               method = "ranger")
print(model)

preds <- predict(model, and_test[,-1])

test_w_preds <- bind_cols(and_test, tibble(predicted = preds))

myGrid <- expand.grid(mtry = 1:19,
                      splitrule = c("variance", "extratrees"),
                      min.node.size = 5) ## Minimal node size; default 1 for classification
model <- train(attendance_rate ~ .,
               data = and_train,
               method = "ranger",
               tuneGrid = myGrid,
               trControl = trainControl(method = "cv",
                                        number = 5,
                                        verboseIter = FALSE))
print(model) 
# The final values used for the model were mtry = 8, splitrule = extratrees and min.node.size = 5.

p <- predict(model, anderson_high)
error <- p - anderson_high$attendance_rate
rmse_xval <- mean(abs(error/anderson_high$attendance_rate)*100) ## xval mape
rmse_xval

daily_att_rf_nest <- daily_attendance_w_features %>% 
  filter(id_date <= as.Date('2020-03-02')) %>% 
  select(system, school, attendance_rate:cal_month) %>% 
  group_by(system, school) %>% 
  nest() 
  
rf_nested_models <- daily_att_rf_nest %>%
  filter(system == 10) %>% 
  mutate(fit_rf = map(.x  = data, 
                       .f = att_model))




