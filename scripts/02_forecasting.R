# Setup ----
## Clear & set ----
rm(list=ls());gc();cat("\f")
memory.limit(999999)

## Load Libraries ----
# install.packages("prophet")
library(prophet)
library(tidyverse)    # Wider functionality 
library(tidymodels)   # Modelling
library(data.table)   # Fast manipulation and reading
library(here)         # Directory rooting
library(fst)          # Fast file read & write

## Load Data ----
working <- read_fst(here("data", "working.fst"), as.data.table = T)

# Forecasting ----
day_mins <- 24 * 60
week_mins = day_mins * 7
month_mins <- day_mins * 30

# Documentation in https://facebook.github.io/prophet/
wide <- dcast(working, time ~ Asset_Name, value.var = "VWAP")[order(time)]

# Start off just bitcoin
btc <- wide[, .(time, Bitcoin)]

# We don't need long term predictions, just 15 mins!!!! Long term doesn't matter
# Taking just 1 week of data
rows <- nrow(btc)
week <- btc[(rows-week_mins):rows, ]

## Prophet ----
prophet_test <- wide[, .(ds = time, y = Bitcoin)][(nrow(wide)-9999):nrow(wide)]

m <- prophet(prophet_test)

future <- make_future_dataframe(m, 2000, freq = 60)

forecast <- predict(m, future)

rows <- nrow(forecast)
small <- forecast[(rows-10100):rows, ]
small$actual <- prophet_test$y[(rows-10100):rows]

ggplot(small, aes(ds, actual)) +
  geom_point(color = "blue", size = 0.5) +
  geom_line(aes(ds, yhat)) +
  geom_line(aes(ds, yhat_upper)) +
  geom_line(aes(ds, yhat_lower)) +
  theme_classic()

plot(small)

