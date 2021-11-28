# Setup ----
## Clear & set ----
rm(list=ls());gc();cat("\f")
memory.limit(999999)

## Load Libraries ----
library(tidyverse)    # Wider functionality 
library(tidymodels)   # Modeling
library(data.table)   # Fast manipulation and reading
library(fst)          # Fast file read & write
library(here)         # Directory rooting

## Load Data ----
train <- fread(here("data", "train.csv"))
asset_details <- fread(here("data", "asset_details.csv"))

# Adjust Working ----
# "Working" file, for on the go changes to train.csv 
working <- copy(train)
# Going to clear train, cos I have barely any RAM on my personal laptop
rm(train)
working[, na_target := is.na(Target)]

day_mins <- 24 * 60
week_mins = day_mins * 7
month_mins <- day_mins * 30

# Converting timestamp column to datetime
working[, time := as.POSIXct(timestamp, origin="1970-01-01")][, date := as.Date(time)]

# Merging on Asset Name
working <- merge(working, asset_details, by = "Asset_ID")

# Check Target NAs ----
# Charting NAs over time
na_time <- working[, .N, keyby = .(date, na_target)]

ggplot(na_time, aes(date, N, color = na_target)) +
  geom_point() +
  theme_classic()

# No big time biases

# Asset ID
working[, .N, keyby = .(na_target, Asset_Name)] %>% 
  .[, pct := N / sum(N), .(Asset_Name)] %>% 
  .[order(Asset_Name, na_target)] %>% 
  .[na_target == T] %>% 
  ggplot(aes(Asset_Name, pct, fill = Asset_Name)) +
    geom_col() +
    theme_classic() + 
    theme(legend.position = "none") +
    scale_y_continuous(labels = scales::percent) +
    coord_flip() 
# Some have more missing, some less

# NAs over time ----
na_time_asset <- working[, .N, keyby = .(date, na_target, Asset_Name)] %>% 
  .[na_target == T, ]

ggplot(na_time_asset, aes(date, N, color = Asset_Name)) +
  geom_point() +
  theme_classic()


# Checking in facet boxplot other columns
cols <- colnames(working)
working[, by = "na_target",
                  lapply(.SD, mean, na.rm = T),
                  .SDcols = setdiff(cols, c("timestamp", "time", "date", 
                                            "Asset_ID", "Target","na_target"))]

# Plotting prices (Close) over time
# Taking daily close or too cluttered
avg_daily_close <- working[, .(avg_close = mean(Close)), 
                           by = .(Asset_Name, date)] 

avg_daily_close[, rel_close := (avg_close / mean(avg_close)), 
                  by = .(Asset_Name)]

ggplot(avg_daily_close, aes(date, avg_close, color = Asset_Name)) +
  geom_point() +
  theme_classic()

# Take relative prices, and filter to top 5 (in terms of weight)
top_5 <- asset_details$Asset_Name[match(sort(asset_details$Weight, 
                                    decreasing = T)[1:5], 
                               asset_details$Weight)]

avg_daily_close[Asset_Name %in% top_5] %>% 
  ggplot(aes(date, rel_close, color = Asset_Name)) +
    geom_point() +
    geom_smooth() +
    theme_classic()
  
# Getting wide data by asset, for correlation
wide_close <- dcast(working, time ~ Asset_Name, value.var = "Close")
wide_target <- dcast(working, time ~ Asset_Name, value.var = "Target")
# wide_target[, lapply(.SD, function(x) round(x, 5)), .SDcols = c(colnames(wide_target)[colnames(wide_target) != "time"])]

# Getting Close correlations
# As not all coins existed for the whole period, excluding some...
cors <- cor(na.omit(wide_close[,-1]))

# At a later date it could be worth testing, if there is a benefit to 
# limiting to just data where all coins exist, or if it makes somehow
# build composite predictions

# Getting Log Returns ----
# Trying to emulate log return function detailed in...
# https://www.kaggle.com/cstein06/tutorial-to-the-g-research-crypto-competition
logs <- log(wide_close$Bitcoin)
log(logs[16]) - log(logs[2])

log_return <- function(series, nperiods) {
  returns <- log(series) - lead(log(series), nperiods)
  return(returns)
}
logs <- log_return(wide_close$Bitcoin, 15)
logs[length(logs)]

wide_close$Bitcoin[nrow(wide_close)]

# Time Alligned Cor (self) ----
# Building cors with previous minutes, up to (1 month)?
cor_fun <- function(x, col, lag) {
  x_lag <- na.omit(data.table(x = x[[col]], 
                      x_lag = lag(x[[col]], lag)))
  cor <- cor(x_lag$x, x_lag$x_lag)
  return(cor)
}

return_cor_table <- function(x, col, lag) {
  table <- data.frame(lag = lag,
                      col = col,
                      cor = cor_fun(x, col, lag))
  return(table)
}

nested <- working %>% 
  as_tibble() %>% 
  arrange(time) %>% 
  group_by(Asset_Name) %>%
  nest() %>% 
  ungroup()

nested <- nested %>% 
  mutate(cor_10 = map(data, ~cor_fun(.x, "Close", 10)),
         cor_100 = map(data, ~cor_fun(.x, "Close", 100)),
         cor_1000 = map(data, ~cor_fun(.x, "Close", 1000)))

library(tictoc)
all_cors <- NULL
for (i in seq(from = 0, to = month_mins, by = 200)) {
  tic(paste0("Lag ", i))
  cor <- map_dfr(nested$data, ~return_cor_table(.x, "Close", i)) %>% mutate(asset = nested$Asset_Name)
  all_cors <- bind_rows(all_cors, cor)
  toc(log = T)
}

all_cors %>% 
  filter(asset %in% top_5) %>% 
  ggplot(aes(lag, cor, color = asset)) +
    geom_line() +
    theme_classic()

nested$cor_10  
nested$cor_100
nested$cor_1000



# Save Files ----
write_fst(working, here("data", "working", "working.fst"), compress = 100)
write_fst(wide_close, here("data", "working", "wide_close.fst"), compress = 100)
write_fst(wide_target, here("data", "working", "wide_target.fst"), compress = 100)


