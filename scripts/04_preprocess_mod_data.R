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
library(tictoc)       # Timing code steps

get_pred_table <- function(new_data, model)  {
  outcome <- model$pre$actions$recipe$recipe$var_info  %>% 
    filter(role == "outcome") %>% 
    .$variable
  
  pred <- bind_cols(
    new_data,
    predict(model, new_data = new_data)
  ) %>% 
    mutate(resid = abs(!!sym(outcome) - .pred), 
           direction_pred = as.factor(if_else(.pred > 0, "up", "down")),
           direction_actual = as.factor(if_else(Bitcoin > 0, "up", "down")))
  
  return(pred)
}

get_metrics <- function(pred_table) {
  out <- vector(mode = "list", length = 3)
  names(out) <- c("reg_metrics", "class_metrics", "table")
  
  out[["reg_metrics"]] <- metrics(pred_table, Bitcoin, .pred)
  out[["class_metrics"]] <- metrics(pred_table, direction_pred, direction_actual)
  out[["table"]] <- table(pred_table %>% select(direction_pred, direction_actual))
  
  return(out)
}

## Load Data ----
wide_target <- read_fst(here("data", "working", "wide_target.fst"), as.data.table = T)
wide_close <- read_fst(here("data", "working", "wide_close.fst"), as.data.table = T)

# Create ----
# Function for getting log return
# TODO: see previous script, this isn't quite matching
log_return <- function(series, nperiods) {
  ps <- nperiods
  if (nperiods > 0) returns <- log(series/ lead(series, ps))
  else returns <- log(series/ lag(series, abs(ps)))
  return(returns)
}

# Build 1 min intervals, x 60
coins <- colnames(wide_close)[-1]

after <- wide_close %>% 
  pivot_longer(-time) %>% 
  filter(!is.na(value)) %>% 
  group_by(name) %>% 
  summarise(min_time = min(time))
  
working <- wide_close %>% 
  filter(time > !!max(after$min_time))

tic("All intervals")
for (i in 1:60) {
  tic(paste0("Interval ", i))
  working <- working %>% 
    mutate(across(all_of(coins), ~log_return(lag(.x, i-1), i), .names = paste0("{.col}_", i)))
  gc()
  toc()
}
toc()

# TODO: Not ran
tic("Remove NAs")
data_in <- setDT(working)[, !..coins] %>% na.omit()
toc()

rm(working)

## PCA ----
tic("PCA")
pca <- princomp(data_in %>% select(-time))
toc()


pca_metrics <- tibble(sdev = pca$sdev,
                      name = names(pca$sdev),
                      num = 1:length(pca$sdev)) %>% 
  mutate(pct = sdev / sum(sdev),
         cum_pct = cumsum(pct))


pca_data <- bind_cols(data_in %>% select(time), as_tibble(pca$scores[, 1:200])) %>% 
  left_join(wide_target %>% select(time, Bitcoin), by = "time") 

pca_data <- pca_data %>% 
  filter(!is.na(Bitcoin))

rm(list = c(setdiff(ls(), "pca_data")))

# Save ----
write_fst(pca_data, here("data", "working", "pca_data.fst"), compress = 100)
fwrite(pca_data, here("data", "working", "pca_data.csv"))

