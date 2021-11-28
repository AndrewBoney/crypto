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

## Load Data ----
wide_target <- read_fst(here("data", "working", "wide_target.fst"))
wide_close <- read_fst(here("data", "working", "wide_close.fst"))

# Model ----
## Linear Regression ----
# Building linear reg model with lag effect
# First, get df wth lag columns
log_return <- function(series, nperiods) {
  ps <- nperiods
  if (nperiods > 0) returns <- log(series/ lead(series, ps))
  else returns <- log(series/ lag(series, abs(ps)))
  return(returns)
}

# Doesn't quite match for some reason, but very close. 
check <- log_return(wide_close$Bitcoin, 15)
comparison <- bind_cols(wide_target, check = check)

# lag_return_df <- wide_close %>%
#   mutate(across(-time, ~log_return(.x, -15), .names = "{.col}_prev_return"))

# Getting intervals of 5
coins <- colnames(wide_close)[-1]
lag_return_df <- wide_close %>% 
  mutate(across(all_of(coins), ~log_return(.x, 5), .names = paste0("{.col}_", 5)),
         across(all_of(coins), ~log_return(lag(.x, 5), 10), .names = paste0("{.col}_", 10)),
         across(all_of(coins), ~log_return(lag(.x, 10), 15), .names = paste0("{.col}_", 15)))
  
# Build a lagged target variable, for dummy prediction
lag_return_df <- lag_return_df %>% 
  mutate(lag_target = lag(Bitcoin_5, 1))

# Initial model we'll just build on bitcoin
# Also remove NA rows.
btc <- lag_return_df %>% 
  select(time, ends_with("_5"), ends_with("_10"), ends_with("_15")) %>%
  left_join(wide_target %>% select(time, Bitcoin), by = "time") %>% 
  filter(if_all(everything(), ~!is.na(.x)))

# Models ----
## Split Data ----
split <- initial_split(btc, prop = 0.8)

train <- training(split)
test <- testing(split)

## Dummy Model ----
# Naive model to compare against. Take lag return
preds_dummy <- test %>%
  left_join(lag_return_df %>% select(time, .pred = lag_target), by = "time") %>% 
  mutate(resid = abs(Bitcoin - .pred), 
         direction_pred = as.factor(if_else(.pred > 0, "up", "down")),
         direction_actual = as.factor(if_else(Bitcoin > 0, "up", "down")))
  
metrics(preds_dummy, Bitcoin, .pred)
table(preds_dummy %>% select(direction_pred, direction_actual))
metrics(preds_dummy, direction_pred, direction_actual)

## LM with tidymodels ----
lm <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

rec <- recipe(Bitcoin ~ ., train) %>% 
  update_role(time, new_role = "id")

wf_lm <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(lm)

fit <- wf_lm %>% 
  fit(train)

tidy <- fit %>% 
  tidy() %>% 
  mutate(p_round = round(p.value, 4))

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

preds_lm <- get_pred_table(test, fit)
table(preds_lm %>% select(direction_pred, direction_actual))

metrics(preds_lm, Bitcoin, .pred)
metrics(preds_lm, direction_pred, direction_actual)

ggplot(preds_lm, aes(Bitcoin, .pred)) +
  geom_point() +
  theme_classic()

## Lasso Reg ----
set.seed(2)
fold <- vfold_cv(train)

tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

rec.lm <- rec %>% 
  step_normalize(all_numeric_predictors()) 

wf.ls.tune <- workflow() %>%
  add_recipe(rec.lm) %>% 
  add_model(tune_spec)

lambda_grid <- grid_regular(penalty(), levels = 50)

# doParallel::registerDoParallel()

set.seed(3)
tic("Train Lasso Grid")
lasso_grid <- tune_grid(
  wf.ls.tune,
  resamples = fold,
  grid = lambda_grid,
  control = control_grid(verbose = T)
)
toc()

lasso_grid %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")

lasso_grid %>% 
  select_best("rmse")

lowest_rmse <- lasso_grid %>%
  select_best("rmse")

final_lasso <- finalize_workflow(
  wf.ls.tune,
  lowest_rmse
)

fit.lasso <- final_lasso %>% 
  fit(train)

preds_lm <- get_pred_table(test, fit.lasso)
table(preds_lm %>% select(direction_pred, direction_actual))

metrics(preds_lm, Bitcoin, .pred)
metrics(preds_lm, direction_pred, direction_actual)

## RF ----
### No Dim Red ----
# Default hyperparameters for now
set.seed(123)
rf <- rand_forest(trees = 200) %>% 
  set_engine("ranger", num.threads = parallel::detectCores()-1) %>% 
  set_mode("regression")

wf_rf <- workflow() %>%
  add_recipe(rec) %>% 
  add_model(rf)

tic("Fit RF")
fit_rf <- wf_rf %>% 
  fit(train)
toc()

tic("Pred RF")
pred_rf <- get_pred_table(test, fit_rf)
toc()

rmse(pred_rf, Bitcoin, .pred)
sd(pred_rf$Bitcoin)

table(pred_rf$direction_pred, pred_rf$direction_actual)

# Checking distributions
ggplot(pred_rf, aes(.pred)) + 
  geom_histogram(bins = 200) +
  scale_y_continuous(limits = c(0, 40000)) +
  scale_x_continuous(limits = c(-0.02, 0.02)) +
  theme_classic()

ggplot(pred_rf, aes(Bitcoin)) +
  geom_histogram(bins = 200) +
  scale_y_continuous(limits = c(0, 40000)) +
  scale_x_continuous(limits = c(-0.02, 0.02)) +
  theme_classic()

# Preds have smaller range

### PCA ----
set.seed(4) 
pca_in <- train %>% select(-c(time, Bitcoin))
pca_obj <- prcomp(pca_in)
summary(pca_obj)

exp <- tibble(pc = paste0("PC", 1:ncol(pca_in)),
              pc_num = 1:ncol(pca_in),
              var_exp = pca_obj$sdev) %>% 
  mutate(var_exp_pct = var_exp / sum(var_exp),
         cum_pct = cumsum(var_exp_pct))

ggplot(exp, aes(pc_num, var_exp_pct)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  theme_classic()

rec.pca <- rec %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), threshold = 0.90)

data <- prep(rec.pca, train) %>% 
  bake(train)

wf_rf.pca <- workflow() %>%
  add_recipe(rec.pca) %>% 
  add_model(rf)

tic("Fit RF")
fit_rf.pca <- wf_rf.pca %>% 
  fit(train)
toc()

beepr::beep("fanfare")

tic("Pred rf.pca")
pred_rf.pca <- get_pred_table(test, fit_rf.pca)
toc()

get_metrics <- function(pred_table) {
  out <- vector(mode = "list", length = 3)
  names(out) <- c("reg_metrics", "class_metrics", "table")
  
  out[["reg_metrics"]] <- metrics(pred_table, Bitcoin, .pred)
  out[["class_metrics"]] <- metrics(pred_table, direction_pred, direction_actual)
  out[["table"]] <- table(pred_table %>% select(direction_pred, direction_actual))
  
  return(out)
}

get_metrics(pred_rf.pca)

# With pca???

## Prophet ----
# For a bit of a different approach, I'm going to run a forecast
# using the prophet package, i.e. approach as a time series model
# rather than a regression problem

# I'm expecting regression to be better, but I think this is worth trying as well

# This is a bit hard to do exactly as test / train doesn't work properly
# I think what I'm going to do is train on sets of 20000 rows (-15 for predictions)

# Splitting into set and remove last group, which won't have the same amount of data
library(prophet)

prophet_set <- setDT(wide_target)[, set := ceiling(.I / 20000)] %>% 
  .[set != max(set)]

make_prophet_pred <- function(data, set) {
  set_filt <- data[set == !!set, .(ds = time, y = Bitcoin)]
  index <- (nrow(set_filt)-14):nrow(set_filt)
  
  train <- set_filt[!index]
  test <- set_filt[index]
  
  tic("Train Model took... ")
  p <- prophet(train, yearly.seasonality = F, weekly.seasonality = F)
  toc()
  
  future <- make_future_dataframe(p, 15, freq = 60)
  
  nrow(future)
  
  tic("Predictions took... ")
  pred <- predict(p, future) %>% 
    mutate(actual = c(train$y, test$y), 
           test = c(rep(F, nrow(train)), rep(T, nrow(test))))
  toc()
  
  return(pred)
}

pred_data <- make_prophet_pred(prophet_set, 1)

pred_data[, c("ds", "actual", "test", "yhat", "yhat_upper", "yhat_lower")] %>%
  pivot_longer(cols = c(actual, yhat), 
               names_to = "var",
               values_to = "value") %>% 
  ggplot(aes(ds, value, color = var)) +
    geom_point() +
    geom_line(aes(ds, yhat_upper), linetype = "dashed") +
    geom_line(aes(ds, yhat_lower), linetype = "dashed") +
    theme_classic()

# This is bad. Don't need to look into detail but predictions are rubbish.
# Makes sense - I think forecasting like this is more for long term.


