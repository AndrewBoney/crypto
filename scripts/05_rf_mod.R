
# Model ----
## Split Data ----
split <- initial_split(pca_data, prop = 0.8)

train <- training(split)
test <- testing(split)

## Build Model ----
set.seed(1)
rec <- recipe(Bitcoin ~ ., train) %>% 
  update_role(time, new_role = "id")

set.seed(2)
rf <- rand_forest(trees = 200) %>% 
  set_engine("ranger", num.threads = parallel::detectCores()-1) %>% 
  set_mode("regression")

wf <- workflow() %>%
  add_recipe(rec) %>% 
  add_model(rf)

tic("Fit RF")
fit_rf <- wf %>% 
  fit(train)
toc()

