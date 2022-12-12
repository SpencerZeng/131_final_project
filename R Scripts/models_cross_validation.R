brfss_recipe <- recipe(heart_disease ~., data = brfss_train) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors()) 

brfss_folds <- vfold_cv(brfss_train, v = 5)


# ENN
elastic_net_spec <- multinom_reg(penalty = tune(), 
                                 mixture = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet")

en_workflow <- workflow() %>% 
  add_recipe(brfss_recipe) %>% 
  add_model(elastic_net_spec)

en_grid <- grid_regular(penalty(range = c(-5, 5)), 
                        mixture(range = c(0, 1)), levels = 5)

tune_res_en <- tune_grid(
  en_workflow,
  resamples = brfss_folds, 
  grid = en_grid
)

best_model_en <- select_best(tune_res_en, metric = "roc_auc")

en_final <- finalize_workflow(en_workflow, best_model_en)

en_final_fit <- fit(en_final, data = brfss_train)

# KNN

knn_spec <- 
  nearest_neighbor(neighbors = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kknn")

knn_workflow <- workflow() %>% 
  add_model(knn_spec) %>% 
  add_recipe(brfss_recipe)

knn_grid <- grid_regular(neighbors(range = c(1, 10)), levels = 5)

tune_res_knn <- tune_grid(
  knn_workflow,
  resamples = brfss_folds, 
  grid = knn_grid
)

best_model_knn <- select_best(tune_res_knn, metric = "roc_auc")

knn_final <- finalize_workflow(knn_workflow, best_model_knn)

knn_final_fit <- fit(knn_final, data = brfss_train)

# random forest

rf_spec<-rand_forest() %>%
  set_engine("ranger",importance="impurity")%>%
  set_mode("classification")

rf_wf<-workflow()%>%
  add_model(rf_spec %>% set_args(mtry=tune(),trees=tune(),min_n=tune()))%>%
  add_formula(heart_disease~.)

rf_grid <-grid_regular(
  mtry(range= c(1,17)),
  trees(range = c(50,200)),
  min_n(range = c(10,30)),
  levels = 5)

tune_res_rf <-tune_grid(
  rf_wf,
  resample = brfss_folds,
  grid = rf_grid,
  metrics = metric_set(roc_auc)
)

best_model_rf <- select_best(tune_res_rf, metric = "roc_auc")

rf_final <- finalize_workflow(rf_wf, best_model_rf)

rf_final_fit <- fit(rf_final, data = brfss_train)

# boosted trees


boost_spec<-boost_tree()%>%
  set_engine("xgboost")%>%
  set_mode("classification")

boost_wf<-workflow()%>%
  add_model(boost_spec %>% set_args(
    learn_rate = tune(),
    trees = tune())) %>%
  add_formula(heart_disease~.)

boost_grid <- grid_regular(
  learn_rate(range = c(-2,0.5)),
  trees(range = c(10,1000)),
  levels = 5)

tune_res_boost <-tune_grid(
  boost_wf,
  resample = brfss_folds,
  grid = boost_grid,
  metrics = metric_set(roc_auc)
)
best_model_boost <- select_best(tune_res_boost, metric = "roc_auc")

boost_final <- finalize_workflow(boost_wf, best_model_boost)

boost_final_fit <- fit(boost_final, data = brfss_train)
