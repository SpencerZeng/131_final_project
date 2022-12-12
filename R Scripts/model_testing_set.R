knn_wf_tuned <- knn_workflow %>% 
  finalize_workflow(select_best(tune_res_boost, metric = "roc_auc"))
best_fit_boost <- fit(boost_wf_tuned, brfss_test)

options(yardstick.event_first = FALSE)

# roc curve 
augment(best_fit_knn, new_data = brfss_test) %>%
  roc_curve(heart_disease, .pred_1) %>%
  autoplot() 
# roc_auc
options(yardstick.event_first = FALSE)
augment(best_fit_knn, new_data = brfss_test) %>%
  roc_auc(heart_disease, estimate = .pred_1) %>%
  select(.estimate) 
# confusion matrix
options(yardstick.event_first = FALSE)
augment(best_fit_knn, brfss_test) %>%
  conf_mat(truth = heart_disease, estimate = .pred_class) %>%
  autoplot(type="heatmap")