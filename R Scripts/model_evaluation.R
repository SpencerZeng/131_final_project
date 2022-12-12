en_acc <- augment(en_final_fit, new_data = brfss_train) %>%
  accuracy(truth = heart_disease, estimate = .pred_class)

knn_acc <- augment(knn_final_fit, new_data = brfss_train) %>%
  accuracy(truth = heart_disease, estimate = .pred_class)

rf_acc <- augment(rf_final_fit, new_data = brfss_train) %>%
  accuracy(truth = heart_disease, estimate = .pred_class)

boost_acc <- augment(boost_final_fit, new_data = brfss_train) %>%
  accuracy(truth = heart_disease, estimate = .pred_class)


accuracies <- c(en_acc$.estimate, knn_acc$.estimate, 
                rf_acc$.estimate, boost_acc$.estimate)
models <- c("elastic net", "knn", "random forest", "boosted tree")

results <- tibble(accuracies = accuracies, models = models)
results %>% 
  arrange(-accuracies)