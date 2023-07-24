
rf_spec<-
  rand_forest(trees = 1000,
              #The best mtry from tuning appeared to be 15
              mtry = 20,
              min_n = 2,
              mode = "regression")%>%
  set_engine("ranger",
             keep.inbag = F,
             importance = "impurity",
             seed = 700,
             replace = T)

rf_spec

rf_spec_randomForest<-
  rand_forest(trees = 1000,
              #The best mtry from tuning appeared to be 15
              mtry = 10,
              mode = "regression")%>%
  set_engine("randomForest")

rf_spec_randomForest


xgb_spec <-
  boost_tree( tree_depth = 5,
              trees = 400,
              mode = "regression"
  ) %>%
  set_engine("xgboost")

xgb_spec


lm_spec <-
  linear_reg()

lm_spec

knn_spec <-
  nearest_neighbor(neighbors = 8, weight_func = "triangular") %>%
  set_engine("kknn") %>%
  set_mode("regression")

knn_spec

nnet_spec<-
  mlp()%>%
  set_mode("regression")%>%
  set_engine("nnet")

nnet_spec

cubist_spec<-
  cubist_rules(
    mode = "regression",
    committees = NULL,
    neighbors = NULL,
    max_rules = NULL,
    engine = "Cubist"
  )

bart_spec<-
  parsnip::bart(trees = 236,
                prior_terminal_node_coef =  0.953,
                prior_terminal_node_expo = 0.788
  ) %>%
  set_engine("dbarts") %>%
  set_mode("regression")
bart_spec

lasso_spec <- linear_reg(penalty = 0.1, mixture = 1)%>%
  set_engine("glmnet")
lasso_spec

stan_spec <-
  linear_reg() %>%
  set_engine("stan")
stan_spec



# Combine all specs into a list
model_list <-list("xgb" = xgb_spec,
                  "rf"= rf_spec,
                  "rfrandomForest" = rf_spec_randomForest,
                  "knn" = knn_spec,
                  "lm" = lm_spec,
                  "nnet" = nnet_spec,
                  "cubist" = cubist_spec,
                  "bart" = bart_spec,
                  "lasso" = lasso_spec,
                  "stan" = stan_spec)