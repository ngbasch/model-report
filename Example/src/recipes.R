
FeatureEngineering<- function(rec){
  rec%>%
    step_mutate(across(where(is.logical), as.numeric)) %>%
    # Remove categorical variables that just have one value
    step_zv(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors())%>%
    #Remove values with zero variance (basically constants)
    step_zv(all_numeric(), -all_outcomes())
}


df_rec <-recipe(
  Formulate(y = outcome_var, x= predict_vars),
  data = df_train
)%>%
  FeatureEngineering()


df_rec_width <-recipe(
  Formulate(y = outcome_var, x= c("Sepal.Width","Petal.Width")),
  data = df_train
)%>%
  FeatureEngineering()


preproc_list<-list("main" = df_rec,
                   "mainwidth" = df_rec_width)
