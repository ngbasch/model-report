registerDoParallel(cores = parallel::detectCores(logical = FALSE))

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = FALSE
  )


#Run workflow set through cross validation -----------------------
tuned_workflows<-workflow_map(
  df_wf_set%>%
    option_add(control = control_stack_grid()),
  "tune_grid",
  metrics = metric_set(yardstick::rmse, yardstick::mape, yardstick::mpe, yardstick::rsq),
  resamples = df_folds,
  control = grid_ctrl,
  # Number of grids only relevant for tuning
  grid = 10,
  verbose = T)
