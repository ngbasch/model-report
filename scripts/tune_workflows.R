fits<-  lapply(df_wf_set$wflow_id,
               TuneExtractModel,
               wf =if(params$n_folds > 1){tuned_workflows}else{df_wf_set},
               is_tuned = params$n_folds > 1,
               split_data = df_split,
               # If no error colours were set, just use the outcome variable
               error_colours = ifelse(sum(is.null(params$error_colours)) > 0, outcome_var,params$error_colours),
               explain_data_input = params$explain_data)

names(fits) <- df_wf_set$wflow_id
