# Split training set into k-fold cross validation for evaluation later
df_folds<- 
  if(is.null(params$group_split_var)){
    print("Non-Group CV")
    vfold_cv(df,v = params$n_folds, strata = params$strata_var)
  } else if (is.null(params$strata_var)){
    print("Group CV, Non Stratified")
    group_vfold_cv(df,prop = params$split_pct, group = params$group_split_var,  v = params$n_folds)
  } else{
    print("Group CV, Stratified")
    group_vfold_cv(df,prop = params$split_pct, group = params$group_split_var,  v = params$n_folds, strata = params$strata_var)
  }
