# Split data into training and test set --------------------
set.seed(123)

df_split <-
  if(is.null(params$group_split_var)){
    print("Non-Group Split")
    initial_split(df,prop = params$split_pct, strata = params$strata_var)
  }else if (is.null(params$strata_var)){
    print("Group Split, Non Stratified")
    group_initial_split (df,prop = params$split_pct, group = params$group_split_var)
  }else{
    print("Group Split, Stratified")
    group_initial_split (df,prop = params$split_pct, group = params$group_split_var, strata = params$strata_var)
  }

df_train <- training(df_split)
df_test <- testing(df_split)
