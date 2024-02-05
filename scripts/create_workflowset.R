

#Remove models we're not interested in running
model_list<-model_list[names(model_list) %in% params$models_to_run]

#Remove recipes were not interested in
preproc_list<-preproc_list[names(preproc_list) %in% params$recipes_to_run]

# This is where we define what's a part of the workflow set
df_wf_set <-
  workflow_set( # NOTE - ensure that preproc does not contain underscore (for later plotting)
    preproc =preproc_list,
    models = model_list,
    cross = T
  )