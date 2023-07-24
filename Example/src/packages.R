# Load functions and other packages
# source("../../Kansas_HRWW/Libraries/load_packages.R")
# source("../../Kansas_HRWW/Libraries/helper_functions.R")
# source("../shared_helper_functions.R")

library(tidymodels)
library(tidyverse)
library(visdat)
library(corrplot)
library(ggridges)
library(hues)
library(ggplotify)
library(gridExtra)
library(here)
library(stacks)
library(DALEXtra)
# Install Keras and Tensor flow
# https://stackoverflow.com/questions/44611325/r-keras-package-error-python-module-tensorflow-contrib-keras-python-keras-was-n
library(keras)
library(tensorflow)
library(finetune)
library(workboots)
library(Cubist)
library(rules)
library(dbarts)
library(reactable)
library(plotly)
library(patchwork)
# https://juba.github.io/rmdformats/
library(rmdformats)
library(readr)
library(janitor)
library(lightgbm)
library(bonsai)
library(vip)
library(philentropy)
library(themis)

# Custom functions

FormatReactable<- function(df,filt = T){
  reactable(df,
            outlined = TRUE,
            highlight = TRUE,
            striped = TRUE,
            showSortable = TRUE,
            compact = FALSE,
            bordered = TRUE,
            defaultExpanded = F,
            filterable = filt,
            showPageSizeOptions = TRUE,
            defaultColDef = colDef(
              format = colFormat(separators = TRUE, digits = 2)))
}

GenerateErrorBars <- function(df, c){
  
  scale<-
    if (class(df[[c]]) == "numeric"){
      scale_colour_distiller(type = "div")
    } else{
      scale_colour_iwanthue()
    }
  
  gg<-
    df%>%
    ggplot(aes_string(x = "id_hash", y = "error", colour = c))+
    geom_point(alpha = 0.5)+
    geom_segment(aes(yend=0,xend=id_hash))+
    geom_abline(slope = 0, colour = "red")+
    facet_wrap(~model, nrow = 1, scales = "free_x")+
    theme(axis.text.x=element_blank()) +
    scale+
    labs(x = "Observation")
  
  return(gg)
  
}



PlotNumericColumns<-function(df, fill_column = NULL){
  
  if (is.null(fill_column)){
    df%>%
      as_tibble()%>%
      # dplyr::select(pilot_emissions_predict_vars, re_predicted_emissions_w_adjustments, error, dataset)%>%
      select_if(function(col) is.numeric(col)
      )%>%
      gather(var, val)%>%
      as_tibble()%>%
      ggplot(aes_string(x = "val"))+
      geom_histogram(alpha = 0.5, position="identity")+
      facet_wrap(~var, scales = "free")+
      theme_bw()
    
  } else{
    df%>%
      as_tibble()%>%
      # dplyr::select(pilot_emissions_predict_vars, re_predicted_emissions_w_adjustments, error, dataset)%>%
      select_if(function(col) is.numeric(col) |
                  all(col %in% .[[fill_column]])
      )%>%
      gather(var, val,!fill_column)%>%
      as_tibble()%>%
      ggplot(aes_string(x = "val", fill = fill_column))+
      geom_histogram(alpha = 0.5, position="identity")+
      facet_wrap(~var, scales = "free")+
      theme_bw()
  }
}


PlotNumericColumnsVOutcome<-function(df, outcome, colour_column = NULL){
  
  if (is.null(colour_column)){
    df%>%
      as_tibble()%>%
      # dplyr::select(pilot_emissions_predict_vars, re_predicted_emissions_w_adjustments, error, dataset)%>%
      select_if(function(col) is.numeric(col)
      )%>%
      gather(var, val, !outcome)%>%
      as_tibble()%>%
      ggplot(aes_string(x = "val", y = outcome))+
      geom_point(alpha = 0.01,position="identity")+
      facet_wrap(~var, scales = "free")+
      theme_bw()
    
  } else{
    df%>%
      as_tibble()%>%
      # dplyr::select(pilot_emissions_predict_vars, re_predicted_emissions_w_adjustments, error, dataset)%>%
      select_if(function(col) is.numeric(col) |
                  all(col %in% .[[colour_column]])
      )%>%
      gather(var, val,!c(colour_column, outcome))%>%
      as_tibble()%>%
      ggplot(aes_string(x = "val", y = outcome,colour = colour_column))+
      geom_point(alpha = 0.01,position="identity")+
      facet_wrap(~var, scales = "free")+
      theme_bw()
  }
}


PlotCategoricalColumns<-function(df, fill_column = NULL){
  
  
  if (is.null(fill_column)){
    df%>%
      as_tibble()%>%
      select_if(function(col) is.logical(col) | is.factor(col) | is.character(col))%>%
      gather(var, val)%>%
      group_by(var, val)%>%
      summarise(n= n())%>%
      ggplot()+
      geom_col(aes(y = val, x = n), alpha = 0.5, position="identity")+
      facet_wrap(~var, scales = "free")+
      theme_bw()
  } else{
    fill_column_sym = sym(fill_column)
    
    df%>%
      as_tibble()%>%
      select_if(function(col) is.logical(col) | is.factor(col) | is.character(col))%>%
      gather(var, val, !fill_column)%>%
      group_by(var, val, !!fill_column_sym)%>%
      summarise(n= n())%>%
      ggplot()+
      geom_col(aes_string(y = "val", x = "n", fill = fill_column), alpha = 0.5, position="identity")+
      facet_wrap(~var, scales = "free")+
      theme_bw()
    
  }
}

PlotCategoricalColumnsVOutcome<-function(df, outcome, fill_column = NULL){
  
  
  if (is.null(fill_column)){
    df%>%
      as_tibble()%>%
      select_if(function(col) is.logical(col) | is.factor(col) | is.character(col)|
                  all(col %in% .[[outcome]]))%>%
      gather(var, val, !outcome)%>%
      ggplot()+
      geom_density_ridges(aes_string(x = outcome, y = "val"),rel_min_height = 0.005, alpha = 0.5,
                          jittered_points = TRUE,
                          position = position_points_jitter(width = 0.5, height = 0),
                          point_shape = "|", point_size = 2)+
      facet_wrap(~var, scales = "free")+
      theme_bw()
  } else{
    
    df%>%
      as_tibble()%>%
      select_if(function(col) is.logical(col) | is.factor(col) | is.character(col)|
                  all(col %in% .[[outcome]]))%>%
      gather(var, val, !c(outcome, fill_column))%>%
      ggplot()+
      geom_density_ridges(aes_string(x = outcome, y = "val", fill = fill_column),rel_min_height = 0.005, alpha = 0.5,
                          jittered_points = TRUE,
                          position = position_points_jitter(width = 0.5, height = 0),
                          point_shape = "|", point_size = 2)+
      facet_wrap(~var, scales = "free")+
      theme_bw()
    
  }
}

Formulate <- function(y, x){
  as.formula(paste0(y, " ~ ", paste(x, collapse=" + ")))
}

ggplot_pdp <- function(obj, x) {
  
  p <-
    as_tibble(obj$agr_profiles) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "^[^_]*_")) %>%
    ggplot(aes(`_x_`, `_yhat_`)) +
    geom_line(data = as_tibble(obj$cp_profiles),
              aes(x = {{ x }}, group = `_ids_`),
              size = 0.5, alpha = 0.05, color = "gray50")
  
  num_colors <- n_distinct(obj$agr_profiles$`_label_`)
  
  if (num_colors > 1) {
    p <- p + geom_line(aes(color = `_label_`), size = 1.2, alpha = 0.8)
  } else {
    p <- p + geom_line(color = "midnightblue", size = 1.2, alpha = 0.8)
  }
  
  p
}

GetFeatureContribData<- function(df, explainer_obj){
  
  feature_contrib <- lapply(1:nrow(df), function(nx){
    # nx <- 5
    print(nx)
    
    out <- DALEX::predict_parts(explainer = explainer_obj,
                                new_observation = df[nx,],
                                type = "break_down")
    fdt <- data.table(out$variable, out$contribution,
                      stringsAsFactors = FALSE)
    setnames(x = fdt, old = colnames(fdt),
             new = c("variable", "contribution"))
    return(fdt)
  })
  
  feature_contrib_data <- rbindlist(feature_contrib)%>%as_tibble()%>%
    mutate(variable_levels = gsub(".*?\\s=\\s(.*?)", "\\1", variable),
           variable_name = gsub("(.*?)\\s=\\s.*", "\\1", variable))
  
  return(feature_contrib_data)
}

FormatReactable<- function(df,filt = T){
  reactable(df,
            outlined = TRUE,
            highlight = TRUE,
            striped = TRUE,
            showSortable = TRUE,
            compact = FALSE,
            bordered = TRUE,
            defaultExpanded = F,
            filterable = filt,
            showPageSizeOptions = TRUE,
            defaultColDef = colDef(
              format = colFormat(separators = TRUE, digits = 2)))
}

GenerateErrorBars <- function(df, c){
  
  scale<-
    if (class(df[[c]]) == "numeric"){
      scale_colour_distiller(type = "div")
    } else{
      scale_colour_iwanthue()
    }
  
  gg<-
    df%>%
    ggplot(aes_string(x = "id_hash", y = "error", colour = c))+
    geom_point(alpha = 0.5)+
    geom_segment(aes(yend=0,xend=id_hash))+
    geom_abline(slope = 0, colour = "red")+
    facet_wrap(~model, nrow = 1, scales = "free_x")+
    theme(axis.text.x=element_blank()) +
    scale+
    labs(x = "Observation")
  
  return(gg)
  
}


TuneExtractModel<- function(tuned_wf, string_model, outcome_var, training_data, testing_data, error_colours, explain_data_input){
  ### This function will extract the best fit from a particular model
  ### If tuning parameters, it will display different results and choose best one
  # library(tidymodels)
  # library(ggplot2)
  # library(dplyr)
  # library(DALEXtra)
  # library(reactable)
  
  
  print(string_model)
  
  fit_model<-
    extract_workflow_set_result(tuned_wf, string_model)
  
  model_metrics<-
    collect_metrics(fit_model, summarize = F)
  
  best_model<-select_best(fit_model,metric = "rmse")
  
  if(length(unique(model_metrics$.config)) > 1){
    tuning_plot<- autoplot(fit_model)+
      theme_bw()
    # only print best model if there are multiple options
    # print(best_model)
    
  }
  else{tuning_plot <- NULL}
  
  
  # Parallelize before training model again
  registerDoParallel(cores = parallel::detectCores(logical = FALSE))
  
  #Choose best random forest
  # Results of 10 fold cross validation
  fit_model_best <-
    tuned_wf %>%
    extract_workflow(string_model)%>%
    finalize_workflow(best_model)%>%
    fit(data = training_data)
  
  explain_data<- if(explain_data_input == "Train"){
    training_data
  }else if(explain_data_input == "Test"){
    testing_data
  }
  
  explainer <-
    explain_tidymodels(fit_model_best,
                       data = explain_data,
                       y = explain_data[[outcome_var]],
                       label = string_model,
                       verbose = F)
  
  
  #oob error (MSE)
  # fit_model_best%>%
  #   extract_fit_engine()%>%
  #   print()
  
  model_preds <-
    testing_data %>%
    mutate(model = "Test")%>%
    bind_cols(predict(fit_model_best, testing_data))%>%
    bind_rows(
      training_data %>%
        mutate(model = "Training")%>%
        bind_cols(predict(fit_model_best, training_data))
    )%>%
    mutate(error = !!sym(outcome_var) - .pred)
  
  #Calculate RMSE of TEST data
  ms<-model_preds%>%
    group_by(model)%>%
    metrics(truth = !!sym(outcome_var), estimate = .pred)%>%
    filter(.metric =="rmse")
  
  rmse_test<-ms%>%filter(model == "Test")%>%pull(.estimate)
  rmse_train<-ms%>%filter(model == "Training")%>%pull(.estimate)
  
  gg<-model_preds%>%
    left_join(ms, by = "model")%>%
    mutate(model = paste0(model, " (RMSE: ", round(.estimate, 2),")" ))%>%
    ggplot(aes(x = !!sym(outcome_var), y = .pred))+
    geom_point()+
    geom_abline(linetype = "dashed",
                color = "gray")+
    labs(x = "actual", y = "predicted", title = paste0(string_model, " Test Actual vs Predicted") )+
    theme_bw()+
    facet_wrap(~model)
  
  
  
  gg_error <- 
    model_preds%>%
    arrange(-error)%>%
    mutate(id_hash = 1:n(),
           id_hash = factor(id_hash, levels = id_hash))%>%
    lapply(error_colours, 
           GenerateErrorBars,
           df = .
    )%>%
    arrangeGrob(grobs = .)%>%
    as.ggplot()
  
  
  return(list(fit_model_best,
              FormatReactable(model_metrics),
              rmse_test,
              rmse_train,
              gg,
              explainer,
              gg_error,
              model_preds,
              tuning_plot,
              best_model
  )
  )
  
}
