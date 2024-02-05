
p<-read_yaml(file.path(here(),"Example/example_params.yaml"), eval.expr=TRUE)[[1]]

RenderRMD(p = p,
          od = file.path(here(),"Example/reports"),
          savetoS3 = F)

# Run manually ---------------------------
params<-p
input_path<-file.path(params$path, "src")

source(file.path(here(), "Example/src/packages.R"))
#data_child.Rmd ---
source(file.path(here(),"Example/src/load_data.R"))
source(file.path(here(),"Example/src/clean_data.R"))
source(file.path(here(),"Example/src/features_outcome.R"))

#tidymodels_setup_child.Rmd --
source(file.path(here(),"scripts/training_test.R"))
source(file.path(here(),"scripts/folds.R"))
source(file.path(here(),"Example/src/recipes.R"))
source(file.path(here(),"Example/src/specs.R"))
source(file.path(here(),"scripts/create_workflowset.R"))

#tuning_metrics_child.Rmd --
source(file.path(here(),"scripts/tune_cv.R"))
# View error if there is one:
# tuned_workflows[2,][["result"]][[1]][[".notes"]]
source(file.path(here(),"scripts/tune_workflows.R"))



