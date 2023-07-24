library(yaml)
library(here)

p<-read_yaml(file.path(here::here(),"Example/example_params.yaml"), eval.expr=TRUE)[[1]]

rmarkdown::render(input = file.path(here::here(),"model-report.Rmd"),
                    params = p,
                    output_file = "model_report_example",
                    output_dir = file.path(here::here(),"Example/reports"))
