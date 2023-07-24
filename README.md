# model-report

## Overview

The goal of this report is to create a flexible, extensible system for modeling.

**Why a report?** It can be difficult to share technical modeling results with other data scientists when developing new models. Documentation saved as wiki pages can quickly get out of date and be difficult to update, making it difficult to leave a clear paper trail of the model development lifecycle. **This challenge makes collaboration and knowledge sharing a challenge.**

**Objective:** Create a code template meant to easily generate shareable model results via an interactive html report. This template will be heavily parameterized such that different data, models, and inputs can be run with the same steps. This process will hopefully save users time re-writing the same code.

The model will allow users to explore important steps in the modeling process including:

-   **Data/Features:** Data origination and standard exploratory data analysis

-   **Modeling setup:** Feature engineering, model choice

-   **Tuning and Metrics:** Model results, including potential parameter tuning opportunities

-   **Explain:** Explainable AI results including variable importance, PDPs, ADP, etc.

## How to set up your own report

See `utils/Example` for what an example directory looks like with a random forest model using the `iris` data.

1.  Create a modeling directory with the following folders: i) `/src` - customized R code, ii) `/reports` - where reports get saved), iii) `/other_chunks` - if you want any additional analysis in the tuning section (optional), iv) `/cache` - where cached data gets saved (optional)
2.  Update `/src` R scripts with whatever you want your analysis to be.
    1.  `src/packages.R` tells the report additional packages/functions/constants to load
    2.  `src/load_data.R` tells the report what data to load
    3.  `src/clean_data.R` tells the report how you want to clean data
    4.  `src/features_outcome.R` is where we define possible predictor variables + outcome variable
    5.  `src/specs.R` is where we define which modeling engines to use (e.g., RF, xgboost, lm)
    6.  `src/recipes.R` is where we define any other feature engineering steps as well as the outcome and predict variable(s) for multiple models
3.  Optionally update `/other_chunks` if you want additional analysis in the tuning section of the report.
4.  Create a set of parameters in a yaml file to tell the Rmarkdown report key information about the model run. Most important in that file is the `path` value, which should be wherever model directory was created in step 1. See example in `utils/Example/example_params.yaml` .
5.  After previous steps are complete, you can then run the model by rendering the markdown file given parameter yaml file. See example in `utils/Example/run_report.R` .
6.  If you are experience any errors, feel free to add a browser() statement in the chunk where the error is occurring to debug. **Be sure to remove debugging code before pushing any RMarkdown changes to Master**. For more information in RMarkdown, see: <https://support.posit.co/hc/en-us/articles/205612627-Debugging-with-RStudio#debugging-in-r-markdown-documents>

## How does the report come together?

The repo structure contains **three** key mechanisms that help us generate an extensible report.

-   General RMarkdown code: `model-report.Rmd` and all `/children/` files help us generate the bones of the html report. **These files will stay constant across reports.**

-   Project-specific R code: After the user creates a directory and adds `/src` folder, they can customize how data is read, cleaned, feature engineered, and modeled. **These files will be unique to a given project/report.**

-   Project-specific parameter YAML file: These are model specific specs defining **compilation-level** information including where to access project-specific R code, stratification variables, specific models to run for that report, etc. **These files will be unique to a given project/report.**
