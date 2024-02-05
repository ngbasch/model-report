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

See `Example/` for what an example directory looks like with a random forest model using the `iris` data.

1.  Create a modeling directory with the following folders: i) `/src` - customized R code, ii) `/reports` - where reports get saved), iii) `/other_chunks` - if you want any additional analysis in the tuning section (optional), iv) `/cache` - where cached data gets saved (optional)
2.  Update `/src` R scripts with whatever you want your analysis to be.
    1.  `src/packages.R` tells the report additional packages/functions/constants to load
    2.  `src/load_data.R` tells the report what data to load
    3.  `src/clean_data.R` tells the report how you want to clean data
    4.  `src/features_outcome.R` is where we define possible predictor variables + outcome variable
    5.  `src/specs.R` is where we define which modeling engines to use (e.g., RF, xgboost, lm)
    6.  `src/recipes.R` is where we define any other feature engineering steps as well as the outcome and predict variable(s) for multiple models
3.  Optionally update `/other_chunks` if you want additional analysis in the tuning section of the report.
4.  Create a set of parameters in a yaml file to tell the Rmarkdown report key information about the model run. Most important in that file is the `path` value, which should be wherever model directory was created in step 1. See example in `Example/example_params.yaml` .
5.  After previous steps are complete, you can then run the model by rendering the markdown file given parameter yaml file. See example in `Example/run_report.R` .
6.  If you are experience any errors, feel free to add a browser() statement in the chunk where the error is occurring to debug. **Be sure to remove debugging code before pushing any RMarkdown changes to Master**. For more information in RMarkdown, see: <https://support.posit.co/hc/en-us/articles/205612627-Debugging-with-RStudio#debugging-in-r-markdown-documents>

## How does the report come together?

The repo structure contains **three** key mechanisms that help us generate an extensible report.

-   General RMarkdown code: `model-report.Rmd` and all `/children/` files help us generate the bones of the html report. **These files will stay constant across reports.**

-   Project-specific R code: After the user creates a directory and adds `/src` folder, they can customize how data is read, cleaned, feature engineered, and modeled. **These files will be unique to a given project/report.**

-   Project-specific parameter YAML file: These are model specific specs defining **compilation-level** information including where to access project-specific R code, stratification variables, specific models to run for that report, etc. **These files will be unique to a given project/report.**

## How to set up the r environment
We will try to  standardize our R version and packages to make our results more reproducible across machines. 

We do this by taking advantage of two tools in R: 1) R [Projects](https://r4ds.hadley.nz/workflow-scripts.html#projects) and 2) the [renv](https://rstudio.github.io/renv/articles/renv.html) package for package management (similar to poetry in Python).

Note: while different R versions across machines may work in most cases, getting on the same R version (4.2 in this case) removes one potential culprit for inconsistency across machines. 

### Install and configure the compiler
One major issue we noticed in setting up a shared R environment was that package installation often failed due to compiler issues. To address these issues, follow the directions below:

Install GCC compiler to get a Fortran compiler. In your terminal, run:
```
brew install gcc
```

Make `.R` dir in your home directory, and `.R/Makevars` file. In your terminal, run:
```
cd ~
makedir .R
touch .R/Makevars
open .R/Makevars
```

Add these lines to your `Makevars` and save the file. These are taken from these StackOverflow threads: [here](https://stackoverflow.com/a/36001449), [here](https://stackoverflow.com/a/29993906) and [here](https://stackoverflow.com/a/43527031).
```
VER=-13
CC=gcc$(VER)
CXX=g++$(VER)
CFLAGS=-mtune=native -g -O2 -Wall -pedantic -Wconversion
CXXFLAGS=-mtune=native -g -O2 -Wall -pedantic -Wconversion
FLIBS=-L`gfortran -print-file-name=libgfortran.dylib | xargs dirname`
FC=/opt/homebrew/bin/gfortran
```

Make a `.Renviron` file in your home directory. In your terminal, run:
```
touch ~/.Renviron
open ~/.Renviron
```

Configure your  `.Renviron` file to point to our brew installed compiler.
The paths we set here have to match the location of your brew installed compiler. First, get this by running in your terminal:
```
readlink -f $(brew --prefix gcc) 
```
e.g.
```
/opt/homebrew/Cellar/gcc/13.2.0
```

We'll now use this path to set these values in your `.Renviron`. Add the block below to your `.Renviron`, and substitute the path you retrieved in the previous step for the `{path}/lib` and `{path}/bin` values. Save your file. This is based on this StackOverflow [response](https://stackoverflow.com/a/41959471)
```
LD_LIBRARY_PATH=/opt/homebrew/Cellar/gcc/13.2.0/lib
PATH=/opt/homebrew/Cellar/gcc/13.2.0/bin
```

Restart Rstudio

### Work within the `model_report.Rproj` Project
To ensure that the correct packages can be installed with `renv` and that you're working in the right environment, make sure to load the `source_research.Rproj` project file. You can confirm that this file is loaded by looking at the top right of your RStudio and seeing "Project: source_research." 

Loading this project will ensure that any time you are working in the repo, RStudio will prioritize the versions of the packages specific to the Lock file associated with the project. In other words, if you have a different version of `dplyr` than what's in the Lock file, RStudio will prioritize the Lock file version of `dplyr` when the project is loaded.

### Install and run `renv`
Install the latest version of `renv`. In your R session, run:
```
install.packages("renv")
```

Rebuild our environment. In your R session, run:
```
renv::restore()
```