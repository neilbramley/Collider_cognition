################################################################################
########### Master script for Collider project started June 2025 ##########
#################################################################################

library(tidyverse)
#library(rjson)
library(ggnewscale)
library(here)
library(RColorBrewer)
library(ggplot2)
library(lme4)
library(lmerTest)

set.seed(12)

# ------------- 0. Source utils -------------
source(here('Scripts', 'cesmUtils.R')) # Functions for running the cesm model - used in 03

source(here('Scripts', 'modelNames.R')) # Static lists with model characteristics - used in 05

source(here('Scripts', 'optimUtils4par.R')) # Functions for modelling likelihood calculation - used in 06

source(here('Scripts', 'plotUtils.R')) # Functions for plotting to compare model and ppts used in 10, 11


#--------------- 1. Get ppt data from behavioural experiment  -------------------
# (JavaScript for the experiment itself is in the folder Experiment.
# For each participant a csv was saved on the server and then transferred out of there into Data)
# Demographics are in the `preprocessing` # file , quiet=TRUE
source(here('Scripts', '01preprocess.R')) # Collates individual csvs, reconciles with prolific report, saves `Data.Rdata` and also `ppts.csv`

#-------------- 2. Create parameters, run cesm, get model predictions and save them ------------
source(here('Scripts', '02setParams.R')) # The baserates of the causal model
source(here('Scripts', '03getPreds.R')) # Gets cesm model predictions using `cesmUtils.R`.

# Process model predictions to be more user friendly: take average of 10 model runs, wrangles and renames variables, splits out node values 0 and 1
source(here('Scripts', '04processPreds.R')) # Also sets a column of 1s and tags like Actual for the lesions

# -------------3. Results: fit model, compare predictions, plot etc -----------------

source(here('Scripts', '05optimise.R')) # Uses `optimUtils4par.R` to fit models.
source(here('Scripts', '06processForPlot.R')) # Make model predictions use friendly for plotting

source(here('Scripts', '07reportFigs.R')) # Main plots on every trial at once. Uses `plotUtils` functions to compare models
source(here('Scripts', '08reportFigs2.R')) # Other plots not using functions; aggregate and split plots

source(here('Scripts', '09fitByppt.R')) # Best model fit by participant. Input: data.rda
source(here('Scripts', '10presentByppt.R')) # chisq tests and component bar plot


# ------------- 4. Standalone analyses ------------
source(here("Scripts", "Standalone", "demogs.R"))
source(here("Scripts", "Standalone", "chisq.R")) # Uses the `unfitmodelpredictions` data but only to get grouped ppt numbers. Doesn't vary with different models
source(here("Scripts", "Standalone", "abnormalInflation.R")) # Tests for overall presence of the effect seen in the plot of the 111 conditions by pgroup and structure - doesn't find any because it's only seen in pgroup 1

# Check if cover story affects answers (it doesn't - except in 2/36 conditions due to noise)
rmarkdown::render(
  input = here("Scripts", "Standalone", "coverTest.Rmd"),
  output_file = here("Other", "Reports", "coverTest.html")
)
