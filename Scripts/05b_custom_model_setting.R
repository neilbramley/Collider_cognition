##################################################################
######## Optimise params and NLL ################
##################################################################
rm(list=ls())

library(tidyverse)
library(here)
library(xtable)
source(here('Scripts', 'modelNames.R'))
source(here('Scripts', 'optimUtils4par.R')) # functions to optimise CURRENTLY NO SCRIPT 5

set.seed(12)

load(here('Data', 'modelData', 'goOptim.rda')) # loads mp and df


# Parameters:

predictions<-get_prediction(
  pars = c(log(1/10),-Inf,log(1),log(1)),
  mp = mp,
  df = df,
  model = models[[1]]
)

for (i in 2:length(models))
{
  predictions<-rbind(predictions,
                     get_prediction(
                       pars = c(log(1/10),-Inf,log(1),log(1)),
                       mp = mp,
                       df = df,
                       model = models[[i]]))
}

# ------------- Combine and process --------------------

df_wide <- predictions |>
  pivot_wider(
    id_cols = c(trial_id, node3),
    names_from = model,
    values_from = predicted_prob
  )


fitforplot <- merge(df_wide, df, by = c('trial_id', 'node3')) # 288 of 37

## ---------------------------------------------------------------------------------------------------------------

save(fitforplot, file = here('Data', 'modelData', 'custom_models.rda')) #
