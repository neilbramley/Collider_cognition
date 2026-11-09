#####################################################
########### REAL Model fit by participant ############
#####################################################

library(tidyverse)
library(here)

set.seed(12)

# --------- Load what we need ---------
#source(here('Scripts', 'optimUtilsNNN3.R')) # functions to optimise
# Unfitted model scores
#load(here('Data', 'modelData', 'modelAndDataUnfitig.rda')) # df, 288 of 23
# Participant data
load(here('Data', 'Data.Rdata')) # This is one big data, 2580, of 215 ppts
source(here('Scripts', 'modelNames.R'))
source(here('Scripts', 'optimUtils4par.R')) # functions to optimise CURRENTLY NO SCRIPT 5

set.seed(12)

load(here('Data', 'modelData', 'goOptim.rda')) # loads mp and df

df$mi[df$post==0]<-0
df$kl[df$post==0]<-0
df$ig[df$post==0]<-0
df$ig_old[df$post==0]<-0

# Split ppt data
byppt <- data |>
  select(subject_id, trial_id, node3)

# Participant data
data_list <- split(byppt, byppt$subject_id) # 215 elements each of 12x14

# Test what one of the elements looks like
#test <- data_list[[23]] # 12 obs of 3: subjid, trialid, node (what they answered in each trial)

# Merges the model1 series into each ppt, so now sum$n=12 for each df in the list
ppt_merges <- lapply(data_list, function(ppt_data) {
  merged <- df |>
    left_join(ppt_data, by = c('trial_id', 'node3')) |>
    mutate(n = ifelse(!is.na(subject_id), 1, 0)) |> # now always uses 'n'
    select(-subject_id)
  merged
})


#test2 <- ppt_merges[[23]] #
#test3 <- ppt_merges[[124]]
test4 <- ppt_merges[[215]]

# The new way. mp is the model predictions but it is not by trial. df is 288 obs of trial n.
# So now, mp will be the same, but df will be each of the participant splits

# -------- Back to optimisation ------------

# Test on one ppt (one element of the list)
get_optimisation(model_names, mp = mp, df = test4, kind_type = 'kl')
get_optimisation('full', mp = mp, df = test4, kind_type = 'kl')

# Call optimize_models(model_names, df) for every element of data_list
results_4par <- lapply(1:length(ppt_merges), function(i) {
  cat(sprintf("Ppt %d/%d finished\n", i, length(ppt_merges)))

  df_i <- ppt_merges[[i]]
  get_optimisation(
    model_names = model_names,
    mp = mp,
    df = df_i,
    kind_type = 'kl'
  )
})

print(results_4par)

results_4par <- lapply(results_4par, function(participant_res) {
  fits <- participant_res$model_fits

  uniform_row <- data.frame(
    model = "baseline",
    tau1 = NA_real_,
    epsilon = NA_real_,
    tau2 = NA_real_,
    logl = NA_real_,
    kappa = NA_real_,
    BIC = 49.9,
    stringsAsFactors = FALSE
  )

  # reorder/subset to the existing columns so rbind cannot mismatch
  uniform_row <- uniform_row[, names(fits), drop = FALSE]

  participant_res$model_fits <- rbind(fits, uniform_row)
  participant_res
})

# Add uniform baseline to each participant's model_fits - from chatGPT cos it is not working...
# results_4par <- lapply(results_4par, function(participant_res) {
#   # Create uniform baseline row (match existing column names)
#   uniform_row <- data.frame(
#     model = "baseline",
#     tau1 = "",
#     epsilon = "",
#     tau2 = "",
#     logl = "",
#     kappa = "",
#     BIC = "49.9"
#     # stringsAsFactors = FALSE,
#     # check.names = FALSE
#   )
#   # Append as 13th row (rbind keeps order)
#   participant_res$model_fits <- rbind(
#     participant_res$model_fits[1:12, ], # first 12 rows
#     uniform_row, # new row as 13th
#     participant_res$model_fits[13:nrow(participant_res$model_fits)] # rest (if any)
#   )
#   participant_res
# })

# Now remove rows 14 and 15 from each ppt [[1]]
# results_4parN <- lapply(results_4par, function(res) {
#   # access the first element, which contains model_fits and predictions
#   res[[1]] <- res[[1]][-c(14, 15), ]
#   res
# })
#
# test7 <- results_4parN[[203]][[1]]

# Save results of both series
save(
  results_4par,
  file = here('Data', 'modelData', 'individFits4.rda')
)
