##################################################################
######## Optimise params and NLL ################
##################################################################

library(tidyverse)
library(here)
library(xtable)
source(here('Scripts', 'modelNames.R'))
source(here('Scripts', 'optimUtils4par.R')) # functions to optimise CURRENTLY NO SCRIPT 5

set.seed(12)


load(here('Data', 'modelData', 'goOptim.rda')) # loads mp and df

# -------------- Run the functions ------------------
# # reviewer suggestion to not pay kindness bonuses to ruled out states.
df$mi[df$post==0]<-0
df$kl[df$post==0]<-0
df$ig[df$post==0]<-0
df$ig_old[df$post==0]<-0

# c1: 000
# c2: 010
# c3: 100
# c4: 110
# c5: 111
# d1: 000
# d2: 010
# d3: 011
# d4: 100
# d5: 101
# d6: 110
# d7: 111
# df.l <-df %>% mutate(kl=kl/max(kl), mi=mi/max(mi)) %>%
#   gather(k_type, kindness, kl:mi) %>% filter(!node3 %in% c('A=0','A=1','B=0','B=1')) %>%
#   mutate(tt = factor(trialtype, levels = c("c1", "c2", "c3", "c4", "c5", "d1", "d2", "d3", "d4", "d5", "d6", "d7"),
#                      labels = c('Conj: A=0,B=0,E=0','Conj: A=0,B=1,E=0','Conj: A=1,B=0,E=0','Conj: A=1,B=1,E=0','Conj: A=1,B=1,E=1',
#                                 'Disj: A=0,B=0,E=0','Disj: A=0,B=1,E=0','Disj: A=0,B=1,E=1','Disj: A=1,B=0,E=0','Disj: A=1,B=0,E=1','Disj: A=1,B=1,E=0','Disj: A=1,B=1,E=1')),
#                      pgroup = factor(pgroup, levels = c('1','2','3'), labels = c("Setting 1: A=.1,Au=.5,B=.8,Bu=.5",
#                                                                                  "Setting 2: A=.5,Au=.1,B=.5,Bu=.8",
#                                                                                  "Setting 3: A=.1,Au=.7,B=.8,Bu=.5")),
#          k_type = factor(k_type, levels = c('kl','mi'), labels = c('KL Divergence', 'Mutual Information')))
         
# ggplot(df.l, aes(x=node3, y=kindness, shape = k_type, colour = k_type, group = k_type)) +
#   geom_point() +
#   geom_line() +
#   facet_grid(tt ~ pgroup) +
#   labs(colour = "Kindness type", group = "Kindness type", group = "Kindness type", shape = "Kindness type", y = "Kindness level", x='Variable state') +
#   theme_bw() +
#   theme(legend.position = 'bottom')
# ggsave('kindness_measures.pdf', width = 8, height = 15)





results <- get_optimisation(
  model_names = model_names, # e.g. names(models)
  mp = mp, #TODO ADD FLAG FOR kindness_type and propagate through
  df = df,
  kind_type='kl'
)

print(results$model_fits)



# REPRODUCE ORIGINAL SUBMISSION RESULT:
# 
# mp.orig <- mp |> # 
#   mutate(Actual = case_when(
#     node2 == 'A' ~ A==E.x,
#     node2 == 'B' ~ B==E.x,
#     node2 == 'Au' ~ Au==E.x,
#     node2 == 'Bu' ~ Bu==E.x
#   ))
# 
# 
# # Condition 2 - many of these are already caught but just to catch the extras 
# mp.orig$Actual[mp.orig$A=='0' & mp.orig$node3=='Au=1'] <- FALSE
# mp.orig$Actual[mp.orig$B=='0' & mp.orig$node3=='Bu=1'] <- FALSE
# 
# 
# mp.orig$cesm[mp.orig$Actual==F]<- -10
# 
# 
# results <- get_optimisation(
#   model_names = model_names, # e.g. names(models)
#   mp = mp.orig, #TODO ADD FLAG FOR kindness_type and propagate through
#   df = df,
#   kind_type='ig_old'
# )

# ---------- Format and save the results

results$model_fits[, -1] <- lapply(results$model_fits[, -1], trim_zeros)
xtable(results$model_fits, digits = 3)

# Move the kappa column to before logl
results$model_fits <- results$model_fits |>
  relocate(kappa, .before = logl)

filename <- "4par" # title for html

xtab1 <- xtable(results$model_fits, digits = 3) # caption = c('Full title', 'Short title') if you wwant to include long tags
row.names(xtab1)<-NULL
xtab1
# Then print to file in both tex and html
print(xtab1, file = here('Data', 'fitTables', paste0(filename, '.tex'))) # LaTeX output
print(
  xtab1,
  type = 'html',
  file = here('Data', 'fitTables', paste0(filename, '.html'))
)

saveRDS(xtab1, file = here('Data', 'fitTables', paste0(filename, '.rds')))


# ------------- Combine and process --------------------

df_wide <- results$predictions |>
  pivot_wider(
    id_cols = c(trial_id, node3),
    names_from = model,
    values_from = predicted_prob
  )


fitforplot <- merge(df_wide, df, by = c('trial_id', 'node3')) # 288 of 37

## ---------------------------------------------------------------------------------------------------------------

save(fitforplot, file = here('Data', 'modelData', 'fit4par.rda')) #
