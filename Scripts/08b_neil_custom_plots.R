# 08b_neil_custom_plots

rm(list=ls())
################################################################################
########### Generate plots and other reporting figs using functions ##############
#################################################################################

library(tidyverse)
library(here)
library(ggnewscale)
library(RColorBrewer)
library(ggplot2)


source(here('Scripts', 'plotUtils_neiladditions.R')) # Functions for plotting to compare model and ppts, using ggplot
load(here('Data', 'modelData', 'fitforplot4par.rda'), verbose = T) # 288 of 31. Generated in script 07.
df_fitted<-df
load(here('Data', 'modelData', 'custom_models_nb.rda'), verbose = T) # 288 of 31. Generated in script 07.
df <- fitforplot


df<-cbind(df, df_fitted[!names(df_fitted)%in%names(df)]) #Add columns we are missing
df<-df[,names(df_fitted)] #Put in same order
df$pgroup<-df_fitted$pgroup


p_full <- plot_model_pgroup('full', 'A=.5,Au=.1,B=.5,Bu=.8', df)
p_noKind <- plot_model_pgroup('noKind', 'A=.5,Au=.1,B=.5,Bu=.8', df)
# p_base <- plot_model_pgroup('noInfnoKindnoSelect', 'A=.5,Au=.1,B=.5,Bu=.8', df)
print(p_full)
print(p_noKind)
# print(p_base)
p<-list()
p[['compare_inf2']] <- plot_two_models_pgroup_nb('noKind', 'noInfnoKind', 'A=.5,Au=.1,B=.5,Bu=.8',df,
                                            plot_title = 'Comparing models with (+) or without (-) Inference\nSetting 2 (A=.5,Au=.1,B=.5,Bu=.8)',
                                            highlight_list = c('Conjunctive: A=1,B=1,E=1', 'Disjunctive: A=1,B=0,E=1', 'Disjunctive: A=0,B=1,E=0'))
p[['compare_select2']] <- plot_two_models_pgroup_nb('noKind', 'noKindnoSelect', 'A=.5,Au=.1,B=.5,Bu=.8', df,
                                               plot_title = 'Comparing models with (+) or without (-) Selection\nSetting 2 (A=.5,Au=.1,B=.5,Bu=.8)',
                                               highlight_list = c('Conjunctive: A=1,B=1,E=1', 'Disjunctive: A=1,B=1,E=1', 'Disjunctive: A=1,B=1,E=0', 'Disjunctive: A=0,B=0,E=0'))
p[['compare_kind2']] <- plot_two_models_pgroup_nb('full', 'noKind', 'A=.5,Au=.1,B=.5,Bu=.8', df,
                                             plot_title = 'Comparing models with (+) or without (-) Kindness\nSetting 2 (A=.5,Au=.1,B=.5,Bu=.8)',
                                             highlight_list = c())

print(p[['compare_inf2']])
print(p[['compare_select2']])
print(p[['compare_kind2']])

p[['compare_inf1']] <- plot_two_models_pgroup_nb('noKind', 'noInfnoKind', 'A=.1,Au=.5,B=.8,Bu=.5',df,
                                            plot_title = 'Comparing models with (+) or without (-) Inference\nSetting 1 (A=.1,Au=.5,B=.8,Bu=.5)',
                                            highlight_list = c())
p[['compare_select1']] <- plot_two_models_pgroup_nb('noKind', 'noKindnoSelect', 'A=.1,Au=.5,B=.8,Bu=.5', df,
                                               plot_title = 'Comparing models with (+) or without (-) Selection\nSetting 1 (A=.1,Au=.5,B=.8,Bu=.5)',
                                               highlight_list = c('Conjunctive: A=1,B=1,E=1', 'Disjunctive: A=1,B=1,E=1', 'Conjunctive: A=0,B=0,E=0', 'Disjunctive: A=0,B=0,E=0', 'Disjunctive: A=1,B=1,E=0', 'Disjunctive: A=0,B=1,E=0', 'Disjunctive: A=1,B=0,E=0'))
p[['compare_kind1']] <- plot_two_models_pgroup_nb('full', 'noKind', 'A=.1,Au=.5,B=.8,Bu=.5', df,
                                             plot_title = 'Comparing models with (+) or without (-) Kindness\nSetting 1 (A=.1,Au=.5,B=.8,Bu=.5)',
                                             highlight_list = c('Conjunctive: A=1,B=1,E=1', 'Disjunctive: A=1,B=0,E=1'))


p[['compare_inf3']] <- plot_two_models_pgroup_nb('noKind', 'noInfnoKind', 'A=.1,Au=.7,B=.8,Bu=.5',df,
                                                 plot_title = 'Comparing models with (+) or without (-) Inference\nSetting 3 (A=.1,Au=.7,B=.8,Bu=.5)',
                                                 highlight_list = c())
p[['compare_select3']] <- plot_two_models_pgroup_nb('noKind', 'noKindnoSelect', 'A=.1,Au=.7,B=.8,Bu=.5', df,
                                                    plot_title = 'Comparing models with (+) or without (-) Selection\nSetting 3 (A=.1,Au=.7,B=.8,Bu=.5)',
                                                    highlight_list = c())
p[['compare_kind3']] <- plot_two_models_pgroup_nb('full', 'noKind', 'A=.1,Au=.7,B=.8,Bu=.5', df,
                                                  plot_title = 'Comparing models with (+) or without (-) Kindness\nSetting 3 (A=.1,Au=.7,B=.8,Bu=.5)',
                                                  highlight_list = c())

# print(p_compare_inf1)
# print(p_compare_select1)
# print(p_compare_kind1)

plot_names<-c('compare_inf1','compare_select1','compare_kind1',
              'compare_inf2','compare_select2','compare_kind2',
              'compare_inf3','compare_select3','compare_kind3')
for (i in 1:length(plot_names))
{
  ggsave(
    filename = paste0(plot_names[i], ".pdf"),
    plot = p[[plot_names[i]]],
    path = here("Other", "Plots"),
    width = 12,
    height = 6,
    units = "in"
  )
  
}



