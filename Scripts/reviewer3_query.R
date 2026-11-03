
data$answer<-as.factor(data$ans)
ss<-unique(data$subject_id)

data$first_time_cover<-F

for (i in 1:length(ss))
{
    to<-data$trial_order[data$subject_i==ss[i]]
    pg<-data$scenario[data$subject_i==ss[i]]
    first<-c(T,T,T)
    this_first_time_cover<-rep(F, 12)
    
    for (j in 1:12)
    {
      ix<-which(to==j)
      if (first[pg[ix]])
      {
        this_first_time_cover[ix]<-T
        first[pg[to[ix]]]<-F
      }

    }
  data$first_time_cover[data$subject_i==ss[i]]<-this_first_time_cover
}
  
 data$trialtype 
ggplot(data %>% filter(trialtype %in% c('c1','d1')), aes(answer, fill=pgroup)) +
  geom_bar(aes(y = after_stat(prop), group = pgroup), position = position_dodge(preserve = "single")) +
  facet_wrap(~trialtype, labeller = labeller(trialtype = c(c1 = "Conjunctive:A=B=E=1", d1 = "Disjunctive:A=B=E=1"))) +
  labs(x='Response option selected', y='Proportion', fill='Setting') +
  theme_bw() +
ggtitle('Comparing Probability Settings All Trials')

ggsave('reviewer3_comparison1.pdf', width = 5, height = 2.5)
ggplot(data %>% filter(first_time_cover==T, trialtype %in% c('c1','d1')), aes(answer, fill=pgroup)) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_discrete(drop = FALSE) +
  facet_wrap(~trialtype, labeller = labeller(trialtype = c(c1 = "Conjunctive:A=B=E=1", d1 = "Disjunctive:A=B=E=1"))) +
  labs(x='Response option selected', y='Proportion', fill='Setting') +
  theme_bw() +
  geom_bar(aes(y = after_stat(prop), group = pgroup), position = position_dodge(preserve = "single")) +
  ggtitle('First Appearance of Each Covery Story Only')
ggsave('reviewer3_comparison2.pdf', width = 5, height = 2.5)
