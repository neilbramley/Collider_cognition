#######################################################
###### Collider - tidy up model predictions  #####
#######################################################

library(here)
library(tidyverse)


# The functions from 'cesmfunctions' generated model predictions for CESM only.
# To make the full model and all the lesioned version, run this preprocessing step and then combine in '06getLesions'

load(here('Data', 'modelData', 'all.rda')) # 1440
load(here('Data', 'Data.rdata')) # loads data 2580 obs of 23 - used further down the script

all$pgroup <- as.factor(all$pgroup)
all$structure <- as.factor(all$structure)

all$A <- as.factor(all$A)
all$Au <- as.factor(all$Au)
all$B <- as.factor(all$B)
all$Bu <- as.factor(all$Bu)

all$E.x <- as.factor(all$E.x)
all$E.y <- as.factor(all$E.y)

# Bring in trialtype and rename as the proper string name
all$trialtype <- all$groupPost
all$trialtype[all$trialtype == 1 & all$structure == 'disjunctive'] <- 'd1'
all$trialtype[all$trialtype == 2 & all$structure == 'disjunctive'] <- 'd2'
all$trialtype[all$trialtype == 3 & all$structure == 'disjunctive'] <- 'd3'
all$trialtype[all$trialtype == 4 & all$structure == 'disjunctive'] <- 'd4'
all$trialtype[all$trialtype == 5 & all$structure == 'disjunctive'] <- 'd5'
all$trialtype[all$trialtype == 6 & all$structure == 'disjunctive'] <- 'd6'
all$trialtype[all$trialtype == 7 & all$structure == 'disjunctive'] <- 'd7'

all$trialtype[all$trialtype == 1 & all$structure == 'conjunctive'] <- 'c1'
all$trialtype[all$trialtype == 2 & all$structure == 'conjunctive'] <- 'c2'
all$trialtype[all$trialtype == 3 & all$structure == 'conjunctive'] <- 'c3'
all$trialtype[all$trialtype == 4 & all$structure == 'conjunctive'] <- 'c4'
all$trialtype[all$trialtype == 5 & all$structure == 'conjunctive'] <- 'c5'

all$trialtype <- as.factor(all$trialtype)

# First we have to average the model runs - goes from 1920 to 192
all <- all |>
  group_by(pgroup, structure, index) |>
  mutate(
    A_cesm = mean(mA),
    Au_cesm = mean(mAu),
    B_cesm = mean(mB),
    Bu_cesm = mean(mBu)
  ) |>
  distinct(pgroup, structure, index, .keep_all = TRUE)


# Pivot longer and list node names with their CESM values
all <- all |>
  pivot_longer(
    cols = c(A_cesm:Bu_cesm),
    names_to = c('node', '.value'),
    names_sep = '_'
  )

all <- all |>
  select(-(mA:run))

# 768 is then 1920/10 = 192 x 4 variables

# The unobserved variables have different explanatory role depending what we presume their value to be.
# So we need to split them out. First one with 6 (just for unobserved)
all$node2 <- all$node
all$node[all$Au == '0' & all$node2 == "Au"] <- 'Au=0'
all$node[all$Au == '1' & all$node2 == "Au"] <- 'Au=1'
all$node[all$Bu == '0' & all$node2 == "Bu"] <- 'Bu=0'
all$node[all$Bu == '1' & all$node2 == "Bu"] <- 'Bu=1'
# Also need one with 8, where every node takes the value it has
all$node3 <- all$node
all$node3[all$A == '0' & all$node2 == 'A'] <- 'A=0'
all$node3[all$A == '1' & all$node2 == 'A'] <- 'A=1'
all$node3[all$B == '0' & all$node2 == 'B'] <- 'B=0'
all$node3[all$B == '1' & all$node2 == 'B'] <- 'B=1'

# Get a tag of the unobserved variables' settings. Then we can group data by this for plotting
all <- all |>
  unite("uAuB", Au, Bu, sep = "", remove = FALSE)

all$node <- as.factor(all$node)
all$node2 <- as.factor(all$node2)
all$node3 <- as.factor(all$node3)
all$uAuB <- as.factor(all$uAuB)


# -------

# Also need a column for the actual settings
# They should be:
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

# Rename to be consistent later

mp <- all

# Add column of 1s called noSelect
mp$noSelect <- 1


# ------- Brief diversion to get the individual posteriors for info gain -----------
# Get the individual posterior, for example:
# For Au, keep Au fixed and sum the joint posterior for each possible value of Bu.

getpost <- mp |>
  filter(!node2 %in% c('A', 'B')) |>
  group_by(pgroup, trialtype, node3, .drop = F) |> # if we need Eig then go back and group by node2
  summarise(post = sum(posterior), prior = sum(PrUn))


# If kindness is KL divergence
unobs_KL <- getpost |>
  group_by(pgroup, trialtype, node3) |>
  mutate(
    kl = round(
      if_else(post == 0, 0, post * log2(post / prior)) +
        if_else(post == 1, 0, (1 - post) * log2((1 - post) / (1 - prior))),
      3
    )
  ) |>
  ungroup()

# If kindness is entropy
unobs_IG <- getpost |>
  group_by(pgroup, trialtype, node3) |>
  mutate(
    ig = -sum(if_else(c(prior, 1-prior) == 0, 0, c(prior, 1-prior) * log2(c(prior, 1-prior)))) -
      -sum(if_else(c(post, 1-post) == 0, 0, c(post, 1-post) * log2(c(post, 1-post))))
  ) |>
  ungroup()

unobs_IG_old <- getpost |>
  group_by(pgroup, trialtype, node3) |>
  summarise(
    prior_entropy = round(-sum(prior * log2(prior + 1e-10)), 3),
    post_entropy = round(-sum(post * log2(post + 1e-10)), 3),
    ig_old = round(prior_entropy - post_entropy, 3)
  ) |>
  ungroup()

# If kindness is mutual information
unobs_MI <- getpost |>
  group_by(pgroup, trialtype, node3) |>
  mutate(
    mi = 0
  ) |>
  ungroup()

ab_obs_c<-matrix(c(0,0,
                   0,1,
                   1,0,
                   1,1,
                   1,1), ncol=2, byrow=T)

ab_obs_d<-matrix(c(0,0,
                   0,1,
                   0,1,
                   1,0,
                   1,0,
                   1,1,
                   1,1), ncol=2, byrow=T)

uss<-matrix(c(0,0,
         0,1,
         1,0,
         1,1), ncol=2, byrow=T)
for (i in 1:nrow(unobs_MI))
{
  if (!unobs_MI$node3[i]%in%c('A=0','A=1','B=0','B=1'))
  {
    if (unobs_MI$pgroup[i]=='1')
    {
      aubu<-c(params1[[2]][2], params1[[2]][4])
    } else if (unobs_MI$pgroup[i]=='2')
    {
      aubu<-c(params2[[2]][2], params2[[2]][4])
    } else if (unobs_MI$pgroup[i]=='3')
    {
      aubu<-c(params3[[2]][2], params3[[2]][4])
    }
    
    u_prior<-c((1-aubu[1])*(1-aubu[2]),(1-aubu[1])*aubu[2],aubu[1]*(1-aubu[2]),aubu[1]*aubu[2])
    
    if (substr(unobs_MI$node3[i], 1,1)=='A')
    {
      viq<-'A'  
    } else if (substr(unobs_MI$node3[i], 1,1)=='B')
    {
      viq<-'B'  
    }
    
    if (substr(unobs_MI$trialtype[i],1,1)=='c')
    {
      #Conjunctive
      ab_obs<-ab_obs_c[as.numeric(substr(unobs_MI$trialtype[i],2,2)),]
      
      if (ab_obs[1]==1 & ab_obs[2]==1)
      {
        pE<-prod(aubu)
      } else
      {
        pE<-0
      }
      pri_ent<- -sum(c(1-pE, pE)* log2(c(1-pE, pE)), na.rm=T)
      EgU<-rowSums(sweep(uss, 2, ab_obs ,'*'))==2
      
    } else if (substr(unobs_MI$trialtype[i],0,1)=='d')
    {
      #Disjunctive
      ab_obs<-ab_obs_d[as.numeric(substr(unobs_MI$trialtype[i],2,2)),]
      
      if (ab_obs[1]==1 & ab_obs[2]==1)
      {
        pE<-1-(1-aubu[1])*(1-aubu[2])
      } else if (ab_obs[1]==1 & ab_obs[2]==0) {
        pE<-aubu[1]
      }else if (ab_obs[1]==0 & ab_obs[2]==1) {
        pE<-aubu[2]
      } else if (ab_obs[1]==0 & ab_obs[2]==0)
      {
        pE<-0
      }
      pri_ent<- -sum(c(1-pE, pE)* log2(c(1-pE, pE)), na.rm=T)
      #au=0, bu=0
      EgU<-rowSums(sweep(uss, 2, ab_obs ,'*'))>0
    }
    
    
    
    if (viq=='A')
    {
      post1<-sum(EgU[uss[,1]==0]*(u_prior[uss[,1]==0]/sum(u_prior[uss[,1]==0]))* c(1-aubu[2], aubu[2])) #if viq is F
      post2<-sum(EgU[uss[,1]==1]*(u_prior[uss[,1]==2]/sum(u_prior[uss[,1]==2]))* c(1-aubu[2], aubu[2])) #if viq is T
      ent1<- -sum(c(post1, 1-post1)*log2(c(post1, 1-post1)), na.rm=T)
      ent2<- -sum(c(post2, 1-post2)*log2(c(post2, 1-post2)), na.rm=T)
      eig<-(pri_ent-ent1)*(1-pE) + (pri_ent-ent2)*pE
    } else if (viq=='B')
    {
      post1<-sum(EgU[uss[,2]==0]*(u_prior[uss[,2]==0]/sum(u_prior[uss[,2]==0]))* c(1-aubu[1], aubu[1])) #if viq is F
      post2<-sum(EgU[uss[,2]==1]*(u_prior[uss[,2]==2]/sum(u_prior[uss[,2]==2]))* c(1-aubu[1], aubu[1])) #if viq is T
      ent1<- -sum(c(post1, 1-post1)*log2(c(post1, 1-post1)), na.rm=T)
      ent2<- -sum(c(post2, 1-post2)*log2(c(post2, 1-post2)), na.rm=T)
      eig<-(pri_ent-ent1)*(1-pE) + (pri_ent-ent2)*pE
    }
    unobs_MI$mi[i]<-eig
  }
  #e.g. p3 d7, 11?, P(E|Au=1)=1 ; P(E|Au=0)=.5 ; P(E) = 1-(1-.7)(1-.5) ; (1*.7) + (0.5*.3)...
}

# This will be 288 obs, same size as data and ppts, in the eventual likelihood, remember to save it with mp
ig <- unobs_IG |>
  select(pgroup, trialtype, node3, ig)
kl <- unobs_KL |>
  select(pgroup, trialtype, node3, kl)
mi <- unobs_MI |>
  select(pgroup, trialtype, node3, mi)

ig_old<-unobs_IG_old |>
  select(pgroup, trialtype, node3, ig_old)
# ---------- Do the work on participant data here ------------

# ------------- 2. Summarise participant data in the 288 trial format ---------------------

dataNorm <- data |>
  group_by(pgroup, trialtype, node3, .drop = FALSE) |>
  tally() |>
  mutate(prop = n / sum(n))


dataNorm <- dataNorm |>
  unite('trial_id', pgroup, trialtype, sep = "_", remove = FALSE)

dataNorm$trial_id <- as.factor(dataNorm$trial_id)

# modelAndData <- modelAndData |>
#   group_by(trial_id) |>
#   mutate(baseline = 1 / n())

# Actually let's call it df
df <- merge(dataNorm, ig, by = c('pgroup', 'trialtype', 'node3')) # adds in ig, 288 obs TODO: SEPARATE AND ADD KL, IG, MI
df <- merge(df, kl, by = c('pgroup', 'trialtype', 'node3'))
df <- merge(df, mi, by = c('pgroup', 'trialtype', 'node3'))
df <- merge(df, ig_old, by = c('pgroup', 'trialtype', 'node3'))

df<-merge(df, getpost, by = c('pgroup', 'trialtype', 'node3'))
#-------- Create variables coding the actual observation -------------
# We had this before in data, but it wasn't right because of the incomplete counterbalancing issue
# We know trialtype is right, so let's recode
df.map <- data.frame(
  condition = c(
    'c1',
    'c2',
    'c3',
    'c4',
    'c5',
    'd1',
    'd2',
    'd3',
    'd4',
    'd5',
    'd6',
    'd7'
  ),
  A = c(0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1),
  B = c(0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1),
  E = c(0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1)
)

df <- df |>
  left_join(df.map, by = c("trialtype" = "condition"))

# Although we don't filter by Include because now we have the epsilon par, still nice to know how many ppts choose noise
df <- df |>
  mutate(
    Include = !((node3 == 'B=0' & B == 1) |
      (node3 == 'B=1' & B == 0) |
      (node3 == 'A=0' & A == 1) |
      (node3 == 'A=1' & A == 0))
  )

df <- df |>
  mutate(
    Observed = if_else(node3 %in% c('A=0', 'A=1', 'B=0', 'B=1'), TRUE, FALSE)
  )

# Apply Actual here, not in MP
df <- df |>
  mutate(
    Actual = str_sub(as.character(node3), -1) == as.character(E)
  )

# The next no Actual
#A=1, B=0, E=0, Bu=0 = F
#A=0, B=1, E=0, Au=0 = F
#A=0, B=1, E=1, Au=1 = F
#A=1, B=0, E=1, Bu=1 = F

#df$Actual[df$B == '1' & df$E == '0' & df$node3 == 'Bu=1'] <- FALSE
#df$Actual[df$B == '0' & df$node3 == 'Bu=1'] <- FALSE
df$Actual[df$A == '0' & df$E == '1' & df$node3 == 'Au=1'] <- FALSE
df$Actual[df$B == '0' & df$E == '1' & df$node3 == 'Bu=1'] <- FALSE


# Now this goes later to go into the likelihood function

# write this as csv in case need it later - 576 rows because: 3 pgroups x 12 trialtypes x 4 nodes x 4 prior possible settings of unobserved variables
save(mp, df, file = here('Data', 'modelData', 'goOptim.rda'))
