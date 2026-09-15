################################################################################
########### Behavioral comparisons analysis for new reporting subsection ##############
#################################################################################

library(tidyverse)
library(here)

load(here('Data', 'modelData', 'fitforplot4par.rda'))

# A=.1,Au=.5,B=.8,Bu=.5
# A=.5,Au=.1,B=.5,Bu=.8
# A=.1,Au=.7,B=.8,Bu=.5

# Filter each time for using n
df1 <- df %>%
  filter(A == '1', B == '0', E == '1', pgroup == 'A=.1,Au=.5,B=.8,Bu=.5')
sum(df1$n)

chisq.test(c(37, 9), p = c(.5, .5))
print(chisq_test_no) # x 3, p .08, no difference
