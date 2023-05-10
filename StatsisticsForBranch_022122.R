library(reshape2)
library(ez)

####Reaction Time Analysis 
rtdatabranch_prac <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/BranchRTs_Practice_022122.csv')
rtdatabranch_prac$Phase <- 'Practice'
rtdatabranch_selec <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/BranchRTs_022122.csv')
rtdatabranch_selec$Phase <- 'Selection'

rtdatabranch <- rbind(rtdatabranch_prac, rtdatabranch_selec)
#melt into a long version
d2branch <- melt(rtdatabranch, id.vars=c('ID', 'Phase'), variable.name = 'condition', value.name = 'RT')
#Assign abstraction by condition
d2branch$Abstraction <- ifelse(d2branch$condition=='Control',2,
                         ifelse(d2branch$condition=='Delay', 2.5,
                                ifelse(d2branch$condition=='Restart', 3, 
                                       ifelse(d2branch$condition=='Dual', 3.5,'else'))))

#First paragraph: ANOVA for RTs
summary(aov(RT~Abstraction*Phase + (1|ID), d2branch))
rtaov <- aov(RT~Abstraction*Phase + (1|ID), d2branch)
eta_sq(rtaov, partial=T)

#For Tukey's tests (not reported, but good to have)
TukeyHSD(aov(RT~Abstraction + (1|ID), d2branch))


####Error Rate Analysis 
ebranch_prac <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/BranchAcc_Practice_022122.csv')
ebranch_prac$Phase <- 'Practice'
ebranch_selec <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/BranchAcc_022122.csv')
ebranch_selec$Phase <- 'Selection'

e_branch <- rbind(ebranch_prac, ebranch_selec)
e2_branch <- melt(e_branch, id.vars=c('ID', 'Phase'), variable.name = 'condition', value.name = 'Acc')
e2_branch$Abstraction <- ifelse(e2_branch$condition=='Control',2,
                               ifelse(e2_branch$condition=='Delay', 2.5,
                                      ifelse(e2_branch$condition=='Restart', 3, 
                                             ifelse(e2_branch$condition=='Dual', 3.5,'else'))))

summary(aov(Acc~Abstraction*Phase + (1|ID), e2_branch))
eaov_branch <- aov(Acc~Abstraction*Phase + (1|ID), e2_branch)
eta_sq(eaov_branch, partial=T)

TukeyHSD(aov(Acc~Abstraction + (1|ID), e2_branch))


####Selection Rate
srdatabranch <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/BranchSRs_022122.csv')

srdatabranch_long <- melt(srdatabranch, id.vars=c('ID'), variable.name = 'condition', value.name = 'SR')
srdatabranch_long$Abstraction <- ifelse(srdatabranch_long$condition=='Control',2,
                                ifelse(srdatabranch_long$condition=='Delay', 2.5,
                                       ifelse(srdatabranch_long$condition=='Restart', 3, 
                                              ifelse(srdatabranch_long$condition=='Dual', 3.5,'else'))))


summary(aov(SR~as.factor(Abstraction) + (1|ID), srdatabranch_long))
srb_aov <- aov(SR~as.factor(Abstraction) + (1|ID), srdatabranch_long)
eta_sq(srb_aov, partial=T)
TukeyHSD(srb_aov)

