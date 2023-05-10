library(reshape2)
library(ez)
library(sjstats)

####Learning Reaction Time Analysis 
rtdata_prac <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/HierCostRTs_Practice_022122.csv')
rtdata_prac$Phase <- 'Practice'
rtdata_selec <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/HierCostRTs_022122.csv')
rtdata_selec$Phase <- 'Selection'

rtdata <- rbind(rtdata_prac, rtdata_selec)
#melt into a long version
d2 <- melt(rtdata, id.vars=c('ID', 'Phase'), variable.name = 'condition', value.name = 'RT')
#Assign abstraction by condition
d2$Abstraction <- ifelse(d2$condition=='HR1','0',
                         ifelse(d2$condition=='HR2' | d2$condition=='HR4' | d2$condition=='HF1', 1,
                                ifelse(d2$condition=='HF2' | d2$condition=='HF4', 2, 'else')))
#Assign number of options by condition
d2$Level <- ifelse(d2$condition=='HR1'  | d2$condition=='HF1' ,1,
                         ifelse(d2$condition=='HR2' | d2$condition == 'HF2', 2,
                                ifelse(d2$condition=='HR4' | d2$condition=='HF4', 4, 'else')))
#First paragraph: ANOVA for RTs
summary(aov(RT~Abstraction*Phase*Level + (1|ID), d2))
rtaov <- aov(RT~Abstraction*Phase*Level + (1|ID), d2)
eta_sq(rtaov, partial=T)


####Error Rate Analysis 
e_prac <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/HierCostAcc_Practice_022122.csv')
e_prac$Phase <- 'Practice'
e_selec <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/HierCostAcc_022122.csv')
e_selec$Phase <- 'Selection'

e <- rbind(e_prac, e_selec)
e2 <- melt(e, id.vars=c('ID', 'Phase'), variable.name = 'condition', value.name = 'Acc')

#e2 <- e2[which(!is.na(e2$Acc)),]
e2$Abstraction <- ifelse(e2$condition=='HR1','0',
                         ifelse(e2$condition=='HR2' | e2$condition=='HR4' | e2$condition=='HF1', 1,
                                ifelse(e2$condition=='HF2' | e2$condition=='HF4', 2, 'else')))
e2$Level <- ifelse(e2$condition=='HR1' |e2$condition=='HF1'  ,1,
                   ifelse(e2$condition=='HR2' | e2$condition=='HF2', 2,
                          ifelse(e2$condition=='HR4' | e2$condition=='HF4', 4, 'else')))


summary(aov(Acc~Abstraction*Phase*Level + (1|ID), e2))
eaov <- aov(Acc~Abstraction*Phase*Level + (1|ID), e2)
eta_sq(eaov, partial=T)

####Selection Rate
srdata <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/HierCostSRs_022122.csv')
srdata$ID <- seq.int(nrow(srdata))
srdata_long <- melt(srdata, id.vars=c('ID'), variable.name = 'condition', value.name = 'SR')
srdata_long$Abstraction <- ifelse(srdata_long$condition=='hr1','0',
                         ifelse(srdata_long$condition=='hr2' | srdata_long$condition=='hr4' | srdata_long$condition=='hf1', 1,
                                ifelse(srdata_long$condition=='hf2' | srdata_long$condition=='hf4', 2, 'else')))
srdata_long$Level <- ifelse(srdata_long$condition=='hr1' |srdata_long$condition=='hf1'  ,1,
                   ifelse(srdata_long$condition=='hr2' | srdata_long$condition=='hf2', 2,
                          ifelse(srdata_long$condition=='hr4' | srdata_long$condition=='hf4', 4, 'else')))

summary(aov(SR~Abstraction*Level + (1|ID), srdata_long))
sr_aov <- aov(SR~Abstraction*Level + (1|ID), srdata_long)
eta_sq(sr_aov, partial=T)



##Reported pairwise analyses
library(effsize)
hierinperson <- read.csv('~/Desktop/Brown/FinishingHierCost/updateddataformodeling/UpdatedAbstractionHierModels.csv')

t.test(hierinperson[which(hierinperson$Condition=='hR1'), 'Selection.Rate'], 
       hierinperson[which(hierinperson$Condition=='hR2'), 'Selection.Rate'], paired=T)
# adjusted p-values for each: t-test above, $p.value and multiply by 7 (number of t-tests)
cohen.d(hierinperson[which(hierinperson$Condition=='hR1'), 'Selection.Rate'], 
        hierinperson[which(hierinperson$Condition=='hR2'), 'Selection.Rate'], hedges.correction = T, paired=T)

t.test(hierinperson[which(hierinperson$Condition=='hR1'), 'Selection.Rate'], 
       hierinperson[which(hierinperson$Condition=='hR4'), 'Selection.Rate'], paired=T)
cohen.d(hierinperson[which(hierinperson$Condition=='hR1'), 'Selection.Rate'], 
        hierinperson[which(hierinperson$Condition=='hR4'), 'Selection.Rate'], hedges.correction = T, paired=T)

t.test(hierinperson[which(hierinperson$Condition=='hR1'), 'Selection.Rate'], 
       hierinperson[which(hierinperson$Condition=='hF1'), 'Selection.Rate'], paired=T)
cohen.d(hierinperson[which(hierinperson$Condition=='hR1'), 'Selection.Rate'], 
        hierinperson[which(hierinperson$Condition=='hF1'), 'Selection.Rate'], hedges.correction = T, paired=T)

t.test(hierinperson[which(hierinperson$Condition=='hF1'), 'Selection.Rate'], 
       hierinperson[which(hierinperson$Condition=='hF2'), 'Selection.Rate'], paired=T)
cohen.d(hierinperson[which(hierinperson$Condition=='hF1'), 'Selection.Rate'], 
        hierinperson[which(hierinperson$Condition=='hF2'), 'Selection.Rate'], hedges.correction = T, paired=T)

t.test(hierinperson[which(hierinperson$Condition=='hF1'), 'Selection.Rate'], 
       hierinperson[which(hierinperson$Condition=='hF4'), 'Selection.Rate'], paired=T)
cohen.d(hierinperson[which(hierinperson$Condition=='hF1'), 'Selection.Rate'], 
        hierinperson[which(hierinperson$Condition=='hF4'), 'Selection.Rate'], hedges.correction = T, paired=T)


t.test(hierinperson[which(hierinperson$Condition=='hR2'), 'Selection.Rate'], 
       hierinperson[which(hierinperson$Condition=='hF1'), 'Selection.Rate'], paired=T)
cohen.d(hierinperson[which(hierinperson$Condition=='hR2'), 'Selection.Rate'], 
        hierinperson[which(hierinperson$Condition=='hF1'), 'Selection.Rate'], hedges.correction = T, paired=T)


t.test(hierinperson[which(hierinperson$Condition=='hR2'), 'Selection.Rate'], 
       hierinperson[which(hierinperson$Condition=='hR4'), 'Selection.Rate'], paired=T)
cohen.d(hierinperson[which(hierinperson$Condition=='hR2'), 'Selection.Rate'], 
        hierinperson[which(hierinperson$Condition=='hR4'), 'Selection.Rate'], hedges.correction = T, paired=T)

t.test(hierinperson[which(hierinperson$Condition=='hF2'), 'Selection.Rate'], 
       hierinperson[which(hierinperson$Condition=='hF4'), 'Selection.Rate'], paired=T)
cohen.d(hierinperson[which(hierinperson$Condition=='hF2'), 'Selection.Rate'], 
        hierinperson[which(hierinperson$Condition=='hF4'), 'Selection.Rate'], hedges.correction = T, paired=T)

#Novel condition t-test
hiernovelpair <- read.csv('~/Desktop/Brown/FinishingHierCost/NovelStimuliSRs_021522.csv')
t.test(hiernovelpair$R3, hiernovelpair$F3, paired=T)
cohen.d(hiernovelpair$R3, hiernovelpair$F3, hedges.correction = T, paired=T)

