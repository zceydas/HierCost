
#########################
###In-Person ANOVAs #####
#########################

#Hier
hierinperson <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/HierCost_SRsRTsAcc_022122.csv')
hierinperson$SelectionErr <- 1-hierinperson$SelectionErr
term1_h <- lmer(SelectionRate ~ 1 + Abstraction + (Abstraction ||Subject), data = na.omit(hierinperson), REML=F)
term12_h <- lmer(SelectionRate ~ 1  + Abstraction + SelectionErr + ( Abstraction + SelectionErr||Subject), data = na.omit(hierinperson), REML=F)
anova(term1_h, term12_h, test='F')

#branch
branchinperson <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/Branch_SRsRTsAcc_022122.csv')
term1_b <- lmer(selectionrate ~ 1 + Abstraction + (Abstraction ||Subject), data = branchinperson, REML=F)
anova(term1_b)

#combined
#combinperson <- read.csv('~/Desktop/Brown/FinishingHierCost/updateddataformodeling/UpdatedCombinedAbstraction_101321.csv')
combinperson <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/Combined_SRsRTsAcc_022122.csv')
combinperson$SelectionErr <- 1-combinperson$SelectionErr
term1_c <- lmer(selectionrate ~ 1 + Abstraction + (Abstraction ||Subject), data = na.omit(combinperson), REML=F)
term12_c <- lmer(selectionrate ~ 1  + Abstraction + SelectionErr + (Abstraction + SelectionErr||Subject), data = na.omit(combinperson), REML=F)

anova(term1_c)
anova(term1_c, term12_c, test='F')


