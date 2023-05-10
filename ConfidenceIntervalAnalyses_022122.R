library(Matrix) 
library(lme4) 
library(lmerTest) 
library(MuMIn)
library(glm2)
library(MASS)
library(rJava)
library(glmulti)
library(leaps)

#Change this to wherever you have the datafile
hier4CI <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/HierCost_SRsRTsAcc_022122.csv')
branch4CI <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/Branch_SRsRTsAcc_022122.csv')
comb4CI <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/Combined_SRsRTsAcc_022122.csv')

###################################################################
########## Functions for use in all ######
###################################################################

lmer.glmulti <- function (formula, data, random = "", ...) {
  lmer(paste(deparse(formula), random), data = data, REML=F, ...)
}

#setMethod('getfit', 'merMod', function(object, ...) {
#  summ=summary(object)$coef
#  summ1=summ[,1:2]
#  if (length(dimnames(summ)[[1]])==1) {
#    summ1=matrix(summ1, nr=1, dimnames=list(c(("Intercept")),c("Estimate","Std.    Error")))
#  }
#  cbind(summ1, df=rep(10000,length(fixef(object))))
#})
#PULL from updated_analysis 

###################################################################
########## Run for Hier, in-person ######
###################################################################

#GLMulti analyses to choose the best model
#Rescale Response columns
hier4CI2 <- hier4CI[which(hier4CI$Condition %in% c('HR1', 'HR2', 'HR4')),]
hier4CI2$PracticeRT <- scale(hier4CI2$PracticeRT, center=T, scale=T)
hier4CI2$SelectionRT <- scale(hier4CI2$SelectionRT, center=T, scale=T)

#Rescale Feature columns
hier4CI3 <- hier4CI[which(hier4CI$Condition %in% c('HF1', 'HF2', 'HF4')),]
hier4CI3$PracticeRT <- scale(hier4CI3$PracticeRT, center=T, scale=T)
hier4CI3$SelectionRT <- scale(hier4CI3$SelectionRT, center=T, scale=T)

#And rebind with scaled RTs
hier4CI <- rbind(hier4CI2, hier4CI3)
hier4CI$SelectionErr <- 1-hier4CI$SelectionErr

fitwellhier_2 <- glmulti(SelectionRate ~ 1 + Abstraction + Level + SelectionRT + SelectionErr, data=hier4CI, 
                       level = 2, fitfunc = lmer.glmulti,confsetsize = 1024, random = "+(1+Abstraction + Level + SelectionRT + SelectionErr ||Subject)", method = "h") 
#Alternatively, once I've run it elsewhere, load it here: 
#fitwellhier_2 <- readRDS("~/Desktop/Brown/FinishingHierCost/FinalSelection/hierinpersonfullglmulti")

hier4CI_coef <- coef.glmulti(fitwellhier_2)

hierCI <- as.data.frame(coef(fitwellhier_2))
hierCI <- data.frame(Estimate=hierCI$Est, SE=sqrt(hierCI$Uncond), Importance=hierCI$Importance, row.names=row.names(hierCI))
hierCI$z <- hierCI$Estimate / hierCI$SE
hierCI$p <- 2*pnorm(abs(hierCI$z), lower.tail=FALSE)
names(hierCI) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
hierCI$ci.lb <- hierCI[[1]] - qnorm(.975) * hierCI[[2]]
hierCI$ci.ub <- hierCI[[1]] + qnorm(.975) * hierCI[[2]]
hierCI <- hierCI[order(hierCI$Importance, decreasing=TRUE), c(1,2,4:7,3)]
round(hierCI, 4)


###################################################################
########## Run for Branch, in-person ######
###################################################################
numcols <- c(5,7)
branch4CI2 <- branch4CI
branch4CI2[,numcols] <- scale(branch4CI2[,numcols])
branch4CI22$SelectionErr <- 1-branch4CI22$SelectionErr

#GLMulti analyses to choose the best model
fitwellbranch_2 <- glmulti(selectionrate ~ 1 + Abstraction + SelectionRT + SelectionErr, data=branch4CI2, 
                         level = 2, fitfunc = lmer.glmulti,confsetsize = 64, random = "+(1+Abstraction + SelectionRT + SelectionErr ||Subject)", method = "h") 
#Alternatively, once I've run it elsewhere, load it here: 
#fitwellbranch_2 <- readRDS("~/Desktop/Brown/FinishingHierCost/FinalSelection/branchinpersonfullglmulti")

branch4CI_coef <- coef.glmulti(fitwellbranch_2)

branchCI <- as.data.frame(coef(fitwellbranch_2))
branchCI <- data.frame(Estimate=branchCI$Est, SE=sqrt(branchCI$Uncond), Importance=branchCI$Importance, row.names=row.names(branchCI))
branchCI$z <- branchCI$Estimate / branchCI$SE
branchCI$p <- 2*pnorm(abs(branchCI$z), lower.tail=FALSE)
names(branchCI) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
branchCI$ci.lb <- branchCI[[1]] - qnorm(.975) * branchCI[[2]]
branchCI$ci.ub <- branchCI[[1]] + qnorm(.975) * branchCI[[2]]
branchCI <- branchCI[order(branchCI$Importance, decreasing=TRUE), c(1,2,4:7,3)]
round(branchCI, 4)

###################################################################
########## Run for Combined, in-person ######
###################################################################

#Rescale Response columns
comb4CI2 <- comb4CI[which(comb4CI$Experiment=='Response'),]
comb4CI2$PracticeRT <- scale(comb4CI2$PracticeRT, center=T, scale=T)
comb4CI2$SelectionRT <- scale(comb4CI2$SelectionRT, center=T, scale=T)

#Rescale Feature columns
comb4CI3 <- comb4CI[which(comb4CI$Experiment=='Feature'),]
comb4CI3$PracticeRT <- scale(comb4CI3$PracticeRT, center=T, scale=T)
comb4CI3$SelectionRT <- scale(comb4CI3$SelectionRT, center=T, scale=T)

#Rescale Branch columns
comb4CI4 <- comb4CI[which(comb4CI$Condition=='Branch'),]
comb4CI4$PracticeRT <- scale(comb4CI4$PracticeRT, center=T, scale=T)
comb4CI4$SelectionRT <- scale(comb4CI4$SelectionRT, center=T, scale=T)

#And rebind with scaled RTs
comb4CI <- rbind(comb4CI2, comb4CI3, comb4CI4)
comb4CI$SelectionErr <- 1-comb4CI$SelectionErr
#GLMulti analyses to choose the best model
fitwellcomb_2 <- glmulti(selectionrate ~ 1 + Abstraction + SelectionRT + SelectionErr, data=comb4CI, 
                           level = 2, fitfunc = lmer.glmulti,confsetsize = 64, random = "+(1+Abstraction + SelectionRT + SelectionErr ||Subject)", method = "h") 
#Alternatively, once I've run it elsewhere, load it here: 
#fitwellcomb_2 <- readRDS("~/Desktop/Brown/FinishingHierCost/FinalSelection/combinpersonfullglmulti")

comb4CI_coef <- coef.glmulti(fitwellcomb_2)
comb4CI_coef_2 <-comb4CI_coef 

combCI <- as.data.frame(coef(fitwellcomb_2))
combCI <- data.frame(Estimate=combCI$Est, SE=sqrt(combCI$Uncond), Importance=combCI$Importance, row.names=row.names(combCI))
combCI$z <- combCI$Estimate / combCI$SE
combCI$p <- 2*pnorm(abs(combCI$z), lower.tail=FALSE)
names(combCI) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
combCI$ci.lb <- combCI[[1]] - qnorm(.975) * combCI[[2]]
combCI$ci.ub <- combCI[[1]] + qnorm(.975) * combCI[[2]]
combCI <- combCI[order(combCI$Importance, decreasing=TRUE), c(1,2,4:7,3)]
round(combCI, 4)




