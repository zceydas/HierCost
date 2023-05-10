##Supplement including WM
library(Matrix) 
library(lme4) 
library(lmerTest) 
library(MuMIn)
library(glm2)
library(MASS)
library(rJava)
library(glmulti)
library(leaps)

####

lmer.glmulti <- function (formula, data, random = "", ...) { 
  lmer(paste(deparse(formula), random), data = data, REML=F, ...) 
} 

setMethod('getfit', 'merMod', function(object, ...) {
  summ=summary(object)$coef
  summ1=summ[,1:2]
  if (length(dimnames(summ)[[1]])==1) {
    summ1=matrix(summ1, nr=1, dimnames=list(c(("Intercept")),c("Estimate","Std.    Error")))
  }
  cbind(summ1, df=rep(10000,length(fixef(object))))
})

lmer.glmulti <- function (formula, data, random = "", ...) { 
  lmer(paste(deparse(formula), random), data = data, REML=F, ...) 
} 

rm(list=ls()) # removes all variables from the workspace :)

branch_supp <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/SupplementWorkingMem_022522.csv')
comb_supp <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/Combined_SupplementWorkingMem_022622.csv')


################################################################### 
########## Branch Analyses ###### 
################################################################### 

numcols <- c(6)
branch_supp_2 <- branch_supp
branch_supp_2[,numcols] <- scale(branch_supp_2[,numcols])
branch_supp_2$SelectionErr <- 1-branch_supp_2$SelectionErr

#Branch: iterative search
fitwell1 <- glmulti(selectionrate ~ 1 + Abstraction + SelectionRT + SelectionErr + WorkingMem, data=branch_supp, 
                    level = 2, fitfunc = lmer.glmulti,confsetsize = 1024, random = "+(1+Abstraction + SelectionRT + SelectionErr + WorkingMem ||Subject)", method = "h") 

summary(fitwell1)

plot(fitwell1, type = "s")
plot(fitwell1, type = "p")

ab <- coef.glmulti(fitwell1)
View(ab)

################################################################### 
########## ANOVAs ###### 
################################################################### 

term1_b <- lmer(selectionrate ~ 1 + Abstraction + (Abstraction ||Subject), data = branch_supp_2, REML=F)
term12_b <- lmer(selectionrate ~ 1 + Abstraction + WorkingMem + (Abstraction + WorkingMem ||Subject), data = branch_supp_2, REML=F)

anova(term1_b)
anova(term1_b, term12_b, test='F')



################################################################### 
########## CIs ###### 
################################################################### 

branchsuppCI <- as.data.frame(ab)
branchsuppCI <- data.frame(Estimate=branchsuppCI$Est, SE=sqrt(branchsuppCI$Uncond), Importance=branchsuppCI$Importance, row.names=row.names(branchsuppCI))
branchsuppCI$z <- branchsuppCI$Estimate / branchsuppCI$SE
branchsuppCI$p <- 2*pnorm(abs(branchsuppCI$z), lower.tail=FALSE)
names(branchsuppCI) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
branchsuppCI$ci.lb <- branchsuppCI[[1]] - qnorm(.975) * branchsuppCI[[2]]
branchsuppCI$ci.ub <- branchsuppCI[[1]] + qnorm(.975) * branchsuppCI[[2]]
branchsuppCI <- branchsuppCI[order(branchsuppCI$Importance, decreasing=TRUE), c(1,2,4:7,3)]
round(branchsuppCI, 4)









################################################################### 
########## Combined Analyses ###### 
################################################################### 

#Rescale Response columns
comb_supp_2 <- comb_supp[which(comb_supp$Experiment=='Response'),]
comb_supp_2$SelectionRT <- scale(comb_supp_2$SelectionRT, center=T, scale=T)

#Rescale Feature columns
comb_supp_3 <- comb_supp[which(comb_supp$Experiment=='Feature'),]
comb_supp_3$SelectionRT <- scale(comb_supp_3$SelectionRT, center=T, scale=T)

#Rescale Branch columns
comb_supp_4 <- comb_supp[which(comb_supp$Condition=='Branch'),]
comb_supp_4$SelectionRT <- scale(comb_supp_4$SelectionRT, center=T, scale=T)

#And rebind with scaled RTs
comb_supp <- rbind(comb_supp_2, comb_supp_3, comb_supp_4)
comb_supp_2 <- comb_supp
comb_supp_2$SelectionErr <- 1-comb_supp_2$SelectionErr

#Branch: iterative search
fitwell2 <- glmulti(selectionrate ~ 1 + Abstraction + SelectionRT + SelectionErr + WorkingMem, data=comb_supp_2, 
                    level = 2, fitfunc = lmer.glmulti,confsetsize = 1024, random = "+(1+Abstraction + SelectionRT + SelectionErr + WorkingMem ||Subject)", method = "h") 

summary(fitwell2)

plot(fitwell2, type = "s")
plot(fitwell2, type = "p")

bc <- coef.glmulti(fitwell2)
View(bc)

################################################################### 
########## ANOVAs ###### 
################################################################### 

term1_c <- lmer(selectionrate ~ 1 + SelectionErr + (SelectionErr ||Subject), data = comb_supp_2, REML=F)
term12_c <- lmer(selectionrate ~ 1 + SelectionErr + Abstraction + (Abstraction + SelectionErr ||Subject), data = comb_supp_2, REML=F)

anova(term1_c)
anova(term1_c, term12_b, test='F')



################################################################### 
########## CIs ###### 
################################################################### 

comb_suppCI <- as.data.frame(bc)
comb_suppCI <- data.frame(Estimate=comb_suppCI$Est, SE=sqrt(comb_suppCI$Uncond), Importance=comb_suppCI$Importance, row.names=row.names(comb_suppCI))
comb_suppCI$z <- comb_suppCI$Estimate / comb_suppCI$SE
comb_suppCI$p <- 2*pnorm(abs(comb_suppCI$z), lower.tail=FALSE)
names(comb_suppCI) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
comb_suppCI$ci.lb <- comb_suppCI[[1]] - qnorm(.975) * comb_suppCI[[2]]
comb_suppCI$ci.ub <- comb_suppCI[[1]] + qnorm(.975) * comb_suppCI[[2]]
comb_suppCI <- comb_suppCI[order(comb_suppCI$Importance, decreasing=TRUE), c(1,2,4:7,3)]
round(comb_suppCI, 4)

