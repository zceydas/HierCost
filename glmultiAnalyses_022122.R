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

setMethod('getfit', 'merMod', function(object, ...) {
    summ=summary(object)$coef
    summ1=summ[,1:2]
    if (length(dimnames(summ)[[1]])==1) {
        summ1=matrix(summ1, nr=1, dimnames=list(c(("Intercept")),c("Estimate","Std.    Error")))
    }
    cbind(summ1, df=rep(10000,length(fixef(object))))
})



rm(list=ls()) # removes all variables from the workspace :)

d <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/HierCost_SRsRTsAcc_022122.csv')
b <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/Branch_SRsRTsAcc_022122.csv')
c <- read.csv('~/Desktop/Brown/FinishingHierCost/HopefullyTrulyFinalSelection_022122/Combined_SRsRTsAcc_022122.csv')


################################################################### 
########## Create lmer wrapper to paste random variable to all models ###### 
################################################################### 

lmer.glmulti <- function (formula, data, random = "", ...) { 
lmer(paste(deparse(formula), random), data = data, REML=F, ...) 
} 

#Rescale Response columns
d2 <- d[which(d$Condition %in% c('HR1', 'HR2', 'HR4')),]
d2$PracticeRT <- scale(d2$PracticeRT, center=T, scale=T)
d2$SelectionRT <- scale(d2$SelectionRT, center=T, scale=T)

#Rescale Feature columns
d3 <- d[which(d$Condition %in% c('HF1', 'HF2', 'HF4')),]
d3$PracticeRT <- scale(d3$PracticeRT, center=T, scale=T)
d3$SelectionRT <- scale(d3$SelectionRT, center=T, scale=T)

#And rebind with scaled RTs
d <- rbind(d2, d3)
d$SelectionErr <- 1-d$SelectionErr

#Hier: iterative search: using Selection Data
fitwell2 <- glmulti(SelectionRate ~ 1 + Abstraction + Level + SelectionRT + SelectionErr, data=d, 
                    level = 2, fitfunc = lmer.glmulti,confsetsize = 1024, random = "+(1+Abstraction + Level + SelectionRT + SelectionErr ||Subject)", method = "h") 

summary(fitwell2)

plot(fitwell2, type = "s")
plot(fitwell2, type = "p")

setMethod('getfit', 'merMod', function(object, ...) {
    summ=summary(object)$coef
    summ1=summ[,1:2]
    if (length(dimnames(summ)[[1]])==1) {
        summ1=matrix(summ1, nr=1, dimnames=list(c(("Intercept")),c("Estimate","Std.    Error")))
    }
    cbind(summ1, df=rep(10000,length(fixef(object))))
})
ab_2 <- coef.glmulti(fitwell2)
View(ab_2)



#Hier: iterative search: using Practice Data
fitwell1 <- glmulti(SelectionRate ~ 1 + Abstraction + Level + PracticeRT + PracticeErr, data=d, 
 level = 2, fitfunc = lmer.glmulti,confsetsize = 1024, random = "+(1+Abstraction + Level + PracticeRT + PracticeErr ||Subject)", method = "h") 

summary(fitwell1)

plot(fitwell1, type = "s")
plot(fitwell1, type = "p")

ab <- coef.glmulti(fitwell1)
View(ab)




##Check model VIFs
hierfullmodel <- lmer(SelectionRate ~ 1 + Abstraction + Level + SelectionRT + SelectionErr +
                          + (1+PracticeRT+Abstraction + Level + SelectionRT + SelectionErr||Subject), data=d,)


#############################Branch
#Branch
#How to rescale
numcols <- c(5,7)
b2 <- b
b2[,numcols] <- scale(b2[,numcols])
b2$SelectionErr <- 1-b2$SelectionErr

#Run with Selection data
fitwell_1 <- glmulti(selectionrate ~ 1 + Abstraction + SelectionRT + SelectionErr, data=b2, 
                     level = 2, fitfunc = lmer.glmulti,confsetsize = 1000, random = "+(1+Abstraction + SelectionRT + SelectionErr ||Subject)", method = "h") 

summary(fitwell_1)

plot(fitwell_1, type = "s")
plot(fitwell_1, type = "p")

a_2 <- coef.glmulti(fitwell_1)
View(a_2)

#Rerun with practice data

fitwell <- glmulti(selectionrate ~ 1 + Abstraction + PracticeRT + PracticeErr, data=b2, 
                   level = 2, fitfunc = lmer.glmulti,confsetsize = 1000, random = "+(1+Abstraction + PracticeRT + PracticeErr ||Subject)", method = "h") 

summary(fitwell)

plot(fitwell, type = "s")
plot(fitwell, type = "p")

a <- coef.glmulti(fitwell)
View(a)

####################################Combined
#Rescale Response columns
c2 <- c[which(c$Experiment=='Response'),]
c2$PracticeRT <- scale(c2$PracticeRT, center=T, scale=T)
c2$SelectionRT <- scale(c2$SelectionRT, center=T, scale=T)

#Rescale Feature columns
c3 <- c[which(c$Experiment=='Feature'),]
c3$PracticeRT <- scale(c3$PracticeRT, center=T, scale=T)
c3$SelectionRT <- scale(c3$SelectionRT, center=T, scale=T)

#Rescale Branch columns
c4 <- c[which(c$Condition=='Branch'),]
c4$PracticeRT <- scale(c4$PracticeRT, center=T, scale=T)
c4$SelectionRT <- scale(c4$SelectionRT, center=T, scale=T)

#And rebind with scaled RTs
c <- rbind(c2, c3, c4)
c2 <- c

c2$SelectionErr <- 1-c2$SelectionErr
fitwell_c <- glmulti(selectionrate ~ 1 + Abstraction + SelectionRT + SelectionErr, data=c2,
                         level = 2, fitfunc = lmer.glmulti,confsetsize = 64, random = "+(1+Abstraction + SelectionRT + SelectionErr ||Subject)", method = "h") 

summary(fitwell_c)
plot(fitwell_c, type = "p")
cc <- coef.glmulti(fitwell_c)
View(cc)
