library(reshape2)
library(ez)
library(ggplot2)

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


branchrtg <- ggplot(d2branch, aes(x=condition, y=RT, fill=Phase)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylim(0,2) +
  ylab('Reaction Time') +
  xlab(' ') +
  geom_boxplot()
branchrtg



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


brancherrg <- ggplot(e2_branch, aes(x=condition, y=(1-Acc), fill=Phase)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylim(0,.5) +
  ylab('Error Rate') +
  xlab(' ') +
  geom_boxplot()
brancherrg
