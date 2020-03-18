library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(Rmisc)
library(lme4)

cbPalette <- c("#E69F00", "#56B4E9", "#B5CB8B")
setwd("C:/Users/s03an7/Documents/GitHub/LinesReward")
rtdat = readRDS(file="data/processedRTandAccData.Rda")
subjectsToRemove = c(22,19,12)#22 and 19 accuracy on homogenous trials below 50%, 12 RT on homogenous trials over 8s
# remove unwanted participants
#rtdat$subj = as.factor(rtdat$subj)
rtdat = (rtdat[!(rtdat$subj%in% subjectsToRemove),])
rtdat$subj = as.factor(rtdat$subj)
rtdat$incentive = factor((rtdat$incentive))
levels(rtdat$incentive) = c("flat","reward")
levels(rtdat$targSide) = c("Heterogeneous","Homogeneous","Absent")
levels(rtdat$block) = c("First","Second")
####looking at accuracy first
summaryAcc <- summarySE(rtdat, measurevar="acc", groupvars=c("block","incentive"))
accdat =aggregate(data=rtdat, acc ~subj + block + incentive, FUN="mean")
write.csv(accdat, "data/accAggregated.txt", row.names=F)

pd <- position_dodge(width = 0.5)
pAcc = ggplot(summaryAcc, aes(x=incentive, y=100*acc, fill=block)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
pAcc = pAcc + scale_y_continuous(name="Accuracy (%)",breaks=c(0,20,40,60,80,100)) + scale_x_discrete(name="Incentive",labels=c("Flat", "Reward"))+scale_fill_manual(name="Block", values=cbPalette) #+ facet_wrap(~targSide)
pAcc = pAcc + geom_errorbar(position=position_dodge(.9), aes(ymin=(acc-ci)*100,ymax=(acc+ci)*100),width=.5)
ggsave("plots/Accuracy2by2.pdf", width=6, height=4)
ggsave("plots/Accuracy2by2.jpg", width=6, height=4)

#####Now RT
#remowe incorrect trials
rtdat = rtdat[which(rtdat$acc==1),]
rtdat$RT=sqrt(rtdat$RT)
RTdat =aggregate(data=rtdat, RT ~ subj + block + incentive, FUN="median")
summaryRT <- summarySE(RTdat, measurevar="RT", groupvars=c("block","incentive"))
RTdat =aggregate(data=RTdat, RT ~subj+ block + incentive, FUN="mean")
#RTdat$RT=sqrt(RTdat$RT)
write.csv(RTdat, "data/RtAggregatedSQRT.txt", row.names=F)

pRT = ggplot(summaryRT, aes(x=incentive, y=RT, fill=block)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
pRT = pRT + scale_y_continuous(name="Reaction Time (s)") + scale_x_discrete(name="Reward",labels=c("Flat", "Reward"))+scale_fill_manual(name="Block", values=cbPalette)# + facet_wrap(~targSide)
pRT = pRT + geom_errorbar(position=position_dodge(.9), aes(ymin=(RT-ci),ymax=(RT+ci)),width=.5)


ggsave("plots/RT2by2sqrt.pdf", width=6, height=4)
ggsave("plots/RT2by2sqrt.jpg", width=6, height=4)

#not split by target side
RTdat =aggregate(data=rtdat, RT ~ subj + block + reward+targSide, FUN="median")
summaryRT <- summarySE(RTdat, measurevar="RT", groupvars=c("block","reward","targSide"))
RTdat =aggregate(data=rtdat, RT ~subj+ block + reward, FUN="mean")


pRT = ggplot(summaryRT, aes(x=reward, y=RT, fill=block)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
pRT = pRT + scale_y_continuous(name="Reaction Time (s)") + scale_x_discrete(name="reward",labels=c("Flat", "Reward"))+scale_fill_manual(name="block", values=cbPalette)+ facet_wrap(~targSide)
pRT = pRT + geom_errorbar(position=position_dodge(.9), aes(ymin=(RT-ci),ymax=(RT+ci)),width=.5)

 

