library(dplyr)
library(ggplot2)
library(binom)
library(lme4)
setwd("C:/Users/s03an7/Documents/GitHub/linesReward")
cbPalette <- c("#E69F00", "#56B4E9")
fixdat = readRDS(file="data/processedFixData.Rda")
subjectsToRemove = c(22,19,12)# 22 and 19 accuracy on homogenous trials below 50%, 12 RT on homogenous trials over 8s
# remove unwanted participants
fixdat$subj = as.factor(fixdat$subj)
fixdat = (fixdat[!(fixdat$subj%in% subjectsToRemove),])
fixdat$subj = factor(fixdat$subj)
fixdat$incentive = as.factor(fixdat$incentive)

# classify every fixation as homo (right), central, or hetro (left)
centralWidth = 64 #change to 1 visual degree
fixdat$side = 'central'
fixdat$fixX = fixdat$fixXflipped+512
fixdat$side[which(fixdat$fixX <(512-centralWidth/2))] = "hetro"
fixdat$side[which(fixdat$fixX >(512+centralWidth/2))] = "homo"
fixdat$side = as.factor(fixdat$side)

#look at accuracte trials only
fixdat = fixdat[which(fixdat$acc==1),]

aggData = (filter(fixdat, 
  side!="central", 
  fixNum<7, 
  fixNum>1, 
  targSideRel=="absent") 
  %>% group_by(fixNum, incentive,block)
    %>% summarise(
     propHetro=mean(side=="hetro"), 
     nTrials=length(trialNum),
     lower = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))
aggregateFix=aggregate(data=aggData, propHetro ~ subj + block+incentive, FUN="mean")
write.csv(aggregateFix, "data/aggregateProportion.txt", row.names=F)



plt = ggplot(aggData, aes(x=fixNum, y=propHetro, ymin=lower, ymax=upper, colour=block))
plt = plt + geom_point() + geom_path() + geom_errorbar()
plt = plt + theme_bw() + facet_wrap(~incentive, ncol=2)
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,3,4,5,6))
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side",limits=c(0,1))
plt = plt + labs(color="block")+scale_color_manual(labels = c("first", "second"),values = cbPalette ) 
ggsave("plots/FixXsideByFixNum.pdf", width=5, height=3.5)
ggsave("plots/FixXsideByFixNum.jpg", width=5, height=3.5)
####now the same but mean for the two groups
aggData = (filter(fixdat, 
  side!="central", 
  fixNum<7, 
  fixNum>1, 
  targSideRel=="absent") 
  %>% group_by(fixNum, version,order)
    %>% summarise(
     propHetro=mean(side=="hetro"), 
     nTrials=length(trial),
     lower = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))


plt = ggplot(aggData, aes(x=fixNum, y=propHetro, ymin=lower, ymax=upper, colour=version))
plt = plt + geom_point() + geom_path() + geom_errorbar()
plt = plt + theme_bw() + facet_wrap(~order, nrow=2)
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,3,4,5))
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side")
plt = plt + labs(color="condition")+scale_color_manual(labels = c("Timed", "Untimed"),values = cbPalette )
ggsave("plots/meanFixSide.pdf", width=5, height=3)
ggsave("plots/meanFixSide.jpg", width=5, height=3)





# get mean person plot
aggData2 = (filter(fixdat, fixNum<6) 
  %>% group_by(fixNum, version, completed) 
    %>% summarise(
     propHetro=mean(side=="hetro"),
     mPropHetro=mean(propHetro), 
     stddev = sd(propHetro),
     stderr=stddev/sqrt(12),
    lower=mPropHetro-1.96*stderr,
    upper=mPropHetro+1.96*stderr))

plt = ggplot(aggData2, aes(x=fixNum,y=mPropHetro, ymin=lower, ymax=upper))
plt = plt + geom_path() + geom_errorbar()# + geom_hline(y=0.5)
plt = plt + theme_bw() +facet_grid(completed~version)
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side", breaks=c(0,0.5,1), limits=c(0,1))
plt = plt + scale_x_continuous('fixation number', breaks=c(2,4,6,8,10))
ggsave("plots/meanPersonSide.pdf", width=4, height=4)
ggsave("plots/meanPersonSide.jpg",dpi=600, width=6, height=4)

####now mean position on horizontal axis
fixdat$fixX = fixdat$fixX - 512
fxdat1 = filter(fixdat, 
	targSide=="absent") 
xdat1 = (fxdat1 
		%>% group_by(fixNum, subj,version, completed) 
		%>% summarise(
			meanX=mean(fixX), 
			nFix=length(fixX)))
# remove entries with fewer than 8 fixations
rtdatAcc = rtdat[which(rtdat$acc==1),]
fixAnalysis=filter(xdat1, nFix>=8)
fixAnalysis = fixAnalysis[which(fixAnalysis$fixNum>1 & fixAnalysis$fixNum<6 ),]
aggregateFix=aggregate(data=fixAnalysis, meanX ~ subj + completed, FUN="mean")
write.csv(aggregateFix, "data/aggregateFix.txt", row.names=F)

xdat1 = filter(xdat1, nFix>=8, fixNum<=6)
plt = ggplot(xdat1, aes(y=meanX, x=fixNum, colour=completed))
plt = plt + geom_point(aes(group=subj),position=position_jitter(height=0.01, width=0.1))
plt = plt + geom_smooth(se=F)+facet_grid(~version)
plt = plt + scale_y_continuous(name="mean x position of fixation", expand=c(0,0), limits=c(-300,300))
plt = plt + scale_x_continuous(name="fixation number",breaks=c(1,2,3,4,5,6,7,8), expand=c(0,0.01))
plt = plt + scale_color_manual(labels = c("First", "Second"),values = cbPalette )
ggsave("plots/meanHorizontalPos.pdf", width=5, height=3)
ggsave("plots/meanHorizontalPos.jpg", width=5, height=3)

#########calculate test re-test reliability
fixdat$fixX = fixdat$fixX - 512
fxdat1 = filter(fixdat, 
	targSide=="absent",
      fixNum<=8) 
meanX =aggregate(data=fxdat1, fixX ~ subj + version + completed, FUN="mean")
write.csv(meanX, "data/meanXreliability.txt", row.names=F)











#  now we want to compare the strategy for trials which timed out, v those that didn't.
fixdat$timedOut = FALSE
for (s in levels(fixdat$subj))
{
  for (t in levels(fixdat$trial))
  {
    
   
    trlDat = filter(fixdat, subj==s, trial==t, version=="T")
if (nrow(trlDat)>0)
      {fixdat$timedOut[which(fixdat$subj==s & fixdat$trial==t, version=="T")] = rep(max(trlDat$fixOn)>4000, nrow(trlDat))}
}
}


aggData = (filter(fixdat, version=="T", side!="central", fixNum<6, fixNum>1, targSide=="absent") 
  %>% group_by(fixNum, subj, timedOut) 
    %>% summarise(
     propHetro=mean(side=="hetro"), 
     nTrials=length(trial),
     lower = binom.confint(propHetro*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(propHetro*nTrials,nTrials, method='wilson')$upper))

plt = ggplot(aggData, aes(x=fixNum, y=propHetro, ymin=lower, ymax=upper, colour=timedOut))
plt = plt + geom_point() + geom_path() + geom_errorbar()
plt = plt + theme_bw() + facet_wrap(~subj, nrow=2)
plt = plt + scale_x_continuous(name="fixation number", breaks=c(2,4,6,8,10))
plt = plt + scale_y_continuous(name="proportion of fixations to heterogeneous side")
ggsave("FixXtimedOut.pdf", width=9, height=4)


aggregate(trial ~ subj+targSide)
