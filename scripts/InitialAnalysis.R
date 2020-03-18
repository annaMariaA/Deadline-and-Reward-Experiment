
library(dplyr)
# TODO: check subjects to replace!
setwd("C:/Users/s03an7/Documents/GitHub/LinesReward/")

# max fixation duration - remove trial if it is exceeded 
maxFixDur = 2000

# read in reaction time and acc data:

print("Processing RT and Acc data")
dat <- read.csv("data/rewardRT.txt", sep="\t",stringsAsFactors=FALSE)

#exclude the few error trials where x Fix==0

dat = (filter(dat, xFix>0,easySide!=" x" ))

dat = select(dat, subNum, block, trialNo,targPresent,targSide, easySide, RT,accuracy)

# Turn categorical data into factor
# one unusual entry with mean x at 0 and target present equals 120???? line deleted

dat$targPresent = as.factor(dat$targPresent)
dat$block = as.factor(dat$block)
dat$targSide= as.factor(dat$targSide)
levels(dat$targPresent) = c("absent", "present")
levels(dat$targSide) = c("left", "right", "absent")
dat$easySide = as.factor(dat$easySide)
levels(dat$easySide) = c("left", "right")

# refdefine targSide relative to easySide

dat$targSideRel = as.factor(as.character(dat$easySide) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("heterogenous", "homogenous", "absent")
dat$targSideRel[which(dat$targPresent=="absent")] = "absent"

# make a new, tidier version of dataframe only including the stuff we want!
rtdat = data.frame(subj=dat$subNum, trial=dat$trialNo, targSide=dat$targSideRel, RT=dat$RT, acc=dat$accuracy, block=dat$block )
# we don't want to be looking at RTs for incorrect trials
#rtdat$RT[rtdat$acc==0] = NaN
# those who got reward depending on performance reward=c(1,4,6,7,10,13,14,16,17,19,21,23,25,27)
# those with flat payment of 10 pounds flatPayment=c(2,3,5,8,9,11,12,15,18,20,22,24,26,28)

rtdat$incentive="r"
rtdat$incentive[rtdat$subj==2 | rtdat$subj==3 | rtdat$subj==5 | rtdat$subj==8 | rtdat$subj==9 | rtdat$subj==11 |
rtdat$subj==12 | rtdat$subj==15 | rtdat$subj==18 |rtdat$subj==20 |rtdat$subj==22 | rtdat$subj==24| rtdat$subj==26 |
rtdat$subj==28 | rtdat$subj==30 | rtdat$subj==36 | rtdat$subj== 38 |rtdat$subj== 42 |rtdat$subj== 44 |rtdat$subj== 46] = "f"
# save!!!
saveRDS(rtdat,file="data/processedRTandAccData.Rda")
write.table(rtdat, "data/processedRTandAccData.txt", sep=",")
# remove data for now
rm(dat, rtdat)


#############################
# now read in fixation data #
#############################

print("Processing Fix data...")
dat <- read.csv("data/rewardFixations.txt", header=T, sep="\t",
	colClass = c(
		"subNum"="factor",
		"block"  ="factor",
		"trialNo"="numeric",
    "fixNo"="numeric",
		"xFix"="numeric", 
    "xFixFlipped"="numeric",
		"yFix" = "numeric",
		"fixStartTime" = "numeric",
		"fixEndTime" = "numeric",
		"targPresent" = "factor",
		"targSide" = "factor",
		"easySide" = "factor",
    "RT"="numeric",
		"accuracy"="numeric"))
names(dat) = c("subj", "block", "trialNum","fixNum", "fixX","fixXflipped","fixY","fixOn","fixOff", "targPresent", "targSide", "easySide","RT","accuracy")

# Turn categorical data into factor
dat$targPresent = as.factor(dat$targPresent)
levels(dat$targPresent) = c("absent", "present")
levels(dat$targSide) = c('left', 'right', 'absent')
dat$easySide = as.factor(dat$easySide)
levels(dat$easySide) = c("left", "right")
# refdefine targSide relative to easySide
dat$targSideRel = as.factor(as.character(dat$easySide) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("hetrogeneous", "homogeneous", "absent")
dat$targSideRel[which(dat$targPresent=="absent")] = "absent"

dat = select(dat, subj,  block, trialNum, fixNum, easySide, fixX,fixXflipped, fixY, fixOn, fixOff, targPresent, targSideRel,accuracy)

dat$incentive="r"
dat$incentive[dat$subj==2 | dat$subj==3 | dat$subj==5 | dat$subj==8 | dat$subj==9 | dat$subj==11 |
               dat$subj==12 | dat$subj==15 | dat$subj==18 | dat$subj==20 | dat$subj==22 | dat$subj==24| dat$subj==26 |
               dat$subj==28 | dat$subj==30 | dat$subj==36 | dat$subj== 38|dat$subj== 42 |dat$subj== 44 |dat$subj== 46] = "f"



saveRDS(dat,file="data/processedFixData.Rda")
write.table(dat, "data/processedFixData.txt", sep=",")
