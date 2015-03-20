##Preliminary data read-in & cleaning/reshaping.  Note below where interesting stuff starts.  Look for
# 'STEVE START HERE'

setwd("/Users/mekline/Dropbox/_Projects/Gesture - Comprehension/Gesture-to-Comprehension/Analysis 1/")
library(languageR)
library(stringr)
library(lme4)
library(multcomp)
library(binom)
library(bootstrap)
mean.na.rm <- function(x) { mean(x,na.rm=T) }
stderr <- function(x) sqrt(var(x)/length(x))

stderr_binom <-function(x){
	p = sum(x)/length(x)
	q = 1-p
	sqrt(p*q/length(x))
}

confint <- function(x){
	qt(0.975,df=length(x)-1)*stderr(x)
}

confint_norm_lower <- function(x){
	foo <- binom.confint(sum(x), length(x), conf.level = 0.95, methods = "asymptotic")
	foo[5]

}

confint_norm_upper <- function(x){
	foo <- binom.confint(sum(x), length(x), conf.level = 0.95, methods = "asymptotic")
	foo[6]

}

turk.files <- list.files('batch')
willow.files <- list.files('log')
		
###############################################################
## Read in the data files from turk
list.number <- 1;  # keep track of the list number
turkdata <- data.frame(NULL)
for(f in turk.files) {
	cat(f)
	tmp <- read.csv(paste('batch/',f, sep=''), header=T)
	tmp$turkfile <- f
	turkdata <- rbind(turkdata, tmp)

	list.number <- list.number + 1
}

dropped.columns <- c("HITId", "HITTypeId","Title", "Description", "Keywords", "Reward", "CreationTime", "MaxAssignments", 	"RequesterAnnotation", "AssignmentDurationInSeconds", "AutoApprovalDelayInSeconds", "Expiration", "NumberOfSimilarHITs", 	"LifetimeInSeconds", "AssignmentId", "AcceptTime", "AutoApprovalTime", "ApprovalTime", "RejectionTime", 	"RequesterFeedback", "AssignmentStatus","LifetimeApprovalRate", "Last30DaysApprovalRate","Last7DaysApprovalRate" )

turkdata <- turkdata[, setdiff(names(turkdata), dropped.columns)]

###############################################################
## Read in the data files from willow
willowdata <- data.frame(NULL)
for (w in willow.files) {
	cat(w)
	tmp <- read.csv(paste('log/',w, sep=''), header=T)
	tmp$willowfile <- w
	tmp$willowcode <- paste(unlist(str_extract_all(w,"[0-9+]")),collapse='')
	willowdata <- rbind(willowdata, tmp)
}

#Remove extra header lines...
willowdata <- willowdata[willowdata$Paycode != "Paycode",]


###############################################################
## Merge the information from turk and willow!  

#How many do we start with?
length(unique(willowdata$Paycode)) #55
length(unique(turkdata$WorkerId)) #30


#Save and standardize
turkdata$Paycode <- turkdata$Answer.payCode
turkdata$Answer.payCode <- NA
turkdata <- turkdata[,setdiff(names(turkdata), c("Answer.payCode"))]
turkdata$HasTurk <- TRUE
willowdata$HasWillow <- TRUE

mydata <- NULL
mydata <- merge(willowdata, turkdata, by=c("Paycode"), all.x=TRUE, all.y=TRUE)

#If your line doesn't have Willow, you are a useless unmatched Turk entry!

#Check who these jokers are!  Will want to make double sure they have not taken the survey twice...
mydata[is.na(mydata$HasWillow),]$Paycode
mydata[is.na(mydata$HasWillow),]$WorkerId


#################################################################
## Check through the data for compliance and multiple takers (the general full-set solution!! 
## Use this for actually running the analysis!

#If your line doesn't have Turk, we'll give you a free pass on language, country, and video presentation
mydata[is.na(mydata$HasTurk),]$Answer.English <- "yes"
mydata[is.na(mydata$HasTurk),]$Answer.country <- "USA"
mydata[is.na(mydata$HasTurk),]$VideoProblem <- FALSE

#Come up with a true subject labeling order
mydata$willowSubNo <- as.numeric(as.character(mydata$willowSubNo))
trueSub <- unique(mydata[,c('willowcode','willowSubNo', 'Paycode')])
trueSub <- trueSub[order(trueSub$willowcode, trueSub$willowSubNo),]
trueSub$trueSubNo <- 1:nrow(trueSub)

#And add it back to the dataframe
mydata <- merge(mydata, trueSub, by=c('willowcode','willowSubNo', 'Paycode'))

#Now check if any Turk workerIDs have MORE THAN ONE subno
participantNos <- aggregate(mydata$trueSubNo, by=list(mydata$WorkerId, mydata$trueSubNo), unique)
participantNos <- participantNos[,1:2]
names(participantNos) <- c('WorkerId','trueSubNo')

#Make sure it's in the correct order!!
participantNos <- participantNos[order(participantNos$trueSubNo),]
participantNos$isDup <- duplicated(participantNos$WorkerId)

#And merge back in
mydata <- merge(mydata, participantNos, by=c('WorkerId','trueSubNo'), all.x=TRUE)
#((if you were willow-only, you pass this part!))
mydata[is.na(mydata$HasTurk),]$isDup <- FALSE


#################################################################
## Recode and clean data & conditions

#Check for number of legal responses given in a single session!!
mydata$GotAnswer <- 0
mydata[is.na(mydata$wasError),]$wasError <- '1' #Fix up missing values

mydata[mydata$Response == 'Yes' & mydata$wasError == '0',]$GotAnswer <- 1
mydata[mydata$Response == 'No' & mydata$wasError == '0',]$GotAnswer <- 1
participant.responsecount <- aggregate(mydata$GotAnswer, by=list(mydata$Paycode), sum)
names(participant.responsecount) <- c("Paycode", "LegalAnswers")
mydata <- merge(mydata, participant.responsecount, by=c("Paycode"), all.x=TRUE)


#Drop for analysis
#(Remember, Willow-onliers got a free pass on the first 3 here...)

length(unique(mydata$Paycode)) #214

length(unique(mydata[mydata$LegalAnswers > 65 & mydata$isDup == FALSE,]$Paycode))


mydata <- mydata[mydata$Answer.country == "USA" &
		mydata$Answer.English == "yes" &
		mydata$LegalAnswers > 65 & #You did at least 3/4 of the trials
		mydata$VideoProblem == FALSE & #No reported lag/issue
		mydata$isDup == FALSE,] #You didn't take it before!!
		
#Throw out people who only had turk data (no code given...)
mydata <- mydata[!is.na(mydata$HasWillow),]

length(unique(mydata$Paycode)) #105



#####
##INTERESTING STUFF STARTS HERE (Look here for paper stats)
#################################################################
## Calculting basic descriptives about the task.

mydata$wasCorrect <- as.numeric(as.character(mydata$wasCorrect))
alldata <- mydata 

#Performance on word learning?
collapsed <- alldata[!duplicated(mydata$Paycode),]

collapsed$nounTrainingTime <- as.numeric(as.character(collapsed$nounTrainingTime))
mean(collapsed$nounTrainingTime)
collapsed$verbTrainingTime <- as.numeric(as.character(collapsed$verbTrainingTime))
mean(collapsed$verbTrainingTime)

sum(collapsed$nounTrainingTime != 8)
sum(collapsed$verbTrainingTime != 12)

#Performance on numbers?

alldata$wordResponseA <- as.numeric(as.character(alldata$wordResponseA))
alldata$wordResponseB <- as.numeric(as.character(alldata$wordResponseB))

mean(alldata$wordResponseA)
stderr(alldata$wordResponseA)
sd(alldata$wordResponseA)

mean(alldata$wordResponseB)
stderr(alldata$wordResponseB)
sd(alldata$wordResponseB)

#Stick them together
a <- alldata$wordResponseA
b<- alldata$wordResponseB
numall <- a;b
mean(numall)
sd(numall)

#################################################################
## CREATE SCORE AND TABULATE DESCRIPTIVES

#Remove filler items!

alldata <- alldata[alldata$sentType == "AO" | alldata$sentType == "IO",]

#Clean up levels
alldata$sentType <- as.factor(as.character(alldata$sentType))
alldata$sentOrder <- as.factor(as.character(alldata$sentOrder))

#Calculate SCORE per person per condition
Scores <- aggregate(alldata$wasCorrect, by=list(alldata$Paycode, alldata$sentType, alldata$sentOrder), sum)
names(Scores) <- c("Paycode", "sentType",  "sentOrder", "CorrectScore")
alldata <- merge(alldata, Scores, by=c("Paycode", "sentType", "sentOrder"))

#TrialScores
TrialScores <- aggregate(alldata$wasCorrect, by=list(alldata$trialNo, alldata$sentType, alldata$sentOrder), mean.na.rm)
names(TrialScores) <- c("trialNo", "sentType",  "sentOrder", "CorrectScore_ByTrial")

#################################################################
## REPORT DESCRIPTIVES

#Report S counts
length(unique(alldata$Paycode))

#Report mean and standard error of SCORES
with(Scores, tapply(CorrectScore, list(sentType, sentOrder), mean, na.rm=TRUE), drop=TRUE)
with(Scores, tapply(CorrectScore, list(sentType, sentOrder), stderr), drop=TRUE)

#Something better: rather than standard error let's do bootstrapped CIs

#CONF INTERVALS
#Bootstrapped confidence intervals around the means of the 4 conditions!
PersonSOV.boot.mean = bootstrap(Scores[Scores$sentType=="AO" & Scores$sentOrder=="SOV",]$CorrectScore, 1000, mean)
quantile(PersonSOV.boot.mean$thetastar, c(0.025, 0.975))
PersonSVO.boot.mean = bootstrap(Scores[Scores$sentType=="AO" & Scores$sentOrder=="SVO",]$CorrectScore, 1000, mean)
quantile(PersonSVO.boot.mean$thetastar, c(0.025, 0.975))
ObjectSOV.boot.mean = bootstrap(Scores[Scores$sentType=="IO" & Scores$sentOrder=="SOV",]$CorrectScore, 1000, mean)
quantile(ObjectSOV.boot.mean$thetastar, c(0.025, 0.975))
ObjectSVO.boot.mean = bootstrap(Scores[Scores$sentType=="IO" & Scores$sentOrder=="SVO",]$CorrectScore, 1000, mean)
quantile(ObjectSVO.boot.mean$thetastar, c(0.025, 0.975))



#TIME FOR STATS
#################

#Were they above chance overall?
binom.test(sum(alldata$wasCorrect), length(alldata$wasCorrect), p=0.5)
#(yes)


##TRY MIXED LOGISTIC REGRESSION

#Here I"m looking at all participants, all items.  

#WITH all data
#The sentCond and changeCond manipulations are within-item and within-subject, so the full random slopes model is:
gesturecomp_maximal_model <- lmer(wasCorrect ~ sentType*sentOrder + (sentType*sentOrder|stimNo) + (sentType*sentOrder|Paycode), data=alldata, family="binomial")

#This doesn't converge.  So reduce the random effects model (take out item slopes - might have been a good idea to do 1 at a time? Not sure. But in any case doesn't converge)
gesturecomp_notmaximal_model <- lmer(wasCorrect ~ sentType*sentOrder + (1|stimNo) + (sentType*sentOrder|Paycode), data=alldata, family="binomial")

#still no convergence, take out participant slopes
gesturecomp_noslopes_model <- lmer(wasCorrect ~ sentType*sentOrder + (1|stimNo) + (1|Paycode), data=alldata, family="binomial")
summary(gesturecomp_noslopes_model)

#Rather than reporting p vals from the above (which is wrong bc of how the levels are specified, thanks jesse), sequentially take out fixed effects

#Remove/compare interaction
gesturecomp_nointer <- lmer(wasCorrect ~ sentType+sentOrder + (1|stimNo) + (1|Paycode), data=alldata, family="binomial")
anova(gesturecomp_noslopes_model, gesturecomp_nointer)

#The remove main effects of Type (animacy)
noType <- lmer(wasCorrect ~ sentOrder + (1|stimNo) + (1|Paycode), data=alldata, family="binomial")
anova(gesturecomp_nointer, noType)

#or of Order (SOV/SVO)
noOrder <- lmer(wasCorrect ~ sentType + (1|stimNo) + (1|Paycode), data=alldata, family="binomial")
anova(gesturecomp_nointer, noOrder)



#################################
# Exploratory analyses

#Look at some data subsets (exploratory...).  The moral is that nothing much interesting transpires - the interaction is 
#not significant in any subset (note smaller #s, but not approaching sig either).  One semi interesting thing:
#In the distractor items, no effect of word order. This is totally sensible!

#Try again removing "Same" items
diffdata <- alldata[alldata$changeType !="Same",]
#Calculate SCORE per person per condition
diffScores <- aggregate(diffdata$wasCorrect, by=list(diffdata$Paycode, diffdata$sentType, diffdata$sentOrder), sum)
names(diffScores) <- c("Paycode", "sentType",  "sentOrder", "CorrectScore_Diff")
diffdata <- merge(diffdata, Scores, by=c("Paycode", "sentType", "sentOrder"))

with(diffScores, tapply(CorrectScore_Diff, list(sentType, sentOrder), mean, na.rm=TRUE), drop=TRUE)
with(diffScores, tapply(CorrectScore_Diff, list(sentType, sentOrder), stderr), drop=TRUE)

revdata <- alldata[alldata$changeType == "Reversal",]
#Calculate SCORE per person per condition
revScores <- aggregate(revdata$wasCorrect, by=list(revdata$Paycode, revdata$sentType, revdata$sentOrder), sum)
names(revScores) <- c("Paycode", "sentType",  "sentOrder", "CorrectScore_Rev")
revdata <- merge(revdata, Scores, by=c("Paycode", "sentType", "sentOrder"))

with(revScores, tapply(CorrectScore_Rev, list(sentType, sentOrder), mean, na.rm=TRUE), drop=TRUE)
with(revScores, tapply(CorrectScore_Rev, list(sentType, sentOrder), stderr), drop=TRUE)

rv_gesturecomp_noslopes_model <- lmer(wasCorrect ~ sentType*sentOrder + (1|stimNo) + (1|Paycode), data=revdata, family="binomial")
summary(rv_gesturecomp_noslopes_model)

#Now split up changes by type...

distdata <- alldata[alldata$changeType == "Distractor",]
#Calculate SCORE per person per condition
distScores <- aggregate(distdata$wasCorrect, by=list(distdata$Paycode, distdata$sentType, distdata$sentOrder), sum)
names(distScores) <- c("Paycode", "sentType",  "sentOrder", "CorrectScore_Dist")
distdata <- merge(distdata, Scores, by=c("Paycode", "sentType", "sentOrder"))

with(distScores, tapply(CorrectScore_Dist, list(sentType, sentOrder), mean, na.rm=TRUE), drop=TRUE)
with(distScores, tapply(CorrectScore_Dist, list(sentType, sentOrder), stderr), drop=TRUE)

ds_gesturecomp_noslopes_model <- lmer(wasCorrect ~ sentType*sentOrder + (1|stimNo) + (1|Paycode), data=distdata, family="binomial")
summary(ds_gesturecomp_noslopes_model)

#Try just Passive questions!

passdata <- alldata[alldata$questionType == "Passive",]
#Calculate SCORE per person per condition
passScores <- aggregate(passdata$wasCorrect, by=list(passdata$Paycode, passdata$sentType, passdata$sentOrder), sum)
names(passScores) <- c("Paycode", "sentType",  "sentOrder", "CorrectScore_Ps")
passdata <- merge(passdata, Scores, by=c("Paycode", "sentType", "sentOrder"))

with(passScores, tapply(CorrectScore_Ps, list(sentType, sentOrder), mean, na.rm=TRUE), drop=TRUE)
with(passScores, tapply(CorrectScore_Ps, list(sentType, sentOrder), stderr), drop=TRUE)


#Test that...
ps_gesturecomp_noslopes_model <- lmer(wasCorrect ~ sentType*sentOrder + (1|stimNo) + (1|Paycode), data=passdata, family="binomial")
summary(ps_gesturecomp_noslopes_model)




#######
#######
#######
#######
#######
#######
#######
#Subset tests etc., very very very exploratory

#What about people who didn't just ceiling out of the task?
personScores <- aggregate(alldata$wasCorrect, by=list(alldata$Paycode), mean.na.rm)
names(personScores) <-c("Paycode","overallScore")
alldata <- merge(alldata, personScores)

lowdata <- alldata[alldata$overallScore < 0.75,]

#What about people who paid more/less attention to the flanking memory task?
responseAScores <- aggregate(as.numeric(as.character(alldata$wordResponseA)), by=list(alldata$Paycode), mean.na.rm)
names(responseAScores) <-c("Paycode","AScore")
responseBScores <- aggregate(as.numeric(as.character(alldata$wordResponseB)), by=list(alldata$Paycode), mean.na.rm)
names(responseBScores) <-c("Paycode","BScore")
wordResponses <- merge(responseAScores, responseBScores, by=c("Paycode"))
wordResponses$wordResponse <- wordResponses$AScore + wordResponses$BScore
alldata <- merge(alldata, wordResponses)

worddata <- alldata[alldata$wordResponse > 8,]
middata <- alldata[alldata$wordResponse < 8,]

#And look at the 1st and 2nd halves of the data!
alldata$trialNo <- as.numeric(as.character(alldata$trialNo))
firstdata <- alldata[alldata$trialNo < 37 ,]
lastdata <- alldata[alldata$trialNo > 18,]



#Calculate SCORE per person per condition - LOWDATA
lowScores <- aggregate(lowdata$wasCorrect, by=list(lowdata$Paycode, lowdata$sentType, lowdata$sentOrder), sum)
names(lowScores) <- c("Paycode", "sentType",  "sentOrder", "CorrectScore")
lowdata <- merge(lowdata, lowScores, by=c("Paycode", "sentType",  "sentOrder"))

#Calculate SCORE per person per condition - WORDDATA
wordScores <- aggregate(worddata$wasCorrect, by=list(worddata$Paycode, worddata$sentType, worddata$sentOrder), sum)
names(wordScores) <- c("Paycode", "sentType",  "sentOrder", "CorrectScore")
worddata <- merge(worddata, wordScores, by=c("Paycode", "sentType",  "sentOrder"))

midScores <- aggregate(middata$wasCorrect, by=list(middata$Paycode, middata$sentType, middata$sentOrder), sum)
names(midScores) <- c("Paycode", "sentType",  "sentOrder", "CorrectScore")
middata <- merge(middata, midScores, by=c("Paycode", "sentType",  "sentOrder"))

#Calculate SCORE per person per condition - first/last data
firstScores <- aggregate(firstdata$wasCorrect, by=list(firstdata$Paycode, firstdata$sentType, firstdata$sentOrder), sum)
names(firstScores) <- c("Paycode", "sentType",  "sentOrder", "CorrectScore")
firstdata <- merge(firstdata, firstScores, by=c("Paycode", "sentType",  "sentOrder"))

lastScores <- aggregate(lastdata$wasCorrect, by=list(lastdata$Paycode, lastdata$sentType, lastdata$sentOrder), sum)
names(lastScores) <- c("Paycode", "sentType",  "sentOrder", "CorrectScore")
lastdata <- merge(lastdata, lastScores, by=c("Paycode", "sentType",  "sentOrder"))


with(lowdata, tapply(Paycode, list(sentType, changeType, sentOrder), unique))
with(worddata, tapply(Paycode, list(sentType, changeType, sentOrder), unique))
with(middata, tapply(Paycode, list(sentType, changeType, sentOrder), unique))


with(lowScores, tapply(CorrectScore, list(sentType, sentOrder), mean, na.rm=TRUE), drop=TRUE)
with(lowScores, tapply(CorrectScore, list(sentType, sentOrder), stderr), drop=TRUE)

with(wordScores, tapply(CorrectScore, list(sentType, sentOrder), mean, na.rm=TRUE), drop=TRUE)
with(wordScores, tapply(CorrectScore, list(sentType, sentOrder), stderr), drop=TRUE)

with(midScores, tapply(CorrectScore, list(sentType, sentOrder), mean, na.rm=TRUE), drop=TRUE)
with(midScores, tapply(CorrectScore, list(sentType, sentOrder), stderr), drop=TRUE)

with(firstScores, tapply(CorrectScore, list(sentType, sentOrder), mean, na.rm=TRUE), drop=TRUE)
with(firstScores, tapply(CorrectScore, list(sentType, sentOrder), stderr), drop=TRUE)

with(lastScores, tapply(CorrectScore, list(sentType, sentOrder), mean, na.rm=TRUE), drop=TRUE)
with(lastScores, tapply(CorrectScore, list(sentType, sentOrder), stderr), drop=TRUE)
with(lastScores, tapply(CorrectScore, list(sentType, sentOrder), confint_norm_lower), drop=TRUE)
with(lastScores, tapply(CorrectScore, list(sentType, sentOrder), confint_norm_upper), drop=TRUE)


# Try just the later items?

alldata <- lastdata

#WITH LOWDATA
lowgesturecomp_maximal_model <- lmer(wasCorrect ~ sentType*sentOrder + (sentType*sentOrder|stimNo) + (sentType*sentOrder|Paycode), data=lowtransdata, family="binomial")
summary(lowgesturecomp_maximal_model)

#WITH LASTDATA
lastgesturecomp_maximal_model <- lmer(wasCorrect ~ sentType*sentOrder + (sentType*sentOrder|stimNo) + (sentType*sentOrder|Paycode), data=lasttransdata, family="binomial")
summary(lastgesturecomp_maximal_model)

lastgesturecomp_nointer_model <- lmer(wasCorrect ~ sentType+sentOrder + (sentType*sentOrder|stimNo) + (sentType*sentOrder|Paycode), data=lasttransdata, family="binomial")
anova(lastgesturecomp_maximal_model, lastgesturecomp_nointer_model)


#Let's try an anova??
lastTransScores <- lastScores[lastScores$sentType == "AO" | lastScores$sentType == "IO",]
fit <- lm(CorrectScore ~ sentType * sentOrder + (1/Paycode), lastTransScores)
anova(fit)

#By item?
lastItemScores <- aggregate(lastdata$wasCorrect, by=list(lastdata$stimNo, lastdata$sentType, lastdata$sentOrder), sum)
names(lastItemScores) <- c("stimNo", "sentType",  "sentOrder", "CorrectScore")
lastItemTransScores <- lastItemScores[lastItemScores$sentType == "AO" | lastItemScores$sentType == "IO",]
fit <- lm(CorrectScore ~ sentType * sentOrder + (1/stimNo), lastItemTransScores)
anova(fit)

#Let's try an anova??
passTransScores <- passScores[passScores$sentType == "AO" | passScores$sentType == "IO",]
fit <- lm(CorrectScore ~ sentType * sentOrder + (1/Paycode), passTransScores)
anova(fit)