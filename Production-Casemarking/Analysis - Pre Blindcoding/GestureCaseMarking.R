
setwd("/Users/mekline/Dropbox/_Projects/Gesture Case Marking/Analysis/")
library(languageR)
library(stringr)
library(lme4)
library(multcomp)
library(binom)
mean.na.rm <- function(x) { mean(x,na.rm=T) }
stderr <- function(x) sqrt(var(x)/length(x))



#Read in the two data files and combine them!

freeData <- read.csv('FreeGestureData.csv', header=T)
handData <- read.csv('CaseInstructionData.csv', header=T)
freeData$Instructions <- "FreeInstructions"
handData$Instructions <- "HandInstructions"
freeData[is.na(freeData$Spatial.Cue),]$Spatial.Cue <- 0
handData$Spatial.Cue <- as.numeric(as.character(handData$Spatial.Cue))
handData[is.na(handData$Spatial.Cue),]$Spatial.Cue <- 0
mydata <- rbind(freeData,handData)

#Get rid of the speech columns, we don't need them!
mydata <- mydata[mydata$Trial.Type == "Gesture",]

#Handle problem people and items!!

mydata <- mydata[(mydata$Trial.Number != 2) & (mydata$Trial.Number != 3),]
mydata <- mydata[mydata$Subject != 28,]

#Important columns!
#Instructions <- None or Case marking
#Type.of.Action <- Transitive/Intransitive sentence
#Object.Type <- Person, Object, or None type patient
#Word.Order <- SOV, SVO <- Participants raw response! Sequence of gestures
#Spatial.Cue <- Did the participants use a spatial cue for the subject and object?
#Exclude <- Individual trials to be excluded
#P_Before_A <- Hand-coded interpretation of the sequences (1 means SOV) - Errorful!

#################################################################
## CATEGORIZE RESPONSES

#We want to know whether S and O are separated by the V or not. Since people sometimes gesture extra, we'll keep 
#the first instance of each item.  So a person who gestures SVSVVO would be scored as SVO, and a person who 
#gestured SOOVS would be coded as SOV.  

#Find the first instance of each item
mydata$S.pos <- str_locate(mydata$Word.Order,"S")[,1]
mydata$V.pos <- str_locate(mydata$Word.Order,"V")[,1]
mydata$O.pos <- str_locate(mydata$Word.Order,"O")[,1]

#And code that for the measure of interest!
#mydata$Participants.Clustered <- ((mydata$S.pos < mydata$V.pos) & (mydata$O.pos < mydata$V.pos)) | ((mydata$S.pos > mydata$V.pos) & (mydata$O.pos > mydata$V.pos))
mydata$Patient.Before.Action <- mydata$V.pos > mydata$O.pos
#Keep only data that has a codeable response!
mydata <- mydata[!is.na(mydata$Patient.Before.Action),]
mydata <- mydata[mydata$Type.of.Action == "Transitive",]
mydata <- mydata[mydata$Object.Type == "Object" | mydata$Object.Type == "Person" ,]

#Make the variables human-readable!!
mydata$Word.Order.Coded <- "SVO"
mydata[mydata$Patient.Before.Action == TRUE,]$Word.Order.Coded <- "SOV"

mydata[mydata$Spatial.Cue == 1,]$Spatial.Cue <- "Spatial.Present"
mydata[mydata$Spatial.Cue == 0,]$Spatial.Cue <- "Spatial.Absent"
#################################################################
## REPORT DESCRIPTIVES
sum.na.rm <- function(x) { sum(x,na.rm=T) }
my.sd <- function(x) {sd(x)/sqrt(length(x))}

#Report S counts
length(unique(mydata$Subject))

#Report counts of SOV versus SVO instances in Free and Hand conditions (TRUE = SOV)
table(mydata$Word.Order.Coded, mydata$Object.Type, mydata$Instructions)

#And when were spatial cues actually produced?
table(mydata$Spatial.Cue, mydata$Word.Order.Coded, mydata$Instructions)
table(mydata$Spatial.Cue, mydata$Object.Type, mydata$Instructions)

justSOV <- mydata[mydata$Word.Order.Coded == "SOV",]
table(justSOV$Spatial.Cue, justSOV$Object.Type, justSOV$Instructions)

#Make scores for each participant
mydata$ChoseSOV <- 0
mydata[mydata$Word.Order.Coded == "SOV",]$ChoseSOV <- 1

ParticipantScores <- aggregate(mydata$ChoseSOV, by=list(mydata$Subject, mydata$Object.Type, mydata$Instructions), sum.na.rm)
names(ParticipantScores) <- c("Subject", "ObjectType", "Instructions", "ChoseSOV")

#Table for scores too
with(ParticipantScores, tapply(ChoseSOV, list(ObjectType, Instructions), mean, na.rm=TRUE), drop=TRUE)

#Time for bootstrapped confidence intervals around the means of the 4 conditions!
library(bootstrap)
PersonFree.boot.mean = bootstrap(ParticipantScores[ParticipantScores$ObjectType=="Person" & ParticipantScores$Instructions=="FreeInstructions",]$ChoseSOV, 1000, mean)
quantile(PersonFree.boot.mean$thetastar, c(0.025, 0.975))
PersonHand.boot.mean = bootstrap(ParticipantScores[ParticipantScores$ObjectType=="Person" & ParticipantScores$Instructions=="HandInstructions",]$ChoseSOV, 1000, mean)
quantile(PersonHand.boot.mean$thetastar, c(0.025, 0.975))

ObjectFree.boot.mean = bootstrap(ParticipantScores[ParticipantScores$ObjectType=="Object" & ParticipantScores$Instructions=="FreeInstructions",]$ChoseSOV, 1000, mean)
quantile(ObjectFree.boot.mean$thetastar, c(0.025, 0.975))
ObjectHand.boot.mean = bootstrap(ParticipantScores[ParticipantScores$ObjectType=="Object" & ParticipantScores$Instructions=="HandInstructions",]$ChoseSOV, 1000, mean)
quantile(ObjectHand.boot.mean$thetastar, c(0.025, 0.975))

#########################################
##STATISTICAL TESTS!!
#########################################

mydata$Word.Order.Coded <- as.factor(mydata$Word.Order.Coded)
mydata$Object.Type <- as.factor(mydata$Object.Type)
mydata$Spatial.Cue <- as.factor(mydata$Spatial.Cue)
mydata$Instructions <- as.factor(mydata$Instructions)

freedata <- mydata[mydata$Instructions == "FreeInstructions",]
handdata <- mydata[mydata$Instructions == "HandInstructions",]
freedata$Word.Order.Coded <- as.factor(freedata$Word.Order.Coded)
handdata$Word.Order.Coded <- as.factor(handdata$Word.Order.Coded)
freedata$Object.Type <- as.factor(freedata$Object.Type)
handdata$Object.Type <- as.factor(handdata$Object.Type)

#Within Experiments
free_model <- lmer(Word.Order.Coded ~ Object.Type  + (1+Object.Type|Subject), data=freedata, family="binomial")
summary(free_model)
hand_model <- lmer(Word.Order.Coded ~ Object.Type  + (1+Object.Type|Subject), data=handdata, family="binomial")
summary(hand_model)

#Across Experiments (Lumped)
word_order_model <- lmer(Word.Order.Coded ~ Object.Type  + (1+Object.Type|Subject), data=mydata, family="binomial")
summary(word_order_model)

cue_model <- lmer(Spatial.Cue ~ Object.Type  + (1+Object.Type|Subject), data=mydata, family="binomial")
summary(cue_model)

cue_order_model <- lmer(Spatial.Cue ~ Word.Order.Coded  + (1+Word.Order.Coded|Subject), data=mydata, family="binomial")
summary(cue_order_model)

#Between Experiments
instructions_model <- lmer(Word.Order.Coded ~ Object.Type*Instructions  + (1+Object.Type*Instructions|Subject), data=mydata, family="binomial")
summary(instructions_model)
instructions_model_spatial <- lmer(Spatial.Cue ~ Object.Type*Instructions  + (1+Object.Type*Instructions|Subject), data=mydata, family="binomial")
summary(instructions_model_spatial)




#################
##TRY MIXED LOGISTIC REGRESSION

#Here I"m looking at all participants, all items.  Models would be same for the other dataset cuts

#WITH ALL CONDITIONS/GOOD SUBJECT DATA 
#The sentCond and changeCond manipulations are within-item and between-subject, so the full random slopes model is:
#allconditions <- goodparticipantdata[goodparticipantdata$sentCond == 'baseline' | goodparticipantdata$sentCond == 'transitive'  | goodparticipantdata$sentCond == 'periphrastic'| goodparticipantdata$sentCond == 'noncausal'| goodparticipantdata$sentCond == 'badpassive',]
#mb_full_maximal_model <- lmer(Response ~ sentCond*changeCond + (sentCond*changeCond|itemNo) + (1|Paycode), data=allconditions, family="binomial")

#Also try with just the participant error term....?
#mb_justparticerror_model <- lmer(Response ~ sentCond*changeCond +  (1|Paycode), data=forlog, family="binomial")

#summary(mb_full_maximal_model)

#WITH GOOD SUBJECT DATA, MAIN CONDITIONS
#The sentCond and changeCond manipulations are within-item and between-subject, so the full random slopes model is:
threeconditions <- goodparticipantdata[goodparticipantdata$sentCond == 'baseline' | goodparticipantdata$sentCond == 'transitive'  | goodparticipantdata$sentCond == 'periphrastic',]
mb_full_maximal_model <- lmer(Response ~ sentCond*changeCond + (sentCond*changeCond|itemNo) + (1|Paycode), data=threeconditions, family="binomial")
summary(mb_full_maximal_model)

#Interaction coding for multiple comparisons!!
threeconditions$SentxChange <- interaction(threeconditions$sentCond, threeconditions$changeCond, drop=T) 
mb_interacted_maximal_model <- lmer(Response ~ SentxChange + (sentCond*changeCond|itemNo) + (1|Paycode), data=threeconditions, family="binomial")
summary(mb_interacted_maximal_model)
foo = glht(mb_interacted_maximal_model, linfct=mcp(SentxChange = "Tukey")) 
summary(foo)

#With just transitive and periphrastic, for comparison
twoconditions <- goodparticipantdata[goodparticipantdata$sentCond == 'transitive'  | goodparticipantdata$sentCond == 'periphrastic',]
mb_full_maximal_model <- lmer(Response ~ sentCond*changeCond + (sentCond*changeCond|itemNo) + (1|Paycode), data=twoconditions, family="binomial")
summary(mb_full_maximal_model)

#WITH JUST FIRST TRIAL
firstthree <- firstgooddata[firstgooddata$sentCond == 'baseline' | firstgooddata$sentCond == 'transitive'  | firstgooddata$sentCond == 'periphrastic',]
mb_full_maximal_model <- lmer(Response ~ sentCond*changeCond + (sentCond*changeCond|itemNo), data=firstthree, family="binomial")
#Note no paycode term because just one observation per paycode!!
summary(mb_full_maximal_model)

firsttwo <- firstgooddata[firstgooddata$sentCond == 'transitive'  | firstgooddata$sentCond == 'periphrastic',]
mb_full_maximal_model <- lmer(Response ~ sentCond*changeCond + (sentCond*changeCond|itemNo), data=firsttwo, family="binomial")
#Note no paycode term because just one observation per paycode!!
summary(mb_full_maximal_model)

#Compare periphrastic to badpassive
peribad <- goodparticipantdata[goodparticipantdata$sentCond == 'badpassive' | goodparticipantdata$sentCond == 'periphrastic',]
mb_full_maximal_model <- lmer(Response ~ sentCond*changeCond + (sentCond*changeCond|itemNo) + (1|Paycode), data=peribad, family="binomial")
summary(mb_full_maximal_model)

#Compare transitive to noncausal and baseline!
transiwho <- goodparticipantdata[goodparticipantdata$sentCond == 'baseline' | goodparticipantdata$sentCond == 'transitive'  | goodparticipantdata$sentCond == 'noncausal',]
mb_full_maximal_model <- lmer(Response ~ sentCond*changeCond + (sentCond*changeCond|itemNo) + (1|Paycode), data=transiwho, family="binomial")
summary(mb_full_maximal_model)

#And those two again on just first trials!!
#Compare periphrastic to badpassive
firstperi <- firstgooddata[firstgooddata$sentCond == 'badpassive' | firstgooddata$sentCond == 'periphrastic',]
mb_full_maximal_model <- lmer(Response ~ sentCond*changeCond + (sentCond*changeCond|itemNo), data=firstperi, family="binomial")
#Note no paycode term because just one observation per paycode!!
summary(mb_full_maximal_model)

firsttransi <- firstgooddata[firstgooddata$sentCond == 'baseline' | firstgooddata$sentCond == 'transitive' | firstgooddata$sentCond == 'noncausal',]
mb_full_maximal_model <- lmer(Response ~ sentCond*changeCond + (sentCond*changeCond|itemNo), data=firsttransi, family="binomial")
#Note no paycode term because just one observation per paycode!!
summary(mb_full_maximal_model)



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



