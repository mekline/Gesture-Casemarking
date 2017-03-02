#Full analysis for GestureCaseMarking experiment
#
#Preprocessing of blind coding data developed by MS . Code copied over on 8/12 and commented
#(up thru the indicated area) but otherwise left alone for now.
#
#Note: First part of this file reproduces output files: LiteralNoAgree.csv and Last3NoAgree.csv, the latter of
#which was then tiebroken by hand by trained RAs. This tiebreaking is stored in Last3NoAgree_Reconciliation.csv,
# which gets read in and used for subsequent analyses.
#

#Reading in all libraries that we'll use
library(irr)
library(stringr)
library(languageR)
library(lme4)
library(multcomp)
library(binom)
library(bootstrap)
library(RColorBrewer)
library(ggplot2)
mean.na.rm <- function(x) { mean(x,na.rm=T) }
sum.na.rm <- function(x) { sum(x,na.rm=T) }
stderr <- function(x) sqrt(var(x)/length(x))

#Get directory of this file. (CHANGE THIS IF RUNNING ON A COMPUTER OTHER THAN MK'S LAPTOP) ##LINE TO REDACT FOR PEER REVIEW
#directory = '/Users/mekline/Dropbox/_Projects/Gesture - Case Marking/Analysis - Post Blindcoding' ##LINE TO REDACT FOR PEER REVIEW
directory = '/Users/mekline/Dropbox/_Projects/Gesture/Gesture-Casemark Repo/Analysis - Post Blindcoding' ##LINE TO REDACT FOR PEER REVIEW

#There is also a tidy data table for people who don't want to debug blind coding/merging steps. (in that case, comment out from line 33 to line 444)
tidydir <- '/Users/mekline/Dropbox/_Projects/Gesture/Gesture-Casemark Repo/Tidy Data/' #CHANGE THIS IF RUNNING ON NOT MY LAPTOP ##LINE TO REDACT FOR PEER REVIEW
#######################################
#Initialize dataset
gestable = data.frame(NULL)

#Load csv with Alldata into variable
gestable = read.csv(paste0(directory, "/SpatialCasemarking_AllGestureData.csv"), header = TRUE)

#Load csv of SubjectInfo
subtable = read.csv(paste0(directory, "/SpatialCasemarking_SubjectInfo.csv"), header = TRUE)

#Get the columns from SubTable that we need (identity of coders and by-subject inclusion information (i.e. was participant inebriated?))
#Merge subtable data to gestable
gestable$Participant <- gestable$Subject
subtable <- subtable[,c("Participant", "Initial.Coder","Blind.Coder","To.include")]
alltable <- merge(gestable, subtable, by=c("Participant"))
gestable <- alltable[alltable$To.include == 1,]


#Add column that compares old and new coding on string-literal basis
#But first! Force uppercase!
gestable$Word.Order <- toupper(as.character(gestable$Word.Order))
gestable$Word.Order.Recode <- toupper(as.character(gestable$Word.Order.Recode))


gestable$Literal.Compare = (as.character(gestable$Word.Order) == as.character(gestable$Word.Order.Recode))
litcompavg = mean(gestable$Literal.Compare)
print(paste0('Literal Direct Agreement: ', litcompavg))

#Spit out CSV table comparing word orders that didn't match
noagreetable <- gestable[gestable$Literal.Compare == 0,]
noagreetable <- noagreetable[,c("Subject", "GestureCondition", "Trial.Number", "Clipped.Movie.File", "Word.Order", "Word.Order.Recode", "Initial.Coder", "Blind.Coder")]

write.csv(noagreetable, file = paste0(directory, "/LiteralNoAgree.csv"))

#####
#Calculate Cohen's Kappa for Literal Compare 
#####

# Note, MS did this by hand! Whoa!

firstlev <- levels(factor(gestable$Word.Order))
secondlev <- levels(factor(gestable$Word.Order.Recode))
alllev <- levels(factor(c(firstlev, secondlev)))
litfirstprob <- rep(0, length(alllev))
litsecondprob <- rep(0, length(alllev))

for (i in gestable$Word.Order) {
	for (j in 1:(length(alllev))) {
		if (i == alllev[j]) {
			litfirstprob[j] <- litfirstprob[j]+1
		}
	}
}

for (k in gestable$Word.Order.Recode) {
	for (l in 1:(length(alllev))) {
		if (k == alllev[l]) {
			litsecondprob[l] <- litsecondprob[l]+1
		}
	}
}

#Come up with random probability percentages for each level
percent1 = litfirstprob/length(gestable$Word.Order)
percent2 = litsecondprob/length(gestable$Word.Order)

litpercents = percent1*percent2

litrandprob = 0
for (w in litpercents) {litrandprob = litrandprob+w}

litkappa = (litcompavg-litrandprob)/(1-litrandprob)
print(paste0('Literal Kappa (manual): ', litkappa))

####
# End Manual Kappa calculation
####

####
# OK, now let's try kappa2 from the irr package, does it match?
####

kappa2(gestable[,c('Word.Order','Word.Order.Recode')]) #Not sure about the weight paradigm! but this matches MS ok

#Add column that compares coding regarding spatial cues.
gestable$SpaCue.Agree = as.character((as.numeric(gestable$Spatial.Cue) == as.numeric(gestable$Spatial.Cue.Recode)))
spacueavg = mean(as.logical(gestable$SpaCue.Agree))

####
#Serious data cleaning starts here: We want to compare strings just based on the last 3 symbols of participants'
#final gesture string. (See the preprint for details & justification on why we do this)
#This will be our main unit of analysis going forward. We'll report kappa on this, as well as breaking down
#those calculations based on what kind of disagreement was found (main takehome: SVO vs SOV is a more serious
#error than SOV vs. OSV)
####

#Get rid of intransitive responses from analysis, they are filler trials
gestable <- gestable[ ! gestable$Type.of.Action %in% "Intransitive",]


#Insert Blank Column for Organization and Initialize counters
gestable$Blank1 <- ''
i = 0
j = 0
k = 0
l = 0
	
#Get the last chunk of string after the Slash (EL's coding) #NOTE: Eun/Mig are actually just labels for the first and second coder, although EL and MS were the first 2 people to work on it. The actual coders are listed in the Initial.Coder and Blind.Coder columns 
oldSsplit = NULL
for (k in as.character(gestable$Word.Order)) {
	splt = unlist(strsplit(k, "[/]"))
	lst = splt[length(splt)]
	oldSsplit <- append(oldSsplit, lst)}
gestable$Eun.S.Split <- oldSsplit #(Eun=EL)


#Get the last chunk of string after the Slash (MS's coding)
newSsplit = NULL
for (l in as.character(gestable$Word.Order.Recode)) {
	splt = unlist(strsplit(l, "[/]"))
	lst = splt[length(splt)]
	newSsplit <- append(newSsplit, lst)}
gestable$Mig.S.Split <- newSsplit #(Mig=MS)


#Insert blank column for organization
gestable$Blank3 <- ''

#Take old set of coding and take only the last 3 characters
EunLast3 = NULL
#for (num in length(gestable$Eun.S.Split))
	for (h in gestable$Eun.S.Split) {
		WOslash <- unlist(strsplit(h, "[/]"))
		WOfpar <- unlist(strsplit(WOslash, "[ ]"))
		WOques <- unlist(strsplit(WOfpar, "[?]"))
		WOall <- unlist(strsplit(as.character(WOques), "[,]"))
		tog <- paste(WOall, sep = '', collapse = '')
		lasttog <- str_match(tog, '[\\(\\)]*[SOV]?[\\(\\)]*[SOV]?[\\(\\)]*[SOV][\\(\\)]*$')
		#Note: the above regex asks for: the last 3 SOV characters, plus any adjacent parentheses that we find
		EunLast3 <- append(EunLast3, lasttog)
		}
gestable$Eun.Last3 = EunLast3
gestable[is.na(gestable$Eun.Last3),]$Eun.Last3 <- ''

gestable$Eun.Last3

#Take set of recoding and take only the last 3 characters
MigLast3 = NULL
for (d in gestable$Mig.S.Split) {
	WOslash <- unlist(strsplit(d, "[/]"))
	WOfpar <- unlist(strsplit(WOslash, "[ ]"))
	WOques <- unlist(strsplit(WOfpar, "[?]"))
	WOall <- unlist(strsplit(as.character(WOques), "[,]"))
	tog <- paste(WOall, sep = '', collapse = '')
	lasttog <- str_match(tog, '[\\(\\)]*[SOV]?[\\(\\)]*[SOV]?[\\(\\)]*[SOV][\\(\\)]*$')
	MigLast3 <- append(MigLast3, lasttog)
	}
gestable$Mig.Last3 = MigLast3
gestable[is.na(gestable$Mig.Last3),]$Mig.Last3 <- ''

gestable$Mig.Last3

#Add column that compares old and new coding of only the last 3 gestures
gestable$Last3.Compare <- (as.character(gestable$Eun.Last3) == as.character(gestable$Mig.Last3))
lst3compavg <- mean(gestable$Last3.Compare)
print(paste0('Last3 Direct Agreement: ', lst3compavg))

####
# Again, by-hand calculation of Cohen's Kappa
####

#Calculate Cohen's Kappa for Last3 Compare
first3lev <- levels(as.factor(gestable$Eun.Last3))
second3lev <- levels(as.factor(gestable$Mig.Last3))
all3lev <- levels(factor(c(first3lev, second3lev)))
litfirst3prob <- rep(0, length(all3lev))
litsecond3prob <- rep(0, length(all3lev))

for (i in gestable$Eun.Last3) {
	for (j in 1:(length(all3lev))) {
		if (i == all3lev[j]) {
			litfirst3prob[j] <- litfirst3prob[j]+1
		}
	}
}

for (k in gestable$Mig.Last3) {
	for (l in 1:(length(all3lev))) {
		if (k == all3lev[l]) {
			litsecond3prob[l] <- litsecond3prob[l]+1
		}
	}
}

#Come up with random probability percentages for each level
percent3_1 = litfirst3prob/length(gestable$Eun.Last3)
percent3_2 = litsecond3prob/length(gestable$Eun.Last3)

lst3percents = percent3_1*percent3_2

lst3randprob = 0
for (w in lst3percents) {lst3randprob = lst3randprob+w}

lst3kappa = (lst3compavg-lst3randprob)/(1-lst3randprob)
print(paste0('Last3 Kappa (Manual): ', lst3kappa))


####
# End manual kappa
####

####
# OK, now let's try kappa2 from the irr package, does it match?
####

kappa2(gestable[,c('Eun.Last3','Mig.Last3')]) #Not sure about the weight paradigm! but this matches MS


#Produce table that spits out disagreement on WORD ORDER.
noagreetable2 <- gestable[gestable$Last3.Compare == 0,]
noagreetable2 <- noagreetable2[,c("Subject", "GestureCondition", "Trial.Number", "Clipped.Movie.File", "Event", "Eun.Last3", "Mig.Last3", "Initial.Coder", "Blind.Coder")]

write.csv(noagreetable2, file = paste0(directory, "/Last3NoAgree.csv"))

#Produce table that spits out disagreement on SPATIAL.
noagreetable4 <- gestable[gestable$SpaCue.Agree == 0,]
noagreetable4 <- noagreetable4[,c("Subject", "GestureCondition", "Trial.Number", "Clipped.Movie.File", "Event", "Spatial.Cue", "Spatial.Cue.Recode", "Initial.Coder", "Blind.Coder")]

write.csv(noagreetable4, file = paste0(directory, "/SpatialNoAgree.csv"))


########
# AT THIS POINT, a third coder worked with MS to tiebreak/resolve by discussion any video that had been coded
# differently by the two coders. For Word Order, their final judgement, and the nature of the disagreement, are recorded in 2
# new columns in the file Last3NoAgree_Reconciliation.csv, loaded back in below.
#
# For Spatial, their final judgment is recorder in SpatialNoAgree_Reconciliation.csv.  It turned out that agreement was
# initially very low, so we reworked the spatial definitions, and Reconciliation was done by adhering to this new
# standard in discussion. Finally, a coder blind to all hypotheses in the experiment coded 10% of the spatial 
# info task following these new instructions.
########

#Load the files back in
wordOrderNoAgreeTable <- read.csv(paste0(directory, "/Last3NoAgreeReconciliation.csv"), header = TRUE)

spatialNoAgreeTable <- read.csv(paste0(directory, "/SpatialNoAgreeReconciliation.csv"), header = TRUE)

#Drop columns that are just duplicated in gestable/we don't need.
wordOrderNoAgreeTable <- wordOrderNoAgreeTable[,c('Subject','GestureCondition','Trial.Number', 'Final.Decision', 'Final.Clean', 'Multiple.V', 'Discussed.With', 'Disagree.Reason')]
spatialNoAgreeTable$Spatial.Final.Decision <- spatialNoAgreeTable$Final.Decision
spatialNoAgreeTable$Spatial.Discussed.With <- spatialNoAgreeTable$Discussed.With
spatialNoAgreeTable <- spatialNoAgreeTable[,c('Subject','GestureCondition','Trial.Number','Spatial.Final.Decision','Spatial.Discussed.With')]

######
#Merge those lines back on! 
#For WordOrder, We need to make a 'final clean' column that takes 1) Eun Last3 coding where there
#is no disagreement, and 2) FinalClean where there was disagreement. Also mark disagreement type as either what
#it was, or as noDisagreement.
#For Spatial, FinalDecision will be the final decision from MS/MG discussion, OR the original if there was
#no disagreement. Disagreement type isn't marked, there's just spatial or no...
#
# Also keep a 'final long' column that lists people's whole gesture sequence, so we can compare those...

alldata <- merge(gestable, wordOrderNoAgreeTable, by=c('Subject','GestureCondition','Trial.Number'), all=TRUE)
alldata <- merge(alldata, spatialNoAgreeTable, by=c('Subject','GestureCondition','Trial.Number'), all=TRUE)

alldata$Disagree.Reason <- as.character(alldata$Disagree.Reason)
alldata[is.na(alldata$Final.Clean),]$Disagree.Reason <- "NoDisagreement"
alldata$Final.Clean <- as.character(alldata$Final.Clean)
alldata[is.na(alldata$Final.Clean),]$Final.Clean <- alldata[is.na(alldata$Final.Clean),]$Eun.Last3
alldata[is.na(alldata$Spatial.Final.Decision),]$Spatial.Final.Decision <- alldata[is.na(alldata$Spatial.Final.Decision),]$Spatial.Cue

alldata$Final.Long <- as.character(alldata$Final.Decision)
alldata[is.na(alldata$Final.Long),]$Final.Long <- alldata[is.na(alldata$Final.Long),]$Word.Order #MS/EL's original original coding!

#######
# Final agreement calculations to report!

#Classify all items as VerbMedial, VerbLateral, or Unclassified
#This will also decide individual items to exclude (for not consisting of exactly one S,V,O, or for having 
# parenthesis orders that make verb medial/final judgment impossible.

alldata$WordOrder.Classified <- "Unclassified"
alldata[alldata$Final.Clean == "SOV",]$WordOrder.Classified <- "VerbLateral"
alldata[alldata$Final.Clean == "OSV",]$WordOrder.Classified <- "VerbLateral"
alldata[alldata$Final.Clean == "VSO",]$WordOrder.Classified <- "VerbLateral"
alldata[alldata$Final.Clean == "VOS",]$WordOrder.Classified <- "VerbLateral"
#Parenthesis cases!
alldata[alldata$Final.Clean == "V(OS)",]$WordOrder.Classified <- "VerbLateral"
alldata[alldata$Final.Clean == "V(SO)",]$WordOrder.Classified <- "VerbLateral"
alldata[alldata$Final.Clean == "(SO)V",]$WordOrder.Classified <- "VerbLateral"
alldata[alldata$Final.Clean == "(OS)V",]$WordOrder.Classified <- "VerbLateral"

alldata[alldata$Final.Clean == "SVO",]$WordOrder.Classified <- "VerbMedial"
alldata[alldata$Final.Clean == "OVS",]$WordOrder.Classified <- "VerbMedial"

#And find out the final kappa numbers we need: Agreement by Medial/Lateral/Undefined, and Spatial agreement ala Katy

#Have to restate Eun.Last3 and Mig.Last3 in the same way as final decisions for this....
alldata[is.na(alldata$Eun.Last3),]$Eun.Last3 <- "NoCode"
alldata$Eun.Classified <- "Unclassified"
alldata[alldata$Eun.Last3 == "SOV",]$Eun.Classified <- "VerbLateral"
alldata[alldata$Eun.Last3 == "OSV",]$Eun.Classified <- "VerbLateral"
alldata[alldata$Eun.Last3 == "VSO",]$Eun.Classified <- "VerbLateral"
alldata[alldata$Eun.Last3 == "VOS",]$Eun.Classified <- "VerbLateral"
alldata[alldata$Eun.Last3 == "V(OS)",]$Eun.Classified <- "VerbLateral"
alldata[alldata$Eun.Last3 == "V(SO)",]$Eun.Classified <- "VerbLateral"
alldata[alldata$Eun.Last3 == "(SO)V",]$Eun.Classified <- "VerbLateral"
alldata[alldata$Eun.Last3 == "(OS)V",]$Eun.Classified <- "VerbLateral"
alldata[alldata$Eun.Last3 == "SVO",]$Eun.Classified <- "VerbMedial"
alldata[alldata$Eun.Last3 == "OVS",]$Eun.Classified <- "VerbMedial"
alldata[is.na(alldata$Mig.Last3),]$Mig.Last3 <- "NoCode"
alldata$Mig.Classified <- "Unclassified"
alldata[alldata$Mig.Last3 == "SOV",]$Mig.Classified <- "VerbLateral"
alldata[alldata$Mig.Last3 == "OSV",]$Mig.Classified <- "VerbLateral"
alldata[alldata$Mig.Last3 == "VSO",]$Mig.Classified <- "VerbLateral"
alldata[alldata$Mig.Last3 == "VOS",]$Mig.Classified <- "VerbLateral"
alldata[alldata$Mig.Last3 == "V(OS)",]$Mig.Classified <- "VerbLateral"
alldata[alldata$Mig.Last3 == "V(SO)",]$Mig.Classified <- "VerbLateral"
alldata[alldata$Mig.Last3 == "(SO)V",]$Mig.Classified <- "VerbLateral"
alldata[alldata$Mig.Last3 == "(OS)V",]$Mig.Classified <- "VerbLateral"
alldata[alldata$Mig.Last3 == "SVO",]$Mig.Classified <- "VerbMedial"
alldata[alldata$Mig.Last3 == "OVS",]$Mig.Classified <- "VerbMedial"

kappa2(alldata[,c('Mig.Classified','Eun.Classified')]) #Not sure about the weight paradigm! but this matches MS
alldata$Classified.WO.Compare <- alldata$Mig.Classified == alldata$Eun.Classified
mean(alldata$Classified.WO.Compare)


#And spatial-Katy agreement
katydata <- alldata[!is.na(alldata$Spatial.Cue.Katy),]
kappa2(katydata[,c('Spatial.Cue.Katy','Spatial.Final.Decision')])
katydata$Classified.Spatial.Compare <- katydata$Spatial.Cue.Katy == katydata$Spatial.Final.Decision
mean(katydata$Classified.Spatial.Compare)

######
# Column cleanup and renaming
#Now drop all the preliminary codings & 'mush' columns, leaving us with just Final.WordOrder.Clean and Final.Spatial.Clean
#And give them slightly more transparent names...

alldata <- alldata[,c("Subject","GestureCondition","Trial.Number","Object.Type","Event","Used.ASL", "Final.Long", "Final.Clean","WordOrder.Classified","Spatial.Final.Decision","Clipped.Movie.File")]     
names(alldata) <- c("Subject", "GestureCondition","Trial.Number", "Object.Type", "Sentence", "Used.ASL", "Final.Full.WordOrder", "WordOrder","WordOrder.Classified","SpatialCue","Clipped.Movie.File" )

#Make some columns easier for humans to read...
alldata$SpatialCue <- as.character(alldata$SpatialCue)
alldata[is.na(alldata$SpatialCue),]$SpatialCue <- "NA"
alldata[alldata$SpatialCue == 1,]$SpatialCue <- "Spatial.Present"
alldata[alldata$SpatialCue == 0,]$SpatialCue <- "Spatial.Absent"



################
#Uploading the embodiment coding (redone from the 'E/no E' style codes), plus participant-level averages for graphing
#################
#'Embodiment' (presence of body-based sign) - added coding and recalculating orders in the way relevant to Hall's theory (deal w edge cases)
# We want to check if Embodiment (in the second task) made a difference for SOV use For this, read in the new Embodiment coding 
#that MS did

embodiment_data <- read.csv(paste0(directory, "/EmbodimentHallRecode.csv"), header = TRUE)

#drop some duplicate columns we dont' need...
embodiment_data <- embodiment_data[,c("Clipped.Movie.File","Trial.Number", "Agent.Embod","Verb.Embod","Patient.Embod")]

alldata <- merge(alldata, embodiment_data, by=c("Clipped.Movie.File","Trial.Number"),all.X=TRUE, all.y=FALSE)
alldata <- na.omit(alldata) #a recalcitrate NA line...

#Did Embodiment change across the 2 conditions? Check Agent Verb Patient
table(alldata$Agent.Embod, alldata$Object.Type, alldata$GestureCondition)
table(alldata$Verb.Embod, alldata$Object.Type, alldata$GestureCondition)
table(alldata$Patient.Embod, alldata$Object.Type, alldata$GestureCondition)

#Recode a typo :)
alldata[alldata$Patient.Embod == "o",]$Patient.Embod <- 0

#Add New WordOrder classifications The distinction is whether s is the last
#entity before the v., rather than whether the S and O are on the same side
#of the V. 

alldata[is.na(alldata$WordOrder),]$WordOrder <- "Unclassified"
alldata$WordOrder.Embod.Classified <- "Unclassified"
alldata[alldata$WordOrder == "SOV",]$WordOrder.Embod.Classified <- "NonAdjacent"
alldata[alldata$WordOrder == "OSV",]$WordOrder.Embod.Classified <- "Adjacent"
alldata[alldata$WordOrder == "VSO",]$WordOrder.Embod.Classified <- "NonAdjacent"
alldata[alldata$WordOrder == "VOS",]$WordOrder.Embod.Classified <- "NonAdjacent"
#Parenthesis cases
alldata[alldata$WordOrder == "V(OS)",]$WordOrder.Embod.Classified <- "NonAdjacent"
alldata[alldata$WordOrder == "V(SO)",]$WordOrder.Embod.Classified <- "NonAdjacent"
alldata[alldata$WordOrder == "(SO)V",]$WordOrder.Embod.Classified <- "NonAdjacent"
alldata[alldata$WordOrder == "(OS)V",]$WordOrder.Embod.Classified <- "NonAdjacent"

alldata[alldata$WordOrder == "SVO",]$WordOrder.Embod.Classified <- "Adjacent"
alldata[alldata$WordOrder == "OVS",]$WordOrder.Embod.Classified <- "NonAdjacent"

######
# Final subject inclusion/checking
# (and find out How many items did each person complete?)

#Make sure the dropped people really got dropped, they snuck back in during item comparisons...
subtable <- subtable[c("Participant","To.include")]
names(subtable) <- c("Subject","ToInclude.Participant")
alldata <- merge(alldata, subtable, by=c("Subject"))
alldata <- alldata[alldata$ToInclude == 1,]

numSigns <- aggregate(alldata$WordOrder, by=list(alldata$Subject),length)
#Yay! Everyone did all the trials!


#Print out a tidy data table for people who don't want to debug blind coding/merging steps.
tidydir <- '/Users/mekline/Dropbox/_Projects/Gesture/Gesture-Casemark Repo/Tidy data/' #CHANGE THIS IF RUNNING ON NOT MY LAPTOP ##LINE TO REDACT FOR PEER REVIEW
write.csv(alldata, file = paste0(tidydir, "alldata_tidy.csv"))



###### START HERE TO USE TIDY DATA INSTEAD OF FULL CODER RECONCILIATION (uncomment the next line)
 alldata <- read.csv(paste0(tidydir, "alldata_tidy.csv"), header = TRUE)

######
#Final *item* inclusion (this was done along the way in previous tests, which was probably incorrect). We do this
#by dropping the small # of items that couldn't be coded on the 3 key dimensions.

alldata <- alldata[!(alldata$WordOrder.Classified == "Unclassified"),] #Word order
alldata <- alldata[!(alldata$SpatialCue == "?"),] #Casemarking
#For RCP/Embodiment, we actually care about the union of P and V embodiment
alldata <- alldata[!(alldata$Verb.Embod == "?"),]
alldata <- alldata[!(alldata$Patient.Embod == "?"),]
alldata$PV.Embod <- (as.numeric(as.character(alldata$Verb.Embod)) == 1) & (as.numeric(as.character(alldata$Patient.Embod)) == 1)

#Optionally, drop non SuperGoodResponses (anything with word order oddities; we include here the ~125 items but it doesn't effect results materially)
#To do this, run the descriptives, then come back up here and uncomment the following.
#alldata <- alldata[alldata$SuperGoodResponse == "Yes",]

#################################################################
## REPORT DESCRIPTIVES

#Report S counts
length(unique(alldata$Subject))

#Report counts of SOV versus SVO instances in Free and Hand conditions
table(alldata$WordOrder.Classified, alldata$Object.Type, alldata$GestureCondition)

#And when were spatial cues actually produced?
table(alldata$SpatialCue, alldata$WordOrder.Classified, alldata$Object.Type)

#Let's look just at the second experiment to see if they really produced spatial info as instructed...
instructiondata <- alldata[alldata$GestureCondition == "Case",]
table(instructiondata$SpatialCue, instructiondata$WordOrder.Classified, instructiondata$Object.Type)

#What about early casemarkers in task ?
noinstructiondata <- alldata[alldata$GestureCondition == "Free",]
table(noinstructiondata$SpatialCue, noinstructiondata$WordOrder.Classified, noinstructiondata$Object.Type)


#More descriptives of responses...
#How many items are just an S, O, and V?
#Does Long not equal Clean?
#Does type equal Unclassified? 191
#Parentheses?
nrow(alldata[alldata$WordOrder == "(SO)V",]) #The gesture string has simultaneous S and O, then V

#Mark all of these in case anyone is worried about our data coding (we keep all, but option to drop SuperGoodResponse=='No')
#If this isn't working, it's because the WordORder columns have been made into factors.  as.character() them)
alldata$SuperGoodResponse <- "Yes"
alldata[as.character(alldata$Final.Full.WordOrder) != as.character(alldata$WordOrder),]$SuperGoodResponse <- "No"
alldata[alldata$WordOrder.Classified == "Unclassified",]$SuperGoodResponse <- "No"
alldata[alldata$WordOrder == "(SO)V",]$SuperGoodResponse <- "No"

#Look here if you're curious about weird word orders that we classified as verb medial or lateral!
#Uncomment to see the full list of 'oddball' gesture orders produced
#foo <- alldata[as.character(alldata$Final.Full.WordOrder) != as.character(alldata$WordOrder) & alldata$WordOrder.Classified != "Unclassified",]$Final.Full.WordOrder
#table(foo)


####
# Add scores-per-participant. (Need this to make the bootstrapped confidence intervals for the graphs...)
###

#Make scores for each participant
alldata$ChoseLateral <- 0
alldata[alldata$WordOrder.Classified == "VerbLateral",]$ChoseLateral <- 1

ParticipantScores <- aggregate(alldata$ChoseLateral, by=list(alldata$Subject, alldata$Object.Type, alldata$GestureCondition), mean.na.rm)
names(ParticipantScores) <- c("Subject", "Object.Type", "GestureCondition", "ChoseLateral")

#Table for scores too
with(ParticipantScores, tapply(ChoseLateral, list(Object.Type, GestureCondition), mean, na.rm=TRUE), drop=TRUE)

#Time for bootstrapped confidence intervals around the means of the 4 conditions! (for the graphs)
PersonFree.boot.mean = bootstrap(ParticipantScores[ParticipantScores$Object.Type=="Person" & ParticipantScores$GestureCondition=="Free",]$ChoseLateral, 1000, mean)
quantile(PersonFree.boot.mean$thetastar, c(0.025, 0.975))
PersonHand.boot.mean = bootstrap(ParticipantScores[ParticipantScores$Object.Type=="Person" & ParticipantScores$GestureCondition=="Case",]$ChoseLateral, 1000, mean)
quantile(PersonHand.boot.mean$thetastar, c(0.025, 0.975))
ObjectFree.boot.mean = bootstrap(ParticipantScores[ParticipantScores$Object.Type=="Object" & ParticipantScores$GestureCondition=="Free",]$ChoseLateral, 1000, mean)
quantile(ObjectFree.boot.mean$thetastar, c(0.025, 0.975))
ObjectHand.boot.mean = bootstrap(ParticipantScores[ParticipantScores$Object.Type=="Object" & ParticipantScores$GestureCondition=="Case",]$ChoseLateral, 1000, mean)
quantile(ObjectHand.boot.mean$thetastar, c(0.025, 0.975))

#And scores on Spatial stuff!

alldata$Casemarked <- 0
alldata[alldata$SpatialCue == "Spatial.Present",]$Casemarked <- 1

SpatialScores <- aggregate(alldata$Casemarked, by=list(alldata$Subject, alldata$WordOrder.Classified, alldata$GestureCondition), mean.na.rm)
names(SpatialScores) <- c("Subject", "WordOrder.Classified", "GestureCondition", "Casemarked")

#Table for scores too
with(SpatialScores, tapply(Casemarked, list(WordOrder.Classified, GestureCondition), mean, na.rm=TRUE), drop=TRUE)

#Time for bootstrapped confidence intervals around the means of the 4 conditions!
PersonFree.boot.mean = bootstrap(SpatialScores[SpatialScores$WordOrder.Classified=="VerbLateral" & SpatialScores$GestureCondition=="Free",]$Casemarked, 1000, mean)
quantile(PersonFree.boot.mean$thetastar, c(0.025, 0.975))
PersonHand.boot.mean = bootstrap(SpatialScores[SpatialScores$WordOrder.Classified=="VerbLateral" & SpatialScores$GestureCondition=="Case",]$Casemarked, 1000, mean)
quantile(PersonHand.boot.mean$thetastar, c(0.025, 0.975))
ObjectFree.boot.mean = bootstrap(SpatialScores[SpatialScores$WordOrder.Classified=="VerbMedial" & SpatialScores$GestureCondition=="Free",]$Casemarked, 1000, mean)
quantile(ObjectFree.boot.mean$thetastar, c(0.025, 0.975))
ObjectHand.boot.mean = bootstrap(SpatialScores[SpatialScores$WordOrder.Classified=="VerbMedial" & SpatialScores$GestureCondition=="Case",]$Casemarked, 1000, mean)
quantile(ObjectHand.boot.mean$thetastar, c(0.025, 0.975))


#########################################
##STATISTICAL TESTS!!
#########################################

#Factorizing everything for the glmers
alldata$WordOrder.Classified <- as.factor(alldata$WordOrder.Classified)
alldata$Object.Type <- as.factor(alldata$Object.Type)
alldata$SpatialCue <- as.factor(alldata$SpatialCue)
alldata$GestureCondition <- as.factor(alldata$GestureCondition)

freedata <- alldata[alldata$GestureCondition == "Free",]
handdata <- alldata[alldata$GestureCondition == "Case",]
#sometimes they de-factor here:p
freedata$WordOrder.Classified <- as.factor(freedata$WordOrder.Classified)
handdata$WordOrder.Classified <- as.factor(handdata$WordOrder.Classified)
freedata$Object.Type <- as.factor(freedata$Object.Type)
handdata$Object.Type <- as.factor(handdata$Object.Type)

########
#Within Experiments- were people more likely to use SVO with Animate?
########

#For all tests, we show the maximal random slopes/intercepts, then the model we actually ran if any of the models in the test didn't converge.
#free_model <- glmer(WordOrder.Classified ~ Object.Type  + (Object.Type|Subject) + (1|Sentence), data=freedata, family="binomial") 
free_model <- glmer(WordOrder.Classified ~ Object.Type  + (1|Subject) + (1|Sentence), data=freedata, family="binomial") 
free_nofix <- glmer(WordOrder.Classified ~ 1 + (1|Subject) + (1|Sentence), data=freedata, family="binomial")
anova(free_model,free_nofix)

#Text to report in paper: "This ordering difference was statistically significant
#in a two-tailed, mixed-effects logistic regression that included random 
#participant slopes and item intercepts, X2 = 22.56, df = 4, p < .001"

hand_model <- glmer(WordOrder.Classified ~ Object.Type  + (Object.Type|Subject) + (1|Sentence), data=handdata, family="binomial")
hand_nofix <- glmer(WordOrder.Classified ~  1 + (Object.Type|Subject) + (1|Sentence), data=handdata, family="binomial")
anova(hand_model,hand_nofix)


#Text: The difference in verb-final gestures for animate and inanimate patients was not statistically 
#significant for this task (X2 = 0.07, df=6, p = .79).

#######
#Between Experiments! - was there an interaction between Experiment and the above?
#######

#word_order_model <- glmer(WordOrder.Classified ~ Object.Type*GestureCondition  + (Object.Type*GestureCondition|Subject) + (GestureCondition|Sentence), data=alldata, family="binomial")
word_order_model <- glmer(WordOrder.Classified ~ Object.Type*GestureCondition  + (1|Subject) + (1|Sentence), data=alldata, family="binomial")

#Check (fixed) effects by removal!
#Interaction?
word_order_model2 <- glmer(WordOrder.Classified ~ Object.Type + GestureCondition  + (1|Subject) + (1|Sentence), data=alldata, family="binomial")
anova(word_order_model, word_order_model2)

#Animacy?
word_order_model3 <- glmer(WordOrder.Classified ~ GestureCondition  + (1|Subject) + (1|Sentence), data=alldata, family="binomial")
anova(word_order_model2, word_order_model3)

#GestureCondition?
word_order_model4 <- glmer(WordOrder.Classified ~ Object.Type  + (1|Subject) + (1|Sentence), data=alldata, family="binomial")
anova(word_order_model2, word_order_model4)

#Text to report: Analyzing the two gesture tasks together, we found a significant task by animacy 
#interaction (X2 = 16.91, df = 6, p < .001), as well as main effects of animacy (X2 = 18.22, df = 5, p < .001) 
#and gesture task (X2 = 184.1, df = 5, p < .001).

##########
# Spatial info/casemarking analyses - does it change between experiments?
##########
#case_model <- glmer(Casemarked ~ Object.Type*GestureCondition  + (GestureCondition*Object.Type|Subject) + (GestureCondition|Sentence), data=alldata, family="binomial")
case_model <- glmer(Casemarked ~ Object.Type*GestureCondition  + (GestureCondition|Subject) + (1|Sentence), data=alldata, family="binomial")

#Check effects by removal: Interaction?
case_model2 <- glmer(Casemarked ~ Object.Type+GestureCondition  + (GestureCondition|Subject) + (1|Sentence), data=alldata, family="binomial")
anova(case_model, case_model2)

#GestureCondition?
case_model3 <- glmer(Casemarked ~ Object.Type  + (GestureCondition|Subject) + (1|Sentence), data=alldata, family="binomial")
anova(case_model2, case_model3)

#Animacy?
case_model4 <- glmer(Casemarked ~ GestureCondition  + (1|Subject) + (1|Sentence), data=alldata, family="binomial")
anova(case_model2, case_model4)

# Text to report: Our intended ‘case marking’ manipulation was successful: after receiving 
#instructions before the second gesture task, participants produced many more gestures with 
#spatial information overall (X2 = 43.05, df = 7, p < .001). 
#HoweverXXXX?????, there was a small but significant interaction between task and animacy (X2 =6.29, df = 8, p < .05) 

###############
# RCP- does it change between experiments?
###############
animdata <- alldata[alldata$Object.Type == "Person",] #PV.Embod/RCP gestures don't exist for inanimate patient sentences! (nobody embodies inanimate patients)

embod_model <- glmer(PV.Embod ~ GestureCondition  + (GestureCondition|Subject) + (GestureCondition|Sentence), data=animdata, family="binomial")
embod_model_nofix <- glmer(PV.Embod ~ 1  + (GestureCondition|Subject) + (GestureCondition|Sentence), data=animdata, family="binomial")
anova(embod_model, embod_model_nofix)

#Within animate-animate trials, RCP gestures declined significantly from the first gesture task to the second. 
#(X2 = 22.24, df = 8, p < .001).

##################
# Word order and casemarking: Across tasks, how were these 2 features of the responses related? (limit to animates!)
##################
free_animdata <- animdata[animdata$GestureCondition == "Free",]
hand_animdata <- animdata[animdata$GestureCondition == "Case",]

spatialvsorder_free <- glmer(WordOrder.Classified ~ Casemarked  + (Casemarked|Subject) + (Casemarked|Sentence), data=free_animdata, family="binomial")
spatialvsorder_free2 <- glmer(WordOrder.Classified ~ 1  + (Casemarked|Subject) + (Casemarked|Sentence), data=free_animdata, family="binomial")
anova(spatialvsorder_free, spatialvsorder_free2)

spatialvsorder_hand <- glmer(WordOrder.Classified ~ Casemarked  + (Casemarked|Subject) + (Casemarked|Sentence), data=hand_animdata, family="binomial")
spatialvsorder_hand2 <- glmer(WordOrder.Classified ~ 1  + (Casemarked|Subject) + (Casemarked|Sentence), data=hand_animdata, family="binomial")
anova(spatialvsorder_hand, spatialvsorder_hand2)

#Between exp
#spatialvsorder <- glmer(WordOrder.Classified ~ Casemarked*GestureCondition + (Casemarked*GestureCondition|Subject) + (Casemarked*GestureCondition|Sentence), data=animdata, family="binomial")
spatialvsorder <- glmer(WordOrder.Classified ~ Casemarked*GestureCondition + (Casemarked|Subject) + (1|Sentence), data=animdata, family="binomial")
spatialvsorder2 <- glmer(WordOrder.Classified ~ Casemarked+GestureCondition + (Casemarked|Subject) + (1|Sentence), data=animdata, family="binomial")
anova(spatialvsorder, spatialvsorder2)

#In both experiments, participants were more likely to gesture in a SOV-type order when they 
#also used spatial information; No-instruction task X2=18.50, df =8, p < 0.001; 
#Case marking instruction task X2= 7.55, df=8, p < 0.01). There was no interaction between task 
#and case marking on the gesture order used (X2=0.05, df =8, p =0.82). 

##################
# Word order and embodiment: Across tasks, how were these 2 features of the responses related?
##################

#bodyvsorder_free <- glmer(WordOrder.Classified ~ PV.Embod  + (PV.Embod|Subject) + (PV.Embod|Sentence), data=free_animdata, family="binomial")
bodyvsorder_free <- glmer(WordOrder.Classified ~ PV.Embod  + (PV.Embod|Subject) + (1|Sentence), data=free_animdata, family="binomial")
bodyvsorder_free2 <- glmer(WordOrder.Classified ~ 1  + (PV.Embod|Subject) + (1|Sentence), data=free_animdata, family="binomial")
anova(bodyvsorder_free, bodyvsorder_free2)

bodyvsorder_hand <- glmer(WordOrder.Classified ~ PV.Embod  + (PV.Embod|Subject) + (PV.Embod|Sentence), data=hand_animdata, family="binomial")
bodyvsorder_hand2 <- glmer(WordOrder.Classified ~ 1  + (PV.Embod|Subject) + (PV.Embod|Sentence), data=hand_animdata, family="binomial")
anova(bodyvsorder_hand, bodyvsorder_hand2)

#across exp
#bodyvsorder <- glmer(WordOrder.Classified ~ PV.Embod*GestureCondition  + (PV.Embod|Subject) + (PV.Embod|Sentence), data=animdata, family="binomial")
bodyvsorder <- glmer(WordOrder.Classified ~ PV.Embod*GestureCondition  + (PV.Embod|Subject) + (1|Sentence), data=animdata, family="binomial")
bodyvsorder2 <- glmer(WordOrder.Classified ~ PV.Embod+GestureCondition  + (PV.Embod|Subject) + (1|Sentence), data=animdata, family="binomial")
anova(bodyvsorder, bodyvsorder2)

#RCP was also closely related to gesture order: participants were less likely to use an SOV-type order 
#when role conflict potential was present.  This difference was significant in the 
#free gesture task (X2= 6.47, df=6, p < .001), and in the
#the case-marking instruction task (X2 = 3.98, df=8, p < 0.05); there was a significant interaction 
#between role conflict and task (X2 = 3.97, df= 8, p< 0.05).

##################
# Casemarking and embodiment - are they related?
##################

#embod_case_model <- glmer(PV.Embod ~ Casemarked  + (Casemarked|Subject) + (Casemarked|Sentence), data=animdata, family="binomial")
embod_case_model <- glmer(PV.Embod ~ Casemarked  + (Casemarked|Subject) + (1|Sentence), data=animdata, family="binomial")
embod_case_model2 <- glmer(PV.Embod ~ 1  + (Casemarked|Subject) + (1|Sentence), data=animdata, family="binomial")
anova(embod_case_model, embod_case_model2)

#the presence of RCP and spatial coding were negatively correlated ((X2 = 19.36, df=6, p < .001). 


#########
# GRAPHS
#########

ParticipantScores <- aggregate(alldata$ChoseLateral, by=list(alldata$Subject, alldata$Object.Type, alldata$GestureCondition), mean.na.rm)
names(ParticipantScores) <- c("Subject", "Object.Type", "GestureCondition", "ChoseLateral")

#Time for bootstrapped confidence intervals around the means of the 4 conditions!
PersonFree.boot.mean = bootstrap(ParticipantScores[ParticipantScores$Object.Type=="Person" & ParticipantScores$GestureCondition=="Free",]$ChoseLateral, 1000, mean)
quantile(PersonFree.boot.mean$thetastar, c(0.025, 0.975))
PersonHand.boot.mean = bootstrap(ParticipantScores[ParticipantScores$Object.Type=="Person" & ParticipantScores$GestureCondition=="Case",]$ChoseLateral, 1000, mean)
quantile(PersonHand.boot.mean$thetastar, c(0.025, 0.975))
ObjectFree.boot.mean = bootstrap(ParticipantScores[ParticipantScores$Object.Type=="Object" & ParticipantScores$GestureCondition=="Free",]$ChoseLateral, 1000, mean)
quantile(ObjectFree.boot.mean$thetastar, c(0.025, 0.975))
ObjectHand.boot.mean = bootstrap(ParticipantScores[ParticipantScores$Object.Type=="Object" & ParticipantScores$GestureCondition=="Case",]$ChoseLateral, 1000, mean)
quantile(ObjectHand.boot.mean$thetastar, c(0.025, 0.975))


GraphScores <- aggregate(ParticipantScores$ChoseLateral, by=list(ParticipantScores$Object.Type, ParticipantScores$GestureCondition), mean.na.rm)
names(GraphScores) <- c("Object.Type", "GestureCondition", "ChoseLateral")
GraphScores$errorLow = 0
GraphScores$errorHigh = 0
GraphScores[GraphScores$Object.Type == "Person" & GraphScores$GestureCondition == "Free",]$errorLow = quantile(PersonFree.boot.mean$thetastar, 0.025)
GraphScores[GraphScores$Object.Type == "Person" & GraphScores$GestureCondition == "Free",]$errorHigh = quantile(PersonFree.boot.mean$thetastar, 0.975)
GraphScores[GraphScores$Object.Type == "Person" & GraphScores$GestureCondition == "Case",]$errorLow = quantile(PersonHand.boot.mean$thetastar, 0.025)
GraphScores[GraphScores$Object.Type == "Person" & GraphScores$GestureCondition == "Case",]$errorHigh = quantile(PersonHand.boot.mean$thetastar, 0.975)
GraphScores[GraphScores$Object.Type == "Object" & GraphScores$GestureCondition == "Free",]$errorLow = quantile(ObjectFree.boot.mean$thetastar, 0.025)
GraphScores[GraphScores$Object.Type == "Object" & GraphScores$GestureCondition == "Free",]$errorHigh = quantile(ObjectFree.boot.mean$thetastar, 0.975)
GraphScores[GraphScores$Object.Type == "Object" & GraphScores$GestureCondition == "Case",]$errorLow = quantile(ObjectHand.boot.mean$thetastar, 0.025)
GraphScores[GraphScores$Object.Type == "Object" & GraphScores$GestureCondition == "Case",]$errorHigh = quantile(ObjectHand.boot.mean$thetastar, 0.975)



GraphScores$ObLabel <- ""
GraphScores[GraphScores$Object.Type == "Object",]$ObLabel <- "Inanimate patient"
GraphScores[GraphScores$Object.Type == "Person",]$ObLabel <- "Animate patient"
GraphScores$ExpLabel <- ""
GraphScores[GraphScores$GestureCondition == "Free",]$ExpLabel <- "Task 1 (Free gesture)"
GraphScores[GraphScores$GestureCondition == "Case",]$ExpLabel <- "Task 2 (Spatial instructions)"

my.cols <- brewer.pal(9, "Purples")
my.cols <- c(my.cols[6], my.cols[3])

#Fix for recalcitrant column ordering
GraphScores$ObLabel <- factor(GraphScores$ObLabel, levels = c("Inanimate patient", "Animate patient"))
GraphScores$ExpLabel <- factor(GraphScores$ExpLabel, levels = c("Task 1 (Free gesture)", "Task 2 (Spatial instructions)"))

ggplot(data=GraphScores, aes(x=ExpLabel, y=ChoseLateral, fill=ObLabel)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=errorLow, ymax=errorHigh), colour="black", width=.1, position=position_dodge(.9)) +
  scale_fill_manual(values=my.cols) +
  coord_cartesian(ylim=c(0,1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1))+
  xlab('') +
  ylab('proportion of SOV-type gesture orders') +
  theme_bw() +
  theme(legend.title=element_blank())

ggsave('Fig3_gesture_condition_vs_order.jpg')

##
#responses with case vs with role conflict orders...
PeopleScores <- table(peopledata$PV.Embod, peopledata$SpatialCue)
GraphScores <- as.data.frame(PeopleScores)
names(GraphScores) <- c("PV.Embod", "SpatialCue", "count")

GraphScores$EmbodLabel <- ""
GraphScores[GraphScores$PV.Embod == TRUE,]$EmbodLabel <- "Gesture order with RCP"
GraphScores[GraphScores$PV.Embod == FALSE,]$EmbodLabel <- "Gesture order without RCP"

GraphScores$SpatLabel <- ""
GraphScores[GraphScores$SpatialCue == "Spatial.Absent",]$SpatLabel <- "Gesture with no spatial encoding"
GraphScores[GraphScores$SpatialCue == "Spatial.Present",]$SpatLabel <- "Gesture with spatial encoding"

my.cols <- brewer.pal(9, "Greens")
my.cols <- c(my.cols[6], my.cols[3])

#Fix for recalcitrant column ordering
GraphScores$EmbodLabel <- factor(GraphScores$EmbodLabel, levels = c("Gesture order with RCP", "Gesture order without RCP"))
GraphScores$SpatLabel <- factor(GraphScores$SpatLabel, levels = c("Gesture with spatial encoding","Gesture with no spatial encoding"))

ggplot(data=GraphScores, aes(x=SpatLabel, y=count, fill=EmbodLabel)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_manual(values=my.cols) +
  xlab('') +
  ylab('Number of gesture sequences produced') +
  theme_bw() +
  theme(legend.title=element_blank())

ggsave('Fig4_gesture_space_vs_order.jpg')


########
#Code snippets from MS, no longer in use...
######## 

###
# The below prints out the presence of various other punctuation in Eun and Mig coding as individual 0/1 columns
###


# #Insert blank column for organization
# gestable$OldPunct <- ''

# #Add columns that say if there was punctuation in the initial coding
# oldhascomma = NULL
# for (e in gestable$Word.Order) {
# 	comma <- unlist(strsplit(e, "[,]"))
# 	if (length(comma) == 1) {oldhascomma <- append(oldhascomma, FALSE)}
# 	else {oldhascomma <- append(oldhascomma, TRUE)}
# 	}
# gestable$OldHasComma <- oldhascomma

# oldhasslash = NULL
# for (f in gestable$Word.Order) {
# 	slash <- unlist(strsplit(f, "[/]"))
# 	if (length(slash) == 1) {oldhasslash <- append(oldhasslash, FALSE)}
# 	else {oldhasslash <- append(oldhasslash, TRUE)}
# 	}
# gestable$OldHasSlash <- oldhasslash

# oldhaspar = NULL
# for (g in gestable$Word.Order) {
# 	par <- unlist(strsplit(g, "[(]"))
# 	if (length(par) == 1) {oldhaspar <- append(oldhaspar, FALSE)}
# 	else {oldhaspar <- append(oldhaspar, TRUE)}
# 	}
# gestable$OldHasPar <- oldhaspar

# oldhasques = NULL
# for (h in gestable$Word.Order) {
# 	ques <- unlist(strsplit(h, "[(]"))
# 	if (length(ques) == 1) {oldhasques <- append(oldhasques, FALSE)}
# 	else {oldhasques <- append(oldhasques, TRUE)}
# 	}
# gestable$OldHasQues <- oldhasques

# #Insert blank column for organization
# gestable$NewPunct <- ''

# #Add columns that say if there was punctuation in the recoding
# newhascomma = NULL
# for (e in gestable$Word.Order.Recode) {
# 	comma <- unlist(strsplit(e, "[,]"))
# 	if (length(comma) == 1) {newhascomma <- append(newhascomma, FALSE)}
# 	else {newhascomma <- append(newhascomma, TRUE)}
# 	}
# gestable$NewHasComma <- newhascomma

# newhasslash = NULL
# for (f in gestable$Word.Order.Recode) {
# 	slash <- unlist(strsplit(f, "[/]"))
# 	if (length(slash) == 1) {newhasslash <- append(newhasslash, FALSE)}
# 	else {newhasslash <- append(newhasslash, TRUE)}
# 	}
# gestable$NewHasSlash <- newhasslash

# newhaspar = NULL
# for (g in gestable$Word.Order.Recode) {
# 	par <- unlist(strsplit(g, "[(]"))
# 	if (length(par) == 1) {newhaspar <- append(newhaspar, FALSE)}
# 	else {newhaspar <- append(newhaspar, TRUE)}
# 	}
# gestable$NewHasPar <- newhaspar

# newhasques = NULL
# for (h in gestable$Word.Order.Recode) {
# 	ques <- unlist(strsplit(h, "[(]"))
# 	if (length(ques) == 1) {newhasques <- append(newhasques, FALSE)}
# 	else {newhasques <- append(newhasques, TRUE)}
# }
# gestable$NewHasQues <- newhasques



# #After it all, create a new table with all this information
# write.csv(gestable, file = paste0(directory, "/AllGestureWithAnalysis.csv"))

###
# End of punctuation-marking section
###

#Calculate Cohen's Kappa Using the IRR Package for Literal Compare
#firsttwo = matrix[2,]
#firsttwo$gestable.Word.Order.Recode = gestable$Word.Order.Recode

#eleckappa <- kappa2(as.character(firsttwo))

#print(paste0('Literal Kappa (irr): ', eleckappa))



#Add column that checks for blank cells in old coding
#gestable$Old.Is.Blank = (nchar(as.character(gestable$Word.Order)) == 0)



# noagreesubnum = gestable[gestable$Literal.Compare == 0,]$Subject
# noagreecond = gestable[gestable$Literal.Compare == 0,]$GestureCondition
# noagreetrial = gestable[gestable$Literal.Compare == 0,]$Trial.Number
# noagree1 = gestable[gestable$Literal.Compare == 0,]$Word.Order
# noagree2 = gestable[gestable$Literal.Compare == 0,]$Word.Order.Recode
# noagreetable = data.frame(factor(noagreesubnum))
# noagreetable$Gest.Cond = noagreecond
# noagreetable$TrialNum = factor(noagreetrial)
# noagreetable$Init.Coding = noagree1
# noagreetable$Recode = noagree2



#Get the last chunk of string after the Comma (EL's coding)
#oldCsplit = NULL
#for (i in as.character(gestable$Word.Order)) {
	#splt = unlist(strsplit(i, "[,]"))
	#lst = splt[length(splt)]
	#oldCsplit <- append(oldCsplit, lst)}
#gestable$Eun.C.Split <- oldCsplit



#Get the last chunk of string after the Comma (MS's coding)
#newCsplit = NULL
#for (j in as.character(gestable$Word.Order.Recode)) {
	#splt = unlist(strsplit(j, "[,]"))
	#lst = splt[length(splt)]
	#newCsplit <- append(newCsplit, lst)}
#gestable$Mig.C.Split <- newCsplit



#Now we repeat the process and remove slashes from comma-spliced strings and vice-versa.
#Initialize counters again and insert a blank column.
#gestable$Blank2 <- ''
#i = 0
#j = 0
#k = 0
#l = 0

#Get the last chunk of string after the Slash (EL's coding)
#oldCSsplit = NULL
#for (i in as.character(gestable$Eun.C.Split)) {
	#splt = unlist(strsplit(i, "[/]"))
	#lst = splt[length(splt)]
	#oldCSsplit <- append(oldCSsplit, lst)}
#gestable$Eun.CS.Split <- oldCSsplit
	
#Get the last chunk of string after the Slash (EL's coding)
#oldSCsplit = NULL
#for (k in as.character(gestable$Eun.S.Split)) {
	#splt = unlist(strsplit(k, "[,]"))
	#lst = splt[length(splt)]
	#oldSCsplit <- append(oldSCsplit, lst)}
#gestable$Eun.SC.Split <- oldSCsplit

#Get the last chunk of string after the Comma (MS's coding)
#newCSsplit = NULL
#for (j in as.character(gestable$Mig.C.Split)) {
	#splt = unlist(strsplit(j, "[/]"))
	#lst = splt[length(splt)]
	#newCSsplit <- append(newCSsplit, lst)}
#gestable$Mig.CS.Split <- newCSsplit

#Get the last chunk of string after the Slash (MS's coding)
#newSCsplit = NULL
#for (l in as.character(gestable$Mig.S.Split)) {
	#splt = unlist(strsplit(l, "[,]"))
	#lst = splt[length(splt)]
	#newSCsplit <- append(newSCsplit, lst)}
#gestable$Mig.SC.Split <- newSCsplit

#Compare EL's comma and slash splits to see if identical
#gestable$EunCSComp = (as.character(gestable$Eun.CS.Split) == as.character(gestable$Eun.SC.Split))

#Compare MS's comma and slash splits to see if indentical
#gestable$MigCSComp = (as.character(gestable$Mig.CS.Split) == as.character(gestable$Mig.SC.Split))



# noagreesubnum2 = gestable[gestable$Literal.Compare == 0,]$Subject
# noagreecond2 = gestable[gestable$Literal.Compare == 0,]$GestureCondition
# noagreetrial2 = gestable[gestable$Literal.Compare == 0,]$Trial.Number
# noagree12 = gestable[gestable$Literal.Compare == 0,]$Eun.Last3
# noagree22 = gestable[gestable$Literal.Compare == 0,]$Mig.Last3
# noagreetable2 = data.frame(factor(noagreesubnum2))
# noagreetable2$Gesture.Condition = noagreecond2
# noagreetable2$TrialNum = factor(noagreetrial2)
# noagreetable2$Init.Coding = noagree12
# noagreetable2$Recode = noagree22


