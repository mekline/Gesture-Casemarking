#Full analysis for GestureCaseMarking experiment
#
#Preprocessing of blind coding data developed by Miguel Salinas. 
#
#Note: First part of this file produces output files: LiteralNoAgree.csv and Last3NoAgree.csv, the latter of
#which was then tiebroken by hand by trained RAs. This tiebreaking is stored in Last3NoAgree_Reconciliation.csv,
# which gets read in and used for subsequent analyses.
#
#Ctrl-F for DESCRIPTIVES AND STATISTICS for analyses reported in Kline et al.

#Reading in all libraries that we'll use
library(irr)
library(stringr)
library(languageR)
library(lme4)
library(multcomp)
library(binom)
library(bootstrap)
mean.na.rm <- function(x) { mean(x,na.rm=T) }
sum.na.rm <- function(x) { sum(x,na.rm=T) }
stderr <- function(x) sqrt(var(x)/length(x))

#Get directory of this file
directory = getwd()

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

# Note, Miguel did this by hand! Whoa!

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

kappa2(gestable[,c('Word.Order','Word.Order.Recode')]) #Not sure about the weight paradigm! but this matches Miguel ok

#Add column that compares coding regarding spatial cues.
gestable$SpaCue.Agree = as.character((as.numeric(gestable$Spatial.Cue) == as.numeric(gestable$Spatial.Cue.Recode)))
spacueavg = mean(as.logical(gestable$SpaCue.Agree))

####
#Serious data cleaning starts here: We want to compare strings just based on the last 3 symbols of participants'
#final gesture string. (See the document called gesture testing & coding procedures for details on this)
#This will be our main unit of analysis going forward. We'll report kappa on this, as well as breaking down
#those calculations based on what kind of disagreement was found (main takehome: SVO vs SOV is a more serious
#error than SOV vs. OSV)
####

#Get rid of intransitive responses from analysis
gestable <- gestable[ ! gestable$Type.of.Action %in% "Intransitive",]


#Insert Blank Column for Organization and Initialize counters
gestable$Blank1 <- ''
i = 0
j = 0
k = 0
l = 0
	
#Get the last chunk of string after the Slash (Eunice's coding)
oldSsplit = NULL
for (k in as.character(gestable$Word.Order)) {
	splt = unlist(strsplit(k, "[/]"))
	lst = splt[length(splt)]
	oldSsplit <- append(oldSsplit, lst)}
gestable$Eun.S.Split <- oldSsplit


#Get the last chunk of string after the Slash (Miguel's coding)
newSsplit = NULL
for (l in as.character(gestable$Word.Order.Recode)) {
	splt = unlist(strsplit(l, "[/]"))
	lst = splt[length(splt)]
	newSsplit <- append(newSsplit, lst)}
gestable$Mig.S.Split <- newSsplit


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

kappa2(gestable[,c('Eun.Last3','Mig.Last3')]) #Not sure about the weight paradigm! but this matches Miguel


#Produce table that spits out disagreement on WORD ORDER.
noagreetable2 <- gestable[gestable$Last3.Compare == 0,]
noagreetable2 <- noagreetable2[,c("Subject", "GestureCondition", "Trial.Number", "Clipped.Movie.File", "Event", "Eun.Last3", "Mig.Last3", "Initial.Coder", "Blind.Coder")]

write.csv(noagreetable2, file = paste0(directory, "/Last3NoAgree.csv"))

#Produce table that spits out disagreement on SPATIAL.
noagreetable4 <- gestable[gestable$SpaCue.Agree == 0,]
noagreetable4 <- noagreetable4[,c("Subject", "GestureCondition", "Trial.Number", "Clipped.Movie.File", "Event", "Spatial.Cue", "Spatial.Cue.Recode", "Initial.Coder", "Blind.Coder")]

write.csv(noagreetable4, file = paste0(directory, "/SpatialNoAgree.csv"))


########
# AT THIS POINT, a third coder worked with Miguel to tiebreak/resolve by discussion any video that had been coded
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
#For Spatial, FinalDecision will be the final decision from Miguel/Mitchell discussion, OR the original if there was
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
alldata[is.na(alldata$Final.Long),]$Final.Long <- alldata[is.na(alldata$Final.Long),]$Word.Order #Miguel/Eunice's original original coding!

#######
# Final agreement calculations to report!

#Classify all items as VerbMedial, VerbLateral, or Unclassified
#This also decides individual items to exclude (for not consisting of exactly one S,V,O, or for having 
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

kappa2(alldata[,c('Mig.Classified','Eun.Classified')]) #Not sure about the weight paradigm! but this matches Miguel
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

alldata <- alldata[,c("Subject","GestureCondition","Trial.Number","Object.Type","Event","Used.ASL","X.Embodied..in.some.way","Final.Long", "Final.Clean","WordOrder.Classified","Spatial.Final.Decision","Clipped.Movie.File")]     
names(alldata) <- c("Subject", "GestureCondition","Trial.Number", "Object.Type", "Sentence", "Used.ASL", "Embodiment", "Final.Full.WordOrder", "WordOrder","WordOrder.Classified","SpatialCue","Clipped.Movie.File" )

#Make some columns easier for humans to read...
alldata$SpatialCue <- as.character(alldata$SpatialCue)
alldata[is.na(alldata$SpatialCue),]$SpatialCue <- "NA"
alldata[alldata$SpatialCue == 1,]$SpatialCue <- "Spatial.Present"
alldata[alldata$SpatialCue == 0,]$SpatialCue <- "Spatial.Absent"


######
# Final item inclusion/checking
# Find out: How many items did each person complete?

#Make sure the dropped people really got dropped, they snuck back in during item comparisons...
subtable <- subtable[c("Participant","To.include")]
names(subtable) <- c("Subject","ToInclude")
alldata <- merge(alldata, subtable, by=c("Subject"))
alldata <- alldata[alldata$ToInclude == 1,]

numSigns <- aggregate(alldata$WordOrder, by=list(alldata$Subject),length)
#Yay! Everyone did all the trials!


#################################################################
#################################################################
#################################################################
##DESCRIPTIVES AND STATISTICS
#################################################################
#################################################################
#################################################################

#################################################################
## Basic descriptions (we report proportions rather than raw # of responses in the paper, see below. )

#Report S counts
length(unique(alldata$Subject))

#Counts of SOV versus SVO instances in Free and Hand conditions
table(alldata$WordOrder.Classified, alldata$Object.Type, alldata$GestureCondition)

#Counts of when spatial cues actually produced?
table(alldata$SpatialCue, alldata$WordOrder.Classified, alldata$Object.Type)

#Spatial cue counts in Gesture task 1...
#What about early casemarkers?
noinstructiondata <- alldata[alldata$GestureCondition == "Free",]
table(noinstructiondata$SpatialCue, noinstructiondata$WordOrder.Classified, noinstructiondata$Object.Type)

#....and 2
instructiondata <- alldata[alldata$GestureCondition == "Case",]
table(instructiondata$SpatialCue, instructiondata$WordOrder.Classified, instructiondata$Object.Type)


#################################################################
## Checking item classification scheme (classification to verb Medial/Nonmedial)

#How many items are just an S, O, and V?
#Does full letter seq not equal final 3?
#Was item Unclassified? 
#Parentheses?
#Mark all those in case anyone is worried about our data coding....

alldata$SuperGoodResponse <- "Yes"
alldata[alldata$Final.Full.WordOrder != alldata$WordOrder,]$SuperGoodResponse <- "No"
alldata[alldata$WordOrder.Classified == "Unclassified",]$SuperGoodResponse <- "No"
alldata[alldata$WordOrder == "(SO)V",]$SuperGoodResponse <- "No"

#Examine items that didn't fit the classifcation scheme well
foo <- alldata[alldata$Final.Full.WordOrder != alldata$WordOrder & alldata$WordOrder.Classified != "Unclassified",]$Final.Full.WordOrder
goo <- as.data.frame(foo)
goo$len <- unlist(lapply(foo, nchar))
goo <- goo[goo$len > 3,]
goo[order(goo$foo),]
#(And do some manual checking about items that might have been misclassified)
#OK: For items longer than length 3 that DID get classified, here are places we might have mistakes:
# OVOSV (1) - maybe not SOV
# SOVSVO 2 - maybe not SVO

#################################################################
#'Embodiment' (presence of body-based sign) - added coding and recalculating orders in the way relevant to Hall's theory (deal w edge cases)
# We want to check if Embodiment (in the second task)
# made a difference for SOV use (ie maybe we did an embodiment manipulation
# alongside...).  For this, read in the new Embodiment coding that Miguel
# did ~ 10/22/14

embodiment_data <- read.csv(paste0(directory, "/EmbodimentRecode.csv"), header = TRUE)

#drop some duplicate columns we dont' need...
embodiment_data <- embodiment_data[,c("Clipped.Movie.File","Trial.Number", "Agent.Embod","Verb.Embod","Patient.Embod")]

alldata <- merge(alldata, embodiment_data, by=c("Clipped.Movie.File","Trial.Number"),all.X=TRUE, all.y=FALSE)

#Did Embodiment change across the 2 conditions? Check Agent Verb Patient
table(alldata$Agent.Embod, alldata$Object.Type, alldata$GestureCondition)
table(alldata$Verb.Embod, alldata$Object.Type, alldata$GestureCondition)
table(alldata$Patient.Embod, alldata$Object.Type, alldata$GestureCondition)

#Embodicment hyp is centrally about avoiding role
#conflict between V and O.  This means a) recode WordOrder for *presence of that string*, and b)
# the Embody metric we care about is intersect(V,O)

#drop ?s!
alldata <- alldata[alldata$Agent.Embod != "?",]
alldata <- alldata[alldata$Verb.Embod != "?",]
alldata <- alldata[alldata$Patient.Embod != "?",]
alldata$Agent.Embod <- as.numeric(as.character(alldata$Agent.Embod))
alldata$Verb.Embod <- as.numeric(as.character(alldata$Verb.Embod))
alldata$Patient.Embod <- as.numeric(as.character(alldata$Patient.Embod))
alldata$PV.Embod <- (alldata$Verb.Embod == 1) & (alldata$Patient.Embod == 1)

#Add New WordOrder classifications! The distinction is whether s is the last
#entity before the v. (this is what the constraint wants)

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




#################################################################
## REPORTED PROPORTION SCORES AND CORRESPONDING MODELS
# Note - "ChoseLateral" = Verb Non-medial, ie the category that's mostly SOV.  


#Drop Unclassified items
alldata <- alldata[!(alldata$WordOrder.Classified == "Unclassified"),]
alldata <- alldata[!(alldata$SpatialCue == "?"),]
alldata[is.na(alldata$WordOrder.Classified),]$WordOrder.Classified <-"Unclassified"

#Forcing everything to be a factor, just in case
alldata$WordOrder.Classified <- as.factor(alldata$WordOrder.Classified)
alldata$Object.Type <- as.factor(alldata$Object.Type)
alldata$SpatialCue <- as.factor(alldata$SpatialCue)
alldata$GestureCondition <- as.factor(alldata$GestureCondition)

freedata <- alldata[alldata$GestureCondition == "Free",]
handdata <- alldata[alldata$GestureCondition == "Case",]
freedata$WordOrder.Classified <- as.factor(freedata$WordOrder.Classified)
handdata$WordOrder.Classified <- as.factor(handdata$WordOrder.Classified)
freedata$Object.Type <- as.factor(freedata$Object.Type)
handdata$Object.Type <- as.factor(handdata$Object.Type)


#Optionally, drop non SuperGoodResponses! Use for checking variation under classifcation scheme, no sig. differences.
#alldata <- alldata[alldata$SuperGoodResponse == "Yes",]



#################
#WORD ORDER

#Make scores for each participant
alldata$ChoseLateral <- 0
alldata[alldata$WordOrder.Classified == "VerbLateral",]$ChoseLateral <- 1

ParticipantScores <- aggregate(alldata$ChoseLateral, by=list(alldata$Subject, alldata$Object.Type, alldata$GestureCondition), mean.na.rm)
names(ParticipantScores) <- c("Subject", "Object.Type", "GestureCondition", "ChoseLateral")

#### GRAPHS AND STATS!!!! ######

# 3/20/15 Reordering stats to match paper order, calculating confints, and redoing model comparisons (adding item intercepts and rather than reporting p vals from the wrong variable coding)



###
# GRAPH #1 - (FIGURE 4) ChoseLateral by Object Type and Gesture Condition 
###
#Table for scores
with(ParticipantScores, tapply(ChoseLateral, list(Object.Type, GestureCondition), mean, na.rm=TRUE), drop=TRUE)

#CONF INTERVALS
#Bootstrapped confidence intervals around the means of the 4 conditions!
PersonFree.boot.mean = bootstrap(ParticipantScores[ParticipantScores$Object.Type=="Person" & ParticipantScores$GestureCondition=="Free",]$ChoseLateral, 1000, mean)
quantile(PersonFree.boot.mean$thetastar, c(0.025, 0.975))
PersonHand.boot.mean = bootstrap(ParticipantScores[ParticipantScores$Object.Type=="Person" & ParticipantScores$GestureCondition=="Case",]$ChoseLateral, 1000, mean)
quantile(PersonHand.boot.mean$thetastar, c(0.025, 0.975))
ObjectFree.boot.mean = bootstrap(ParticipantScores[ParticipantScores$Object.Type=="Object" & ParticipantScores$GestureCondition=="Free",]$ChoseLateral, 1000, mean)
quantile(ObjectFree.boot.mean$thetastar, c(0.025, 0.975))
ObjectHand.boot.mean = bootstrap(ParticipantScores[ParticipantScores$Object.Type=="Object" & ParticipantScores$GestureCondition=="Case",]$ChoseLateral, 1000, mean)
quantile(ObjectHand.boot.mean$thetastar, c(0.025, 0.975))

#MODELS for graph 1

#In each experiment- were people more likely to use SVO with Animate?

free_model <- lmer(WordOrder.Classified ~ Object.Type  + (Object.Type|Subject) + (1|Sentence), data=freedata, family="binomial")
free_nofix <- lmer(WordOrder.Classified ~ (Object.Type|Subject) + (1|Sentence), data=freedata, family="binomial")
anova(free_model,free_nofix)

hand_model <- lmer(WordOrder.Classified ~ Object.Type  + (Object.Type|Subject) + (1|Sentence), data=handdata, family="binomial")
hand_nofix <- lmer(WordOrder.Classified ~  (Object.Type|Subject) + (1|Sentence), data=handdata, family="binomial")
anova(hand_model,hand_nofix)

#Does that differ between experiments? (Yes)
#word_order_model <- lmer(WordOrder.Classified ~ Object.Type*GestureCondition  + (Object.Type*GestureCondition|Subject) + (1|Sentence), data=alldata, family="binomial")
#doesn't converge! Remove interaction...
#word_order_model <- lmer(WordOrder.Classified ~ Object.Type*GestureCondition  + (Object.Type+GestureCondition|Subject) + (1|Sentence), data=alldata, family="binomial")
#still no
#word_order_model <- lmer(WordOrder.Classified ~ Object.Type*GestureCondition  + (Object.Type|Subject) + (1|Sentence), data=alldata, family="binomial")
#still no
word_order_model <- lmer(WordOrder.Classified ~ Object.Type*GestureCondition  + (1|Subject) + (1|Sentence), data=alldata, family="binomial")

#Check effects by removal!

#no interaction
word_order_model2 <- lmer(WordOrder.Classified ~ Object.Type+GestureCondition  + (1|Subject) + (1|Sentence), data=alldata, family="binomial")
anova(word_order_model, word_order_model2)

#then no Animacy
word_order_model3 <- lmer(WordOrder.Classified ~ GestureCondition  + (1|Subject) + (1|Sentence), data=alldata, family="binomial")
anova(word_order_model2, word_order_model3)

#or no GestureCondition
word_order_model4 <- lmer(WordOrder.Classified ~ Object.Type  + (1|Subject) + (1|Sentence), data=alldata, family="binomial")
anova(word_order_model2, word_order_model4)


#################
#CASEMARKING

alldata$Casemarked <- 0
alldata[is.na(alldata$SpatialCue),]$SpatialCue <- "Spatial.Absent"
alldata[alldata$SpatialCue == "Spatial.Present",]$Casemarked <- 1



###
# GRAPH 3 - Spatial Cue by Animacy and Experiment
###

#Did they casemark differently depending on animacy?

AnimacySpatialScores <- aggregate(alldata$Casemarked, by=list(alldata$Subject, alldata$Object.Type, alldata$GestureCondition), mean.na.rm)
names(AnimacySpatialScores) <- c("Subject", "Object.Type", "GestureCondition", "Casemarked")
with(AnimacySpatialScores, tapply(Casemarked, list(Object.Type, GestureCondition), mean, na.rm=TRUE), drop=TRUE)

#CONF INTERVALS

#MODEL
#animacy_model <- lmer(Casemarked ~ Object.Type*GestureCondition  + (Object.Type*GestureCondition|Subject), data=alldata, family="binomial")
#noconverge
animacy_model <- lmer(Casemarked ~ Object.Type*GestureCondition  + (Object.Type|Subject) + (1|Trial.Number), data=alldata, family="binomial")

animacy_model2 <- lmer(Casemarked ~ Object.Type+GestureCondition  + (Object.Type|Subject) + (1|Trial.Number), data=alldata, family="binomial")

animacy_model3 <- lmer(Casemarked ~ GestureCondition  + (Object.Type|Subject) + (1|Trial.Number), data=alldata, family="binomial")

anova(animacy_model2,animacy_model3)

#Moral: Weak sig. by animacy, but swamped by the condition difference: casemarking carried over in a big way.

###
# Graph 4 - ChoseLateral by Spatial Cue & Experiment - WITHIN PEOPLE TRIALS ONLY
###

#OK, what about casemarking within just people trials?
peopledata <- alldata[alldata$Object.Type == 'Person',]
PeopleSpatialScores <- aggregate(peopledata$ChoseLateral, by=list(peopledata$Subject, peopledata$Casemarked, peopledata$GestureCondition), mean.na.rm)
names(PeopleSpatialScores) <- c("Subject", "Casemarked", "GestureCondition", "ChoseLateral")
with(PeopleSpatialScores, tapply(ChoseLateral, list(Casemarked, GestureCondition), mean, na.rm=TRUE), drop=TRUE)

peopledata_free <- peopledata[peopledata$GestureCondition == "Free",]
peopledata_hand <- peopledata[peopledata$GestureCondition == "Case",]

#CONF INTERVALS

#MODEL
#Check spatial pattern within JUST people trials!
spatial_model_people_free <- lmer(ChoseLateral ~ SpatialCue  + (SpatialCue|Subject), data=peopledata_free, family="binomial")
summary(spatial_model_people_free)

#noconverge# spatial_model_people_hand <- lmer(ChoseLateral ~ SpatialCue  + (SpatialCue|Subject), data=peopledata_hand, family="binomial") 
spatial_model_people_hand <- lmer(ChoseLateral ~ SpatialCue  + (1|Subject), data=peopledata_hand, family="binomial")
summary(spatial_model_people_hand)

#Diff across expeirments? No, no interaction
#spatial_model_people <- lmer(ChoseLateral ~ SpatialCue*GestureCondition  + (SpatialCue*GestureCondition|Subject), data=peopledata, family="binomial")
#Noconverge, remove slope
spatial_model_people <- lmer(ChoseLateral ~ SpatialCue*GestureCondition  + (GestureCondition|Subject), data=peopledata, family="binomial")
summary(spatial_model_people)


###################
#BODY BASED

#####
# GRAPH 5 - Body-Based by Experiment (that's it!)
#####
#Embodiment/Body-Based signs - Did this change between experiments? (Remember, no PVEmbod predicted at all for ObjType=Object)

EmbodPVScores <- aggregate(alldata$PV.Embod, by=list(alldata$Subject, alldata$Object.Type, alldata$GestureCondition), mean.na.rm)
names(EmbodPVScores) <- c("Subject", "Object.Type", "GestureCondition", "PV.Embod")
with(EmbodPVScores, tapply(PV.Embod, list(Object.Type, GestureCondition), mean, na.rm=TRUE), drop=TRUE)

#OK! So it looks like our manipulation did decrease PVembod from Free (65%) to Case (30%)

#CONFINTERVALS

#MODEL

embod_model <- lmer(PV.Embod ~ GestureCondition + (GestureCondition|Subject), data=alldata, family="binomial")
summary(embod_model)

#####
# GRAPH 6 - ChoseLateral by PV.Embod and Experiment  [FOR ALL TRIALS, MIGHT NOT REPORT SINCE ANIMATE TRIALS ARE KEY TEST]
#####


#Did embodiment affect word order? - Yes, it generally decreases SOV order, as predicted by Hall!
EmbodCLScores <- aggregate(alldata$ChoseLateral, by=list(alldata$Subject, alldata$GestureCondition, alldata$PV.Embod), mean.na.rm)
names(EmbodCLScores) <- c("Subject","GestureCondition", "PV.Embod", "ChoseLateral")
with(EmbodCLScores, tapply(ChoseLateral, list(PV.Embod, GestureCondition), mean, na.rm=TRUE), drop=TRUE)

#CONFINTS

#MODELS

#In each experiment
embod_model_free <- lmer(WordOrder.Classified ~ PV.Embod  + (PV.Embod|Subject), data=freedata, family="binomial")
summary(embod_model_free)

embod_model_hand <- lmer(WordOrder.Classified ~ PV.Embod  + (PV.Embod|Subject), data=handdata, family="binomial")
summary(embod_model_hand)

#Did this differ between Experiments?
#exp_embod_model <- lmer(ChoseLateral ~ GestureCondition*PV.Embod  + (GestureCondition*PV.Embod|Subject), data=alldata, family="binomial")
#Doesn't converge, drop a slope
exp_embod_model <- lmer(ChoseLateral ~ GestureCondition*PV.Embod  + (GestureCondition|Subject), data=alldata, family="binomial")
summary(exp_embod_model)

#Summary: Embodiment definitely predicted ordering in experiment 1, marginally in epximent 2 (NOTE, this is possibly becuase Embodiment is RARE in exp 2!!!!)

#####
# GRAPH 7 - Embodiment by Spatial Cuing (overally, showing that they trade off!)
#####

#Did casemarking actually trade off with embodiment? Let's look at how closely associated the 2 are. (Note: PVEmbod obly relevant on animates!)
peopledata <- alldata[alldata$Object.Type == 'Person',]
table(peopledata$PV.Embod, peopledata$SpatialCue)

#CONFINTS

#MODEL
embod_case_model <- lmer(PV.Embod ~ SpatialCue  + (SpatialCue|Subject), data=alldata, family="binomial")
summary(embod_case_model)

#####
# GRAPH 8 - ChoseLateral by PV.Embod and Experiment - WITHIN ANIMATE PATIENTS ONLY!!!! (Graph 8 is to Graph 6 as Graph 4 is to Graph 2)
#####

#Now ask whether embodiment predicts word order, within animates (Parallel to casemarking predicting word order above)
#Did the people who embodied their (verb/patient) gestures use more SVO (SV adj)?
PeopleEmbodScores <- aggregate(peopledata$ChoseLateral, by=list(peopledata$Subject, peopledata$PV.Embod, peopledata$GestureCondition), mean.na.rm)
names(PeopleEmbodScores) <- c("Subject", "PVEmbod", "GestureCondition", "ChoseLateral")
#Table for scores too
with(PeopleEmbodScores, tapply(ChoseLateral, list(PVEmbod, GestureCondition), mean, na.rm=TRUE), drop=TRUE)
#Yes - if you embodied, your CHoseLateral (SOV) score is lower, in both experiments.

# CONFINTS

#MODEL
#In each experiment
peopleembod_model_free <- lmer(WordOrder.Classified ~ PV.Embod  + (PV.Embod|Subject), data=peopledata_free, family="binomial")
summary(peopleembod_model_free)

peopleembod_model_hand <- lmer(WordOrder.Classified ~ PV.Embod  + (PV.Embod|Subject), data=peopledata_hand, family="binomial")
summary(peopleembod_model_hand)

#Did this differ between Experiments?
#peopleexp_embod_model <- lmer(ChoseLateral ~ GestureCondition*PV.Embod  + (GestureCondition*PV.Embod|Subject), data=peopledata, family="binomial")
#Doesn't converge, drop a slope
peopleexp_embod_model <- lmer(ChoseLateral ~ GestureCondition*PV.Embod  + (PV.Embod|Subject), data=peopledata, family="binomial")
summary(peopleexp_embod_model)






#####
# GRAPH N? (Optional, probably just state...)

#Check that word order pattern obtains with the alternate clasification (see above) to more closely match Hall assignments
peopledata$ChoseNonAdj <- 0
peopledata[is.na(peopledata$WordOrder.Embod.Classified),]$WordOrder.Embod.Classified <- "Unclassified"
peopledata[peopledata$WordOrder.Embod.Classified=="NonAdjacent",]$ChoseNonAdj <- 1 #1=SOV
PeopleEmbodScoresHall <- aggregate(peopledata$ChoseNonAdj, by=list(peopledata$Subject, peopledata$PV.Embod, peopledata$GestureCondition), mean.na.rm)
names(PeopleEmbodScoresHall) <- c("Subject", "PVEmbod", "GestureCondition", "ChoseNonAdj")
#Table for scores too
with(PeopleEmbodScoresHall, tapply(ChoseNonAdj, list(PVEmbod, GestureCondition), mean, na.rm=TRUE), drop=TRUE)
#Qualitatively similar!

###
#GRAPH 2 - ChoseLateral by Spatial Cue and Experiment [FOR ALL TRIALS LUMPING ANIMACY, MIGHT NOT REPORT SINCE ANIMATE TRIALS ARE KEY TEST]
###
SpatialScores <- aggregate(alldata$ChoseLateral, by=list(alldata$Subject, alldata$Casemarked, alldata$GestureCondition), mean.na.rm)
names(SpatialScores) <- c("Subject", "Casemarked", "GestureCondition", "ChoseLateral")

#Table for scores too
with(SpatialScores, tapply(ChoseLateral, list(Casemarked, GestureCondition), mean, na.rm=TRUE), drop=TRUE)

#CONF INTERVALS
#Time for bootstrapped confidence intervals around the means of the 4 conditions!
NoCaseFree.boot.mean = bootstrap(SpatialScores[SpatialScores$Casemarked==0 & SpatialScores$GestureCondition=="Free",]$ChoseLateral, 1000, mean)
quantile(NoCaseFree.boot.mean$thetastar, c(0.025, 0.975))
NoCaseHand.boot.mean = bootstrap(SpatialScores[SpatialScores$Casemarked==0 & SpatialScores$GestureCondition=="Case",]$ChoseLateral, 1000, mean)
quantile(NoCaseHand.boot.mean$thetastar, c(0.025, 0.975))
CaseFree.boot.mean = bootstrap(SpatialScores[SpatialScores$Casemarked==1 & SpatialScores$GestureCondition=="Free",]$ChoseLateral, 1000, mean)
quantile(CaseFree.boot.mean$thetastar, c(0.025, 0.975))
CaseHand.boot.mean = bootstrap(SpatialScores[SpatialScores$Casemarked==1 & SpatialScores$GestureCondition=="Case",]$ChoseLateral, 1000, mean)
quantile(CaseHand.boot.mean$thetastar, c(0.025, 0.975))

#MODEL

#In each experiment, were people more likely to use SOV if they provided a spatial cue?
spatial_free_model <- lmer(WordOrder.Classified ~ SpatialCue  + (SpatialCue|Subject), data=freedata, family="binomial")
summary(spatial_free_model)
spatial_hand_model <- lmer(WordOrder.Classified ~ SpatialCue  + (SpatialCue|Subject), data=handdata, family="binomial")
summary(spatial_hand_model)

#Did this differ across experiments?
#exp_spatial_model <- lmer(WordOrder.Classified ~ GestureCondition*SpatialCue  + (GestureCondition*SpatialCue|Subject), data=alldata, family="binomial")
#Doesn't converge, drop a slope
exp_spatial_model <- lmer(WordOrder.Classified ~ GestureCondition*SpatialCue  + (GestureCondition|Subject), data=alldata, family="binomial")
summary(exp_spatial_model)

#########################################
##SUBSEQUENT/EXPLORATORY ANALYSES
#########################################


######
# A new thing. Need more detail about the casemarking in the casemarked trials! Did each participant use a consistent system (eg left = agent r=patient)? To facilitate, print out
# a new file, SpatialCasemarking_Casemarked_Trials.csv, with just the (legal subject) casemarking
# trials. This will get coded by coders for *each individual sign*

orig_all_data <- read.csv(paste0(directory, "/SpatialCasemarking_AllGestureData.csv"), header = TRUE)
alldata_short <- alldata[,c("Subject", "Trial.Number","GestureCondition","SpatialCue", "Final.Full.WordOrder")]
foo <- merge(orig_all_data, alldata_short, by=c("Subject", "Trial.Number","GestureCondition"))
foo$SpatialCue.FinalDecision <- foo$SpatialCue
foo <- foo[foo$SpatialCue.FinalDecision == "Spatial.Present",]
write.csv(foo, file = paste0(directory, "/SpatialCasemarking_OnlyCaseTrials.csv"))








########
#Code snippets from miguel, no longer in use...
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



#Get the last chunk of string after the Comma (Eunice's coding)
#oldCsplit = NULL
#for (i in as.character(gestable$Word.Order)) {
	#splt = unlist(strsplit(i, "[,]"))
	#lst = splt[length(splt)]
	#oldCsplit <- append(oldCsplit, lst)}
#gestable$Eun.C.Split <- oldCsplit



#Get the last chunk of string after the Comma (Miguel's coding)
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

#Get the last chunk of string after the Slash (Eunice's coding)
#oldCSsplit = NULL
#for (i in as.character(gestable$Eun.C.Split)) {
	#splt = unlist(strsplit(i, "[/]"))
	#lst = splt[length(splt)]
	#oldCSsplit <- append(oldCSsplit, lst)}
#gestable$Eun.CS.Split <- oldCSsplit
	
#Get the last chunk of string after the Slash (Eunice's coding)
#oldSCsplit = NULL
#for (k in as.character(gestable$Eun.S.Split)) {
	#splt = unlist(strsplit(k, "[,]"))
	#lst = splt[length(splt)]
	#oldSCsplit <- append(oldSCsplit, lst)}
#gestable$Eun.SC.Split <- oldSCsplit

#Get the last chunk of string after the Comma (Miguel's coding)
#newCSsplit = NULL
#for (j in as.character(gestable$Mig.C.Split)) {
	#splt = unlist(strsplit(j, "[/]"))
	#lst = splt[length(splt)]
	#newCSsplit <- append(newCSsplit, lst)}
#gestable$Mig.CS.Split <- newCSsplit

#Get the last chunk of string after the Slash (Miguel's coding)
#newSCsplit = NULL
#for (l in as.character(gestable$Mig.S.Split)) {
	#splt = unlist(strsplit(l, "[,]"))
	#lst = splt[length(splt)]
	#newSCsplit <- append(newSCsplit, lst)}
#gestable$Mig.SC.Split <- newSCsplit

#Compare Eunice's comma and slash splits to see if identical
#gestable$EunCSComp = (as.character(gestable$Eun.CS.Split) == as.character(gestable$Eun.SC.Split))

#Compare Miguel's comma and slash splits to see if indentical
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


