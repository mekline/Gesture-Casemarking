#Gesture-Study 1
### Codename: Gesture-Casemarking

Understanding human word-order preferences with gesture/pantomime paradigms. See the OSF project repository () and preprint () for the full description of the project. 

This repository contains all stimuli, data, and analyses for study 1. 

##Gesture-Casemark Repo.RProj

Open to load the R project. Packrat should prompt you to install and then load all the package dependencies from the local lib. 

##Analysis_GestureCasemarking.R

This is the main analysis script. Running this reproduces the pipeline from raw data to tests, graphs, etc. reported in the paper, *except for 1 step*: 

The first part of script produces some 'diff' files pulling out trials that two coders disagreed on. These trials were resolved by a third coder and saved to a new file. The script then picks up those 'resolved' trials and merges it back into the larger dataset that includes all the trials the coders originally agreed on. (So note that you *do* need to run the script all the way from the beginning to get the dataframe that's used for our analyses.) 

Note that the 'embodiment' codings referred to in the early data cleaning are the ones that merely identify a gesture string as containing 'some embodiment' or 'no embodiment'; we then broke this into body-based gestures used for the subject, object, and verb specifically, following the Hall et al. (2013) proposal (see EmbodimentHallRecode.csv) This information is incorportated later in the file.

Interesting lines: 

Lines 0-30: Modify and run these to ensure all packages load and the files will run on your computer (see bottom for version info)

Lines 31-435: Comment out if you don't wish to run/debug the blind coding and just use the tidy data.

Lines 444-510: Descriptive statistics

Line 490: A full list of the 'weird' word orders that we wound up classified as verb medial or lateral, or dropped bc we couldn't classify; consult this if you're skeptical about our gesture coding scheme.

Lines 550- 707: Inferential statistics

Lines 711-799: Graphs produced!

##Stimuli 

This folder contains the two keynote slide decks used to present event movies to participants. (All participants see/gesture all trials; some warmup trials followed by a random order of the key items in A, and the same list reversed, in B.)

##Tidy Data
All trials from participants included in the analysis, with the final word orders, spatial cue, and body-based/embodiment codings following reconciling blind coder disagreements. alldata_tidy_columns.txt explains the columns. 

##Analysis - Post blindcoding/

This is the main folder for intermediate files produced in the final analysis pipeline.

Because this was an in-person study that produced dense video data, and because at least 1 coder for each trial had to be blind to BOTH hypothesis & condition, there are a number of coding steps and files in between the raw data and the final word orders used for analysis. These steps are fully documented in the paper, and the purpose of each csv is explained in '1File Descriptions.txt'. In short, we start with *raw* word orders and participant markings from two separate coders, diff the two codings, reconcile disagreements, and then simplify the resulting strings for analysis. 


##Analysis - Pre blindcoding/

Initial results by EL (who conducted the study) - we later determined that we really needed blind-to-hypothesis coding, but retain the original codings for completeness.  

* FreeGestureData.csv contains all trial data from the no-instruction task
* CaseInstructionData.csv contains all trial data from task performed after instructions about marking participants were given
* GestureCaseMarking.R contains my initial analysis on these datasets

##packrat/
packrat package stuffs

## Reproducibility notes

In theory the packratting should get you all the R dependencies, I havent tested this...

R Studio version: 0.98.1102

R version:

> R.Version()
$platform
[1] "x86_64-apple-darwin13.4.0"

$arch
[1] "x86_64"

$os
[1] "darwin13.4.0"

$system
[1] "x86_64, darwin13.4.0"

$status
[1] ""

$major
[1] "3"

$minor
[1] "1.2"

$year
[1] "2014"

$month
[1] "10"

$day
[1] "31"

$`svn rev`
[1] "66913"

$language
[1] "R"

$version.string
[1] "R version 3.1.2 (2014-10-31)"

$nickname
[1] "Pumpkin Helmet"
