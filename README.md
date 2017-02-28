#Gesture-Study 1
### Codename: Gesture-Casemarking

Understanding human word-order preferences with gesture/pantomime paradigms. See the osf repository () and preprint () for the full description of the project. 

This repository contains all stimuli, data, and analyses for study 1. 

##Analysis_GestureCasemarking.R

This is the main analysis script. Running this reproduces the pipeline from raw data to tests, graphs, etc. reported in the paper, *except for 1 step*: 

The first part of script produces some 'diff' files pulling out trials that two coders disagreed on. These trials were resolved by a third coder and saved to a new file. The script then picks up those 'resolved' trials and merges it back into the larger dataset that includes all the trials the coders originally agreed on. (So note that you *do* need to run the script all the way from the beginning to get the dataframe that's used for our analyses.) 

#CHANGE THAT in once we have the clean data to show!

To change/check: 
graph names
folders for the initial datafiles
clean csvs for: full gesture lists, final 3-referent orders (/Tidy Data)
anonymization! No last names, or any version of my name

##Stimuli 

This folder contains the two keynote slide decks used to present event movies to participants. (All participants see/gesture all trials; some warmup trials followed by a random order of the key items in A, and the same list reversed, in B.)

##Analysis - Post blindcoding

This is the main folder for intermediate files produced in the final analysis pipeline.

Because this was an in-person study that produced dense video data, and because coders had to be blind to hypothesis as well as condition, there are a number of coding steps and files in between the raw data and the final word orders used for analysis. These steps are fully documented in the paper, and the purpose of each csv is explained in '1File Descriptions.txt'. In short, we start with *raw* word orders and participant markings from two separate coders, diff the two codings, reconcile disagreements, and then simplify the resulting strings for analysis. 


##Analysis - Pre blindcoding

Initial results by EL (who conducted the study) - we later determined that we really needed blind-to-hypothesis coding, but retain the original codings for completeness.  

* FreeGestureData.csv contains all trial data from the no-instruction task
* CaseInstructionData.csv contains all trial data from task performed after instructions about marking participants were given
* GestureCaseMarking.R contains my initial analysis on these datasets


