#Gesture-Study 1
### Codename: Gesture-Casemarking

Understanding human word-order preferences with gesture/pantomime paradigms. See the osf repository () and preprint () for the full description of the project. 

This repository contains all stimuli, data, and analyses for study 1. 

##Analysis_GestureCasemarking.R

This is the main analysis script. Running this reproduces the pipeline from raw data to tests, graphs, etc. reported in the paper, except for 1 step: the first part of script produces some 'diff' files pulling out trials that two coders disagreed on. These trials were resolved by a third coder and saved to a new file. The script then picks up those 'resolved' trials and merges it back into the larger dataset that includes all the trials the coders originally agreed on. (So note that you *do* need to run the script all the way from the beginning to get the dataframe that's used for our analyses. 

##Stimuli 

This folder contains the two keynote slide decks used to present event movies to participants. (All participants see/gesture all trials; some warmup trials followed by a random order of the key items in A, and the same list reversed, in B.)




Because this was an in-person study that produced dense video data, there are a number of coding steps in between the raw data and the final word orders used for analysis. These steps are fully documented in the paper, but in short, we start with *raw* word orders and participant markings from two separate coders, diff the two codings, reconcile disagreements, and then simplify the resulting strings for analysis. 

Because of how the initial video codings were created (team of coders, blinded to the hypotheses)

This raw data is located at 
SpatialCasemarking_AllGestureData.csv
SpatialCasemarking_SubjectInfo.csv

T

Full analysis for GestureCaseMarking experiment

Preprocessing of blind coding data developed by Miguel Salinas. Code copied over on 8/12 and commented
(up thru the indicated area) but otherwise left alone for now.

Note: First part of this file produces output files: LiteralNoAgree.csv and Last3NoAgree.csv, the latter of
which was then tiebroken by hand by trained RAs. This tiebreaking is stored in Last3NoAgree_Reconciliation.csv,
which gets read in and used for subsequent analyses.


Reading in all libraries that we'll use

Put in a clean dataset if you want!
