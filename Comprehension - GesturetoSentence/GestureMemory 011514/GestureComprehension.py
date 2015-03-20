#Import packages we need, including Willow
import random
import copy
from willow.willow import *

def session(me):

	def ping():
		put({"tag":"PING", "client": me})
	
	#Make a unique paycode for this user!  Note that all paycodes end with the session number as a reality check!
	x = random.randint(0, 16777215000)
	payCode = "%x" % x
	payCode = random.choice('abcdefghij')  + payCode + random.choice('abcdefghij')
	payCode = payCode + str(me) 
	print(payCode)
	
	#####
	# Item Randomization!!
	#####

	#Open up the file
	f = open('GestureComp.csv', 'r')
	items = f.read().splitlines()
	items = items[1:]

	#Set up new randomizations!  Within the Animate and Inanimate transitives, we'll want half SOV/SVO, 1/3 Distractor/Reversal/Same, and 2/3 Passive/Active question
	#And within the Animate and Inanimate intransitives, we'll want a majority Same, and ALL ACTIVE!
	#For the transitives (each set is 24 long)

	#1/3 Same/Reverse/Distractor
	changeTypePicker = ["Same" for x in range(8)] #8
	changeTypePicker.extend(["Reversal" for x in range(8)]) #16
	changeTypePicker.extend(["Distractor" for x in range(8)]) #24

	#Half of each is SVO/SOV
	sentOrderPicker1 = ["SVO" for x in range(4)] #4
	sentOrderPicker1.extend(["SOV" for x in range(4)]) #8
	sentOrderPicker = sentOrderPicker1 + sentOrderPicker1 #16
	sentOrderPicker.extend(sentOrderPicker1) #24

	#And 3/4 of each of THOSE is Passive/Active
	questionTypePicker1 = ["Passive" for x in range(3)] #3
	questionTypePicker1.extend(["Active" for x in range(1)])#4
	questionTypePicker1.extend(questionTypePicker1) #8
	questionTypePicker = questionTypePicker1 + questionTypePicker1 #16
	questionTypePicker.extend(questionTypePicker1) #24

	#And for the Intransitives (each set is 12 long)

	#Majority same to balance things!
	changeTypePickerShort = ["Same" for x in range(8)] #8
	changeTypePickerShort.extend(["Distractor" for x in range(2)]) #10
	changeTypePickerShort.extend(["Reversal" for x in range(2)]) #12

	#All sentences order will be SV, no problem!

	#All intransitive questions are active!
	questionTypePickerShort = ["Active" for x in range(12)]

	####
	#And build out the full lists we'll use for the experiment!

	sentType = []
	changeType = []
	sentOrder = []
	questionType = []

	#Animate-Object
	randState = random.getstate()
	fixedShuffle(changeTypePicker, randState)
	fixedShuffle(sentOrderPicker, randState)
	fixedShuffle(questionTypePicker, randState)

	sentType.extend(["AO" for x in range(24)])
	changeType.extend(changeTypePicker)
	sentOrder.extend(sentOrderPicker)
	questionType.extend(questionTypePicker)

	#Animate-Intransitive
	randState = random.getstate()
	fixedShuffle(changeTypePickerShort, randState)
	fixedShuffle(questionTypePickerShort, randState)

	sentType.extend(["AI" for x in range(12)])
	changeType.extend(changeTypePickerShort)
	sentOrder.extend(["SV" for x in range(12)])
	questionType.extend(questionTypePickerShort)

	#Inanimate-Object
	randState = random.getstate()
	fixedShuffle(changeTypePicker, randState)
	fixedShuffle(sentOrderPicker, randState)
	fixedShuffle(questionTypePicker, randState)

	sentType.extend(["IO" for x in range(24)])
	changeType.extend(changeTypePicker)
	sentOrder.extend(sentOrderPicker)
	questionType.extend(questionTypePicker)

	#Inanimate-Intransitive
	randState = random.getstate()
	fixedShuffle(changeTypePickerShort, randState)
	fixedShuffle(questionTypePickerShort, randState)

	sentType.extend(["II" for x in range(12)])
	changeType.extend(changeTypePickerShort)
	sentOrder.extend(["SV" for x in range(12)])
	questionType.extend(questionTypePickerShort)

	print(len(sentType))
	print(len(changeType))
	print(len(sentOrder))
	print(len(questionType))

	#Now we can go through the item file and store all the information we need to!
	stimNo = []
	agent = []
	verb = []
	patient = []
	distractorType = [] #For comprehension not actually using these...
	distractor = []

	for i in xrange(len(items)):
		fields = items[i].strip().split(',')
		print(fields)
		stimNo.append(fields[0])
		agent.append(fields[5])
		verb.append(fields[6])
		patient.append(fields[7])
		distractorType.append(fields[8])
		distractor.append(fields[9])

	#And finally randomize the order of presentation!
	randState = random.getstate()
	fixedShuffle(sentType,randState)
	fixedShuffle(changeType,randState)
	fixedShuffle(sentOrder,randState)
	fixedShuffle(questionType,randState)
	fixedShuffle(stimNo,randState)
	fixedShuffle(agent,randState)
	fixedShuffle(verb,randState)
	fixedShuffle(patient,randState)
	fixedShuffle(distractorType,randState)
	fixedShuffle(distractor,randState)

	# #This is just a debugging section, it prints out a file containing the items that a participant sees
	# # printlines = []
	# # for i in xrange(len(items)):
	# # 	thisline = 'List: ' + listno[i] + ' Sequence: ' + sequence[i] +  ' \nChange Type: ' + trialChanges[i] + '\nBaseline Movie: ' + BaselineMovie[i] +  '\nTest Movie: ' + TestMovie[i] 
	# # 	printlines.append(thisline)
		
	# # printout = '\n\n'.join(printlines)
	# # f = open(str(me) + 'items.txt', 'w')
	# # f.write(printout)





	#####
	# Consent and preliminaries
	#####

	# #Okay, now we start the experiment!
	
	#First, write a header line for any variables we'll want to print out in the data
	log('willowSubNo','Paycode','movieCheck', 'nounTrainingTime', 'verbTrainingTime', 'refreshTrainingTime', 'trialNo',
		'stimNo', 'sentType', 'changeType', 'sentOrder', 'questionType', 'agent', 'verb', 'patient', 'distractorType', 'distractor',
		'Response', 'wasCorrect','wasError', 'mathResponseA', 'mathResponseB', 'testSequence')


	
	#Consent screen
	toScreen("consent.html")
	goOn(me)
	
	#System requirements screen with button
	toScreen("requirements.html")
	goOn(me)

	#Test movie page
	toScreen("Video_exposure.html",[("XXX", "http://tedlab.mit.edu/~mekline/Calibration/Balloon - Silent")])

	#Wait until the video is finished
	background(ping, 7)
	take({"tag":"PING", "client": me})
	#...and ADD the question at the bottom
	addToScreen("moviecheck.html")

	#Get response back
	sawMovie = getButtonResponse(me)
	
	#Kick them off if they didn't see it!  (Deadends this user)
	if (sawMovie=="No"):
		toScreen("sorry.html")
	else: 	#Otherwise make them describe!
		toScreen("moviecheck2.html")
	movieCheck = getTextResponse(me, "#MovieDescrip")	

	#####
	# Sign Learning
	#####

	#Instructions for the whole task!
	toScreen("opening_instructions.html")
	goOn(me)

	#Wrap up the functions for the presentation and test in functions, since they are basically the same

	#Train a set of signs, with optional pictures
	def trainSigns(myWordList, instructions, prepPage):
		toScreen(instructions)
		goOn(me)
		random.shuffle(myWordList)
		# #Present each one!
		for n in xrange(len(myWordList)):
			toScreen(prepPage,[("XXX", myWordList[n])])
			goOn(me)
			toScreen("Video_exposure.html", [("XXX", "http://tedlab.mit.edu/~mekline/GestureMovies/TrainingMovies/" + myWordList[n])])
			#Wait until the video is finished
			background(ping, 3)
			take({"tag":"PING", "client": me})
			#...and ADD the description at the bottom
			addToScreen("movieDescrip.html", [("XXX", myWordList[n])])
			goOn(me)

	#And test them! Here's the general system for doing a word test
	def testSigns(myWordList, scoreTarget):
		random.shuffle(myWordList)
		toScreen("train_instructions.html", [("ZZZ", str(scoreTarget))])
		goOn(me)
		trainScore = 0
		currWord = 0
		timeToExit = 0

		while (trainScore < scoreTarget):
			thisWord = myWordList[currWord % len(myWordList)]
			foilWord = myWordList[(currWord + 5) % len(myWordList)]

			#Play thisWord's movie
			toScreen("Video_exposure.html", [("XXX", "http://tedlab.mit.edu/~mekline/GestureMovies/TrainingMovies/" + thisWord)])
			#Wait until the video is finished
			background(ping, 3)
			take({"tag":"PING", "client": me})
			#Ask for an answer - flip a coin to decide where the right answer is
			if random.random() > 0.5:
				addToScreen("whichsign_question.html", [("AAA", thisWord),("BBB", foilWord)])
			else:
				addToScreen("whichsign_question.html", [("BBB", thisWord),("AAA", foilWord)])

			wordChoice = getButtonResponse(me)

			if (wordChoice == thisWord):
				toScreen("feedback.html",[("XXX", "RIGHT")])
				background(ping, 2)
				take({"tag":"PING", "client": me})
				#Add to score
				trainScore = trainScore + 1
			else:
				toScreen("feedback.html",[("XXX", "WRONG")])
				background(ping, 2)
				take({"tag":"PING", "client": me})
				#Reset score
				trainScore = 0

			currWord = currWord + 1
			timeToExit = timeToExit + 1

		return timeToExit

	#Test the nouns, blocking until we get 8 in a row correct
	myNouns = ["Girl", "Boy", "Old Lady", "Fireman", "Heart","Car","Ball","Star"]
	trainSigns(myNouns, "noun_instructions.html", "noun_prep.html")
	nounTrainingTime = testSigns(myNouns, 8)

	#Test the verbs, blocking until we get 12 in a row correct
	myVerbs = ["Elbow", "Kick", "Kiss", "Throw", "Poke", "Fall", "Tumble", "Move", "Jump", "Push", "Lift", "Rub"]
	trainSigns(myVerbs, "verb_instructions.html", "verb_prep.html")
	verbTrainingTime = testSigns(myVerbs, 12)

	#####
	# Memory Task!!
	#####

	toScreen("memory_instructions.html")
	goOn(me)

	#To store responses and percents correct
	Response = []
	participantScore = 0
	refreshTrainingTime = "NA" #This will get set halfway through!

	#FOR A SHORT PILOT!!!
	#items = items[:12]
	#movieCheck = "NA"
	#nounTrainingTime = "NA"
	#verbTrainingTime = "NA"

	for i in xrange(len(items)):

		####
		#Are we halfway through? Run the Refresher before allowing people to go on!
		####
		if (i==len(items)/2):
			toScreen("refresh_instructions.html")
			goOn(me)
			refreshTrainingTime = testSigns(myVerbs + myNouns, 10)

		####
		#Get variables for this trial
		####
		moviePrefix = str(stimNo[i]) + sentType[i]
		if (len(moviePrefix) == 3):
			moviePrefix = "0" + moviePrefix
		moviePrefix = "http://tedlab.mit.edu/~mekline/GestureMovies/" + moviePrefix
		BaselineMovie = moviePrefix + "_Target" + sentOrder[i]

		#Constructing Test MOVIE
		# if changeType[i] == "Same":
		# 	TestMovie = BaselineMovie
		# elif changeType[i] == "Distractor":
		# 	TestMovie = moviePrefix + "_Distractor" + sentOrder[i]
		# else:
		# 	#Need a special thing for the silly VS/SV things!!
		# 	if sentOrder[i] == "SV":
		# 		TestMovie = moviePrefix + "_Reverse" + "VS"
		# 	else:
		# 		TestMovie = moviePrefix + "_Reverse" + sentOrder[i]

		#Constructing Test SEQUENCE
		myAgent = agent[i]
		myVerb = verb[i]
		myPatient = patient[i]
		if changeType[i] =="Distractor":
			if distractorType[i] == "A":
				myAgent = distractor[i]
			elif distractorType[i] =="V":
				myVerb = distractor[i]
			elif distractorType[i] == "P":
				myPatient = distractor[i]
		elif changeType[i] =="Reversal":
			myAgent = patient[i]
			myPatient = agent[i]

		myAgent = 'the ' + myAgent.lower()
		myPatient = 'the ' + myPatient.lower()
		myVerb = myVerb.lower()
		if myPatient=="the na":
			myPatient = ""
		if myVerb == "rub":
			pastVerb = "rubbed"
		elif myVerb == "throw":
			pastVerb = "thrown"
		elif myVerb == "poke":
			pastVerb = "poked"
		else:
			pastVerb = myVerb + "ed"

		#Construct Test Sentence
		if questionType[i] == "Active":
			TestSequence = "Did " + myAgent + " " + myVerb + " " + myPatient + "?"
		else: #Passive!
			TestSequence = "Was " + myPatient + " " + pastVerb + " by " + myAgent + "?"


		#Debug!
		#TestSequence = TestSequence  + '_' + sentOrder[i] + '_' + changeType[i] + '_' + mySovOrder


		mathWordList = ["one","two","three","four","five","six","seven","eight","nine"]
		mathNumList = [1,2,3,4,5,6,7,8,9]
		randState = random.getstate()
		fixedShuffle(mathWordList,randState)
		fixedShuffle(mathNumList,randState)

		####
		#Start display - Hand out number list
		####

		toScreen("GM_math_prep.html", [("aaa", mathWordList[0]), ("bbb", mathWordList[1]),("ccc", mathWordList[2]),("ddd", mathWordList[3]),("eee", mathWordList[4]),("fff", mathWordList[5])])
		#Few seconds to read those
		background(ping, 4)
		take({"tag":"PING", "client": me})

		####
		#Prep participant for movie!!
		####

		#Warning screen
		toScreen("GM_video_prep.html")
		#Few seconds to get ready
		background(ping, 2)
		take({"tag":"PING", "client": me})

	 	####
	 	#Baseline movie
	 	####

	 	toScreen("Video_exposure.html", [("XXX", BaselineMovie)])
	 	#Wait until the movie is nearly over to give the go-on cue
	 	if sentOrder[i] == "SV":
	 		background(ping, 7.5)
	 		#background(ping, 2.5)
	 	else:
	 		background(ping, 9.5)
	 		#background(ping, 2.5)

	 	take({"tag":"PING", "client": me})
	 	addToScreen("GoOnButton.html")
	 	goOn(me)

	 	#Or for debugging!
	 	#toScreen("String_exposure.html", [("XXX", agent[i]+verb[i]+patient[i])])
	 	#background(ping, 2)
		#take({"tag":"PING", "client": me})

	 	####
	 	#Test math and get mathResponse
	 	####
	 	toScreen("GM_math_test.html")
		goOn(me)
		#And look at the answers!
		mathAnswers = []
		for j in range(6):
			myField = "#math" + str(j+1)
			myResp = (peek(myField))
			myResp = myResp.strip()
			if re.match('[0-9]+', myResp):
				mathAnswers.append(int(myResp))
			else:
				mathAnswers.append(0)

		#How many correct?
		def is_number(s):
		    try:
		        float(s)
		        return True
		    except ValueError:
		        return False

		mathResponseCorrectA = 0
		for j in range(6):
			if mathAnswers[j] == mathNumList[j]:
				mathResponseCorrectA = mathResponseCorrectA + 1

	 	####
	 	# Show the new number list and tell them to expect the next movie.
	 	####

	 	#Then make a new list for the second half!
		randState = random.getstate()
		fixedShuffle(mathWordList,randState)
		fixedShuffle(mathNumList,randState)

		####
		toScreen("GM_math_prep.html", [("aaa", mathWordList[0]), ("bbb", mathWordList[1]),("ccc", mathWordList[2]),("ddd", mathWordList[3]),("eee", mathWordList[4]),("fff", mathWordList[5])])
		#Few seconds to read those
		background(ping, 4)
		take({"tag":"PING", "client": me})


		####
	 	#Test STRING and get Response
	 	####

	 	toScreen("String_exposure.html", [("XXX", TestSequence)])
	 	addToScreen("yesno_stringquestion.html")
		Response.append(getButtonResponse(me))


		####
	 	#Record results for Movies!
	 	####

	 	wasError = 0
	 	wasCorrect = 0

	 	if (Response[i].strip() == "No") & (changeType[i] == "Reversal"):
	 		wasCorrect = 1
	 	elif (Response[i].strip() == "No") & (changeType[i] == "Distractor"):
	 		wasCorrect = 1
	 	elif (Response[i].strip() == "Yes") & (changeType[i] == "Same"):
	 		wasCorrect = 1
	 	elif (Response[i] == "Error"):
	 		wasError = 1

	 	participantScore = participantScore + wasCorrect

	 	####
	 	# Test Math again!
	 	####

	 	toScreen("GM_math_test.html")
		goOn(me)
		#And look at the answers!
		mathAnswers = []
		for j in range(6):
			myField = "#math" + str(j+1)
			myResp = (peek(myField))
			myResp = myResp.strip()
			if re.match('[0-9]+', myResp):
				mathAnswers.append(int(myResp))
			else:
				mathAnswers.append(0)

		#How many correct?
		def is_number(s):
		    try:
		        float(s)
		        return True
		    except ValueError:
		        return False

		mathResponseCorrectB = 0
		for j in range(6):
			if mathAnswers[j] == mathNumList[j]:
				mathResponseCorrectB = mathResponseCorrectB + 1

	 	####
	 	#Show feedback screen
	 	####
	 	
	 	if (wasCorrect & (mathResponseCorrectA + mathResponseCorrectB==10)):
	 		toScreen("progress_feedback.html",[("XXX", "RIGHT"),("BBB", "right"),("YYY",str(mathResponseCorrectA + mathResponseCorrectB)),("ZZZ", str(i+1)),("PPP", str(100*(i+1)/len(items))),("AAA", str(len(items)))])
	 	elif wasCorrect:
	 		toScreen("progress_feedback.html",[("XXX", "WRONG"),("BBB", "right"),("YYY",str(mathResponseCorrectA + mathResponseCorrectB)),("ZZZ", str(i+1)),("PPP", str(100*(i+1)/len(items))),("AAA", str(len(items)))])
		else:
			toScreen("progress_feedback.html",[("XXX", "WRONG"),("BBB", "wrong"),("YYY",str(mathResponseCorrectA + mathResponseCorrectB)),("ZZZ", str(i+1)),("PPP", str(100*(i+1)/len(items))),("AAA", str(len(items)))])
		

		#Little pause before going on
		background(ping,4)
		take({"tag":"PING", "client": me})
		
		
		#Record all data from this trial!!	
		log(me, payCode, movieCheck, nounTrainingTime, verbTrainingTime, refreshTrainingTime, i, stimNo[i], sentType[i],
			changeType[i], sentOrder[i], questionType[i], agent[i], verb[i], patient[i], distractorType[i], distractor[i], Response[i],
			wasCorrect,wasError, mathResponseCorrectA, mathResponseCorrectB, TestSequence)
				
	#End Memory loop
	
	#Show thankyou and total score!
	toScreen("thankyou.html",[("YYY", payCode),("QQQ", str(participantScore))])

#End Session function!	


###
# Helper Functions Etc.
###
		
#A helper function for yoked randomizations
def fixedShuffle(aList, aState):
	random.setstate(aState)
	random.shuffle(aList)
	return aList

#Function wrapper for moving on with no data recorded
def goOn(ses):
	take({"tag": "click", "id": "GoOnButton", "client": ses})

#Function wrapper for moving on after looking at a text field
def getTextResponse(ses, myField):
	take({"tag": "click", "id": "GoOnButton", "client": ses})
	myVal = peek(myField)
	return myVal

#Function wrapper for moving on after recording the button clicked
def getButtonResponse(ses):
	msg = take({"tag": "click", "client": ses})
	return(msg["id"])

#Simple function that displays a screen with replacements
def toScreen(fileName, replacementPairs = []):
	let("")
	addToScreen(fileName, replacementPairs)

#Same as above, but doesn't clear first!
def addToScreen(fileName, replacementPairs = []):
	toPut = open(fileName, "r").read()
	for i in xrange(len(replacementPairs)):
		toPut = toPut.replace(replacementPairs[i][0], replacementPairs[i][1])
	add(toPut)


run(session, 2999)

	 # 	####
	 # 	#Math Interlude
	 # 	####

		# mathA = range(10)
		# mathB = range(10)
		# random.shuffle(mathA)
		# random.shuffle(mathB)
		# mathpairs = []
		# for j in range(10):
		# 	mathpairs.append(("f" + str(j+1) + "a", str(mathA[j])))
		# 	mathpairs.append(("f" + str(j+1) + "b", str(mathB[j])))
		# toScreen("mathproblems.html", mathpairs)
		# background(ping, 10)
		# take({"tag":"PING", "client": me})
		# #And look at the answers!
		# mathAnswers = []
		# for j in range(10):
		# 	myField = "#math" + str(j+1)
		# 	myResp = (peek(myField))
		# 	if re.match('[0-9]+', myResp):
		# 		mathAnswers.append(int(myResp))
		# 	else:
		# 		mathAnswers.append(0)

		# #How many correct?
		# def is_number(s):
		#     try:
		#         float(s)
		#         return True
		#     except ValueError:
		#         return False

		# numCorrect = 0
		# for j in range(10):
		# 	rightAnswer = -100
		# 	if is_number(mathA[j]) & is_number(mathB[j]):
		# 		rightAnswer = mathA[j] + mathB[j]
		# 	if mathAnswers[j] == rightAnswer:
		# 		numCorrect = numCorrect + 1


		# #And just show then wait a minute for them to read
		# toScreen("goodMath.html", [("XXX", str(numCorrect))])
		# background(ping, 3)
	 # 	take({"tag":"PING", "client": me})


