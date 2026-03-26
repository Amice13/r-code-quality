#!bin/usr/R
# Thomas J. Leeper and Kevin J. Mullinix
# March 8, 2013

# Dataset combining for parallel experiments

craigslist <- read.csv("Craigslist.csv")
exitpoll <- read.csv("ExitPoll.csv")
mturk <- read.csv("MTurk.csv")
staff <- read.csv("Staff.csv")
student <- read.csv("Student.csv")
tess <- read.csv("TESS.csv")
facebook <- read.csv("FacebookCombined.csv")

combined <- merge(craigslist, exitpoll, all=TRUE)
combined <- merge(combined, mturk, all=TRUE)
combined <- merge(combined, staff, all=TRUE)
combined <- merge(combined, student, all=TRUE)
combined <- merge(combined, tess, all=TRUE)
combined <- merge(combined, facebook, all=TRUE)

write.csv(combined,"data-combined.csv")

# CHECK DIMENSIONS
#dim(combined)
#dim(craigslist)[1] +
#	dim(exitpoll)[1] +
#	dim(mturk)[1] +
#	dim(staff)[1] +
#	dim(student)[1] +
#	dim(tess)[1]

mydat <- read.csv("data-combined.csv")
names(mydat)


##Variable check
#TESS
tess <- subset(mydat, Sample==1)
table(tess$LoanKnow)
table(tess$LoanGroup)
table(tess$LoanSupp)
table(tess$DREAMGroup)
table(tess$RallyGroup)
table(tess$Interest)

#Exit Poll
exit <- subset(mydat, Sample==2)
table(exit$LoanKnow)
table(exit$LoanGroup)
table(exit$LoanSupp)
table(exit$DREAMGroup)
table(exit$RallyGroup)
table(exit$Interest7)

#Students
stud <- subset(mydat, Sample==3)
table(stud$LoanKnow)
table(stud$LoanGroup)
table(stud$LoanSupp)
table(stud$DREAMGroup)
table(stud$RallyGroup)
table(stud$Interest)

#Staff
staff <- subset(mydat, Sample==4)
table(staff$LoanKnow)
table(staff$LoanGroup)
table(staff$LoanSupp)
table(staff$DREAMGroup)
table(staff$RallyGroup)
table(staff$Interest)

#Mturk
mturk <- subset(mydat, Sample==5)
table(mturk$LoanKnow)
table(mturk$LoanGroup)
table(mturk$LoanSupp)
table(mturk$DREAMGroup)
table(mturk$RallyGroup)
table(mturk$Interest)

#Craigslist
craig <- subset(mydat, Sample==6)
table(craig$LoanKnow)
table(craig$LoanGroup)
table(craig$LoanSupp)
table(craig$DREAMGroup)
table(craig$RallyGroup)
table(craig$Interest)

#Facebook
face <- subset(mydat, Sample==7)
table(face$LoanKnow)
table(face$LoanGroup)
table(face$LoanSupp)
table(face$DREAMGroup)
table(face$RallyGroup)
table(face$Interest)
