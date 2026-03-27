

##CODE TO MERGE PS-LEVEL ELECTION DATA AND MERGEFILES WITH CENSUS INDICATORS FROM 2019
#By Francesca R. Jensenius, Pradeep Chhibber, Sanjeer Alam, Pranav Gupta, and Madhavan Somanathan

#Approximate runtime 30 minutes

library(foreign)
library(xtable)


setwd("") ##Enter the directory where the data are saved

#Merge PS-level data 2019
fileshere<-list.files("PS_data2019", pattern=".csv", recursive=TRUE, full.names=TRUE)

PSdata2019 <-read.csv(fileshere)	

#Summary statistics PS-level data 2019
dim(PSdata2019)
summary(PSdata2019)

#Collapse main and auxiliary booths to allow merge with mergfiles
table(PSdata2019$PS_id)
table(PSdata2019$AC_no)
length(table(PSdata2019$AC_no[PSdata2019$PS_id==1]))

#Strip PS-numbers of spaces and letters
PSdata2019$PS_id<-gsub("\\s","", PSdata2019$PS_id)
PSdata2019$PS_id<-gsub("\240","", PSdata2019$PS_id)
PSdata2019$PS_id<-gsub("\200","", PSdata2019$PS_id)
PSdata2019$PS_id<-gsub("\220","", PSdata2019$PS_id)
PSdata2019$PS_id<-gsub("\225","", PSdata2019$PS_id)
PSdata2019$PS_id<-gsub("[[:punct:]]","", PSdata2019$PS_id)
PSdata2019$PS_id<-gsub("[[:alpha:]]","", PSdata2019$PS_id)
PSdata2019$PS_id<-as.integer(as.character(PSdata2019$PS_id))

table(PSdata2019$PS_id)
table(PSdata2019$AC_no)
length(table(PSdata2019$AC_no))

sum(duplicated(paste(PSdata2019$State_no2011, PSdata2019$AC_no, PSdata2019$PS_id)))

sum.rm.na.logical <- function(x){
    # sums logical vector, treating NA's as 0 unless all NA the NA is returned      
    res <- if(all(is.na(x))) NA else sum(x, na.rm=T) 
    return(res)
}

###Collapse data 
dim(PSdata2019)
PSdata2019new<-aggregate(PSdata2019[,c(7:51)], by=list(PSdata2019$State_no2011, PSdata2019$PC_no, PSdata2019$PC_name, PSdata2019$AC_no, PSdata2019$AC_name, PSdata2019$PS_id), sum.rm.na.logical)

dim(PSdata2019new)
names(PSdata2019new)<-names(PSdata2019)
table(PSdata2019new$PS_id)
table(PSdata2019new$AC_no)
length(table(PSdata2019new$AC_no))
length(table(PSdata2019new$AC_no[PSdata2019new$PS_id==1]))
summary(PSdata2019new)


#Merge the 2019 mergefiles
fileshere<-list.files("Mergefiles_2019", pattern=".csv", recursive=TRUE, full.names=TRUE)

merge2019 <-read.csv(fileshere)	

#Summary statistics mergefiles 2019
head(merge2019)
summary(merge2019$PS_id)

merge2019$uniquePS<-paste(merge2019$State_no2011, merge2019$AC_no, merge2019$PS_id, sep="_")
PSdata2019$uniquePS<-paste(PSdata2019$State_no2011, PSdata2019$AC_no, PSdata2019$PS_id, sep="_")
PSdata2019new$uniquePS<-paste(PSdata2019new$State_no2011, PSdata2019new$AC_no, PSdata2019new$PS_id, sep="_")

sum(PSdata2019$uniquePS %in% merge2019$uniquePS)/length(PSdata2019$uniquePS) ##The percentage of PS in PS that is found in the mergefile

##Merge PS-data and mergefiles and collapse to the level of census units
dim(PSdata2019new)
length(unique(PSdata2019new$uniquePS)) ##There should be no duplicates since Elec19 was already collapsed to PS-level

dim(merge2019)
length(unique(merge2019$uniquePS)) ##Here there should be duplicates, as a PS can match several villages and vice versa

Elec19<-merge(merge2019, PSdata2019new[,-c(1:6)], by="uniquePS", all.x=F, all.y=F) ##Keeping only merged rows

#Create Elec19$PSs -- a variable coding the number of PSs to a census unit
Elec19$PSs<-ifelse(!is.na( Elec19$Vill_no2011), 1, NA) #Set a default of one village having one PS

#Identify how many villages that a PS is shared between for villages that share a PS

PSs<-Elec19$uniquePS[duplicated(Elec19$uniquePS)] #Identify duplicated PS
PSs<-unique(PSs)
if(length(grep("NA", PSs))>0){
	PSs <-PSs[-grep("NA", PSs)]
}

for (i in c(1:length(PSs))){
thecases<-Elec19[Elec19$uniquePS==PSs[i],]
thecases<-thecases[!duplicated(thecases$Vill_no2011),]
Elec19$PSs[Elec19$uniquePS==PSs[i]]<-1/dim(thecases)[1]
print(i)
}

#Identify the number of PSs in villages and towns with multiple PSs
vill<-Elec19$Vill_no2011[duplicated(Elec19$Vill_no2011)] #Identify duplicated villages
vill<-unique(vill)

for (i in c(1:length(vill))){
thecases<-Elec19[Elec19$Vill_no2011 == vill[i],]
thecases<-thecases[!duplicated(thecases$uniquePS),]
thecases<-thecases[!is.na(thecases$uniquePS),]
Elec19$PSs[Elec19$Vill_no2011 ==vill[i]]<-dim(thecases)[1]
print(i)
}

#Create census-unit level dataset -- in this case dividing votes equally between villages if there are multiple villages to a PS. This works well if one is primarily interested in using the voting data to create variables like vote shares, turnout, or effective number of parties. If actual vote shares in a village are of interest, these can be estimated by merging in the village population and weighting the votes by the population size of each village 
vill<-unique(Elec19$Vill_no2011)
vill<-vill[!is.na(Elec19$PSs)]
vill<-vill[!is.na(vill)]

for (i in c(1:length(vill))){
	
	if(Elec19$PSs[Elec19$Vill_no2011 ==vill[i]][1]<1){
		
Elec19[Elec19$Vill_no2011 ==vill[i], c(12:57)]<-Elec19[Elec19$Vill_no2011 ==vill[i], c(12:57)]*Elec19[Elec19$Vill_no2011 ==vill[i], 58]			
		
	} else if (Elec19$PSs[Elec19$Vill_no2011 ==vill[i]][1]>1){

thecases<-Elec19[Elec19$Vill_no2011 == vill[i],]	
Elec19[Elec19$Vill_no2011 ==vill[i],c(12:57)][1,]<-colSums(thecases[,c(12:57)], na.rm=T)

} 
print(i)
}


#Remove duplicated census entries
Elec19_census<-Elec19[!duplicated(Elec19$Vill_no2011) & complete.cases(Elec19$Vill_no2011),]
Elec19_census<-Elec19_census[,-c(1, 9,10,11)]

names(Elec19_census)
head(Elec19_census)		

#######Summary statistics

summary(Elec19_census$PSs)
prop.table(table(Elec19_census$PSs>1))
prop.table(table(Elec19_census$PSs<1))
prop.table(table(Elec19_census$PSs==1))

dim(PSdata2019)[1]
round(mean(PSdata2019$Votes_total, na.rm=T))
round(100*sum(PSdata2019$uniquePS %in% merge2019$uniquePS)/length(PSdata2019$uniquePS), 1)
dim(Elec19_census)[1]
