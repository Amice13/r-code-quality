

##CODE TO MERGE PS-LEVEL ELECTION DATA AND MERGEFILES WITH CENSUS INDICATORS FROM 2009
#By Francesca R. Jensenius, Pradeep Chhibber, Sanjeer Alam, Pranav Gupta, and Madhavan Somanathan

#Approximate runtime 135 minutes

library(foreign)
library(xtable)

setwd("") ##Enter the directory where the data are saved

#Merge PS-level data 2009
fileshere<-list.files("PS_data_2009", pattern=".csv", recursive=TRUE, full.names=TRUE)

for (h in c(1:length(fileshere))){
	
onefile<-read.csv(fileshere[h])	

if(h==1){
PSdata2009<-onefile
} else {
PSdata2009 <-rbind(PSdata2009, onefile)	
}
}

#Summary statistics PS-level data 2009
dim(PSdata2009)
summary(PSdata2009)

#Collapse main and auxiliary booths to allow merge with mergefiles

sum(duplicated(paste(PSdata2009$State_no2011, PSdata2009$PC_no, PSdata2009$AC_no, PSdata2009$PS_id)))
table(PSdata2009$PS_id)

#Strip PS-numbers of spaces and letters
PSdata2009$PS_id<-gsub("\\s","", PSdata2009$PS_id)
PSdata2009$PS_id<-gsub("\240","", PSdata2009$PS_id)
PSdata2009$PS_id<-gsub("[[:punct:]]","", PSdata2009$PS_id)
PSdata2009$PS_id<-gsub("[[:alpha:]]","", PSdata2009$PS_id)
PSdata2009$PS_id<-as.integer(as.character(PSdata2009$PS_id))

table(PSdata2009$PS_id)
sum(duplicated(paste(PSdata2009$State_no2011, PSdata2009$PC_no, PSdata2009$AC_no, PSdata2009$PS_id)))

sum.rm.na.logical <- function(x){
    # sums logical vector, treating NA's as 0 unless all NA the NA is returned      
    res <- if(all(is.na(x))) NA else sum(x, na.rm=T) 
    return(res)
}

###Collapse data 
dim(PSdata2009)
PSdata2009new<-aggregate(PSdata2009[,c(7:51)], by=list(PSdata2009$State_no2011, PSdata2009$PC_no, PSdata2009$PC_name, PSdata2009$AC_no, PSdata2009$AC_name, PSdata2009$PS_id), sum.rm.na.logical)

dim(PSdata2009new)
names(PSdata2009new)<-names(PSdata2009)
summary(PSdata2009new)
table(PSdata2009new$PS_id)

#Merge the 2009 mergefiles
fileshere<-list.files("Mergefiles_2009", pattern=".csv", recursive=TRUE, full.names=TRUE)

for (h in c(1:length(fileshere))){
	
onefile<-read.csv(fileshere[h])	

if(h==1){
merge2009<-onefile
} else {
merge2009 <-rbind(merge2009, onefile)	
}
}

#Summary statistics mergefiles 2009
head(merge2009)
summary(merge2009$PS_id)

merge2009$uniquePS<-paste(merge2009$State_no2011, merge2009$AC_no, merge2009$PS_id, sep="_")
PSdata2009$uniquePS<-paste(PSdata2009$State_no2011, PSdata2009$AC_no, PSdata2009$PS_id, sep="_")
PSdata2009new$uniquePS<-paste(PSdata2009new$State_no2011, PSdata2009new$AC_no, PSdata2009new$PS_id, sep="_")

sum(PSdata2009$uniquePS %in% merge2009$uniquePS)/length(PSdata2009$uniquePS) ##The percentage of PS in PS that is found in the mergefile

##Merge PS-data and mergefiles and collapse to the level of census units
dim(PSdata2009new)
length(unique(PSdata2009new$uniquePS)) ##There should be no duplicates since Elec09 was already collapsed to PS-level

dim(merge2009)
length(unique(merge2009$uniquePS)) ##Here there should be duplicates, as a PS can match several villages and vice versa

sum(!(PSdata2009new$uniquePS %in% merge2009$uniquePS)) ##Some PS are missing from the mergefile
sum(!(merge2009$uniquePS %in% PSdata2009new$uniquePS)) ##Some PS are missing from the election data (missing ACs from raw data)

Elec09<-merge(merge2009, PSdata2009new[,-c(1:6)], by="uniquePS", all.x=F, all.y=F) ##Keeping only merged rows

#Create Elec09$PSs -- a variable coding the number of PSs to a census unit
Elec09$PSs<-ifelse(!is.na( Elec09$Vill_no2011), 1, NA) #Set a default of one village having one PS

#Identify how many villages that a PS is shared between for villags that share a PS

PSs<-Elec09$uniquePS[duplicated(Elec09$uniquePS)] #Identify duplicated PS
PSs<-unique(PSs)
if(length(grep("NA", PSs))>0){
	PSs <-PSs[-grep("NA", PSs)]
}

for (i in c(1:length(PSs))){
thecases<-Elec09[Elec09$uniquePS==PSs[i],]
thecases<-thecases[!duplicated(thecases$Vill_no2011),]
Elec09$PSs[Elec09$uniquePS==PSs[i]]<-1/dim(thecases)[1]
print(i)
}

#Identify the number of PSs in villages and towns with multiple PSs
Elec09$unique_vill<-paste(Elec09$State_no2011, Elec09$District_no2011, Elec09$Block_no2011, Elec09$Vill_no2011, sep="_")
vill<-Elec09$unique_vill[duplicated(Elec09$unique_vill)] #Identify duplicated villages
vill<-unique(vill)
vill<-vill[-grep("NA",vill)]

for (i in c(1:length(vill))){
thecases<-Elec09[Elec09$unique_vill == vill[i],]
thecases<-thecases[!duplicated(thecases$uniquePS),]
thecases<-thecases[!is.na(thecases$uniquePS),]
Elec09$PSs[Elec09$unique_vill ==vill[i]]<-dim(thecases)[1]
print(i)
}

#Create census-unit level dataset -- in this case dividing votes equally between villages if there are multiple villages to a PS. This works well if one is primarily interested in using the voting data to create variables like vote shares, turnout, or effective number of parties. If actual vote shares in a village are of interest, these can be estimated by merging in the village population and weighting the votes by the population size of each village 
vill<-unique(Elec09$unique_vill)
vill<-vill[!is.na(Elec09$PSs)]

for (i in c(1:length(vill))){
	
	if(Elec09$PSs[Elec09$unique_vill==vill[i]][1]<1){
		
Elec09[Elec09$unique_vill==vill[i], c(12:57)]<-Elec09[Elec09$unique_vill==vill[i], c(12:57)]*Elec09[Elec09$unique_vill==vill[i], 58]			
		
	} else if (Elec09$PSs[Elec09$unique_vill==vill[i]][1]>1){

thecases<-Elec09[Elec09$unique_vill==unique(Elec09$unique_vill)[i],]	
Elec09[Elec09$unique_vill==vill[i],c(12:57)][1,]<-colSums(thecases[,c(12:57)], na.rm=T)

} 
print(i)
}

#Remove duplicated census entries
Elec09_census<-Elec09[!duplicated(Elec09$unique_vill) & complete.cases(Elec09$unique_vill),]
Elec09_census<-Elec09_census[,-c(1, 9,10,11)]

names(Elec09_census)
head(Elec09_census)		
	
summary(Elec09_census$PSs)
prop.table(table(Elec09_census$PSs>1))
prop.table(table(Elec09_census$PSs<1))
prop.table(table(Elec09_census$PSs==1))


#######Create tables with summary statistics
mytable<-matrix(nrow=12, ncol=6)

mystatename<-c("Uttarakhand", "Jharkhand", "Rajasthan", "Punjab", "Uttar Pradesh", "Madhya Pradesh", "Bihar", "Himachal Pradesh", "Gujarat", "Andhra Pradesh", "West Bengal") 
mystatenumbers<-c("05", "20", "08", "03", "09", "23", "10", "02", "24", "28", "19")  


for(f in c(1:length(mystatenumbers))){
mytable[f,1]<-mystatename[f]	
mytable[f,3]<-dim(PSdata2009[PSdata2009$State_no2011==as.numeric(mystatenumbers)[f],])[1]
mytable[f,4]<-round(mean(PSdata2009$Votes_total[PSdata2009$State_no2011==as.numeric(mystatenumbers)[f]], na.rm=T))
mytable[f,5]<-round(100*sum(PSdata2009$uniquePS[PSdata2009$State_no2011==as.numeric(mystatenumbers)[f]] %in% merge2009$uniquePS[merge2009$State_no2011==as.numeric(mystatenumbers)[f]])/length(PSdata2009$uniquePS[PSdata2009$State_no2011==as.numeric(mystatenumbers)[f]]),1)
mytable[f,6]<-dim(Elec09_census[Elec09_census$State_no2011==as.numeric(mystatenumbers)[f],])[1]
}

#Adding in official number of PS by state (from General Elections 2024, Press Information Bureau)
mytable[,2]<-c(9300, 23696, 42699, 18846, 129446, 47812, 57020, 7253, 42568, 66760, 66109, NA)

mytable[order(mytable[,1]),]

mytable[12,1]<-"Total"	
mytable[12,2]<-sum(as.numeric(mytable[,2]), na.rm=T)
mytable[12,3]<-dim(PSdata2009)[1]
mytable[12,4]<-round(mean(PSdata2009$Votes_total, na.rm=T))
mytable[12,5]<-round(100*sum(PSdata2009$uniquePS %in% merge2009$uniquePS)/length(PSdata2009$uniquePS), 1)
mytable[12,6]<-dim(Elec09_census)[1]

print(xtable(mytable), include.rownames=FALSE)

