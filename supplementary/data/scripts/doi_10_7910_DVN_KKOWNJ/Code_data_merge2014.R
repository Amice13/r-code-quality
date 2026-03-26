

##CODE TO MERGE PS-LEVEL ELECTION DATA AND MERGEFILES WITH CENSUS INDICATORS FROM 2014
#By Francesca R. Jensenius, Pradeep Chhibber, Sanjeer Alam, Pranav Gupta, and Madhavan Somanathan

#Approximate runtime 117 minutes

library(foreign)
library(xtable)

setwd("") ##Enter the directory where the data are saved

#Merge PS-level data 2014
fileshere<-list.files("PS_data_2014", pattern=".csv", recursive=TRUE, full.names=TRUE)

for (h in c(1:length(fileshere))){
	
onefile<-read.csv(fileshere[h])	

if(h==1){
PSdata2014<-onefile
} else {
PSdata2014 <-rbind(PSdata2014, onefile)	
}
}

#Summary statistics PS-level data 2014
dim(PSdata2014)
summary(PSdata2014)

#Collapse main and auxiliary booths to allow merge with mergfiles
table(PSdata2014$PS_id)

#Strip PS-numbers of spaces and letters
PSdata2014$PS_id<-gsub(":00","", PSdata2014$PS_id)
PSdata2014$PS_id<-gsub("\\s","", PSdata2014$PS_id)
PSdata2014$PS_id<-gsub("\240","", PSdata2014$PS_id)
PSdata2014$PS_id<-gsub("\200","", PSdata2014$PS_id)
PSdata2014$PS_id<-gsub("\220","", PSdata2014$PS_id)
PSdata2014$PS_id<-gsub("\225","", PSdata2014$PS_id)
PSdata2014$PS_id<-gsub("[[:punct:]]","", PSdata2014$PS_id)
PSdata2014$PS_id<-gsub("[[:alpha:]]","", PSdata2014$PS_id)
PSdata2014$PS_id<-as.numeric(as.character(PSdata2014$PS_id))

table(PSdata2014$PS_id)

sum.rm.na.logical <- function(x){
    # sums logical vector, treating NA's as 0 unless all NA the NA is returned      
    res <- if(all(is.na(x))) NA else sum(x, na.rm=T) 
    return(res)
}

###Collapse data 
PSdata2014new<-aggregate(PSdata2014[,c(7:51)], by=list(PSdata2014$State_no2011, PSdata2014$PC_no, PSdata2014$PC_name, PSdata2014$AC_no, PSdata2014$AC_name, PSdata2014$PS_id), sum.rm.na.logical)
dim(PSdata2014new)
names(PSdata2014new)<-names(PSdata2014)
summary(PSdata2014new)


#Merge the 2014 mergefiles
fileshere<-list.files("Mergefiles_2014", pattern=".csv", recursive=TRUE, full.names=TRUE)

for (h in c(1:length(fileshere))){
	
onefile<-read.csv(fileshere[h])	

if(h==1){
merge2014<-onefile
} else {
merge2014 <-rbind(merge2014, onefile)	
}
}

#Summary statistics mergefiles 2014
dim(merge2014)
head(merge2014)
summary(merge2014$PS_id)

merge2014$uniquePS<-paste(merge2014$State_no2011, merge2014$AC_no, merge2014$PS_id, sep="_")
PSdata2014new$uniquePS<-paste(PSdata2014new$State_no2011, PSdata2014new$AC_no, PSdata2014new$PS_id, sep="_")
PSdata2014$uniquePS<-paste(PSdata2014$State_no2011, PSdata2014$AC_no, PSdata2014$PS_id, sep="_")
sum(duplicated(PSdata2014$uniquePS)) ## These are all the cases with multiple booths in one PS

sum(PSdata2014$uniquePS %in% merge2014$uniquePS)/length(PSdata2014$uniquePS) ##The percentage of PS in PS that is found in the mergefile

##Merge PS-data and mergefiles and collapse to the level of census units
dim(PSdata2014new)
length(unique(PSdata2014new$uniquePS)) ##There should be no duplicates since Elec14 was already collapsed to PS-level

dim(merge2014)
length(unique(merge2014$uniquePS)) ##Here there should be duplicates, as a PS can match several villages and vice versa

sum(!(PSdata2014new$uniquePS %in% merge2014$uniquePS)) ##Some PS are missing from the mergefile
sum(!(merge2014$uniquePS %in% PSdata2014new$uniquePS)) ##Some PS are missing from the election data (missing ACs from raw data)

Elec14<-merge(merge2014, PSdata2014new[,-c(1:6)], by="uniquePS", all.x=F, all.y=F) ##Keep only merged rows

#Create Elec14$PSs -- a variable coding the number of PSs to a census unit
Elec14$PSs<-ifelse(!is.na( Elec14$Vill_no2011), 1, NA) #Set a default of one village having one PS

#Identify how many villages that a PS is shared between for villags that share a PS

PSs<-Elec14$uniquePS[duplicated(Elec14$uniquePS)] #Identify duplicated PS
PSs<-unique(PSs)
if(length(grep("NA", PSs))>0){
	PSs <-PSs[-grep("NA", PSs)]
}

for (i in c(1:length(PSs))){
thecases<-Elec14[Elec14$uniquePS==PSs[i],]
thecases<-thecases[!duplicated(thecases$Vill_no2011),]
Elec14$PSs[Elec14$uniquePS==PSs[i]]<-1/dim(thecases)[1]
}

#Identify the number of PSs in villages and towns with multiple PSs
Elec14$unique_vill<-paste(Elec14$State_no2011, Elec14$District_no2011, Elec14$Block_no2011, Elec14$Vill_no2011, sep="_")
vill<-Elec14$unique_vill[duplicated(Elec14$unique_vill)] #Identify duplicated villages
vill<-unique(vill)
vill<-vill[-grep("NA",vill)]

for (i in c(1:length(vill))){
thecases<-Elec14[Elec14$unique_vill == vill[i],]
thecases<-thecases[!duplicated(thecases$uniquePS),]
thecases<-thecases[!is.na(thecases$uniquePS),]
Elec14$PSs[Elec14$unique_vill ==vill[i]]<-dim(thecases)[1]
print(i)
}

#Create census-unit level dataset -- in this case dividing votes equally between villages if there are multiple villages to a PS. This works well if one is primarily interested in using the voting data to create variables like vote shares, turnout, or effective number of parties. If actual vote shares in a village are of interest, these can be estimated by merging in the village population and weighting the votes by the population size of each village 
vill<-unique(Elec14$unique_vill)
vill<-vill[!is.na(Elec14$PSs)]

for (i in c(1:length(vill))){
	
	if(Elec14$PSs[Elec14$unique_vill==vill[i]][1]<1){
		
Elec14[Elec14$unique_vill==vill[i], c(12:57)]<-Elec14[Elec14$unique_vill==vill[i], c(12:57)]*Elec14[Elec14$unique_vill==vill[i], 58]			
		
	} else if (Elec14$PSs[Elec14$unique_vill==vill[i]][1]>1){

thecases<-Elec14[Elec14$unique_vill==unique(Elec14$unique_vill)[i],]	
Elec14[Elec14$unique_vill==vill[i],c(12:57)][1,]<-colSums(thecases[,c(12:57)][1,], na.rm=T)

} 
print(i)
}

#Remove duplicated census entries
Elec14_census<-Elec14[!duplicated(Elec14$unique_vill) & complete.cases(Elec14$unique_vill),]
Elec14_census<-Elec14_census[,-c(1, 9,10,11)]

names(Elec14_census)
head(Elec14_census)		
	
summary(Elec14_census$PSs)
prop.table(table(Elec14_census$PSs>1))
prop.table(table(Elec14_census$PSs<1))
prop.table(table(Elec14_census$PSs==1))

#######Create tables with summary statistics
mytable<-matrix(nrow=12, ncol=6)

mystatename<-c("Uttarakhand", "Jharkhand", "Rajasthan", "Punjab", "Uttar Pradesh", "Madhya Pradesh", "Bihar", "Himachal Pradesh", "Gujarat", "Andhra Pradesh", "West Bengal") 
mystatenumbers<-c("05", "20", "08", "03", "09", "23", "10", "02", "24", "28", "19")  


for(f in c(1:length(mystatenumbers))){
mytable[f,1]<-mystatename[f]	
mytable[f,3]<-dim(PSdata2014[PSdata2014$State_no2011==as.numeric(mystatenumbers)[f],])[1]
mytable[f,4]<-round(mean(PSdata2014$Votes_total[PSdata2014$State_no2011==as.numeric(mystatenumbers)[f]], na.rm=T))
mytable[f,5]<-round(100*sum(PSdata2014$uniquePS[PSdata2014$State_no2011==as.numeric(mystatenumbers)[f]] %in% merge2014$uniquePS[merge2014$State_no2011==as.numeric(mystatenumbers)[f]])/length(PSdata2014$uniquePS[PSdata2014$State_no2011==as.numeric(mystatenumbers)[f]]),1)
mytable[f,6]<-dim(Elec14_census[Elec14_census$State_no2011==as.numeric(mystatenumbers)[f],])[1]
}

#Adding in official number of PS by state (from General Elections 2024, Press Information Bureau)
mytable[,2]<-c(10078, 24751, 47947, 22019, 140485, 54844, 61721, 7385, 45383, 71225, 77252, NA)

mytable[order(mytable[,1]),]

mytable[12,1]<-"Total"	
mytable[12,2]<-sum(as.numeric(mytable[,2]), na.rm=T)
mytable[12,3]<-dim(PSdata2014)[1]
mytable[12,4]<-round(mean(PSdata2014$Votes_total, na.rm=T))
mytable[12,5]<-round(100*sum(PSdata2014$uniquePS %in% merge2014$uniquePS)/length(PSdata2014$uniquePS), 1)
mytable[12,6]<-dim(Elec14_census)[1]

print(xtable(mytable), include.rownames=FALSE)
