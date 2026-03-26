#devtools::install_version("dummies",version="1.5.6")
library(fixest);library(AER);library(pBrackets);library(dummies);library(rdrobust);library(rddensity);library(survival);library(cmprsk)


# Set the working directory to the parent directory
# For replication, change the directory as needed
setwd("~/Dropbox/Spanish Civil War/replication_material/")


# Create subfolders within the parent directory
subfolders <- c("data", "code","code/paper","code/appendix","figure_output")
for (sub in subfolders) {
  dir.create(sub, recursive = TRUE, showWarnings = FALSE)
}

data<-read.csv("data/IBData.csv")

data$comm<-ifelse(data$pOrg %in% c("CP","CPA","CP/LP","CPNZ","CP/IRC","CPUSA","YCL","CPA/LP","CPI","CPSA","YCL & CP", "CPI/IRC","CPIraq","CPC"),1,0 )

data$other_left <- ifelse(data$pOrg %in% c("","None")==F,1,0) - data$comm 

data$ISCO<-ifelse(data$ocupation=="Painter",7,data$ISCO)
data$ISCO<-ifelse(data$ISCO==10,9,data$ISCO)


data$birthYear<-ifelse(data$birthYear=="191","1910",data$birthYear)
data$birthYear<-gsub("\\?","",data$birthYear)

data$age<-1936 - as.numeric(data$birthYear)


female_names<-c("Felicia","Muriel","Ruth",
                "Doris",   "Bertha","Margaret",
                "Phyllis",  "Ethyl",  "Ethel 'Molly'",
                "Anne", "Ada (Penny)",   "Penny",        
                "Dorothy","Thora","Mary",
                "Ailleen","Marguerita", "Una",  
                "Constance","Ailleen", "Eileen",
                "Elizabeth", "Ethel", "Esther",
                "Frida", "Janet", "Kathleen" ,
                "Margaret" ,  "Marguerita", "Marguerite", "May",
                "Muriel", "Nan", "Phyllis",
                "Rosaleen", "Rosamund", "Rose", "Susan","Thora", "Vera",
                "Winifred"
)


data$woman<-ifelse(data$firstName %in% female_names,1,0)


names(data)[names(data)=="ocupation"] <- "Occupation"


occs<-read.csv(file="data/IB_occ_1.csv")


data<-merge(data,occs,by.x="Occupation",all.x=T)
suppressWarnings(data$Weekly.Wage<-as.numeric(data$Weekly.Wage))

data$Ordered_Occ<- ifelse(data$Occup=="1A",-1,NA)
data$Ordered_Occ<- ifelse(data$Occup=="1B",0,data$Ordered_Occ)
data$Ordered_Occ<- ifelse(data$Occup=="2A",1,data$Ordered_Occ)
data$Ordered_Occ<- ifelse(data$Occup=="2B",2,data$Ordered_Occ)
data$Ordered_Occ<- ifelse(data$Occup=="3",3,data$Ordered_Occ)
data$Ordered_Occ<- ifelse(data$Occup=="4",4,data$Ordered_Occ)
data$Ordered_Occ<- ifelse(data$Occup=="5",5,data$Ordered_Occ)
data$Ordered_Occ<- ifelse(data$Occup=="6",6,data$Ordered_Occ)
data$Ordered_Occ<- ifelse(data$Occup=="7",7,data$Ordered_Occ)



##################

deserter<-rep(NA,nrow(data))

for(i in 1:nrow(data)){
  
  deserter[i]<-grepl("desert", tolower(data$comments[i]),perl=TRUE)
  
}

data$deserter<-ifelse(deserter,1,0)



student<-rep(NA,nrow(data))

for(i in 1:nrow(data)){
  
  student[i]<-grepl("student", tolower(data$Occupation[i]),perl=TRUE)
  
}



data$student<-ifelse(student,1,0)




#####################




arrival_date<-gsub(" ","/",data$arrival)

depart_date<-gsub(" ","/",data$depart)

killed_date<-gsub(" ","/",data$deathDate)




for(i in 1:nrow(data)){
  
  a<-strsplit(arrival_date[i],"/")
  b<-strsplit(depart_date[i],"/")
  c<-strsplit(killed_date[i],"/")
  
  
  if(length(a[[1]])==2){ arrival_date [i]<-paste("28/",arrival_date[i],sep="") }
  if(length(b[[1]])==2){ depart_date [i]<-paste("28/",depart_date[i],sep="") }
  if(length(c[[1]])==2){ killed_date [i]<-paste("28/",killed_date[i],sep="") }
  
  
}

###############





data$depart_date<-as.Date(depart_date,format="%d/%b/%Y")
data$killed_date<-as.Date(killed_date,format="%d/%b/%Y")
data$arrival_date<-as.Date(arrival_date,format="%d/%b/%Y")

data$arrival_pre<-ifelse(data$arrival_date < as.Date("17/July/1936",format="%d/%b/%Y"), 1 , 0)

data$arrival_date[data$arrival_pre==1]<-as.Date("17/July/1936",format="%d/%b/%Y")



data$post_fea <- ifelse(data$arrival_date > as.Date("10/January/1937",format="%d/%b/%Y"),1,0)

data$arrival_date_run<- as.numeric(data$arrival_date- as.Date("11/January/1937",format="%d/%b/%Y"))

data$arrival_date_run2<- as.numeric(data$arrival_date- as.Date("13/January/1937",format="%d/%b/%Y"))




data$killed<-ifelse(!is.na(data$killed_date),1,0)



data$depart_date[data$depart_date=="1193-10-28"]<-as.Date("28/October/1939",format="%d/%b/%Y")                                
data$depart_date[data$depart_date=="37-07-27"]<-as.Date("27/July/1937",format="%d/%b/%Y")


data$stood_down<-ifelse(data$depart_date > as.Date("22/September/1938",format="%d/%b/%Y"),1,0)


data$depart_date[data$stood_down==1]<-as.Date("23/September/1938",format="%d/%b/%Y")


data$status<-ifelse((data$stood_down==1 | data$killed==1),0,1)


data$leave_duration<-data$depart_date-data$arrival_date

data$leave_duration<-ifelse(is.na(data$leave_duration),data$killed_date-data$arrival_date,data$leave_duration)


unique_arrivals<-sort(unique(data$arrival_date))

unique_arrivals<-data.frame(unique_arrivals, c(NA,diff(unique_arrivals)))
names(unique_arrivals)<-c("arrival_date","time_since_last")

num_unique<-tapply(data$arrival_date,data$arrival_date,length)
num_unique<-data.frame(as.Date(names(num_unique)), num_unique)
names(num_unique)<-c("arrival_date","number")


unique_arrivals_2<-merge(unique_arrivals,num_unique,by="arrival_date")

sum((unique_arrivals_2$time_since_last*unique_arrivals_2$number),na.rm = T)/sum(unique_arrivals_2$number)


dat2<-merge(unique_arrivals,data,by="arrival_date")

mean(dat2$time_since_last,na.rm=T)
median(dat2$time_since_last,na.rm=T)
sd(dat2$time_since_last,na.rm=T)

########### Make Mean Number of Communists at any moment############################

data$min_out_date<-data$depart_date
data$min_out_date<-as.Date(ifelse(!is.na(data$killed_date),data$killed_date,data$min_out_date))

dates<-seq(min(data$arrival_date,na.rm=T),max(data$min_out_date,na.rm=T),by=1)


frac_comm<-rep(0,length(dates))

for(i in 1:length(dates)){
  sub<-data[which(data$arrival_date <= dates[i] & data$min_out_date >= dates[i]),]
  
  frac_comm[i]<-mean(sub$comm,na.rm=T)
  
}

date_frac<-data.frame(dates,frac_comm)

data$mean_comms<-NA

for(i in 1:nrow(data)){
  data$mean_comms[i]<-mean(date_frac[date_frac[, 1] >= data[i,"arrival_date"] & date_frac[, 1] <=  data[i,"min_out_date"],2],na.rm=T) 
}





df_cnt<-data.frame(df_cnt<-tapply(data$comm,data$country,mean,na.rm=T),names(df_cnt))
names(df_cnt)<-c("mean_country","country")
data<-merge(df_cnt,data,by="country")



