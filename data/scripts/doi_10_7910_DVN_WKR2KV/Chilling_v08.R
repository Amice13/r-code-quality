###################################
##   Chilling or learning effect?##
##### Dynamic Panel Analysis ######
###################################

# installing/loading the latest installr package:
install.packages("installr"); library(installr) # install+load installr
updateR() # updating R.


# Remove all
rm(list=ls())


#Load packages
library(car)
#library(pscl)
#library(AER)
library(MASS)
#library(mlogit)     #MLR with reshaping the data
library(nnet)       #Multinomial regression without reshaping data
library(plm)        #Panel data analysis
library(mice)       #Missing data
#library(calibrate)  #Label plot
#library(Hmisc)      #Summary statistics of two datasets
library(stargazer)  #Extract tables to Latex
library(corrplot)   #Produce some nice correlation plots
library(foreign)    #Read different files (dta, sppss)
library(plyr)
library(ggplot2)
library(brms)
library(MCMCglmm)
library(DataCombine)



#Set working Directory
getwd()
setwd("C:/Users/u0112618/Documents/1. Nicolas/1. KU Leuven/1. Euthority/3.R Analysis/2. Paper II")

setwd("C:/Users/u0112618/Desktop/2. Paper II")


#Strasbourg BETA server
setwd("C:/Users/BetaEdf/Desktop/2. Paper II")


#Functions
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

lag1 <- function(x)c(NA, x[1:(length(x)-1)])

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#########
#Load the data
DJR<-read.csv2("DJR_dates.csv")

#########
#Clean up the database remove non-necessary rows, NS and Removals
DJR<-subset(DJR,DJR$Submit<2015)

#Exclude removals
DJR<-subset(DJR, !(DJR$Case %in% "NS"))
DJR<-subset(DJR, !(DJR$Decision %in% 4))
DJR<-DJR[!is.na(DJR$Judgment),]
DJR<-DJR[!is.na(DJR$Judgment_date),]
DJR$Year<-as.numeric(paste(substrRight(as.character(DJR$Judgment_date),4)))

DJR$Diff.year<-DJR$Year-DJR$Submit
str(DJR)
names(DJR)
DJR<-DJR[,c(-2,-3)]

#########
#Load economic data
GDP<-read.csv2("GDP.csv")              # Gross domestic product, Total economy by employee (Income approach) {Unit: EUR/Mrd} [Source: Eurostat Ameco database]
Pop<-read.csv2("POP.csv")              # Total Population (France Metropolitan) {Unit: 1000 Persons} [Source: Eurostat Ameco database]
#EMP<-read.csv2("EMP_serv.csv")         # Employees in services {Unit: 1000 Persons} [Source: Eurostat Ameco database]
Export<-read.csv2("Export.csv")         # Intra-EU export of goods and services {Unit: EUR/Mrd} [[Source: Eurostat Ameco database]]
Import<-read.csv2("Import.csv")         # Intra-EU import of goods and services {Unit: EUR/Mrd} [[Source: Eurostat Ameco database]]


#Judges<-read.csv2("Judges.csv")              # United Nations Crime and Drugs database Total number of Judges
#Judg.pop<-read.csv2("Judges.pop.csv")   

###
#Political and legal variables

Euro<-read.csv("Euro_baro.csv")              #Eurobarometer Political support  
Veto.wb<-read.csv2("Veto.WB.csv")            #Veto World Bank Data 
Jud.r<-read.table("ccpcnc_v2.txt",
                  sep="\t", 
                  fill=FALSE, 
                  strip.white=TRUE) #Judicial review
Jud.ccp<-read.csv2("Judiciary.csv")
Peak.courts<-read.csv2("Peak.courts.csv")
Peak.courts<-Peak.courts[1:28,-1]


#Change colnames
colnames(GDP)<-c("Country",2014:1961)
colnames(Pop)<-c("Country",1961:2014)
#colnames(EMP)<-c("Country",1961:2014)
colnames(Import)<-c("Country",1961:2014)
colnames(Export)<-c("Country",1961:2014)


#Agreggate the data
#Transform to longitudinal data 
panel<-plm.data(DJR,index=c("Court","Submit"))
head(panel)
panel$Decision<-as.factor(panel$Decision)
panel$Level<-as.factor(panel$Level)
panel$Joined<-as.factor(panel$Joined)
panel$Judgment<-as.factor(panel$Judgment)
panel$Type<-as.factor(panel$Type)
panel$Case<-as.numeric(panel$Case)
panel$Year<-as.factor(panel$Year)
panel$TO<-as.factor(panel$TO)
str(panel)

#Check the relationship between case and trade over time
Decision<-aggregate(panel$Decision ~ panel$Court+panel$Submit,FUN=summary)
colnames(Decision)<-c("Court","Submit","Decision")

Level<-aggregate(panel$Level ~ panel$Court+panel$Submit,FUN=summary)
colnames(Level)<-c("Court","Submit","Level")

TO.f<-aggregate(panel$TO ~ panel$Court+panel$Submit,FUN=summary)
colnames(TO.f)<-c("Court","Submit","TO.f")


#Type<-aggregate(panel$Type ~ panel$Court+panel$Year,FUN=summary)
#colnames(Type)<-c("Court","Year","Type")

#
str(panel)
panel.data <- ddply(panel, .(Court, Submit), summarize, Case = length(Case),Country=Mode(Country),
                   TO=Mode(TO),Year=Mode(Year),Joined=Mode(Joined),Judgment=Mode(Judgment))

head(panel.data)
#Merging the three data frames
panel.data<-merge(panel.data,Decision,by.x=c("Court","Submit"),by.y=c("Court","Submit"))
panel.data<-merge(panel.data,Level,by.x=c("Court","Submit"),by.y=c("Court","Submit"))
panel.data<-merge(panel.data,TO.f,by.x=c("Court","Submit"),by.y=c("Court","Submit"))

#panel.data<-merge(panel.data,Type,by.x=c("Court","Year"),by.y=c("Court","Year"))
str(panel.data)

head(panel.data)

panel.data$Decision.1<-panel.data$Decision[,1]
panel.data$Decision.2<-panel.data$Decision[,2]
panel.data$Decision.5<-panel.data$Decision[,3]
#panel.data$Decision.5<-panel.data$Decision[,4]


panel.data$Level.1<-panel.data$Level[,1]
panel.data$Level.2<-panel.data$Level[,2]
panel.data$Level.3<-panel.data$Level[,3]
panel.data$Level.4<-panel.data$Level[,4]
panel.data$Level.5<-panel.data$Level[,5]

panel.data$TO.f.0<-panel.data$TO.f[,1]
panel.data$TO.f.1<-panel.data$TO.f[,2]
panel.data$TO.f.2<-panel.data$TO.f[,3]
panel.data$TO.f.3<-panel.data$TO.f[,4]
panel.data$TO.f.4<-panel.data$TO.f[,5]


names(panel.data)
panel.data<-panel.data[,c(-9,-10,-11)]

head(panel.data)

#Time period 1961-2015
#panel.data<-panel.data[which(as.numeric(panel.data$Year)==2018),]

#Create dummy variable for peak courts
for (i in 1:nrow(panel.data)){
  if (panel.data$Level.3[i]>0){
    panel.data$peak[i]<-1}
  else {panel.data$peak[i]<-0}
}


#Expand the panel data
iddat <- expand.grid(id = unique(panel.data$Court), time = (c(1961:2014)))
iddat <- iddat[order(iddat$id, iddat$time), ]
panel.data <- merge(panel.data, iddat, all.x=TRUE, all.y=TRUE, by.x=c("Court", "Submit"),by.y=c("id","time"))

head(panel.data)
#Include 0 
panel.data$Submission<-0 #Variable Not Submitting==1, 0 otherwise

for(i in 1:nrow(panel.data)){
  if(is.na(panel.data$Case[i])){
    panel.data$Case[i]<-0
    panel.data$Decision.1[i]<-0
    panel.data$Decision.2[i]<-0
    panel.data$Submission[i]<-1
    panel.data$Decision.5[i]<-0
    panel.data$peak[i]<-0
    panel.data$Country[i]<-0
    panel.data$TO.f.0[i]<-0
    panel.data$TO.f.1[i]<-0
    panel.data$TO.f.2[i]<-0
    panel.data$TO.f.3[i]<-0
    panel.data$TO.f.4[i]<-0
    }
}

#Observations from the first time the court enter in the panel
panel.data$block<-NA
l<-nrow(panel.data)
j<-1
panel.data$peakcourt<-NA
panel.data$Court<-as.numeric(paste(panel.data$Court))
panel.data<-panel.data[order(panel.data$Court),]
rownames(panel.data)<-c(1:nrow(panel.data))
#Complete the peak court variable
for (i in seq(1,l,by=54)){
  panel.data$block[i:(i+53)]<-j
  if (any(panel.data$peak[i:(i+53)]>0)){
    panel.data$peakcourt[i:(i+53)]<-1}
  else{panel.data$peakcourt[i:(i+53)]<-0}
  j<-j+1
}

###
#Complete the country variable 
for (i in 1:nrow(panel.data)){
  if (nchar(as.character(panel.data$Court[i]))==3){
    panel.data$Country[i]<-substring(panel.data$Court[i],1,1)}
  else if (nchar(as.character(panel.data$Court[i]))==4){
    if(substring(panel.data$Court[i],1,2)>28){
      panel.data$Country[i]<-substring(panel.data$Court[i],1,1)}
    else{panel.data$Country[i]<-substring(panel.data$Court[i],1,2)}
  }
  else if (nchar(as.character(panel.data$Court[i]))==5){
    panel.data$Country[i]<-substring(panel.data$Court[i],1,2)}
}


# Create Years of Membership
Fg<-c(6,5,10,11,9,2)
Enl.1<-c(15,3,8)
Enl.2<-c(7)
Enl.3<-c(12,13)
Enl.4<-c(1,4,14)
Enl.5<-c(16:25)
Enl.6<-c(26:27)
Enl.7<-c(28)

#Create the variable EC Membership years
panel.data$yec<-NA

for (i in 1:nrow(panel.data)){
  if(as.numeric(panel.data$Country[i])%in%Enl.7==T)
  {panel.data$yec[i]<-as.numeric(paste(panel.data$Submit[i]))-2013}
  else{
    if(as.numeric(panel.data$Country[i])%in%Enl.6==T)
    {panel.data$yec[i]<-as.numeric(paste(panel.data$Submit[i]))-2007}
    else{
      if(as.numeric(panel.data$Country[i])%in%Enl.5==T)
      {panel.data$yec[i]<-as.numeric(paste(panel.data$Submit[i]))-2004}
      else{
        if(as.numeric(panel.data$Country[i])%in%Enl.4==T)
        {panel.data$yec[i]<-as.numeric(paste(panel.data$Submit[i]))-1995}
        else{
          if(as.numeric(panel.data$Country[i])%in%Enl.3==T)
          {panel.data$yec[i]<-as.numeric(paste(panel.data$Submit[i]))-1986}
          else{
            if(as.numeric(panel.data$Country[i])%in%Enl.2==T)
            {panel.data$yec[i]<-as.numeric(paste(panel.data$Submit[i]))-1981}
            else{
              if(as.numeric(panel.data$Country[i])%in%Enl.1==T)
              {panel.data$yec[i]<-as.numeric(paste(panel.data$Submit[i]))-1973}
              else{
                if(as.numeric(panel.data$Country[i])%in%Fg==T)
                {panel.data$yec[i]<-as.numeric(paste(panel.data$Submit[i]))-1951}  
              }}}}}}}
}


#### 
# Identify first submission of the court i
panel.data$FirstSub<-0

for (i in seq(1,nrow(panel.data),by=54)){
  if (any(panel.data$Case[i:(i+53)]>0)){
  Id<-which(panel.data$Case[i:(i+53)]>0)[1]
  panel.data$FirstSub[(Id+i-1)]<-1}
}

View(panel.data)

#New Member States

#Need to adjust for enlargement year
court.data<-panel.data[which(panel.data$yec>=0),]
str(court.data)
court.data$Country<-as.numeric(court.data$Country)

court.data$NMS<-NA

New_member<-c(1,4,14,16:28)

for (i in 1:nrow(court.data)){  
  if(court.data$Country[i] %in% New_member){
  court.data$NMS[i]<-1}
  else{court.data$NMS[i]<-0}
  }


#Add number of peak courts
#court.data<-merge(court.data,Peak.courts,by.x=c("Country"),by.y=c("Code"))



###############
#Lagging variables

library(dplyr)
library(DataCombine)
library(plyr)


#court.data<-court.data[with(court.data, order(Court,Submit)), ]
#court.data$Court<-as.factor(court.data$Court)


#1: Chilling Hypothesis: Order (numeric) Lag variables (t-1)

court.data<-court.data[with(court.data, order(Court,Submit)), ]
rownames(court.data)<-1:nrow(court.data)
court.data$Lag.order1<-0
k<-0

for(j in 1:unique(court.data$block)[length(unique(court.data$block))]){
  l<-length(court.data$block[court.data$block==j])
  for (i in 1:(l-1)){
  if((!is.na(court.data$Year[i+k]))&(as.numeric(paste(court.data$Year[i+k]))<2014)){
    diff<-as.numeric(paste(court.data$Year[i+k]))-as.numeric(paste(court.data$Submit[i+k]))
    if(court.data$Decision.2[i+k]){
    court.data$Lag.order1[(i+k+diff+1)]<-court.data$Decision.2[i+k]}
    else if(court.data$Judgment[i+k]==0){
    court.data$Lag.order1[(i+k+diff+1)]<-1}
    }
    }
  k<-k+l
}

View(court.data)

#2: Chilling Hypothesis: Type of Order (ordinal) Lag variables (t-1)
court.data<-court.data[with(court.data, order(Court,Submit)), ]
rownames(court.data)<-1:nrow(court.data)
court.data$Lag.TO<-0
k<-0

for(j in 1:unique(court.data$block)[length(unique(court.data$block))]){
  l<-length(court.data$block[court.data$block==j])
  for (i in 1:(l-1)){
    if((!is.na(court.data$Year[i+k]))&(as.numeric(paste(court.data$Year[i+k]))<2014)){
      diff<-as.numeric(paste(court.data$Year[i+k]))-as.numeric(paste(court.data$Submit[i+k]))
      if (court.data$TO.f.4[i+k]>0){
        court.data$Lag.TO[(i+k+diff+1)]<-4}
      else if (court.data$TO.f.3[i+k]>0){
        court.data$Lag.TO[(i+k+diff+1)]<-3}
      else if (court.data$TO.f.2[i+k]>0){
        court.data$Lag.TO[(i+k+diff+1)]<-2}
      else if (court.data$TO.f.1[i+k]>0){
        court.data$Lag.TO[(i+k+diff+1)]<-1}
      else if (court.data$TO.f.0[i+k]>0){
        court.data$Lag.TO[(i+k+diff+1)]<-0}
    }
  }
  k<-k+l
}

View(court.data)


#court.data<-ddply(court.data, .(Court), transform, Lag.order1 = lag1(Decision.2))
#Lag variables (t-2)
#court.data<-slide(court.data, Var="Case", TimeVar="Year", GroupVar="Court", NewVar="Lag.ref2", 
#                  slideBy = -2, keepInvalid = FALSE, reminder = FALSE)
#Lag variables (t-3)
#court.data<-slide(court.data, Var="Case", TimeVar="Year", GroupVar="Court", NewVar="Lag.ref3", 
#                  slideBy = -3, keepInvalid = FALSE, reminder = FALSE)



##############################
### MACROECONOMIC VARIABLE ###
##############################

#Transform proxy variables from wide to long format
panel.gdp <- reshape(GDP, 
                     varying = list(names(GDP)[2:55]), 
                     v.names="Gdp",
                     timevar = "Submit", 
                     times=2014:1961,
                     new.row.names =1:1512,
                     idvar = "Country", 
                     direction="long" 
)
panel.gdp <- panel.gdp[order(panel.gdp$Country),]


#panel.emp <- reshape(EMP, 
#                      varying = list(names(EMP)[2:55]), 
#                      v.names="Emp",
#                      timevar = "Year", 
#                      times=1961:2014,
#                      new.row.names =1:1512,
#                      idvar = "Country", 
#                      direction="long" 
#)
#panel.emp <- panel.emp[order(panel.emp$Country),]

panel.pop <- reshape(Pop, 
                     varying = list(names(Pop)[2:55]), 
                     v.names="Pop",
                     timevar = "Submit", 
                     times=1961:2014,
                     new.row.names =1:1512,
                     idvar = "Country", 
                     direction="long" 
)
panel.pop <- panel.pop[order(panel.pop$Country),]

panel.exp <- reshape(Export, 
                     varying = list(names(Export)[2:55]), 
                     v.names="Export",
                     timevar = "Submit", 
                     times=1961:2014,
                     new.row.names =1:1512,
                     idvar = "Country", 
                     direction="long" 
)
panel.exp <- panel.exp[order(panel.exp$Country),]

panel.imp <- reshape(Import, 
                     varying = list(names(Import)[2:55]), 
                     v.names="Import",
                     timevar = "Submit", 
                     times=1961:2014,
                     new.row.names =1:1512,
                     idvar = "Country", 
                     direction="long" 
)
panel.imp <- panel.imp[order(panel.imp$Country),]


#Merge the proxies with the longitudinal data
pdata<-merge(court.data,panel.gdp,by.x=c('Country','Submit'),by.y=c('Country','Submit'),all=F)
pdata<-merge(pdata,panel.pop,by.x=c('Country','Submit'),by.y=c('Country','Submit'),all=F)
#pdata<-merge(pdata,panel.emp,by.x=c('Country','Submit'),by.y=c('Country','Submit'),all=F)
#pdata<-merge(pdata,panel.exp,by.x=c('Country','Submit'),by.y=c('Country','Submit'),all=F)
#pdata<-merge(pdata,panel.imp,by.x=c('Country','Submit'),by.y=c('Country','Submit'),all=F)


#Create variable GDP per capita
pdata$Gdp.capita<-pdata$Gdp/pdata$Pop

#Creat variable Absolute number of Intra-EU Trade
#pdata$Trade.abs<-pdata$Export+pdata$Import

#Creat variable Intra-EU trade per GDP
#pdata$Trade.gdp<-pdata$Trade/pdata$Gdp

##################################
### POLITICAL & LEGAL VARIABLE ###
##################################

#Political support
Euro<-Euro[,c(2:4)]
Euro$country<-as.factor(Euro$country)
Euro$year<-as.factor(Euro$year)

pdata<-merge(pdata,Euro,by.x=c("Country", "Submit"), by.y=c("country", "year"),all=F)

###
#Veto players checks
Veto<-subset(Veto.wb,select=c("ifs","year","checks"))
colnames(Veto)<-c("Country","Year","Veto.checks")

pdata<-merge(pdata,Veto,by.x=c("Country", "Submit"), by.y=c("Country", "Year"),all=F)

###
Jud.r<-read.csv2("Jud.review.csv")
str(Jud.r)

Jud.r<-subset(Jud.r,select=c("cowcode","year","chalstag","treatst","judind"))

Jud.r$scope<-NA
Jud.r$exist<-NA
Jud.r$monism<-NA
Jud.r$Const<-NA

Jud.r<-subset(Jud.r,!is.na(Jud.r$chalstag))

for (i in 1:nrow(Jud.r)){
  if (Jud.r$chalstag[i]==98)
  {Jud.r$scope[i]<-0
   Jud.r$exist[i]<-0
  }
  else{Jud.r$scope[i]<-Jud.r$chalstag[i]
       Jud.r$exist[i]<-1
  }
}

Jud.r$scope<-Jud.r$scope+1

for (i in 1:nrow(Jud.r)){
  if (Jud.r$treatst[i]==1)
  {Jud.r$monism[i]<-1}
  else{Jud.r$monism[i]<-0}
}

head(Jud.r)

colnames(Jud.r)<-c("Code","Time","chalstag","treatst","judind","scope","existence","monism","Const")


#Create constitution variable: No Austiran vs Austrian model
Const<-c("3","17","5","7","12","14")

for (i in 1:nrow(Jud.r)){
  if(as.numeric(Jud.r$Code[i])%in%Const==T)
  {Jud.r$Const[i]<-0}
  else{ Jud.r$Const[i]<-1}
}

head(Jud.r)
Jud.r<-Jud.r[,-3]

pdata<-merge(pdata,Jud.r,by.x=c("Country","Submit"),by.y=c("Code","Time"),all=F)

### 
#Elkins database CCP
Judic<-read.csv2("Judiciary.csv")
Judic<-Judic[which(Judic$year>1960 & Judic$year<2016),]
Judic$country<-as.factor(Judic$country)
Judic$year<-as.factor(Judic$year)
Judic$Code<-as.factor(Judic$Code)

pdata$Country<-as.factor(pdata$Country)

pdata<-merge(pdata,Judic,by.x=c("Country","Submit"),by.y=c("Code","year"),all=F)



# Judicial power index
pdata$Judicial.power<-(as.numeric(pdata$decentr))+(as.numeric(pdata$const))+(as.numeric(pdata$supct))+
  (as.numeric(pdata$supcham))+(as.numeric(pdata$exec))+(as.numeric(pdata$leg))+
  (as.numeric(pdata$public))+(as.numeric(pdata$cts))+(as.numeric(pdata$chalothr))+
  (as.numeric(pdata$void))-(as.numeric(pdata$indcase))-(as.numeric(pdata$retleg))-
  (as.numeric(pdata$effother))

pdata$Judicial.power<-as.integer(pdata$Judicial.power)


#########
# Final Data
names(pdata)
pdata<-pdata[,-12:-16] #delete levels

# Dummy
str(pdata)
pdata$Country<-as.factor(pdata$Country)
pdata$Court<-as.factor(pdata$Court)
pdata$peakcourt<-as.factor(pdata$peakcourt)
pdata$Submission<-as.factor(pdata$Submission)
pdata$block<-as.factor(pdata$block)
pdata$existence<-as.factor(pdata$existence)
pdata$monism<-as.factor(pdata$monism)
pdata$Const<-as.factor(pdata$Const)
pdata$FirstSub<-as.factor(pdata$FirstSub)
pdata$NMS<-as.factor(pdata$NMS)
names(pdata)
names <- c(38:51)
pdata[,names] <- lapply(pdata[,names] , factor)
str(pdata)
pdata<-pdata[,c(-36,-51)]

#Export data set to csv file
write.csv2(pdata,"chilling.panel.submit.csv")

#Load data
pdata<-read.csv2("chilling.panel.submit.csv")

################################
# Checking for missing data
summary(pdata)

library(VIM)
md.pattern(pdata)


aggr_plot <- aggr(pdata, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(pdata), 
                  cex.axis=.7, gap=1, ylab=c("Histogram of missing data","Pattern"))


################################
#Number of references per court
Nosubmit<-aggregate(Case ~ Court, data = pdata, sum)
Nosubmit<-Nosubmit[which(Nosubmit$Case>=1),]
Nosubmit<-Nosubmit[order(-Nosubmit$Case),]

mp<-barplot(Nosubmit$Case,xaxt="n",ylim=c(0,450),
        horiz = F, las = 2, main = "",ylab="Number of preliminary references")
axis(1,at=mp,labels=as.character(Nosubmit$Court),cex.axis=0.7)

ggplot(data = Nosubmit, aes(x = Court, y = Case)) +
  geom_bar(stat = "identity", position = "stack") 


##########################################
#Check the probability of rejection before
#and after 1995

data.61<-pdata[which(as.numeric(paste(pdata$Year))<1995),]
data.61<-data.61[,c(2:5,7)]
data.61<-na.omit(data.61)
data.14<-pdata[which(as.numeric(paste(pdata$Year))>=1995),]
data.14<-data.14[,c(2:5,7)]
data.14<-na.omit(data.14)

data.61<-data.61[order(data.61$Court),]
data.61<-data.61[!duplicated(data.61[c("Year", "Court")]),]
rownames(data.61)<-1:16957

data.14<-data.14[order(data.14$Court),]
data.14<-data.14[!duplicated(data.14[c("Year", "Court")]),]
rownames(data.14)<-1:17118

Num.rej<-0
Country<-unique(data.61$Country)

for (j in Country){
  data<-data.61[which(data.61$Country==j),]
  Nosubmit<-aggregate(Case ~ Court, data = data, sum)
  Nosubmit.Id<-Nosubmit$Court[which(Nosubmit$Case<1)]
  data<-subset(data,!(Court %in% Nosubmit.Id))
  l<-length(unique(data$Year))

for (i in seq(1,nrow(data),by=l)){
  if (any(data$Case[i:(i+(l-1))]>0)){
    Id<-which(data$Case[i:(i+(l-1))]>0)[1]
    Rej<-which(data$Decision.2[(Id+i):(i+(l-1))]>0)
    Num.rej<-Num.rej+length(Rej)
    print(Num.rej)}
}}

####
#Probability of rejection in 1961
p.61<-(Num.rej/nrow(data.61))*100
print(p.61)

Num.rej<-0
Country<-unique(data.14$Country)

for (j in Country){
  data<-data.14[which(data.14$Country==j),]
  Nosubmit<-aggregate(Case ~ Court, data = data, sum)
  Nosubmit.Id<-Nosubmit$Court[which(Nosubmit$Case<1)]
  data<-subset(data,!(Court %in% Nosubmit.Id))
  l<-length(unique(data$Year))
  
  for (i in seq(1,nrow(data),by=l)){
    if (any(data$Case[i:(i+(l-1))]>0)){
      Id<-which(data$Case[i:(i+(l-1))]>0)[1]
      Rej<-which(data$Decision.2[(Id+i):(i+(l-1))]>0)
      Num.rej<-Num.rej+length(Rej)
      print(Num.rej)}
  }}

####
#Probability of rejection in 1961
p.14<-(Num.rej/nrow(data.14))*100
print(p.14)

##########################################
# Restrict data to >1995 and Prelim ref>1
##########################################
pdata<-na.omit(pdata)
#pdata<-pdata[which(as.numeric(paste(pdata$Year))>=1995),]

pdata<-pdata[order(pdata$block),]

#Create lag specific variable 
rownames(pdata)<-1:nrow(pdata)
print(unique(pdata$block))

check<-seq(1,nrow(pdata),by=1)
test<-unique(pdata$block)
which(!check %in% test) 

pdata.learn<-pdata
Id<-unique(pdata.learn$block)

#####
#Add pending cases
Pending<-read.csv2("Pending.cases.csv")
colnames(Pending)[1]<-"Submit"
Pending$Submit<-as.factor(Pending$Submit)
pdata<-merge(pdata,Pending)

#####
# Learning Hypothesis
pdata.learn<-subset(pdata,select=c(Country,
                                   Court,
                                   Submit,
                                   Case,
                                   peakcourt,
                                   yec,
                                   NMS,
                                   Lag.order1,
                                   Lag.TO,
                                   Gdp.capita,
                                   pol.supp,
                                   block,
                                   Decision.2,
                                   Pend
))
str(pdata.learn)
pdata.learn<-na.omit(pdata.learn)

pdata.learn$Lag<-NA
pdata.learn<-pdata.learn[with(pdata.learn, order(Court,Submit)), ]
Id<-unique(pdata.learn$block)

j<-0
for (i in Id){
  l<-length(pdata.learn$block[pdata.learn$block==i])
  pdata.learn$Lag[(1+j):(l+j)]<-seq(0,l,by=1)
  j<-j+l 
} 

Nosubmit<-aggregate(Case ~ Court, data = pdata.learn, sum)
Nosubmit.Id<-Nosubmit$Court[which(Nosubmit$Case<1)]

pdata.learn<-subset(pdata.learn,!(Court %in% Nosubmit.Id))

################################
# Run Classification Tree

library(rpart)
library(rpart.plot)
library(lme4)
source("glmertree.R")
library(partykit)
library(caret)

################################

# Run Classification Tree 
str(pdata)
pdata$Country<-as.factor(pdata$Country)
pdata$Year<-as.factor(pdata$Year)
pdata$Court<-as.factor(pdata$Court)
pdata$peakcourt<-as.factor(pdata$peakcourt)
pdata$Submiss<-as.factor(pdata$Submiss)
pdata$monism<-as.factor(pdata$monism)
pdata$Const<-as.factor(pdata$Const)
pdata$Lag.order1<-as.factor(pdata$Lag.order1)
pdata$existence<-as.factor(pdata$existence)
pdata$TO<-as.factor(pdata$TO)


#####
#First sample: Hypothesis 1-2 Chilling
pdata.chilling<-pdata
pdata.chilling$Lag.order1<-as.integer(pdata.chilling$Lag.order1)
pdata.chilling$Lag.TO<-as.integer(pdata.chilling$Lag.TO)
str(pdata.chilling)

#Convert to numerci to Lag the variable
#pdata.chilling$TO<-as.numeric(paste(pdata.chilling$TO.ord))

####
#Lag (t-1)
#pdata.chilling<-ddply(pdata.chilling, ~Court, transform, Lag.TO = lag1(TO))

#pdata.chilling<-na.omit(pdata.chilling)

#Select the necessary data
pdata.chilling<-subset(pdata.chilling, select=c(Country,
                                         Court,
                                         Submit,
                                         Case,
                                         peakcourt,
                                         yec,
                                         NMS,
                                         Lag.order1,
                                         Lag.TO,
                                         Gdp.capita,
                                         pol.supp,
                                         Veto.checks,
                                         monism,
                                         Const,
                                         judrev,
                                         Judicial.power,
                                         FirstSub,
                                         block
                                         ))

pdata.chilling<-na.omit(pdata.chilling)
str(pdata.chilling)

#####
#Observe courts which do not submit
Nosubmit<-aggregate(Case ~ Court, data = pdata.chilling, sum)
Nosubmit.Id<-Nosubmit$Court[which(Nosubmit$Case<1)]
pdata.chilling<-subset(pdata.chilling,!(Court %in% Nosubmit.Id))
str(pdata.chilling)

#Training/Validation/Testing set
idxTrain <- sample(nrow(pdata.chilling),as.integer(nrow(pdata.chilling)*0.70)) #Training group 70% of sample
idxNotTrain <- which(! 1:nrow(pdata.chilling) %in% idxTrain ) # Rest
idxVal <- sample(idxNotTrain,as.integer(length(idxNotTrain)*0.30)) #Validation
idxTest <- idxNotTrain[which(! idxNotTrain %in% idxVal)] #Testing group

#Glmertree

#########
#Dependent variable: Preliminary references in time t per Court i 
f<-Case~1|(1|Country)+(1|Court)|peakcourt+Lag.order1+Lag.TO+Gdp.capita+yec+NMS+
  pol.supp+Veto.checks+monism+Const+Judicial.power

#GlmerTree
mixedtree<-glmertree(f,data=pdata.chilling[idxTrain,],
                     family=poisson(),alpha = 0.05, prune = "BIC")

plot(mixedtree)

ranef(mixedtree)

pred <- predict(mixedtree, newdata=pdata.chilling[idxTest,],allow.new.levels=T)
actual<-pdata.chilling[idxTest,]$Case

#Different levels
x <- as.integer(pred)
y <- pdata.chilling[idxTest,]$Case
l <- union(x, y)
Table2 <- table(factor(x, l), factor(y, l))
confusionMatrix(Table2)

##############
#Dependent variable: Order in time t per Court i 

#Create outcome variable
pdata.learn$Order<-NA

for (i in 1:nrow(pdata.learn)){
  if(pdata.learn$Decision.2[i]>0){
    pdata.learn$Order[i]<-1}
  else{pdata.learn$Order[i]<-0}
}
pdata.learn<-na.omit(pdata.learn)

#Training and Testing Set
idxTrain <- sample(nrow(pdata.learn),as.integer(nrow(pdata.learn)*0.7)) #Training group 70% of sample
idxNotTrain <- which(! 1:nrow(pdata.learn) %in% idxTrain ) # Rest
idxVal <- sample(idxNotTrain,as.integer(length(idxNotTrain)*0.3)) #Validation
idxTest <- idxNotTrain[which(! idxNotTrain %in% idxVal)] #Testing group


#Formula
f<-Order~1|(1|Country)+(1|Court)|peakcourt+Emp+Gdp.capita+Emp+yec+
  pol.supp+Veto.checks+monism+const+Judicial.power

#GlmmerTree
mixedtree<-glmertree(f,data=pdata.learn[idxTrain,],
                     family=binomial(link="logit"),alpha = 0.05, prune = "BIC")

plot(mixedtree)

pred <- predict(mixedtree, newdata=pdata.learn[idxTest,],allow.new.levels=T)
actual<-pdata.learn[idxTest,]$Order

#Different levels
x <- as.integer(pred)
y <- pdata.learn[idxTest,]$Order
l <- union(x, y)
Table2 <- table(factor(x, l), factor(y, l))
confusionMatrix(Table2)


################################
#Testing the chilling hypothesis
#Bayesian estimation
################################
library(MCMCglmm)
library(brms) 

# For negative binomial distribution family=negbin
# With informative flat prior

pdata.bay<-na.omit(pdata.chilling)
str(pdata.bay)
pdata.bay$Lag.order1<-as.integer(pdata.bay$Lag.order1)
summary(pdata.bay)



##
#Use binary variable for testing poor drafting hypothesis
#for (i in 1:nrow(pdata.bay)){
#  if (pdata.bay$TO[i]==4){
#    pdata.bay$TO.f[i]<-1}
#  else {pdata.bay$TO.f[i]<-0}
#  }

####
#Convert to numerci to Lag the variable
#pdata.bay$TO.f<-as.numeric(paste(pdata.bay$TO.f))
#pdata.bay$TO<-as.numeric(paste(pdata.bay$TO))

####
#Lag (t-1)
#pdata.bay<-ddply(pdata.bay, ~Court, transform, Lag.TO = lag1(TO))
#pdata.bay<-ddply(pdata.bay, ~Court, transform, Lag.TO.f = lag1(TO.f))

#pdata.bay<-na.omit(pdata.bay)

####
#Convert back to factor
#pdata.bay$Lag.TO.f<-as.factor(paste(pdata.bay$Lag.TO.f))
#str(pdata.bay)

###
#Oberve only courts which had poor drafting quality
#Id.draft<-unique(pdata.bay$Court[which(pdata.bay$TO==2)])
#pdata.draft<-pdata.bay[pdata.bay$Court %in% Id.draft,]

#pdata.draft<-na.omit(pdata.draft)

###########
# 3 Hypothesis: First Submitter's preliminary references are prone to end in an order

pdata.first<-subset(pdata, select=c(Country,
                                    Court,
                                    Submit,
                                    Case,
                                    peakcourt,
                                    yec,
                                    NMS,
                                    Lag.order1,
                                    Lag.TO,
                                    Gdp.capita,
                                    pol.supp,
                                    Veto.checks,
                                    monism,
                                    Const,
                                    judrev,
                                    Judicial.power,
                                    FirstSub,
                                    block))

pdata.first<-na.omit(pdata.first)
str(pdata.chilling)

pdata.first$FirstSub<-as.numeric(paste(pdata.first$FirstSub))


Nofirst<-aggregate(FirstSub ~ Court, data = pdata.first, sum)
Nofirst.Id<-Nofirst$Court[which(Nofirst$FirstSub<1)]

pdata.first<-subset(pdata.first,!(Court %in% Nofirst.Id))

pdata.first<-pdata.first[with(pdata.first, order(Court,Submit)), ]
str(pdata.first)

# Select those observations from FirstSub==1
test<-pdata.first[1:100,]

rownames(test)<-1:nrow(test)
test$Check<-0
i<-1
k<-c(unique(test$block))

for(j in unique(test$block)){
  l<-length(test$block[test$block==j])
    if (any(test$FirstSub[i:(i+l-1)]>0)){
      Id.firstsub<-which(test$FirstSub[i:(i+l-1)]==1)
      test$Check[(i+Id.firstsub-1):(i+l-1)]<-1
      i<-i+l    
      }
}


#####

rownames(pdata.first)<-1:nrow(pdata.first)
pdata.first$Check<-0
i<-1

for(j in unique(pdata.first$block)){
  l<-length(pdata.first$block[pdata.first$block==j])
  if (any(pdata.first$FirstSub[i:(i+l-1)]>0)){
    Id.firstsub<-which(pdata.first$FirstSub[i:(i+l-1)]==1)
    pdata.first$Check[(i+Id.firstsub-1):(i+l-1)]<-1
    i<-i+l    
  }
}

pdata.first<-pdata.first[which(pdata.first$Check==1),]
str(pdata.first)

pdata.first$Lag.order1<-as.integer(pdata.first$Lag.order1)

###
#Dirstibution of panel data restricted to FirstSubmission
x<-pdata.first$Case
fc<-plot(as.factor(x),ylim=c(0,12000),xlab="Number of preliminary references",
         ylab="Frequencies",cex.main=1.8,col=rgb(0,0,1,1/2),
         xaxt="n",cex.lab=1.2)#,col=c("darkcyan"))
axis(1,at=fc,labels=levels(as.factor(x)),cex.axis=1.3)

#########################
#Distribution of the data
#########################
x<-pdata.bay$Case
fc<-plot(as.factor(x),ylim=c(0,12000),xlab="Number of preliminary references",
         ylab="Frequencies",cex.main=1.8,col=rgb(0,0,1,1/2),
         xaxt="n",cex.lab=1.2)#,col=c("darkcyan"))
axis(1,at=fc,labels=levels(as.factor(x)),cex.axis=1.3)

#QQ normal distribution
qqnorm(pdata.bay$Case) 
qqline(pdata.bay$Case)

qqp(pdata.bay$Case, "norm",xlab="",
    ylab="")

#Log Normal distribution
qqp(pdata.bay$Case, "lnorm",xlab="",
    ylab="")

#Gamma distribution
library(fitdistrplus)

gamma <- fitdistr(pdata.bay$Case[pdata.bay$Case!=0], "gamma")
qqp(pdata.bay$Case, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

nbinom <- fitdistr(pdata.bay$Case, "Negative Binomial")
qqp(pdata.bay$Case, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]], xlab="",
    ylab="")

poisson <- fitdistr(pdata.bay$Case, "Poisson")
qqp(pdata.bay$Case, "pois", poisson$estimate,xlab="",
    ylab="")

#Exponential 
params = fitdistr(pdata.bay$Case, "exponential")
simdata <- qexp(ppoints(length(pdata.bay$Case)), rate = params$estimate)
qqplot(pdata.bay$Case, simdata)
qqline(pdata.bay$Case, simdata)

#Distribution of the data
descdist(pdata.bay$Case,boot=3000)

#Poisson distribution
fit.y <- glm(Case ~ 1, family=poisson(),data=panel.data)
plot(fit.y)


########################################################################
# First Model: Estimating preliminary references submitted by the court
########################################################################
# Using multicore to compute parallel execution of R code on systems 
# @parallel package on Windows (base package in R)

#Example
library("parallel")

#Multicore: Example
square<-function(x){
  x^2
}
num_cores <- detectCores()
cl <- makeCluster(num_cores)
Values<-rnorm(100000)
system.time(
  result <- parLapply(cl, Values, square) )  # paralel execution, with time wrapper
stopCluster(cl)

#Formula: Predictors from classification tree
#f1<-Case~Lag.order1+NMS+peakcourt+pol.supp+Gdp.capita+yec+Veto.checks+Judicial.power+monism+Const+(1|Court:Country)
#f2<-Case~Lag.TO+NMS+peakcourt+pol.supp+Gdp.capita+yec+Veto.checks+Judicial.power+monism+Const++(1|Court:Country)
f3<-Case~Lag.order1*NMS+peakcourt+pol.supp+Gdp.capita+yec+Judicial.power+(1|Court:Country)
f4<-Case~Lag.TO*NMS+peakcourt+pol.supp+Gdp.capita+yec+Judicial.power+(1|Court:Country)
f5<-Case~Lag.order1*NMS+peakcourt+pol.supp+Gdp.capita+yec+Judicial.power+(1|Court:Country)
#f6<-Case~Lag.TO*NMS+peakcourt+pol.supp+Gdp.capita+yec+Veto.checks+Judicial.power+monism+Const+(1|Court:Country)

#Get information on all priors
#print(get_prior(f1,data=pdata.bay,family=zero_inflated_negbinomial()))
#print(get_prior(f2,data=pdata.bay,family=zero_inflated_negbinomial()))
print(get_prior(f3,data=pdata.bay,family=zero_inflated_negbinomial()))
print(get_prior(f4,data=pdata.bay,family=zero_inflated_negbinomial()))
print(get_prior(f5,data=pdata.first,family=zero_inflated_negbinomial()))
#print(get_prior(f6,data=pdata.first,family=zero_inflated_negbinomial()))

#Run multicore to reduce time 
library(rstan)
rstan_options(autowrite = T)
options(mc.cores = parallel::detectCores())


system.time(zero_negbin.prior<-brm(f1, 
                                   family=zero_inflated_negbinomial(link = "log"),
                                   data=pdata.bay,
                                   warmup=1000, iter=5000, chains=4,#cores=7,
                                   prior=c(set_prior("normal(0,1)", class = "b"),
                                           #set_prior("student_t(3,0,10)", class = "sd",group= "Court"),
                                           set_prior("student_t(3,0,10)", class = "sd",group= "Court:Country")),
                                   control=list(adapt_delta=0.99),
                                   init_r=0.1
))
summary(zero_negbin.prior)
on.exit(stopCluster(cl))

system.time(zero_negbin.prior.type<-brm(f2, 
                                   family=zero_inflated_negbinomial(link = "log"),
                                   data=pdata.bay,
                                   warmup=1000, iter=5000, chains=4,#cores=7,
                                   prior=c(set_prior("normal(0,1)", class = "b"),
                                           #set_prior("student_t(3,0,10)", class = "sd",group= "Court"),
                                           set_prior("student_t(3,0,10)", class = "sd",group= "Court:Country")),
                                   control=list(adapt_delta=0.99),
                                   init_r=0.1
))
summary(zero_negbin.prior.type)
on.exit(stopCluster(cl))

system.time(zero_negbin.prior.int<-brm(f3, 
                                        family=zero_inflated_negbinomial(link = "log"),
                                        data=pdata.bay,
                                        warmup=1000, iter=5000, chains=4,#cores=7,
                                        prior=c(set_prior("normal(0,1)", class = "b"),
                                                #set_prior("student_t(3,0,10)", class = "sd",group= "Court"),
                                                set_prior("student_t(3,0,10)", class = "sd",group= "Court:Country")),
                                        control=list(adapt_delta=0.99),
                                        init_r=0.1
))
summary(zero_negbin.prior.int)
on.exit(stopCluster(cl))

system.time(zero_negbin.prior.int.type<-brm(f4, 
                                        family=zero_inflated_negbinomial(link = "log"),
                                        data=pdata.bay,
                                        warmup=1000, iter=5000, chains=4,#cores=7,
                                        prior=c(set_prior("normal(0,1)", class = "b"),
                                                #set_prior("student_t(3,0,10)", class = "sd",group= "Court"),
                                                set_prior("student_t(3,0,10)", class = "sd",group= "Court:Country")),
                                        control=list(adapt_delta=0.99),
                                        init_r=0.1
))
summary(zero_negbin.prior.int.type)
on.exit(stopCluster(cl))

system.time(zero_negbin.prior.first<-brm(f5, 
                                            family=zero_inflated_negbinomial(link = "log"),
                                            data=pdata.first,
                                            warmup=1000, iter=5000, chains=4,#cores=7,
                                            prior=c(set_prior("normal(0,1)", class = "b"),
                                                    #set_prior("student_t(3,0,10)", class = "sd",group= "Court"),
                                                    set_prior("student_t(3,0,10)", class = "sd",group= "Court:Country")),
                                            control=list(adapt_delta=0.99),
                                            init_r=0.1
))
summary(zero_negbin.prior.first)
on.exit(stopCluster(cl))

system.time(zero_negbin.prior.first.type<-brm(f6, 
                                         family=zero_inflated_negbinomial(link = "log"),
                                         data=pdata.first,
                                         warmup=1000, iter=5000, chains=4,#cores=7,
                                         prior=c(set_prior("normal(0,1)", class = "b"),
                                                 #set_prior("student_t(3,0,10)", class = "sd",group= "Court"),
                                                 set_prior("student_t(3,0,10)", class = "sd",group= "Court:Country")),
                                         control=list(adapt_delta=0.99),
                                         init_r=0.1
))
summary(zero_negbin.prior.first.type)


WAIC(zero_negbin.prior.int)
WAIC(zero_negbin.prior.int.type)
WAIC(zero_negbin.prior.first)

res.chill1<-round(summary(zero_negbin.prior.int)$fixed[,c(1,3:4)],3)
res.chill2<-round(summary(zero_negbin.prior.int.type)$fixed[,c(1,3:4)],3)
res.chill3<-round(summary(zero_negbin.prior.first)$fixed[,c(1,3:4)],3)
result<-as.data.frame(cbind(res.chill1,res.chill2,res.chill3))
colnames(result)<-c("Post.mean","l-95% CI","u-95% CI","Post.mean","l-95% CI","u-95% CI",
                    "Post.mean","l-95% CI","u-95% CI")

stargazer(result,
          digits=3,
          summary=F)

####
# Extract figures: Posterior distribution
plot(zero_negbin.prior$fit,pars=c("b_Intercept","b_Lag.order1"))

#Intercept
pdf("M1_Intercept.dens.pdf")
p1<-stan_dens(zero_negbin.prior.int$fit,pars="b_Intercept",separate_chains = F)
print(p1)
dev.off()

pdf("M2_Intercept.dens.pdf")
p2<-stan_dens(zero_negbin.prior.int.type$fit,pars="b_Intercept",separate_chains = F)
print(p2)
dev.off()

pdf("M3_Intercept.dens.pdf")
p3<-stan_dens(zero_negbin.prior.first$fit,pars="b_Intercept",separate_chains = F)
print(p3)
dev.off()

#Lag (t-1)
pdf("M1_Lag1.dens.pdf")
p1<-stan_dens(zero_negbin.prior.int$fit,pars="b_Lag.order1",separate_chains = F)
print(p1)
dev.off()

pdf("M2_Lag1.dens.pdf")
p2<-stan_dens(zero_negbin.prior.int.type$fit,pars="b_Lag.TO",separate_chains = F)
print(p2)
dev.off()

pdf("M3_Lag1.dens.pdf")
p3<-stan_dens(zero_negbin.prior.first$fit,pars="b_Lag.order1",separate_chains = F)
print(p3)
dev.off()

######
#NMS
pdf("M1_NMS.dens.pdf")
p1<-stan_dens(zero_negbin.prior.int$fit,pars="b_NMS1",separate_chains = F)
print(p1)
dev.off()

pdf("M2_NMS.dens.pdf")
p2<-stan_dens(zero_negbin.prior.int.type$fit,pars="b_NMS1",separate_chains = F)
print(p2)
dev.off()

pdf("M3_NMS.dens.pdf")
p3<-stan_dens(zero_negbin.prior.first$fit,pars="b_NMS1",separate_chains = F)
print(p3)
dev.off()

#Peak court
pdf("M1_peakcourt.dens.pdf")
p1<-stan_dens(zero_negbin.prior.int$fit,pars="b_peakcourt1",separate_chains = F)
print(p1)
dev.off()

pdf("M2_peakcourt.dens.pdf")
p2<-stan_dens(zero_negbin.prior.int.type$fit,pars="b_peakcourt1",separate_chains = F)
print(p2)
dev.off()

pdf("M3_peakcourt.dens.pdf")
p3<-stan_dens(zero_negbin.prior.first$fit,pars="b_peakcourt1",separate_chains = F)
print(p3)
dev.off()

#####
#EU support
pdf("M1_pol.supp.dens.pdf")
p1<-stan_dens(zero_negbin.prior.int$fit,pars="b_Intercept",separate_chains = F)
print(p1)
dev.off()

pdf("M2_pol.supp.dens.pdf")
p2<-stan_dens(zero_negbin.prior.int.type$fit,pars="b_pol.supp",separate_chains = F)
print(p2)
dev.off()

pdf("M3_pol.supp.dens.pdf")
p3<-stan_dens(zero_negbin.prior.first$fit,pars="b_pol.supp",separate_chains = F)
print(p3)
dev.off()

#Gdp.capita
pdf("M1_Gdp.capita.dens.pdf")
p1<-stan_dens(zero_negbin.prior.int$fit,pars="b_Gdp.capita",separate_chains = F)
print(p1)
dev.off()

pdf("M2_Gdp.capita.dens.pdf")
p2<-stan_dens(zero_negbin.prior.int.type$fit,pars="b_Gdp.capita",separate_chains = F)
print(p2)
dev.off()

pdf("M3_Gdp.capita.dens.pdf")
p3<-stan_dens(zero_negbin.prior.first$fit,pars="b_Gdp.capita",separate_chains = F)
print(p3)
dev.off()

#Yec
pdf("M1_yec.dens.pdf")
p1<-stan_dens(zero_negbin.prior.int$fit,pars="b_yec",separate_chains = F)
print(p1)
dev.off()

pdf("M2_yec.dens.pdf")
p2<-stan_dens(zero_negbin.prior.int.type$fit,pars="b_yec",separate_chains = F)
print(p2)
dev.off()

pdf("M3_yec.dens.pdf")
p3<-stan_dens(zero_negbin.prior.first$fit,pars="b_yec",separate_chains = F)
print(p3)
dev.off()

#Judicial.power
pdf("M1_Judicial.power.dens.pdf")
p1<-stan_dens(zero_negbin.prior.int$fit,pars="b_Judicial.power",separate_chains = F)
print(p1)
dev.off()

pdf("M2_Judicial.power.dens.pdf")
p2<-stan_dens(zero_negbin.prior.int.type$fit,pars="b_Judicial.power",separate_chains = F)
print(p2)
dev.off()

pdf("M3_Judicial.power.dens.pdf")
p3<-stan_dens(zero_negbin.prior.first$fit,pars="b_Judicial.power",separate_chains = F)
print(p3)
dev.off()

#Lag.order1:NMS1
pdf("M1_Lag.order1_NMS1.dens.pdf")
p1<-stan_dens(zero_negbin.prior.int$fit,pars="b_Lag.order1:NMS1",separate_chains = F)
print(p1)
dev.off()

pdf("M2_Lag.TO_NMS1.dens.pdf")
p2<-stan_dens(zero_negbin.prior.int.type$fit,pars="b_Lag.TO:NMS1",separate_chains = F)
print(p2)
dev.off()

pdf("M3_Lag.order1_NMS1.dens.pdf")
p3<-stan_dens(zero_negbin.prior.first$fit,pars="b_Lag.order1:NMS1",separate_chains = F)
print(p3)
dev.off()

#sd_Court:Country_Intercept
pdf("sd_Court_Country_Intercept.dens.pdf")
p1<-stan_dens(zero_negbin.prior.int$fit,pars="sd_Court:Country__Intercept",separate_chains = F)
print(p1)
dev.off()

pdf("M2_sd_Court_Country_Intercept.dens.pdf")
p2<-stan_dens(zero_negbin.prior.int.type$fit,pars="sd_Court:Country__Intercept",separate_chains = F)
print(p2)
dev.off()

pdf("M3_sd_Court_Country_Intercept.dens.pdf")
p3<-stan_dens(zero_negbin.prior.first$fit,pars="sd_Court:Country__Intercept",separate_chains = F)
print(p3)
dev.off()

#####
#shape
pdf("M1_shape.dens.pdf")
p1<-stan_dens(zero_negbin.prior.int$fit,pars="shape",separate_chains = F)
print(p1)
dev.off()

pdf("M2_shape.dens.pdf")
p2<-stan_dens(zero_negbin.prior.int.type$fit,pars="shape",separate_chains = F)
print(p2)
dev.off()

pdf("M3_shape.dens.pdf")
p3<-stan_dens(zero_negbin.prior.first$fit,pars="shape",separate_chains = F)
print(p3)
dev.off()

#####
#zi
pdf("M1_zi.dens.pdf")
p1<-stan_dens(zero_negbin.prior.int$fit,pars="zi",separate_chains = F)
print(p1)
dev.off()

pdf("M2_zi.dens.pdf")
p2<-stan_dens(zero_negbin.prior.int.type$fit,pars="zi",separate_chains = F)
print(p2)
dev.off()

pdf("M3_zi.dens.pdf")
p3<-stan_dens(zero_negbin.prior.first$fit,pars="zi",separate_chains = F)
print(p3)
dev.off()

######################################
####
# Extract figures: Trace Plot

#Intercept
png("M1_Intercept.trace.png")
p1<-stan_trace(zero_negbin.prior.int$fit,pars="b_Intercept")
p1<-p1+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
  
print(p1)
dev.off()

png("M2_Intercept.trace.png")
p2<-stan_trace(zero_negbin.prior.int.type$fit,pars="b_Intercept")
p2<-p2+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p2)
dev.off()

png("M3_Intercept.trace.png")
p3<-stan_trace(zero_negbin.prior.first$fit,pars="b_Intercept")
p3<-p3+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p3)
dev.off()

#Lag (t-1)
png("M1_Lag1.trace.png")
p1<-stan_trace(zero_negbin.prior.int$fit,pars="b_Lag.order1")
p1<-p1+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p1)
dev.off()

png("M2_Lag1.trace.png")
p2<-stan_trace(zero_negbin.prior.int.type$fit,pars="b_Lag.TO")
p2<-p2+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p2)
dev.off()

png("M3_Lag1.trace.png")
p3<-stan_trace(zero_negbin.prior.first$fit,pars="b_Lag.order1")
p3<-p3+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p3)
dev.off()

######
#NMS
png("M1_NMS.trace.png")
p1<-stan_trace(zero_negbin.prior.int$fit,pars="b_NMS1")
p1<-p1+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p1)
dev.off()

png("M2_NMS.trace.png")
p2<-stan_trace(zero_negbin.prior.int.type$fit,pars="b_NMS1")
p2<-p2+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p2)
dev.off()

png("M3_NMS.trace.png")
p3<-stan_trace(zero_negbin.prior.first$fit,pars="b_NMS1")
p3<-p3+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p3)
dev.off()

#Peak court
png("M1_peakcourt.trace.png")
p1<-stan_trace(zero_negbin.prior.int$fit,pars="b_peakcourt1")
p1<-p1+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p1)
dev.off()

png("M2_peakcourt.trace.png")
p2<-stan_trace(zero_negbin.prior.int.type$fit,pars="b_peakcourt1")
p2<-p2+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p2)
dev.off()

png("M3_peakcourt.trace.png")
p3<-stan_trace(zero_negbin.prior.first$fit,pars="b_peakcourt1")
p3<-p3+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p3)
dev.off()

#####
#EU support
png("M1_pol.supp.trace.png")
p1<-stan_trace(zero_negbin.prior.int$fit,pars="b_Intercept")
p1<-p1+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p1)
dev.off()

png("M2_pol.supp.trace.png")
p2<-stan_trace(zero_negbin.prior.int.type$fit,pars="b_pol.supp")
p2<-p2+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p2)
dev.off()

png("M3_pol.supp.trace.png")
p3<-stan_trace(zero_negbin.prior.first$fit,pars="b_pol.supp")
p3<-p3+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p3)
dev.off()

#Gdp.capita
png("M1_Gdp.capita.trace.png")
p1<-stan_trace(zero_negbin.prior.int$fit,pars="b_Gdp.capita")
p1<-p1+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p1)
dev.off()

png("M2_Gdp.capita.trace.png")
p2<-stan_trace(zero_negbin.prior.int.type$fit,pars="b_Gdp.capita")
p2<-p2+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p2)
dev.off()

png("M3_Gdp.capita.trace.png")
p3<-stan_trace(zero_negbin.prior.first$fit,pars="b_Gdp.capita")
p3<-p3+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p3)
dev.off()

#Yec
png("M1_yec.trace.png")
p1<-stan_trace(zero_negbin.prior.int$fit,pars="b_yec")
p1<-p1+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p1)
dev.off()

png("M2_yec.trace.png")
p2<-stan_trace(zero_negbin.prior.int.type$fit,pars="b_yec")
p2<-p2+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p2)
dev.off()

png("M3_yec.trace.png")
p3<-stan_trace(zero_negbin.prior.first$fit,pars="b_yec")
p3<-p3+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p3)
dev.off()

#Judicial.power
png("M1_Judicial.power.trace.png")
p1<-stan_trace(zero_negbin.prior.int$fit,pars="b_Judicial.power")
p1<-p1+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p1)
dev.off()

png("M2_Judicial.power.trace.png")
p2<-stan_trace(zero_negbin.prior.int.type$fit,pars="b_Judicial.power")
p2<-p2+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p2)
dev.off()

png("M3_Judicial.power.trace.png")
p3<-stan_trace(zero_negbin.prior.first$fit,pars="b_Judicial.power")
p3<-p3+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p3)
dev.off()

#Lag.order1:NMS1
png("M1_Lag.order1_NMS1.trace.png")
p1<-stan_trace(zero_negbin.prior.int$fit,pars="b_Lag.order1:NMS1")
p1<-p1+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p1)
dev.off()

png("M2_Lag.TO_NMS1.trace.png")
p2<-stan_trace(zero_negbin.prior.int.type$fit,pars="b_Lag.TO:NMS1")
p2<-p2+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p2)
dev.off()

png("M3_Lag.order1_NMS1.trace.png")
p3<-stan_trace(zero_negbin.prior.first$fit,pars="b_Lag.order1:NMS1")
p3<-p3+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p3)
dev.off()

#sd_Court:Country_Intercept
png("M1_sd_Court_Country_Intercept.trace.png")
p1<-stan_trace(zero_negbin.prior.int$fit,pars="sd_Court:Country__Intercept")
p1<-p1+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p1)
dev.off()

png("M2_sd_Court_Country_Intercept.trace.png")
p2<-stan_trace(zero_negbin.prior.int.type$fit,pars="sd_Court:Country__Intercept")
p2<-p2+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p2)
dev.off()

png("M3_sd_Court_Country_Intercept.trace.png")
p3<-stan_trace(zero_negbin.prior.first$fit,pars="sd_Court:Country__Intercept")
p3<-p3+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p3)
dev.off()

#####
#shape
png("M1_shape.trace.png")
p1<-stan_trace(zero_negbin.prior.int$fit,pars="shape")
p1<-p1+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p1)
dev.off()

png("M2_shape.trace.png")
p2<-stan_trace(zero_negbin.prior.int.type$fit,pars="shape")
p2<-p2+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p2)
dev.off()

png("M3_shape.trace.png")
p3<-stan_trace(zero_negbin.prior.first$fit,pars="shape")
p3<-p3+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p3)
dev.off()

#####
#zi
png("M1_zi.trace.png")
p1<-stan_trace(zero_negbin.prior.int$fit,pars="zi")
p1<-p1+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p1)
dev.off()

png("M2_zi.trace.png")
p2<-stan_trace(zero_negbin.prior.int.type$fit,pars="zi")
p2<-p2+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p2)
dev.off()

png("M3_zi.trace.png")
p3<-stan_trace(zero_negbin.prior.first$fit,pars="zi")
p3<-p3+theme(legend.position = "none",axis.text=element_text(size=18),axis.title.y=element_blank())+
  scale_color_manual(values = c("black", "black", "black", "black"))
print(p3)
dev.off()

#################################
#####
# Autocorrelation plot
#Intercept
png("M1_Intercept.ac.png")
p1<-stan_ac(zero_negbin.prior.int$fit,pars="b_Intercept")
p1<-p1+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p1)
dev.off()


png("M2_Intercept.ac.png")
p2<-stan_ac(zero_negbin.prior.int.type$fit,pars="b_Intercept")
p2<-p2+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p2)
dev.off()

png("M3_Intercept.ac.png")
p3<-stan_ac(zero_negbin.prior.first$fit,pars="b_Intercept")
p3<-p3+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p3)
dev.off()

#Lag (t-1)
png("M1_Lag1.ac.png")
p1<-stan_ac(zero_negbin.prior.int$fit,pars="b_Lag.order1")
p1<-p1+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p1)
dev.off()

png("M2_Lag1.ac.png")
p2<-stan_ac(zero_negbin.prior.int.type$fit,pars="b_Lag.TO")
p2<-p2+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p2)
dev.off()

png("M3_Lag1.ac.png")
p3<-stan_ac(zero_negbin.prior.first$fit,pars="b_Lag.order1")
p3<-p3+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p3)
dev.off()

######
#NMS
png("M1_NMS.ac.png")
p1<-stan_ac(zero_negbin.prior.int$fit,pars="b_NMS1")
p1<-p1+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p1)
dev.off()

png("M2_NMS.ac.png")
p2<-stan_ac(zero_negbin.prior.int.type$fit,pars="b_NMS1")
p2<-p2+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p2)
dev.off()

png("M3_NMS.ac.png")
p3<-stan_ac(zero_negbin.prior.first$fit,pars="b_NMS1")
p3<-p3+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p3)
dev.off()

#Peak court
png("M1_peakcourt.ac.png")
p1<-stan_ac(zero_negbin.prior.int$fit,pars="b_peakcourt1")
p1<-p1+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p1)
dev.off()

png("M2_peakcourt.ac.png")
p2<-stan_ac(zero_negbin.prior.int.type$fit,pars="b_peakcourt1")
p2<-p2+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p2)
dev.off()

png("M3_peakcourt.ac.png")
p3<-stan_ac(zero_negbin.prior.first$fit,pars="b_peakcourt1")
p3<-p3+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p3)
dev.off()

#####
#EU support
png("M1_pol.supp.ac.png")
p1<-stan_ac(zero_negbin.prior.int$fit,pars="b_Intercept")
p1<-p1+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p1)
dev.off()

png("M2_pol.supp.ac.png")
p2<-stan_ac(zero_negbin.prior.int.type$fit,pars="b_pol.supp")
p2<-p2+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p2)
dev.off()

png("M3_pol.supp.ac.png")
p3<-stan_ac(zero_negbin.prior.first$fit,pars="b_pol.supp")
p3<-p3+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p3)
dev.off()

#Gdp.capita
png("M1_Gdp.capita.ac.png")
p1<-stan_ac(zero_negbin.prior.int$fit,pars="b_Gdp.capita")
p1<-p1+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p1)
dev.off()

png("M2_Gdp.capita.ac.png")
p2<-stan_ac(zero_negbin.prior.int.type$fit,pars="b_Gdp.capita")
p2<-p2+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
dev.off()

png("M3_Gdp.capita.ac.png")
p3<-stan_ac(zero_negbin.prior.first$fit,pars="b_Gdp.capita")
p3<-p3+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p3)
dev.off()

#Yec
png("M1_yec.ac.png")
p1<-stan_ac(zero_negbin.prior.int$fit,pars="b_yec")
p1<-p1+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p1)
dev.off()

png("M2_yec.ac.png")
p2<-stan_ac(zero_negbin.prior.int.type$fit,pars="b_yec")
p2<-p2+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p2)
dev.off()

png("M3_yec.ac.png")
p3<-stan_ac(zero_negbin.prior.first$fit,pars="b_yec")
p3<-p3+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p3)
dev.off()

#Judicial.power
png("M1_Judicial.power.ac.png")
p1<-stan_ac(zero_negbin.prior.int$fit,pars="b_Judicial.power")
p1<-p1+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p1)
dev.off()

png("M2_Judicial.power.ac.png")
p2<-stan_ac(zero_negbin.prior.int.type$fit,pars="b_Judicial.power")
p2<-p2+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p2)
dev.off()

png("M3_Judicial.power.ac.png")
p3<-stan_ac(zero_negbin.prior.first$fit,pars="b_Judicial.power")
p3<-p3+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p3)
dev.off()

#Lag.order1:NMS1
png("M1_Lag.order1_NMS1.ac.png")
p1<-stan_ac(zero_negbin.prior.int$fit,pars="b_Lag.order1:NMS1")
p1<-p1+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p1)
dev.off()

png("M2_Lag.TO_NMS1.ac.png")
p2<-stan_ac(zero_negbin.prior.int.type$fit,pars="b_Lag.TO:NMS1")
p2<-p2+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p2)
dev.off()

png("M3_Lag.order1_NMS1.ac.png")
p3<-stan_ac(zero_negbin.prior.first$fit,pars="b_Lag.order1:NMS1")
p3<-p3+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p3)
dev.off()

#sd_Court:Country_Intercept
png("M1_sd_Court_Country_Intercept.ac.png")
p1<-stan_ac(zero_negbin.prior.int$fit,pars="sd_Court:Country__Intercept")
p1<-p1+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p1)
dev.off()

png("M2_sd_Court_Country_Intercept.ac.png")
p2<-stan_ac(zero_negbin.prior.int.type$fit,pars="sd_Court:Country__Intercept")
p2<-p2+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p2)
dev.off()

png("M3_sd_Court_Country_Intercept.ac.png")
p3<-stan_ac(zero_negbin.prior.first$fit,pars="sd_Court:Country__Intercept")
p3<-p3+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p3)
dev.off()

#####
#shape
png("M1_shape.ac.png")
p1<-stan_ac(zero_negbin.prior.int$fit,pars="shape")
p1<-p1+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p1)
dev.off()

png("M2_shape.ac.png")
p2<-stan_ac(zero_negbin.prior.int.type$fit,pars="shape")
p2<-p2+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p2)
dev.off()

png("M3_shape.ac.png")
p3<-stan_ac(zero_negbin.prior.first$fit,pars="shape")
p3<-p3+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p3)
dev.off()

#####
#zi
png("M1_zi.ac.png")
p1<-stan_ac(zero_negbin.prior.int$fit,pars="zi")
p1<-p1+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p1)
dev.off()

png("M2_zi.ac.png")
p2<-stan_ac(zero_negbin.prior.int.type$fit,pars="zi")
p2<-p2+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p2)
dev.off()

png("M3_zi.ac.png")
p3<-stan_ac(zero_negbin.prior.first$fit,pars="zi")
p3<-p3+theme(axis.title.x=element_blank(),axis.text=element_text(size=18),axis.title.y=element_blank())
print(p3)
dev.off()



####################

#R2glmm marginal
R2m_model1<-function(x){
  Design.matrix<-as.mcmc(x,combine_chains=4)
  
  #Fixed effects
  Fixed <- fixef(x)[2]*Design.matrix[,2]+ fixef(x)[3]*Design.matrix[,3]+ fixef(x)[4]*Design.matrix[,4]+
    fixef(x)[5]*Design.matrix[,5]+ fixef(x)[6]*Design.matrix[,6]+ fixef(x)[7]*Design.matrix[,7]+
    fixef(x)[8]*Design.matrix[,8]+ fixef(x)[9]*Design.matrix[,9]
  
  varF<-var(Fixed)
  
  R2m<-varF/(varF+Design.matrix[,10]+VarCorr(x)$'Court:Country'$sd[1])
  
  mean(R2m)
}


#####
#R2glmm conditional
R2c_model1<-function(x){
  Design.matrix<-as.mcmc(x,combine_chains=4)
  
  #Fixed effects
  Fixed <- fixef(x)[2]*Design.matrix[,2]+ fixef(x)[3]*Design.matrix[,3]+ fixef(x)[4]*Design.matrix[,4]+
    fixef(x)[5]*Design.matrix[,5]+ fixef(x)[6]*Design.matrix[,6]+ fixef(x)[7]*Design.matrix[,7]+
    fixef(x)[8]*Design.matrix[,8]+ fixef(x)[9]*Design.matrix[,9]
  
  
  varF<-var(Fixed)
  
  R2c<-(varF+Design.matrix[,10])/(varF+Design.matrix[,10]+VarCorr(x)$'Court:Country'$sd[1])
  
  mean(R2c)
}


####
#Print both Rglmm 
R2m_model1(zero_negbin.prior.int)
R2m_model1(zero_negbin.prior.int.type)
R2m_model1(zero_negbin.prior.first)

R2c_model1(zero_negbin.prior.int)
R2c_model1(zero_negbin.prior.int.type)
R2c_model1(zero_negbin.prior.first)


####
#Heidelberg Autocorrelation
heidel.diag(zero_negbin.prior)
heidel.diag(zero_negbin.prior.draft)

####
# Coefplot 

coefplot2(summary(zero_negbin.prior.int)$fixed[,1],
          sds=summary(zero_negbin.prior.int)$fixed[,2], 
          #lower1=summary(zero_negbin.prior.int)$fixed[,3],
          #upper1=summary(zero_negbin.prior.int)$fixed[,4],
          vertical=T, varnames=rownames(summary(zero_negbin.prior.int)$fixed), main="")

coefplot2(summary(zero_negbin.prior.int.type)$fixed[,1],
          sds=summary(zero_negbin.prior.int.type)$fixed[,2], 
          #lower1=summary(zero_negbin.prior.int.type)$fixed[,3],
          #upper1=summary(zero_negbin.prior.int.type)$fixed[,4],
          vertical=T, varnames=rownames(summary(zero_negbin.prior.int.type)$fixed), main="",add=T,col.pts="red")

par(oma=c(0, 0, 0, 0))
coefplot(summary(zero_negbin.prior.int)$fixed[,1],
          sds=summary(zero_negbin.prior.int)$fixed[,2], 
          lower.conf.bounds=summary(zero_negbin.prior.int)$fixed[,3],
          upper.conf.bounds=summary(zero_negbin.prior.int)$fixed[,4],
          vertical=T, varnames=rownames(summary(zero_negbin.prior.int)$fixed), main="",col.pts="black",pch.pts=16)


coefplot(summary(zero_negbin.prior.int.type)$fixed[,1],
          sds=summary(zero_negbin.prior.int.type)$fixed[,2], 
          lower1=summary(zero_negbin.prior.int.type)$fixed[,3],
          upper1=summary(zero_negbin.prior.int.type)$fixed[,4],
          vertical=T, varnames=rownames(summary(zero_negbin.prior.int.type)$fixed), main="",add=T,col.pts="red",offset=0.25,pch.pts=16)

coefplot(summary(zero_negbin.prior.first)$fixed[,1],
         sds=summary(zero_negbin.prior.first)$fixed[,2], 
         lower1=summary(zero_negbin.prior.first)$fixed[,3],
         upper1=summary(zero_negbin.prior.first)$fixed[,4],
         vertical=T, varnames=rownames(summary(zero_negbin.prior.first)$fixed), main="",add=T,offset=0.45,col.pts="blue",pch.pts=16)
legend("topright",c("H1","H2","H3"),inset=c(-0.2,0),col=c("black","red","blue"),pch=c(16,16,16),lty=c(1,1,1))


model1Frame <- data.frame(Variable = c("Intercept","Lag (t-1)", "New Member States", "Peak court", 
                                       "EU support", "GDP per capita", "Membership","Judicial power index",
                                       "Interaction"),
                          Coefficient = summary(zero_negbin.prior.int)$fixed[,1],
                          SE = summary(zero_negbin.prior.int)$fixed[,2],
                          Model = "Test1")
model2Frame <- data.frame(Variable = c("Intercept","Lag (t-1)", "New Member States", "Peak court", 
                                       "EU support", "GDP per capita", "Membership","Judicial power index",
                                       "Interaction"),
                          Coefficient = summary(zero_negbin.prior.int.type)$fixed[,1],
                          SE = summary(zero_negbin.prior.int.type)$fixed[,2],
                          Model = "Test2")
model3Frame <- data.frame(Variable =c("Intercept","Lag (t-1)", "New Member States", "Peak court", 
                                      "EU support", "GDP per capita", "Membership","Judicial power index",
                                      "Interaction"),
                          Coefficient = summary(zero_negbin.prior.first)$fixed[,1],
                          SE = summary(zero_negbin.prior.first)$fixed[,2],
                          Model = "Test3")

# Combine these data.frames
allModelFrame <- data.frame(rbind(model1Frame, model2Frame,model3Frame))  # etc.

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
pdf("plot.chilling.new.pdf")
zp1 <- ggplot(allModelFrame, aes(colour = Model,linetype=Model,shape=Model))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
#zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
#                                ymax = Coefficient + SE*interval1),
#                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = -1/2), # Negative width changes the order of factors
                              fill = "WHITE")
zp1 <- zp1 + coord_flip()+theme_bw()+theme(axis.title.x=element_blank(),
                                           axis.title.y=element_blank(),
                                           axis.text=element_text(size=12))
#zp1 <- zp1 + ggtitle("Original vs. Imputed coefficient estimates")
print(zp1) # The trick to these is position_dodge().
dev.off()



##############


#R2glmm marginal
R2m_model1<-function(x){
  Design.matrix<-as.mcmc(x,combine_chains=4)
  
  #Fixed effects
  Fixed <- fixef(x)[2]*Design.matrix[,2]+ fixef(x)[3]*Design.matrix[,3]+ fixef(x)[4]*Design.matrix[,4]+
    fixef(x)[5]*Design.matrix[,5]+ fixef(x)[6]*Design.matrix[,6]+ fixef(x)[7]*Design.matrix[,7]+
    fixef(x)[8]*Design.matrix[,8]+ fixef(x)[9]*Design.matrix[,9]+fixef(x)[10]*Design.matrix[,10]+
    fixef(x)[11]*Design.matrix[,11]+fixef(x)[12]*Design.matrix[,12]
  
  varF<-var(Fixed)
  
  R2m<-varF/(varF+Design.matrix[,13]+VarCorr(x)$'Court:Country'$sd[1])
  
  mean(R2m)
}



#####
#R2glmm conditional
R2c_model1<-function(x){
  Design.matrix<-as.mcmc(x,combine_chains=4)
  
  #Fixed effects
  Fixed <- fixef(x)[2]*Design.matrix[,2]+ fixef(x)[3]*Design.matrix[,3]+ fixef(x)[4]*Design.matrix[,4]+
    fixef(x)[5]*Design.matrix[,5]+ fixef(x)[6]*Design.matrix[,6]+ fixef(x)[7]*Design.matrix[,7]+
    fixef(x)[8]*Design.matrix[,8]+ fixef(x)[9]*Design.matrix[,9]+fixef(x)[10]*Design.matrix[,10]+
    fixef(x)[11]*Design.matrix[,11]+fixef(x)[12]*Design.matrix[,12]
  
  
  varF<-var(Fixed)
  
  R2c<-(varF+Design.matrix[,13])/(varF+Design.matrix[,13]+VarCorr(x)$'Court:Country'$sd[1])
  
  mean(R2c)
}

####
#Print both Rglmm 
R2m_model1(zero_negbin.prior)
R2c_model1(zero_negbin.prior)

####
#Heidelberg Autocorrelation
heidel.diag(zero_negbin.prior)

launch_shiny(zero_negbin.prior)

########
#Zero-Inflated Poisson

system.time(zero_poisson.prior<-brm(f1, 
                                   family=zero_inflated_poisson(link = "log"),
                                   data=pdata.bay,
                                   warmup=1000, iter=4000, chains=4,#cores=7,
                                   prior=c(set_prior("normal(0,1)", class = "b"),
                                           #set_prior("student_t(3,0,10)", class = "sd",group= "Court"),
                                           set_prior("student_t(3,0,10)", class = "sd",group= "Court:Country")),
                                   control=list(adapt_delta=0.99),
                                   init_r=0.1
))
summary(zero_poisson.prior)
on.exit(stopCluster(cl))
WAIC(zero_poisson.prior)

#####################################
#Testing the poor drafting hypothesis

#####################################
#Learning effect: Judgment

f2<-Decision.1~Lag.ref1+Lag.ref2+Lag.ref3+#(1|Court)
  +(1|Court:Country)

print(get_prior(f2,data=pdata.class,family=zero_inflated_negbinomial()))

rstan_options(autowrite = T)
options(mc.cores = parallel::detectCores())


system.time(learning.prior<-brm(f2, 
                                family=zero_inflated_negbinomial(link = "log"),
                                data=pdata.class,
                                warmup=1000, iter=5000, chains=4,#cores=7,
                                prior=c(set_prior("normal(0,1)", class = "b"),
                                        #set_prior("student_t(3,0,10)", class = "sd",group= "Court"),
                                        set_prior("student_t(3,0,10)", class = "sd",group= "Court:Country")),
                                control=list(adapt_delta=0.99),
                                init_r=0.1
))
summary(learning.prior)



############################


########################################################################
# Lag Distribution Model: Estimating the decision of ECJ (Logit/Probit)
########################################################################

#####
#Create outcome variable
pdata.learn$Order<-NA

for (i in 1:nrow(pdata.learn)){
  if(pdata.learn$Decision.2[i]>0){
    pdata.learn$Order[i]<-1}
  else{pdata.learn$Order[i]<-0}
}
  
pdata.learn$NMS<-NA
New_member<-c(1,4,14,16:28)

for (i in 1:nrow(pdata.learn)){  
if(as.numeric(paste(pdata.learn$Country[i])) %in% New_member){
    pdata.learn$NMS[i]<-1}
  else {pdata.learn$NMS[i]<-0}
}

#Convert to numerci to Lag the variable
pdata.learn$TO<-as.numeric(paste(pdata.learn$TO))

####
#Lag (t-1)
pdata.learn<-ddply(pdata.learn, ~Court, transform, Lag.TO = lag1(TO))

pdata.learn<-na.omit(pdata.learn)

#####
# 1961-2014


####
# Lag specific effects total sample
Lag.learn<-aggregate(Decision.2 ~ Lag, data = pdata.learn, mean)
#Lag.chilling<-aggregate(Decision.2~Lag,pdata.chilling,mean)

#Plot 
scatter.smooth(Lag.learn$Lag,Lag.learn$Decision.2,col="red",xlab="Lag",ylab="Average Number of Decisions Ending in Order",
               cex=1,cex.lab=1.3,pch=16, lpars =list(lwd = 1))
scatter.smooth(Lag.chilling$Lag,Lag.chilling$Decision.2,col="blue",xlab="Lag",ylab="Average Number of Decisions Ending in Order",
               cex=1,cex.lab=1.3,pch=16, lpars =list(lwd = 1))


#Plot with confidence interval
ggplot(Lag.learn, aes(Lag,Decision.2)) + geom_point() + geom_smooth()+ coord_cartesian(ylim = c(0, 0.04)) 
ggplot(Lag.chilling, aes(Lag,Decision.2)) + geom_point() + geom_smooth()


#Without Italy (Mamma mia)
mamma.mia<-pdata[!(pdata$Country %in% 9), ]

Lag.trend.mia<-aggregate(Decision.2 ~ Lag, data = mamma.mia, mean)

scatter.smooth(Lag.trend.mia$Lag,Lag.trend.mia$Decision.2,col="blue",xlab="Lag",ylab="Average Number of Decisions Ending in Order",
               cex=1,cex.lab=1.3,pch=16, lpars =list(lwd = 1))

#Plot with confidence interval
ggplot(Lag.trend.mia, aes(Lag,Decision.2)) + geom_point() + geom_smooth()


####
# Lag specific effects Old Member States
new.ms<-pdata.learn[which(as.numeric(paste(pdata.learn$Country)) %in% New_member),]
old.ms<-pdata.learn[which(!as.numeric(paste(pdata.learn$Country)) %in% New_member),]

Lag.trend.old<-aggregate(Decision.2 ~ Lag, data = old.ms, mean)
#Lag.case.old<-aggregate(Case ~ Lag, data = old.ms, mean)

scatter.smooth(Lag.trend.old$Lag,Lag.trend.old$Decision.2,col="red",xlab="Lag",main="Old Member States >=1995",
               ylab="Average Number of Decisions Ending in Order",cex=1,cex.lab=1.3,pch=16, lpars =list(lwd = 1))
#scatter.smooth(Lag.case.old$Lag,Lag.case.old$Case,col="red",xlab="Lag",main="Old Member States >=1995",
               ylab="Average Number of Decisions Ending in Order",cex=1,cex.lab=1.3,pch=16, lpars =list(lwd = 1))


#Plot with confidence interval
ggplot(Lag.trend.old, aes(Lag,Decision.2)) + geom_point() + geom_smooth()
#ggplot(Lag.case.old, aes(Lag,Case)) + geom_point() + geom_smooth()


Lag.trend.new<-aggregate(Decision.2 ~ Lag, data = new.ms, mean)
#Lag.case.new<-aggregate(Case~Lag,data=new.ms,mean)

#Loess polot
scatter.smooth(Lag.trend.new$Lag,Lag.trend.new$Decision.2,col="blue",xlab="Lag",main="New Member States >=1995",
               ylab="Average Number of Decisions Ending in Order",cex=1,cex.lab=1.3,pch=16, lpars =list(lwd = 1))
#scatter.smooth(Lag.case.new$Lag,Lag.case.new$Case,col="blue",xlab="Lag",main="New Member States >=1995",
               ylab="Average Number of Decisions Ending in Order",cex=1,cex.lab=1.3,pch=16, lpars =list(lwd = 1))


#Plot with confidence interval
ggplot(Lag.trend.new, aes(Lag,Decision.2)) + geom_point() + geom_smooth()
#ggplot(Lag.case.new, aes(Lag,Case)) + geom_point() + geom_smooth()


#################################
#Distributed Lag Non-Linear model
#################################
library(dlnm)
library(lme4)
library(lmerTest)
library(piecewiseSEM)
library(MuMIn)

#Restrict to lag10
pdata.<-pdata.learn[which(pdata.learn$Lag<11),]
pdata.learn<-na.omit(pdata.learn)
str(pdata.learn)
pdata.learn$Lag.order1<-as.numeric(paste(pdata.learn$Lag.order1))

####
#First step: Basis matrix Z - non-linear effects of x
poly<-poly(pdata.learn$Lag.order1,degree=3)
plot(poly)
b<-onebasis(pdata.learn$Lag.order1, "lin")
summary(b)

####
#Cross basis TOTAL sample: Relationship in two dimensions predictor and lags
cb1.order.lin<-crossbasis(pdata.learn$Decision.2,lag=52,argvar=list(fun="lin"),arglag=list(fun="lin"))
summary(cb1.order.lin)

cb1.order.pol<-crossbasis(pdata.learn$Decision.2,lag=52,argvar=list(fun="lin"),arglag=list(fun="poly",degree=3))
summary(cb1.order.pol)

####
#Cross basis OMS: Relationship in two dimensions predictor and lags
cb1.old.lin<-crossbasis(old.ms$Decision.2,lag=52,argvar=list(fun="lin"),arglag=list(fun="lin"))
summary(cb1.old.lin)

cb1.old.pol<-crossbasis(old.ms$Decision.2,lag=52,argvar=list(fun="lin"),arglag=list(fun="poly",degree=2))
summary(cb1.old.pol)

####
#Cross basis NMS: Relationship in two dimensions predictor and lags
cb1.new.lin<-crossbasis(new.ms$Decision.2,lag=13,argvar=list(fun="lin"),arglag=list(fun="lin"))
summary(cb1.new.lin)

cb1.new.pol<-crossbasis(new.ms$Decision.2,lag=13,argvar=list(fun="lin"),arglag=list(fun="poly",degree=2))
summary(cb1.new.pol)

####
#Glmer Model: Quality hypothesis
model.q1<-glmer(Order~cb1.order.lin+Pend+NMS+peakcourt+yec+(1|Court/Country),
                family=binomial(link="logit"),data=pdata.learn)
summary(model.q1)

model.q2<-glmer(Order~cb1.order.pol+Pend+NMS+peakcourt+yec+(1|Court/Country),
                family=binomial(link="logit"),data=pdata.learn)
summary(model.q2)

model1<-glmer(Order~cb1.order.lin+Lag.TO+NMS+peakcourt+Nb.courts+Judicial.power+monism+Veto.checks+yec+Gdp.capita+(1|Court/Country),family=binomial(link="logit"),data=pdata.learn)
summary(model1)

model2<-glmer(Order~cb1.order.pol+NMS+peakcourt+Nb.courts+Judicial.power+monism+Veto.checks+yec+Gdp.capita+(1|Court/Country),family=binomial(link="logit"),data=pdata.learn)
summary(model2)

####
#Glmer Model: Total Sample
model1<-glmer(Order~cb1.order.lin+Lag.TO+NMS+peakcourt+Nb.courts+Judicial.power+monism+Veto.checks+yec+Gdp.capita+(1|Court/Country),family=binomial(link="logit"),data=pdata.learn)
summary(model1)

model2<-glmer(Order~cb1.order.pol+NMS+peakcourt+Nb.courts+Judicial.power+monism+Veto.checks+yec+Gdp.capita+(1|Court/Country),family=binomial(link="logit"),data=pdata.learn)
summary(model2)


#####
#Glmer Model: Old MS
model3<-glmer(Order~cb1.old.lin+peakcourt+Nb.courts+Judicial.power+monism+Veto.checks+yec+Gdp.capita+(1|Court/Country),family=binomial(link="logit"),data=old.ms)
summary(model3)

model4<-glmer(Order~cb1.old.pol+peakcourt+Nb.courts+Judicial.power+monism+Veto.checks+yec+Gdp.capita+(1|Court/Country),family=binomial(link="logit"),data=old.ms)
summary(model4)

#####
#Glmer Model: New MS
model5<-glmer(Order~cb1.new.lin+peakcourt+Nb.courts+Judicial.power+monism+Veto.checks+yec+Gdp.capita+(1|Court/Country),family=binomial(link="logit"),data=new.ms)
summary(model5)

model6<-glmer(Order~cb1.new.pol+peakcourt+Nb.courts+Judicial.power+monism+Veto.checks+yec+Gdp.capita+(1|Court/Country),family=binomial(link="logit"),data=new.ms)
summary(model6)


library(stargazer)
stargazer(model.q1,model.q2)
r.squaredLR(model.q1)
r.squaredLR(model.q2)



stargazer(model1,model2,model3,model4,model5,model6)

r.squaredLR(model1)
r.squaredLR(model2)
r.squaredLR(model3)
r.squaredLR(model4)
r.squaredLR(model5)
r.squaredLR(model6)

pred1.pm <- crosspred(cb1.order.lin, model.q1, at=0:49, bylag=0.2, cumul=TRUE)
pred2.pm <- crosspred(cb1.order.pol, model.q2, at=0:49, bylag=0.2, cumul=TRUE)

# plot the lag-response curves for specific and incremental cumulative effects
plot(pred1.pm, "slices", var=1, col=3, ylab="Order", ci.arg=list(density=15,lwd=2),
     main="")
plot(pred2.pm, "slices", var=1, col=3, ylab="Order", ci.arg=list(density=15,lwd=2),
     main="")


#################################
#Lag for learning

poly<-poly(pdata.new$Case,degree3)
plot(poly)
b<-onebasis(pdata.new$Case, "poly", degree=2)
summary(b)

#Cross basis: Relationship in two dimensions predictor and lags
cb1.case<-crossbasis(pdata.new$Case,lag=10,argvar=list(fun="lin"),arglag=list(fun="lin"))
summary(cb1.case)


#Glmer Model
model<-glmer.nb(Case~cb1.case+peakcourt+Lag.order1+Judicial.power+Submiss+Gdp.capita+(1|Court/Country),data=pdata.new)
summary(model)

stargazer(model)

pred1.pm <- crosspred(cb1.case, model, at=0:10, bylag=0.2, cumul=TRUE)

# plot the lag-response curves for specific and incremental cumulative effects
plot(pred1.pm, "slices", var=10, col=3, ylab="Case", ci.arg=list(density=15,lwd=2),
     main="")

##################################


##########################################
# Bayesian model
pdata.bay<-pdata.new




#Formula
f4<-Order~Lag.order1+(1|Court:Country)


#Check priors
print(get_prior(f4,data=pdata.bay,family=bernoulli(link="logit")))


library(rstan)
rstan_options(autowrite = T)
options(mc.cores = parallel::detectCores())


system.time(logit.prior<-brm(f4, 
                                   family=bernoulli(link = "logit"),
                                   data=pdata.bay,
                                   warmup=1000, iter=5000, chains=4,#cores=7,
                                   prior=c(set_prior("normal(0,1)", class = "b"),
                                           set_prior("student_t(3,0,10)", class = "sd",group= "Court"),
                                           set_prior("student_t(3,0,10)", class = "sd",group= "Court:Country")),
                                   control=list(adapt_delta=0.99),
                                   init_r=0.1
))
summary(logit.prior)
on.exit(stopCluster(cl))


#Mamma mia effect 
mamma.mia<-pdata.bay[!(pdata.bay$Country %in% 9), ]

f4<-Order~Lag.order1*NMS+(1|Court:Country)


#Check priors
print(get_prior(f4,data=mamma.mia,family=binomial(link="logit")))


library(rstan)
rstan_options(autowrite = T)
options(mc.cores = parallel::detectCores())


system.time(logit.prior<-brm(f4, 
                             family=bernoulli(link = "logit"),
                             data=mamma.mia,
                             warmup=1000, iter=5000, chains=4,#cores=7,
                             prior=c(set_prior("normal(0,1)", class = "b"),
                                     #set_prior("student_t(3,0,10)", class = "sd",group= "Court"),
                                     set_prior("student_t(3,0,10)", class = "sd",group= "Court:Country")),
                             control=list(adapt_delta=0.99),
                             init_r=0.1
))
summary(logit.prior)

on.exit(stopCluster(cl))

########################
# Model inference
########################

# Diagnostic and Convergence
launch_shiny(nb15)

# Autocorrelation test
heidel.diag(nb15)

#R2glmm
R2m_model1<-function(x){
  Design.matrix<-as.mcmc(x,combine_chains=4)
  
  #Fixed effects
  Fixed <- fixef(x)[2]*Design.matrix[,2]+ fixef(x)[3]*Design.matrix[,3]+ fixef(x)[4]*Design.matrix[,4]+
    fixef(x)[5]*Design.matrix[,5]+ fixef(x)[6]*Design.matrix[,6]+ fixef(x)[7]*Design.matrix[,7]+
    fixef(x)[8]*Design.matrix[,8]+ fixef(x)[9]*Design.matrix[,9]+ fixef(x)[10]*Design.matrix[,10]+
    fixef(x)[11]*Design.matrix[,11]+ fixef(x)[12]*Design.matrix[,12]+fixef(x)[13]*Design.matrix[,13]
  
  varF<-var(Fixed)
  
  R2m<-varF/(varF+Design.matrix[,14]+VarCorr(x)$country$sd[1])
  
  mean(R2m)
}

R2c_model1<-function(x){
  Design.matrix<-as.mcmc(x,combine_chains=4)
  
  #Fixed effects
  Fixed <- fixef(x)[2]*Design.matrix[,2]+ fixef(x)[3]*Design.matrix[,3]+ fixef(x)[4]*Design.matrix[,4]+
    fixef(x)[5]*Design.matrix[,5]+ fixef(x)[6]*Design.matrix[,6]+ fixef(x)[7]*Design.matrix[,7]+
    fixef(x)[8]*Design.matrix[,8]+ fixef(x)[9]*Design.matrix[,9]+ fixef(x)[10]*Design.matrix[,10]+
    fixef(x)[11]*Design.matrix[,11]+ fixef(x)[12]*Design.matrix[,12]+fixef(x)[13]*Design.matrix[,13]
  
  varF<-var(Fixed)
  
  R2c<-(varF+Design.matrix[,14])/(varF+Design.matrix[,14]+VarCorr(x)$country$sd[1])
  
  mean(R2c)
}

R2m_model1(nb15)
R2c_model1(nb15)


