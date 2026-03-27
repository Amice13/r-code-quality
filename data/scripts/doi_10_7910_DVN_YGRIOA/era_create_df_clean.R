library(tidyverse)
library(rio)
library(officer)
library(dplyr)
library(ggrepel)
library(ggplot2)
library(rstudioapi)
library(foreign)
library(haven)
library(devEMF)
library(spatstat)
library(stringr)

setwd("location_of_data_and_code")

#############################
#Import ERA data
########################
#make sure tracts all have 12 digits
#Import ERA2 Q1 file- geocoded by HUD
infile23.1 <- import("data/data_file1")
infile23.1<-infile23.1 %>% mutate(tract=paste("00",var1,sep=""))
infile23.1<-infile23.1 %>% mutate(tract=substr(tract,nchar(tract)-11,nchar(tract)))

#Import ERA 2 Q2 file- geocoded by Treasury
infile23.2<-  import("data/data_file2")
infile23.2<-infile23.2 %>% mutate(tract=paste("00",var1,sep=""))
infile23.2<-infile23.2 %>% mutate(tract=substr(tract,nchar(tract)-11,nchar(tract)))

#Import ERA 1 Close out file- geocoded by Treasury
infile23<-import("data/data_file3")
infile23<-infile23 %>% mutate(tract=paste("00",var1,sep=""))
infile23<-infile23 %>% mutate(tract=substr(tract,nchar(tract)-11,nchar(tract)))

#keep only the columns we need, make sure date/time columns in same format, make everything lower case
#ERA 2 Q1
colnames(infile23.1)
# Code removed to ensure confidentiality; code limits the data to particular columns
infile23.1%>%group_by(var20)%>%summarize(n())
infile23.1<-infile23.1%>%select(-c("var17","var18"))
names(infile23.1)[c(1,4,5)]<-c("var2","var5","var6")

infile23.1<-infile23.1 %>%  mutate(var7=substr(var7,1,nchar(var7)-8))
infile23.1$var7<-format(as.Date(infile23.1$var7,"%m/%d/%Y"),"%Y-%m-%d")
infile23.1<-infile23.1 %>%  mutate(var8=substr(var8,1,nchar(var8)-8))
infile23.1$var8<-format(as.Date(infile23.1$var8,"%m/%d/%Y"),"%Y-%m-%d")
infile23.1<-infile23.1 %>%  mutate(var9=substr(var9,1,nchar(var9)-8))
infile23.1$var9<-format(as.Date(infile23.1$var9,"%m/%d/%Y"),"%Y-%m-%d")

infile23.1<-infile23.1%>%mutate(var2=tolower(var2),
                                var3=tolower(var3),
                                var4=tolower(var4),
                                var5=tolower(var5),
                                var6=tolower(var6),
                                var10=tolower(var10),
                                var11=tolower(var11),
                                var12=tolower(var12),
                                var13=tolower(var13),
                                var14=tolower(var14),
                                var15=var15)
nrow(infile23.1)
sum(infile23.1$var15)

#Repeat with ERA2 Q2
colnames(infile23.2)
names(infile23.2)[c(1,4,5)]<-c("var2","var5","var6")
infile23.2<-infile23.2%>%select(-c("var18"))
nrow(infile23.2)
sum(infile23.2$var15)

#Repeat with ERA1 Closeout
colnames(infile23)
names(infile23)[c(1,2,3)]<-c("var2","var5","var6")
# Code removed for confidentiality 
infile23%>%group_by(var20)%>%summarize(n())
infile23<-infile23%>%select(-c("var17","var19"))

infile23<-infile23 %>%  mutate(var7=substr(var7,1,nchar(var7)-8))
infile23$var7<-format(as.Date(infile23$var7,"%m/%d/%Y"),"%Y-%m-%d")
infile23<-infile23 %>%  mutate(var8=substr(var8,1,nchar(var8)-8))
infile23$var8<-format(as.Date(infile23$var8,"%m/%d/%Y"),"%Y-%m-%d")
infile23<-infile23 %>%  mutate(var9=substr(var9,1,nchar(var9)-8))
infile23$var9<-format(as.Date(infile23$var9,"%m/%d/%Y"),"%Y-%m-%d")

infile23<-infile23%>%mutate(var2=tolower(var2),
                            var3=tolower(var3),
                            var4=tolower(var4),
                            var5=tolower(var5),
                            var6=tolower(var6),
                            var10=tolower(var10),
                            var11=tolower(var11),
                            var12=tolower(var12),
                            var13=tolower(var13),
                            var14=tolower(var14),
                            var15=var15)
nrow(infile23)
sum(infile23$var15)

#Join ERA2 files
#We want to keep all ERA2 Q2 and only keep recipients from ERA2 Q1 who did not report in ERA2 Q2
recipientsQ2<-infile23.2%>% group_by(var12, var19)%>%summarize(n())
fundsQ1<-infile23.1%>%filter(!var12 %in% recipientsQ2$var12 & !var19 %in% recipientsQ2$var19)
fundsQ2<-full_join(infile23.2,fundsQ1)
nrow(fundsQ2) 
sum(fundsQ2$var15) 

#join ERA1 and ERA2 files
stack <- full_join(infile23,fundsQ2)

# Total Observations
nrow(stack) 
sum(stack$var15) 

# Remove observations that come from non-state and local government entities
# Code has been removed for confidentiality reasons

# Observations after removing ones that cannot be geocoded
# Code has been removed for confidentiality reasons

stack <- stack %>%
  subset(nchar(tract)==12)
nrow(stack) 
sum(stack$var15) 

# Observation matched to a location outside the 50 states or DC
stack <- stack %>% 
  mutate(st_fips = as.numeric(str_sub(tract,2,3)))
stack <- stack %>%
  subset(!st_fips %in% c(60,66,69,72,78))
summary(stack$st_fips)
stack <- stack %>% filter(as.numeric(tract)<60000000000)
nrow(stack) 
sum(stack$var15) 

#Create unique address variable
stack<-stack%>%mutate(address=paste(var2,var3,var5,var6,var16)) %>%
  mutate(tract = str_sub(tract,2,12))

stacksave<-stack
##################################
# Find observations that cannot be matched to the ACS statistics
xw_county_tract <- import("data/data_file5") %>%
  slice(-1) %>%
  transmute(
    key=county,
    tract = paste0(county,
                   tract %>%
                     gsub("\\.", "", .) %>%
                     str_pad(6, "left", "0")),
    pop = as.numeric(pop20),
    name=CountyName
  )


# Merge the ACS to the transaction-level data
stack <- stack %>%
  merge(xw_county_tract,all.x=T,all.y=F)
#make sure have all original stack rows and fund amounts
nrow(stack) 
sum(stack$var15) 
#Remove tracts with no ACS data
stack <- stack %>%
  subset(!is.na(pop))

nrow(stack) 
sum(stack$var15)  

##############
# Observations after removing duplicates
stack<-distinct(stack)
nrow(stack) 
sum(stack$var15) 

# Remove observations from one grantee; code has been removed for confidentiality reasons

##############################
#Remove transactions with no renters
Nrenters <- import("data/data_file6") %>%
  mutate(tract=paste0(str_pad(ipums_var_20,2,"left","0"),
                      str_pad(ipums_var_21,3,"left","0"),
                      str_pad(ipums_var_22,6,"left","0"))) %>%
  transmute(tract,
            renthh=ipums_var_15)
stack <- stack %>%
  merge(Nrenters,all.x=T,all.y=F)

nrow(stack)
sum(stack$var15)

stack<-stack%>%filter(renthh>0)

nrow(stack) 
sum(stack$var15) 

####################################################
#Remove partial ACS data that was used to remove tracts without ACS data
colnames(stack)
stack<-stack[,1:24]

#Save final stack file
export(stack,"data/data_file14")

#####################################

#group by tract to get sums
tractfunds <- stack%>%mutate(tract=paste("0",tract,sep=""))%>%group_by(tract) %>% 
  summarize(funds=sum(var15,na.rm=T),
            npayments=n(),
            naddresses=length(unique(address)))

#Add in full ACS data
xw_county_tract <- import("data/data_file5") %>%
  slice(-1) %>%
  transmute(
    key=county,
    tract = paste0(county,
                   tract %>%
                     gsub("\\.", "", .) %>%
                     str_pad(6, "left", "0")),
    pop = as.numeric(pop20),
    name=CountyName
  )

#poverty of tract population (not just renters!)
acs_raw <- import("data/data_file7")
acs <- acs_raw %>% 
  transmute(tract=paste0(str_pad(ipums_var_20,3,"left","0"),
                         str_pad(ipums_var_21,3,"left","0"),
                         str_pad(ipums_var_22,6,"left","0")),
            pov_num=ipums_var_2+ipums_var_3,
            pov_denom=ipums_var_4,
            pov=pov_num/pov_denom)

#ACS data with median rent
acs_raw_withrent <- import("data/data_file8")
acs_withrent <- acs_raw_withrent %>% 
  transmute(tract=paste0(str_pad(ipums_var_20,3,"left","0"),
                         str_pad(ipums_var_21,3,"left","0"),
                         str_pad(ipums_var_22,6,"left","0")),
            mediangrossrent=ipums_var_5,
            rentburden=ipums_var_19)

#ACS data with speaking English
acs_raw_withenglish <- import("data/data_file9")
names(acs_raw_withenglish) <- tolower(names(acs_raw_withenglish))
acs_withenglish <- acs_raw_withenglish %>% 
  transmute(tract=paste0(str_pad(ipums_var_20,3,"left","0"),
                         str_pad(ipums_var_21,3,"left","0"),
                         str_pad(ipums_var_22,6,"left","0")),
            speakenglish_univ = ipums_var_6, #Total
            speakspanish=ipums_var_7)

#Renters with children
kids <- import("data/data_file10") %>% 
  transmute(tract=paste0(str_pad(ipums_var_20,3,"left","0"),
                         str_pad(ipums_var_21,3,"left","0"),
                         str_pad(ipums_var_22,6,"left","0")),
            renthh_wkidsunder18=ipums_var_8,
            renthh_denom1=ipums_var_9,
            renthh_singlemomwkids=ipums_var_10,
            renthh_denom2=ipums_var_11)
sum(kids$renthh_denom1!=kids$renthh_denom2)

#Number of renters
Nrenters <- import("data/data_file6") %>% 
  mutate(tract=paste0(str_pad(ipums_var_20,3,"left","0"),
                      str_pad(ipums_var_21,3,"left","0"),
                      str_pad(ipums_var_22,6,"left","0"))) %>% 
  transmute(tract,
            renthh=ipums_var_15)

#Race of renters
renter_race <- import("data/data_file11") %>%
  mutate(tract=paste0(str_pad(ipums_var_20,3,"left","0"),
                      str_pad(ipums_var_21,3,"left","0"),
                      str_pad(ipums_var_22,6,"left","0")))  %>%
  subset(ipums_var_15 > 0) %>%
  transmute(tract,
            renters = ipums_var_15, #this is same as renthh
            renters_whitenonhispanic = ipums_var_12,
            renters_black = ipums_var_13,
            renters_hispanic = ipums_var_14,
            renters_color=renters_black+renters_hispanic,
            sharerenting = ipums_var_15 / ipums_var_16)

#Renter income
renter_income<-import("data/data_file12")%>%
  mutate(tract=paste0(str_pad(ipums_var_20,3,"left","0"),
                      str_pad(ipums_var_21,3,"left","0"),
                      str_pad(ipums_var_22,6,"left","0")))  %>%
  transmute(tract,
            med_income=ipums_var_17,
            renter_med_income=ipums_var_18)

#Merge all the ACS data
tracts <- xw_county_tract %>% 
  mutate(tract=paste("0",tract,sep="")) %>%
  merge(acs,
        all.x = T,all.y=F)%>%
  merge(acs_withrent,
        all.x = T,all.y=F) %>% 
  merge(acs_withenglish,
        all.x = T,all.y=F) %>% 
  merge(Nrenters,
        all.x=T,all.y=F) %>% 
  merge(kids,
        all.x=T,all.y=F) %>% 
  merge(renter_race,
        all.x=T,all.y=F) %>%
 merge(renter_income,
        all.x=T, all.y=F)

#Keep only US states
tracts<-tracts%>%filter(tract<"057000000000")

#Join the ACS data with the ERA data
combo <- tracts %>%
  merge(tractfunds,all.x=T,all.y=F) %>%
  mutate(funds=case_when(is.na(funds) ~ 0,
                         T ~ funds))

combo <- combo %>% 
  mutate(naddresses=case_when(is.na(naddresses) ~ 0,
                              T ~ naddresses))

#These should match funds and number of transactions in stack file
sum(combo$funds)
sum(combo$npayments,na.rm=TRUE)

#######################
#Save combo data
export(combo,"data/data_file14")

