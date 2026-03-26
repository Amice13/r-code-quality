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
library(arrow)
library(cNORM)

setwd("location_of_data_and_code")

#############################
#Import data
########################
stack<-import("data/data_file14")
tractfunds <- stack%>%mutate(tract=paste("0",tract,sep=""))%>%group_by(tract) %>% 
  summarize(funds=sum(Amount_of_Payment,na.rm=T),
            npayments=n(),
            naddresses=length(unique(address)))
tracts<-read.csv("data/datafile_13")
tracts<-tracts %>% mutate(tract=paste("00",var1,sep=""))
tracts<-tracts %>% mutate(tract=substr(tract,nchar(tract)-11,nchar(tract)))

combo<-import("data/data_file14")

#############################
#Figure 1: ERA Funds per Renting Household by Deciles of the Annual Eviction Filings
#Convert tracts from 2020 census tracts to 2010 census tracks
conversion<-read.csv("data/datafile_14")
conversion<-conversion%>%select(var_23,var_24)
names(conversion)[1:2]<-c("fips2010","tract")

#make sure all tracts have 12 digits
conversion<-conversion %>% mutate(tract=paste("00",tract,sep=""),
                                  fips2010=paste("00",fips2010,sep=""))
conversion<-conversion %>% mutate(tract=substr(tract,nchar(tract)-11,nchar(tract)),
                                  fips2010=substr(fips2010,nchar(fips2010)-11,nchar(fips2010)))

tractfunds<-left_join(tractfunds,conversion)

##tracts without a 1-to-1 conversion
tracts_diff<-tractfunds[tractfunds$tract %in% tractfunds$tract[duplicated(tractfunds$tract)],]
#tracts with a 1-to-1 conversion
tracts_same<-tractfunds[!tractfunds$tract %in% tracts_diff$tract,]
#make sure have all tracts
nrow(tracts_same)+nrow(tracts_diff)

#tracts without a 1-to-1 conversion need to have all addresses in them recoded
tracts_to_recode<-distinct(tracts_diff,tract)
stack_recode <- left_join(tracts_to_recode,stack%>%mutate(tract=paste("0",tract,sep="")))

#Import the recoded address
outfile22<-import("data/data_file15") #geocded by Treasury
outfile3<-import("data/data_file16") #recoded era1 closeout addresses that didn't have a one-to-one match. done by Treasury

outfile<-full_join(outfile22,outfile3)

names(outfile)[1:3]<-c("Address_Line_1","City_Name","State_Code")
############
outfile<-distinct(outfile)
colnames(outfile)
outfile<-outfile[,c(1:8,23,24)]
outfile<-distinct(outfile)
###############
#Keep only the recoded addresses we need
recoded<-left_join(stack_recode,outfile)

# Remove observations that that still could not be geocoded to a tract
# Code has been removed for confidentiality reasons

#combine the files with the covariate data
recoded<-left_join(recoded,tracts)

#Group the funding data
#since grouping by same tract, take average renter data
mid_step<-recoded %>% group_by(fips2010,tract) %>%
  summarize(funds=sum(Amount_of_Payment,na.rm=T),
            npayments=n(),
            naddresses=length(unique(address)),
            renters=mean(renters),
            renters_whitenonhispanic=mean(renters_whitenonhispanic),
            renters_black=mean(renters_black),
            renters_hispanic=mean(renters_hispanic),
            renters_color=mean(renters_color),
            sharerenting=mean(sharerenting),
            renter_med_income=mean(renter_med_income),
            renthh_wkidsunder18 =mean(renthh_wkidsunder18),
            mediangrossrent=mean(mediangrossrent),
            renthh_singlemomwkids=mean(renthh_singlemomwkids))

#now since combining different tracts, sum renter data
tractfunds_recode <- mid_step %>% group_by(fips2010) %>% 
  summarize(funds=sum(funds,na.rm=T),
            npayments=sum(npayments),
            naddresses=sum(naddresses),
            renters=sum(renters),
            renters_whitenonhispanic=sum(renters_whitenonhispanic),
            renters_black=sum(renters_black),
            renters_hispanic=sum(renters_hispanic),
            renters_color=sum(renters_color),
            sharerenting=mean(sharerenting),
            renter_med_income=mean(renter_med_income),
            renthh_wkidsunder18 =sum(renthh_wkidsunder18),
            mediangrossrent=mean(mediangrossrent),
            renthh_singlemomwkids=sum(renthh_singlemomwkids))

#add race data to same tracts
tracts_same<-left_join(tracts_same,tracts)

#join the newly recoded tracts and tracts with a 1-to-1 conversion together
tracts_same<-tracts_same%>%select(fips2010,funds,npayments,naddresses, renters, renters_whitenonhispanic,
                                  renters_black, renters_hispanic,renters_color,sharerenting,renter_med_income,
                                  renthh_wkidsunder18, mediangrossrent,renthh_singlemomwkids)
combo2<-rbind(tracts_same,tractfunds_recode)
combo2 <- combo2 %>% group_by(fips2010) %>% 
  summarize(funds=sum(funds,na.rm=T),
            npayments=sum(npayments),
            naddresses=sum(naddresses),
            renters=sum(renters),
            renters_whitenonhispanic=sum(renters_whitenonhispanic),
            renters_black=sum(renters_black),
            renters_hispanic=sum(renters_hispanic),
            renters_color=sum(renters_color),
            sharerenting=mean(sharerenting),
            renter_med_income=mean(renter_med_income),
            renthh_wkidsunder18 =sum(renthh_wkidsunder18),
            mediangrossrent=mean(mediangrossrent),
            renthh_singlemomwkids=sum(renthh_singlemomwkids))

#Pull eviction lab data
evict<-import("data/data_file17")
#if NA, that means data is missing, not 0. 

#anonymize variables
evict<-evict%>%mutate(year=evic_lab_var1,
                      filings=evic_lab_var2,
                      fips=evic_lab_var3)

#average filing 2015-2018
evict<-evict%>%filter(year>=2015)
baseline<-evict %>% group_by(fips)%>%summarize(base_ann_evic=mean(filings,na.rm=TRUE))
names(baseline)[1]<-"fips2010"

baseline<-baseline %>% mutate(fips2010=paste("00",fips2010,sep=""))
baseline<-baseline %>% mutate(fips2010=substr(fips2010,nchar(fips2010)-11,nchar(fips2010)))

#Keep only US and DC
baseline<-baseline%>%filter(fips2010<"057000000000")

#Join funding/race data with eviction data
baseline<-left_join(combo2,baseline)

#Figure
fig4 <- baseline %>% 
  filter(renters>0) %>%
  mutate(fundsperrenthh=funds/renters,
         fundsperrenthh=case_when(is.na(fundsperrenthh) ~ 0,
                                  T ~ fundsperrenthh),
         evic_per_renthh=base_ann_evic/renters) %>%
  subset(!is.na(base_ann_evic))%>%
  arrange(evic_per_renthh)%>%
  mutate(cumrenthh=(cumsum(renters)/sum(renters))*10,
         evicbin=cumrenthh %>% ceiling()) %>%
  group_by(evicbin) %>% 
  summarize(evic_per_renthh=weighted.mean(evic_per_renthh,renters),
            fundsperrenthh=weighted.mean(fundsperrenthh,renters),
            ntracts=n(),
            totalrenthh = sum(renters),
            npayments=sum(npayments,na.rm=TRUE),
            naddresses=sum(naddresses)) 

export(fig4, "paperfigs/eviction.dta")

###############################################
#Figure: ERA Funds per renting household by decile of rent burden (Median Gross Rent as a Percentage of Household Income)
#not all tracks have rent burden data. Drop ones we don't have
fig_rent_burden <- combo %>% 
  subset(renters>0 & !is.na((rentburden))) %>% 
  transmute(tract,rentburden,renters,funds,npayments,naddresses,
            fundsperrenthh=funds/renters,
            addressesperrenthh=naddresses/renters) %>% 
  arrange(rentburden) %>% 
  mutate(cumrenthh=(cumsum(renters)/sum(renters))*10,
         bin=cumrenthh %>% ceiling()) %>% 
  group_by(bin) %>% 
  summarize(avg_fundsperrenthh=weighted.mean(fundsperrenthh,renters),
            avg_addressesperrenthh=weighted.mean(addressesperrenthh,renters),
            med_fundsperrenthh=median(fundsperrenthh),
            med_addressesperrenthh=median(addressesperrenthh),
            rentburden=weighted.mean(rentburden,renters),
            ntract=n(),
            npayments=sum(npayments,na.rm=TRUE),
            naddress=sum(naddresses),
            renters=sum(renters))

export(fig_rent_burden,"paperfigs/rentburden.dta")

##################################################################
#Figure: ERA Funds per Renting Household by Deciles of the Poverty Rate
fig_pov <- combo %>%
  subset(renthh>0) %>%
  transmute(tract,pov,pov_denom,funds,renthh,npayments,naddresses, HH,
            fundsperrenthh=funds/renthh,
            addressesperrenthh=naddresses/renters_povuniv) %>%
  arrange(pov) %>%
  mutate(cumrenthh=(cumsum(renthh)/sum(renthh))*10,
         bin=cumrenthh %>% ceiling()) %>%
  group_by(bin) %>%
  summarize(avg_fundsperrenthh=weighted.mean(fundsperrenthh,renthh),
            avg_addressesperrenthh=weighted.mean(addressesperrenthh,renthh),
            med_fundsperrenthh=weighted.median(fundsperrenthh,renthh),
            med_addressesperrenthh=median(addressesperrenthh),
            pov=weighted.mean(pov,pov_denom),
            totalrenthh = sum(renthh),
            fundsperrenthh=totalerafundsdistributed/totalrenthh,
            ntracts=n(),
            npayments=sum(npayments,na.rm=TRUE),
            naddresses=sum(naddresses))

export(fig_pov,"paperfigs/pov_renthh.dta")

#####################################################################################
#Figure: ERA Funds per Renting Household by Deciles of Share of the Renting Householders who are Black
fig_black_head <- combo %>% 
  subset(renters>0) %>% 
  transmute(tract,renters_black,renters,funds,npayments,naddresses,
            fundsperrenthh=funds/renters,
            addressesperrenthh=naddresses/renters,
            perblack=renters_black/renters) %>% 
  arrange(perblack) %>% 
  mutate(cumrenthh=(cumsum(renters)/sum(renters))*10,
         bin=cumrenthh %>% ceiling()) %>% 
  group_by(bin) %>% 
  summarize(avg_fundsperrenthh=weighted.mean(fundsperrenthh,renters),
            avg_addressesperrenthh=weighted.mean(addressesperrenthh,renters),
            med_fundsperrenthh=median(fundsperrenthh),
            med_addressesperrenthh=median(addressesperrenthh),
            perblack=weighted.mean(perblack,renters),
            ntract=n(),
            npayments=sum(npayments,na.rm=TRUE),
            naddress=sum(naddresses),
            renters=sum(renters))

export(fig_black_head,"paperfigs/black_renters.dta")


#Figure: ERA Funds per Renting Household by Deciles of Share of the Renting Householders who are Hispanic
fig_hispanic_head <- combo %>% 
  subset(renters>0) %>% 
  transmute(tract,renters_hispanic,renters,funds,npayments,naddresses,
            fundsperrenthh=funds/renters,
            addressesperrenthh=naddresses/renters,
            perhispanic=renters_hispanic/renters) %>% 
  arrange(perhispanic) %>% 
  mutate(cumrenthh=(cumsum(renters)/sum(renters))*10,
         bin=cumrenthh %>% ceiling()) %>% 
  group_by(bin) %>% 
  summarize(avg_fundsperrenthh=weighted.mean(fundsperrenthh,renters),
            avg_addressesperrenthh=weighted.mean(addressesperrenthh,renters),
            med_fundsperrenthh=median(fundsperrenthh),
            med_addressesperrenthh=median(addressesperrenthh),
            perhispanic=weighted.mean(perhispanic,renters),
            ntract=n(),
            npayments=sum(npayments,na.rm=TRUE),
            naddress=sum(naddresses),
            renters=sum(renters))

export(fig_hispanic_head,"paperfigs/hispanic_renters.dta")

####################################
#Figure: ERA Funds per Renting Household by Deciles of the Share of the Census Tract that is Renting Households with KidsChildren
fig_w_kids <- combo %>%
  subset(renthh>0) %>%
  mutate(renthh_wkidsunder18 = renthh_wkidsunder18 / renthh,
         fundsperrenthh=funds/renthh,
         addressesperrenthh=naddresses/renthh) %>%
  subset(!is.na(renthh_wkidsunder18)) %>%
  arrange(renthh_wkidsunder18) %>%
  mutate(cumhh=(cumsum(renthh)/sum(renthh))*10,
         bin=cumhh %>% ceiling()) %>%
  group_by(bin) %>%
  summarize(renthh_wkidsunder18=weighted.mean(renthh_wkidsunder18,renthh),
            avg_fundsperrenthh=weighted.mean(fundsperrenthh,renthh),
            avg_addressesperrenthh=weighted.mean(addressesperrenthh,renthh),
            med_fundsperrenthh=weighted.median(fundsperrenthh,renthh),
            med_addressesperrenthh=median(addressesperrenthh),
            totalrenthh = sum(renthh),
            tracts=n(),
            npayments=sum(npayments,na.rm=TRUE),
            naddresses=sum(naddresses))

export(fig_w_kids, "paperfigs/rent_w_kids.dta")

#############################
#Figure: ERA Funds per Renting Household by Deciles of the Share of the Census Tract that is Renting Households that are Single Mothers
fig_smom <- combo %>%
  subset(renthh>0) %>%
  mutate(renthh_singlemomwkids=renthh_singlemomwkids/renthh,
         fundsperrenthh=funds/renthh,
         addressesperrenthh=naddresses/renthh) %>%
  subset(!is.na(renthh_singlemomwkids)) %>%
  arrange(renthh_singlemomwkids) %>%
  mutate(cumhh=(cumsum(renthh)/sum(renthh))*10,
         bin=cumhh %>% ceiling()) %>%
  group_by(bin) %>%
  summarize(renthh_singlemomwkids=weighted.mean(renthh_singlemomwkids,renthh),
            avg_fundsperrenthh=weighted.mean(fundsperrenthh,renthh),
            avg_addressesperrenthh=weighted.mean(addressesperrenthh,renthh),
            med_fundsperrenthh=weighted.median(fundsperrenthh,renthh),
            med_addressesperrenthh=median(addressesperrenthh),
            totalrenthh = sum(renthh),
            tracts=n(),
            npayments=sum(npayments,na.rm=TRUE),
            naddresses=sum(naddresses))

export(fig_smom,"paperfigs/singlemom.dta")

##########################################
#Figure: eviction risk factor
#Take the census tracts and rank them from 1 to N in ascending order, weighted by number of renters, for each of the x-axis characteristics: 
#rent burden, poverty rate, renting householders who are black, renting householders with kids, and renting householders headed by single mothers. 
#Then, average the rank across the tracts. Then break the tracts into average rank deciles and graph the funds per renting household by average rank decile.
combo_rank<-combo_updated%>%transmute(tract,pop,renters,
                              rentburden,
                              pov,
                              shareblackrenters=renters_black/renters,
                              withkids=renthh_wkidsunder18/renthh_denom1,
                              singlemom=renthh_singlemomwkids/renthh_denom2,
                              funds,npayments,naddresses,
                              weight=renters/sum(renters,na.rm=TRUE))

combo_rank<-combo_rank%>%mutate(rentburden_rank=rank(rentburden*weight,na.last=FALSE,ties.method = "average" ),
                                pov_rank=rank(pov*weight,na.last=FALSE,ties.method = "average"),
                                black_rank=rank(shareblackrenters*weight,na.last=FALSE,ties.method = "average"),
                                kids_rank=rank(withkids*weight,na.last=FALSE,ties.method = "average"),
                                singlemom_rank=rank(singlemom*weight,na.last=FALSE,ties.method = "average"),
                                rank=(rentburden_rank+pov_rank+black_rank+kids_rank+singlemom_rank)/5)


fig_riskfactor <- combo_rank %>% 
  subset(renters>0) %>% 
  transmute(tract,renters,funds,npayments,naddresses,
            fundsperrenthh=funds/renters,
            addressesperrenthh=naddresses/renters,
            rank) %>% 
  arrange(rank) %>% 
  mutate(cumrenthh=(cumsum(renters)/sum(renters))*10,
         bin=cumrenthh %>% ceiling()) %>% 
  group_by(bin) %>% 
  summarize(avg_fundsperrenthh=weighted.mean(fundsperrenthh,renters),
            avg_addressesperrenthh=weighted.mean(addressesperrenthh,renters),
            med_fundsperrenthh=median(fundsperrenthh),
            med_addressesperrenthh=median(addressesperrenthh),
            rank=weighted.mean(rank,renters),
            ntract=n(),
            npayments=sum(npayments,na.rm=TRUE),
            naddress=sum(naddresses),
            renters=sum(renters))

fig_riskfactor<-fig_riskfactor%>%mutate(bin=bin*10)
sum(fig_riskfactor$ntract)

export(fig_riskfactor,"paperfigs/rank_weighted.dta")
