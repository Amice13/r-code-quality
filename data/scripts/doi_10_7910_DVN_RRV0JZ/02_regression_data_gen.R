####
#Author: M.R. Kenwick
#Date: Dec 28, 2020
#Purpose: builds a dataset for regression analysis and validation.
####
#Install packages, as necessary
#install.packages('tidyverse')
#install.packages('WDI')
#install.packages('countrycode')
#install.packages('haven')
#install.packages('reshape') 
#install.packages('stringr')
#install.packages('zoo')

rm(list=ls())
library(tidyverse)
library(WDI)
library(countrycode)
library(haven)
library(reshape) 
library(stringr)
library(zoo)

#Set working directory to top level of replication folder
setwd()
#Load base data and latent estimates (generated in bo_irt_model.R from the "model" folder)
load('model/base_data.RData')

load('model/bo_static_output.RData')
#extract means and sds of latent trait
wall$xi<-apply(output$xi,2,mean)
wall$xi_sd<-apply(output$xi,2,sd)
data<-merge(data,dplyr::select(wall,ddyr,xi,xi_sd),by=c('ddyr'),all.x=T,all.y=F)
data$theta<-apply(output$theta,2,mean)
data$theta_sd<-apply(output$theta,2,sd)
rm(output)
#generating an additive index of manifest indicators at border crossings
data$index<-(data$gate/3)+(data$building/4)+(data$split/2)

###Merge in covariates

#GDP
#gdp<-WDI(indicator='NY.GDP.PCAP.KD', start=2000, end=2018)
#selecting GDP per capita in constant 2010 US$
gdp<-read_csv('regression/gdp/worldbank_gdp_dec20_2019.csv')
gdp <- gdp %>%
  pivot_longer(`2000`:`2018`, names_to = "year", values_to = "value")
gdp<-dplyr::select(gdp,iso2c = 'Country Code', year, NY.GDP.PCAP.KD = 'value')
gdp$state1<-countrycode(gdp$iso2c,origin="iso3c",destination='cown')
gdp$state1[gdp$iso2c=="SRB"]<-345
gdp<-dplyr::select(gdp,state1,year,NY.GDP.PCAP.KD)
colnames(gdp)<-c('state1','year','gdp_pc_1')
data<-merge(data,gdp,by=c('state1','year'),all.x=T,all.y=F)
colnames(gdp)<-c('state2','year','gdp_pc_2')
data<-merge(data,gdp,by=c('state2','year'),all.x=T,all.y=F)
data$lngdp1<-log(data$gdp_pc_1)
data$lngdp2<-log(data$gdp_pc_2)
data$lngdp_diff <- data$lngdp1 - data$lngdp2

#Dyad indicator (for later)
data$dyad<-ifelse(data$state1<data$state2,
                  data$state1*1000+data$state2,
                  data$state2*1000+data$state1)

#ACD civil conflict data
acd<-read.csv("regression/acd/ucdp-prio-acd-172.csv")
acd<-subset(acd,type_of_conflict==3 & year>1979 & year<=1999)
acd = acd %>% mutate(epid = group_indices(acd,conflict_id,start_date2))
acd = acd %>%
  group_by(epid) %>%
  mutate(epint = max(intensity_level))
names(acd)[names(acd)=='location']<-"location_name"
names(acd)[names(acd)=='gwno_loc']<-"location"
acd$loc1<-as.numeric(str_split_fixed(acd$location,',',6)[,1])
acd$loc2<-as.numeric(str_split_fixed(acd$location,',',6)[,2])
acd$loc3<-as.numeric(str_split_fixed(acd$location,',',6)[,3])
acd$loc4<-as.numeric(str_split_fixed(acd$location,',',6)[,4])
acd$loc5<-as.numeric(str_split_fixed(acd$location,',',6)[,5])
acd$loc6<-as.numeric(str_split_fixed(acd$location,',',6)[,6])
acd$numlocations<-apply(!is.na(acd[,30:35]),1,sum)

f = function(x) with(x, data.frame(conflict_id,location_name,side_a,side_a_2nd,         
                                   side_b,side_b_id,side_b_2nd,incompatibility,  
                                   territory_name,year,intensity_level,cumulative_intensity,
                                   type_of_conflict,start_date,start_prec,start_date2,    
                                   start_prec2,ep_end,ep_end_date,ep_end_prec,       
                                   gwno_a,gwno_a_2nd,gwno_b,gwno_b_2nd,          
                                   location,region,version,epid,         
                                   epint,loc1,loc2,loc3,               
                                   loc4,loc5,loc6,numlocations, 
                                   loct = seq(1, numlocations, by=1)))   
acd = do.call('rbind', by(acd, 1:nrow(acd), f))
acd = acd[order(acd$conflict_id, acd$year),]
for(ii in 1:6){
  acd$loc[acd$loct==ii]<-acd[acd$loct==ii,paste("loc",ii,sep="")]
}
acd$civ<-1
acd = acd %>%
  group_by(loc) %>%
  mutate(civ = sum(civ)) 
acd = acd %>% dplyr::select(loc,civ)
acd = unique(as.data.frame(acd))
data = merge(data, acd, by.x=c('state1'),by.y=c('loc'),all.x=T,all.y=F)
data$civ[is.na(data$civ)]<-0
names(data)[names(data) == 'civ'] <- 'civ1'
data = merge(data, acd, by.x=c('state2'),by.y=c('loc'),all.x=T,all.y=F)
data$civ[is.na(data$civ)]<-0
names(data)[names(data) == 'civ'] <- 'civ2'


#Cultural homogeneity
frac<-read.csv('regression/cultural/fractionalization.csv',fileEncoding="UTF-8-BOM")
dim(frac)
frac$state1<-countrycode(frac$Country,origin="country.name",destination='cown')
frac<-frac[frac$Country!="Yugoslavia (pre 1991)",]
dim(frac)
frac$Ethnic<-as.numeric(as.character(frac$Ethnic))
frac$Language<-as.numeric(as.character(frac$Language))
frac$Religion<-as.numeric(as.character(frac$Religion))
frac$cultural_homog<- (1 - frac$Ethnic) + (1 - frac$Language) + (1 - frac$Religion)
frac<-dplyr::select(frac,state1,cultural_homog)
data<-merge(data,frac,by="state1",all.x=T,all.y=F)


#Polity
polity<-read.csv('regression/polity/p4v2017.csv')
polity<-dplyr::select(polity,ccode,year,polity2)
colnames(polity)<-c('ccode','year','polity2_1')
data<-merge(data,polity,by.x=c('state1','year'),by.y=c('ccode','year'),all.x=T,all.y=F)
colnames(polity)<-c('ccode','year','polity2_2')
data<-merge(data,polity,by.x=c('state2','year'),by.y=c('ccode','year'),all.x=T,all.y=F)


#Ease of doing business across borders
trade<-read.csv('regression/ease_of_trade/trading_across_borders.csv')
#Note--we manually deleted sub-state measures
trade$X<-NULL
colnames(trade)[1]<-"country"
trade$state1<-countrycode(trade$country,origin="country.name",destination="cown")
trade$state1[trade$country=="West Bank and Gaza"]<-991
trade$state1[trade$country=="Serbia"]<-345
trade<-dplyr::select(trade,state1,Trading.across.Borders.rank)
trade<-na.omit(trade)
data<-merge(data,trade,by='state1',all.x=T,all.y=F)


#Populism
populism<-read.csv('regression/populism/populism_guardian.csv')
populism$state1<-countrycode(populism$country,origin='country.name',destination='cown')
populism<- populism %>% 
  group_by(state1) %>%
  mutate(pop = mean(average.score,na.rm=T)) %>% 
  dplyr::select(state1,pop) %>% 
  unique()
data<-merge(data,populism,by='state1',all.x=T,all.y=F)


#Visa data
visa<-read.csv('regression/visa/mau_visa.csv',stringsAsFactors = F)
colnames(visa)[1]<-"country1"
visa <- reshape::melt(visa)
colnames(visa)[2:3]<-c("country2","visa_waiver")
visa$country2<-as.character(visa$country2)
visa$country1[visa$country1=="Central African Rep."]<-"Central African Republic"
visa$country2[visa$country2=="Central African Rep."]<-"Central African Republic"
visa$country2[visa$country2=="Central.African.Rep."]<-"Central African Republic"
visa$country1[visa$country1=="Korea (Peoples Rep.)"]<-"North Korea"
visa$country2[visa$country2=="Korea (Peoples Rep.)"]<-"North Korea"
visa$country2[visa$country2=="Korea..Peoples.Rep.."]<-"North Korea"
visa$country1[visa$country1=="Kyrgystan"]<-"Kyrgyzstan"
visa$country2[visa$country2=="Kyrgystan"]<-"Kyrgyzstan"
visa$country1[visa$country1=="Taijikistan"]<-"Tajikistan"
visa$country2[visa$country2=="Taijikistan"]<-"Tajikistan"
visa$country1[visa$country1=="Serbia"]<-"Yugoslavia"
visa$country2[visa$country2=="Serbia"]<-"Yugoslavia"
visa$country1[visa$country1=="Swasiland"]<-"Swaziland"
visa$country2[visa$country2=="Swasiland"]<-"Swaziland"
visa$state1<-countrycode(visa$country1,origin='country.name',destination='cown')
visa$state2<-countrycode(visa$country2,origin='country.name',destination='cown')
visa$dyad<-ifelse(visa$state1<visa$state2,
                  visa$state1*1000+visa$state2,
                  visa$state2*1000+visa$state1)
visa<- visa %>% dplyr::select(state1,state2,visa_waiver)
#Note: flipped states 1 and 2 because in the visa data state1 is the destination country
data<-merge(data,visa,by.x=c('state1','state2'),by.y=c('state2','state1'),all.x=T,all.y=F)


#Religion vars
relig<-read.csv('regression/religion/WRP_national.csv')
relig<-dplyr::select(relig,state,name,year,chrstgenpct,chrstprotpct,chrstcatpct,chrstorthpct,
                     chrstangpct,judorthpct,judgenpct,judconspct,judrefpct,islmgenpct,islmsunpct,islmshipct,
                     hindgenpct,budgenpct,syncgenpct,anmgenpct,nonreligpct)
relig$dom_lev1<-c('christianity','judaism','islam','hindu','buddhism','syncretic','animism','non-religious')[apply(relig[,c('chrstgenpct','judgenpct','islmgenpct','hindgenpct',
                              'budgenpct','syncgenpct','anmgenpct','nonreligpct')],1,which.max)]
dom2_names<-c('protestant','catholic','christ_ortho','anglican','orthodox','conservative','reform','sunni','shia','hindu','buddhism','syncretic','animism','non-religious')
relig$dom_lev2<-dom2_names[apply(relig[,c('chrstprotpct','chrstcatpct','chrstorthpct','chrstangpct','judorthpct',
               'judconspct','judrefpct','islmsunpct','islmshipct','hindgenpct','budgenpct','syncgenpct','anmgenpct','nonreligpct')],1,which.max)]
relig<-dplyr::select(relig,state,year,chrstgenpct,judgenpct,islmgenpct,hindgenpct,dom_lev1,dom_lev2)
colnames(relig)<-c('state','year','chrstgenpct_1','judgenpct_1','islmgenpct_1','hindgenpct_1','dom_lev1_1','dom_lev2_1')
data<-merge(data,relig,by.x=c('state1','year'),by.y=c('state','year'),all.x=T,all.y=F)
colnames(relig)<-c('state','year','chrstgenpct_2','judgenpct_2','islmgenpct_2','hindgenpct_2','dom_lev1_2','dom_lev2_2')
data<-merge(data,relig,by.x=c('state2','year'),by.y=c('state','year'),all.x=T,all.y=F)

#Interpolate the religious difference indicators
data$relig_1_shared<-ifelse(data$dom_lev1_1==data$dom_lev1_2,1,0)
data$relig_2_shared<-ifelse(data$dom_lev2_1==data$dom_lev2_2,1,0)
data$yr_cat[data$year>=2000 & data$year<2005]<-1
data$yr_cat[data$year>=2005 & data$year<2010]<-2
data$yr_cat[data$year>=2010]<-3
data<- data %>% 
  group_by(state1,state2,yr_cat) %>% 
  mutate(relig_1_shared_i = max(relig_1_shared,na.rm=T)) %>% 
  mutate(relig_2_shared_i = max(relig_2_shared,na.rm=T)) %>% 
  ungroup()
data$relig_1_shared_i[data$relig_1_shared_i=="-Inf"]<-NA
data$relig_2_shared_i[data$relig_2_shared_i=="-Inf"]<-NA


#Schengen Indicators
data$schengen1<-0
data$schengen1[(data$country1=="Austria" & data$year>=1998 )  | 
                 (data$country1=="Belgium"& data$year>=1996) | 
                 (data$country1=="Czech Republic" & data$year>=2008) | 
                 (data$country1=="Denmark" & data$year>=2002) | 
                 (data$country1=="Estonia" & data$year>=2008) | 
                 (data$country1=="Finland" & data$year>=2002) | 
                 (data$country1=="France" & data$year>=1996) | 
                 (data$country1=="Germany" & data$year>=1996) | 
                 (data$country1=="Greece" & data$year>=2001) | 
                 (data$country1=="Hungary" & data$year>=2008) | 
                 (data$country1=="Iceland" & data$year>=2002) | 
                 (data$country1=="Italy" & data$year>=1998)| 
                 (data$country1=="Latvia" & data$year>=2008) | 
                 (data$country1=="Liechtenstein" & data$year>=2012) | 
                 (data$country1=="Lithuania" & data$year>=2008) | 
                 (data$country1=="Luxembourg" & data$year>=1996) | 
                 (data$country1=="Malta" & data$year>=2008) | 
                 (data$country1=="Netherlands" & data$year>=1996) | 
                 (data$country1=="Norway" & data$year>=2002) | 
                 (data$country1=="Poland" & data$year>=2008) | 
                 (data$country1=="Portugal" & data$year>=1996) | 
                 (data$country1=="Slovakia" & data$year>=2008) | 
                 (data$country1=="Slovenia" & data$year>=2008) | 
                 (data$country1=="Spain" & data$year>=1996) | 
                 (data$country1=="Sweden" & data$year>=2002) | 
                 (data$country1=="Switzerland" & data$year>=2009)]<-1
data$schengen2<-0
data$schengen2[(data$country2=="Austria" & data$year>=1998 )  | 
                 (data$country2=="Belgium"& data$year>=1996) | 
                 (data$country2=="Czech Republic" & data$year>=2008) | 
                 (data$country2=="Denmark" & data$year>=2002) | 
                 (data$country2=="Estonia" & data$year>=2008) | 
                 (data$country2=="Finland" & data$year>=2002) | 
                 (data$country2=="France" & data$year>=1996) | 
                 (data$country2=="Germany" & data$year>=1996) | 
                 (data$country2=="Greece" & data$year>=2001) | 
                 (data$country2=="Hungary" & data$year>=2008) | 
                 (data$country2=="Iceland" & data$year>=2002) | 
                 (data$country2=="Italy" & data$year>=1998)| 
                 (data$country2=="Latvia" & data$year>=2008) | 
                 (data$country2=="Liechtenstein" & data$year>=2012) | 
                 (data$country2=="Lithuania" & data$year>=2008) | 
                 (data$country2=="Luxembourg" & data$year>=1996) | 
                 (data$country2=="Malta" & data$year>=2008) | 
                 (data$country2=="Netherlands" & data$year>=1996) | 
                 (data$country2=="Norway" & data$year>=2002) | 
                 (data$country2=="Poland" & data$year>=2008) | 
                 (data$country2=="Portugal" & data$year>=1996) | 
                 (data$country2=="Slovakia" & data$year>=2008) | 
                 (data$country2=="Slovenia" & data$year>=2008) | 
                 (data$country2=="Spain" & data$year>=1996) | 
                 (data$country2=="Sweden" & data$year>=2002) | 
                 (data$country2=="Switzerland" & data$year>=2009)]<-1


#ID indicators
data$borderid_dir<-data$borderid
data$borderid<-str_split_fixed(data$borderid,'_',2)[,1]
data<-arrange(data,borderid,year,ddyad)


#Neighbor scores on latent trait
data$theta_neighbor<-NA
data$theta_sd_neighbor<-NA
data$xi_neighbor<-NA
data$xi_sd_neighbor<-NA
for(ii in 2:nrow(data)){
  data$theta_neighbor[ii]<-ifelse(is.na(data$theta_neighbor[ii]) & data$dyad[ii]==data$dyad[ii-1] & 
                                    data$borderid[ii]==data$borderid[ii-1] & data$year[ii]==data$year[ii-1],
                                  data$theta[ii-1],NA) 
  data$theta_sd_neighbor[ii]<-ifelse(is.na(data$theta_sd_neighbor[ii]) & data$dyad[ii]==data$dyad[ii-1] & 
                                       data$borderid[ii]==data$borderid[ii-1] & data$year[ii]==data$year[ii-1],
                                     data$theta_sd[ii-1],NA) 
  data$xi_neighbor[ii]<-ifelse(is.na(data$xi_neighbor[ii]) & data$dyad[ii]==data$dyad[ii-1] & 
                                 data$borderid[ii]==data$borderid[ii-1] & data$year[ii]==data$year[ii-1],
                               data$xi[ii-1],NA) 
  data$xi_sd_neighbor[ii]<-ifelse(is.na(data$xi_sd_neighbor[ii]) & data$dyad[ii]==data$dyad[ii-1] & 
                                    data$borderid[ii]==data$borderid[ii-1] & data$year[ii]==data$year[ii-1],
                                  data$xi_sd[ii-1],NA)
}
for(ii in 1:nrow(data)-1){
  data$theta_neighbor[ii]<-ifelse(is.na(data$theta_neighbor[ii]) & data$dyad[ii]==data$dyad[ii+1] & 
                                    data$borderid[ii]==data$borderid[ii+1] & data$year[ii]==data$year[ii+1],
                                    data$theta[ii+1],data$theta_neighbor[ii]) 
  data$theta_sd_neighbor[ii]<-ifelse(is.na(data$theta_sd_neighbor[ii]) & data$dyad[ii]==data$dyad[ii+1] & 
                                       data$borderid[ii]==data$borderid[ii+1] & data$year[ii]==data$year[ii+1],
                                       data$theta_sd[ii+1],data$theta_sd_neighbor[ii]) 
  data$xi_neighbor[ii]<-ifelse(is.na(data$xi_neighbor[ii]) & data$dyad[ii]==data$dyad[ii+1] & 
                                 data$borderid[ii]==data$borderid[ii+1] & data$year[ii]==data$year[ii+1],
                                data$xi[ii+1],data$xi_neighbor[ii]) 
  data$xi_sd_neighbor[ii]<-ifelse(is.na(data$xi_sd_neighbor[ii]) & data$dyad[ii]==data$dyad[ii+1] & 
                                    data$borderid[ii]==data$borderid[ii+1] & data$year[ii]==data$year[ii+1],
                                    data$xi_sd[ii+1],data$xi_sd_neighbor[ii])
}


#Dyadic Schengen indicators
data$schengen_pair<-ifelse(data$schengen1==1 & data$schengen2==1,1,0)
data$schengen_border<-ifelse(data$schengen1!=data$schengen2,1,0)
data$schengen_v_nonsch<-ifelse(data$schengen1==1 & data$schengen2==0,1,0)
data$nonsch_v_schengen<-ifelse(data$schengen1==0 & data$schengen2==1,1,0)


#Terrorism (GTD)
#NOTE: START will not allow their data to be made publicly available for replication.
#See the FAQ at https://start.umd.edu/gtd/ The most recent 
#data can be downloaded manually at https://start.umd.edu/gtd/ after users
#create an account and agree to terms and conditions. The specific data
#used in this analysis comes from the July 2018 distribution, named
# globalterrorismdb_0718dist.csv. To fully replicate this script, 
#obtain that file, store it in the regression/gtd/ folder and un-comment
#the following lines of code. Otherwise, the following lines will load
#an already reduced and transformed version of the START data. 


#gtd<-read.csv('regression/gtd/globalterrorismdb_0718dist.csv')
#gtd<-subset(gtd,iyear>=1990)
#gtd$country_txt[gtd$country_txt=="Serbia"]<-"Yugoslavia"
#gtd$country_txt[gtd$country_txt=="Serbia-Montenegro"]<-"Yugoslavia"
#gtd$ccode<-countrycode(gtd$country_txt,origin="country.name",destination="cown")
#gtd$ccode[gtd$country_txt=='West Bank and Gaza Strip']<-991
##generate count indicator
#gtd<- gtd %>% 
#    group_by(ccode,iyear) %>%
#    add_tally() %>% 
#    dplyr::select(ccode,iyear,n) %>% 
#    dplyr::rename(year=iyear,gtd_count=n) %>%
#    arrange(ccode,year) %>%
#    unique()
##Expand out missing years, assigning zeros
#gtd<-gtd %>%
#  group_by(ccode) %>%
#  complete(year=c(1990:1992,1994:2017),fill = list(gtd_count = 0))
##generate a ten-year moving average (1993 is left out entirely)
#gtd <- gtd %>%
#  group_by(ccode) %>%
#  arrange(ccode, year) %>%
#  mutate(gtd_count_10yr_ma = rollmean(x = gtd_count, 10, align = "right", fill = NA))
##lag 1 year for merge
#gtd$year<-gtd$year+1
#save(gtd,file="regression/gtd/gtd_reduced.RData")

load("regression/gtd/gtd_reduced.RData")
data<-merge(data,gtd,by.x=c('state1','year'),by.y=c('ccode','year'),all.x=T,all.y=F)

#Foreign Born Population
migra<-read.csv('regression/migration/table3_reduced.csv')
migra<-migra[,-c(2,4)]
migra<- migra %>% 
  gather(key=year,value=foreign_born_pct,X1990:X2017) 
migra$year<-as.numeric(substring(as.character(migra$year),2))
migra<- migra %>% arrange(code, year)
migra$country<-as.character(migra$country)
migra$country[migra$country=="Serbia"]<-"Yugoslavia"
migra$ccode<-countrycode(migra$country,origin='country.name',destination='cown')
migra$ccode[migra$country=='State of Palestine']<-991
migra$ccode[migra$country=='Gibraltar']<-992
migra<- migra %>% arrange(ccode,year)
migra$chng_fborn_pct<-NA
for(ii in 2:nrow(migra)){
  migra$chng_fborn_pct[ii]<-ifelse(is.na(migra$chng_fborn_pct[ii]) & 
                                   migra$country[ii]==migra$country[ii-1],
                                   migra$foreign_born_pct[ii]-migra$foreign_born_pct[ii-1],
                                   NA)
}
migra$endyr<-NULL
migra$endyr[migra$year==1990]<-1990
migra$endyr[migra$year==1995]<-1999
migra$endyr[migra$year==2000]<-2004
migra$endyr[migra$year==2005]<-2009
migra$endyr[migra$year==2010]<-2014
migra$endyr[migra$year==2015]<-2016
migra$endyr[migra$year==2017]<-2017
migra<- migra %>% dplyr::select(ccode,year,foreign_born_pct,chng_fborn_pct,endyr)
f = function(x) with(x, data.frame(ccode,year,foreign_born_pct,chng_fborn_pct,endyr, 
                                   iyear = seq(year, endyr, by=1))) 
migra <- do.call('rbind', by(migra, 1:nrow(migra), f))
migra[,c('year','endyr')]<-NULL
rownames(migra)<-NULL
colnames(migra)[4]<-"year"
migra$fborn_1990<-ifelse(migra$year==1990,migra$foreign_born_pct,NA)
migra$fborn_2000<-ifelse(migra$year==2000,migra$foreign_born_pct,NA)
migra<- migra %>% 
  group_by(ccode) %>% 
  mutate(fborn_1990 = max(fborn_1990,na.rm=T)) %>% 
  mutate(fborn_2000 = max(fborn_2000,na.rm=T)) %>% 
  mutate(chng_fborn_2000_1990 = fborn_2000 - fborn_1990) %>% 
  ungroup()
migra[,c('fborn_1990','fborn_2000')]<-NULL
data<-merge(data,migra,by.x=c('state1','year'),by.y=c('ccode','year'),all.x=T,all.y=F)


#KOF globalization index
glbz<-read.csv('regression/kof_globalization/kof_globalization.csv',stringsAsFactors = F)
glbz$country[glbz$country=="Serbia"]<-"Yugoslavia"
glbz$ccode<-countrycode(glbz$country,origin='country.name',destination='cown')
glbz$ccode[glbz$code=="PRK"]<-731
glbz$ccode[glbz$country=="West Bank and Gaza"]<-991
#Five year moving average
glbz <- glbz %>%
  dplyr::select(ccode,year,KOFGIdf) %>%
  group_by(ccode) %>%
  arrange(ccode, year) %>%
  mutate(KOFGIdf_5yr_ma = rollmean(x = KOFGIdf, 5, align = "right", fill = NA))
#Five year change
glbz$KOFGIdf_5yr_chng<-NA
for(ii in 6:nrow(glbz)){
  glbz$KOFGIdf_5yr_chng[ii]<-ifelse(is.na(glbz$KOFGIdf_5yr_chng[ii]) & 
                                   glbz$ccode[ii]==glbz$ccode[ii-1] &
                                   glbz$year[ii]-5==glbz$year[ii-5],
                                   glbz$KOFGIdf[ii]-glbz$KOFGIdf[ii-5],
                                   NA)
}
glbz$year<-glbz$year+1
data<-merge(data,glbz,by.x=c('state1','year'),by.y=c('ccode','year'),all.x=T,all.y=F)


#ICOW
icow<-read.csv('regression/icow/ICOWprovyr101.csv')
icow$teriss<-1
icow$year<-icow$year+1
icow<- icow %>% dplyr::select(dyad,teriss,year) %>% unique()
data<-merge(data,icow,by=c('dyad','year'),all.x=T,all.y=F)
data$teriss[is.na(data$teriss) & data$year<=2002]<-0


#Elevation data
#note: computed separately using the 'rgbif' package, which extracts data via Geonames
#commands not reported here because they require a unique username/login ID
cs<-read.csv("regression/elevation/elevation.csv")
data<-merge(data,cs,
             by.x=c('borderid'),
             by.y=c('borderid'),
             all.x=T,all.y=F)
data_lag<-dplyr::select(data,borderid,ddyad,year,xi_lag = xi,xi_neighbor_lag =xi_neighbor,theta_lag = theta, theta_neighbor_lag = theta_neighbor)
data_lag$year<-data_lag$year-1
data<-merge(data,data_lag,by=c('borderid','ddyad','year'),all.x=T,all.y=F)
data$relig_1same_2diff <- ifelse(data$relig_1_shared_i==1 & data$relig_2_shared_i==0,1,0)
data$relig_1diff_2diff<-ifelse(data$relig_1_shared_i==0 & data$relig_2_shared_i==0,1,0)
data$relig_1diff_2same<-ifelse(data$relig_1_shared_i==0 & data$relig_2_shared_i==1,1,0)


#MID Data
mid<-read.csv('regression/mid/dyadic MIDs 3.1.csv')
mid<-subset(mid,year==strtyr & year> 1979 & year<=1999)
mid$dyad<-ifelse(mid$statea<mid$stateb,
                 mid$statea*1000+mid$stateb,
                 mid$stateb*1000+mid$statea)
mid$initiate<-1
#taking unique obs so that every dispute is only represented once
mid<-unique(dplyr::select(mid,dyad,initiate,disno))
mid <- as.data.frame(mid %>%
                       group_by(dyad) %>%
                       mutate(mid_count = sum(initiate,na.rm=T)) %>%
                       dplyr::select(dyad,mid_count) %>%
                       unique()
)
data<-merge(data,mid,by="dyad",all.x=T,all.y=F)
data$mid_count[is.na(data$mid_count) & data$state1<990 & data$state2<990]<-0

#Select most relevant variables for subsequent analyses
data<-dplyr::select(data, borderid,
                    borderid_dir,	
                    dyad,	
                    ddyad,	
                    year,	
                    state1,	
                    state2,	
                    country1,	
                    country2,	
                    long,	
                    lat,
                    gate,	
                    building,	
                    split,	
                    multi,	
                    cp_wall,	
                    pol_orientation,
                    xi,	
                    xi_sd,	
                    theta,	
                    theta_sd,	
                    lngdp1,	
                    lngdp2,	
                    lngdp_diff,	
                    mid_count,	
                    civ2,	
                    cultural_homog,	
                    polity2_1,	
                    polity2_2,	
                    Trading.across.Borders.rank,	
                    pop,	
                    visa_waiver,	
                    relig_1same_2diff,	
                    relig_1diff_2diff,	
                    relig_1diff_2same,
                    schengen1,	
                    schengen2,	
                    theta_neighbor,	
                    theta_sd_neighbor,	
                    xi_neighbor,	
                    xi_sd_neighbor,
                    xi_lag,
                    xi_lag,
                    theta_neighbor_lag,
                    xi_neighbor_lag,
                    schengen_pair,	
                    schengen_border,	
                    schengen_v_nonsch,	
                    nonsch_v_schengen,	
                    gtd_count_10yr_ma,	
                    foreign_born_pct,
                    chng_fborn_pct,	
                    KOFGIdf_5yr_chng,	
                    teriss,	
                    elevation)
data<-arrange(data,borderid_dir,year)
write.csv(data,file="regression/regression_data.csv",na="",row.names=F)

              

