# Produces SI tables 8-11, results using different measures of electoral competitiveness
# Requires:
#    - full.Rdata
#    - country_aggregated.Rdata
# Produces:
#    - SI tables 8-11

# if not already installed
# install.packages("tidyverse","lfe","texreg")

# please set your working directory to the /code folder in the parent directory which also contains the /data and /figures folders
# setwd()

library(tidyverse)
library(lfe)
library(texreg)

load("../data/output/full.Rdata")
load("../data/output/country_aggregated.Rdata")

#### Reproduce results from paper tables 4 and 5 for reference
full$margin.norm<-100-abs(full$votediff)
full$close80<-as.numeric(full$margin.norm>80)
full$close80[is.na(full$margin.norm)==1]<-0
full$close80[full$margin.norm<=80]<-NA
full$close80[is.na(full$election_DPI)]<-NA

# get elections where the margin is less than 10 compared to non-election years
full$close90<-as.numeric(full$margin.norm>90)
full$close90[is.na(full$margin.norm)==1]<-0
full$close90[full$margin.norm<=90]<-NA
full$close90[is.na(full$election_DPI)]<-NA

# get elections where the margin is less than 20 compared to non-election years
country_full$margin.norm<-100-abs(country_full$votediff)
country_full$close80<-as.numeric(country_full$margin.norm>80)
country_full$close80[is.na(country_full$margin.norm)==1]<-0
country_full$close80[country_full$margin.norm<=80]<-NA
country_full$close80[is.na(country_full$election_DPI)]<-NA

# get elections where the margin is less than 10 compared to non-election years
country_full$close90<-as.numeric(country_full$margin.norm>90)
country_full$close90[is.na(country_full$margin.norm)==1]<-0
country_full$close90[country_full$margin.norm<=90]<-NA
country_full$close90[is.na(country_full$election_DPI)]<-NA


#### Election ####
full_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|FID + un + year|0|un + year, data=full)
full_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|FID + un + year|0|un + year, data=full)
full_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|FID + un + year|0|un + year, data=full)
full_Nat_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|un + year|0|un + year, data=country_full)
full_Nat_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|un + year|0|un + year, data=country_full)
full_Nat_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|un + year|0|un + year, data=country_full)

screenreg(list(full_m1,
               full_m2,
               full_m3,
               full_Nat_m1,
               full_Nat_m2,
               full_Nat_m3), 
          custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth",
                                "Election Year", "Autocracy","Democracy",
                                "Election:Autocracy","Election:Democracy",
                                "Margin < 20", "Margin<20:Autocracy","Margin<20:Democracy",
                                "Margin < 10","Margin<10:Autocracy","Margin<10:Democracy"),
          custom.model.names = c("Cell","Cell","Cell","National","National","National"),
          reorder.coef = c(5,10,13,8,11,14,9,12,15,7,6,1:4), booktabs = T,
          include.rsquared = F, include.adjrs = TRUE,
          caption = "Regressions of forest change on democracy",
          label = "table:4.5",
          float.pos = "h",
          custom.gof.rows = list("Country + Year Fixed Effects" = c(rep("Yes",6))))

texreg(list(full_m1,
            full_m2,
            full_m3,
            full_Nat_m1,
            full_Nat_m2,
            full_Nat_m3), 
       custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth",
                             "Election Year", "Autocracy","Democracy",
                             "Election:Autocracy","Election:Democracy",
                             "Margin < 20", "Margin<20:Autocracy","Margin<20:Democracy",
                             "Margin < 10","Margin<10:Autocracy","Margin<10:Democracy"),
       custom.model.names = c("Cell","Cell","Cell","National","National","National"),
       reorder.coef = c(5,10,13,8,11,14,9,12,15,7,6,1:4), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on election year. Election years are subset by how competitive they were: 
       columns 1 is all election years, 2 is years with a margin of victory less than 20 points, 3 is years 
       with a margin of victory less than 10 points. Election year is interacted with government type with Anocracy as the base case.
       Margin of victory is calculated as the percent of seats won by the largest party minus the percent of seats won by the second
       largest party. This is the measure used in the main text.",
       label = "table:4.5",
       float.pos = "h",
       custom.gof.rows = list("Country + Year Fixed Effects" = c(rep("Yes",6))))

#### Margin ####
print("margin")
full_margin_all <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|FID|0|un + year, data=full)
full_margin_Nat_all <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|un|0|un + year, data=country_full)

texreg(list(full_margin_all,
            full_margin_Nat_all),
       custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth", "Competitiveness","Autocracy",
                             "Democracy","Comp:Autocracy","Comp:Democracy"),
       custom.model.names = c("Cell","National"),
       reorder.coef = c(5,8,9,6,7,1:4), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on electoral competitiveness. Competitiveness is interacted with government type with Anocracy as the base case.
       Margin of victory is calculated as the percent of seats won by the largest party minus the percent of seats won by the second
       largest party. This is the measure used in the main text.",
       label = "table:4.6",
       float.pos = "h",
       custom.gof.rows = list("Country Fixed Effects" = c(rep("Yes",2))))


#### MARGIN OF VICTORY AS 100-abs(INCUMBENT COALITION VOTE MARGIN) ####
full$margin.norm<-100-abs(full$margin)
full$close80<-as.numeric(full$margin.norm>80)
full$close80[is.na(full$margin.norm)==1]<-0
full$close80[full$margin.norm<=80]<-NA
full$close80[is.na(full$election_DPI)]<-NA

# get elections where the margin is less than 10 compared to non-election years
full$close90<-as.numeric(full$margin.norm>90)
full$close90[is.na(full$margin.norm)==1]<-0
full$close90[full$margin.norm<=90]<-NA
full$close90[is.na(full$election_DPI)]<-NA

country_full$margin.norm<-100-abs(country_full$margin)
country_full$close80<-as.numeric(country_full$margin.norm>80)
country_full$close80[is.na(country_full$margin.norm)==1]<-0
country_full$close80[country_full$margin.norm<=80]<-NA
country_full$close80[is.na(country_full$election_DPI)]<-NA

# get elections where the margin is less than 10 compared to non-election years
country_full$close90<-as.numeric(country_full$margin.norm>90)
country_full$close90[is.na(country_full$margin.norm)==1]<-0
country_full$close90[country_full$margin.norm<=90]<-NA
country_full$close90[is.na(country_full$election_DPI)]<-NA

#### PRODUCES APPENDIX TABLE 8 ####
full_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|FID + un + year|0|un + year, data=full)
full_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|FID + un + year|0|un + year, data=full)
full_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|FID + un + year|0|un + year, data=full)
full_Nat_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|un + year|0|un + year, data=country_full)
full_Nat_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|un + year|0|un + year, data=country_full)
full_Nat_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|un + year|0|un + year, data=country_full)


screenreg(list(full_m1,
               full_m2,
               full_m3,
               full_Nat_m1,
               full_Nat_m2,
               full_Nat_m3), 
          custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth",
                                "Election Year", "Autocracy","Democracy",
                                "Election:Autocracy","Election:Democracy",
                                "Margin < 20", "Margin<20:Autocracy","Margin<20:Democracy",
                                "Margin < 10","Margin<10:Autocracy","Margin<10:Democracy"),
          custom.model.names = c("Cell","Cell","Cell","National","National","National"),
          reorder.coef = c(5,10,13,8,11,14,9,12,15,7,6,1:4), booktabs = T,
          include.rsquared = F, include.adjrs = TRUE,
          caption = "Regressions of forest change on democracy",
          label = "table:4.1",
          float.pos = "h",
          custom.gof.rows = list("Country + Year Fixed Effects" = c(rep("Yes",6))))

texreg(list(full_m1,
            full_m2,
            full_m3,
            full_Nat_m1,
            full_Nat_m2,
            full_Nat_m3), 
       custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth",
                             "Election Year", "Autocracy","Democracy",
                             "Election:Autocracy","Election:Democracy",
                             "Margin < 20", "Margin<20:Autocracy","Margin<20:Democracy",
                             "Margin < 10","Margin<10:Autocracy","Margin<10:Democracy"),
       custom.model.names = c("Cell","Cell","Cell","National","National","National"),
       reorder.coef = c(5,10,13,8,11,14,9,12,15,7,6,1:4), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on election year. Election years are subset by how competitive they were: 
       columns 1 is all election years, 2 is years with a margin of victory less than 20 points, 3 is years 
       with a margin of victory less than 10 points. Election year is interacted with government type with Anocracy as the base case.
       Margin of victory is calculated as the absolute value of percent of votes for the incumbent coalition minus the percent of votes of the 
       opposition coalition.",
       label = "table:4.1",
       float.pos = "h",
       custom.gof.rows = list("Country + Year Fixed Effects" = c(rep("Yes",6))))

#### PRODUCES APPENDIX TABLE 9 ####
print("margin")
full_margin_all <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|FID|0|un + year, data=full)
full_margin_Nat_all <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|un|0|un + year, data=country_full)

texreg(list(full_margin_all,
            full_margin_Nat_all),
       custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth", "Competitiveness","Autocracy",
                             "Democracy","Comp:Autocracy","Comp:Democracy"),
       custom.model.names = c("Cell","National"),
       reorder.coef = c(5,8,9,6,7,1:4), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on electoral competitiveness. Competitiveness is interacted with government type with Anocracy as the base case.
       Margin of victory is calculated as the absolute value of percent of votes for the incumbent coalition minus the percent of votes of the 
       opposition coalition.",
       label = "table:4.2",
       float.pos = "h",
       custom.gof.rows = list("Country Fixed Effects" = c(rep("Yes",2))))

rm(full_m1,full_m2,full_m3,full_margin_all)

#### MARGIN OF VICTORY AS 100-abs(50-(INCUMBENT COALITION SEAT SHARE)) ####
# get elections where the margin is less than 20 compared to non-election years
full$margin.norm<-100-abs(50-full$maj*100)*2
full$close80<-as.numeric(full$margin.norm>80)
full$close80[is.na(full$margin.norm)==1]<-0
full$close80[full$margin.norm<=80]<-NA
full$close80[is.na(full$election_DPI)]<-NA

# get elections where the margin is less than 10 compared to non-election years
full$close90<-as.numeric(full$margin.norm>90)
full$close90[is.na(full$margin.norm)==1]<-0
full$close90[full$margin.norm<=90]<-NA
full$close90[is.na(full$election_DPI)]<-NA

# get elections where the margin is less than 20 compared to non-election years
country_full$margin.norm<-100-abs(50-country_full$maj*100)*2
country_full$close80<-as.numeric(country_full$margin.norm>80)
country_full$close80[is.na(country_full$margin.norm)==1]<-0
country_full$close80[country_full$margin.norm<=80]<-NA
country_full$close80[is.na(country_full$election_DPI)]<-NA

# get elections where the margin is less than 10 compared to non-election years
country_full$close90<-as.numeric(country_full$margin.norm>90)
country_full$close90[is.na(country_full$margin.norm)==1]<-0
country_full$close90[country_full$margin.norm<=90]<-NA
country_full$close90[is.na(country_full$election_DPI)]<-NA

#### PRODUCES APPENDIX TABLE 10 ####
full_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|FID + un + year|0|un + year, data=full)
full_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|FID + un + year|0|un + year, data=full)
full_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|FID + un + year|0|un + year, data=full)
full_Nat_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|un + year|0|un + year, data=country_full)
full_Nat_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|un + year|0|un + year, data=country_full)
full_Nat_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|un + year|0|un + year, data=country_full)

screenreg(list(full_m1,
               full_m2,
               full_m3,
               full_Nat_m1,
               full_Nat_m2,
               full_Nat_m3), 
          custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth",
                                "Election Year", "Autocracy","Democracy",
                                "Election:Autocracy","Election:Democracy",
                                "Margin < 20", "Margin<20:Autocracy","Margin<20:Democracy",
                                "Margin < 10","Margin<10:Autocracy","Margin<10:Democracy"),
          custom.model.names = c("Cell","Cell","Cell","National","National","National"),
          reorder.coef = c(5,10,13,8,11,14,9,12,15,7,6,1:4), booktabs = T,
          include.rsquared = F, include.adjrs = TRUE,
          caption = "Regressions of forest change on democracy",
          label = "table:4.3",
          float.pos = "h",
          custom.gof.rows = list("Country + Year Fixed Effects" = c(rep("Yes",6))))

texreg(list(full_m1,
            full_m2,
            full_m3,
            full_Nat_m1,
            full_Nat_m2,
            full_Nat_m3), 
       custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth",
                             "Election Year", "Autocracy","Democracy",
                             "Election:Autocracy","Election:Democracy",
                             "Margin < 20", "Margin<20:Autocracy","Margin<20:Democracy",
                             "Margin < 10","Margin<10:Autocracy","Margin<10:Democracy"),
       custom.model.names = c("Cell","Cell","Cell","National","National","National"),
       reorder.coef = c(5,10,13,8,11,14,9,12,15,7,6,1:4), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on election year. Election years are subset by how competitive they were: 
       columns 1 is all election years, 2 is years with a margin of victory less than 20 points, 3 is years 
       with a margin of victory less than 10 points. Election year is interacted with government type with Anocracy as the base case.
       Margin of victory is calculated as the percentage of seats won by the victorious coalition minus 50.",
       label = "table:4.3",
       float.pos = "h",
       custom.gof.rows = list("Country + Year Fixed Effects" = c(rep("Yes",6))))


#### PRODUCES APPENDIX TABLE 11 ####
full_margin_all <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|FID|0|un + year, data=full)
full_margin_Nat_all <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|un|0|un + year, data=country_full)

texreg(list(full_margin_all,
            full_margin_Nat_all),
       custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth", "Competitiveness","Autocracy",
                             "Democracy","Comp:Autocracy","Comp:Democracy"),
       custom.model.names = c("Cell","National"),
       reorder.coef = c(5,8,9,6,7,1:4), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on electoral competitiveness. Competitiveness is interacted with government type with Anocracy as the base case.
       Margin of victory is calculated as the percentage of seats won by the victorious coalition minus 50.",
       label = "table:4.4",
       float.pos = "h",
       custom.gof.rows = list("Country Fixed Effects" = c(rep("Yes",2))))

