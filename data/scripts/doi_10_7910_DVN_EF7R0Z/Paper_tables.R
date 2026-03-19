# Requires:
#         - full.Rdata
#         - country_aggregated.Rdata
# Produces: Tables 3, 4, and 5 and statistics for tables 1 and 2


# If not installed already
# install.packages("tidyverse","lfe","texreg")

# please set your working directory to the /code folder in the parent directory which also contains the /data and /figures folders
# setwd()

library(tidyverse)
library(lfe)
library(texreg)

#### FULL ####
load("../data/output/full.Rdata")

load("../data/output/country_aggregated.Rdata")

#### PRODUCES INFORMATION FOR TABLE 1 ####

# Number of cell-years
sum(!is.na(full$forest))

# number of forested cells
length(unique(full$FID))

# number of countries in dataset
length(unique(country_full$un))

# number of country-years in dataset
nrow(country_full)

# number of elections
sum(country_full$election_DPI, na.rm = T)


#### PRODUCES INFORMATION FOR TABLE 2

country_full %>% filter(election_DPI==1) %>% group_by(Polity_class,pr) %>% summarise(n=n())

country_full %>% filter(close80==1) %>% group_by(Polity_class,pr) %>% summarise(n=n())

country_full %>% filter(close90==1) %>% group_by(Polity_class,pr) %>% summarise(n=n())



#### PRODUCES TABLE 3 ####
print("democracy")

# include country fe to get number of countries in regression output
m1_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|FID + un + year|0|un + year, data=full)
m2_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|0|0|un + year, data=full)
m3_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|un + year|0|un + year, data=country_full)
m4_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|0|0|un + year, data=country_full)


# generate paper table
texreg(list(m1_df,
               m2_df,
               m3_df,
               m4_df), 
          custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth","Democracy","Constant"),
          custom.model.names = c("Cell","Cell","National","National"),
          reorder.coef = c(5,1,2,3,4,6), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on democracy",
       label = "table1.1",
       float.pos = "h",
       custom.gof.rows = list("Country + Year Fixed Effects" = c("Yes", "No", "Yes","No")))

screenreg(list(m1_df,
            m2_df,
            m3_df,
            m4_df), 
       custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth","Democracy","Constant"),
       custom.model.names = c("Cell","Cell","National","National"),
       reorder.coef = c(5,1,2,3,4,6), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       custom.gof.rows = list("Country + Year Fixed Effects" = c("Yes", "No", "Yes","No")))

rm(full_nofe_m1,full_Nat_m1,full_Nat_nofe_m1)

#### PRODUCES TABLE 4 ####
print("election")
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
       label = "table1.1",
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
       with a margin of victory less than 10 points. Election year is interacted with government type with Anocracy as the base case.",
          label = "table:2.0",
          float.pos = "h",
          custom.gof.rows = list("Country + Year Fixed Effects" = c(rep("Yes",6))))


#### PRODUCES TABLE 5 ####
print("margin")
full_margin_all <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|FID + un |0|un + year, data=full)
full_margin_Nat_all <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|un|0|un + year, data=country_full)

screenreg(list(full_margin_all,
            full_margin_Nat_all),
          custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth", "Competitiveness","Autocracy",
                                "Democracy","Comp:Autocracy","Comp:Democracy"),
          custom.model.names = c("Cell","National"),
          reorder.coef = c(5,8,9,6,7,1:4), booktabs = T,
          include.rsquared = F, include.adjrs = TRUE,
          caption = "Regressions of forest change on electoral competitiveness. Competitiveness is interacted with government type with Anocracy as the base case.",
          label = "table:4",
          float.pos = "h",
          custom.gof.rows = list("Country Fixed Effects" = c(rep("Yes",2))))

texreg(list(full_margin_all,
               full_margin_Nat_all),
          custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth", "Competitiveness","Autocracy",
                                "Democracy","Comp:Autocracy","Comp:Democracy"),
          custom.model.names = c("Cell","National"),
          reorder.coef = c(5,8,9,6,7,1:4), booktabs = T,
          include.rsquared = F, include.adjrs = TRUE,
          caption = "Regressions of forest change on electoral competitiveness. Competitiveness is interacted with government type with Anocracy as the base case.",
          label = "table:4",
          float.pos = "h",
          custom.gof.rows = list("Country Fixed Effects" = c(rep("Yes",2))))

## Code for Brazil example in second to last paragraph of Section 4
# Brazil average rate
average_rate <- country_full %>% filter(un == 76) %>% # Brazil is UNSD Code 76
    summarise(mean_deforestation = mean(forest.diff,na.rm=T)) %>% 
    pull(mean_deforestation)

# Global average effect
margin_coef <- full_margin_all$coefficients[5,]

# 2 pp change compared to overall rate
margin_coef*2/average_rate
