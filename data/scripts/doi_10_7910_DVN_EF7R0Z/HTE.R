# Produces Supporting Information tables 1-3 and table 11 which show heterogeneous treatment effects with 
# agriculture and electoral system (1-3) and 
# the regressions of close elections with lead and lag years included
# Requires 
#    - full.Rdata
#    - country_aggregated.Rdata
# Produces
#    - SI tables 1-4, 11

# If not previously installed
# install.packages("tidyverse","lfe","texreg")

# please set your working directory to the /code folder in the parent directory which also contains the /data and /figures folders
# setwd()

library(tidyverse)
library(lfe)
library(texreg)

#### FULL ####

load("../data/output/full.Rdata")

load("../data/output/country_aggregated.Rdata")


#### PRODUCES APPENDIX TABLE 1 ####

m1_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|FID +un + year|0|un + year, data=full)
m2_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ag.emp.l*democracy_BX|FID +un + year|0|un + year, data=full)
m3_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + Ag.pct.l*democracy_BX|FID +un + year|0|un + year, data=full)
m4_df <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + pr*democracy_BX|FID +un + year|0|un + year, data=full)

texreg(list(m1_df,
            m2_df,
            m3_df,
            m4_df),
       custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth","Democracy","Ag employment","Ag emp:Democracy","Ag pct GDP","Ag pct GDP:Democracy",
                             "PR","PR:Democracy"),
       custom.model.names = c("None","Ag Employment","Ag Pct Economy","Electoral System"),
       reorder.coef = c(5:11,1,2,3,4), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on democracy (Model 1) interacted with percent of workforce employed in agriculture (Model 2), 
       Agriculture as a percent of GDP (Model 3), and Electoral system (Model 4). Note that it is possible though rare for nondemocracies to 
       have a ``voting system.''",
       label = "table:1.1",
       float.pos = "h",
       custom.gof.rows = list("Country + Year Fixed Effects" = c("Yes", "Yes", "Yes","Yes")))
rm(m1_df,
   m2_df,
   m3_df,
   m4_df)

#### PRODUCES SUPPORTING INFORMATION TABLE 2 and 3 ####

full_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + Polity_class + election_DPI + election_DPI:Polity_class|FID + un + year|0|un + year, data=full)
full_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|FID + un + year|0|un + year, data=full)
full_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|FID + un + year|0|un + year, data=full)
full_m4 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI*Ag.emp.l|FID + un + year|0|un + year, data=full)
full_m5 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80*Ag.emp.l|FID + un + year|0|un + year, data=full)
full_m6 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90*Ag.emp.l|FID + un + year|0|un + year, data=full)
full_m7 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI*Ag.pct.l|FID + un + year|0|un + year, data=full)
full_m8 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80*Ag.pct.l|FID + un + year|0|un + year, data=full)
full_m9 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90*Ag.pct.l|FID + un + year|0|un + year, data=full)
full_m10 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI*pr|FID + un + year|0|un + year, data=full)
full_m11 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80*pr|FID + un + year|0|un + year, data=full)
full_m12 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90*pr|FID + un + year|0|un + year, data=full)

texreg(list(full_m1,
               full_m2,
               full_m3,
               full_m10,
               full_m11,
               full_m12),
          custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth", "Autocracy","Democracy",
                                "Election Year","Election:Autocracy","Election:Democracy",
                                "Margin < 20", "Margin<20:Autocracy","Margin<20:Democracy",
                                "Margin < 10","Margin<10:Autocracy","Margin<10:Democracy",
                                "PR","Election:PR","Margin<20:PR","Margin<10:PR"),
          custom.model.names = c("Polity Class","Polity Class","Polity Class","Electoral System","Electoral System","Electoral System"),
          reorder.coef = c(7,10,13,8,11,14,9,12,15,17,18,19,16,6,5,1:4), booktabs = T,
          include.rsquared = F, include.adjrs = TRUE,
          caption = "Regressions of forest change on election year. Election years are subset by how competitive they were: 
          column (1) and (4) are all election years, (2) and (5) are years with a margin of victory less than 20 points, (3) and (6) are years 
          with a margin of victory less than 10 points. Election year is interacted with government type with Anocracy as the base case in models (1)-(3)
          and with electoral system in (4)-(6).",
          label = "table:1.2",
          float.pos = "h",
          custom.gof.rows = list("Country + Year Fixed Effects" = c(rep("Yes",6))))

texreg(list(full_m4,
               full_m5,
               full_m6,
               full_m7,
               full_m8,
               full_m9),
          custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth",
                                "Election Year", "Ag employment", "Election:Ag employment",
                                "Margin < 20", "Margin<20:Ag employment",
                                "Margin < 10","Margin<10:Ag employment",
                                "Ag pct of economy","Election:Ag pct of economy",
                                "Margin<20 : Ag pct of economy","Margin<10 : Ag pct of economy"),
          custom.model.names = c("Ag Employment","Ag Employment","Ag Employment","Ag pct Econ","Ag pct Econ","Ag pct Econ"),
          reorder.coef = c(5,8,10,6,7,9,11,12,13:15,1:4), booktabs = T,
          include.rsquared = F, include.adjrs = TRUE,
          caption = "Regressions of forest change on election year. Election years are subset by how competitive they were: 
          column (1) and (4) are all election years, (2) and (5) are years with a margin of victory less than 20 points, (3) and (6) are years 
          with a margin of victory less than 10 points. Election year is interacted with percent of labor force employed in agriculture in models (1)-(3)
          and with agriculture as a percent of the economy in (4)-(6).",
          label = "table:1.3",
          float.pos = "h",
          custom.gof.rows = list("Country + Year Fixed Effects" = c(rep("Yes",6))))
rm(full_m1,
   full_m2,
   full_m3,
   full_m10,
   full_m11,
   full_m12,
   full_m4,
   full_m5,
   full_m6,
   full_m7,
   full_m8,
   full_m9)

#### PRODUCES SUPPORTING INFORMATION TABLE 4 ####

margin_1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Polity_class|FID + un|0|un + year, data=full)
margin_2 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Ag.emp.l|FID + un|0|un + year, data=full)
margin_3 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Ag.pct.l|FID + un|0|un + year, data=full)
margin_4 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*pr|FID + un|0|un + year, data=full)

texreg(list(margin_1,
               margin_2,
               margin_3,
               margin_4),
          custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth", "Competitiveness","Autocracy",
                                "Democracy","Comp:Autocracy","Comp:Democracy",
                                "Ag employment","Comp:Ag employment",
                                "Ag pct of economy","Comp:Ag pct of economy",
                                "PR","Comp:PR"),
          reorder.coef = c(5,8,9:15,6,7,1:4), booktabs = T,
          include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on electoral competitiveness. Competitiveness is interacted with government type with Anocracy as the base case in (1),
       percent of labor force employed in agriculture in (2), agriculture as a percent of the economy in (3), and electoral system in (4).",
       label = "table:1.4",
          float.pos = "h",
          digits = 3,
          custom.gof.rows = list("Country Fixed Effects" = c(rep("Yes",4))))
rm(margin_1,margin_2,margin_3,margin_4)


#### PRODUCES SUPPORTING INFORMATION TABLE 11 ####

full<-full %>% 
   mutate(close90.lag1=dplyr::lag(close90)) %>%
   mutate(close90.lag2=dplyr::lag(close90.lag1)) %>%
   mutate(close90.lead1=dplyr::lead(close90)) %>%
   mutate(close90.lead2=dplyr::lead(close90.lead1))

country_full<-country_full %>% 
   mutate(close90.lag1=dplyr::lag(close90)) %>%
   mutate(close90.lag2=dplyr::lag(close90.lag1)) %>%
   mutate(close90.lead1=dplyr::lead(close90)) %>%
   mutate(close90.lead2=dplyr::lead(close90.lead1))
full_m0 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90*Polity_class|FID + un + year|0|un + year, data=full)
full_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90*Polity_class + close90.lag1*Polity_class + close90.lead1*Polity_class|FID + un + year|0|un + year, data=full)
full_Nat_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90*Polity_class|un + year|0|un + year, data=country_full)
full_Nat_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90*Polity_class + close90.lag1*Polity_class + close90.lead1*Polity_class|un + year|0|un + year, data=country_full)

texreg(list(full_m0,
            full_m1,
            full_Nat_m2,
            full_Nat_m3), 
       custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth","Close Election","Autocracy",
                             "Democracy","Close:Autocracy","Close:Democracy","Close Lag","Close Lead",
                             "Close Lag:Autocracy","Close Lag:Democracy","Close Lead:Autocracy","Close Lead:Democracy"),
       custom.model.names = c("Cell","Cell, lead lag","National","National lead lag"),
       reorder.coef = c(5,10,11,9,13,15,1,2,3,4,7,6,8,12,14), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on election",
       label = "table:6.1",
       float.pos = "h",
       custom.gof.rows = list("Unit + Year Fixed Effects" = c("Yes", "Yes", "Yes","Yes")))
