# Produces SI tables 12-14, results with interacted fixed effects on previous level of forest cover and 
# previous level of neighbors forest cover
#         Requires:
#             - full.Rdata
#             - country_aggregated.Rdata
#         Produces: 
#             - SI tables 12-14

# if not already installed
# install.packages("tidyverse","lfe","texreg")

# please set your working directory to the /code folder in the parent directory which also contains the /data and /figures folders
# setwd()

library(tidyverse)
library(lfe)
library(texreg)

load("../data/output/full.Rdata")

### saturate forest and nearest neighbor forest fixed effects and their interaction
full <- full %>% mutate(forest_cuts = cut(forest.l, breaks = seq(0,100,by=10)))
summary(full$forest_cuts)
full <- full %>% mutate(nn_forest_cuts = cut(nn_forest.l, breaks = seq(0,100,by=10)))
summary(full$nn_forest_cuts)
full <- full %>% mutate(saturated_FEs = interaction(forest_cuts, nn_forest_cuts))


load("../data/output/country_aggregated.Rdata")

#### PRODUCES SUPPORTING INFORMATION TABLE 12 ####
print("democracy")
m1_df <- felm(forest.diff ~ PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|FID + un + year + saturated_FEs|0|un + year, data=full)
m2_df <- felm(forest.diff ~ PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|saturated_FEs|0|un + year, data=full)
m3_df <- felm(forest.diff ~ PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|un + year|0|un + year, data=country_full)
m4_df <- felm(forest.diff ~ PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|0|0|un + year, data=country_full)

texreg(list(m1_df,
            m2_df,
            m3_df,
            m4_df), 
       custom.coef.names = c("PCGDP","$\\Delta$ PCGDP","Pop Growth","Democracy","Constant"),
       custom.model.names = c("Cell","Cell","National","National"),
       reorder.coef = c(4,1,2,3,5), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on democracy. Fixed effects for each ten percent of lagged forest cover and each ten percent of 
       average neighbor forest cover and their interaction.",
       label = "table:6.1",
       float.pos = "h",
       custom.gof.rows = list("Country + Year Fixed Effects" = c("Yes", "No", "Yes","No")))

screenreg(list(m1_df,
               m2_df,
               m3_df,
               m4_df), 
          custom.coef.names = c("PCGDP","$\\Delta$ PCGDP","Pop Growth","Democracy","Constant"),
          custom.model.names = c("Cell","Cell","National","National"),
          reorder.coef = c(4,1,2,3,5), booktabs = T,
          include.rsquared = F, include.adjrs = TRUE,
          custom.gof.rows = list("Country + Year Fixed Effects" = c("Yes", "No", "Yes","No")))

#### PRODUCES SUPPORTING INFORMATION TABLE 13 ####
print("election")
full_m1 <- felm(forest.diff ~PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|FID + year + saturated_FEs|0|un + year, data=full)
full_m2 <- felm(forest.diff ~PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|FID + year + saturated_FEs|0|un + year, data=full)
full_m3 <- felm(forest.diff ~PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|FID + year + saturated_FEs|0|un + year, data=full)
full_Nat_m1 <- felm(forest.diff ~PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|un + year|0|un + year, data=country_full)
full_Nat_m2 <- felm(forest.diff ~PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|un + year|0|un + year, data=country_full)
full_Nat_m3 <- felm(forest.diff ~PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|un + year|0|un + year, data=country_full)

screenreg(list(full_m1,
               full_m2,
               full_m3,
               full_Nat_m1,
               full_Nat_m2,
               full_Nat_m3), 
          custom.coef.names = c("PCGDP","$\\Delta$ PCGDP","Pop Growth", "Election Year",
                                "Autocracy","Democracy","Election:Autocracy","Election:Democracy",
                                "Margin < 20", "Margin<20:Autocracy","Margin<20:Democracy",
                                "Margin < 10","Margin<10:Autocracy","Margin<10:Democracy"),
          custom.model.names = c("Cell","Cell","Cell","National","National","National"),
          reorder.coef = c(4,9,12,7,10,13,8,11,14,6,5,1:3), booktabs = T,
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
       custom.coef.names = c("PCGDP","$\\Delta$ PCGDP","Pop Growth", "Election Year",
                             "Autocracy","Democracy","Election:Autocracy","Election:Democracy",
                             "Margin < 20", "Margin<20:Autocracy","Margin<20:Democracy",
                             "Margin < 10","Margin<10:Autocracy","Margin<10:Democracy"),
       custom.model.names = c("Cell","Cell","Cell","National","National","National"),
       reorder.coef = c(4,9,12,7,10,13,8,11,14,6,5,1:3), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on election year. Election years are subset by how competitive they were: 
       columns 1 is all election years, 2 is years with a margin of victory less than 20 points, 3 is years 
       with a margin of victory less than 10 points. Election year is interacted with government type with Anocracy as the base case.
       Fixed effects for each ten percent of lagged forest cover and each ten percent of 
       average neighbor forest cover and their interaction.",
       label = "table:6.2",
       float.pos = "h",
       custom.gof.rows = list("Country + Year Fixed Effects" = c(rep("Yes",6))))

#### PRODUCES SUPPORTING INFORMATION TABLE 14 ####
print("margin")
full_margin_all <- felm(forest.diff ~ PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|FID|0|un + year, data=full)
full_margin_Nat_all <- felm(forest.diff ~ PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|un|0|un + year, data=country_full)

screenreg(list(full_margin_all,
               full_margin_Nat_all),
          custom.coef.names = c("PCGDP","$\\Delta$ PCGDP","Pop Growth", "Competitiveness","Autocracy",
                                "Democracy","Comp:Autocracy","Comp:Democracy"),
          custom.model.names = c("Cell","National"),
          reorder.coef = c(4,7,8,5,6,1:3), booktabs = T,
          include.rsquared = F, include.adjrs = TRUE,
          caption = "Regressions of forest change on electoral competitiveness. Competitiveness is interacted with government type with Anocracy as the base case.
          Fixed effects for each ten percent of lagged forest cover and each ten percent of 
       average neighbor forest cover and their interaction.",
          label = "table:6.3",
          float.pos = "h",
          custom.gof.rows = list("Country Fixed Effects" = c(rep("Yes",2))))

texreg(list(full_margin_all,
            full_margin_Nat_all),
       custom.coef.names = c("PCGDP","$\\Delta$ PCGDP","Pop Growth", "Competitiveness","Autocracy",
                             "Democracy","Comp:Autocracy","Comp:Democracy"),
       custom.model.names = c("Cell","National"),
       reorder.coef = c(4,7,8,5,6,1:3), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on electoral competitiveness. Competitiveness is interacted with government type with Anocracy as the base case.
       Fixed effects for each ten percent of lagged forest cover and each ten percent of 
       average neighbor forest cover and their interaction.",
       label = "table:6.3",
       float.pos = "h",
       custom.gof.rows = list("Country Fixed Effects" = c(rep("Yes",2))))

