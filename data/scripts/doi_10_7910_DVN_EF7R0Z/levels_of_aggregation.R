# Produces tables 5-7, results with different levels of aggregation
# Requires:
#         - full.Rdata
#         - full_upsampled_10.Rdata
#         - GID_1_aggregated.Rdata
#         - GID_2_aggregated.Rdata
#         - country_aggregated.Rdata
# Produces:
#         - SI tables 5-7



# if not already installed 
# install.packages("tidyverse","lfe","texreg")

# please set your working directory to the /code folder in the parent directory which also contains the /data and /figures folders
# setwd()

library(tidyverse)
library(lfe)
library(texreg)


#### Load Full Data ####
# load 0.5dd data
load("../data/output/full_upsampled_10.Rdata")

full_up<-full

# load 0.05dd data
load("../data/output/full.Rdata")

# load level 1 aggregated data
load("../data/output/GID_1_aggregated.Rdata")

# load level 2 aggregated data
load("../data/output/GID_2_aggregated.Rdata")

# load country aggregated data
load("../data/output/country_aggregated.Rdata")

#### GENERATES SUPPORTING INFORMATION TABLE 5 ####
full_m1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|FID + year|0|un + year, data=full)
full_up_m1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|FID + year|0|un + year, data=full_up)
full_L2_m1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|GID_2 + year|0|un + year, data=GID_2_full)
full_L1_m1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|GID_1 + year|0|un + year, data=GID_1_full)
full_Nat_m1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + democracy_BX|un + year|0|un + year, data=country_full)

texreg(list(full_m1,
            full_up_m1,
            full_L2_m1,
            full_L1_m1,
            full_Nat_m1), 
       custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth","Democracy"),
       custom.model.names = c("0.05dd","0.5dd","L2","L1","National"),
       reorder.coef = c(5,1:4), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Democracy results across levels of aggregation. 0.05 decimal degree is the cell level discussed in the paper, 0.5 dd is aggregated to cells 100 times as large,
       L2 is level two administrative units, L1 is level 1 administrative units, National is national borders as discussed in the paper.",
       label = "table:2.1",
       float.pos = "h",
       custom.gof.rows = list("Country + Year Fixed Effects" = c(rep("Yes",5))))



#### GENERATES SUPPORTING INFORMATION TABLE 6 ####
full_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|FID + year|0|un + year, data=full)
full_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|FID + year|0|un + year, data=full)
full_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|FID + year|0|un + year, data=full)
full_up_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|FID + year|0|un + year, data=full_up)
full_up_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|FID + year|0|un + year, data=full_up)
full_up_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|FID + year|0|un + year, data=full_up)
full_L2_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|GID_2 + year|0|un + year, data=GID_2_full)
full_L2_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|GID_2 + year|0|un + year, data=GID_2_full)
full_L2_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|GID_2 + year|0|un + year, data=GID_2_full)
full_L1_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|GID_1 + year|0|un + year, data=GID_1_full)
full_L1_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|GID_1 + year|0|un + year, data=GID_1_full)
full_L1_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|GID_1 + year|0|un + year, data=GID_1_full)
full_Nat_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|un + year|0|un + year, data=country_full)
full_Nat_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|un + year|0|un + year, data=country_full)
full_Nat_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|un + year|0|un + year, data=country_full)

texreg(list(full_m1,
            full_m2,
            full_m3,
            full_up_m1,
            full_up_m2,
            full_up_m3,
            full_L2_m1,
            full_L2_m2,
            full_L2_m3,
            full_L1_m1,
            full_L1_m2,
            full_L1_m3,
            full_Nat_m1,
            full_Nat_m2,
            full_Nat_m3), 
       custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth",
                             "Election Year", "Autocracy","Democracy",
                             "Election:Autocracy","Election:Democracy",
                             "Margin < 20", "Margin<20:Autocracy","Margin<20:Democracy",
                             "Margin < 10","Margin<10:Autocracy","Margin<10:Democracy"),
       custom.model.names = c("Cell","Cell","Cell","0.5dd","0.5dd","0.5dd","L2","L2","L2","L1","L1","L1","National","National","National"),
       reorder.coef = c(5,10,13,8,11,14,9,12,15,7,6,1:4), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on election year. 0.05 decimal degree is the cell level discussed in the paper, 0.5 dd is aggregated to cells 100 times as large,
       L2 is level two administrative units, L1 is level 1 administrative units, National is national borders as discussed in the paper.",
       label = "table:2.2",
       float.pos = "h",
       custom.gof.rows = list("Country + Year Fixed Effects" = c(rep("Yes",15))))



#### GENERATES SUPPORTING INFORMATION TABLE 7 ####

full_m1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|FID|0|un + year, data=full)
full_up_m1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|FID|0|un + year, data=full_up)
full_L2_m1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|GID_2|0|un + year, data=GID_2_full)
full_L1_m1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|GID_1|0|un + year, data=GID_1_full)
full_Nat_m1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm + Polity_class + margin.norm:Polity_class|un|0|un + year, data=country_full)

texreg(list(full_m1,
            full_up_m1,
            full_L2_m1,
            full_L1_m1,
            full_Nat_m1),
       custom.coef.names = c("Forest","PCGDP","$\\Delta$ PCGDP","Pop Growth", "Competitiveness","Autocracy",
                             "Democracy","Comp:Autocracy","Comp:Democracy"),
       custom.model.names = c("0.05dd","0.5dd","L2","L1","National"),
       reorder.coef = c(5,8,9,6,7,1:4), booktabs = T,
       include.rsquared = F, include.adjrs = TRUE,
       caption = "Regressions of forest change on electoral competitiveness. Competitiveness is interacted with government type with Anocracy as the base case. 
       0.05 decimal degree is the cell level discussed in the paper, 0.5 dd is aggregated to cells 100 times as large,
       L2 is level two administrative units, L1 is level 1 administrative units, National is national borders as discussed in the paper.",
       label = "table:2.3",
       float.pos = "h",
       custom.gof.rows = list("Unit Fixed Effects" = c(rep("Yes",5))))




