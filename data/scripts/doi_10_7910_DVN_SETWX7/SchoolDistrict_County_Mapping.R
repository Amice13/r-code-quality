#Code to map NCES School Districts to U.S. Counties
#Author: Ernani F. Choma 

# Please change the following lines according to the correct paths
# Line 9 -- Working directory
# Lines 18-20; 28-30; 36; 45; and 54 -- Data Inputs 
# Lines 213-214 -- Data Outputs

setwd("Results_with_Emissions/SchoolDistrict_County_Mapping/")

###### 1. Loading Inputs ######
# 1.1. 2022 population by Census Block Group (2022, 5-year ACS estimates)
# Source: U.S. Census Bureau, 2018-2022 American Community Survey 5-Year Estimates
# Table Info: ID: ACSDT5Y2022.B01001. Title: Sex by Age
# Available at: https://data.census.gov/table/ACSDT5Y2022.B01001?t=Age%20and%20Sex&g=010XX00US$1500000
# Option Selected: ACS 5-Year Estimates Detailed Tables | Year 2022
# Downloaded on 02/19/24
USCB.2022.BG.Age <- read.csv("Inputs/ACSDT5Y2022.B01001-Data.csv",skip=1)
USCB.2022.BG.Age.H <- read.csv("Inputs/ACSDT5Y2022.B01001-Data.csv",nrows=1,header=F)
USCB.2022.BG.Age.H2 <- read.csv("Inputs/ACSDT5Y2022.B01001-Data.csv",skip=1,nrows=1,header=F)

# 1.2. 2020 population by Census Block Group from the Decennial Census
# Source: U.S. Census Bureau, 2020 Census Demographic and Housing Characteristics File (DHC)
# Table Info: ID: DECENNIALDHC2020.P1 Title: TOTAL POPULATION
# Available at: https://data.census.gov/table/DECENNIALDHC2020.P1?q=population&g=010XX00US$1500000&tid=DECENNIALDHCAS2020.P1
# Option Selected: DEC Demographic and Housing Characteristics | 2020
# Downloaded on 02/20/24
USCB.2020.BG <- read.csv("Inputs/DECENNIALDHC2020.P1-Data.csv",skip=1)
USCB.2020.BG.H <- read.csv("Inputs/DECENNIALDHC2020.P1-Data.csv",nrows=1,header=F)
USCB.2020.BG.H2 <- read.csv("Inputs/DECENNIALDHC2020.P1-Data.csv",skip=1,nrows=1)

# 1.3. List of U.S. States
# Source: U.S. Census Bureau
# Available at: https://www2.census.gov/geo/docs/reference/state.txt 
# Downloaded on 02/19/24
state <- read.csv("Inputs/state.txt",sep="|")
state <- state[,-4]

# 1.4. Connecticut: 2022 County Subdivision to 2020 Block Groups for Connecticut Relationship
# Source: U.S. Census Bureau
# 2022 County Subdivision to 2020 Block Groups for Connecticut Relationship File Layout (acs22_cousub22_blkgrp20_st09.txt)
# Available at: https://www2.census.gov/geo/docs/maps-data/data/rel2022/acs22_cousub22_blkgrp20_st09.txt
# Downloaded on 02/20/24
# Documentation available at: https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2022-connecticut-record-layout.html (accessed February 20, 2024)
CT.Mapping <- read.csv("Inputs/acs22_cousub22_blkgrp20_st09.txt",sep="|")

# 1.5. NCES Relationship files
# National Center for Education Statistics, 
# 2023 School District Geographic Relationship Files. 
# Available at: https://nces.ed.gov/programs/edge/data/GRF23.zip 
# Downloaded on 02/19/24
# After unzipping, the file used was the file 'grf23_lea_blkgrp.xlsx'
# The file was opened in Excel and saved as .csv
NCES <- read.csv("Inputs/grf23_lea_blkgrp_CSV.csv")

###### 2. Processing USCB Population Data ######
#2.1. USCB 2022 Block Group Population Data
names(USCB.2022.BG.Age) <- USCB.2022.BG.Age.H
names(USCB.2022.BG.Age.H2) <- USCB.2022.BG.Age.H

USCB.2022.BG.Age$aux.GEOID <- substr(USCB.2022.BG.Age$GEO_ID,10,21)
USCB.2022.BG.Age$Num.GEOID <- as.numeric(USCB.2022.BG.Age$aux.GEOID)

#Selecting the population between 5 and 19 years old
#Male
#<5 years old : B01001_003E / M (Estimate/Margin of error)
#5-9 yo : B01001_004E / M 
#10-14 yo : B01001_005E / M 
#15-17 yo : B01001_006E / M 
#18-19 yo : B01001_007E / M 

#Female
#<5 years old : B01001_027E / M (Estimate/Margin of error)
#5-9 yo : B01001_028E / M 
#10-14 yo : B01001_029E / M 
#15-17 yo : B01001_030E / M 
#18-19 yo : B01001_031E / M 

sel.col.names <- c("Num.GEOID",paste0("B01001_00",4:7,"E"),paste0("B01001_0",28:31,"E"))
USCB.POP <- subset(USCB.2022.BG.Age,select=sel.col.names)
USCB.POP$Tot.5.19 <- rowSums(USCB.POP[-1])

#2.2. USCB 2020 Block Group Population Data
names(USCB.2020.BG) <- USCB.2020.BG.H
names(USCB.2020.BG.H2) <- USCB.2020.BG.H

USCB.2020.BG$aux.GEOID <- substr(USCB.2020.BG$GEO_ID,10,21)
USCB.2020.BG$Num.GEOID <- as.numeric(USCB.2020.BG$aux.GEOID)


###### 3. Merging USCB and NCES ######
aux.USCB <- subset(USCB.POP,select=c("Num.GEOID","Tot.5.19"))
NCES <- merge(x=NCES,y=aux.USCB,by.x="BLKGRP",by.y="Num.GEOID",all.x=TRUE,all.y=FALSE)
NCES <- NCES[!is.na(NCES$Tot.5.19),]

#aggregating land area
Tot.LAREA <- aggregate(NCES$LANDAREA, by=list(NCES$BLKGRP), FUN=sum)
names(Tot.LAREA) <- c("BLKGRP","BG.LAREA")

NCES <- merge(x=NCES,y=Tot.LAREA,by="BLKGRP")

#Allocation factor by area
NCES$Alloc.Fac <- (NCES$LANDAREA/NCES$BG.LAREA)
# sum(NCES$BG.LAREA==0)
# table(NCES$Tot.5.19[NCES$BG.LAREA==0])
NCES$Alloc.Fac[NCES$BG.LAREA==0] <- 1
NCES$Alloc.Pop <- NCES$Alloc.Fac * NCES$Tot.5.19

NCES$STCOU <- NCES$BLKGRP %/% 1e7

#Aggregating Population by School District and County
Agg.Pop <- aggregate(NCES$Alloc.Pop, by=c(list(NCES$LEAID),list(NCES$STCOU)), FUN=sum)
names(Agg.Pop) <- c("LEAID","STCOU","POP_BY_STCOU")

#Aggregating Population by School District
Agg.Tot.Pop <- aggregate(NCES$Alloc.Pop, by=list(NCES$LEAID), FUN=sum)
names(Agg.Tot.Pop) <- c("LEAID","LEAID_TOT_POP")

Agg.Pop <- merge(x=Agg.Pop,y=Agg.Tot.Pop,by="LEAID",all=TRUE)

# 3.1. Calculating share of LEAID population in each county
Agg.Pop$STCOU.Share <- Agg.Pop$POP_BY_STCOU/Agg.Pop$LEAID_TOT_POP

aux.LEAID.zeropop <- Agg.Tot.Pop$LEAID[Agg.Tot.Pop$LEAID_TOT_POP==0]
NCES[NCES$LEAID %in% aux.LEAID.zeropop,]

#Districts LEAID  2382004, 2382013, 2505340 have zero total pop here
#But each of them are entirely in a single county

# Mapping of these LEAID to STCOU
# 2382004 -- 23023
# 2382013 -- 23015
# 2505340 -- 25007

# > NCES[NCES$LEAID %in% aux.LEAID.zeropop,]
# BLKGRP   LEAID                         NAME_LEA23 COUNT  LANDAREA  WATERAREA Tot.5.19  BG.LAREA Alloc.Fac
# 127875 230159756003 2382013 Louds Island Unorganized Territory     1  1.562537  13.414776        0  1.562537         1
# 128123 230239702004 2382004    Sagadahoc Unorganized Territory     1  2.301307   1.436131        0  2.301307         1
# 133398 250072004006 2505340            Gosnold School District     2 13.186048  26.356172        0 13.186048         1
# 133401 250079900000 2505340            Gosnold School District     2  0.000000 100.629673        0  0.000000         1
# Alloc.Pop STCOU
# 127875         0 23015
# 128123         0 23023
# 133398         0 25007
# 133401         0 25007
#

Agg.Pop[Agg.Pop$LEAID %in% aux.LEAID.zeropop,]
# > Agg.Pop[Agg.Pop$LEAID %in% aux.LEAID.zeropop,]
# LEAID STCOU POP_BY_STCOU LEAID_TOT_POP STCOU.Share
# 6408 2382004 23023            0             0         NaN
# 6417 2382013 23015            0             0         NaN
# 6546 2505340 25007            0             0         NaN

Agg.Pop$STCOU.Share[Agg.Pop$LEAID==2382004 & Agg.Pop$STCOU==23023] <- 1
Agg.Pop$STCOU.Share[Agg.Pop$LEAID==2382013 & Agg.Pop$STCOU==23015] <- 1
Agg.Pop$STCOU.Share[Agg.Pop$LEAID==2505340 & Agg.Pop$STCOU==25007] <- 1

aux.NCES <- subset(NCES,select=c("LEAID","NAME_LEA23"))
aux.NCES <- unique(aux.NCES)

Agg.Pop.2 <- merge(x=Agg.Pop, y=aux.NCES, by="LEAID", all.x=TRUE, all.y=FALSE)

#Creating State variable
Agg.Pop.2$ST <- Agg.Pop.2$STCOU %/% 1000
Agg.Pop.2 <- merge(x=Agg.Pop.2,y=state,by.x="ST",by.y="STATE",all.x=TRUE,all.y=FALSE)

###### 4. Mapping old (pre-2022) Connecticut Counties to new 2022 Connecticut Counties
aux.merge <- subset(USCB.2020.BG, select=c("Num.GEOID","P1_001N"))

CT.Mapping <- merge(x=CT.Mapping, y=aux.merge, by.x="GEOID_BLKGRP_20", by.y="Num.GEOID", all.x=TRUE, all.y=FALSE)

CT.Mapping$Share.Area <- CT.Mapping$AREALAND_PART/CT.Mapping$AREALAND_BLKGRP_20

CT.Mapping$P1_001N[is.na(CT.Mapping$Share.Area)] #all NA have 0 population

CT.Mapping$Calc.Pop <- CT.Mapping$Share.Area * CT.Mapping$P1_001N
CT.Mapping$Calc.Pop[is.na(CT.Mapping$Share.Area)] <- 0

#Creating New and Old County Variables
CT.Mapping$New.STCOU <- CT.Mapping$GEOID_COUSUB_22 %/% 1e5
CT.Mapping$Old.STCOU <- CT.Mapping$GEOID_BLKGRP_20 %/% 1e7

#Aggregating population to old and new counties
CT.STCOU.MAP <- aggregate(CT.Mapping$Calc.Pop, by=c(list(CT.Mapping$New.STCOU),list(CT.Mapping$Old.STCOU)),FUN=sum)
names(CT.STCOU.MAP) <- c("New.STCOU","Old.STCOU","Pop")

aux.agg <- aggregate(CT.STCOU.MAP$Pop, by=list(CT.STCOU.MAP$New.STCOU), FUN=sum)
names(aux.agg) <- c("New.STCOU","Tot.Pop.NewSTCOU")

CT.STCOU.MAP <- merge(x=CT.STCOU.MAP, y=aux.agg, by="New.STCOU", all=TRUE)

#Creating Allocation
CT.STCOU.MAP$Allocation <- CT.STCOU.MAP$Pop/CT.STCOU.MAP$Tot.Pop.NewSTCOU

###### 5. Merging CT relationship to NCES & Population data
aux.merge <- subset(CT.STCOU.MAP, select=c("New.STCOU","Old.STCOU","Allocation"))

Agg.Pop.3 <- merge(x=Agg.Pop.2, y=aux.merge, by.x="STCOU", by.y="New.STCOU",all=TRUE)

colSums(is.na(Agg.Pop.3))
length(Agg.Pop.2$STCOU[Agg.Pop.2$STCOU %in% aux.merge$New.STCOU])

#For districts outside CT -- no mapping needed
Agg.Pop.3$Allocation[is.na(Agg.Pop.3$Allocation)] <- 1
Agg.Pop.3$Old.STCOU[is.na(Agg.Pop.3$Old.STCOU)] <- Agg.Pop.3$STCOU[is.na(Agg.Pop.3$Old.STCOU)]

#Creating final share
Agg.Pop.3$Old.STCOU.Share <- Agg.Pop.3$STCOU.Share * Agg.Pop.3$Allocation

SchoolDistrict.County.Mapping <- subset(Agg.Pop.3, select=c("LEAID","NAME_LEA23","ST","STUSAB","STATE_NAME","Old.STCOU","Old.STCOU.Share"))

# write.csv(SchoolDistrict.County.Mapping, file="Outputs/SchoolDistrict_County_Mapping.csv",row.names=FALSE)
save(SchoolDistrict.County.Mapping, file="Outputs/SchoolDistrict_County_Mapping.RData")
