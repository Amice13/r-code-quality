#______________________________________________________________________________________________________________________________________________________________
#______________________________________________________________________________________________________________________________________________________________
#____________  FILE NAME:  TX_Oil_Gas_Income_HIV_MVC_2006and2018.r   __________________________________________________________________________________________
#____________                                                        __________________________________________________________________________________________
#____________  PURPOSE:  This R script reads in Texas County-level   __________________________________________________________________________________________
#____________             data to investigate associations with oil  __________________________________________________________________________________________
#____________             and gas production in 2006 and 2018. It    __________________________________________________________________________________________
#____________             loads demographic data, creates maps,      __________________________________________________________________________________________
#____________             conducts spatial analysis, and performs    __________________________________________________________________________________________
#____________             regression modeling.                       __________________________________________________________________________________________
#____________                                                        __________________________________________________________________________________________
#____________  AUTHOR:  Joel Chávez, Texas, USA                      __________________________________________________________________________________________
#____________                                                        __________________________________________________________________________________________
#____________  AFFILIATION: Dept. of Public Service and Admin.       __________________________________________________________________________________________
#____________               The Bush School of Govt. and Pub. Serv., __________________________________________________________________________________________
#____________               Texas A&M University                     __________________________________________________________________________________________
#____________                                                        __________________________________________________________________________________________
#____________  CONTACT: joel.chavez@tamu.edu                         __________________________________________________________________________________________
#____________                                                        __________________________________________________________________________________________
#____________  CREATED:  09.01.2020                                  __________________________________________________________________________________________
#____________  LAST MODIFIED:  12.03.2020 (JC)                       __________________________________________________________________________________________
#______________________________________________________________________________________________________________________________________________________________
#______________________________________________________________________________________________________________________________________________________________

      # installing and loading libraries: 'sf', 'openxlsx', 'ggplot2' 'spdep' 
install.packages("sf","openxlsx", "ggplot2", "spdep")
library(sf)
      # The sf package allows Texas county boundaries to be read initially using 'st_read'. see section 2
      # More info on this package can be found at https://cran.r-project.org/web/packages/sf/sf.pdf
library(openxlsx)
      # The "openxlsx" package allows Microsoft Excel Files to be read in effectively using the function "read.xlsx". see section 6
      # More info on this package can be found at https://cran.r-project.org/web/packages/openxlsx/openxlsx.pdf
library(ggplot2)
      # The "ggplot2" package is used to create the maps for this project using the various functions (ggplot, geom_sf, etc.). see sections 13a-14d
      # More info on this package can be found at https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
library(spdep)
      # The "spdep" package is used to calculate the Getis-Ord Gi* statistic for hot/cold spot analysis. see sections 5b, 5c and 13a-13f.
      # More info on this package can be found at https://cran.r-project.org/web/packages/spdep/spdep.pdf
#______________________________________________________________________________________________________________________________________________________________
#______________________________________________________________________________________________________________________________________________________________
#________             TABLE OF CONTENTS                                       _________________________________________________________________________________
#________                                                                     _________________________________________________________________________________
#________   I. SECTIONS 1-11 (lines 55-522): Loading in Data                  _________________________________________________________________________________
#________                                                                     _________________________________________________________________________________
#________  II. SECTIONS 12-12k (lines 523-808): Spatial Analysis, Calculating _________________________________________________________________________________
#________                                      Getis-Ord Gi* Statistic        _________________________________________________________________________________
#________                                                                     _________________________________________________________________________________
#________ III. SECTION 13-13i  (lines 809-871): Regression Modeling           _________________________________________________________________________________
#________                                                                     _________________________________________________________________________________
#______________________________________________________________________________________________________________________________________________________________
#______________________________________________________________________________________________________________________________________________________________

# ***NOTE: "CHNG" IN THIS FILE MEANS CHANGE FROM 2006 TO 2018 ("THE GIVEN PERIOD")

          # 1.  ---- **THIS CODE CLEARS OUT ALL VARIABLES FROM R ENVIRONMENT ----
rm(list=ls()) 
#______________________________________________________________________________________________________________________________________________________________
#______________________________________________________________________________________________________________________________________________________________
      

          # 2a. ---- **THIS CODE LOADS COUNTY SHAPES FROM .KML, DATA SOURCE: TxDOT (2020) ----
              # ---- THIS CODE DOES NOT READ IN COUNTY NAMES**
tx_counties_sf <- st_read("https://opendata.arcgis.com/datasets/9b2eb7d232584572ad53bad41c76b04d_0.kml",)[,c(3)]

          # 2b. ---- **THIS CODE LOADS COUNTY NAMES FROM .CSV, DATA SOURCE: TxDOT (2020) ----
              # ---- READS IN COUNTY NAMES IN SAME ORDER AS KML IN 2a., WHICH DOES NOT PROPERLY READ IN COUNTY NAMES
CNTY_NM <- read.csv("https://opendata.arcgis.com/datasets/9b2eb7d232584572ad53bad41c76b04d_0.csv",)[,c("CNTY_NM")]

          # 2c. --- HERE, BINDS COUNTY SHAPES WITH COUNTY NAMES 
tx_counties_sf <- cbind(CNTY_NM,tx_counties_sf)
rm(CNTY_NM)

          # 2d ---- THIS CODE MAKES COUNTY NAMES ALL UPPERCASE, MAKES FOR SMOOTH MERGING OF DATA 
tx_counties_sf$CNTY_NM <- toupper(tx_counties_sf$CNTY_NM)

          # 2E ---- I MAKE ANOTHER COUNTY NAME COLUMN THAT HAS THE WORD "COUNTY" AFTER COUNTY NAME, FOR SMOOTH MERGING
              # ---- BECAUSE SOME DATA SOURCES HAVE "COUNTY" AFTER A COUNTY NAME IN THE COLUMN OF SPREADSHEET 
tx_counties_sf$COUNTY <- paste(tx_counties_sf$CNTY_NM,"COUNTY")

          # 2F ---- HERE, I SORT BY COUNTY NAME
tx_counties_sf <- tx_counties_sf[order(tx_counties_sf$CNTY_NM),]
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________
      
          # 3a. ---- **THIS CODE READS IN 2018 DEMOGRAPHIC DATA, DATA SOURCE: U.S. CENSUS BUREAU (2020) ----
  demographics2018_all <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/asrh/cc-est2019-alldata-48.csv")

              # ---- THIS CODE CREATES SUBSET OF DATA....Year 11 = 2018, AGEGRP 0= ALL AGES
  demographics2018 <- subset(demographics2018_all, YEAR==11 & AGEGRP==0)
  
              # ---- THIS CODE CREATES SUBSET OF DATA...COLUMN 5 IS COUNTY NAME, 8 IS TOTAL POP, 9 IS TOTAL MALE POP, 10 IS TOTAL FEMALE POP
  demographics2018 <- demographics2018[,c(5,8:18,57:58)]
  demographics2018$CTYNAME <- toupper(demographics2018$CTYNAME)
  demographics2018$CTYNAME <- replace(demographics2018$CTYNAME,demographics2018$CTYNAME=="DEWITT COUNTY","DE WITT COUNTY")

              # ---- THIS CODE ADJUSTS COLUMN NAMES
  names(demographics2018)[names(demographics2018) == "TOT_POP"] <- "Pop2018"
  names(demographics2018)[names(demographics2018) == "TOT_MALE"] <- "PopMale2018"
  names(demographics2018)[names(demographics2018) == "TOT_FEMALE"] <- "PopFemale2018"
  names(demographics2018)[names(demographics2018) == "WA_MALE"] <- "PopMaleWhite2018"
  names(demographics2018)[names(demographics2018) == "WA_FEMALE"] <- "PopFemaleWhite2018"
  names(demographics2018)[names(demographics2018) == "BA_MALE"] <- "PopMaleBlack2018"
  names(demographics2018)[names(demographics2018) == "BA_FEMALE"] <- "PopFemaleBlack2018"
  names(demographics2018)[names(demographics2018) == "IA_MALE"] <- "PopMaleAmIndian2018"
  names(demographics2018)[names(demographics2018) == "IA_FEMALE"] <- "PopFemaleAmIndian2018"
  names(demographics2018)[names(demographics2018) == "AA_MALE"] <- "PopMaleAsian2018"
  names(demographics2018)[names(demographics2018) == "AA_FEMALE"] <- "PopFemaleAsian2018"
  names(demographics2018)[names(demographics2018) == "H_MALE"] <- "PopMaleHisp2018"
  names(demographics2018)[names(demographics2018) == "H_FEMALE"] <- "PopFemaleHisp2018"
  demographics2018$PopWhite2018 <- demographics2018$PopMaleWhite2018+demographics2018$PopFemaleWhite2018
  demographics2018$PopBlack2018 <- demographics2018$PopMaleBlack2018+demographics2018$PopFemaleBlack2018
  demographics2018$PopAmIndian2018 <- demographics2018$PopMaleAmIndian2018+demographics2018$PopFemaleAmIndian2018
  demographics2018$PopAsian2018 <- demographics2018$PopMaleAsian2018+demographics2018$PopFemaleAsian2018
  demographics2018$PopOther2018 <- demographics2018$Pop2018-demographics2018$PopWhite2018-demographics2018$PopBlack2018-demographics2018$PopAmIndian2018-demographics2018$PopAsian2018
  demographics2018$PopHisp2018 <- demographics2018$PopMaleHisp2018 + demographics2018$PopFemaleHisp2018
  demographics2018$MaleFemaleRatio2018 <- demographics2018$PopMale2018/demographics2018$PopFemale2018
  demographics2018$MalePCT2018 <- demographics2018$PopMale2018/demographics2018$Pop2018

              # ---- THIS CODE MERGES THIS SECTIONS DATA WITH THE KEY OBJECT, "tx_counties_sf"
tx_counties_sf <- merge(tx_counties_sf,demographics2018, by.x="COUNTY", by.y="CTYNAME")


              # ---- HERE I SUBSET 2018 DATA FOR RETRIEVING AGE GROUPS
  demographics2018_agegroups <- subset(demographics2018_all, YEAR==11)
  demographics2018_agegroups$CTYNAME <- toupper(demographics2018_agegroups$CTYNAME)
  demographics2018_agegroups$CTYNAME <- replace(demographics2018_agegroups$CTYNAME,demographics2018_agegroups$CTYNAME=="DEWITT COUNTY","DE WITT COUNTY")

          # 3b. ---- **THIS CODE CREATES 2018 15 to 29 AGE COHORT ----
demographics2018_agegroups15to19 <- subset(demographics2018_agegroups, AGEGRP==4)
demographics2018_agegroups15to19 <- demographics2018_agegroups15to19[,c(5,8)]
names(demographics2018_agegroups15to19)[names(demographics2018_agegroups15to19) == "TOT_POP"] <- "Pop2018_15to19"

demographics2018_agegroups20to24<- subset(demographics2018_agegroups, AGEGRP==5)
demographics2018_agegroups20to24 <- demographics2018_agegroups20to24[,c(8)]

demographics2018_agegroups25to29 <- subset(demographics2018_agegroups, AGEGRP==6)
demographics2018_agegroups25to29 <- demographics2018_agegroups25to29[,c(8)]

demographics2018_agegroups15to29<-cbind(demographics2018_agegroups15to19,demographics2018_agegroups20to24,demographics2018_agegroups25to29)
demographics2018_agegroups15to29$Pop2018age15to29 <- rowSums(demographics2018_agegroups15to29[c(2:4)])

      # ---- THIS CODE MERGES THIS SECTIONS DATA WITH THE KEY OBJECT, "tx_counties_sf"
tx_counties_sf <- merge(tx_counties_sf,demographics2018_agegroups15to29, by.x="COUNTY", by.y="CTYNAME")

          # 3c. ---- **THIS CODE CREATES 2018 65 and OLDER AGE COHORT ----
demographics2018_agegroups65to69<- subset(demographics2018_agegroups, AGEGRP==14)
demographics2018_agegroups65to69 <- demographics2018_agegroups65to69[,c(5,8)]
names(demographics2018_agegroups65to69)[names(demographics2018_agegroups65to69) == "TOT_POP"] <- "Pop2018_65to69"

demographics2018_agegroups70to74<- subset(demographics2018_agegroups, AGEGRP==15)
demographics2018_agegroups70to74 <- demographics2018_agegroups70to74[,c(8)]

demographics2018_agegroups75to79<- subset(demographics2018_agegroups, AGEGRP==16)
demographics2018_agegroups75to79 <- demographics2018_agegroups75to79[,c(8)]

demographics2018_agegroups80to84<- subset(demographics2018_agegroups, AGEGRP==17)
demographics2018_agegroups80to84 <- demographics2018_agegroups80to84[,c(8)]

demographics2018_agegroups85andOlder<- subset(demographics2018_agegroups, AGEGRP==18)
demographics2018_agegroups85andOlder <- demographics2018_agegroups85andOlder[,c(8)]

demographics2018_agegroups65andOlder<-cbind(demographics2018_agegroups65to69,demographics2018_agegroups70to74,demographics2018_agegroups75to79,demographics2018_agegroups80to84,demographics2018_agegroups85andOlder)
demographics2018_agegroups65andOlder$Pop2018age65andOlder <- rowSums(demographics2018_agegroups65andOlder[c(2:6)])
tx_counties_sf <- merge(tx_counties_sf,demographics2018_agegroups65andOlder, by.x="COUNTY", by.y="CTYNAME")

#_____________________________________________________________________________________________________________________________________________

      # 3d. ---- THIS CODE READS IN TX POP. DATA FOR 2000-2010 FROM .CSV FILE, DATA SOURCE: FROM US CENSUS BUREAU (2020)
demographics2000to2010 <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est00int-alldata-48.csv")

          # 3e. ---- **THIS CODE CREATES A SUBSET OF 2006 DEMO. DATA: Year 8 = 2006, AGEGRP 99 = ALL AGES ----
demographics2006 <- subset(demographics2000to2010, YEAR==8 & AGEGRP==99)
demographics2006 <- demographics2006[,c(5,8:18,37:38)]
demographics2006$CTYNAME <- toupper(demographics2006$CTYNAME)

      # ---- THIS CODE ALTERS NAME FOR DE WITT COUNTY, OTHERWISE IT WOULD BE EXCLUDED
demographics2006$CTYNAME <- replace(demographics2006$CTYNAME,demographics2006$CTYNAME=="DEWITT COUNTY","DE WITT COUNTY")

      # ---- THIS CODE ADJUSTS COLUMN NAMES
names(demographics2006)[names(demographics2006) == "TOT_POP"] <- "Pop2006"
names(demographics2006)[names(demographics2006) == "TOT_MALE"] <- "PopMale2006"
names(demographics2006)[names(demographics2006) == "TOT_FEMALE"] <- "PopFemale2006"
names(demographics2006)[names(demographics2006) == "WA_MALE"] <- "PopMaleWhite2006"
names(demographics2006)[names(demographics2006) == "WA_FEMALE"] <- "PopFemaleWhite2006"
names(demographics2006)[names(demographics2006) == "BA_MALE"] <- "PopMaleBlack2006"
names(demographics2006)[names(demographics2006) == "BA_FEMALE"] <- "PopFemaleBlack2006"
names(demographics2006)[names(demographics2006) == "IA_MALE"] <- "PopMaleAmIndian2006"
names(demographics2006)[names(demographics2006) == "IA_FEMALE"] <- "PopFemaleAmIndian2006"
names(demographics2006)[names(demographics2006) == "AA_MALE"] <- "PopMaleAsian2006"
names(demographics2006)[names(demographics2006) == "AA_FEMALE"] <- "PopFemaleAsian2006"
names(demographics2006)[names(demographics2006) == "H_MALE"] <- "PopMaleHisp2006"
names(demographics2006)[names(demographics2006) == "H_FEMALE"] <- "PopFemaleHisp2006"
demographics2006$PopWhite2006 <- demographics2006$PopMaleWhite2006+demographics2006$PopFemaleWhite2006
demographics2006$PopBlack2006 <- demographics2006$PopMaleBlack2006+demographics2006$PopFemaleBlack2006
demographics2006$PopAmIndian2006 <- demographics2006$PopMaleAmIndian2006+demographics2006$PopFemaleAmIndian2006
demographics2006$PopAsian2006 <- demographics2006$PopMaleAsian2006+demographics2006$PopFemaleAsian2006
demographics2006$PopOther2006 <- demographics2006$Pop2006-demographics2006$PopWhite2006-demographics2006$PopBlack2006-demographics2006$PopAmIndian2006-demographics2006$PopAsian2006
demographics2006$PopHisp2006 <- demographics2006$PopMaleHisp2006 + demographics2006$PopFemaleHisp2006
demographics2006$MalePCT2006 <- demographics2006$PopMale2006/demographics2006$Pop2006
demographics2006$MaleFemaleRatio2006 <- demographics2006$PopMale2006/demographics2006$PopFemale2006

      # ---- THIS CODE MERGES THIS SECTIONS DATA WITH THE KEY OBJECT, "tx_counties_sf"
tx_counties_sf <- merge(tx_counties_sf,demographics2006, by.x="COUNTY", by.y="CTYNAME")

          # 3f. ---- **THIS CODE CALCULATES CHANGE IN DEMOGRAPHICS FROM 2006 TO 2018 ----
tx_counties_sf$CHNG_Pop <- (tx_counties_sf$Pop2018 - tx_counties_sf$Pop2006)
tx_counties_sf$CHNG_MaleToFemaleRatio <- tx_counties_sf$MaleFemaleRatio2018 - tx_counties_sf$MaleFemaleRatio2006
tx_counties_sf$PCT_CHNG_MaleToFemaleRatio <- 100*((tx_counties_sf$MaleFemaleRatio2018 - tx_counties_sf$MaleFemaleRatio2006)/tx_counties_sf$MaleFemaleRatio2006)
tx_counties_sf$PCT_Male_CHNG <- 100*(tx_counties_sf$PopMale2018 - tx_counties_sf$PopMale2006)/demographics2006$PopMale2006
tx_counties_sf$CHNG_White <- tx_counties_sf$PopWhite2018 - tx_counties_sf$PopWhite2006
tx_counties_sf$CHNG_Black <- tx_counties_sf$PopBlack2018 - tx_counties_sf$PopBlack2006
tx_counties_sf$CHNG_AmericanIndian <- tx_counties_sf$PopAmIndian2018 - tx_counties_sf$PopAmIndian2006
tx_counties_sf$CHNG_Asian <- tx_counties_sf$PopAsian2018 - tx_counties_sf$PopAsian2006
tx_counties_sf$CHNG_Other <- tx_counties_sf$PopOther2018 - tx_counties_sf$PopOther2006
tx_counties_sf$CHNG_Hispanic <- tx_counties_sf$PopHisp2018 - tx_counties_sf$PopHisp2006
tx_counties_sf$CHNG_MalePCT <- tx_counties_sf$MalePCT2018 - tx_counties_sf$MalePCT2006

          # 3g. ---- **THIS CODE SUBSETS DATA FOR 2006 AGE GROUPS ----
  demographics2006_agegroups <- subset(demographics2000to2010, YEAR==8)
  demographics2006_agegroups$CTYNAME <- toupper(demographics2006_agegroups$CTYNAME)
  demographics2006_agegroups$CTYNAME <- replace(demographics2006_agegroups$CTYNAME,demographics2006_agegroups$CTYNAME=="DEWITT COUNTY","DE WITT COUNTY")

          # 3h. ---- **THIS CODE CREATES 2006 15 to 29 AGE COHORT ----
  demographics2006_agegroups15to19<- subset(demographics2006_agegroups, AGEGRP==4)
  demographics2006_agegroups15to19 <- demographics2006_agegroups15to19[,c(5,8)]
  names(demographics2006_agegroups15to19)[names(demographics2006_agegroups15to19) == "TOT_POP"] <- "Pop2006_15to19"

  demographics2006_agegroups20to24<- subset(demographics2006_agegroups, AGEGRP==5)
  demographics2006_agegroups20to24 <- demographics2006_agegroups20to24[,c(8)]
  names(demographics2006_agegroups20to24)[names(demographics2006_agegroups20to24) == "TOT_POP"] <- "Pop2006_20to24"

  demographics2006_agegroups25to29<- subset(demographics2006_agegroups, AGEGRP==6)
  demographics2006_agegroups25to29 <- demographics2006_agegroups25to29[,c(8)]
  names(demographics2006_agegroups25to29)[names(demographics2006_agegroups25to29) == "TOT_POP"] <- "Pop2006_25to29"

  demographics2006_agegroups15to29<-cbind(demographics2006_agegroups15to19,demographics2006_agegroups20to24,demographics2006_agegroups25to29)
  demographics2006_agegroups15to29$Pop2006age15to29 <- rowSums(demographics2006_agegroups15to29[c(2:4)])

      # ---- THIS CODE MERGES THIS SECTIONS DATA WITH THE KEY OBJECT, "tx_counties_sf"
tx_counties_sf <- merge(tx_counties_sf,demographics2006_agegroups15to29, by.x="COUNTY", by.y="CTYNAME")

          # 3i. ---- **THIS CODE CREATES 2006 65 and OLDER AGE COHORT ----
  demographics2006_agegroups65to69<- subset(demographics2006_agegroups, AGEGRP==14)
  demographics2006_agegroups65to69 <- demographics2006_agegroups65to69[,c(5,8)]
  names(demographics2006_agegroups65to69)[names(demographics2006_agegroups65to69) == "TOT_POP"] <- "Pop2006_65to69"

  demographics2006_agegroups70to74<- subset(demographics2006_agegroups, AGEGRP==15)
  demographics2006_agegroups70to74 <- demographics2006_agegroups70to74[,c(8)]

  demographics2006_agegroups75to79<- subset(demographics2006_agegroups, AGEGRP==16)
  demographics2006_agegroups75to79 <- demographics2006_agegroups75to79[,c(8)]

  demographics2006_agegroups80to84<- subset(demographics2006_agegroups, AGEGRP==17)
  demographics2006_agegroups80to84 <- demographics2006_agegroups80to84[,c(8)]

  demographics2006_agegroups85andOlder<- subset(demographics2006_agegroups, AGEGRP==18)
  demographics2006_agegroups85andOlder <- demographics2006_agegroups85andOlder[,c(8)]

  demographics2006_agegroups65andOlder<-cbind(demographics2006_agegroups65to69,demographics2006_agegroups70to74,demographics2006_agegroups75to79,demographics2006_agegroups80to84,demographics2006_agegroups85andOlder)
  demographics2006_agegroups65andOlder$Pop2006age65andOlder <- rowSums(demographics2006_agegroups65andOlder[c(2:6)])

tx_counties_sf <- merge(tx_counties_sf,demographics2006_agegroups65andOlder, by.x="COUNTY", by.y="CTYNAME")
      # ---- THIS CODE CALCULATES THE CHANGE IN AGE COHORTS FROM 2006 TO 2018
tx_counties_sf$CHNG_15to29 <- tx_counties_sf$Pop2018age15to29-tx_counties_sf$Pop2006age15to29
tx_counties_sf$CHNG_65andOlder <- tx_counties_sf$Pop2018age65andOlder-tx_counties_sf$Pop2006age65andOlder

#_____________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

          # 4a. ---- **THIS CODE LOADS INCOME FIGURES FOR 2006, DATA SOURCE: U.S. CENSUS BUREAU (2020) ----
  income2006<-read.table('https://www2.census.gov/programs-surveys/saipe/datasets/2006/2006-state-and-county/est06-tx.txt',
                       strip.white = TRUE, fill=TRUE,skip=1,blank.lines.skip=TRUE)[,c(3,21,24:25)]
  income2006<-na.omit(income2006)
  income2006$V24 <- toupper(income2006$V24)
  
      # ---- THIS CODE ADJUSTS DATA TO CORRECT FOR THE READ-IN, WHICH DOES NOT PROPERLY READ IN DATA FOR TWO-NAME COUNTIES
  income2006$V24 <- replace(income2006$V24,income2006$V24=="DEAF","DEAF SMITH")
  income2006$V24 <- replace(income2006$V24,income2006$V24=="DEWITT","DE WITT")
  income2006$V24 <- replace(income2006$V24,income2006$V24=="EL","EL PASO")
  income2006$V24 <- replace(income2006$V24,income2006$V24=="FORT","FORT BEND")
  income2006$V24 <- replace(income2006$V24,income2006$V24=="JEFF","JEFF DAVIS")
  income2006$V24 <- replace(income2006$V24,income2006$V25=="Hogg","JIM HOGG")
  income2006$V24 <- replace(income2006$V24,income2006$V25=="Wells","JIM WELLS")
  income2006$V24 <- replace(income2006$V24,income2006$V25=="Salle","LA SALLE")
  income2006$V24 <- replace(income2006$V24,income2006$V25=="Oak","LIVE OAK")
  income2006$V24 <- replace(income2006$V24,income2006$V25=="River","RED RIVER")
  income2006$V24 <- replace(income2006$V24,income2006$V25=="Augustine","SAN AUGUSTINE")
  income2006$V24 <- replace(income2006$V24,income2006$V25=="Jacinto","SAN JACINTO")
  income2006$V24 <- replace(income2006$V24,income2006$V25=="Pinto","PALO PINTO")
  income2006$V24 <- replace(income2006$V24,income2006$V25=="Patricio","SAN PATRICIO")
  income2006$V24 <- replace(income2006$V24,income2006$V25=="Saba","SAN SABA")
  income2006$V24 <- replace(income2006$V24,income2006$V25=="Green","TOM GREEN")
  income2006$V24 <- replace(income2006$V24,income2006$V25=="Verde","VAL VERDE")
  income2006$V24 <- replace(income2006$V24,income2006$V25=="Zandt","VAN ZANDT")
  income2006 = subset(income2006, select = -c(V25) )
  
      # ---- THIS CODE ADJUSTS COLUMN NAMES
  names(income2006)[names(income2006) == "V3"] <- "Census2006PovertyEstimate"
  names(income2006)[names(income2006) == "V21"] <- "Census2006MedianHHincome"
  names(income2006)[names(income2006) == "V24"] <- "CountyName"
          # 4b. ---- **THIS CODE CONVERTS 2006 MEDIAN HH INCOME TO REAL 2018 DOLLARS----
      #    ---- ACCORDING TO US BUREAU OF LABOR STATISTICS, THE CPI-U-RS FOR 2018 IS 369.8 AND THE CPI-U-RS FOR 2006 IS 296.2 
      #    ---- SOURCE: https://www.bls.gov/cpi/research-series/allitems.pdf
income2006$Census2006MedianHHincome_2018D <- income2006$Census2006MedianHHincome*(369.8/ 296.2) 

      # ---- THIS CODE MERGES THIS SECTIONS DATA WITH THE KEY OBJECT, "tx_counties_sf"
tx_counties_sf <- merge(tx_counties_sf,income2006, by.x="CNTY_NM", by.y="CountyName")

          # 4c. ---- **THIS CODE LOADS TEXAS COUNTY INCOME FIGURES FOR 2018, DATA SOURCE: U.S. CENSUS BUREAU (2020) ----
  income2018<-read.table('https://www2.census.gov/programs-surveys/saipe/datasets/2018/2018-state-and-county/est18-tx.txt',
                       strip.white = TRUE, fill=TRUE,skip=1,blank.lines.skip=TRUE)[,c(3,21,24:25)]
  income2018<-na.omit(income2018)
  income2018$V24 <- toupper(income2018$V24)
  
      # ---- THIS CODE ADJUSTS DATA TO CORRECT FOR THE READ-IN, WHICH DOES NOT PROPERLY READ IN DATA FOR TWO-NAME COUNTIES
  income2018$V24 <- replace(income2018$V24,income2018$V24=="DEAF","DEAF SMITH")
  income2018$V24 <- replace(income2018$V24,income2018$V24=="DEWITT","DE WITT")
  income2018$V24 <- replace(income2018$V24,income2018$V24=="EL","EL PASO")
  income2018$V24 <- replace(income2018$V24,income2018$V24=="FORT","FORT BEND")
  income2018$V24 <- replace(income2018$V24,income2018$V24=="JEFF","JEFF DAVIS")
  income2018$V24 <- replace(income2018$V24,income2018$V25=="Hogg","JIM HOGG")
  income2018$V24 <- replace(income2018$V24,income2018$V25=="Wells","JIM WELLS")
  income2018$V24 <- replace(income2018$V24,income2018$V25=="Salle","LA SALLE")
  income2018$V24 <- replace(income2018$V24,income2018$V25=="Oak","LIVE OAK")
  income2018$V24 <- replace(income2018$V24,income2018$V25=="River","RED RIVER")
  income2018$V24 <- replace(income2018$V24,income2018$V25=="Augustine","SAN AUGUSTINE")
  income2018$V24 <- replace(income2018$V24,income2018$V25=="Jacinto","SAN JACINTO")
  income2018$V24 <- replace(income2018$V24,income2018$V25=="Pinto","PALO PINTO")
  income2018$V24 <- replace(income2018$V24,income2018$V25=="Patricio","SAN PATRICIO")
  income2018$V24 <- replace(income2018$V24,income2018$V25=="Saba","SAN SABA")
  income2018$V24 <- replace(income2018$V24,income2018$V25=="Green","TOM GREEN")
  income2018$V24 <- replace(income2018$V24,income2018$V25=="Verde","VAL VERDE")
  income2018$V24 <- replace(income2018$V24,income2018$V25=="Zandt","VAN ZANDT")
  income2018 = subset(income2018, select = -c(V25) )
  names(income2018)[names(income2018) == "V3"] <- "Census2018PovertyEstimate"
  names(income2018)[names(income2018) == "V21"] <- "Census2018MedianHHincome"
  names(income2018)[names(income2018) == "V24"] <- "CountyName"

      # ---- THIS CODE MERGES THIS SECTIONS DATA WITH THE KEY OBJECT, "tx_counties_sf"
tx_counties_sf <- merge(tx_counties_sf,income2018, by.x="CNTY_NM", by.y="CountyName")
      # ---- THIS CODE CREATES NEW COLUMN TO REPRESENT CHANGE IN MEDIAN HOUSEHOLD INCOME, 2006 TO 2018, DATA SOURCE: US CENSUS BUREAU (2020)
tx_counties_sf$CHNG_MedianHHincome <- tx_counties_sf$Census2018MedianHHincome - tx_counties_sf$Census2006MedianHHincome_2018D 
tx_counties_sf$PCT_CHNG_MedianHHincome <- 100*((tx_counties_sf$Census2018MedianHHincome - tx_counties_sf$Census2006MedianHHincome_2018D)/tx_counties_sf$Census2006MedianHHincome_2018D)

      # ---- THIS CODE CREATES NEW COLUMN TO REPRESENT CHANGE IN POVERTY, 2006 TO 2018, DATA SOURCE: US CENSUS BUREAU (2020)
tx_counties_sf$CHNG_Poverty <- (tx_counties_sf$Census2018PovertyEstimate/tx_counties_sf$Pop2018) - (tx_counties_sf$Census2006PovertyEstimate/tx_counties_sf$Pop2006)
tx_counties_sf$PCT_CHNG_Poverty <- 100*((tx_counties_sf$Census2018PovertyEstimate - tx_counties_sf$Census2006PovertyEstimate)/tx_counties_sf$Census2006PovertyEstimate)

#_____________________________________________________________________________________________________________________________________

          # 5.  ---- **THIS CODE READS IN OIL AND GAS PRODUCTION DATA FROM 2008 AND 2018, DATA SOURCE: TEXAS RAILROAD COMMISSION (2020) ----
  oilgasproductionTX <- read.csv("https://dataverse.harvard.edu/api/access/datafile/4193440", skip=5)


      # ---- THIS CODE MERGES THIS SECTIONS DATA WITH THE KEY OBJECT, "tx_counties_sf"
tx_counties_sf <- merge(tx_counties_sf,oilgasproductionTX,by.x="CNTY_NM",by.y="County", 
                        all = TRUE)
tx_counties_sf[is.na(tx_counties_sf)] <- 0

          # 6.  ---- **THIS CODE CALCULATES CHANGES IN OIL AND NATURAL GAS PRODUCTION, 2006 TO 2018 ----
tx_counties_sf$CHNG_OilProd <- tx_counties_sf$OilProd2018_BBL-tx_counties_sf$OilProd2006_BBL
tx_counties_sf$CHNG_GasProd <- tx_counties_sf$GasProd2018_MCF-tx_counties_sf$GasProd2006_MCF
tx_counties_sf$CHNG_GasProd_cf <- 1000000*(tx_counties_sf$GasProd2018_MCF-tx_counties_sf$GasProd2006_MCF)
tx_counties_sf$CHNG_GasProd_Tcf <- (tx_counties_sf$GasProd2018_MCF-tx_counties_sf$GasProd2006_MCF) / 1000000
tx_counties_sf$PCT_CHNG_OilProd <- 100*((tx_counties_sf$OilProd2018_BBL-tx_counties_sf$OilProd2006_BBL)/tx_counties_sf$OilProd2006_BBL)
tx_counties_sf$PCT_CHNG_OilProd[tx_counties_sf$PCT_CHNG_OilProd==Inf] <- 0
tx_counties_sf$PCT_CHNG_GasProd <- 100*((tx_counties_sf$GasProd2018_MCF-tx_counties_sf$GasProd2006_MCF)/tx_counties_sf$GasProd2006_MCF)
tx_counties_sf$PCT_CHNG_GasProd[tx_counties_sf$PCT_CHNG_GasProd==Inf] <- 0

      # ---- THIS CALCULATES TOP QUARTILES CHANGE IN FOR OIL & GAS PRODUCTION
tx_counties_sf$CHNG_Oil_90thPCT <- ifelse((tx_counties_sf$CHNG_OilProd>=(quantile(tx_counties_sf$CHNG_OilProd, probs = c(0.9)))),1,0)
tx_counties_sf$CHNG_Gas_90thPCT <- ifelse((tx_counties_sf$CHNG_GasProd>=(quantile(tx_counties_sf$CHNG_GasProd, probs = c(0.9)))),1,0)
tx_counties_sf$CHNG_Oil_Q1 <- ifelse((tx_counties_sf$CHNG_OilProd<(quantile(tx_counties_sf$CHNG_OilProd, probs = c(0.25)))),1,0)
tx_counties_sf$CHNG_Gas_Q1 <- ifelse((tx_counties_sf$CHNG_GasProd<(quantile(tx_counties_sf$CHNG_GasProd, probs = c(0.25)))),1,0)

          # 7.  ---- **THIS CODE READS IN NATURAL GAS FLARING VOLUME DATA FOR 2018 FROM NOAA Satellites **NOW PROVIDED THROUGH Colorado School of Mines** ----
#      flaring2018 <- read.xlsx("https://eogdata.mines.edu/global_flare_data/VIIRS_Global_flaring_d.7_slope_0.029353_2018_web.xlsx",sheet=4)
#     flaring2018 <- st_as_sf(flaring2018, coords = c("Longitude","Latitude"), crs=4326)
#     flaringTX2018 <- st_intersection(tx_counties_sf, flaring2018)
#     flaringTX2018 <- aggregate(flaringTX2018$BCM.2018,list(flaringTX2018$CNTY_NM), FUN=sum)
#     names(flaringTX2018)[names(flaringTX2018) == "x"] <- "Flaring2018Volumes"
#     tx_counties_sf <- merge(tx_counties_sf,flaringTX2018, by.x="CNTY_NM",by.y="Group.1", all=TRUE)
#     map_flaringTX2018 <- ggplot(tx_counties_sf) + geom_sf(color='#808080',stroke=0.1,aes(fill=`Flaring2018Volumes`))+
#       scale_fill_gradient(low='white', high='#377369', na.value= 'white',name="2018 Gas Production\n by County as %\n of State Total") + theme_void()
#     map_flaringTX2018
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

          # 8.  ---- **THIS CODE LOADS 2006 & 2018 Alcohol Impaired Crashes, DATA SOURCE: TxDOT (2020) ----

duicrashesTX <- read.csv("https://dataverse.harvard.edu/api/access/datafile/4193442",skip=2)
duicrashesTX$County <- toupper(duicrashesTX$County)
      
      # ---- THIS CODE MERGES THIS SECTIONS DATA WITH THE KEY OBJECT, "tx_counties_sf"
tx_counties_sf <- merge(tx_counties_sf,duicrashesTX, by.x="CNTY_NM",by.y="County")

      # 8a. ---- I CALCULATE DUI CRASH RATES PER 100K POP. AND THEN CALCULATE THE CHANGE IN SUCH RATES FROM 2006 TO 2018.
tx_counties_sf$Total2006DUICrashes_Per100K <- tx_counties_sf$Total2006DUICrashes * (100000/ tx_counties_sf$Pop2006 )
tx_counties_sf$Total2018DUICrashes_Per100K <- tx_counties_sf$Total2018DUICrashes * (100000/ tx_counties_sf$Pop2018 ) 
tx_counties_sf$CHNG_DUICrashes_Per100K <- tx_counties_sf$Total2018DUICrashes_Per100K - tx_counties_sf$Total2006DUICrashes_Per100K 

      # 8b. ---- I CALCULATE THE FATAL DUI CRASH RATES PER 100K POP. AND THEN CALCULATE THE CHANGE IN SUCH RATES FROM 2006 TO 2018.
tx_counties_sf$Fatal2006DUICrashes_Per100K <- tx_counties_sf$Fatal2006DUICrashes * (100000/ tx_counties_sf$Pop2006 )
tx_counties_sf$Fatal2018DUICrashes_Per100K <- tx_counties_sf$Fatal2018DUICrashes * (100000/ tx_counties_sf$Pop2018 )
tx_counties_sf$CHNG_FatalDUI_Per100K <- tx_counties_sf$Fatal2018DUICrashes_Per100K - tx_counties_sf$Fatal2006DUICrashes_Per100K

      # 8c. ---- I CALCULATE THE FATAL DUI CRASH RATES PER CAPITA. AND THEN CALCULATE THE CHANGE IN SUCH RATES FROM 2006 TO 2018.
tx_counties_sf$Fatal2006DUICrashes_PerCapita <- tx_counties_sf$Fatal2006DUICrashes/ tx_counties_sf$Pop2006
tx_counties_sf$Fatal2018DUICrashes_PerCapita <- tx_counties_sf$Fatal2018DUICrashes/ tx_counties_sf$Pop2018
tx_counties_sf$CHNG_FatalDUI_PerCapita <- tx_counties_sf$Fatal2018DUICrashes_PerCapita - tx_counties_sf$Fatal2006DUICrashes_PerCapita
 
      # 8c. ---- I CALCULATE THE FATAL DUI CRASH RATES PER CAPITA. AND THEN CALCULATE THE CHANGE IN SUCH RATES FROM 2006 TO 2018.
tx_counties_sf$PCT_CHNG_DUICrashes <- 100*((tx_counties_sf$Total2018DUICrashes - tx_counties_sf$Total2006DUICrashes)/tx_counties_sf$Total2006DUICrashes)
tx_counties_sf$PCT_CHNG_FatalDUICrashes <- 100*((tx_counties_sf$Fatal2018DUICrashes_Per100K - tx_counties_sf$Fatal2006DUICrashes_Per100K)/tx_counties_sf$Fatal2006DUICrashes_Per100K)
tx_counties_sf$PCT_CHNG_FatalDUICrashes <- replace(tx_counties_sf$PCT_CHNG_FatalDUICrashes,tx_counties_sf$CHNG_DUICrashes_Per100K==0,0)
tx_counties_sf[is.na(tx_counties_sf)]<- 0
  
  tx_counties_sf$PCT_CHNG_FatalDUICrashes[tx_counties_sf$PCT_CHNG_FatalDUICrashes == Inf] <- tx_counties_sf$Fatal2018DUICrashes_Per100K
      summary(tx_counties_sf$PCT_CHNG_FatalDUICrashes)  
      
          # 9.  ---- **THIS CODE LOADS 2006 & 2018 ALL CRASHES, DATA SOURCE: TxDOT (2020) ----
  allcrashesTX <- read.csv("https://dataverse.harvard.edu/api/access/datafile/4193481",skip=2)
  allcrashesTX$County <- toupper(allcrashesTX$County)

      # ---- THIS CODE MERGES THIS SECTIONS DATA WITH THE KEY OBJECT, "tx_counties_sf"
tx_counties_sf <- merge(tx_counties_sf,allcrashesTX, by.x="CNTY_NM",by.y="County")
tx_counties_sf[is.na(tx_counties_sf)]<- 0

tx_counties_sf$Crashes2006_Per100K <- tx_counties_sf$Total2006Crashes * (100000 / tx_counties_sf$Pop2006)
tx_counties_sf$Crashes2018_Per100K <- tx_counties_sf$Total2018Crashes * (100000 / tx_counties_sf$Pop2018)
tx_counties_sf$CHNG_Crashes_Per100K <- (tx_counties_sf$Crashes2018_Per100K - tx_counties_sf$Crashes2006_Per100K)
  
tx_counties_sf$Crashes2006_PerCapita <- tx_counties_sf$Total2006Crashes/tx_counties_sf$Pop2006
tx_counties_sf$Crashes2018_PerCapita <- tx_counties_sf$Total2018Crashes/tx_counties_sf$Pop2018
tx_counties_sf$CHNG_Crashes_PerCapita <- (tx_counties_sf$Crashes2018_PerCapita - tx_counties_sf$Crashes2006_PerCapita)

tx_counties_sf$FatalCrashes2006_Per100K <- tx_counties_sf$Fatal2006Crashes * (100000/ tx_counties_sf$Pop2006 )
tx_counties_sf$FatalCrashes2018_Per100K <- tx_counties_sf$Fatal2018Crashes * (100000/ tx_counties_sf$Pop2018 )
tx_counties_sf$CHNG_FatalCrashes_Per100K <- (tx_counties_sf$FatalCrashes2018_Per100K - tx_counties_sf$FatalCrashes2006_Per100K)
  
tx_counties_sf$FatalCrashes2006_PerCapita <- tx_counties_sf$Fatal2006Crashes/tx_counties_sf$Pop2006
tx_counties_sf$FatalCrashes2018_PerCapita <- tx_counties_sf$Fatal2018Crashes/tx_counties_sf$Pop2018
tx_counties_sf$CHNG_FatalCrashes_PerCapita <- (tx_counties_sf$FatalCrashes2018_PerCapita - tx_counties_sf$FatalCrashes2006_PerCapita)

tx_counties_sf[is.na(tx_counties_sf)]<- 0
#_____________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

          # 10. ---- **THIS CODE LOADS IN HIV PREVALENCE RATES, DATA SOURCE: U.S. CDC & TxDSHS (2020) ----      
  HIV_tx_counties_2006to2010and2018 <- read.csv("https://dataverse.harvard.edu/api/access/datafile/4193443", skip=7)
  HIV_tx_counties_2006to2010and2018$County <- toupper(HIV_tx_counties_2006to2010and2018$County)

      # ---- THIS CODE MERGES THIS SECTIONS DATA WITH THE KEY OBJECT, "tx_counties_sf"
tx_counties_sf <- merge(tx_counties_sf,HIV_tx_counties_2006to2010and2018,by.x='CNTY_NM',by.y="County")

# ---- I CALCULATE THE CHANGE IN HIV PREVALENCE RATE FROM 2006 TO 2018
tx_counties_sf$CHNG_HIV_Prevalence <- tx_counties_sf$PLWH_Rate2018 - tx_counties_sf$PLWH_Rate2006
tx_counties_sf$PCT_CHNG_HIV_Prevalence <- 100*((tx_counties_sf$PLWH_Rate2018 - tx_counties_sf$PLWH_Rate2006)/tx_counties_sf$PLWH_Rate2006)
tx_counties_sf$PCT_CHNG_HIV_Prevalence[tx_counties_sf$PCT_CHNG_HIV_Prevalence==Inf]<-100

          # 11. ---- **THIS CODE READS IN PERCENT OF PERSONS 25 YEARS OF AGE OR OLDER WHO ARE HIGH SCHOOL, DATA SOURCE: TEXAS DEMOGRAPHIC CENTER (2020) ----
  education2000<-read.table("https://demographics.texas.gov/Resources/Decennial/2000/DP2_4/County/tab-003.txt",
                          strip.white = TRUE, fill=TRUE,skip=14,blank.lines.skip=TRUE)[,c(1,2,5,6)]
  education2000$V1 <- toupper(education2000$V1)

  education2000$V1 <- replace(education2000$V1,education2000$V1=="DEAF","DEAF SMITH")
  education2000$V5 <- replace(education2000$V5,education2000$V6==60.9,60.9)
  education2000$V1 <- replace(education2000$V1,education2000$V1=="DEWITT","DE WITT")
  education2000$V1 <- replace(education2000$V1,education2000$V1=="EL","EL PASO")
  education2000$V5 <- replace(education2000$V5,education2000$V6==65.8,65.8)
  education2000$V1 <- replace(education2000$V1,education2000$V1=="FORT","FORT BEND")
  education2000$V5 <- replace(education2000$V5,education2000$V6==84.3,84.3)
  education2000$V1 <- replace(education2000$V1,education2000$V1=="JEFF","JEFF DAVIS")
  education2000$V5 <- replace(education2000$V5,education2000$V6==74.7,74.7)
  education2000$V1 <- replace(education2000$V1,education2000$V2=="Hogg","JIM HOGG")
  education2000$V5 <- replace(education2000$V5,education2000$V6==58.0,58.0)
  education2000$V1 <- replace(education2000$V1,education2000$V2=="Wells","JIM WELLS")
  education2000$V5 <- replace(education2000$V5,education2000$V6==64.8,64.8)
  education2000$V1 <- replace(education2000$V1,education2000$V2=="Salle","LA SALLE")
  education2000$V5 <- replace(education2000$V5,education2000$V6==50.1,50.1)
  education2000$V1 <- replace(education2000$V1,education2000$V2=="Oak","LIVE OAK")
  education2000$V5 <- replace(education2000$V5,education2000$V6==67.1,67.1)
  education2000$V1 <- replace(education2000$V1,education2000$V2=="River","RED RIVER")
  education2000$V5 <- replace(education2000$V5,education2000$V6==65.7,65.7)
  education2000$V1 <- replace(education2000$V1,education2000$V2=="Augustine","SAN AUGUSTINE")
  education2000$V5 <- replace(education2000$V5,education2000$V6==69.9,69.9)
  education2000$V1 <- replace(education2000$V1,education2000$V2=="Jacinto","SAN JACINTO")
  education2000$V5 <- replace(education2000$V5,education2000$V6==72.6,72.6)
  education2000$V1 <- replace(education2000$V1,education2000$V2=="Pinto","PALO PINTO")
  education2000$V5 <- replace(education2000$V5,education2000$V6==71.2,71.2)
  education2000$V1 <- replace(education2000$V1,education2000$V2=="Patricio","SAN PATRICIO")
  education2000$V5 <- replace(education2000$V5,education2000$V6==71.4,71.4)
  education2000$V1 <- replace(education2000$V1,education2000$V2=="Saba","SAN SABA")
  education2000$V5 <- replace(education2000$V5,education2000$V6==70.0,70.0)
  education2000$V1 <- replace(education2000$V1,education2000$V2=="Green","TOM GREEN")
  education2000$V5 <- replace(education2000$V5,education2000$V6==76.2,76.2)
  education2000$V1 <- replace(education2000$V1,education2000$V2=="Verde","VAL VERDE")
  education2000$V5 <- replace(education2000$V5,education2000$V6==58.7,58.7)
  education2000$V1 <- replace(education2000$V1,education2000$V2=="Zandt","VAN ZANDT")
  education2000$V5 <- replace(education2000$V5,education2000$V6==72.0,72.0)

      # ---- HERE, I CREATE CODE THAT GETS RID OF ALL COLUMNS EXCEPT COUNTY NAME AND HS ATTAINMENT PERCENTAGES FOR TX COUNTIES IN 2000, THEN REMOVES NA ROWS
  education2000 = subset(education2000, select = -c(V2,V6) )
  education2000<-na.omit(education2000)

      # ---- I ADJUST COLUMN NAMES HERE
  names(education2000)[names(education2000) == "V1"] <- "County"
  names(education2000)[names(education2000) == "V5"] <- "HS_Educ_PCT2000"

      # ---- THIS CODE MERGES THIS SECTIONS DATA WITH THE KEY OBJECT, "tx_counties_sf"
tx_counties_sf <- merge(tx_counties_sf,education2000, by.x="CNTY_NM", by.y="County")

      # ---- HERE I EXTRAPOLATE 2006 COUNTY HS POP. 25 YEARS AND OLDER  
      # ---- THAT COMPLETED AT LEAST A HIGH SCHOOL LEVEL EDUCATION. 
      # ---- I USE LEVELS FROM 2000 AND 2018 TO GENERATE AN ESTIMATE OF 2006 LEVELS 
tx_counties_sf$HS_Educ_PCT2006_EXTP <- ((tx_counties_sf$HS_Educ_PCT2000 * 2) + tx_counties_sf$HS_Educ_PCT2018)/3
tx_counties_sf$CHNG_Educ_Att <- tx_counties_sf$HS_Educ_PCT2018 - tx_counties_sf$HS_Educ_PCT2006_EXTP
tx_counties_sf$PCT_CHNG_Educ_Att <- 100*((tx_counties_sf$HS_Educ_PCT2018 - tx_counties_sf$HS_Educ_PCT2006_EXTP)/tx_counties_sf$HS_Educ_PCT2006_EXTP)

tx_counties_sf_more10K <- subset(tx_counties_sf,tx_counties_sf$Pop2018>10000)


          # 12. ---- **I CONDUCT SPATIAL ANALYSIS IN THIS SECTION. FIRST I CREATE A SPATIAL NEIGHBORS LIST  ----
      # ---- THE FUNCTION "poly2nb()" USES A QUEENS CONTIGUITY IN 1ST ORDER TO CREATE AN "nb" OR NEIGHBORS OBJECT
tx_counties_sf_nb <-  poly2nb(tx_counties_sf)
      # ---- I USE THE FUNCTION "nb2listw()" AND USE THE OBJECT I CREATE ABOVE "tx_counties_sf_nb" TO CREATE A SPATIALLY WEIGHTED LIST STANDARDIZED ON ROWS OVER SUMS
tx_counties_sf.lw <- nb2listw(tx_counties_sf_nb)

          # 12a.---- **THIS CODE CREATES SHADES OF BLUE AND RED COLORS FOR 90%/95%/99% HOT/COLD SPOT MAPS ----
  hotcoldspot_colors <-c("Cold Spot: 99% Conf."="#000059", "Cold Spot: 95% Conf."="#323299", "Cold Spot: 90% Conf."="#b2b2d8","Not Significant"="#e5e5e5", "Hot Spot: 90% Conf."="#e0b2b2","Hot Spot: 95% Conf."="#ad3232","Hot Spot: 99% Conf."="#6b0000")
tx_counties_sf_2ndOrderQueen<-nblag(tx_counties_sf_nb,2) 
tx_counties_sf_2ndOrderQueen<-nblag_cumul(tx_counties_sf_2ndOrderQueen)
tx_counties_sf.lw2 <- nb2listw(tx_counties_sf_2ndOrderQueen)


          # 12b.---- **HERE, I CALCULATE AND MAP CHANGE IN OIL/GAS HOTSPOTS GETIS-ORD Gi* STATISTIC ----
tx_counties_sf$HCS_CHNG_Gas <- as.numeric(localG(tx_counties_sf$CHNG_GasProd ,tx_counties_sf.lw))
tx_counties_sf$HCS_CHNG_Oil <- as.numeric(localG(tx_counties_sf$CHNG_OilProd ,tx_counties_sf.lw))

      # ---- THIS CODE CREATES DUMMY VARIABLES FOR CHANGE IN OIL AND NATURAL GAS HOT SPOTS
tx_counties_sf$CHNG_Gas_Hotspot95 <- ifelse(tx_counties_sf$HCS_CHNG_Gas>=1.96,1,0)
tx_counties_sf$CHNG_Oil_Hotspot95 <- ifelse(tx_counties_sf$HCS_CHNG_Oil>=1.96,1,0)

      # ---- THIS CODE TRANSORMS NATURAL GAS STATISTIC INTO CONFIDENCE LEVELS, FOR MAPPING PURPOSES
tx_counties_sf$HCS_CHNG_Gas_brks <- cut(tx_counties_sf$HCS_CHNG_Gas, 
                                        breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                        labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))

      # ---- THIS CODE CREATES MAP FOR HOT/COLD SPOT ANALYSIS FOR CHANGE IN GAS PRODUCTION
map_HCS_CHNG_Gas_brks <- ggplot(tx_counties_sf) + geom_sf(color='#e5e5e5',stroke=0.1,aes(fill=`HCS_CHNG_Gas_brks`))+
  scale_fill_manual(values=hotcoldspot_colors,name = "Getis-Ord Gi* \nHot/Cold Spot Analysis",guide=guide_legend(reverse = TRUE))+  theme_void(base_line_size = 0.1, base_size = 12)+
  theme(plot.caption = element_text(hjust = 0.5, size=12))+labs(caption = 'Chávez, Joel. 2020. "Gas Production hot spots, 2018." Texas A&M University')

      # ---- THIS CODE SAVES MAP PLOT ABOVE IN .PNG FORMAT
png(filename="C:/Users/newnu/OneDrive - Texas A&M University/PHD Application Files/WritingSampleFigures/map_HCS_CHNG_Gas_brks.png", 
    width=1200, height=1000, pointsize = 12)
plot(map_HCS_CHNG_Gas_brks)
dev.off()

      # ---- **MAPS FOR CHANGE IN  OIL PRODUCTION BELOW
      # ---- THIS CODE TRANSFORMS STATISTIC INTO CONFIDENCE LEVELS 
tx_counties_sf$HCS_CHNG_Oil_brks <- cut(tx_counties_sf$HCS_CHNG_Oil, 
                                        breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                        labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))

      # ---- THIS CODE CREATES MAP FOR HOT/COLD SPOT ANALYSIS FOR CHANGE IN OIL PRODUCTION
map_HCS_CHNG_Oil_brks <- ggplot(tx_counties_sf) + geom_sf(color='#e5e5e5',stroke=0.1,aes(fill=`HCS_CHNG_Oil_brks`))+
  scale_fill_manual(values=hotcoldspot_colors,name = "Getis-Ord Gi* \nHot/Cold Spot Analysis",guide=guide_legend(reverse = TRUE))+  theme_void(base_line_size = 0.1, base_size = 12)+
  theme(plot.caption = element_text(hjust = 0.5, size=12))+labs(caption = 'Chávez, Joel. 2020. "Gas Production hot spots, 2018." Texas A&M University')

      # ---- THIS CODE SAVES MAP PLOT ABOVE IN .PNG FORMAT
png(filename="C:/Users/newnu/OneDrive - Texas A&M University/PHD Application Files/WritingSampleFigures/map_HCS_CHNG_Oil_brks.png", 
    width=1200, height=1000, pointsize = 12)
plot(map_HCS_CHNG_Oil_brks)
dev.off()

          # 12b.---- **HERE, I CALCULATE AND MAP CHANGE IN MEDIAN HH INCOME GETIS ORD Gi* HOT SPOT ANALYSIS STATISTIC -----
tx_counties_sf$HCS_CHNG_MedianHHincome <- as.numeric(localG(tx_counties_sf$CHNG_MedianHHincome,tx_counties_sf.lw))
tx_counties_sf$HCS_CHNG_MedianHHincome_brks <- cut(tx_counties_sf$HCS_CHNG_MedianHHincome, 
                                                     breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                                     labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))

      # ---- THIS CODE CREATES MAP FOR HOT/COLD SPOT ANALYSIS FOR CHANGE IN CRASHES, 2008 TO 2018 
map_HCS_CHNG_MedianHHincome_brks <- ggplot(tx_counties_sf) + geom_sf(color='#e5e5e5',stroke=0.1,aes(fill=`HCS_CHNG_MedianHHincome_brks`))+
  scale_fill_manual(values=hotcoldspot_colors,name = "Getis-Ord Gi* \nHot/Cold Spot Analysis",guide=guide_legend(reverse = TRUE))+  theme_void(base_line_size = 0.1, base_size = 12)+
  theme(plot.caption = element_text(hjust = 0.5, size=12))+labs(caption = 'Chávez, Joel. 2020. "Change in Median Household Income in Texas Counties, 2006 to 2018." Texas A&M University')


      # ---- THIS CODE SAVES MAP PLOT ABOVE IN .PNG FORMAT
png(filename="C:/Users/newnu/OneDrive - Texas A&M University/PHD Application Files/WritingSampleFigures/map_HCS_CHNG_MedianHHincome_brks.png", 
    width=1200, height=1000, pointsize = 12)
plot(map_HCS_CHNG_MedianHHincome_brks)
dev.off()

tx_counties_sf$HHBivariateLISA_Oil_HIV <- ifelse((tx_counties_sf$CHNG_Oil_Hotspot + tx_counties_sf$CHNG_HIV_Prevalence_Hotspot)==2,1,0)

          # 12c.---- **HERE, I CALCULATE AND MAP CHANGE IN FATAL DUI CRASH RATE GETIS ORD Gi* HOT SPOT ANALYSIS STATISTIC ----
tx_counties_sf$HCS_CHNG_FatalDUI_Per100K <- as.numeric(localG(tx_counties_sf$CHNG_FatalDUI_Per100K,tx_counties_sf.lw))

      # ---- THIS CODE TRANSORMS STATISTIC INTO CONFIDENCE LEVELS, FOR MAPPING PURPOSES
tx_counties_sf$HCS_CHNG_FatalDUI_Per100K_brks <- cut(tx_counties_sf$HCS_CHNG_FatalDUI_Per100K, 
                                                   breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                                   labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))
      # ---- THIS CODE CREATES MAP FOR HOT/COLD SPOT ANALYSIS FOR CHANGE IN FATAL DUI CRASH RATE
map_HCS_CHNG_FatalDUI_Per100K_brks <- ggplot(tx_counties_sf) + geom_sf(color='#e5e5e5',stroke=0.1,aes(fill=`HCS_CHNG_FatalDUI_Per100K_brks`))+
          scale_fill_manual(values=hotcoldspot_colors,name = "Getis-Ord Gi* \nHot/Cold Spot Analysis",guide=guide_legend(reverse = TRUE))+  theme_void(base_line_size = 0.1, base_size = 12)+
          theme(plot.caption = element_text(hjust = 0.5, size=12))+labs(caption = 'Chávez, Joel. 2020. "Change in Fatal DUI Crashes in Texas Counties, 2006 to 2018." Texas A&M University')


      # ---- THIS CODE SAVES MAP PLOT ABOVE IN .PNG FORMAT 
png(filename="C:/Users/newnu/OneDrive - Texas A&M University/PHD Application Files/WritingSampleFigures/map_HCS_CHNG_FatalDUI_Per100K_brks.png", 
          width=1200, height=1000, pointsize = 12)
plot(map_HCS_CHNG_FatalDUI_Per100K_brks)
dev.off()

          # 12d.---- **HERE, I CALCULATE AND MAP CHANGE IN FATAL CRASH RATE GETIS ORD Gi* HOT SPOT ANALYSIS STATISTIC  ----
tx_counties_sf$HCS_CHNG_FatalCrashes_Per100K <- as.numeric(localG(tx_counties_sf$CHNG_FatalCrashes_Per100K,tx_counties_sf.lw))
      
      # ---- THIS CODE TRANSORMS STATISTIC INTO CONFIDENCE LEVELS, FOR MAPPING PURPOSES
tx_counties_sf$HCS_CHNG_FatalCrashes_Per100K_brks <- cut(tx_counties_sf$HCS_CHNG_FatalCrashes_Per100K, 
                                                   breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                                   labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))
      
      # ---- THIS CODE CREATES MAP FOR HOT/COLD SPOT ANALYSIS FOR CHANGE IN FATAL CRASH RATE, 2006 TO 2018
map_HCS_CHNG_FatalCrashes_Per100K_brks <- ggplot(tx_counties_sf) + geom_sf(color='#e5e5e5',stroke=0.1,aes(fill=`HCS_CHNG_FatalCrashes_Per100K_brks`))+
        scale_fill_manual(values=hotcoldspot_colors,name = "Getis-Ord Gi* \nHot/Cold Spot Analysis",guide=guide_legend(reverse = TRUE))+  theme_void(base_line_size = 0.1, base_size = 12)+
        theme(plot.caption = element_text(hjust = 0.5, size=12))+labs(caption = 'Chávez, Joel. 2020. "Change in Fatal Crash Rate in Texas Counties, 2006 to 2018." Texas A&M University')
      
      # ---- THIS CODE SAVES MAP PLOT ABOVE IN .PNG FORMAT
png(filename="C:/Users/newnu/OneDrive - Texas A&M University/PHD Application Files/WritingSampleFigures/map_HCS_CHNG_FatalCrashes_Per100K_brks.png", 
          width=1200, height=1000, pointsize = 12)
plot(map_HCS_CHNG_FatalCrashes_Per100K_brks)
dev.off()   

          # 12e.---- **HERE, I CALCULATE AND MAP CHANGE IN CRASH RATE GETIS ORD Gi* HOT SPOT ANALYSIS STATISTIC  ----
tx_counties_sf$HCS_CHNG_Crashes_Per100K <- as.numeric(localG(tx_counties_sf$CHNG_Crashes_Per100K,tx_counties_sf.lw))
      summary(tx_counties_sf$HCS_CHNG_Crashes_Per100K)
      # ---- THIS CODE TRANSORMS STATISTIC INTO CONFIDENCE LEVELS, FOR MAPPING PURPOSES
tx_counties_sf$HCS_CHNG_Crashes_Per100K_brks <- cut(tx_counties_sf$HCS_CHNG_Crashes_Per100K, 
                                                               breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                                               labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))
      
      # ---- THIS CODE CREATES MAP FOR HOT/COLD SPOT ANALYSIS FOR CHANGE IN CRASH RATE, 2006 TO 2018 
map_HCS_CHNG_Crashes_Per100K_brks <- ggplot(tx_counties_sf) + geom_sf(color='#e5e5e5',stroke=0.1,aes(fill=`HCS_CHNG_Crashes_Per100K_brks`))+
        scale_fill_manual(values=hotcoldspot_colors,name = "Getis-Ord Gi* \nHot/Cold Spot Analysis",guide=guide_legend(reverse = TRUE))+  theme_void(base_line_size = 0.1, base_size = 12)+
        theme(plot.caption = element_text(hjust = 0.5, size=12))+labs(caption = 'Chávez, Joel. 2020. "Change in Crash Rate in Texas Counties, 2006 to 2018." Texas A&M University')
      
      # ---- THIS CODE SAVES MAP PLOT ABOVE IN .PNG FORMAT
png(filename="C:/Users/newnu/OneDrive - Texas A&M University/PHD Application Files/WritingSampleFigures/map_HCS_CHNG_Crashes_Per100K_brks.png", 
          width=1200, height=1000, pointsize = 12)
plot(map_HCS_CHNG_Crashes_Per100K_brks)
dev.off()   
          # 12e.---- **HERE, I CALCULATE AND MAP CHANGE IN DUI CRASH RATE GETIS ORD Gi* HOT SPOT ANALYSIS STATISTIC  ----
tx_counties_sf$HCS_CHNG_DUICrashes_Per100K <- as.numeric(localG(tx_counties_sf$CHNG_DUICrashes_Per100K,tx_counties_sf.lw))
summary(tx_counties_sf$HCS_CHNG_DUICrashes_Per100K)
      # ---- THIS CODE TRANSORMS STATISTIC INTO CONFIDENCE LEVELS, FOR MAPPING PURPOSES
tx_counties_sf$HCS_CHNG_DUICrashes_Per100K_brks <- cut(tx_counties_sf$HCS_CHNG_DUICrashes_Per100K, 
                                                    breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                                    labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))

# ---- THIS CODE CREATES MAP FOR HOT/COLD SPOT ANALYSIS FOR CHANGE IN CRASH RATE, 2006 TO 2018 
map_HCS_CHNG_DUICrashes_Per100K_brks <- ggplot(tx_counties_sf) + geom_sf(color='#ffffff',stroke=0.1,aes(fill=`HCS_CHNG_DUICrashes_Per100K_brks`))+
  scale_fill_manual(values=hotcoldspot_colors,name = "Getis-Ord Gi* \nHot/Cold Spot Analysis",guide=guide_legend(reverse = TRUE))+  theme_void(base_line_size = 0.1, base_size = 12)+
  theme(plot.caption = element_text(hjust = 0.5, size=12))+labs(caption = 'Chávez, Joel. 2020. "Change in Crash Rate in Texas Counties, 2006 to 2018." Texas A&M University')

# ---- THIS CODE SAVES MAP PLOT ABOVE IN .PNG FORMAT
png(filename="C:/Users/newnu/OneDrive - Texas A&M University/PHD Application Files/WritingSampleFigures/HCS_CHNG_DUICrashes_Per100K_brks.png", 
    width=1200, height=1000, pointsize = 12)
plot(map_HCS_CHNG_DUICrashes_Per100K_brks)
dev.off() 
          # 12f.---- **HERE, I CALCULATE AND MAP CHANGE IN HIV PREVALENCE RATE GETIS ORD Gi* HOT SPOT ANALYSIS STATISTIC  ----
tx_counties_sf$HCS_CHNG_HIV_Prevalence <- as.numeric(localG(tx_counties_sf$CHNG_HIV_Prevalence,tx_counties_sf.lw))
tx_counties_sf$CHNG_HIV_Prevalence_Hotspot <- ifelse(tx_counties_sf$HCS_CHNG_HIV_Prevalence>=1.65,1,0)

      # ---- THIS CODE TRANSORMS STATISTIC INTO CONFIDENCE LEVELS, FOR MAPPING PURPOSES
tx_counties_sf$HCS_CHNG_HIV_Prevalence_brks <- cut(tx_counties_sf$HCS_CHNG_HIV_Prevalence, 
                                                           breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                                           labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))

      # ---- CREATES MAP FOR HOT/COLD SPOT ANALYSIS FOR CHANGE IN CRASHES, 2008 TO 2018 ***
map_HCS_CHNG_HIV_Prevalence_brks <- ggplot(tx_counties_sf) + geom_sf(color='black',stroke=0.1,aes(fill=`HCS_CHNG_HIV_Prevalence_brks`))+
  scale_fill_manual(values=hotcoldspot_colors,name = "Getis-Ord Gi* \nHot/Cold Spot Analysis",guide=guide_legend(reverse = TRUE))+  theme_void(base_line_size = 0.1, base_size = 12)+
  theme(plot.caption = element_text(hjust = 0.5, size=12))+labs(caption = 'Chávez, Joel. 2020. "Change in HIV Prevalence Rates, 2006 to 2018." Texas A&M University')

# ---- SAVING MAP PLOT IN .png ***
png(filename="C:/Users/newnu/OneDrive - Texas A&M University/PHD Application Files/WritingSampleFigures/map_HCS_CHNG_HIV_PREVALENCE_brks.png", 
    width=1200, height=1000, pointsize = 12)
plot(map_HCS_CHNG_HIV_Prevalence_brks)
dev.off() 

          # 12g.---- **HERE, I CALCULATE AND MAP CHANGE IN EDUCATIONAL ATTAINMENT % GETIS ORD Gi* HOT SPOT ANALYSIS STATISTIC  ----
tx_counties_sf$HCS_CHNG_Education <- as.numeric(localG(tx_counties_sf$CHNG_Educ_Att ,tx_counties_sf.lw))

      # ---- THIS CODE TRANSORMS STATISTIC INTO CONFIDENCE LEVELS, FOR MAPPING PURPOSES
tx_counties_sf$HCS_CHNG_Education_brks <- cut(tx_counties_sf$HCS_CHNG_Education, 
                                              breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                              labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))

      # ---- THIS CODE CREATES MAP FOR HOT/COLD SPOT ANALYSIS FOR CHANGE IN CHANGE IN EDUCATIONAL ATTAINMENT %
map_HCS_CHNG_Education_brks <- ggplot(tx_counties_sf) + geom_sf(color='black',stroke=0.1,aes(fill=`HCS_CHNG_Education_brks`))+
  scale_fill_manual(values=hotcoldspot_colors,name = "Getis-Ord Gi* \nHot/Cold Spot Analysis",guide=guide_legend(reverse = TRUE))+  theme_void(base_line_size = 0.1, base_size = 12)+
  theme(plot.caption = element_text(hjust = 0.5, size=12))+labs(caption = 'Chávez, Joel. 2020. "Change in HS Attainment Rates in Texas counties, 2006 to 2018." Texas A&M University')
      
      # ---- THIS CODE SAVES MAP PLOT ABOVE IN .PNG FORMAT
png(filename="C:/Users/newnu/OneDrive - Texas A&M University/PHD Application Files/WritingSampleFigures/map_HCS_CHNG_Education_brks.png", 
    width=1200, height=1000, pointsize = 12)
plot(map_HCS_CHNG_Education_brks)
dev.off() 
          # 12H.---- **HERE, I CALCULATE AND MAP 2006 GAS PRODUCTION GETIS ORD Gi* HOT SPOT ANALYSIS STATISTIC  ----
tx_counties_sf[is.na(tx_counties_sf)]<- 0
gas_hotcoldspot_colors <-c("Cold Spot: 99% Conf."="#000059", "Cold Spot: 95% Conf."="#323299", "Cold Spot: 90% Conf."="#b2b2d8","Not Significant"="#e5e5e5", "Hot Spot: 90% Conf."="#81DEFF","Hot Spot: 95% Conf."="#00B0F0","Hot Spot: 99% Conf."="#0078A2")

tx_counties_sf$HCS_GasProd2006 <- as.numeric(localG(tx_counties_sf$GasProd2006_MCF,tx_counties_sf.lw))

      # ---- THIS CODE TRANSORMS STATISTIC INTO CONFIDENCE LEVELS, FOR MAPPING PURPOSES
tx_counties_sf$HCS_GasProd2006_brks <- cut(tx_counties_sf$HCS_GasProd2006, 
                                           breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                           labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))

      # ---- THIS CODE CREATES MAP FOR HOT/COLD SPOT ANALYSIS FOR GAS PRODUCTION IN 2006
map_HCS_GasProd2006_brks <- ggplot(tx_counties_sf) + geom_sf(color='#e5e5e5',stroke=0.1,aes(fill=`HCS_GasProd2006_brks`))+
  scale_fill_manual(values=gas_hotcoldspot_colors,name = "Getis-Ord Gi* \nHot/Cold Spot Analysis",guide=guide_legend(reverse = TRUE))+  theme_void(base_line_size = 0.1, base_size = 12)+
  theme(plot.caption = element_text(hjust = 0.5, size=12))+labs(caption = 'Chávez, Joel. 2020. "Gas Production Hot Spots, 2006." Texas A&M University')


      # ---- THIS CODE SAVES MAP PLOT ABOVE IN .PNG FORMAT
png(filename="C:/Users/newnu/OneDrive - Texas A&M University/PHD Application Files/WritingSampleFigures/map_HCS_GasProd2006_brks.png", 
    width=1200, height=1000, pointsize = 12)
plot(map_HCS_GasProd2006_brks)
dev.off()

          # 12i.---- **HERE, I CALCULATE AND MAP 2018 GAS PRODUCTION GETIS ORD Gi* HOT SPOT ANALYSIS STATISTIC  ----
tx_counties_sf$HCS_GasProd2018 <- as.numeric(localG(tx_counties_sf$GasProd2018_MCF,tx_counties_sf.lw))
      # ---- THIS CODE TRANSORMS STATISTIC INTO CONFIDENCE LEVELS, FOR MAPPING PURPOSES
tx_counties_sf$HCS_GasProd2018_brks <- cut(tx_counties_sf$HCS_GasProd2018, 
                                           breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                           labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))

      # ---- THIS CODE CREATES MAP FOR HOT/COLD SPOT ANALYSIS FOR GAS PRODUCTION IN 2018
map_HCS_GasProd2018_brks <- ggplot(tx_counties_sf) + geom_sf(color='#e5e5e5',stroke=0.1,aes(fill=`HCS_GasProd2018_brks`))+
  scale_fill_manual(values=gas_hotcoldspot_colors,name = "Getis-Ord Gi* \nHot/Cold Spot Analysis",guide=guide_legend(reverse = TRUE))+  theme_void(base_line_size = 0.1, base_size = 12)+
  theme(plot.caption = element_text(hjust = 0.5, size=12))+labs(caption = 'Chávez, Joel. 2020. "Gas Production hot spots, 2018." Texas A&M University')


      # ---- THIS CODE SAVES MAP PLOT ABOVE IN .PNG FORMAT
  png(filename="C:/Users/newnu/OneDrive - Texas A&M University/PHD Application Files/WritingSampleFigures/map_HCS_GasProd2018_brks2.png", 
    width=1200, height=1000, pointsize = 12)
  plot(map_HCS_GasProd2018_brks)
  dev.off()
          # 12j.---- **HERE, I CALCULATE AND MAP 2006 OIL PRODUCTION GETIS ORD Gi* HOT SPOT ANALYSIS STATISTIC ----
oil_hotcoldspot_colors <-c("Cold Spot: 99% Conf."="#000059", "Cold Spot: 95% Conf."="#323299", "Cold Spot: 90% Conf."="#b2b2d8","Not Significant"="#e5e5e5", "Hot Spot: 90% Conf."="#d2b48c","Hot Spot: 95% Conf."="#7e6c54","Hot Spot: 99% Conf."="#2a241c")

tx_counties_sf$HCS_OilProd2006_BBL <- as.numeric(localG(tx_counties_sf$OilProd2006_BBL,tx_counties_sf.lw))

      # ---- THIS CODE TRANSFORMS STATISTIC INTO CONFIDENCE LEVELS
tx_counties_sf$HCS_OilProd2006_BBL_brks <- cut(tx_counties_sf$HCS_OilProd2006_BBL, 
                                               breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                               labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))
      # ---- THIS CODE CREATES A MAP OF OIL PRODUCTION HOT SPOTS IN 2006
map_HCS_OilProd2006_BBL_brks <- ggplot(tx_counties_sf) + geom_sf(color='#e5e5e5',stroke=0.1,aes(fill=`HCS_OilProd2006_BBL_brks`))+
  scale_fill_manual(values=oil_hotcoldspot_colors,name = "Getis-Ord Gi* \nHot/Cold Spot Analysis",guide=guide_legend(reverse = TRUE))+  theme_void(base_line_size = 0.1, base_size = 12)+
  theme(plot.caption = element_text(hjust = 0.5, size=12))+labs(caption = 'Chávez, Joel. 2020. "Getis Ord Gi* Hot/Cold Spot Analysis on 2006\n Oil Production by Texas Counties" Texas A&M University')

      # ---- THIS CODE SAVES MAP PLOT ABOVE IN .PNG FORMAT
png(filename="C:/Users/newnu/OneDrive - Texas A&M University/PHD Application Files/WritingSampleFigures/map_HCS_OilProd2006_BBL_brks.png", 
    width=1200, height=1000, pointsize = 12)
plot(map_HCS_OilProd2006_BBL_brks)
dev.off()


          # 12k.---- **HERE, I CALCULATE AND MAP 2018 OIL PRODUCTION GETIS ORD Gi* HOT SPOT ANALYSIS STATISTIC ----
tx_counties_sf$HCS_OilProd2018 <- as.numeric(localG(tx_counties_sf$OilProd2018_BBL,tx_counties_sf.lw))

      # ---- THIS CODE TRANSFORMS STATISTIC INTO CONFIDENCE LEVELS
tx_counties_sf$HCS_OilProd2018_BBL_brks <- cut(tx_counties_sf$HCS_OilProd2018, 
                                                  breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                                  labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))

      # ---- THIS CODE CREATES MAP FOR HOT SPOTS IN OIL PRODUCTION, 2018
map_HCS_OilProd2018_BBL_brks <- ggplot(tx_counties_sf) + geom_sf(color='#e5e5e5',stroke=0.1,aes(fill=`HCS_OilProd2018_BBL_brks`))+
  scale_fill_manual(values=oil_hotcoldspot_colors,name = "Getis-Ord Gi* \nHot/Cold Spot Analysis",guide=guide_legend(reverse = TRUE))+  theme_void(base_line_size = 0.1, base_size = 12)+
  theme(plot.caption = element_text(hjust = 0.5, size=12))+labs(caption = 'Chávez, Joel. 2020. "Getis Ord Gi* Hot/Cold Spot Analysis of 2018\n Oil Production by Texas Counties" Texas A&M University')

      # ---- THIS CODE SAVES MAP PLOT ABOVE IN .PNG FORMAT -replace filepath if you wish to save map locally
#png(filename="C:/Users/newnu/OneDrive - Texas A&M University/PHD Application Files/WritingSampleFigures/map_HCS_OilProd2018_BBL_brks.png", 
#    width=1200, height=1000, pointsize = 12)
#plot(map_HCS_OilProd2018_BBL_brks)
#dev.off()




#******************************************************************************************************
#******************************************************************************************************
#******************************************************************************************************

tx_counties_sf$MoransI_CHNG_HIV_Prevalence <- as.numeric(localmoran(tx_counties_sf$CHNG_HIV_Prevalence,tx_counties_sf.lw))
tx_counties_sf$MoransI_CHNG_HIV_Prevalence_brks <- cut(tx_counties_sf$`MoransI_CHNG_HIV_Prevalence.Pr(z > 0)`, 
                                                   breaks=c(-Inf,-2.58,-1.96,-1.65,1.65,1.96,2.58,Inf),
                                                   labels=c("Cold Spot: 99% Conf.", "Cold Spot: 95% Conf.", "Cold Spot: 90% Conf.","Not Significant", "Hot Spot: 90% Conf.","Hot Spot: 95% Conf.","Hot Spot: 99% Conf."))

summary(tx_counties_sf$`MoransI_CHNG_HIV_Prevalence.Pr(z > 0)`)

tx_counties_sf$HHBivariateLISA_Oil_HIV <- ifelse((tx_counties_sf$CHNG_Oil_Hotspot + tx_counties_sf$CHNG_HIV_Prevalence_Hotspot)==2,1,0)
tx_counties_sf$HHBivariateLISA_Oil_Crashes <- ifelse((tx_counties_sf$CHNG_Oil_Hotspot + tx_counties_sf$CHNG_HIV_Prevalence_Hotspot)==2,1,0)

tx_counties_sf$HHBivariateLISA_Oil_HIV
ggplot(tx_counties_sf)+geom_sf(color='#e5e5e5',stroke=0.1,aes(fill=`HHBivariateLISA_Oil_HIV`))
          # 13. ---- **THIS SECTION IS WHERE I RUN LEAST-SQUARES REGRESSION MODELING ----
      # ---- DOUBLE CHECKING THAT "NA" WILL BE TREATED AS ZERO
tx_counties_sf[is.na(tx_counties_sf)]<- 0

      # ---- HERE I FORMAT DIGITS TO BE TO 2 DECIMAL POINTS, AND INCLUDE SCIENTIFIC NOTATION
options(digits = 3, scipen = 0)

#Model A  # 13a.---- **HERE, I REGRESS CHANGE IN HIV RATES REGRESSION ON SELECTED VARIABLES *NO REGIONAL-FIXED EFFECTS, ** NO PETROLEUM PRODUCTION ----
LeastSquaresReg_CHNG_HIV_Prevalence_NoFixedRegion_NoPetro <- lm(CHNG_HIV_Prevalence ~ CHNG_MedianHHincome+CHNG_Poverty+CHNG_Educ_Att+CHNG_MaleToFemaleRatio+
                                                                  CHNG_White+CHNG_Hispanic+CHNG_Black,
                                                                data=tx_counties_sf)
summary(LeastSquaresReg_CHNG_HIV_Prevalence_NoFixedRegion_NoPetro)
write.csv((coef(summary(LeastSquaresReg_CHNG_HIV_Prevalence_NoFixedRegion_NoPetro))),file="C:/Users/newnu/Downloads/LeastSquaresReg_CHNG_HIV_Prevalence_NoFixedRegion_NoPetro.csv")

#Model B  # 13b.---- **HERE, I REGRESS CHANGE IN HIV RATES REGRESSION ON SELECTED VARIABLES *NO REGION-FIXED EFFECTS **INCLUDES PETROLEUM PRODUCTION ---- 
LeastSquaresReg_CHNG_HIV_Prevalence_Race_NoFixedEffects <- lm(CHNG_HIV_Prevalence ~ CHNG_MedianHHincome+CHNG_Poverty+CHNG_Educ_Att+CHNG_MaleToFemaleRatio+
                                                                CHNG_White+CHNG_Hispanic+CHNG_Black+CHNG_OilProd+CHNG_GasProd_cf,
                                                              data=tx_counties_sf)
summary(LeastSquaresReg_CHNG_HIV_Prevalence_Race_NoFixedEffects)
write.csv((coef(summary(LeastSquaresReg_CHNG_HIV_Prevalence_Race_NoFixedEffects))),file="C:/Users/newnu/Downloads/LeastSquaresReg_CHNG_HIV_Prevalence_Race_NoFixedEffects.csv") 

#Model I  # 13c.---- **HERE, I REGRESS CHANGE IN HIV RATES REGRESSION ON SELECTED VARIABLES *REGION-FIXED EFFECTS, **INCLUDES PETROLEUM PRODUCTION ---- 
LeastSquaresReg_CHNG_HIV_Prevalence <- lm(CHNG_HIV_Prevalence ~ CHNG_MedianHHincome+CHNG_Poverty+CHNG_Educ_Att+CHNG_MaleToFemaleRatio+CHNG_OilProd+CHNG_GasProd_cf+
                                            CHNG_White+CHNG_Hispanic+CHNG_Black+
                                            PH_Region1+PH_Region2+PH_Region3+PH_Region4+PH_Region5+PH_Region6+PH_Region7+PH_Region8+PH_Region10+PH_Region11+PH_Region9,
                                          data=tx_counties_sf)
summary(LeastSquaresReg_CHNG_HIV_Prevalence)
tx_counties_sf$Predicted_CHNG_HIV <-fitted(LeastSquaresReg_CHNG_HIV_Prevalence)

          # 13d.---- **HERE, I REGRESS CHANGE IN TOTAL CRASHES ON SELECTED VARIABLES *NO PETROLEUM PRODUCTION ----
LeastSquaresReg_CHNG_AllCrashes_NoPetro <- lm(CHNG_Crashes_Per100K ~ SqMiles+CHNG_65andOlder+CHNG_15to29+CHNG_MedianHHincome+CHNG_Poverty+CHNG_Educ_Att+CHNG_MaleToFemaleRatio,
                                              data=tx_counties_sf)
summary(LeastSquaresReg_CHNG_AllCrashes_NoPetro)

#Model II # 13e.---- **HERE, I REGRESS CHANGE IN TOTAL CRASHES ON SELECTED VARIABLES **INCLUDES PETROLEUM PRODUCTION ---- 
LeastSquaresReg_CHNG_AllCrashes <- lm(CHNG_Crashes_Per100K ~ CHNG_MedianHHincome+CHNG_Poverty+CHNG_Educ_Att+CHNG_MaleToFemaleRatio+
                                        CHNG_Oil_Hotspot95+CHNG_Gas_Hotspot95+SqMiles,
                                      data=tx_counties_sf)
summary(LeastSquaresReg_CHNG_AllCrashes)
write.csv((coef(summary(LeastSquaresReg_CHNG_AllCrashes))),file="C:/Users/newnu/Downloads/LeastSquaresReg_CHNG_AllCrashes.csv")


          # 13f.---- **HERE, I REGRESS CHANGE IN FATAL CRASH RATE ON SELECTED VARIABLES *NO PETROLEUM PRODUCTION ---- 
LeastSquaresReg_CHNG_FatalCrashes_NoPetro <- lm(CHNG_FatalCrashes_Per100K ~ CHNG_MedianHHincome+CHNG_Poverty+CHNG_Educ_Att+CHNG_MaleToFemaleRatio+SqMiles,
                                                data=tx_counties_sf)
summary(LeastSquaresReg_CHNG_FatalCrashes_NoPetro)

#Model III# 13g.---- **HERE, I REGRESS CHANGE IN FATAL CRASH RATE ON SELECTED VARIABLES **INCLUDES PETROLEUM PRODUCTION ---- 
LeastSquaresReg_CHNG_FatalCrashes <- lm(CHNG_FatalCrashes_Per100K ~ CHNG_MedianHHincome+CHNG_Poverty+CHNG_Educ_Att+CHNG_MaleToFemaleRatio+
                                          CHNG_Oil_Hotspot95 +CHNG_Gas_Hotspot95+SqMiles,
                                        data=tx_counties_sf)
summary(LeastSquaresReg_CHNG_FatalCrashes)
write.csv((coef(summary(LeastSquaresReg_CHNG_FatalCrashes))),file="C:/Users/newnu/Downloads/LeastSquaresReg_CHNG_FatalCrashes.csv")


#Model IV # 13h.---- **HERE, I REGRESS CHANGE IN ALCOHOL-IMPAIRED CRASH RATE ON SELECTED VARIABLES *INCLUDES PETROLEUM PRODUCTION ---- 
LeastSquaresReg_CHNG_DUI_Crashes <- lm(CHNG_DUICrashes_Per100K   ~ CHNG_MedianHHincome+CHNG_Poverty+CHNG_Educ_Att+CHNG_MaleToFemaleRatio+
                                         CHNG_Oil_Hotspot95 +CHNG_Gas_Hotspot95+SqMiles,
                                       data=tx_counties_sf)
summary(LeastSquaresReg_CHNG_DUI_Crashes)
write.csv((coef(summary(LeastSquaresReg_CHNG_DUI_Crashes))),file="C:/Users/newnu/Downloads/LeastSquaresReg_CHNG_DUI_Crashes.csv")

#Model V  # 13i.---- **HERE, I REGRESS CHANGE IN ALCOHOL-IMPAIRED FATAL CRASH RATE ON SELECTED VARIABLES *INCLUDES PETROLEUM PRODUCTION ---- 
LeastSquaresReg_CHNG_FatalDUI <- lm(CHNG_FatalDUI_Per100K   ~ CHNG_MedianHHincome+CHNG_Poverty+CHNG_Educ_Att+CHNG_MaleToFemaleRatio+
                                      CHNG_OilProd+CHNG_GasProd_cf+SqMiles,
                                    data=tx_counties_sf)
summary(LeastSquaresReg_CHNG_FatalDUI)
write.csv((coef(summary(LeastSquaresReg_CHNG_FatalDUI))),file="C:/Users/newnu/Downloads/LeastSquaresReg_CHNG_FatalDUI.csv")
#______________________________________________________________________________________________________________________________________________________________
#______________________________________________________________________________________________________________________________________________________________
#----
#----
         #**** THS CODE SAVES THE DATA FRAME AS A .CSV**** NOTE: replace file path with your custom path to save file locally----
        write.csv((st_set_geometry(tx_counties_sf, NULL)),file="C:/Users/newnu/Downloads/tx_counties_sf.csv")


#______________________________________________________________________________________________________________________________________________________________----
#______________________________________________________________________________________________________________________________________________________________----