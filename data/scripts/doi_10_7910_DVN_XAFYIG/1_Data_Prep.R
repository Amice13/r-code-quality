# The goal of this script is to get the data into a working data frame I can use for analysis. I will do this by first tabulating the total number of WAI and the number of unique sites for each WAI class at CBG level (Section A). I then make a data frame for the cluster outlier analysis based on three filters (Section B). I then create the data frame used in the logistic regression ("2_Paid_Public_Sites_Model.R") by getting 2023 5-year average ACS data, filtering any CBG with less that 250 people and missing demographic data (Section C).

library(openxlsx)
library(tidyverse)
library(tidycensus)
options(scipen = 999)

# A. Tabulate WAI, sites, and shoreline at CBG and tract level ------------------------

setwd("C:/Users/jeff.beauvais/Documents/Projects/CoastalVA_Access/Archive/Data/Tabluar") ## CHANGE TO YOUR WORKING DIRECTORY
Paid.Public <- read.csv("VIMS_WAI_Final_PaidPublic.csv") # This is the exported table for the "VIMS_WAI_Final_PaidPublic" layer in ArcPro. Made using the 'Table to Excel' tool.

# The goal here is to prepare the data for the cluster outlier analysis and logistic regression by summarizing how many total paid/public WAI and unique paid/public sites there are in each eligible CBG.

# First, let's just check how many unique CBGs (GEOID) there are.

length(unique(Paid.Public$GEOID))

# Now let's do the work. I want to see how many total WAI of each class there are (CBG.1). Then, we look at how many unique sites (CBG.2). Then we join them together. I replace all NA values with 0 here because it is a true zero. 

CBG.1 <- Paid.Public %>% count(GEOID, WAI_Class) %>% rename(Total_WAI = n) %>% pivot_wider(names_from = WAI_Class, names_glue = "{WAI_Class}_Total_WAI", values_from = Total_WAI) %>% replace(is.na(.), 0)
CBG.2 <- Paid.Public %>% filter(Unique_Site == 0) %>% count(GEOID,WAI_Class) %>% rename(Total_Sites = n) %>% pivot_wider(names_from = WAI_Class, names_glue = "{WAI_Class}_Total_Sites", values_from = Total_Sites) %>% replace(is.na(.), 0) 
CBG.Join <- inner_join(CBG.1, CBG.2, by = "GEOID") %>% mutate(Paid_Public_Total_WAI = Paid_Total_WAI + Public_Total_WAI,
                                                              Paid_Public_Total_Sites = Paid_Total_Sites + Public_Total_Sites)
rm(CBG.1, CBG.2)

# So we know how many paid/public WAI and sites there are in the CBGs that have them. Let's bring the underlying CBG info in and join them.

CBG.FullStudyArea <- read.csv("Boundaries_VA_CBGs_FullStudyArea.csv") # This is the exported table for the "Boundaries_VA_CBGs_FullStudyArea" layer in ArcPro. Made using the 'Table to Excel' tool.

unique(complete.cases(CBG.FullStudyArea)) # Making sure there are no NA values in the CBG data set. TRUE is the only unique value so everything seems good.

CBG.FullStudyArea <- left_join(CBG.FullStudyArea, CBG.Join, by = "GEOID") # Not going to replace the NAs in the Total_WAI/Sites columns introduced by the join to 0s here yet because landlocked CBGs should probably be an NA since they can't have WAI. We will remove those landlocked CBGs (as well as military/industrial) in Section B.

# So we have our paid/public WAI and sites joined to the CBG info. Now we want to calculate how much shoreline there is in each CBG/tract. I load in my shoreline data from ArcPro. 

Shoreline.Data <- read.csv("C:/Users/jeff.beauvais/Documents/R/CoastalVA_Access/Data/ArcPro_Data/LUBC_VIMS_EditsStepFinal.csv") # This is the exported table for the "LUBC_VIMS_EditsStepFinal" layer in ArcPro. Made using the 'Table to Excel' tool.

# I want to see the amount of shoreline grouped by the "Shore_Keep" column and the total amount of shoreline. If you wanted even finer-grain detail you could do the "Shore_Type_Final" column instead of Shore_Keep.

Shoreline.CBG <- Shoreline.Data %>% group_by(GEOID, Shore_Keep) %>% 
  summarise(Shoreline = sum(Shape_Length)/1000) %>% # Convert to km  
  pivot_wider(names_from = Shore_Keep, names_glue = "Shoreline_{Shore_Keep}", values_from = Shoreline) %>%
  replace(is.na(.), 0) %>% # Again, replace the NA with 0 because these are true 0
  mutate(Shoreline_Total = sum(Shoreline_Beach + Shoreline_Conservation + Shoreline_No + Shoreline_Yes)) %>%
  mutate(across(1:5, round, 2)) # Remember 1:5 means columns 2-6 b/c column 1 has location = 0

# So 793 CBGs (21.1% of all CBGs) have some amount of shoreline. Let's join that to the CBG data frame and save. 

CBG.FullStudyArea <- left_join(CBG.FullStudyArea, Shoreline.CBG, by = "GEOID")
CBG.FullStudyArea <- CBG.FullStudyArea[, c(1:3, 19:29, 4:18)] # Change around the column order for my own personal preference. Quicker to do by index in base R for this many columns.

# Now let's save these as Rda so we don't have to redo these steps.

setwd("C:/Users/jeff.beauvais/Documents/Projects/CoastalVA_Access/Archive/Data/Tabluar") ## CHANGE TO YOUR WORKING DIRECTORY
save(CBG.FullStudyArea, file = "CBG_FullStudyArea.Rda")

# B. CBG and tract filters 1-3 ------------------------------------------------

setwd("C:/Users/jeff.beauvais/Documents/Projects/CoastalVA_Access/Archive/Data/Tabluar") ## CHANGE TO YOUR WORKING DIRECTORY
load("CBG_FullStudyArea.Rda")

# First, we want to remove any CBGs that are labeled as military/industrial (filter 1). These are already part of the data I imported from ArcPro because it takes some visual inspection to determine these things. 

CBG.MI <- CBG.FullStudyArea %>% filter(Military_Industrial == "Yes") # No military/industrial CBGs w/ paid/public WAI, so that's good.
rm(CBG.MI)

CBG.Filter1 <- CBG.FullStudyArea %>% filter(Military_Industrial == "No")
rm(CBG.FullStudyArea)

# Next, let's remove the landlocked CBGs (filter 2).

CBG.Landlocked <- CBG.Filter1 %>% filter(is.na(Shoreline_Total)) # No landlocked CBGs w/ paid/public WAI, so that's good.
rm(CBG.Landlocked)

CBG.Filter2 <- CBG.Filter1 %>% filter(!is.na(Shoreline_Total))
rm(CBG.Filter1)

# Next, I want to remove anything that has <= than 100 m of "Yes" and "Conservation" shoreline and no paid/public WAI in it (filter 3). Before we start filtering by distance, I want to check whether any CBGs or tracts where the only shoreline is either "Beach" and/or "No" have any paid/public WAI. 

No.Check.CBG <- CBG.Filter2 %>% filter(Shoreline_Total - Shoreline_No == 0) # Look good. 7 CBGs only have "No" shoreline and none have any paid/public WAI.
rm(No.Check.CBG)

Beach.Check.CBG <- CBG.Filter2 %>% filter(Shoreline_Total - Shoreline_Beach == 0) # Small issue here. 15 CBGs only have "Beach" shoreline, but one of them (516500110012) has 2 public WAI at 1 site. Looking at these in the WAI data they are the James T. Wilson (OBJECTID 189) and Buckroe Beach Park (OBJECTID 190) public piers.

# So what to do here? I could either make an exception for that one CBG and include it in the study or cut that CBG out entirely. Because I care more about WAI and I do include a few other beach piers, I will make an exception for that one CBG and include it in the study. I probably should exclude those beach piers since I'm not really examining beaches, but there are so few I will let it slide for now. There are 4 other beach piers in my study and they are listed in the "Beaches" section of my "VA Access Complete Methods" document. Those other 4 occur in CBGs that have "Yes" shoreline, so I will just use the "Yes" shoreline for those. But CBG 516500110012 only has "Beach" shoreline" I don't want to consider beach shoreline in my models, so I will hard code the amount of "Beach" shoreline CBG 516500110012 as "Yes" shoreline and hard code the "Beach" shoreline to 0. Again, this is not ideal, but I am in a time crunch and don't want to remake the point density figures after removing these 5 beach piers, so this is what I'm going to do. It's only 5 WAI at 3 sites, so this is really a minor issue.

CBG.Filter2 <- CBG.Filter2 %>% mutate(Shoreline_Yes = case_when(GEOID == "516500110012" ~ Shoreline_Beach,
                                                                TRUE ~ Shoreline_Yes),
                                      Shoreline_Beach = case_when(GEOID == "516500110012" ~ 0,
                                                                  TRUE ~ Shoreline_Beach))
# Running the Beach.Check.CBG above again we see we successfully changed the shoreline information for CBG 516500110012 and did not disturb anything else.
rm(Beach.Check.CBG)

# Now let's filter based on minimum shoreline. I choose a 100 m cutoff threshold to ensure I'm only selecting CBGs with enough developable shoreline. This is because of both the way CBGs are drawn and the way VIMS determined their shoreline, since these aren't synced, some CBGs have tiny slivers of shoreline that couldn't reasonably be developed to contain WAI. You could of course edit this threshold as you wish to see if it affects results. Only one public WAI occurs in a CBG (515708304002) with <= 100 m of shoreline, so I preserve that one.

CBG.ShoreCutoff <- CBG.Filter2 %>% filter(Shoreline_Conservation + Shoreline_Yes <= 0.1) 

CBG.Filter3 <- CBG.Filter2 %>% filter(Shoreline_Conservation + Shoreline_Yes > 0.1 | GEOID == "515708304002")
rm(CBG.Filter2, CBG.ShoreCutoff)

# Now that we have all the CBGs that could feasibly have water/WAI, let's change any NA values (which should all be in the total WAI, sites, or shoreline columns) to 0, since these do represent true 0s. 

CBG.Filter3 <- CBG.Filter3 %>% replace(is.na(.), 0)

# Great. This is what I want to use for my cluster outlier analysis. First I save filter 3 on it's own. Then I make the data I will use in ArcPro for the cluster outlier analysis. I am only going to do this analysis at the site level, so I remove some of the unnecessary columns and then calculate two new density columns (sites/shoreline length and sites/total land area in sq. km). I then write these to Excel files so we can join them to the CBG layer in ArcPro. I write to xlsx format and not csv here because ArcPro natively handles xlsx files better than csv files in my experience (expecially for the GEOID because csv files don't preserve formatting).

setwd("C:/Users/jeff.beauvais/Documents/Projects/CoastalVA_Access/Archive/Data/Tabluar") ## CHANGE TO YOUR WORKING DIRECTORY
save(CBG.Filter3, file = "CBG_Filter3.Rda")

CBG.ClusterOutlier <- CBG.Filter3[ , c(1, 3:15, 24)]
rm(CBG.Filter3)

CBG.ClusterOutlier <- CBG.ClusterOutlier %>% mutate(Paid_Public_SiteDensity_Shore = Paid_Public_Total_Sites/(Shoreline_Conservation + Shoreline_Yes),
                                                    Paid_Public_SiteDensity_LandArea = (Paid_Public_Total_Sites/ALAND)*(10^6))

CBG.ClusterOutlier <- CBG.ClusterOutlier %>% mutate(across(c(16:17), ~ scales::rescale(.x, to = c(0, 1)))) # Scale all the density values between 0-1.

write.xlsx(CBG.ClusterOutlier, file = "ClusterOutlierData_CBGLevel.xlsx")

# C. Census data and filter 4 ---------------------------------------------

# Now we need to grab the ACS data we will use. I have updated all the paid/public to 2025, so we can use the most recently available ACS 5-year average for our demographic variables (as of 3/28/2025 the 2023 vintage was available via API). Let's get those now.

v23 <- load_variables(2023, "acs5", cache = TRUE)

ACS.Raw.CBG <- get_acs(geography = "block group", state = "Virginia", county = c(001, 013, 033, 036, 041, 057, 059, 073, 085, 087, 093, 095, 097, 099, 101, 103, 115, 119, 127, 131, 133, 149, 153, 159, 177, 179, 181, 193, 199, 510, 550, 570, 600, 610, 630, 650, 670, 683, 685, 700, 710, 730, 735, 740, 760, 800, 810, 830), year = 2023, variables = c(
  POP = "B03002_001",
  MHI = "B19013_001", 
  AGE = "B01002_001",
  EDUCATION_TOTAL = "B15003_001", EDUCATION_UNDERGRAD = "B15003_022", EDUCATION_MASTERS = "B15003_023", EDUCATION_PROF = "B15003_024", 
  EDUCATION_DOCTORATE = "B15003_025",
  T_OWNER = "B25003_002", T_RENTER = "B25003_003", MHV = "B25077_001", 
  YSB_2010 = "B25034_002", YSB_2000 = "B25034_003", YSB_1990= "B25034_004", YSB_1980 = "B25034_005", YSB_1970 = "B25034_006", YSB_1960 = "B25034_007", 
  YSB_1950 = "B25034_008", YSB_1940 = "B25034_009", YSB_EARLIER = "B25034_010"), output = 'wide')

# Now we remove the 12 empty CBGs that are only water. 

ACS.Raw.CBG <- ACS.Raw.CBG %>% filter(!(str_detect(NAME, 'Block Group 0')))

ACS.2023.CBG <- transmute(ACS.Raw.CBG, 
                      GEOID = GEOID, 
                      POP = POPE, 
                      MHI = MHIE, 
                      AGE = AGEE,
                      P_COLLEGE = ((EDUCATION_UNDERGRADE + EDUCATION_MASTERSE + EDUCATION_PROFE + EDUCATION_DOCTORATEE)/EDUCATION_TOTALE) * 100,
                      P_OWN = (T_OWNERE/(T_OWNERE + T_RENTERE)) * 100,
                      MHV = MHVE)

# Our ACS data is good to go. Let's join it to our CBG_Filter3 data

setwd("C:/Users/jeff.beauvais/Documents/Projects/CoastalVA_Access/Archive/Data/Tabluar") ## CHANGE TO YOUR WORKING DIRECTORY
load("CBG_Filter3.Rda")

CBG.Filter3 <- CBG.Filter3 %>% mutate(GEOID = as.character(GEOID), Tract_ID = as.character(Tract_ID)) # Convert to character for the join and use in the code.
CBG.Filter3 <- left_join(CBG.Filter3, ACS.2023.CBG, by = "GEOID")
CBG.Filter3 <- CBG.Filter3[, c(1:3, 30:35, 10:14, 4:9, 15:29)]

# Now let's filter by CBGs with too low a population (filter 4). I want to only keep CBGs with at least 250 people. This results in us losing 9 CBGs in 5 tracts with 29 total WAI at 7 sites. Three of these are CBGs that I did not label as military/industrial b/c they already had WAI in them. More on that and why I felt it was appropriate to keep them for the point density and cluster outlier analysis in the "Filter 1 - Military" section of my "VA Access Complete Methods" document. 

Low.Pop <- CBG.Filter3 %>% filter(POP <= 250)
CBG.Filter4 <- CBG.Filter3 %>% filter(POP > 250)
length(unique(CBG.Filter4$Tract_ID)) # Lose 5 tracts
rm(ACS.2023.CBG, CBG.Filter3, Low.Pop)

# Looking at this data we also see a problem - we have 55 CBGs in 50 tracts are missing either their MHI or MHV values. To deal with this, I will pull these variables at the tract level and use a case when statement to use the tract level MHI or MHV when the CBG level is missing.

Missing.MHI <- CBG.Filter4 %>% filter(is.na(MHI))
Missing.MHV <- CBG.Filter4 %>% filter(is.na(MHV))
Missing.Both <- CBG.Filter4 %>% filter(is.na(MHI) & is.na(MHV))
rm(Missing.MHI, Missing.MHV, Missing.Both)
Missing.Data <- CBG.Filter4 %>% filter(is.na(MHI) | is.na(MHV))
length(unique(Missing.Data$Tract_ID))

# Get the tract level data.

ACS.Missing.Tract <- get_acs(geography = "tract", state = "Virginia", county = c(001, 013, 033, 036, 041, 057, 059, 073, 085, 087, 093, 095, 097, 099, 101, 103, 115, 119, 127, 131, 133, 149, 153, 159, 177, 179, 181, 193, 199, 510, 550, 570, 600, 610, 630, 650, 670, 683, 685, 700, 710, 730, 735, 740, 760, 800, 810, 830), year = 2023, variables = c(
  MHI = "B19013_001", 
  MHV = "B25077_001"), output = 'wide')

Tract.Replace <- ACS.Missing.Tract %>% filter(GEOID %in% Missing.Data$Tract_ID)

# Looking at this table, only one tract is missing that MHI data, but 10 are missing MHV data. 

Missing.MHV.Tract <- Tract.Replace %>% filter(is.na(MHVE)) # Pull the tracts with no MHV values
No.MHV.Saving.CBG <- Missing.Data %>% filter(Tract_ID %in% Missing.MHV.Tract$GEOID) # How many CBGs can't get MHV values?

# Based on the two lines above. 11 CBGs are in tracts that also do not have MHV estimates. Instead of removing these CBGs from analysis, I will drop median home value from my model. MHV and MHI are probably collinear anyway. 

# Now let's look at that one tract without an MHI estimate. 

rm(Missing.MHV.Tract, No.MHV.Saving.CBG)
Missing.MHI.Tract <- Tract.Replace %>% filter(is.na(MHIE)) # Pull the one tract with missing MHI data.
No.MHI.Saving.CBG <- Missing.Data %>% filter(Tract_ID %in% Missing.MHI.Tract$GEOID) # Only 1 CBG (517100048001) with 3 WAI at 1 unique site.

# I'm ok with dropping one CBG, so let's do that and drop the MHV variable (filter 5). 

CBG.Filter4 <- CBG.Filter4 %>% select(-c("MHV")) %>% filter(GEOID != "517100048001")

# Now, let's replace the missing CBG level MHI values with their tract level value

Tract.Replace <- Tract.Replace %>% select(-c(2, 4:6)) %>% rename(Tract_ID = GEOID, MHI_TRACT = MHIE)
CBG.Filter4 <- left_join(CBG.Filter4, Tract.Replace, by = "Tract_ID")

CBG.Filter4 <- CBG.Filter4 %>% mutate(FIXED_MHI = case_when(
  is.na(MHI) ~ MHI_TRACT,
  TRUE ~ MHI
))

unique(complete.cases(CBG.Filter4$FIXED_MHI)) # All CBGs have a FIXED_MHI value.

# One more issue. CBG 511498501001 has no homeowner info. Let's also replace that. Since it is only one value I will hard code it.

Missing.Home <- get_acs(geography = "tract", state = "Virginia", county = 149, year = 2023, variables = c(
  T_OWNER = "B25003_002", 
  T_RENTER = "B25003_003"), output = 'wide')

Missing.Home <- Missing.Home %>% filter(GEOID == "51149850100") %>% mutate(P_OWN = (T_OWNERE/(T_OWNERE + T_RENTERE)) * 100)
CBG.Filter4 <- CBG.Filter4 %>% mutate(P_OWN = case_when(
  GEOID == "511498501001" ~ 58.10398,
  TRUE ~ P_OWN
))

unique(complete.cases(CBG.Filter4$P_OWN)) # All CBGs have a P_OWN value.

# Now let's save our data.

Paid.Public.Final <- CBG.Filter4
setwd("C:/Users/jeff.beauvais/Documents/Projects/CoastalVA_Access/Archive/Data/Tabluar") ## CHANGE TO YOUR WORKING DIRECTORY
save(Paid.Public.Final, file = "Paid_Public_FinalData.Rda")
