# Instructions ----

# To set the working directory, insert the path with forward slashes (instead of backslashes) between two quotation marks:
setwd("")
# Now you should be able to replicate the data underlying all tables and figures by executing the whole script. To do so, select all (CTRL+A) and execute the code (Ctrl+ENTER).

# Installing packages ----

install.packages("readxl")
install.packages("dplyr")
install.packages("openxlsx")
install.packages("plyr")
install.packages("stats")
install.packages("broom")
install.packages("brms")

# Loading packages ----

library("readxl")
library("dplyr")
library("openxlsx")
library("plyr")
library("stats")
library("broom")
library("brms")

# Importing country data ----

Data_Australia <- read_excel("./data/AnonymousData_Australia.xlsx")
Data_Canada <- read_excel("./data/AnonymousData_Canada.xlsx")
Data_Colombia <- read_excel("./data/AnonymousData_Colombia.xlsx")
Data_Egypt <- read_excel("./data/AnonymousData_Egypt.xlsx")
Data_France <- read_excel("./data/AnonymousData_France.xlsx")
Data_Hungary <- read_excel("./data/AnonymousData_Hungary.xlsx")
Data_Indonesia <- read_excel("./data/AnonymousData_Indonesia.xlsx")
Data_Kenya <- read_excel("./data/AnonymousData_Kenya.xlsx")
Data_SouthKorea <- read_excel("./data/AnonymousData_SouthKorea.xlsx")
Data_Turkey <- read_excel("./data/AnonymousData_Turkey.xlsx")

Data_Australia$Country <- "Australia"
Data_Canada$Country <- "Canada"
Data_Colombia$Country <- "Colombia"
Data_Egypt$Country <- "Egypt"
Data_France$Country <- "France"
Data_Hungary$Country <- "Hungary"
Data_Indonesia$Country <- "Indonesia"
Data_Kenya$Country <- "Kenya"
Data_SouthKorea$Country <- "SouthKorea"
Data_Turkey$Country <- "Turkey"

# Merging country data files ----

Data_AllCountries <- rbind.fill(Data_Australia,Data_Canada,Data_Colombia,Data_Egypt,Data_France,Data_Hungary,Data_Indonesia,Data_Kenya,Data_SouthKorea,Data_Turkey)

# Creating full dataset ####

Data_ResponseRequests <- Data_AllCountries

# Filtering out respondents who do not commit to thorough reading ####

Data_ResponseRequests <- filter(Data_ResponseRequests, E5.1 != 1)

# Creating numerical treatment variable ####

Data_ResponseRequests$ResponseRequest_0or1 <- case_when(
  Data_ResponseRequests$ResponseRequest=="No" ~ 0,
  Data_ResponseRequests$ResponseRequest=="Yes" ~ 1,
  TRUE ~ NA
)

# Creating world politics interest skipping variable #####

Data_ResponseRequests$WorldPoliticsInterest_Skipped <- case_when(
  Data_ResponseRequests$E11.1==3~1
  ,Data_ResponseRequests$E11.1==-99~1
  ,Data_ResponseRequests$E17.1==3~1
  ,Data_ResponseRequests$E17.1==-99~1
  ,Data_ResponseRequests$E23.1==3~1
  ,Data_ResponseRequests$E23.1==-99~1
  ,Data_ResponseRequests$E29.1==3~1
  ,Data_ResponseRequests$E29.1==-99~1
  ,Data_ResponseRequests$E34.1==5~1
  ,Data_ResponseRequests$E34.1==-99~1
  ,Data_ResponseRequests$E39.1==5~1
  ,Data_ResponseRequests$E39.1==-99~1
  ,Data_ResponseRequests$E44.1==5~1
  ,Data_ResponseRequests$E44.1==-99~1
  ,Data_ResponseRequests$E49.1==5~1
  ,Data_ResponseRequests$E49.1==-99~1
  ,Data_ResponseRequests$E55.1==5~1
  ,Data_ResponseRequests$E55.1==-99~1
  ,Data_ResponseRequests$E60.1==5~1
  ,Data_ResponseRequests$E60.1==-99~1
  ,Data_ResponseRequests$E65.1==5~1
  ,Data_ResponseRequests$E65.1==-99~1
  ,Data_ResponseRequests$E70.1==5~1
  ,Data_ResponseRequests$E70.1==-99~1
  ,Data_ResponseRequests$E75.1==6~1
  ,Data_ResponseRequests$E75.1==-99~1
  ,Data_ResponseRequests$E80.1==6~1
  ,Data_ResponseRequests$E80.1==-99~1
  ,Data_ResponseRequests$E85.1==6~1
  ,Data_ResponseRequests$E85.1==-99~1
  ,Data_ResponseRequests$E90.1==6~1
  ,Data_ResponseRequests$E90.1==-99~1
  ,Data_ResponseRequests$E95.1==6~1
  ,Data_ResponseRequests$E95.1==-99~1
  ,Data_ResponseRequests$E100.1==6~1
  ,Data_ResponseRequests$E100.1==-99~1
  ,Data_ResponseRequests$E105.1==6~1
  ,Data_ResponseRequests$E105.1==-99~1
  ,Data_ResponseRequests$E110.1==6~1
  ,Data_ResponseRequests$E110.1==-99~1
  ,Data_ResponseRequests$E115.1==7~1
  ,Data_ResponseRequests$E115.1==-99~1
  ,Data_ResponseRequests$E120.1==7~1
  ,Data_ResponseRequests$E120.1==-99~1
  ,Data_ResponseRequests$E125.1==7~1
  ,Data_ResponseRequests$E125.1==-99~1
  ,Data_ResponseRequests$E130.1==7~1
  ,Data_ResponseRequests$E130.1==-99~1
  ,Data_ResponseRequests$E135.1==7~1
  ,Data_ResponseRequests$E135.1==-99~1
  ,Data_ResponseRequests$E140.1==7~1
  ,Data_ResponseRequests$E140.1==-99~1
  ,Data_ResponseRequests$E145.1==7~1
  ,Data_ResponseRequests$E145.1==-99~1
  ,Data_ResponseRequests$E150.1==7~1
  ,Data_ResponseRequests$E150.1==-99~1
  ,Data_ResponseRequests$E155.1==8~1
  ,Data_ResponseRequests$E155.1==-99~1
  ,Data_ResponseRequests$E160.1==8~1
  ,Data_ResponseRequests$E160.1==-99~1
  ,Data_ResponseRequests$E165.1==8~1
  ,Data_ResponseRequests$E165.1==-99~1
  ,Data_ResponseRequests$E170.1==8~1
  ,Data_ResponseRequests$E170.1==-99~1
  ,Data_ResponseRequests$E175.1==8~1
  ,Data_ResponseRequests$E175.1==-99~1
  ,Data_ResponseRequests$E180.1==8~1
  ,Data_ResponseRequests$E180.1==-99~1
  ,Data_ResponseRequests$E185.1==8~1
  ,Data_ResponseRequests$E185.1==-99~1
  ,Data_ResponseRequests$E190.1==8~1
  ,Data_ResponseRequests$E190.1==-99~1
  ,Data_ResponseRequests$E195.1==11~1
  ,Data_ResponseRequests$E195.1==-99~1
  ,Data_ResponseRequests$E200.1==11~1
  ,Data_ResponseRequests$E200.1==-99~1
  ,Data_ResponseRequests$E205.1==11~1
  ,Data_ResponseRequests$E205.1==-99~1
  ,Data_ResponseRequests$E210.1==11~1
  ,Data_ResponseRequests$E210.1==-99~1
  ,Data_ResponseRequests$E215.1==12~1
  ,Data_ResponseRequests$E215.1==-99~1
  ,Data_ResponseRequests$E220.1==12~1
  ,Data_ResponseRequests$E220.1==-99~1
  ,Data_ResponseRequests$E225.1==12~1
  ,Data_ResponseRequests$E225.1==-99~1
  ,Data_ResponseRequests$E230.1==12~1
  ,Data_ResponseRequests$E230.1==-99~1
  ,Data_ResponseRequests$E236.1_1==101~1
  ,Data_ResponseRequests$E236.1_1==-99~1
  ,Data_ResponseRequests$E241.1_1==101~1
  ,Data_ResponseRequests$E241.1_1==-99~1
  ,Data_ResponseRequests$E246.1_1==101~1
  ,Data_ResponseRequests$E246.1_1==-99~1
  ,Data_ResponseRequests$E251.1_1==101~1
  ,Data_ResponseRequests$E251.1_1==-99~1
  ,Data_ResponseRequests$E256.1_1==101~1
  ,Data_ResponseRequests$E256.1_1==-99~1
  ,Data_ResponseRequests$E261.1_1==101~1
  ,Data_ResponseRequests$E261.1_1==-99~1
  ,Data_ResponseRequests$E266.1_1==101~1
  ,Data_ResponseRequests$E266.1_1==-99~1
  ,Data_ResponseRequests$E271.1_1==101~1
  ,Data_ResponseRequests$E271.1_1==-99~1
  ,TRUE ~ 0
)


## Testing for differences between treatment and control groups (Manuscript Figures 1 and 2, Supplementary Material 6.1) ####

t.test(WorldPoliticsInterest_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests)
ttest_ResponseRequests_WorldPoliticsInterest_Skipped <- t.test(WorldPoliticsInterest_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests)
tidy_ttest_ResponseRequests_WorldPoliticsInterest_Skipped <- broom::tidy(ttest_ResponseRequests_WorldPoliticsInterest_Skipped)
write.xlsx(tidy_ttest_ResponseRequests_WorldPoliticsInterest_Skipped, "./results/tidy_ttest_ResponseRequests_WorldPoliticsInterest_Skipped.xlsx")

# Creating cultural norms skipping variable #####

Data_ResponseRequests$CulturalNorms_Skipped <- case_when(
  Data_ResponseRequests$E12.1==3~1
  ,Data_ResponseRequests$E12.1==-99~1
  ,Data_ResponseRequests$E18.1==3~1
  ,Data_ResponseRequests$E18.1==-99~1
  ,Data_ResponseRequests$E24.1==3~1
  ,Data_ResponseRequests$E24.1==-99~1
  ,Data_ResponseRequests$E30.1==3~1
  ,Data_ResponseRequests$E30.1==-99~1
  ,Data_ResponseRequests$E35.1==5~1
  ,Data_ResponseRequests$E35.1==-99~1
  ,Data_ResponseRequests$E40.1==5~1
  ,Data_ResponseRequests$E40.1==-99~1
  ,Data_ResponseRequests$E45.1==5~1
  ,Data_ResponseRequests$E45.1==-99~1
  ,Data_ResponseRequests$E50.1==5~1
  ,Data_ResponseRequests$E50.1==-99~1
  ,Data_ResponseRequests$E56.1==5~1
  ,Data_ResponseRequests$E56.1==-99~1
  ,Data_ResponseRequests$E61.1==5~1
  ,Data_ResponseRequests$E61.1==-99~1
  ,Data_ResponseRequests$E66.1==5~1
  ,Data_ResponseRequests$E66.1==-99~1
  ,Data_ResponseRequests$E71.1==5~1
  ,Data_ResponseRequests$E71.1==-99~1
  ,Data_ResponseRequests$E76.1==6~1
  ,Data_ResponseRequests$E76.1==-99~1
  ,Data_ResponseRequests$E81.1==6~1
  ,Data_ResponseRequests$E81.1==-99~1
  ,Data_ResponseRequests$E86.1==6~1
  ,Data_ResponseRequests$E86.1==-99~1
  ,Data_ResponseRequests$E91.1==6~1
  ,Data_ResponseRequests$E91.1==-99~1
  ,Data_ResponseRequests$E96.1==6~1
  ,Data_ResponseRequests$E96.1==-99~1
  ,Data_ResponseRequests$E101.1==6~1
  ,Data_ResponseRequests$E101.1==-99~1
  ,Data_ResponseRequests$E106.1==6~1
  ,Data_ResponseRequests$E106.1==-99~1
  ,Data_ResponseRequests$E111.1==6~1
  ,Data_ResponseRequests$E111.1==-99~1
  ,Data_ResponseRequests$E116.1==7~1
  ,Data_ResponseRequests$E116.1==-99~1
  ,Data_ResponseRequests$E121.1==7~1
  ,Data_ResponseRequests$E121.1==-99~1
  ,Data_ResponseRequests$E126.1==7~1
  ,Data_ResponseRequests$E126.1==-99~1
  ,Data_ResponseRequests$E131.1==7~1
  ,Data_ResponseRequests$E131.1==-99~1
  ,Data_ResponseRequests$E136.1==7~1
  ,Data_ResponseRequests$E136.1==-99~1
  ,Data_ResponseRequests$E141.1==7~1
  ,Data_ResponseRequests$E141.1==-99~1
  ,Data_ResponseRequests$E146.1==7~1
  ,Data_ResponseRequests$E146.1==-99~1
  ,Data_ResponseRequests$E151.1==7~1
  ,Data_ResponseRequests$E151.1==-99~1
  ,Data_ResponseRequests$E158.1==8~1
  ,Data_ResponseRequests$E158.1==-99~1
  ,Data_ResponseRequests$E163.1==8~1
  ,Data_ResponseRequests$E163.1==-99~1
  ,Data_ResponseRequests$E166.1==8~1
  ,Data_ResponseRequests$E166.1==-99~1
  ,Data_ResponseRequests$E171.1==8~1
  ,Data_ResponseRequests$E171.1==-99~1
  ,Data_ResponseRequests$E176.1==8~1
  ,Data_ResponseRequests$E176.1==-99~1
  ,Data_ResponseRequests$E181.1==8~1
  ,Data_ResponseRequests$E181.1==-99~1
  ,Data_ResponseRequests$E186.1==8~1
  ,Data_ResponseRequests$E186.1==-99~1
  ,Data_ResponseRequests$E191.1==8~1
  ,Data_ResponseRequests$E191.1==-99~1
  ,Data_ResponseRequests$E196.1==11~1
  ,Data_ResponseRequests$E196.1==-99~1
  ,Data_ResponseRequests$E201.1==11~1
  ,Data_ResponseRequests$E201.1==-99~1
  ,Data_ResponseRequests$E206.1==11~1
  ,Data_ResponseRequests$E206.1==-99~1
  ,Data_ResponseRequests$E211.1==11~1
  ,Data_ResponseRequests$E211.1==-99~1
  ,Data_ResponseRequests$E216.1==12~1
  ,Data_ResponseRequests$E216.1==-99~1
  ,Data_ResponseRequests$E221.1==12~1
  ,Data_ResponseRequests$E221.1==-99~1
  ,Data_ResponseRequests$E226.1==12~1
  ,Data_ResponseRequests$E226.1==-99~1
  ,Data_ResponseRequests$E231.1==12~1
  ,Data_ResponseRequests$E231.1==-99~1
  ,Data_ResponseRequests$E237.1_1==101~1
  ,Data_ResponseRequests$E237.1_1==-99~1
  ,Data_ResponseRequests$E242.1_1==101~1
  ,Data_ResponseRequests$E242.1_1==-99~1
  ,Data_ResponseRequests$E247.1_1==101~1
  ,Data_ResponseRequests$E247.1_1==-99~1
  ,Data_ResponseRequests$E252.1_1==101~1
  ,Data_ResponseRequests$E252.1_1==-99~1
  ,Data_ResponseRequests$E257.1_1==101~1
  ,Data_ResponseRequests$E257.1_1==-99~1
  ,Data_ResponseRequests$E262.1_1==101~1
  ,Data_ResponseRequests$E262.1_1==-99~1
  ,Data_ResponseRequests$E267.1_1==101~1
  ,Data_ResponseRequests$E267.1_1==-99~1
  ,Data_ResponseRequests$E272.1_1==101~1
  ,Data_ResponseRequests$E272.1_1==-99~1
  , TRUE ~ 0
)

## Testing for differences between treatment and control groups (Manuscript Figures 1 and 2, Supplementary Material 6.1) ####

t.test(CulturalNorms_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests)
ttest_ResponseRequests_CulturalNorms_Skipped <- t.test(CulturalNorms_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests)
tidy_ttest_ResponseRequests_CulturalNorms_Skipped <- broom::tidy(ttest_ResponseRequests_CulturalNorms_Skipped)
write.xlsx(tidy_ttest_ResponseRequests_CulturalNorms_Skipped, "./results/tidy_ttest_ResponseRequests_CulturalNorms_Skipped.xlsx")

# Creating market intervention skipping variable #####

Data_ResponseRequests$MarketIntervention_Skipped <- case_when(
  Data_ResponseRequests$E13.1==3~1
  ,Data_ResponseRequests$E13.1==-99~1
  ,Data_ResponseRequests$E19.1==3~1
  ,Data_ResponseRequests$E19.1==-99~1
  ,Data_ResponseRequests$E25.1==3~1
  ,Data_ResponseRequests$E25.1==-99~1
  ,Data_ResponseRequests$E31.1==3~1
  ,Data_ResponseRequests$E31.1==-99~1
  ,Data_ResponseRequests$E36.1==5~1
  ,Data_ResponseRequests$E36.1==-99~1
  ,Data_ResponseRequests$E41.1==5~1
  ,Data_ResponseRequests$E41.1==-99~1
  ,Data_ResponseRequests$E46.1==5~1
  ,Data_ResponseRequests$E46.1==-99~1
  ,Data_ResponseRequests$E51.1==5~1
  ,Data_ResponseRequests$E51.1==-99~1
  ,Data_ResponseRequests$E57.1==5~1
  ,Data_ResponseRequests$E57.1==-99~1
  ,Data_ResponseRequests$E62.1==5~1
  ,Data_ResponseRequests$E62.1==-99~1
  ,Data_ResponseRequests$E67.1==5~1
  ,Data_ResponseRequests$E67.1==-99~1
  ,Data_ResponseRequests$E72.1==5~1
  ,Data_ResponseRequests$E72.1==-99~1
  ,Data_ResponseRequests$E77.1==6~1
  ,Data_ResponseRequests$E77.1==-99~1
  ,Data_ResponseRequests$E82.1==6~1
  ,Data_ResponseRequests$E82.1==-99~1
  ,Data_ResponseRequests$E87.1==6~1
  ,Data_ResponseRequests$E87.1==-99~1
  ,Data_ResponseRequests$E92.1==6~1
  ,Data_ResponseRequests$E92.1==-99~1
  ,Data_ResponseRequests$E97.1==6~1
  ,Data_ResponseRequests$E97.1==-99~1
  ,Data_ResponseRequests$E102.1==6~1
  ,Data_ResponseRequests$E102.1==-99~1
  ,Data_ResponseRequests$E107.1==6~1
  ,Data_ResponseRequests$E107.1==-99~1
  ,Data_ResponseRequests$E112.1==6~1
  ,Data_ResponseRequests$E112.1==-99~1
  ,Data_ResponseRequests$E117.1==7~1
  ,Data_ResponseRequests$E117.1==-99~1
  ,Data_ResponseRequests$E122.1==7~1
  ,Data_ResponseRequests$E122.1==-99~1
  ,Data_ResponseRequests$E127.1==7~1
  ,Data_ResponseRequests$E127.1==-99~1
  ,Data_ResponseRequests$E132.1==7~1
  ,Data_ResponseRequests$E132.1==-99~1
  ,Data_ResponseRequests$E137.1==7~1
  ,Data_ResponseRequests$E137.1==-99~1
  ,Data_ResponseRequests$E142.1==7~1
  ,Data_ResponseRequests$E142.1==-99~1
  ,Data_ResponseRequests$E147.1==7~1
  ,Data_ResponseRequests$E147.1==-99~1
  ,Data_ResponseRequests$E152.1==7~1
  ,Data_ResponseRequests$E152.1==-99~1
  ,Data_ResponseRequests$E157.1==8~1
  ,Data_ResponseRequests$E157.1==-99~1
  ,Data_ResponseRequests$E162.1==8~1
  ,Data_ResponseRequests$E162.1==-99~1
  ,Data_ResponseRequests$E167.1==8~1
  ,Data_ResponseRequests$E167.1==-99~1
  ,Data_ResponseRequests$E172.1==8~1
  ,Data_ResponseRequests$E172.1==-99~1
  ,Data_ResponseRequests$E177.1==8~1
  ,Data_ResponseRequests$E177.1==-99~1
  ,Data_ResponseRequests$E182.1==8~1
  ,Data_ResponseRequests$E182.1==-99~1
  ,Data_ResponseRequests$E187.1==8~1
  ,Data_ResponseRequests$E187.1==-99~1
  ,Data_ResponseRequests$E192.1==8~1
  ,Data_ResponseRequests$E192.1==-99~1
  ,Data_ResponseRequests$E197.1==11~1
  ,Data_ResponseRequests$E197.1==-99~1
  ,Data_ResponseRequests$E202.1==11~1
  ,Data_ResponseRequests$E202.1==-99~1
  ,Data_ResponseRequests$E207.1==11~1
  ,Data_ResponseRequests$E207.1==-99~1
  ,Data_ResponseRequests$E212.1==11~1
  ,Data_ResponseRequests$E212.1==-99~1
  ,Data_ResponseRequests$E217.1==12~1
  ,Data_ResponseRequests$E217.1==-99~1
  ,Data_ResponseRequests$E222.1==12~1
  ,Data_ResponseRequests$E222.1==-99~1
  ,Data_ResponseRequests$E227.1==12~1
  ,Data_ResponseRequests$E227.1==-99~1
  ,Data_ResponseRequests$E232.1==12~1
  ,Data_ResponseRequests$E232.1==-99~1
  ,Data_ResponseRequests$E238.1_1==101~1
  ,Data_ResponseRequests$E238.1_1==-99~1
  ,Data_ResponseRequests$E243.1_1==101~1
  ,Data_ResponseRequests$E243.1_1==-99~1
  ,Data_ResponseRequests$E248.1_1==101~1
  ,Data_ResponseRequests$E248.1_1==-99~1
  ,Data_ResponseRequests$E253.1_1==101~1
  ,Data_ResponseRequests$E253.1_1==-99~1
  ,Data_ResponseRequests$E258.1_1==101~1
  ,Data_ResponseRequests$E258.1_1==-99~1
  ,Data_ResponseRequests$E263.1_1==101~1
  ,Data_ResponseRequests$E263.1_1==-99~1
  ,Data_ResponseRequests$E268.1_1==101~1
  ,Data_ResponseRequests$E268.1_1==-99~1
  ,Data_ResponseRequests$E273.1_1==101~1
  ,Data_ResponseRequests$E273.1_1==-99~1
  ,TRUE ~ 0
)

## Testing for differences between treatment and control groups (Manuscript Figures 1 and 2, Supplementary Material 6.1) ####

t.test(MarketIntervention_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests)
ttest_ResponseRequests_MarketIntervention_Skipped <- t.test(MarketIntervention_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests)
tidy_ttest_ResponseRequests_MarketIntervention_Skipped <- broom::tidy(ttest_ResponseRequests_MarketIntervention_Skipped)
write.xlsx(tidy_ttest_ResponseRequests_MarketIntervention_Skipped, "./results/tidy_ttest_ResponseRequests_MarketIntervention_Skipped.xlsx")

# Creating environmentalism skipping variable #####

Data_ResponseRequests$Environmentalism_Skipped <- case_when(
  Data_ResponseRequests$E14.1==3~1
  ,Data_ResponseRequests$E14.1==-99~1
  ,Data_ResponseRequests$E20.1==3~1
  ,Data_ResponseRequests$E20.1==-99~1
  ,Data_ResponseRequests$E26.1==3~1
  ,Data_ResponseRequests$E26.1==-99~1
  ,Data_ResponseRequests$E32.1==3~1
  ,Data_ResponseRequests$E32.1==-99~1
  ,Data_ResponseRequests$E37.1==5~1
  ,Data_ResponseRequests$E37.1==-99~1
  ,Data_ResponseRequests$E42.1==5~1
  ,Data_ResponseRequests$E42.1==-99~1
  ,Data_ResponseRequests$E47.1==5~1
  ,Data_ResponseRequests$E47.1==-99~1
  ,Data_ResponseRequests$E52.1==5~1
  ,Data_ResponseRequests$E52.1==-99~1
  ,Data_ResponseRequests$E58.1==5~1
  ,Data_ResponseRequests$E58.1==-99~1
  ,Data_ResponseRequests$E63.1==5~1
  ,Data_ResponseRequests$E63.1==-99~1
  ,Data_ResponseRequests$E68.1==5~1
  ,Data_ResponseRequests$E68.1==-99~1
  ,Data_ResponseRequests$E73.1==5~1
  ,Data_ResponseRequests$E73.1==-99~1
  ,Data_ResponseRequests$E78.1==6~1
  ,Data_ResponseRequests$E78.1==-99~1
  ,Data_ResponseRequests$E83.1==6~1
  ,Data_ResponseRequests$E83.1==-99~1
  ,Data_ResponseRequests$E88.1==6~1
  ,Data_ResponseRequests$E88.1==-99~1
  ,Data_ResponseRequests$E93.1==6~1
  ,Data_ResponseRequests$E93.1==-99~1
  ,Data_ResponseRequests$E98.1==6~1
  ,Data_ResponseRequests$E98.1==-99~1
  ,Data_ResponseRequests$E103.1==6~1
  ,Data_ResponseRequests$E103.1==-99~1
  ,Data_ResponseRequests$E108.1==6~1
  ,Data_ResponseRequests$E108.1==-99~1
  ,Data_ResponseRequests$E113.1==6~1
  ,Data_ResponseRequests$E113.1==-99~1
  ,Data_ResponseRequests$E118.1==7~1
  ,Data_ResponseRequests$E118.1==-99~1
  ,Data_ResponseRequests$E123.1==7~1
  ,Data_ResponseRequests$E123.1==-99~1
  ,Data_ResponseRequests$E128.1==7~1
  ,Data_ResponseRequests$E128.1==-99~1
  ,Data_ResponseRequests$E133.1==7~1
  ,Data_ResponseRequests$E133.1==-99~1
  ,Data_ResponseRequests$E138.1==7~1
  ,Data_ResponseRequests$E138.1==-99~1
  ,Data_ResponseRequests$E143.1==7~1
  ,Data_ResponseRequests$E143.1==-99~1
  ,Data_ResponseRequests$E148.1==7~1
  ,Data_ResponseRequests$E148.1==-99~1
  ,Data_ResponseRequests$E153.1==7~1
  ,Data_ResponseRequests$E153.1==-99~1
  ,Data_ResponseRequests$E156.1==8~1
  ,Data_ResponseRequests$E156.1==-99~1
  ,Data_ResponseRequests$E161.1==8~1
  ,Data_ResponseRequests$E161.1==-99~1
  ,Data_ResponseRequests$E168.1==8~1
  ,Data_ResponseRequests$E168.1==-99~1
  ,Data_ResponseRequests$E173.1==8~1
  ,Data_ResponseRequests$E173.1==-99~1
  ,Data_ResponseRequests$E178.1==8~1
  ,Data_ResponseRequests$E178.1==-99~1
  ,Data_ResponseRequests$E183.1==8~1
  ,Data_ResponseRequests$E183.1==-99~1
  ,Data_ResponseRequests$E188.1==8~1
  ,Data_ResponseRequests$E188.1==-99~1
  ,Data_ResponseRequests$E193.1==8~1
  ,Data_ResponseRequests$E193.1==-99~1
  ,Data_ResponseRequests$E198.1==11~1
  ,Data_ResponseRequests$E198.1==-99~1
  ,Data_ResponseRequests$E203.1==11~1
  ,Data_ResponseRequests$E203.1==-99~1
  ,Data_ResponseRequests$E208.1==11~1
  ,Data_ResponseRequests$E208.1==-99~1
  ,Data_ResponseRequests$E213.1==11~1
  ,Data_ResponseRequests$E213.1==-99~1
  ,Data_ResponseRequests$E218.1==12~1
  ,Data_ResponseRequests$E218.1==-99~1
  ,Data_ResponseRequests$E223.1==12~1
  ,Data_ResponseRequests$E223.1==-99~1
  ,Data_ResponseRequests$E228.1==12~1
  ,Data_ResponseRequests$E228.1==-99~1
  ,Data_ResponseRequests$E233.1==12~1
  ,Data_ResponseRequests$E233.1==-99~1
  ,Data_ResponseRequests$E239.1_1==101~1
  ,Data_ResponseRequests$E239.1_1==-99~1
  ,Data_ResponseRequests$E244.1_1==101~1
  ,Data_ResponseRequests$E244.1_1==-99~1
  ,Data_ResponseRequests$E249.1_1==101~1
  ,Data_ResponseRequests$E249.1_1==-99~1
  ,Data_ResponseRequests$E254.1_1==101~1
  ,Data_ResponseRequests$E254.1_1==-99~1
  ,Data_ResponseRequests$E259.1_1==101~1
  ,Data_ResponseRequests$E259.1_1==-99~1
  ,Data_ResponseRequests$E264.1_1==101~1
  ,Data_ResponseRequests$E264.1_1==-99~1
  ,Data_ResponseRequests$E269.1_1==101~1
  ,Data_ResponseRequests$E269.1_1==-99~1
  ,Data_ResponseRequests$E274.1_1==101~1
  ,Data_ResponseRequests$E274.1_1==-99~1
  ,TRUE ~ 0
)

## Testing for differences between treatment and control groups (Manuscript Figures 1 and 2, Supplementary Material 6.1) ####

t.test(Environmentalism_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests)
ttest_ResponseRequests_Environmentalism_Skipped <- t.test(Environmentalism_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests)
tidy_ttest_ResponseRequests_Environmentalism_Skipped <- broom::tidy(ttest_ResponseRequests_Environmentalism_Skipped)
write.xlsx(tidy_ttest_ResponseRequests_Environmentalism_Skipped, "./results/tidy_ttest_ResponseRequests_Environmentalism_Skipped.xlsx")

# Creating global responsibility skipping variable #####

Data_ResponseRequests$GlobalResponsibility_Skipped <- case_when(
  Data_ResponseRequests$E15.1==3~1
  ,Data_ResponseRequests$E15.1==-99~1
  ,Data_ResponseRequests$E21.1==3~1
  ,Data_ResponseRequests$E21.1==-99~1
  ,Data_ResponseRequests$E27.1==3~1
  ,Data_ResponseRequests$E27.1==-99~1
  ,Data_ResponseRequests$E33.1==3~1
  ,Data_ResponseRequests$E33.1==-99~1
  ,Data_ResponseRequests$E38.1==5~1
  ,Data_ResponseRequests$E38.1==-99~1
  ,Data_ResponseRequests$E43.1==5~1
  ,Data_ResponseRequests$E43.1==-99~1
  ,Data_ResponseRequests$E48.1==5~1
  ,Data_ResponseRequests$E48.1==-99~1
  ,Data_ResponseRequests$E53.1==5~1
  ,Data_ResponseRequests$E53.1==-99~1
  ,Data_ResponseRequests$E59.1==5~1
  ,Data_ResponseRequests$E59.1==-99~1
  ,Data_ResponseRequests$E64.1==5~1
  ,Data_ResponseRequests$E64.1==-99~1
  ,Data_ResponseRequests$E69.1==5~1
  ,Data_ResponseRequests$E69.1==-99~1
  ,Data_ResponseRequests$E74.1==5~1
  ,Data_ResponseRequests$E74.1==-99~1
  ,Data_ResponseRequests$E79.1==6~1
  ,Data_ResponseRequests$E79.1==-99~1
  ,Data_ResponseRequests$E84.1==6~1
  ,Data_ResponseRequests$E84.1==-99~1
  ,Data_ResponseRequests$E89.1==6~1
  ,Data_ResponseRequests$E89.1==-99~1
  ,Data_ResponseRequests$E94.1==6~1
  ,Data_ResponseRequests$E94.1==-99~1
  ,Data_ResponseRequests$E99.1==6~1
  ,Data_ResponseRequests$E99.1==-99~1
  ,Data_ResponseRequests$E104.1==6~1
  ,Data_ResponseRequests$E104.1==-99~1
  ,Data_ResponseRequests$E109.1==6~1
  ,Data_ResponseRequests$E109.1==-99~1
  ,Data_ResponseRequests$E114.1==6~1
  ,Data_ResponseRequests$E114.1==-99~1
  ,Data_ResponseRequests$E119.1==7~1
  ,Data_ResponseRequests$E119.1==-99~1
  ,Data_ResponseRequests$E124.1==7~1
  ,Data_ResponseRequests$E124.1==-99~1
  ,Data_ResponseRequests$E129.1==7~1
  ,Data_ResponseRequests$E129.1==-99~1
  ,Data_ResponseRequests$E134.1==7~1
  ,Data_ResponseRequests$E134.1==-99~1
  ,Data_ResponseRequests$E139.1==7~1
  ,Data_ResponseRequests$E139.1==-99~1
  ,Data_ResponseRequests$E144.1==7~1
  ,Data_ResponseRequests$E144.1==-99~1
  ,Data_ResponseRequests$E149.1==7~1
  ,Data_ResponseRequests$E149.1==-99~1
  ,Data_ResponseRequests$E154.1==7~1
  ,Data_ResponseRequests$E154.1==-99~1
  ,Data_ResponseRequests$E159.1==8~1
  ,Data_ResponseRequests$E159.1==-99~1
  ,Data_ResponseRequests$E164.1==8~1
  ,Data_ResponseRequests$E164.1==-99~1
  ,Data_ResponseRequests$E169.1==8~1
  ,Data_ResponseRequests$E169.1==-99~1
  ,Data_ResponseRequests$E174.1==8~1
  ,Data_ResponseRequests$E174.1==-99~1
  ,Data_ResponseRequests$E179.1==8~1
  ,Data_ResponseRequests$E179.1==-99~1
  ,Data_ResponseRequests$E184.1==8~1
  ,Data_ResponseRequests$E184.1==-99~1
  ,Data_ResponseRequests$E189.1==8~1
  ,Data_ResponseRequests$E189.1==-99~1
  ,Data_ResponseRequests$E194.1==8~1
  ,Data_ResponseRequests$E194.1==-99~1
  ,Data_ResponseRequests$E199.1==11~1
  ,Data_ResponseRequests$E199.1==-99~1
  ,Data_ResponseRequests$E204.1==11~1
  ,Data_ResponseRequests$E204.1==-99~1
  ,Data_ResponseRequests$E209.1==11~1
  ,Data_ResponseRequests$E209.1==-99~1
  ,Data_ResponseRequests$E214.1==11~1
  ,Data_ResponseRequests$E214.1==-99~1
  ,Data_ResponseRequests$E219.1==12~1
  ,Data_ResponseRequests$E219.1==-99~1
  ,Data_ResponseRequests$E224.1==12~1
  ,Data_ResponseRequests$E224.1==-99~1
  ,Data_ResponseRequests$E229.1==12~1
  ,Data_ResponseRequests$E229.1==-99~1
  ,Data_ResponseRequests$E234.1==12~1
  ,Data_ResponseRequests$E234.1==-99~1
  ,Data_ResponseRequests$E240.1_1==101~1
  ,Data_ResponseRequests$E240.1_1==-99~1
  ,Data_ResponseRequests$E245.1_1==101~1
  ,Data_ResponseRequests$E245.1_1==-99~1
  ,Data_ResponseRequests$E250.1_1==101~1
  ,Data_ResponseRequests$E250.1_1==-99~1
  ,Data_ResponseRequests$E255.1_1==101~1
  ,Data_ResponseRequests$E255.1_1==-99~1
  ,Data_ResponseRequests$E260.1_1==101~1
  ,Data_ResponseRequests$E260.1_1==-99~1
  ,Data_ResponseRequests$E265.1_1==101~1
  ,Data_ResponseRequests$E265.1_1==-99~1
  ,Data_ResponseRequests$E270.1_1==101~1
  ,Data_ResponseRequests$E270.1_1==-99~1
  ,Data_ResponseRequests$E275.1_1==101~1
  ,Data_ResponseRequests$E275.1_1==-99~1
  ,TRUE ~ 0
)

## Testing for differences between treatment and control groups (Manuscript Figures 1 and 2, Supplementary Material 6.1) ####

t.test(GlobalResponsibility_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests)
ttest_ResponseRequests_GlobalResponsibility_Skipped <- t.test(GlobalResponsibility_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests)
tidy_ttest_ResponseRequests_GlobalResponsibility_Skipped <- broom::tidy(ttest_ResponseRequests_GlobalResponsibility_Skipped)
write.xlsx(tidy_ttest_ResponseRequests_GlobalResponsibility_Skipped, "./results/tidy_ttest_ResponseRequests_GlobalResponsibility_Skipped.xlsx")

# Creating IMF knowledge skipping variable ####

Data_ResponseRequests$IMFknowledge_Skipped <- case_when(
  Data_ResponseRequests$E10.1 == 31 ~ 1,
  Data_ResponseRequests$E10.1 == -99 ~ 1,
  Data_ResponseRequests$E16.1 == 31 ~ 1,
  Data_ResponseRequests$E16.1 == -99 ~ 1,
  Data_ResponseRequests$E22.1 == -99 ~ 1,
  Data_ResponseRequests$E28.1 == -99 ~ 1,
  TRUE ~ 0
)



## Testing for differences between treatment and control groups  (Manuscript Figures 1 and 2, Supplementary Material 6.1) ####

t.test(IMFknowledge_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests)
ttest_ResponseRequests_IMFknowledge_Skipped <- t.test(IMFknowledge_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests)
tidy_ttest_ResponseRequests_IMFknowledge_Skipped <- broom::tidy(ttest_ResponseRequests_IMFknowledge_Skipped)
write.xlsx(tidy_ttest_ResponseRequests_IMFknowledge_Skipped, "./results/tidy_ttest_ResponseRequests_IMFknowledge_Skipped.xlsx")


## Testing for differences between treatment and control groups  (Manuscript Figure 5, Supplementary Material 6.3) ####

t.test(IMFknowledge_RightAmongAll ~ ResponseRequest_0or1, data=Data_ResponseRequests)
ttest_ResponseRequests_IMFknowledge_RightAmongAll <- t.test(IMFknowledge_RightAmongAll ~ ResponseRequest_0or1, data=Data_ResponseRequests)
tidy_ttest_ResponseRequests_IMFknowledge_RightAmongAll <- broom::tidy(ttest_ResponseRequests_IMFknowledge_RightAmongAll)
write.xlsx(tidy_ttest_ResponseRequests_IMFknowledge_RightAmongAll, "./results/tidy_ttest_ResponseRequests_IMFknowledge_RightAmongAll.xlsx")

## Testing for differences between treatment and control groups  (Manuscript Figure 5, Supplementary Material 6.3) ####

t.test(IMFknowledge_RightAmongSubstantive ~ ResponseRequest_0or1, data=Data_ResponseRequests)
ttest_ResponseRequests_IMFknowledge_RightAmongSubstantive <- t.test(IMFknowledge_RightAmongSubstantive ~ ResponseRequest_0or1, data=Data_ResponseRequests)
tidy_ttest_ResponseRequests_IMFknowledge_RightAmongSubstantive <- broom::tidy(ttest_ResponseRequests_IMFknowledge_RightAmongSubstantive)
write.xlsx(tidy_ttest_ResponseRequests_IMFknowledge_RightAmongSubstantive, "./results/tidy_ttest_ResponseRequests_IMFknowledge_RightAmongSubstantive.xlsx")

# Random effects models (Manuscript Table 1; Appendices 7.1.1, 7.2.1, 7.3.1, 7.4.1, 7.5.1, 7.6.1, 7.7.1, 7.8.1) ----

Bayes_ResponseRequest_RandomEffects_WorldPoliticsInterest_Skipped <- brm(WorldPoliticsInterest_Skipped ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests)
summary(Bayes_ResponseRequest_RandomEffects_WorldPoliticsInterest_Skipped)
coef(Bayes_ResponseRequest_RandomEffects_WorldPoliticsInterest_Skipped)

Bayes_ResponseRequest_RandomEffects_CulturalNorms_Skipped <- brm(CulturalNorms_Skipped ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests)
summary(Bayes_ResponseRequest_RandomEffects_CulturalNorms_Skipped)
coef(Bayes_ResponseRequest_RandomEffects_CulturalNorms_Skipped)

Bayes_ResponseRequest_RandomEffects_Environmentalism_Skipped <- brm(Environmentalism_Skipped ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests)
summary(Bayes_ResponseRequest_RandomEffects_Environmentalism_Skipped)
coef(Bayes_ResponseRequest_RandomEffects_Environmentalism_Skipped)

Bayes_ResponseRequest_RandomEffects_IMFknowledge_Skipped <- brm(IMFknowledge_Skipped ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests)
summary(Bayes_ResponseRequest_RandomEffects_IMFknowledge_Skipped)
coef(Bayes_ResponseRequest_RandomEffects_IMFknowledge_Skipped)

Bayes_ResponseRequest_RandomEffects_GlobalResponsibility_Skipped <- brm(GlobalResponsibility_Skipped ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests)
summary(Bayes_ResponseRequest_RandomEffects_GlobalResponsibility_Skipped)
coef(Bayes_ResponseRequest_RandomEffects_GlobalResponsibility_Skipped)

Bayes_ResponseRequest_RandomEffects_MarketIntervention_Skipped <- brm(MarketIntervention_Skipped ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests)
summary(Bayes_ResponseRequest_RandomEffects_MarketIntervention_Skipped)
coef(Bayes_ResponseRequest_RandomEffects_MarketIntervention_Skipped)

Bayes_ResponseRequest_RandomEffects_IMFknowledge_RightAmongAll <- brm(IMFknowledge_RightAmongAll ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests)
summary(Bayes_ResponseRequest_RandomEffects_IMFknowledge_RightAmongAll)
coef(Bayes_ResponseRequest_RandomEffects_IMFknowledge_RightAmongAll)

Bayes_ResponseRequest_RandomEffects_IMFknowledge_RightAmongSubstantive <- brm(IMFknowledge_RightAmongSubstantive ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests)
summary(Bayes_ResponseRequest_RandomEffects_IMFknowledge_RightAmongSubstantive)
coef(Bayes_ResponseRequest_RandomEffects_IMFknowledge_RightAmongSubstantive)

# Creating dataset without "I don't know" (DK) options ####

Data_ResponseRequests_NoDK <- Data_AllCountries

# Limiting to respondents who did not get the explicit DK option ####

Data_ResponseRequests_NoDK <- filter(Data_ResponseRequests_NoDK, DontKnowOption=="No")

# Creating numerical treatment variable ####

Data_ResponseRequests_NoDK$ResponseRequest_0or1 <- case_when(
  Data_ResponseRequests_NoDK$ResponseRequest=="No" ~ 0,
  Data_ResponseRequests_NoDK$ResponseRequest=="Yes" ~ 1,
  TRUE ~ NA
)

# Creating world politics interest skipping variable #####

Data_ResponseRequests_NoDK$WorldPoliticsInterest_Skipped <- case_when(
  Data_ResponseRequests_NoDK$E11.1==3~1
  ,Data_ResponseRequests_NoDK$E11.1==-99~1
  ,Data_ResponseRequests_NoDK$E17.1==3~1
  ,Data_ResponseRequests_NoDK$E17.1==-99~1
  ,Data_ResponseRequests_NoDK$E23.1==3~1
  ,Data_ResponseRequests_NoDK$E23.1==-99~1
  ,Data_ResponseRequests_NoDK$E29.1==3~1
  ,Data_ResponseRequests_NoDK$E29.1==-99~1
  ,Data_ResponseRequests_NoDK$E34.1==5~1
  ,Data_ResponseRequests_NoDK$E34.1==-99~1
  ,Data_ResponseRequests_NoDK$E39.1==5~1
  ,Data_ResponseRequests_NoDK$E39.1==-99~1
  ,Data_ResponseRequests_NoDK$E44.1==5~1
  ,Data_ResponseRequests_NoDK$E44.1==-99~1
  ,Data_ResponseRequests_NoDK$E49.1==5~1
  ,Data_ResponseRequests_NoDK$E49.1==-99~1
  ,Data_ResponseRequests_NoDK$E55.1==5~1
  ,Data_ResponseRequests_NoDK$E55.1==-99~1
  ,Data_ResponseRequests_NoDK$E60.1==5~1
  ,Data_ResponseRequests_NoDK$E60.1==-99~1
  ,Data_ResponseRequests_NoDK$E65.1==5~1
  ,Data_ResponseRequests_NoDK$E65.1==-99~1
  ,Data_ResponseRequests_NoDK$E70.1==5~1
  ,Data_ResponseRequests_NoDK$E70.1==-99~1
  ,Data_ResponseRequests_NoDK$E75.1==6~1
  ,Data_ResponseRequests_NoDK$E75.1==-99~1
  ,Data_ResponseRequests_NoDK$E80.1==6~1
  ,Data_ResponseRequests_NoDK$E80.1==-99~1
  ,Data_ResponseRequests_NoDK$E85.1==6~1
  ,Data_ResponseRequests_NoDK$E85.1==-99~1
  ,Data_ResponseRequests_NoDK$E90.1==6~1
  ,Data_ResponseRequests_NoDK$E90.1==-99~1
  ,Data_ResponseRequests_NoDK$E95.1==6~1
  ,Data_ResponseRequests_NoDK$E95.1==-99~1
  ,Data_ResponseRequests_NoDK$E100.1==6~1
  ,Data_ResponseRequests_NoDK$E100.1==-99~1
  ,Data_ResponseRequests_NoDK$E105.1==6~1
  ,Data_ResponseRequests_NoDK$E105.1==-99~1
  ,Data_ResponseRequests_NoDK$E110.1==6~1
  ,Data_ResponseRequests_NoDK$E110.1==-99~1
  ,Data_ResponseRequests_NoDK$E115.1==7~1
  ,Data_ResponseRequests_NoDK$E115.1==-99~1
  ,Data_ResponseRequests_NoDK$E120.1==7~1
  ,Data_ResponseRequests_NoDK$E120.1==-99~1
  ,Data_ResponseRequests_NoDK$E125.1==7~1
  ,Data_ResponseRequests_NoDK$E125.1==-99~1
  ,Data_ResponseRequests_NoDK$E130.1==7~1
  ,Data_ResponseRequests_NoDK$E130.1==-99~1
  ,Data_ResponseRequests_NoDK$E135.1==7~1
  ,Data_ResponseRequests_NoDK$E135.1==-99~1
  ,Data_ResponseRequests_NoDK$E140.1==7~1
  ,Data_ResponseRequests_NoDK$E140.1==-99~1
  ,Data_ResponseRequests_NoDK$E145.1==7~1
  ,Data_ResponseRequests_NoDK$E145.1==-99~1
  ,Data_ResponseRequests_NoDK$E150.1==7~1
  ,Data_ResponseRequests_NoDK$E150.1==-99~1
  ,Data_ResponseRequests_NoDK$E155.1==8~1
  ,Data_ResponseRequests_NoDK$E155.1==-99~1
  ,Data_ResponseRequests_NoDK$E160.1==8~1
  ,Data_ResponseRequests_NoDK$E160.1==-99~1
  ,Data_ResponseRequests_NoDK$E165.1==8~1
  ,Data_ResponseRequests_NoDK$E165.1==-99~1
  ,Data_ResponseRequests_NoDK$E170.1==8~1
  ,Data_ResponseRequests_NoDK$E170.1==-99~1
  ,Data_ResponseRequests_NoDK$E175.1==8~1
  ,Data_ResponseRequests_NoDK$E175.1==-99~1
  ,Data_ResponseRequests_NoDK$E180.1==8~1
  ,Data_ResponseRequests_NoDK$E180.1==-99~1
  ,Data_ResponseRequests_NoDK$E185.1==8~1
  ,Data_ResponseRequests_NoDK$E185.1==-99~1
  ,Data_ResponseRequests_NoDK$E190.1==8~1
  ,Data_ResponseRequests_NoDK$E190.1==-99~1
  ,Data_ResponseRequests_NoDK$E195.1==11~1
  ,Data_ResponseRequests_NoDK$E195.1==-99~1
  ,Data_ResponseRequests_NoDK$E200.1==11~1
  ,Data_ResponseRequests_NoDK$E200.1==-99~1
  ,Data_ResponseRequests_NoDK$E205.1==11~1
  ,Data_ResponseRequests_NoDK$E205.1==-99~1
  ,Data_ResponseRequests_NoDK$E210.1==11~1
  ,Data_ResponseRequests_NoDK$E210.1==-99~1
  ,Data_ResponseRequests_NoDK$E215.1==12~1
  ,Data_ResponseRequests_NoDK$E215.1==-99~1
  ,Data_ResponseRequests_NoDK$E220.1==12~1
  ,Data_ResponseRequests_NoDK$E220.1==-99~1
  ,Data_ResponseRequests_NoDK$E225.1==12~1
  ,Data_ResponseRequests_NoDK$E225.1==-99~1
  ,Data_ResponseRequests_NoDK$E230.1==12~1
  ,Data_ResponseRequests_NoDK$E230.1==-99~1
  ,Data_ResponseRequests_NoDK$E236.1_1==101~1
  ,Data_ResponseRequests_NoDK$E236.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E241.1_1==101~1
  ,Data_ResponseRequests_NoDK$E241.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E246.1_1==101~1
  ,Data_ResponseRequests_NoDK$E246.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E251.1_1==101~1
  ,Data_ResponseRequests_NoDK$E251.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E256.1_1==101~1
  ,Data_ResponseRequests_NoDK$E256.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E261.1_1==101~1
  ,Data_ResponseRequests_NoDK$E261.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E266.1_1==101~1
  ,Data_ResponseRequests_NoDK$E266.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E271.1_1==101~1
  ,Data_ResponseRequests_NoDK$E271.1_1==-99~1
  ,TRUE ~ 0
)


## Testing for differences between treatment and control groups (Manuscript Figures 3 and 4, Supplementary Material 6.2) ####

t.test(WorldPoliticsInterest_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests_NoDK)
ttest_ResponseRequests_NoDK_WorldPoliticsInterest_Skipped <- t.test(WorldPoliticsInterest_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests_NoDK)
tidy_ttest_ResponseRequests_NoDK_WorldPoliticsInterest_Skipped <- broom::tidy(ttest_ResponseRequests_NoDK_WorldPoliticsInterest_Skipped)
write.xlsx(tidy_ttest_ResponseRequests_NoDK_WorldPoliticsInterest_Skipped, "./results/tidy_ttest_ResponseRequests_NoDK_WorldPoliticsInterest_Skipped.xlsx")

# Creating cultural norms skipping variable #####

Data_ResponseRequests_NoDK$CulturalNorms_Skipped <- case_when(
  Data_ResponseRequests_NoDK$E12.1==3~1
  ,Data_ResponseRequests_NoDK$E12.1==-99~1
  ,Data_ResponseRequests_NoDK$E18.1==3~1
  ,Data_ResponseRequests_NoDK$E18.1==-99~1
  ,Data_ResponseRequests_NoDK$E24.1==3~1
  ,Data_ResponseRequests_NoDK$E24.1==-99~1
  ,Data_ResponseRequests_NoDK$E30.1==3~1
  ,Data_ResponseRequests_NoDK$E30.1==-99~1
  ,Data_ResponseRequests_NoDK$E35.1==5~1
  ,Data_ResponseRequests_NoDK$E35.1==-99~1
  ,Data_ResponseRequests_NoDK$E40.1==5~1
  ,Data_ResponseRequests_NoDK$E40.1==-99~1
  ,Data_ResponseRequests_NoDK$E45.1==5~1
  ,Data_ResponseRequests_NoDK$E45.1==-99~1
  ,Data_ResponseRequests_NoDK$E50.1==5~1
  ,Data_ResponseRequests_NoDK$E50.1==-99~1
  ,Data_ResponseRequests_NoDK$E56.1==5~1
  ,Data_ResponseRequests_NoDK$E56.1==-99~1
  ,Data_ResponseRequests_NoDK$E61.1==5~1
  ,Data_ResponseRequests_NoDK$E61.1==-99~1
  ,Data_ResponseRequests_NoDK$E66.1==5~1
  ,Data_ResponseRequests_NoDK$E66.1==-99~1
  ,Data_ResponseRequests_NoDK$E71.1==5~1
  ,Data_ResponseRequests_NoDK$E71.1==-99~1
  ,Data_ResponseRequests_NoDK$E76.1==6~1
  ,Data_ResponseRequests_NoDK$E76.1==-99~1
  ,Data_ResponseRequests_NoDK$E81.1==6~1
  ,Data_ResponseRequests_NoDK$E81.1==-99~1
  ,Data_ResponseRequests_NoDK$E86.1==6~1
  ,Data_ResponseRequests_NoDK$E86.1==-99~1
  ,Data_ResponseRequests_NoDK$E91.1==6~1
  ,Data_ResponseRequests_NoDK$E91.1==-99~1
  ,Data_ResponseRequests_NoDK$E96.1==6~1
  ,Data_ResponseRequests_NoDK$E96.1==-99~1
  ,Data_ResponseRequests_NoDK$E101.1==6~1
  ,Data_ResponseRequests_NoDK$E101.1==-99~1
  ,Data_ResponseRequests_NoDK$E106.1==6~1
  ,Data_ResponseRequests_NoDK$E106.1==-99~1
  ,Data_ResponseRequests_NoDK$E111.1==6~1
  ,Data_ResponseRequests_NoDK$E111.1==-99~1
  ,Data_ResponseRequests_NoDK$E116.1==7~1
  ,Data_ResponseRequests_NoDK$E116.1==-99~1
  ,Data_ResponseRequests_NoDK$E121.1==7~1
  ,Data_ResponseRequests_NoDK$E121.1==-99~1
  ,Data_ResponseRequests_NoDK$E126.1==7~1
  ,Data_ResponseRequests_NoDK$E126.1==-99~1
  ,Data_ResponseRequests_NoDK$E131.1==7~1
  ,Data_ResponseRequests_NoDK$E131.1==-99~1
  ,Data_ResponseRequests_NoDK$E136.1==7~1
  ,Data_ResponseRequests_NoDK$E136.1==-99~1
  ,Data_ResponseRequests_NoDK$E141.1==7~1
  ,Data_ResponseRequests_NoDK$E141.1==-99~1
  ,Data_ResponseRequests_NoDK$E146.1==7~1
  ,Data_ResponseRequests_NoDK$E146.1==-99~1
  ,Data_ResponseRequests_NoDK$E151.1==7~1
  ,Data_ResponseRequests_NoDK$E151.1==-99~1
  ,Data_ResponseRequests_NoDK$E158.1==8~1
  ,Data_ResponseRequests_NoDK$E158.1==-99~1
  ,Data_ResponseRequests_NoDK$E163.1==8~1
  ,Data_ResponseRequests_NoDK$E163.1==-99~1
  ,Data_ResponseRequests_NoDK$E166.1==8~1
  ,Data_ResponseRequests_NoDK$E166.1==-99~1
  ,Data_ResponseRequests_NoDK$E171.1==8~1
  ,Data_ResponseRequests_NoDK$E171.1==-99~1
  ,Data_ResponseRequests_NoDK$E176.1==8~1
  ,Data_ResponseRequests_NoDK$E176.1==-99~1
  ,Data_ResponseRequests_NoDK$E181.1==8~1
  ,Data_ResponseRequests_NoDK$E181.1==-99~1
  ,Data_ResponseRequests_NoDK$E186.1==8~1
  ,Data_ResponseRequests_NoDK$E186.1==-99~1
  ,Data_ResponseRequests_NoDK$E191.1==8~1
  ,Data_ResponseRequests_NoDK$E191.1==-99~1
  ,Data_ResponseRequests_NoDK$E196.1==11~1
  ,Data_ResponseRequests_NoDK$E196.1==-99~1
  ,Data_ResponseRequests_NoDK$E201.1==11~1
  ,Data_ResponseRequests_NoDK$E201.1==-99~1
  ,Data_ResponseRequests_NoDK$E206.1==11~1
  ,Data_ResponseRequests_NoDK$E206.1==-99~1
  ,Data_ResponseRequests_NoDK$E211.1==11~1
  ,Data_ResponseRequests_NoDK$E211.1==-99~1
  ,Data_ResponseRequests_NoDK$E216.1==12~1
  ,Data_ResponseRequests_NoDK$E216.1==-99~1
  ,Data_ResponseRequests_NoDK$E221.1==12~1
  ,Data_ResponseRequests_NoDK$E221.1==-99~1
  ,Data_ResponseRequests_NoDK$E226.1==12~1
  ,Data_ResponseRequests_NoDK$E226.1==-99~1
  ,Data_ResponseRequests_NoDK$E231.1==12~1
  ,Data_ResponseRequests_NoDK$E231.1==-99~1
  ,Data_ResponseRequests_NoDK$E237.1_1==101~1
  ,Data_ResponseRequests_NoDK$E237.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E242.1_1==101~1
  ,Data_ResponseRequests_NoDK$E242.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E247.1_1==101~1
  ,Data_ResponseRequests_NoDK$E247.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E252.1_1==101~1
  ,Data_ResponseRequests_NoDK$E252.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E257.1_1==101~1
  ,Data_ResponseRequests_NoDK$E257.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E262.1_1==101~1
  ,Data_ResponseRequests_NoDK$E262.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E267.1_1==101~1
  ,Data_ResponseRequests_NoDK$E267.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E272.1_1==101~1
  ,Data_ResponseRequests_NoDK$E272.1_1==-99~1
  , TRUE ~ 0
)

## Testing for differences between treatment and control groups (Manuscript Figures 3 and 4, Supplementary Material 6.2)  ####

t.test(CulturalNorms_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests_NoDK)
ttest_ResponseRequests_NoDK_CulturalNorms_Skipped <- t.test(CulturalNorms_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests_NoDK)
tidy_ttest_ResponseRequests_NoDK_CulturalNorms_Skipped <- broom::tidy(ttest_ResponseRequests_NoDK_CulturalNorms_Skipped)
write.xlsx(tidy_ttest_ResponseRequests_NoDK_CulturalNorms_Skipped, "./results/tidy_ttest_ResponseRequests_NoDK_CulturalNorms_Skipped.xlsx")

# Creating market intervention skipping variable #####

Data_ResponseRequests_NoDK$MarketIntervention_Skipped <- case_when(
  Data_ResponseRequests_NoDK$E13.1==3~1
  ,Data_ResponseRequests_NoDK$E13.1==-99~1
  ,Data_ResponseRequests_NoDK$E19.1==3~1
  ,Data_ResponseRequests_NoDK$E19.1==-99~1
  ,Data_ResponseRequests_NoDK$E25.1==3~1
  ,Data_ResponseRequests_NoDK$E25.1==-99~1
  ,Data_ResponseRequests_NoDK$E31.1==3~1
  ,Data_ResponseRequests_NoDK$E31.1==-99~1
  ,Data_ResponseRequests_NoDK$E36.1==5~1
  ,Data_ResponseRequests_NoDK$E36.1==-99~1
  ,Data_ResponseRequests_NoDK$E41.1==5~1
  ,Data_ResponseRequests_NoDK$E41.1==-99~1
  ,Data_ResponseRequests_NoDK$E46.1==5~1
  ,Data_ResponseRequests_NoDK$E46.1==-99~1
  ,Data_ResponseRequests_NoDK$E51.1==5~1
  ,Data_ResponseRequests_NoDK$E51.1==-99~1
  ,Data_ResponseRequests_NoDK$E57.1==5~1
  ,Data_ResponseRequests_NoDK$E57.1==-99~1
  ,Data_ResponseRequests_NoDK$E62.1==5~1
  ,Data_ResponseRequests_NoDK$E62.1==-99~1
  ,Data_ResponseRequests_NoDK$E67.1==5~1
  ,Data_ResponseRequests_NoDK$E67.1==-99~1
  ,Data_ResponseRequests_NoDK$E72.1==5~1
  ,Data_ResponseRequests_NoDK$E72.1==-99~1
  ,Data_ResponseRequests_NoDK$E77.1==6~1
  ,Data_ResponseRequests_NoDK$E77.1==-99~1
  ,Data_ResponseRequests_NoDK$E82.1==6~1
  ,Data_ResponseRequests_NoDK$E82.1==-99~1
  ,Data_ResponseRequests_NoDK$E87.1==6~1
  ,Data_ResponseRequests_NoDK$E87.1==-99~1
  ,Data_ResponseRequests_NoDK$E92.1==6~1
  ,Data_ResponseRequests_NoDK$E92.1==-99~1
  ,Data_ResponseRequests_NoDK$E97.1==6~1
  ,Data_ResponseRequests_NoDK$E97.1==-99~1
  ,Data_ResponseRequests_NoDK$E102.1==6~1
  ,Data_ResponseRequests_NoDK$E102.1==-99~1
  ,Data_ResponseRequests_NoDK$E107.1==6~1
  ,Data_ResponseRequests_NoDK$E107.1==-99~1
  ,Data_ResponseRequests_NoDK$E112.1==6~1
  ,Data_ResponseRequests_NoDK$E112.1==-99~1
  ,Data_ResponseRequests_NoDK$E117.1==7~1
  ,Data_ResponseRequests_NoDK$E117.1==-99~1
  ,Data_ResponseRequests_NoDK$E122.1==7~1
  ,Data_ResponseRequests_NoDK$E122.1==-99~1
  ,Data_ResponseRequests_NoDK$E127.1==7~1
  ,Data_ResponseRequests_NoDK$E127.1==-99~1
  ,Data_ResponseRequests_NoDK$E132.1==7~1
  ,Data_ResponseRequests_NoDK$E132.1==-99~1
  ,Data_ResponseRequests_NoDK$E137.1==7~1
  ,Data_ResponseRequests_NoDK$E137.1==-99~1
  ,Data_ResponseRequests_NoDK$E142.1==7~1
  ,Data_ResponseRequests_NoDK$E142.1==-99~1
  ,Data_ResponseRequests_NoDK$E147.1==7~1
  ,Data_ResponseRequests_NoDK$E147.1==-99~1
  ,Data_ResponseRequests_NoDK$E152.1==7~1
  ,Data_ResponseRequests_NoDK$E152.1==-99~1
  ,Data_ResponseRequests_NoDK$E157.1==8~1
  ,Data_ResponseRequests_NoDK$E157.1==-99~1
  ,Data_ResponseRequests_NoDK$E162.1==8~1
  ,Data_ResponseRequests_NoDK$E162.1==-99~1
  ,Data_ResponseRequests_NoDK$E167.1==8~1
  ,Data_ResponseRequests_NoDK$E167.1==-99~1
  ,Data_ResponseRequests_NoDK$E172.1==8~1
  ,Data_ResponseRequests_NoDK$E172.1==-99~1
  ,Data_ResponseRequests_NoDK$E177.1==8~1
  ,Data_ResponseRequests_NoDK$E177.1==-99~1
  ,Data_ResponseRequests_NoDK$E182.1==8~1
  ,Data_ResponseRequests_NoDK$E182.1==-99~1
  ,Data_ResponseRequests_NoDK$E187.1==8~1
  ,Data_ResponseRequests_NoDK$E187.1==-99~1
  ,Data_ResponseRequests_NoDK$E192.1==8~1
  ,Data_ResponseRequests_NoDK$E192.1==-99~1
  ,Data_ResponseRequests_NoDK$E197.1==11~1
  ,Data_ResponseRequests_NoDK$E197.1==-99~1
  ,Data_ResponseRequests_NoDK$E202.1==11~1
  ,Data_ResponseRequests_NoDK$E202.1==-99~1
  ,Data_ResponseRequests_NoDK$E207.1==11~1
  ,Data_ResponseRequests_NoDK$E207.1==-99~1
  ,Data_ResponseRequests_NoDK$E212.1==11~1
  ,Data_ResponseRequests_NoDK$E212.1==-99~1
  ,Data_ResponseRequests_NoDK$E217.1==12~1
  ,Data_ResponseRequests_NoDK$E217.1==-99~1
  ,Data_ResponseRequests_NoDK$E222.1==12~1
  ,Data_ResponseRequests_NoDK$E222.1==-99~1
  ,Data_ResponseRequests_NoDK$E227.1==12~1
  ,Data_ResponseRequests_NoDK$E227.1==-99~1
  ,Data_ResponseRequests_NoDK$E232.1==12~1
  ,Data_ResponseRequests_NoDK$E232.1==-99~1
  ,Data_ResponseRequests_NoDK$E238.1_1==101~1
  ,Data_ResponseRequests_NoDK$E238.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E243.1_1==101~1
  ,Data_ResponseRequests_NoDK$E243.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E248.1_1==101~1
  ,Data_ResponseRequests_NoDK$E248.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E253.1_1==101~1
  ,Data_ResponseRequests_NoDK$E253.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E258.1_1==101~1
  ,Data_ResponseRequests_NoDK$E258.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E263.1_1==101~1
  ,Data_ResponseRequests_NoDK$E263.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E268.1_1==101~1
  ,Data_ResponseRequests_NoDK$E268.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E273.1_1==101~1
  ,Data_ResponseRequests_NoDK$E273.1_1==-99~1
  ,TRUE ~ 0
)

## Testing for differences between treatment and control groups (Manuscript Figures 3 and 4, Supplementary Material 6.2)  ####

t.test(MarketIntervention_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests_NoDK)
ttest_ResponseRequests_NoDK_MarketIntervention_Skipped <- t.test(MarketIntervention_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests_NoDK)
tidy_ttest_ResponseRequests_NoDK_MarketIntervention_Skipped <- broom::tidy(ttest_ResponseRequests_NoDK_MarketIntervention_Skipped)
write.xlsx(tidy_ttest_ResponseRequests_NoDK_MarketIntervention_Skipped, "./results/tidy_ttest_ResponseRequests_NoDK_MarketIntervention_Skipped.xlsx")

# Creating environmentalism skipping variable #####

Data_ResponseRequests_NoDK$Environmentalism_Skipped <- case_when(
  Data_ResponseRequests_NoDK$E14.1==3~1
  ,Data_ResponseRequests_NoDK$E14.1==-99~1
  ,Data_ResponseRequests_NoDK$E20.1==3~1
  ,Data_ResponseRequests_NoDK$E20.1==-99~1
  ,Data_ResponseRequests_NoDK$E26.1==3~1
  ,Data_ResponseRequests_NoDK$E26.1==-99~1
  ,Data_ResponseRequests_NoDK$E32.1==3~1
  ,Data_ResponseRequests_NoDK$E32.1==-99~1
  ,Data_ResponseRequests_NoDK$E37.1==5~1
  ,Data_ResponseRequests_NoDK$E37.1==-99~1
  ,Data_ResponseRequests_NoDK$E42.1==5~1
  ,Data_ResponseRequests_NoDK$E42.1==-99~1
  ,Data_ResponseRequests_NoDK$E47.1==5~1
  ,Data_ResponseRequests_NoDK$E47.1==-99~1
  ,Data_ResponseRequests_NoDK$E52.1==5~1
  ,Data_ResponseRequests_NoDK$E52.1==-99~1
  ,Data_ResponseRequests_NoDK$E58.1==5~1
  ,Data_ResponseRequests_NoDK$E58.1==-99~1
  ,Data_ResponseRequests_NoDK$E63.1==5~1
  ,Data_ResponseRequests_NoDK$E63.1==-99~1
  ,Data_ResponseRequests_NoDK$E68.1==5~1
  ,Data_ResponseRequests_NoDK$E68.1==-99~1
  ,Data_ResponseRequests_NoDK$E73.1==5~1
  ,Data_ResponseRequests_NoDK$E73.1==-99~1
  ,Data_ResponseRequests_NoDK$E78.1==6~1
  ,Data_ResponseRequests_NoDK$E78.1==-99~1
  ,Data_ResponseRequests_NoDK$E83.1==6~1
  ,Data_ResponseRequests_NoDK$E83.1==-99~1
  ,Data_ResponseRequests_NoDK$E88.1==6~1
  ,Data_ResponseRequests_NoDK$E88.1==-99~1
  ,Data_ResponseRequests_NoDK$E93.1==6~1
  ,Data_ResponseRequests_NoDK$E93.1==-99~1
  ,Data_ResponseRequests_NoDK$E98.1==6~1
  ,Data_ResponseRequests_NoDK$E98.1==-99~1
  ,Data_ResponseRequests_NoDK$E103.1==6~1
  ,Data_ResponseRequests_NoDK$E103.1==-99~1
  ,Data_ResponseRequests_NoDK$E108.1==6~1
  ,Data_ResponseRequests_NoDK$E108.1==-99~1
  ,Data_ResponseRequests_NoDK$E113.1==6~1
  ,Data_ResponseRequests_NoDK$E113.1==-99~1
  ,Data_ResponseRequests_NoDK$E118.1==7~1
  ,Data_ResponseRequests_NoDK$E118.1==-99~1
  ,Data_ResponseRequests_NoDK$E123.1==7~1
  ,Data_ResponseRequests_NoDK$E123.1==-99~1
  ,Data_ResponseRequests_NoDK$E128.1==7~1
  ,Data_ResponseRequests_NoDK$E128.1==-99~1
  ,Data_ResponseRequests_NoDK$E133.1==7~1
  ,Data_ResponseRequests_NoDK$E133.1==-99~1
  ,Data_ResponseRequests_NoDK$E138.1==7~1
  ,Data_ResponseRequests_NoDK$E138.1==-99~1
  ,Data_ResponseRequests_NoDK$E143.1==7~1
  ,Data_ResponseRequests_NoDK$E143.1==-99~1
  ,Data_ResponseRequests_NoDK$E148.1==7~1
  ,Data_ResponseRequests_NoDK$E148.1==-99~1
  ,Data_ResponseRequests_NoDK$E153.1==7~1
  ,Data_ResponseRequests_NoDK$E153.1==-99~1
  ,Data_ResponseRequests_NoDK$E156.1==8~1
  ,Data_ResponseRequests_NoDK$E156.1==-99~1
  ,Data_ResponseRequests_NoDK$E161.1==8~1
  ,Data_ResponseRequests_NoDK$E161.1==-99~1
  ,Data_ResponseRequests_NoDK$E168.1==8~1
  ,Data_ResponseRequests_NoDK$E168.1==-99~1
  ,Data_ResponseRequests_NoDK$E173.1==8~1
  ,Data_ResponseRequests_NoDK$E173.1==-99~1
  ,Data_ResponseRequests_NoDK$E178.1==8~1
  ,Data_ResponseRequests_NoDK$E178.1==-99~1
  ,Data_ResponseRequests_NoDK$E183.1==8~1
  ,Data_ResponseRequests_NoDK$E183.1==-99~1
  ,Data_ResponseRequests_NoDK$E188.1==8~1
  ,Data_ResponseRequests_NoDK$E188.1==-99~1
  ,Data_ResponseRequests_NoDK$E193.1==8~1
  ,Data_ResponseRequests_NoDK$E193.1==-99~1
  ,Data_ResponseRequests_NoDK$E198.1==11~1
  ,Data_ResponseRequests_NoDK$E198.1==-99~1
  ,Data_ResponseRequests_NoDK$E203.1==11~1
  ,Data_ResponseRequests_NoDK$E203.1==-99~1
  ,Data_ResponseRequests_NoDK$E208.1==11~1
  ,Data_ResponseRequests_NoDK$E208.1==-99~1
  ,Data_ResponseRequests_NoDK$E213.1==11~1
  ,Data_ResponseRequests_NoDK$E213.1==-99~1
  ,Data_ResponseRequests_NoDK$E218.1==12~1
  ,Data_ResponseRequests_NoDK$E218.1==-99~1
  ,Data_ResponseRequests_NoDK$E223.1==12~1
  ,Data_ResponseRequests_NoDK$E223.1==-99~1
  ,Data_ResponseRequests_NoDK$E228.1==12~1
  ,Data_ResponseRequests_NoDK$E228.1==-99~1
  ,Data_ResponseRequests_NoDK$E233.1==12~1
  ,Data_ResponseRequests_NoDK$E233.1==-99~1
  ,Data_ResponseRequests_NoDK$E239.1_1==101~1
  ,Data_ResponseRequests_NoDK$E239.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E244.1_1==101~1
  ,Data_ResponseRequests_NoDK$E244.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E249.1_1==101~1
  ,Data_ResponseRequests_NoDK$E249.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E254.1_1==101~1
  ,Data_ResponseRequests_NoDK$E254.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E259.1_1==101~1
  ,Data_ResponseRequests_NoDK$E259.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E264.1_1==101~1
  ,Data_ResponseRequests_NoDK$E264.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E269.1_1==101~1
  ,Data_ResponseRequests_NoDK$E269.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E274.1_1==101~1
  ,Data_ResponseRequests_NoDK$E274.1_1==-99~1
  ,TRUE ~ 0
)

## Testing for differences between treatment and control groups (Manuscript Figures 3 and 4, Supplementary Material 6.2)  ####

t.test(Environmentalism_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests_NoDK)
ttest_ResponseRequests_NoDK_Environmentalism_Skipped <- t.test(Environmentalism_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests_NoDK)
tidy_ttest_ResponseRequests_NoDK_Environmentalism_Skipped <- broom::tidy(ttest_ResponseRequests_NoDK_Environmentalism_Skipped)
write.xlsx(tidy_ttest_ResponseRequests_NoDK_Environmentalism_Skipped, "./results/tidy_ttest_ResponseRequests_NoDK_Environmentalism_Skipped.xlsx")

# Creating global responsibility skipping variable #####

Data_ResponseRequests_NoDK$GlobalResponsibility_Skipped <- case_when(
  Data_ResponseRequests_NoDK$E15.1==3~1
  ,Data_ResponseRequests_NoDK$E15.1==-99~1
  ,Data_ResponseRequests_NoDK$E21.1==3~1
  ,Data_ResponseRequests_NoDK$E21.1==-99~1
  ,Data_ResponseRequests_NoDK$E27.1==3~1
  ,Data_ResponseRequests_NoDK$E27.1==-99~1
  ,Data_ResponseRequests_NoDK$E33.1==3~1
  ,Data_ResponseRequests_NoDK$E33.1==-99~1
  ,Data_ResponseRequests_NoDK$E38.1==5~1
  ,Data_ResponseRequests_NoDK$E38.1==-99~1
  ,Data_ResponseRequests_NoDK$E43.1==5~1
  ,Data_ResponseRequests_NoDK$E43.1==-99~1
  ,Data_ResponseRequests_NoDK$E48.1==5~1
  ,Data_ResponseRequests_NoDK$E48.1==-99~1
  ,Data_ResponseRequests_NoDK$E53.1==5~1
  ,Data_ResponseRequests_NoDK$E53.1==-99~1
  ,Data_ResponseRequests_NoDK$E59.1==5~1
  ,Data_ResponseRequests_NoDK$E59.1==-99~1
  ,Data_ResponseRequests_NoDK$E64.1==5~1
  ,Data_ResponseRequests_NoDK$E64.1==-99~1
  ,Data_ResponseRequests_NoDK$E69.1==5~1
  ,Data_ResponseRequests_NoDK$E69.1==-99~1
  ,Data_ResponseRequests_NoDK$E74.1==5~1
  ,Data_ResponseRequests_NoDK$E74.1==-99~1
  ,Data_ResponseRequests_NoDK$E79.1==6~1
  ,Data_ResponseRequests_NoDK$E79.1==-99~1
  ,Data_ResponseRequests_NoDK$E84.1==6~1
  ,Data_ResponseRequests_NoDK$E84.1==-99~1
  ,Data_ResponseRequests_NoDK$E89.1==6~1
  ,Data_ResponseRequests_NoDK$E89.1==-99~1
  ,Data_ResponseRequests_NoDK$E94.1==6~1
  ,Data_ResponseRequests_NoDK$E94.1==-99~1
  ,Data_ResponseRequests_NoDK$E99.1==6~1
  ,Data_ResponseRequests_NoDK$E99.1==-99~1
  ,Data_ResponseRequests_NoDK$E104.1==6~1
  ,Data_ResponseRequests_NoDK$E104.1==-99~1
  ,Data_ResponseRequests_NoDK$E109.1==6~1
  ,Data_ResponseRequests_NoDK$E109.1==-99~1
  ,Data_ResponseRequests_NoDK$E114.1==6~1
  ,Data_ResponseRequests_NoDK$E114.1==-99~1
  ,Data_ResponseRequests_NoDK$E119.1==7~1
  ,Data_ResponseRequests_NoDK$E119.1==-99~1
  ,Data_ResponseRequests_NoDK$E124.1==7~1
  ,Data_ResponseRequests_NoDK$E124.1==-99~1
  ,Data_ResponseRequests_NoDK$E129.1==7~1
  ,Data_ResponseRequests_NoDK$E129.1==-99~1
  ,Data_ResponseRequests_NoDK$E134.1==7~1
  ,Data_ResponseRequests_NoDK$E134.1==-99~1
  ,Data_ResponseRequests_NoDK$E139.1==7~1
  ,Data_ResponseRequests_NoDK$E139.1==-99~1
  ,Data_ResponseRequests_NoDK$E144.1==7~1
  ,Data_ResponseRequests_NoDK$E144.1==-99~1
  ,Data_ResponseRequests_NoDK$E149.1==7~1
  ,Data_ResponseRequests_NoDK$E149.1==-99~1
  ,Data_ResponseRequests_NoDK$E154.1==7~1
  ,Data_ResponseRequests_NoDK$E154.1==-99~1
  ,Data_ResponseRequests_NoDK$E159.1==8~1
  ,Data_ResponseRequests_NoDK$E159.1==-99~1
  ,Data_ResponseRequests_NoDK$E164.1==8~1
  ,Data_ResponseRequests_NoDK$E164.1==-99~1
  ,Data_ResponseRequests_NoDK$E169.1==8~1
  ,Data_ResponseRequests_NoDK$E169.1==-99~1
  ,Data_ResponseRequests_NoDK$E174.1==8~1
  ,Data_ResponseRequests_NoDK$E174.1==-99~1
  ,Data_ResponseRequests_NoDK$E179.1==8~1
  ,Data_ResponseRequests_NoDK$E179.1==-99~1
  ,Data_ResponseRequests_NoDK$E184.1==8~1
  ,Data_ResponseRequests_NoDK$E184.1==-99~1
  ,Data_ResponseRequests_NoDK$E189.1==8~1
  ,Data_ResponseRequests_NoDK$E189.1==-99~1
  ,Data_ResponseRequests_NoDK$E194.1==8~1
  ,Data_ResponseRequests_NoDK$E194.1==-99~1
  ,Data_ResponseRequests_NoDK$E199.1==11~1
  ,Data_ResponseRequests_NoDK$E199.1==-99~1
  ,Data_ResponseRequests_NoDK$E204.1==11~1
  ,Data_ResponseRequests_NoDK$E204.1==-99~1
  ,Data_ResponseRequests_NoDK$E209.1==11~1
  ,Data_ResponseRequests_NoDK$E209.1==-99~1
  ,Data_ResponseRequests_NoDK$E214.1==11~1
  ,Data_ResponseRequests_NoDK$E214.1==-99~1
  ,Data_ResponseRequests_NoDK$E219.1==12~1
  ,Data_ResponseRequests_NoDK$E219.1==-99~1
  ,Data_ResponseRequests_NoDK$E224.1==12~1
  ,Data_ResponseRequests_NoDK$E224.1==-99~1
  ,Data_ResponseRequests_NoDK$E229.1==12~1
  ,Data_ResponseRequests_NoDK$E229.1==-99~1
  ,Data_ResponseRequests_NoDK$E234.1==12~1
  ,Data_ResponseRequests_NoDK$E234.1==-99~1
  ,Data_ResponseRequests_NoDK$E240.1_1==101~1
  ,Data_ResponseRequests_NoDK$E240.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E245.1_1==101~1
  ,Data_ResponseRequests_NoDK$E245.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E250.1_1==101~1
  ,Data_ResponseRequests_NoDK$E250.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E255.1_1==101~1
  ,Data_ResponseRequests_NoDK$E255.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E260.1_1==101~1
  ,Data_ResponseRequests_NoDK$E260.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E265.1_1==101~1
  ,Data_ResponseRequests_NoDK$E265.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E270.1_1==101~1
  ,Data_ResponseRequests_NoDK$E270.1_1==-99~1
  ,Data_ResponseRequests_NoDK$E275.1_1==101~1
  ,Data_ResponseRequests_NoDK$E275.1_1==-99~1
  ,TRUE ~ 0
)

## Testing for differences between treatment and control groups (Manuscript Figures 3 and 4, Supplementary Material 6.2)  ####

t.test(GlobalResponsibility_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests_NoDK)
ttest_ResponseRequests_NoDK_GlobalResponsibility_Skipped <- t.test(GlobalResponsibility_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests_NoDK)
tidy_ttest_ResponseRequests_NoDK_GlobalResponsibility_Skipped <- broom::tidy(ttest_ResponseRequests_NoDK_GlobalResponsibility_Skipped)
write.xlsx(tidy_ttest_ResponseRequests_NoDK_GlobalResponsibility_Skipped, "./results/tidy_ttest_ResponseRequests_NoDK_GlobalResponsibility_Skipped.xlsx")

# Creating IMF knowledge skipping variable ####

Data_ResponseRequests_NoDK$IMFknowledge_Skipped <- case_when(
  Data_ResponseRequests_NoDK$E10.1 == 31 ~ 1,
  Data_ResponseRequests_NoDK$E10.1 == -99 ~ 1,
  Data_ResponseRequests_NoDK$E16.1 == 31 ~ 1,
  Data_ResponseRequests_NoDK$E16.1 == -99 ~ 1,
  Data_ResponseRequests_NoDK$E22.1 == -99 ~ 1,
  Data_ResponseRequests_NoDK$E28.1 == -99 ~ 1,
  TRUE ~ 0
)



## Testing for differences between treatment and control groups (Manuscript Figures 3 and 4, Supplementary Material 6.2)  ####

t.test(IMFknowledge_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests_NoDK)
ttest_ResponseRequests_NoDK_IMFknowledge_Skipped <- t.test(IMFknowledge_Skipped ~ ResponseRequest_0or1, data=Data_ResponseRequests_NoDK)
tidy_ttest_ResponseRequests_NoDK_IMFknowledge_Skipped <- broom::tidy(ttest_ResponseRequests_NoDK_IMFknowledge_Skipped)
write.xlsx(tidy_ttest_ResponseRequests_NoDK_IMFknowledge_Skipped, "./results/tidy_ttest_ResponseRequests_NoDK_IMFknowledge_Skipped.xlsx")

## Random effects model (Manuscript Table 2; Appendices 7.1.2, 7.2.2, 7.3.2, 7.4.2, 7.5.2, 7.6.2, 7.7.2, 7.8.2) ----

ResponseRequest_BayesRandomEffects_NoDK_WorldPoliticsInterest_Skipped <- brm(WorldPoliticsInterest_Skipped ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests_NoDK)
summary(ResponseRequest_BayesRandomEffects_NoDK_WorldPoliticsInterest_Skipped)
coef(ResponseRequest_BayesRandomEffects_NoDK_WorldPoliticsInterest_Skipped)

ResponseRequest_BayesRandomEffects_NoDK_CulturalNorms_Skipped <- brm(CulturalNorms_Skipped ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests_NoDK)
summary(ResponseRequest_BayesRandomEffects_NoDK_CulturalNorms_Skipped)
coef(ResponseRequest_BayesRandomEffects_NoDK_CulturalNorms_Skipped)

ResponseRequest_BayesRandomEffects_NoDK_Environmentalism_Skipped <- brm(Environmentalism_Skipped ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests_NoDK)
summary(ResponseRequest_BayesRandomEffects_NoDK_Environmentalism_Skipped)
coef(ResponseRequest_BayesRandomEffects_NoDK_Environmentalism_Skipped)

ResponseRequest_BayesRandomEffects_NoDK_IMFknowledge_Skipped <- brm(IMFknowledge_Skipped ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests_NoDK)
summary(ResponseRequest_BayesRandomEffects_NoDK_IMFknowledge_Skipped)
coef(ResponseRequest_BayesRandomEffects_NoDK_IMFknowledge_Skipped)

ResponseRequest_BayesRandomEffects_NoDK_GlobalResponsibility_Skipped <- brm(GlobalResponsibility_Skipped ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests_NoDK)
summary(ResponseRequest_BayesRandomEffects_NoDK_GlobalResponsibility_Skipped)
coef(ResponseRequest_BayesRandomEffects_NoDK_GlobalResponsibility_Skipped)

ResponseRequest_BayesRandomEffects_NoDK_MarketIntervention_Skipped <- brm(MarketIntervention_Skipped ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests_NoDK)
summary(ResponseRequest_BayesRandomEffects_NoDK_MarketIntervention_Skipped)
coef(ResponseRequest_BayesRandomEffects_NoDK_MarketIntervention_Skipped)

ResponseRequest_BayesRandomEffects_NoDK_IMFknowledge_RightAmongAll <- brm(IMFknowledge_RightAmongAll ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests_NoDK)
summary(ResponseRequest_BayesRandomEffects_NoDK_IMFknowledge_RightAmongAll)
coef(ResponseRequest_BayesRandomEffects_NoDK_IMFknowledge_RightAmongAll)

ResponseRequest_BayesRandomEffects_NoDK_IMFknowledge_RightAmongSubstantive <- brm(IMFknowledge_RightAmongSubstantive ~ ResponseRequest_0or1 + (1 + ResponseRequest_0or1 | Country), Data_ResponseRequests_NoDK)
summary(ResponseRequest_BayesRandomEffects_NoDK_IMFknowledge_RightAmongSubstantive)
coef(ResponseRequest_BayesRandomEffects_NoDK_IMFknowledge_RightAmongSubstantive)



# Supplementary Material 2: Country sample characteristics ####
## Australia ####

Data_Australia$AgeGenderGroup <- case_when(
  Data_Australia$Males_18to24 == 1 ~ "Men, 18-24",
  Data_Australia$Males_25to34 == 1 ~ "Men, 25-34",
  Data_Australia$Males_35to44 == 1 ~ "Men, 35-44",
  Data_Australia$Males_45to54 == 1 ~ "Men, 45-54",
  Data_Australia$Males_55to64 == 1 ~ "Men, 55-64",
  Data_Australia$Males_65plus == 1 ~ "Men, 65+",
  Data_Australia$Females_18to24 == 1 ~ "Women, 18-24",
  Data_Australia$Females_25to34 == 1 ~ "Women, 25-34",
  Data_Australia$Females_35to44 == 1 ~ "Women, 35-44",
  Data_Australia$Females_45to54 == 1 ~ "Women, 45-54",
  Data_Australia$Females_55to64 == 1 ~ "Women, 55-64",
  Data_Australia$Females_65plus == 1 ~ "Women, 65+",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Australia$AgeGenderGroup), "./results/Australia_AgeGender_counts.xlsx")

Data_Australia$EducationGroup <- case_when(
  Data_Australia$Education_Primary == 1 ~ "Primary",
  Data_Australia$Education_Secondary == 1 ~ "Secondary",
  Data_Australia$Education_Tertiary == 1 ~ "Tertiary",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Australia$EducationGroup), "./results/Australia_Education_counts.xlsx")

Data_Australia$Region <- case_when(
  Data_Australia$Region_AustralianCapitalTerritory == 1 ~ "Australian Capital Territory",
  Data_Australia$Region_NewSouthWales == 1 ~ "New South Wales",
  Data_Australia$Region_NorthernTerritory == 1 ~ "Northern Territory",
  Data_Australia$Region_Queensland == 1 ~ "Queensland",
  Data_Australia$Region_SouthAustralia == 1 ~ "South Australia",
  Data_Australia$Region_Tasmania == 1 ~ "Tasmania",
  Data_Australia$Region_Victoria == 1 ~ "Victoria",
  Data_Australia$Region_WesternAustralia == 1 ~ "Western Australia",
  Data_Australia$Region_OtherAustralianTerritory == 1 ~ "Other Australian territory",
  Data_Australia$Region_OutsideAustralia == 1 ~ "Outside Australia",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Australia$Region), "./results/Australia_Region_counts.xlsx.xlsx")

## Canada ####

Data_Canada$AgeGenderGroup <- case_when(
  Data_Canada$Males_18to24 == 1 ~ "Men, 18-24",
  Data_Canada$Males_25to34 == 1 ~ "Men, 25-34",
  Data_Canada$Males_35to44 == 1 ~ "Men, 35-44",
  Data_Canada$Males_45to54 == 1 ~ "Men, 45-54",
  Data_Canada$Males_55to64 == 1 ~ "Men, 55-64",
  Data_Canada$Males_65plus == 1 ~ "Men, 65+",
  Data_Canada$Females_18to24 == 1 ~ "Women, 18-24",
  Data_Canada$Females_25to34 == 1 ~ "Women, 25-34",
  Data_Canada$Females_35to44 == 1 ~ "Women, 35-44",
  Data_Canada$Females_45to54 == 1 ~ "Women, 45-54",
  Data_Canada$Females_55to64 == 1 ~ "Women, 55-64",
  Data_Canada$Females_65plus == 1 ~ "Women, 65+",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Canada$AgeGenderGroup), "./results/Canada_AgeGenderGroup_counts.xlsx")

Data_Canada$EducationGroup <- case_when(
  Data_Canada$Education_Primary == 1 ~ "Primary",
  Data_Canada$Education_Secondary == 1 ~ "Secondary",
  Data_Canada$Education_Tertiary == 1 ~ "Tertiary",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Canada$EducationGroup), "./results/Canada_EducationGroup_counts.xlsx")


Data_Canada$Region <- case_when(
  Data_Canada$Region_Alberta == 1 ~ "Alberta",
  Data_Canada$Region_BritishColumbia == 1 ~ "British Columbia",
  Data_Canada$Region_Manitoba == 1 ~ "Manitoba",
  Data_Canada$Region_NewBrunswick == 1 ~ "New Brunswick",
  Data_Canada$Region_NewfoundlandAndLabrador == 1 ~ "Newfoundland and Labrador",
  Data_Canada$Region_NorthwestTerritories == 1 ~ "Northwest territories",
  Data_Canada$Region_NovaScotia == 1 ~ "Nova Scotia",
  Data_Canada$Region_Nunavut == 1 ~ "Nunavut",
  Data_Canada$Region_Ontario == 1 ~ "Ontario",
  Data_Canada$Region_PrinceEdwardIsland == 1 ~ "Prince Edward Island",
  Data_Canada$Region_Quebec == 1 ~ "Quebec",
  Data_Canada$Region_Saskatchewan == 1 ~ "Saskatchewan",
  Data_Canada$Region_Yukon == 1 ~ "Yukon",
  Data_Canada$Region_OutsideCanada == 1 ~ "Outside Canada",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Canada$Region), "./results/Canada_Region_counts.xlsx")

## Colombia ####

Data_Colombia$AgeGenderGroup <- case_when(
  Data_Colombia$Males_18to24 == 1 ~ "Men, 18-24",
  Data_Colombia$Males_25to34 == 1 ~ "Men, 25-34",
  Data_Colombia$Males_35to44 == 1 ~ "Men, 35-44",
  Data_Colombia$Males_45to54 == 1 ~ "Men, 45-54",
  Data_Colombia$Males_55to64 == 1 ~ "Men, 55-64",
  Data_Colombia$Males_65plus == 1 ~ "Men, 65+",
  Data_Colombia$Females_18to24 == 1 ~ "Women, 18-24",
  Data_Colombia$Females_25to34 == 1 ~ "Women, 25-34",
  Data_Colombia$Females_35to44 == 1 ~ "Women, 35-44",
  Data_Colombia$Females_45to54 == 1 ~ "Women, 45-54",
  Data_Colombia$Females_55to64 == 1 ~ "Women, 55-64",
  Data_Colombia$Females_65plus == 1 ~ "Women, 65+",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Colombia$AgeGenderGroup), "./results/Colombia_AgeGenderGroup_counts.xlsx")

Data_Colombia$EducationGroup <- case_when(
  Data_Colombia$Education_Primary == 1 ~ "Primary",
  Data_Colombia$Education_Secondary == 1 ~ "Secondary",
  Data_Colombia$Education_Tertiary == 1 ~ "Tertiary",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Colombia$EducationGroup), "./results/Colombia_EducationGroup_counts.xlsx")

Data_Colombia$Region <- case_when(
  Data_Colombia$Region_Amazonas == 1 ~ "Amazonas",
  Data_Colombia$Region_Antioquia == 1 ~ "Antioquia",
  Data_Colombia$Region_Arauca == 1 ~ "Arauca",
  Data_Colombia$Region_Atlantico == 1 ~ "Atlantico",
  Data_Colombia$Region_Bogota == 1 ~ "Bogota",
  Data_Colombia$Region_Bolivar == 1 ~ "Bolivar",
  Data_Colombia$Region_Boyaca == 1 ~ "Boyaca",
  Data_Colombia$Region_Caldas == 1 ~ "Caldas",
  Data_Colombia$Region_Caqueta == 1 ~ "Caqueta",
  Data_Colombia$Region_Casanare == 1 ~ "Casanare",
  Data_Colombia$Region_Cauca == 1 ~ "Cauca",
  Data_Colombia$Region_Cesar == 1 ~ "Cesar",
  Data_Colombia$Region_Choco == 1 ~ "Choco",
  Data_Colombia$Region_Cordoba == 1 ~ "Cordoba",
  Data_Colombia$Region_Cundinamarca == 1 ~ "Cundinamarca",
  Data_Colombia$Region_Guainia == 1 ~ "Guainia",
  Data_Colombia$Region_Guaviare == 1 ~ "Guaviare",
  Data_Colombia$Region_Huila == 1 ~ "Huila",
  Data_Colombia$Region_LaGuajira == 1 ~ "La Guajira",
  Data_Colombia$Region_Magdalena == 1 ~ "Magdalena",
  Data_Colombia$Region_Meta == 1 ~ "Meta",
  Data_Colombia$Region_Narino == 1 ~ "Narino",
  Data_Colombia$Region_NorteDeSantander == 1 ~ "Norte de Santander",
  Data_Colombia$Region_Putumayo == 1 ~ "Putumayo",
  Data_Colombia$Region_Quindio == 1 ~ "Quindio",
  Data_Colombia$Region_Risaralda == 1 ~ "Risaralda",
  Data_Colombia$Region_SanAndresYProvidencia == 1 ~ "San Andres y Providencia",
  Data_Colombia$Region_Santander == 1 ~ "Santander",
  Data_Colombia$Region_Sucre == 1 ~ "Sucre",
  Data_Colombia$Region_Tolima == 1 ~ "Tolima",
  Data_Colombia$Region_ValleDelCauca == 1 ~ "Valle del Cauca",
  Data_Colombia$Region_Vaupes == 1 ~ "Vaupes",
  Data_Colombia$Region_Vichada == 1 ~ "Vichada",
  Data_Colombia$Region_OutsideColombia == 1 ~ "Outside Colombia",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Colombia$Region), "./results/Colombia_Region_counts.xlsx")


## Egypt ####

Data_Egypt$AgeGenderGroup <- case_when(
  Data_Egypt$Males_18to24 == 1 ~ "Men, 18-24",
  Data_Egypt$Males_25to34 == 1 ~ "Men, 25-34",
  Data_Egypt$Males_35to44 == 1 ~ "Men, 35-44",
  Data_Egypt$Males_45to54 == 1 ~ "Men, 45-54",
  Data_Egypt$Males_55to64 == 1 ~ "Men, 55-64",
  Data_Egypt$Males_65plus == 1 ~ "Men, 65+",
  Data_Egypt$Females_18to24 == 1 ~ "Women, 18-24",
  Data_Egypt$Females_25to34 == 1 ~ "Women, 25-34",
  Data_Egypt$Females_35to44 == 1 ~ "Women, 35-44",
  Data_Egypt$Females_45to54 == 1 ~ "Women, 45-54",
  Data_Egypt$Females_55to64 == 1 ~ "Women, 55-64",
  Data_Egypt$Females_65plus == 1 ~ "Women, 65+",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Egypt$AgeGenderGroup), "./results/Egypt_AgeGenderGroup_counts.xlsx")

Data_Egypt$EducationGroup <- case_when(
  Data_Egypt$Education_Primary == 1 ~ "Primary",
  Data_Egypt$Education_Secondary == 1 ~ "Secondary",
  Data_Egypt$Education_Tertiary == 1 ~ "Tertiary",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Egypt$EducationGroup), "./results/Egypt_EducationGroup_counts.xlsx")

Data_Egypt$Region <- case_when(
  Data_Egypt$Region_Aswan == 1 ~ "Aswan",
  Data_Egypt$Region_Asyut == 1 ~ "Asyut",
  Data_Egypt$Region_Alexandria == 1 ~ "Alexandria",
  Data_Egypt$Region_Ismailia == 1 ~ "Ismailia",
  Data_Egypt$Region_Luxor == 1 ~ "Luxor",
  Data_Egypt$Region_RedSea == 1 ~ "Red Sea",
  Data_Egypt$Region_Beheira == 1 ~ "Beheira",
  Data_Egypt$Region_Giza == 1 ~ "Giza",
  Data_Egypt$Region_Dakahlia == 1 ~ "Dakahlia",
  Data_Egypt$Region_Suez == 1 ~ "Suez",
  Data_Egypt$Region_Sharqia == 1 ~ "Sharqia",
  Data_Egypt$Region_Gharbia == 1 ~ "Gharbia",
  Data_Egypt$Region_Faiyum == 1 ~ "Faiyum",
  Data_Egypt$Region_Cairo == 1 ~ "Cairo",
  Data_Egypt$Region_Qalyubia == 1 ~ "Qalyubia",
  Data_Egypt$Region_Monufia == 1 ~ "Monufia",
  Data_Egypt$Region_Minya == 1 ~ "Minya",
  Data_Egypt$Region_NewValley == 1 ~ "New Valley",
  Data_Egypt$Region_BeniSuef == 1 ~ "Beni Suef",
  Data_Egypt$Region_PortSaid == 1 ~ "Port Said",
  Data_Egypt$Region_SouthSinai == 1 ~ "South Sinai",
  Data_Egypt$Region_Damietta == 1 ~ "Damietta",
  Data_Egypt$Region_Sohag == 1 ~ "Sohag",
  Data_Egypt$Region_NorthSinai == 1 ~ "North Sinai",
  Data_Egypt$Region_Qena == 1 ~ "Qena",
  Data_Egypt$Region_KafrElSheikh == 1 ~ "Kafr El Sheikh",
  Data_Egypt$Region_Matruh == 1 ~ "Matruh",
  Data_Egypt$Region_OutsideEgypt == 1 ~ "Outside Egypt",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Egypt$Region), "./results/Egypt_Region_counts.xlsx")


## France ####

Data_France$AgeGenderGroup <- case_when(
  Data_France$Males_18to24 == 1 ~ "Men, 18-24",
  Data_France$Males_25to34 == 1 ~ "Men, 25-34",
  Data_France$Males_35to44 == 1 ~ "Men, 35-44",
  Data_France$Males_45to54 == 1 ~ "Men, 45-54",
  Data_France$Males_55to64 == 1 ~ "Men, 55-64",
  Data_France$Males_65plus == 1 ~ "Men, 65+",
  Data_France$Females_18to24 == 1 ~ "Women, 18-24",
  Data_France$Females_25to34 == 1 ~ "Women, 25-34",
  Data_France$Females_35to44 == 1 ~ "Women, 35-44",
  Data_France$Females_45to54 == 1 ~ "Women, 45-54",
  Data_France$Females_55to64 == 1 ~ "Women, 55-64",
  Data_France$Females_65plus == 1 ~ "Women, 65+",
  TRUE ~ "Unknown")

write.xlsx(table(Data_France$AgeGenderGroup), "./results/France_AgeGenderGroup_counts.xlsx")

Data_France$EducationGroup <- case_when(
  Data_France$Education_Primary == 1 ~ "Primary",
  Data_France$Education_Secondary == 1 ~ "Secondary",
  Data_France$Education_Tertiary == 1 ~ "Tertiary",
  TRUE ~ "Unknown")

write.xlsx(table(Data_France$EducationGroup), "./results/France_Education_counts.xlsx")

Data_France$Region <- case_when(
  Data_France$Region_AuvergneRhoneAlpes == 1 ~ "Auvergne Rhone Alpes",
  Data_France$Region_BourgogneFrancheComte == 1 ~ "Bourgogne Franche Comte",
  Data_France$Region_Bretagne == 1 ~ "Bretagne",
  Data_France$Region_CentreValdeLoire == 1 ~ "Centre Val de Loire",
  Data_France$Region_Corse == 1 ~ "Corse",
  Data_France$Region_GrandEst == 1 ~ "Grand Est",
  Data_France$Region_Guadeloupe == 1 ~ "Guadeloupe",
  Data_France$Region_Guyane == 1 ~ "Guyane",
  Data_France$Region_HautsdeFrance == 1 ~ "Hauts de France",
  Data_France$Region_IledeFrance == 1 ~ "Ile de France",
  Data_France$Region_LaReunion == 1 ~ "La Reunion",
  Data_France$Region_Martinique == 1 ~ "Martinique",
  Data_France$Region_Mayotte == 1 ~ "Mayotte",
  Data_France$Region_Normandie == 1 ~ "Normandie",
  Data_France$Region_NouvelleAquitaine == 1 ~ "Nouvelle Aquitaine",
  Data_France$Region_Occitanie == 1 ~ "Occitanie",
  Data_France$Region_PaysdelaLoire == 1 ~ "Pays de la Loire",
  Data_France$Region_ProvenceAlpesCotedAzur == 1 ~ "Provence Alpes Cote d'Azur",
  Data_France$Region_OutsideFrance == 1 ~ "Outside France",
  TRUE ~ "Unknown")

write.xlsx(table(Data_France$Region), "./results/France_Region_counts.xlsx")

## Hungary ####

Data_Hungary$AgeGenderGroup <- case_when(
  Data_Hungary$Males_18to24 == 1 ~ "Men, 18-24",
  Data_Hungary$Males_25to34 == 1 ~ "Men, 25-34",
  Data_Hungary$Males_35to44 == 1 ~ "Men, 35-44",
  Data_Hungary$Males_45to54 == 1 ~ "Men, 45-54",
  Data_Hungary$Males_55to64 == 1 ~ "Men, 55-64",
  Data_Hungary$Males_65plus == 1 ~ "Men, 65+",
  Data_Hungary$Females_18to24 == 1 ~ "Women, 18-24",
  Data_Hungary$Females_25to34 == 1 ~ "Women, 25-34",
  Data_Hungary$Females_35to44 == 1 ~ "Women, 35-44",
  Data_Hungary$Females_45to54 == 1 ~ "Women, 45-54",
  Data_Hungary$Females_55to64 == 1 ~ "Women, 55-64",
  Data_Hungary$Females_65plus == 1 ~ "Women, 65+",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Hungary$AgeGenderGroup), "./results/Hungary_AgeGender_counts.xlsx")

Data_Hungary$EducationGroup <- case_when(
  Data_Hungary$Education_Primary == 1 ~ "Primary",
  Data_Hungary$Education_Secondary == 1 ~ "Secondary",
  Data_Hungary$Education_Tertiary == 1 ~ "Tertiary",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Hungary$EducationGroup), "./results/Hungary_Education_counts.xlsx")

Data_Hungary$Region <- case_when(
  Data_Hungary$Region_Baranya == 1 ~ "Baranya",
  Data_Hungary$Region_BacsKiskun == 1 ~ "Bacs Kiskun",
  Data_Hungary$Region_Bekes == 1 ~ "Bekes",
  Data_Hungary$Region_BorsodAbaujZemplen == 1 ~ "Borsod Abauj Zemplen",
  Data_Hungary$Region_Budapest == 1 ~ "Budapest",
  Data_Hungary$Region_CsongradCsanad == 1 ~ "Csongrad Csanad",
  Data_Hungary$Region_Fejer == 1 ~ "Fejer",
  Data_Hungary$Region_GyorMosonSopron == 1 ~ "Gyor Moson Sopron",
  Data_Hungary$Region_HajduBihar == 1 ~ "Hajdu Bihar",
  Data_Hungary$Region_Heves == 1 ~ "Heves",
  Data_Hungary$Region_JaszNagykunSzolnok == 1 ~ "Jasz Nagykun Szolnok",
  Data_Hungary$Region_KomaromEsztergom == 1 ~ "Komarom Esztergom",
  Data_Hungary$Region_Nograd == 1 ~ "Nograd",
  Data_Hungary$Region_Pest == 1 ~ "Pest",
  Data_Hungary$Region_Somogy == 1 ~ "Somogy",
  Data_Hungary$Region_SzabolcsSzatmarBereg == 1 ~ "Szabolcz Szatmar Bereg",
  Data_Hungary$Region_Tolna == 1 ~ "Tolna",
  Data_Hungary$Region_Vas == 1 ~ "Vas",
  Data_Hungary$Region_Veszprem == 1 ~ "Veszprem",
  Data_Hungary$Region_Zala == 1 ~ "Zala",
  Data_Hungary$Region_OutsideHungary == 1 ~ "Outside Hungary",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Hungary$Region), "./results/Hungary_Region_counts.xlsx")


## Indonesia ####

Data_Indonesia$AgeGenderGroup <- case_when(
  Data_Indonesia$Males_18to24 == 1 ~ "Men, 18-24",
  Data_Indonesia$Males_25to34 == 1 ~ "Men, 25-34",
  Data_Indonesia$Males_35to44 == 1 ~ "Men, 35-44",
  Data_Indonesia$Males_45to54 == 1 ~ "Men, 45-54",
  Data_Indonesia$Males_55to64 == 1 ~ "Men, 55-64",
  Data_Indonesia$Males_65plus == 1 ~ "Men, 65+",
  Data_Indonesia$Females_18to24 == 1 ~ "Women, 18-24",
  Data_Indonesia$Females_25to34 == 1 ~ "Women, 25-34",
  Data_Indonesia$Females_35to44 == 1 ~ "Women, 35-44",
  Data_Indonesia$Females_45to54 == 1 ~ "Women, 45-54",
  Data_Indonesia$Females_55to64 == 1 ~ "Women, 55-64",
  Data_Indonesia$Females_65plus == 1 ~ "Women, 65+",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Indonesia$AgeGenderGroup), "./results/Indonesia_AgeGender_counts.xlsx")

Data_Indonesia$EducationGroup <- case_when(
  Data_Indonesia$Education_Primary == 1 ~ "Primary",
  Data_Indonesia$Education_Secondary == 1 ~ "Secondary",
  Data_Indonesia$Education_Tertiary == 1 ~ "Tertiary",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Indonesia$EducationGroup), "./results/Indonesia_Education_counts.xlsx")

Data_Indonesia$Region <- case_when(
  Data_Indonesia$Region_Aceh == 1 ~ "Aceh",
  Data_Indonesia$Region_Bali == 1 ~ "Bali",
  Data_Indonesia$Region_BangkaBelitung == 1 ~ "Bangka Belitung",
  Data_Indonesia$Region_Banten == 1 ~ "Banten",
  Data_Indonesia$Region_Bengkulu == 1 ~ "Bengkulu",
  Data_Indonesia$Region_Yogyakarta == 1 ~ "Yobyakarta",
  Data_Indonesia$Region_Jakarta == 1 ~ "Jakarta",
  Data_Indonesia$Region_Gorontalo == 1 ~ "Gorontalo",
  Data_Indonesia$Region_Jambi == 1 ~ "Jambi",
  Data_Indonesia$Region_WestJava == 1 ~ "West Java",
  Data_Indonesia$Region_CentralJava == 1 ~ "Central Java",
  Data_Indonesia$Region_EastJava == 1 ~ "East Java",
  Data_Indonesia$Region_WestKalimantan == 1 ~ "West Kalimantan",
  Data_Indonesia$Region_SouthKalimantan == 1 ~ "South Kalimantan",
  Data_Indonesia$Region_CentralKalimantan == 1 ~ "Central Kalimantan",
  Data_Indonesia$Region_EastKalimantan == 1 ~ "East Kalimantan",
  Data_Indonesia$Region_NorthKalimantan == 1 ~ "North Kalimantan",
  Data_Indonesia$Region_RiauIslands == 1 ~ "Riau Islands",
  Data_Indonesia$Region_Lampung == 1 ~ "Lampung",
  Data_Indonesia$Region_Maluku == 1 ~ "Maluku",
  Data_Indonesia$Region_NorthMaluku == 1 ~ "North Maluku",
  Data_Indonesia$Region_WestNusaTenggara == 1 ~ "West Nusa Tenggara",
  Data_Indonesia$Region_EastNusaTenggara == 1 ~ "East Nusa Tenggara",
  Data_Indonesia$Region_Papua == 1 ~ "Papua",
  Data_Indonesia$Region_WestPapua == 1 ~ "West Papua",
  Data_Indonesia$Region_Riau == 1 ~ "Riau",
  Data_Indonesia$Region_WestSulawesi == 1 ~ "West Sulawesi",
  Data_Indonesia$Region_SouthSulawesi == 1 ~ "South Sulawesi",
  Data_Indonesia$Region_CentralSulawesi == 1 ~ "Central Sulawesi",
  Data_Indonesia$Region_SoutheastSulawesi == 1 ~ "Southeast Sulawesi",
  Data_Indonesia$Region_NorthSulawesi == 1 ~ "North Sulawesi",
  Data_Indonesia$Region_WestSumatra == 1 ~ "West Sumatra",
  Data_Indonesia$Region_SouthSumatra == 1 ~ "South Sumatra",
  Data_Indonesia$Region_NorthSumatra == 1 ~ "North Sumatra",
  Data_Indonesia$Region_OutsideIndonesia == 1 ~ "Outside Indonesia",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Indonesia$Region), "./results/Indonesia_Region_counts.xlsx")


## Kenya ####

Data_Kenya$AgeGenderGroup <- case_when(
  Data_Kenya$Males_18to24 == 1 ~ "Men, 18-24",
  Data_Kenya$Males_25to34 == 1 ~ "Men, 25-34",
  Data_Kenya$Males_35to44 == 1 ~ "Men, 35-44",
  Data_Kenya$Males_45to54 == 1 ~ "Men, 45-54",
  Data_Kenya$Males_55to64 == 1 ~ "Men, 55-64",
  Data_Kenya$Males_65plus == 1 ~ "Men, 65+",
  Data_Kenya$Females_18to24 == 1 ~ "Women, 18-24",
  Data_Kenya$Females_25to34 == 1 ~ "Women, 25-34",
  Data_Kenya$Females_35to44 == 1 ~ "Women, 35-44",
  Data_Kenya$Females_45to54 == 1 ~ "Women, 45-54",
  Data_Kenya$Females_55to64 == 1 ~ "Women, 55-64",
  Data_Kenya$Females_65plus == 1 ~ "Women, 65+",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Kenya$AgeGenderGroup), "./results/Kenya_AgeGender_counts.xlsx")

Data_Kenya$EducationGroup <- case_when(
  Data_Kenya$Education_Primary == 1 ~ "Primary",
  Data_Kenya$Education_Secondary == 1 ~ "Secondary",
  Data_Kenya$Education_Tertiary == 1 ~ "Tertiary",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Kenya$EducationGroup), "./results/Kenya_Education_counts.xlsx")

Data_Kenya$Province<- case_when(
  Data_Kenya$Region_Baringo == 1 ~ "Rift Valley",
  Data_Kenya$Region_Bomet == 1 ~ "Rift Valley",
  Data_Kenya$Region_Bungoma == 1 ~ "Western",
  Data_Kenya$Region_Busia == 1 ~ "Western",
  Data_Kenya$Region_ElgeyoMarakwet == 1 ~ "Rift Valley",
  Data_Kenya$Region_Embu == 1 ~ "Eastern",
  Data_Kenya$Region_Garissa == 1 ~ "North Eastern",
  Data_Kenya$Region_HomaBay == 1 ~ "Nyanza",
  Data_Kenya$Region_Isiolo == 1 ~ "Eastern",
  Data_Kenya$Region_Kajiado == 1 ~ "Rift Valley",
  Data_Kenya$Region_Kakamega == 1 ~ "Western",
  Data_Kenya$Region_Kericho == 1 ~ "Rift Valley",
  Data_Kenya$Region_Kiambu == 1 ~ "Central",
  Data_Kenya$Region_Kilifi == 1 ~ "Coast",
  Data_Kenya$Region_Kirinyaga == 1 ~ "Central",
  Data_Kenya$Region_Kisii == 1 ~ "Nyanza",
  Data_Kenya$Region_Kisumu == 1 ~ "Nyanza",
  Data_Kenya$Region_Kitui == 1 ~ "Eastern",
  Data_Kenya$Region_Kwale == 1 ~ "Coast",
  Data_Kenya$Region_Laikipia == 1 ~ "Rift Valley",
  Data_Kenya$Region_Lamu == 1 ~ "Coast",
  Data_Kenya$Region_Machakos == 1 ~ "Eastern",
  Data_Kenya$Region_Makueni == 1 ~ "Eastern",
  Data_Kenya$Region_Mandera == 1 ~ "North Eastern",
  Data_Kenya$Region_Marsabit == 1 ~ "Eastern",
  Data_Kenya$Region_Meru == 1 ~ "Eastern",
  Data_Kenya$Region_Migori == 1 ~ "Nyanza",
  Data_Kenya$Region_Mombasa == 1 ~ "Coast",
  Data_Kenya$Region_Muranga == 1 ~ "Central",
  Data_Kenya$Region_Nairobi == 1 ~ "Nairobi",
  Data_Kenya$Region_Nakuru == 1 ~ "Rift Valley",
  Data_Kenya$Region_Nandi == 1 ~ "Rift Valley",
  Data_Kenya$Region_Narok == 1 ~ "Rift Valley",
  Data_Kenya$Region_Nyamira == 1 ~ "Nyanza",
  Data_Kenya$Region_Nyandarua == 1 ~ "Central",
  Data_Kenya$Region_Nyeri == 1 ~ "Central",
  Data_Kenya$Region_Samburu == 1 ~ "Rift Valley",
  Data_Kenya$Region_Siaya == 1 ~ "Nyanza",
  Data_Kenya$Region_TaitaTaveta == 1 ~ "Coast",
  Data_Kenya$Region_TanaRiver == 1 ~ "Coast",
  Data_Kenya$Region_TharakaNithi == 1 ~ "Eastern",
  Data_Kenya$Region_TransNzoia == 1 ~ "Rift Valley",
  Data_Kenya$Region_Turkana == 1 ~ "Rift Valley",
  Data_Kenya$Region_UasinGishu == 1 ~ "Rift Valley",
  Data_Kenya$Region_Vihiga == 1 ~ "Western",
  Data_Kenya$Region_Wajir == 1 ~ "North Eastern",
  Data_Kenya$Region_WestPokot == 1 ~ "Rift Valley",
  Data_Kenya$Region_OutsideKenya == 1 ~ "Outside Kenya",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Kenya$Province), "./results/Kenya_Province_counts.xlsx")


## South Korea ####

Data_SouthKorea$AgeGenderGroup <- case_when(
  Data_SouthKorea$Males_18to24 == 1 ~ "Men, 18-24",
  Data_SouthKorea$Males_25to34 == 1 ~ "Men, 25-34",
  Data_SouthKorea$Males_35to44 == 1 ~ "Men, 35-44",
  Data_SouthKorea$Males_45to54 == 1 ~ "Men, 45-54",
  Data_SouthKorea$Males_55to64 == 1 ~ "Men, 55-64",
  Data_SouthKorea$Males_65plus == 1 ~ "Men, 65+",
  Data_SouthKorea$Females_18to24 == 1 ~ "Women, 18-24",
  Data_SouthKorea$Females_25to34 == 1 ~ "Women, 25-34",
  Data_SouthKorea$Females_35to44 == 1 ~ "Women, 35-44",
  Data_SouthKorea$Females_45to54 == 1 ~ "Women, 45-54",
  Data_SouthKorea$Females_55to64 == 1 ~ "Women, 55-64",
  Data_SouthKorea$Females_65plus == 1 ~ "Women, 65+",
  TRUE ~ "Unknown")

write.xlsx(table(Data_SouthKorea$AgeGenderGroup), "./results/SouthKorea_AgeGender_counts.xlsx")

Data_SouthKorea$EducationGroup <- case_when(
  Data_SouthKorea$Education_Primary == 1 ~ "Primary",
  Data_SouthKorea$Education_Secondary == 1 ~ "Secondary",
  Data_SouthKorea$Education_Tertiary == 1 ~ "Tertiary",
  TRUE ~ "Unknown")

write.xlsx(table(Data_SouthKorea$EducationGroup), "./results/SouthKorea_Education_counts.xlsx")

Data_SouthKorea$Region <- case_when(
  Data_SouthKorea$Region_Seoul == 1 ~ "Seoul",
  Data_SouthKorea$Region_Busan == 1 ~ "Busan",
  Data_SouthKorea$Region_Daegu == 1 ~ "Daegu",
  Data_SouthKorea$Region_Incheon == 1 ~ "Incheon",
  Data_SouthKorea$Region_Gwangju == 1 ~ "Gwangju",
  Data_SouthKorea$Region_Daejeon == 1 ~ "Daejeon",
  Data_SouthKorea$Region_Ulsan == 1 ~ "Ulsan",
  Data_SouthKorea$Region_Sejong == 1 ~ "Sejong",
  Data_SouthKorea$Region_Gyeonggi == 1 ~ "Gyeonggi",
  Data_SouthKorea$Region_Gangwon == 1 ~ "Gangwon",
  Data_SouthKorea$Region_NorthChungcheong == 1 ~ "North Chungcheong",
  Data_SouthKorea$Region_SouthChungcheong == 1 ~ "South Chungcheong",
  Data_SouthKorea$Region_NorthJeolla == 1 ~ "North Jeolla",
  Data_SouthKorea$Region_SouthJeolla == 1 ~ "South Jeolla",
  Data_SouthKorea$Region_NorthGyeongsang == 1 ~ "North Gyeongsang",
  Data_SouthKorea$Region_SouthGyeongsang == 1 ~ "South Gyeongsang",
  Data_SouthKorea$Region_Jeju == 1 ~ "Jeju",
  Data_SouthKorea$Region_OutsideSouthKorea == 1 ~ "Outside SouthKorea",
  TRUE ~ "Unknown")

write.xlsx(table(Data_SouthKorea$Region), "./results/SouthKorea_Region_counts.xlsx")


## Turkey ####

Data_Turkey$AgeGenderGroup <- case_when(
  Data_Turkey$Males_18to24 == 1 ~ "Men, 18-24",
  Data_Turkey$Males_25to34 == 1 ~ "Men, 25-34",
  Data_Turkey$Males_35to44 == 1 ~ "Men, 35-44",
  Data_Turkey$Males_45to54 == 1 ~ "Men, 45-54",
  Data_Turkey$Males_55to64 == 1 ~ "Men, 55-64",
  Data_Turkey$Males_65plus == 1 ~ "Men, 65+",
  Data_Turkey$Females_18to24 == 1 ~ "Women, 18-24",
  Data_Turkey$Females_25to34 == 1 ~ "Women, 25-34",
  Data_Turkey$Females_35to44 == 1 ~ "Women, 35-44",
  Data_Turkey$Females_45to54 == 1 ~ "Women, 45-54",
  Data_Turkey$Females_55to64 == 1 ~ "Women, 55-64",
  Data_Turkey$Females_65plus == 1 ~ "Women, 65+",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Turkey$AgeGenderGroup), "./results/Turkey_AgeGender_counts.xlsx")

Data_Turkey$EducationGroup <- case_when(
  Data_Turkey$Education_Primary == 1 ~ "Primary",
  Data_Turkey$Education_Secondary == 1 ~ "Secondary",
  Data_Turkey$Education_Tertiary == 1 ~ "Tertiary",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Turkey$EducationGroup), "./results/Turkey_Education_counts.xlsx")

Data_Turkey$Province <- case_when(
  Data_Turkey$Region_Adana == 1 ~ "Mediterranean",
  Data_Turkey$Region_Adiyaman == 1 ~ "Southeast Anatolia",
  Data_Turkey$Region_Afyonkarahisar == 1 ~ "Aegean",
  Data_Turkey$Region_Agri == 1 ~ "Northeast Anatolia",
  Data_Turkey$Region_Aksaray == 1 ~ "Central Anatolia",
  Data_Turkey$Region_Amasya == 1 ~ "West Black Sea",
  Data_Turkey$Region_Ankara == 1 ~ "West Anatolia",
  Data_Turkey$Region_Antalya == 1 ~ "Mediterranean",
  Data_Turkey$Region_Ardahan == 1 ~ "Northeast Anatolia",
  Data_Turkey$Region_Artvin == 1 ~ "East Black Sea",
  Data_Turkey$Region_Aydin == 1 ~ "Aegean",
  Data_Turkey$Region_Balikesir == 1 ~ "West Marmara",
  Data_Turkey$Region_Bartin == 1 ~ "West Black Sea",
  Data_Turkey$Region_Batman == 1 ~ "Southeast Anatolia",
  Data_Turkey$Region_Bayburt == 1 ~ "Northeast Anatolia",
  Data_Turkey$Region_Bilecik == 1 ~ "East Marmara",
  Data_Turkey$Region_Bingol == 1 ~ "Central East Anatolia",
  Data_Turkey$Region_Bitlis == 1 ~ "Central East Anatolia",
  Data_Turkey$Region_Bolu == 1 ~ "East Marmara",
  Data_Turkey$Region_Burdur == 1 ~ "Mediterranean",
  Data_Turkey$Region_Bursa == 1 ~ "East Marmara",
  Data_Turkey$Region_Canakkale == 1 ~ "West Marmara",
  Data_Turkey$Region_Cankiri == 1 ~ "West Black Sea",
  Data_Turkey$Region_Corum == 1 ~ "West Black Sea",
  Data_Turkey$Region_Denizli == 1 ~ "Aegean",
  Data_Turkey$Region_Diyarbakir == 1 ~ "Southeast Anatolia",
  Data_Turkey$Region_Duzce == 1 ~ "East Marmara",
  Data_Turkey$Region_Edirne == 1 ~ "West Marmara",
  Data_Turkey$Region_Elazig == 1 ~ "Central East Anatolia",
  Data_Turkey$Region_Erzincan == 1 ~ "Northeast Anatolia",
  Data_Turkey$Region_Erzurum == 1 ~ "Northeast Anatolia",
  Data_Turkey$Region_Eskisehir == 1 ~ "East Marmara",
  Data_Turkey$Region_Gaziantep == 1 ~ "Southeast Anatolia",
  Data_Turkey$Region_Giresun == 1 ~ "East Black Sea",
  Data_Turkey$Region_Gumushane == 1 ~ "East Black Sea",
  Data_Turkey$Region_Hakkari == 1 ~ "Central East Anatolia",
  Data_Turkey$Region_Hatay == 1 ~ "Mediterranean",
  Data_Turkey$Region_Igdir == 1 ~ "Northeast Anatolia",
  Data_Turkey$Region_Isparta == 1 ~ "Mediterranean",
  Data_Turkey$Region_Istanbul == 1 ~ "Istanbul",
  Data_Turkey$Region_Izmir == 1 ~ "Aegean",
  Data_Turkey$Region_Kahramanmaras == 1 ~ "Mediterranean",
  Data_Turkey$Region_Karabuk == 1 ~ "West Black Sea",
  Data_Turkey$Region_Karaman == 1 ~ "West Anatolia",
  Data_Turkey$Region_Kars == 1 ~ "Northeast Anatolia",
  Data_Turkey$Region_Kastamonu == 1 ~ "West Black Sea",
  Data_Turkey$Region_Kayseri == 1 ~ "Central Anatolia",
  Data_Turkey$Region_Kilis == 1 ~ "Southeast Anatolia",
  Data_Turkey$Region_Kirikkale == 1 ~ "Central Anatolia",
  Data_Turkey$Region_Kirklareli == 1 ~ "West Marmara",
  Data_Turkey$Region_Kirsehir == 1 ~ "Central Anatolia",
  Data_Turkey$Region_Kocaeli == 1 ~ "East Marmara",
  Data_Turkey$Region_Konya == 1 ~ "West Anatolia",
  Data_Turkey$Region_Kutahya == 1 ~ "Aegean",
  Data_Turkey$Region_Malatya == 1 ~ "Central East Anatolia",
  Data_Turkey$Region_Manisa == 1 ~ "Aegean",
  Data_Turkey$Region_Mardin == 1 ~ "Southeast Anatolia",
  Data_Turkey$Region_Mersin == 1 ~ "Mediterranean",
  Data_Turkey$Region_Mugla  == 1 ~ "Aegean",
  Data_Turkey$Region_Mus == 1 ~ "Central East Anatolia",
  Data_Turkey$Region_Nevsehir == 1 ~ "Central Anatolia",
  Data_Turkey$Region_Nigde == 1 ~ "Central Anatolia",
  Data_Turkey$Region_Ordu == 1 ~ "East Black Sea",
  Data_Turkey$Region_Osmaniye == 1 ~ "Mediterranean",
  Data_Turkey$Region_Rize == 1 ~ "East Black Sea",
  Data_Turkey$Region_Sakarya == 1 ~ "East Marmara",
  Data_Turkey$Region_Samsun == 1 ~ "West Black Sea",
  Data_Turkey$Region_Sanliurfa == 1 ~ "Southeast Anatolia",
  Data_Turkey$Region_Siirt == 1 ~ "Southeast Anatolia",
  Data_Turkey$Region_Sinop == 1 ~ "West Black Sea",
  Data_Turkey$Region_Sirnak == 1 ~ "Southeast Anatolia",
  Data_Turkey$Region_Sivas == 1 ~ "Central Anatolia",
  Data_Turkey$Region_Tekirdag == 1 ~ "West Marmara",
  Data_Turkey$Region_Tokat == 1 ~ "West Black Sea",
  Data_Turkey$Region_Trabzon == 1 ~ "East Black Sea",
  Data_Turkey$Region_Tunceli == 1 ~ "Central East Anatolia",
  Data_Turkey$Region_Usak == 1 ~ "Aegean",
  Data_Turkey$Region_Van == 1 ~ "Central East Anatolia",
  Data_Turkey$Region_Yalova == 1 ~ "East Marmara",
  Data_Turkey$Region_Yozgat == 1 ~ "Central Anatolia",
  Data_Turkey$Region_Zonguldak == 1 ~ "West Black Sea",
  Data_Turkey$Region_OutsideTurkey == 1 ~ "Outside Turkey",
  TRUE ~ "Unknown")

write.xlsx(table(Data_Turkey$Province), "./results/Turkey_Province_counts.xlsx")
