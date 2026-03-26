#Title: Get and Clean Data - APPAM SBIR Project####
#Author: Grant A. Allard
#Purpose: Get and clean SBIR/STTR data for analysis. Created joined data sets including integrating data collected by Web Crawler. 


#Set Up####
#Get Session Info (useful for troubleshooting later)
sessionInfo()

#Libraries.  Need to check these to see if I actually use them in this document. #### 
library(httr)
library(jsonlite)
library(dplyr) 
library(readxl)
library(tidyr)
library(ggplot2)
library(stringr)
library(rebus)
library(lubridate)
require(zipcode)
data("zipcode")
library(imputeTS)



#Install Commmands (if needed, delete the '#' then run the command. Be sure to add the '#' back before saving or closing file)
#install.packages("httr", dependencies = TRUE)
#install.packages("jsonlite", dependencies = TRUE)
#install.packages("RCurl", dependencies = TRUE)
#install.packages("XML", dependencies = TRUE)
#install.packages("dplyr", dependencies = TRUE)
#install.packages("readxl", dependencies = TRUE)
#install.packages("tidyr", dependencies = TRUE)
#install.packages("reshape", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("sjPlot", dependencies = TRUE)
#install.packages("lubridate", dependencies = TRUE)
#install.packages("purrr", dependencies = TRUE)
#install.packages("microbenchmark", dependencies = TRUE)
#install.packages("zipcode", dependencies = TRUE)
#install.packages("ggmap", dependencies = TRUE)
#install.packages("USAboundaries", dependencies = TRUE)
#install.packages("naniar", dependencies = TRUE)
#install.packages("mtsdi", dependencies = TRUE)


#Get and Clean Data####
#There are several types of information we need: 1) Congressional Set-Aside 2) Awards 3) Solicitations 3) Budget Data. This step will provide code to import to R, wrangle, and (where necessary) combine datasets. 

#1) Get congressional set-aside data####
Set_Aside_df<- read_excel("Congress.xlsx")
str(Set_Aside_df)

#Change ColNames
colnames(Set_Aside_df)<-c("year", "set_asidepercent")
str(Set_Aside_df)

#Calculate a YoY Change
Set_Aside_df<- Set_Aside_df %>% 
  mutate(SetAsideYoY= `set_asidepercent` - lag(`set_asidepercent`, default=first(`set_asidepercent`)))
str(Set_Aside_df)

#Code Factor Variable to Indicate YoY Change
Set_Aside_df$SetAsideChange<- ifelse (Set_Aside_df$SetAsideYoY>0 ,1,0)
str(Set_Aside_df)

#2) Awards Data####
#Awards data is organized by agency thus I need to get the data for each of the 11 agencies, 

#Use  a httr/jsonlite approach for getting awards data. First build the base and endpoint of the query.
Awards_base<-("https://www.sbir.gov/api/awards.json?")
Awards_endpoint<-"agency"

#2) Next get the awards data for each agency. 
#DOD Awards Data - Not working
#Awards_DOD<-"DOD"
#Awards_DOD_Call<-paste(Awards_base,Awards_endpoint,"=",Awards_DOD,sep="")
#Get_Awards_DOD<-GET(Awards_DOD_Call)
#Get_Awards_DOD_Text<-content(Get_Awards_DOD, "text")
#Get_Awards_DOD_JSON<-fromJSON(Get_Awards_DOD_Text, flatten=TRUE)
#Get_Awards_DOD_df<-as.data.frame(Get_Awards_DOD_JSON)
#Awards_DoD_df<-Get_Awards_DOD_df

#HHS Awards Data
Awards_HHS<-"HHS"
Awards_HHS_Call<-paste(Awards_base,Awards_endpoint,"=",Awards_HHS,sep="")
Get_Awards_HHS<-GET(Awards_HHS_Call)
Get_Awards_HHS_Text<-content(Get_Awards_HHS, "text")
Get_Awards_HHS_JSON<-fromJSON(Get_Awards_HHS_Text, flatten=TRUE)
Get_Awards_HHS_df<-as.data.frame(Get_Awards_HHS_JSON)
Awards_HHS_df<-Get_Awards_HHS_df

#NASA Awards Data
Awards_NASA<-"NASA"
Awards_NASA_Call<-paste(Awards_base,Awards_endpoint,"=",Awards_NASA,sep="")
Get_Awards_NASA<-GET(Awards_NASA_Call)
Get_Awards_NASA_Text<-content(Get_Awards_NASA, "text")
Get_Awards_NASA_JSON<-fromJSON(Get_Awards_NASA_Text, flatten=TRUE)
Get_Awards_NASA_df<-as.data.frame(Get_Awards_NASA_JSON)
Awards_NASA_df<-Get_Awards_NASA_df

#NSF Awards Data
Awards_NSF<-"NSF"
Awards_NSF_Call<-paste(Awards_base,Awards_endpoint,"=",Awards_NSF,sep="")
Get_Awards_NSF<-GET(Awards_NSF_Call)
Get_Awards_NSF_Text<-content(Get_Awards_NSF, "text")
Get_Awards_NSF_JSON<-fromJSON(Get_Awards_NSF_Text, flatten=TRUE)
Get_Awards_NSF_df<-as.data.frame(Get_Awards_NSF_JSON)
Awards_NSF_df<-Get_Awards_NSF_df

#DOE Awards Data
Awards_DOE<-"DOE"
Awards_DOE_Call<-paste(Awards_base,Awards_endpoint,"=",Awards_DOE,sep="")
Get_Awards_DOE<-GET(Awards_DOE_Call)
Get_Awards_DOE_Text<-content(Get_Awards_DOE, "text")
Get_Awards_DOE_JSON<-fromJSON(Get_Awards_DOE_Text, flatten=TRUE)
Get_Awards_DOE_df<-as.data.frame(Get_Awards_DOE_JSON)
Awards_DOE_df<-Get_Awards_DOE_df

#USDA Awards Data
Awards_USDA<-"USDA"
Awards_USDA_Call<-paste(Awards_base,Awards_endpoint,"=",Awards_USDA,sep="")
Get_Awards_USDA<-GET(Awards_USDA_Call)
Get_Awards_USDA_Text<-content(Get_Awards_USDA, "text")
Get_Awards_USDA_JSON<-fromJSON(Get_Awards_USDA_Text, flatten=TRUE)
Get_Awards_USDA_df<-as.data.frame(Get_Awards_USDA_JSON)
Awards_USDA_df<-Get_Awards_USDA_df

#USDA Awards Data
Awards_USDA<-"USDA"
Awards_USDA_Call<-paste(Awards_base,Awards_endpoint,"=",Awards_USDA,sep="")
Get_Awards_USDA<-GET(Awards_USDA_Call)
Get_Awards_USDA_Text<-content(Get_Awards_USDA, "text")
Get_Awards_USDA_JSON<-fromJSON(Get_Awards_USDA_Text, flatten=TRUE)
Get_Awards_USDA_df<-as.data.frame(Get_Awards_USDA_JSON)
Awards_USDA_df<-Get_Awards_USDA_df

#EPA Awards Data
Awards_EPA<-"EPA"
Awards_EPA_Call<-paste(Awards_base,Awards_endpoint,"=",Awards_EPA,sep="")
Get_Awards_EPA<-GET(Awards_EPA_Call)
Get_Awards_EPA_Text<-content(Get_Awards_EPA, "text")
Get_Awards_EPA_JSON<-fromJSON(Get_Awards_EPA_Text, flatten=TRUE)
Get_Awards_EPA_df<-as.data.frame(Get_Awards_EPA_JSON)
Awards_EPA_df<-Get_Awards_EPA_df

#DOC Awards Data
Awards_DOC<-"DOC"
Awards_DOC_Call<-paste(Awards_base,Awards_endpoint,"=",Awards_DOC,sep="")
Get_Awards_DOC<-GET(Awards_DOC_Call)
Get_Awards_DOC_Text<-content(Get_Awards_DOC, "text")
Get_Awards_DOC_JSON<-fromJSON(Get_Awards_DOC_Text, flatten=TRUE)
Get_Awards_DOC_df<-as.data.frame(Get_Awards_DOC_JSON)
Awards_DOC_df<-Get_Awards_DOC_df

#ED Awards Data
Awards_ED<-"ED"
Awards_ED_Call<-paste(Awards_base,Awards_endpoint,"=",Awards_ED,sep="")
Get_Awards_ED<-GET(Awards_ED_Call)
Get_Awards_ED_Text<-content(Get_Awards_ED, "text")
Get_Awards_ED_JSON<-fromJSON(Get_Awards_ED_Text, flatten=TRUE)
Get_Awards_ED_df<-as.data.frame(Get_Awards_ED_JSON)
Awards_ED_df<-Get_Awards_ED_df

#DOT Awards Data
Awards_DOT<-"DOT"
Awards_DOT_Call<-paste(Awards_base,Awards_endpoint,"=",Awards_DOT,sep="")
Get_Awards_DOT<-GET(Awards_DOT_Call)
Get_Awards_DOT_Text<-content(Get_Awards_DOT, "text")
Get_Awards_DOT_JSON<-fromJSON(Get_Awards_DOT_Text, flatten=TRUE)
Get_Awards_DOT_df<-as.data.frame(Get_Awards_DOT_JSON)
Awards_DOT_df<-Get_Awards_DOT_df


#DHS Awards Data
Awards_DHS<-"DHS"
Awards_DHS_Call<-paste(Awards_base,Awards_endpoint,"=",Awards_DHS,sep="")
Get_Awards_DHS<-GET(Awards_DHS_Call)
Get_Awards_DHS_Text<-content(Get_Awards_DHS, "text")
Get_Awards_DHS_JSON<-fromJSON(Get_Awards_DHS_Text, flatten=TRUE)
Get_Awards_DHS_df<-as.data.frame(Get_Awards_DHS_JSON)
Awards_DHS_df<-Get_Awards_DHS_df

#3) Solicitation Data####
#Repeat the above steps for solicitations. 

#Use  a httr/jsonlite approach for getting awards data. First build the base and endpoint of the query.
Solicit_base<-("https://www.sbir.gov/api/solicitations.json?")
Solicit_endpoint<-"agency"

#DOD Solicit Data - Not working
Solicit_DOD<-"DOD"
Solicit_DOD_Call<-paste(Solicit_base,Solicit_endpoint,"=",Solicit_DOD,sep="")
Get_Solicit_DOD<-GET(Solicit_DOD_Call)
Get_Solicit_DOD_Text<-content(Get_Solicit_DOD, "text")
Get_Solicit_DOD_JSON<-fromJSON(Get_Solicit_DOD_Text, flatten=TRUE)
Get_Solicit_DOD_df<-as.data.frame(Get_Solicit_DOD_JSON)
Solicit_DoD_df<-Get_Solicit_DOD_df

#Rename 'agency' column
Solicit_DoD_df$agency<-"DOD"

#HHS Solicit Data
Solicit_HHS<-"HHS"
Solicit_HHS_Call<-paste(Solicit_base,Solicit_endpoint,"=",Solicit_HHS,sep="")
Get_Solicit_HHS<-GET(Solicit_HHS_Call)
Get_Solicit_HHS_Text<-content(Get_Solicit_HHS, "text")
Get_Solicit_HHS_JSON<-fromJSON(Get_Solicit_HHS_Text, flatten=TRUE)
Get_Solicit_HHS_df<-as.data.frame(Get_Solicit_HHS_JSON)
Solicit_HHS_df<-Get_Solicit_HHS_df

#Rename 'agency' column
Solicit_HHS_df$agency<-"HHS"

#NASA Solicit Data
Solicit_NASA<-"NASA"
Solicit_NASA_Call<-paste(Solicit_base,Solicit_endpoint,"=",Solicit_NASA,sep="")
Get_Solicit_NASA<-GET(Solicit_NASA_Call)
Get_Solicit_NASA_Text<-content(Get_Solicit_NASA, "text")
Get_Solicit_NASA_JSON<-fromJSON(Get_Solicit_NASA_Text, flatten=TRUE)
Get_Solicit_NASA_df<-as.data.frame(Get_Solicit_NASA_JSON)
Solicit_NASA_df<-Get_Solicit_NASA_df

#Rename 'agency' column
Solicit_NASA_df$agency<-"NASA"

#NSF Solicit Data
Solicit_NSF<-"NSF"
Solicit_NSF_Call<-paste(Solicit_base,Solicit_endpoint,"=",Solicit_NSF,sep="")
Get_Solicit_NSF<-GET(Solicit_NSF_Call)
Get_Solicit_NSF_Text<-content(Get_Solicit_NSF, "text")
Get_Solicit_NSF_JSON<-fromJSON(Get_Solicit_NSF_Text, flatten=TRUE)
Get_Solicit_NSF_df<-as.data.frame(Get_Solicit_NSF_JSON)
Solicit_NSF_df<-Get_Solicit_NSF_df

#Rename 'agency' column
Solicit_NSF_df$agency<-"NSF"

#DOE Solicit Data
Solicit_DOE<-"DOE"
Solicit_DOE_Call<-paste(Solicit_base,Solicit_endpoint,"=",Solicit_DOE,sep="")
Get_Solicit_DOE<-GET(Solicit_DOE_Call)
Get_Solicit_DOE_Text<-content(Get_Solicit_DOE, "text")
Get_Solicit_DOE_JSON<-fromJSON(Get_Solicit_DOE_Text, flatten=TRUE)
Get_Solicit_DOE_df<-as.data.frame(Get_Solicit_DOE_JSON)
Solicit_DOE_df<-Get_Solicit_DOE_df

#Rename 'agency' column
Solicit_DOE_df$agency<-"DOE"


#USDA Solicit Data
Solicit_USDA<-"USDA"
Solicit_USDA_Call<-paste(Solicit_base,Solicit_endpoint,"=",Solicit_USDA,sep="")
Get_Solicit_USDA<-GET(Solicit_USDA_Call)
Get_Solicit_USDA_Text<-content(Get_Solicit_USDA, "text")
Get_Solicit_USDA_JSON<-fromJSON(Get_Solicit_USDA_Text, flatten=TRUE)
Get_Solicit_USDA_df<-as.data.frame(Get_Solicit_USDA_JSON)
Solicit_USDA_df<-Get_Solicit_USDA_df

#Rename 'agency' column
Solicit_USDA_df$agency<-"USDA"


#EPA Solicit Data
Solicit_EPA<-"EPA"
Solicit_EPA_Call<-paste(Solicit_base,Solicit_endpoint,"=",Solicit_EPA,sep="")
Get_Solicit_EPA<-GET(Solicit_EPA_Call)
Get_Solicit_EPA_Text<-content(Get_Solicit_EPA, "text")
Get_Solicit_EPA_JSON<-fromJSON(Get_Solicit_EPA_Text, flatten=TRUE)
Get_Solicit_EPA_df<-as.data.frame(Get_Solicit_EPA_JSON)
Solicit_EPA_df<-Get_Solicit_EPA_df

#Rename 'agency' column
Solicit_EPA_df$agency<-"EPA"

#DOC Solicit Data
Solicit_DOC<-"DOC"
Solicit_DOC_Call<-paste(Solicit_base,Solicit_endpoint,"=",Solicit_DOC,sep="")
Get_Solicit_DOC<-GET(Solicit_DOC_Call)
Get_Solicit_DOC_Text<-content(Get_Solicit_DOC, "text")
Get_Solicit_DOC_JSON<-fromJSON(Get_Solicit_DOC_Text, flatten=TRUE)
Get_Solicit_DOC_df<-as.data.frame(Get_Solicit_DOC_JSON)
Solicit_DOC_df<-Get_Solicit_DOC_df

#Rename 'agency' column
Solicit_DOC_df$agency<-"DOC"

#ED Solicit Data
Solicit_ED<-"ED"
Solicit_ED_Call<-paste(Solicit_base,Solicit_endpoint,"=",Solicit_ED,sep="")
Get_Solicit_ED<-GET(Solicit_ED_Call)
Get_Solicit_ED_Text<-content(Get_Solicit_ED, "text")
Get_Solicit_ED_JSON<-fromJSON(Get_Solicit_ED_Text, flatten=TRUE)
Get_Solicit_ED_df<-as.data.frame(Get_Solicit_ED_JSON)
Solicit_ED_df<-Get_Solicit_ED_df

#Rename 'agency' column
Solicit_ED_df$agency<-"ED"

#DOT Solicit Data
Solicit_DOT<-"DOT"
Solicit_DOT_Call<-paste(Solicit_base,Solicit_endpoint,"=",Solicit_DOT,sep="")
Get_Solicit_DOT<-GET(Solicit_DOT_Call)
Get_Solicit_DOT_Text<-content(Get_Solicit_DOT, "text")
Get_Solicit_DOT_JSON<-fromJSON(Get_Solicit_DOT_Text, flatten=TRUE)
Get_Solicit_DOT_df<-as.data.frame(Get_Solicit_DOT_JSON)
Solicit_DOT_df<-Get_Solicit_DOT_df

#Rename 'agency' column
Solicit_DOT_df$agency<-"DOT"


#DHS Solicit Data
Solicit_DHS<-"DHS"
Solicit_DHS_Call<-paste(Solicit_base,Solicit_endpoint,"=",Solicit_DHS,sep="")
Get_Solicit_DHS<-GET(Solicit_DHS_Call)
Get_Solicit_DHS_Text<-content(Get_Solicit_DHS, "text")
Get_Solicit_DHS_JSON<-fromJSON(Get_Solicit_DHS_Text, flatten=TRUE)
Get_Solicit_DHS_df<-as.data.frame(Get_Solicit_DHS_JSON)
Solicit_DHS_df<-Get_Solicit_DHS_df

#Rename 'agency' column
Solicit_DHS_df$agency<-"DHS"


#4) Get Budget Data####
#Budget data is accessible on SBIR Dashboard 

#Figure out query language of SBIR
Budget_Base<-("https://www.sbir.gov/awards/annual-reports/xls?program=SBIR&abbr%5B%5D=")
Budget_Suffix<-("&view_by=Year&xls_table=SBIR_trends&dataid=SbirAnnualReportsSummarySqlYearSbir")

#I will remove commas from 'extramuralbudget', 'obligated_dollars', 'programbudget', and 'deficit dollars'

#Write Replace Commas function
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

#Budget: DOD
Agency_DOD<-"DOD"
Budget_DOD_Call<-paste(Budget_Base,Agency_DOD, Budget_Suffix,sep="")
Budget_DOD_Raw<-"Budget_DOD_Raw.xlsx"
download.file(Budget_DOD_Call,Budget_DOD_Raw )
Budget_DOD_data<-read_excel(Budget_DOD_Raw)

#Wrangle 'Budget_DOD_data'
Budget_DOD_df<-Budget_DOD_data

#Add Column Names
Budget_col_names<-c("year", "extramuralbudget", "obligated_dollars", "programbudget","deficits_dollars","extramural_perc","num_proposal","num_awards")
Budget_DOD_df<-Budget_DOD_df[-1,]
colnames(Budget_DOD_df)<-Budget_col_names

#Create Agency Column
Budget_DOD_df$agency<-"DOD"

#Convert Variables to Numeric
Budget_DOD_df$year<-as.numeric(Budget_DOD_df$year)
Budget_DOD_df$num_awards<-as.numeric(Budget_DOD_df$num_awards)
Budget_DOD_df$num_proposal<-as.numeric(Budget_DOD_df$num_proposal)

#Now replace commas, then convert to numeric: 'extramuralbudget'
Budget_DOD_df$extramuralbudget<-replaceCommas(Budget_DOD_df$extramuralbudget)
Budget_DOD_df$extramuralbudget<-as.numeric(Budget_DOD_df$extramuralbudget, length = 1)

#Now replace commas, then convert to numeric: 'obligated_dollars'
Budget_DOD_df$obligated_dollars<-replaceCommas(Budget_DOD_df$obligated_dollars)
Budget_DOD_df$obligated_dollars<-as.numeric(Budget_DOD_df$obligated_dollars)

#Then 'program budget' and 'deficit dollars'
Budget_DOD_df$programbudget<-replaceCommas(Budget_DOD_df$programbudget)
Budget_DOD_df$programbudget<-as.numeric(Budget_DOD_df$programbudget)

Budget_DOD_df$deficits_dollars<-replaceCommas(Budget_DOD_df$deficits_dollars)
Budget_DOD_df$deficits_dollars<-as.numeric(Budget_DOD_df$deficits_dollars)

#Budget: HHS
Agency_HHS<-"HHS"
Budget_HHS_Call<-paste(Budget_Base,Agency_HHS, Budget_Suffix,sep="")
Budget_HHS_Raw<-"Budget_HHS_Raw.xlsx"
download.file(Budget_HHS_Call,Budget_HHS_Raw )
Budget_HHS_data<-read_excel(Budget_HHS_Raw)

#Wrangle 'Budget_HHS_data'
Budget_HHS_df<-Budget_HHS_data

#Add Column Names
Budget_col_names<-c("year", "extramuralbudget", "obligated_dollars", "programbudget","deficits_dollars","extramural_perc","num_proposal","num_awards")
Budget_HHS_df<-Budget_HHS_df[-1,]
colnames(Budget_HHS_df)<-Budget_col_names

#Create Agency Column
Budget_HHS_df$agency<-"HHS"

#Convert Variables to Numeric
Budget_HHS_df$year<-as.numeric(Budget_HHS_df$year)
Budget_HHS_df$num_awards<-as.numeric(Budget_HHS_df$num_awards)
Budget_HHS_df$num_proposal<-as.numeric(Budget_HHS_df$num_proposal)

#Now replace commas, then convert to numeric: 'extramuralbudget'
Budget_HHS_df$extramuralbudget<-replaceCommas(Budget_HHS_df$extramuralbudget)
Budget_HHS_df$extramuralbudget<-as.numeric(Budget_HHS_df$extramuralbudget, length = 1)

#Now replace commas, then convert to numeric: 'obligated_dollars'
Budget_HHS_df$obligated_dollars<-replaceCommas(Budget_HHS_df$obligated_dollars)
Budget_HHS_df$obligated_dollars<-as.numeric(Budget_HHS_df$obligated_dollars)

#Then 'program budget' and 'deficit dollars'
Budget_HHS_df$programbudget<-replaceCommas(Budget_HHS_df$programbudget)
Budget_HHS_df$programbudget<-as.numeric(Budget_HHS_df$programbudget)

Budget_HHS_df$deficits_dollars<-replaceCommas(Budget_HHS_df$deficits_dollars)
Budget_HHS_df$deficits_dollars<-as.numeric(Budget_HHS_df$deficits_dollars)

#Budget: NASA
Agency_NASA<-"NASA"
Budget_NASA_Call<-paste(Budget_Base,Agency_NASA, Budget_Suffix,sep="")
Budget_NASA_Raw<-"Budget_NASA_Raw.xlsx"
download.file(Budget_NASA_Call,Budget_NASA_Raw )
Budget_NASA_data<-read_excel(Budget_NASA_Raw)

#Wrangle 'Budget_NASA_data'
Budget_NASA_df<-Budget_NASA_data

#Add Column Names
Budget_col_names<-c("year", "extramuralbudget", "obligated_dollars", "programbudget","deficits_dollars","extramural_perc","num_proposal","num_awards")
Budget_NASA_df<-Budget_NASA_df[-1,]
colnames(Budget_NASA_df)<-Budget_col_names

#Create Agency Column
Budget_NASA_df$agency<-"NASA"

#Convert Variables to Numeric
Budget_NASA_df$year<-as.numeric(Budget_NASA_df$year)
Budget_NASA_df$num_awards<-as.numeric(Budget_NASA_df$num_awards)
Budget_NASA_df$num_proposal<-as.numeric(Budget_NASA_df$num_proposal)

#Now replace commas, then convert to numeric: 'extramuralbudget'
Budget_NASA_df$extramuralbudget<-replaceCommas(Budget_NASA_df$extramuralbudget)
Budget_NASA_df$extramuralbudget<-as.numeric(Budget_NASA_df$extramuralbudget, length = 1)

#Now replace commas, then convert to numeric: 'obligated_dollars'
Budget_NASA_df$obligated_dollars<-replaceCommas(Budget_NASA_df$obligated_dollars)
Budget_NASA_df$obligated_dollars<-as.numeric(Budget_NASA_df$obligated_dollars)

#Then 'program budget' and 'deficit dollars'
Budget_NASA_df$programbudget<-replaceCommas(Budget_NASA_df$programbudget)
Budget_NASA_df$programbudget<-as.numeric(Budget_NASA_df$programbudget)

Budget_NASA_df$deficits_dollars<-replaceCommas(Budget_NASA_df$deficits_dollars)
Budget_NASA_df$deficits_dollars<-as.numeric(Budget_NASA_df$deficits_dollars)

#Budget: NSF
Agency_NSF<-"NSF"
Budget_NSF_Call<-paste(Budget_Base,Agency_NSF, Budget_Suffix,sep="")
Budget_NSF_Raw<-"Budget_NSF_Raw.xlsx"
download.file(Budget_NSF_Call,Budget_NSF_Raw )
Budget_NSF_data<-read_excel(Budget_NSF_Raw)

#Wrangle 'Budget_NSF_data'
Budget_NSF_df<-Budget_NSF_data

#Add Column Names
Budget_col_names<-c("year", "extramuralbudget", "obligated_dollars", "programbudget","deficits_dollars","extramural_perc","num_proposal","num_awards")
Budget_NSF_df<-Budget_NSF_df[-1,]
colnames(Budget_NSF_df)<-Budget_col_names

#Create Agency Column
Budget_NSF_df$agency<-"NSF"

#Convert Variables to Numeric
Budget_NSF_df$year<-as.numeric(Budget_NSF_df$year)
Budget_NSF_df$num_awards<-as.numeric(Budget_NSF_df$num_awards)
Budget_NSF_df$num_proposal<-as.numeric(Budget_NSF_df$num_proposal)

#Now replace commas, then convert to numeric: 'extramuralbudget'
Budget_NSF_df$extramuralbudget<-replaceCommas(Budget_NSF_df$extramuralbudget)
Budget_NSF_df$extramuralbudget<-as.numeric(Budget_NSF_df$extramuralbudget, length = 1)

#Now replace commas, then convert to numeric: 'obligated_dollars'
Budget_NSF_df$obligated_dollars<-replaceCommas(Budget_NSF_df$obligated_dollars)
Budget_NSF_df$obligated_dollars<-as.numeric(Budget_NSF_df$obligated_dollars)

#Then 'program budget' and 'deficit dollars'
Budget_NSF_df$programbudget<-replaceCommas(Budget_NSF_df$programbudget)
Budget_NSF_df$programbudget<-as.numeric(Budget_NSF_df$programbudget)

Budget_NSF_df$deficits_dollars<-replaceCommas(Budget_NSF_df$deficits_dollars)
Budget_NSF_df$deficits_dollars<-as.numeric(Budget_NSF_df$deficits_dollars)

#Budget: DOE
Agency_DOE<-"DOE"
Budget_DOE_Call<-paste(Budget_Base,Agency_DOE, Budget_Suffix,sep="")
Budget_DOE_Raw<-"Budget_DOE_Raw.xlsx"
download.file(Budget_DOE_Call,Budget_DOE_Raw )
Budget_DOE_data<-read_excel(Budget_DOE_Raw)

#Wrangle 'Budget_DOE_data'
Budget_DOE_df<-Budget_DOE_data

#Add Column Names
Budget_col_names<-c("year", "extramuralbudget", "obligated_dollars", "programbudget","deficits_dollars","extramural_perc","num_proposal","num_awards")
Budget_DOE_df<-Budget_DOE_df[-1,]
colnames(Budget_DOE_df)<-Budget_col_names

#Create Agency Column
Budget_DOE_df$agency<-"DOE"

#Convert Variables to Numeric
Budget_DOE_df$year<-as.numeric(Budget_DOE_df$year)
Budget_DOE_df$num_awards<-as.numeric(Budget_DOE_df$num_awards)
Budget_DOE_df$num_proposal<-as.numeric(Budget_DOE_df$num_proposal)

#Now replace commas, then convert to numeric: 'extramuralbudget'
Budget_DOE_df$extramuralbudget<-replaceCommas(Budget_DOE_df$extramuralbudget)
Budget_DOE_df$extramuralbudget<-as.numeric(Budget_DOE_df$extramuralbudget, length = 1)

#Now replace commas, then convert to numeric: 'obligated_dollars'
Budget_DOE_df$obligated_dollars<-replaceCommas(Budget_DOE_df$obligated_dollars)
Budget_DOE_df$obligated_dollars<-as.numeric(Budget_DOE_df$obligated_dollars)

#Then 'program budget' and 'deficit dollars'
Budget_DOE_df$programbudget<-replaceCommas(Budget_DOE_df$programbudget)
Budget_DOE_df$programbudget<-as.numeric(Budget_DOE_df$programbudget)

Budget_DOE_df$deficits_dollars<-replaceCommas(Budget_DOE_df$deficits_dollars)
Budget_DOE_df$deficits_dollars<-as.numeric(Budget_DOE_df$deficits_dollars)

#Budget: USDA
Agency_USDA<-"USDA"
Budget_USDA_Call<-paste(Budget_Base,Agency_USDA, Budget_Suffix,sep="")
Budget_USDA_Raw<-"Budget_USDA_Raw.xlsx"
download.file(Budget_USDA_Call,Budget_USDA_Raw )
Budget_USDA_data<-read_excel(Budget_USDA_Raw)

#Wrangle 'Budget_USDA_data'
Budget_USDA_df<-Budget_USDA_data

#Add Column Names
Budget_col_names<-c("year", "extramuralbudget", "obligated_dollars", "programbudget","deficits_dollars","extramural_perc","num_proposal","num_awards")
Budget_USDA_df<-Budget_USDA_df[-1,]
colnames(Budget_USDA_df)<-Budget_col_names

#Create Agency Column
Budget_USDA_df$agency<-"USDA"

#Convert Variables to Numeric
Budget_USDA_df$year<-as.numeric(Budget_USDA_df$year)
Budget_USDA_df$num_awards<-as.numeric(Budget_USDA_df$num_awards)
Budget_USDA_df$num_proposal<-as.numeric(Budget_USDA_df$num_proposal)

#Now replace commas, then convert to numeric: 'extramuralbudget'
Budget_USDA_df$extramuralbudget<-replaceCommas(Budget_USDA_df$extramuralbudget)
Budget_USDA_df$extramuralbudget<-as.numeric(Budget_USDA_df$extramuralbudget, length = 1)

#Now replace commas, then convert to numeric: 'obligated_dollars'
Budget_USDA_df$obligated_dollars<-replaceCommas(Budget_USDA_df$obligated_dollars)
Budget_USDA_df$obligated_dollars<-as.numeric(Budget_USDA_df$obligated_dollars)

#Then 'program budget' and 'deficit dollars'
Budget_USDA_df$programbudget<-replaceCommas(Budget_USDA_df$programbudget)
Budget_USDA_df$programbudget<-as.numeric(Budget_USDA_df$programbudget)

Budget_USDA_df$deficits_dollars<-replaceCommas(Budget_USDA_df$deficits_dollars)
Budget_USDA_df$deficits_dollars<-as.numeric(Budget_USDA_df$deficits_dollars)

#Budget: EPA
Agency_EPA<-"EPA"
Budget_EPA_Call<-paste(Budget_Base,Agency_EPA, Budget_Suffix,sep="")
Budget_EPA_Raw<-"Budget_EPA_Raw.xlsx"
download.file(Budget_EPA_Call,Budget_EPA_Raw )
Budget_EPA_data<-read_excel(Budget_EPA_Raw)

#Wrangle 'Budget_EPA_data'
Budget_EPA_df<-Budget_EPA_data

#Add Column Names
Budget_col_names<-c("year", "extramuralbudget", "obligated_dollars", "programbudget","deficits_dollars","extramural_perc","num_proposal","num_awards")
Budget_EPA_df<-Budget_EPA_df[-1,]
colnames(Budget_EPA_df)<-Budget_col_names

#Create Agency Column
Budget_EPA_df$agency<-"EPA"

#Convert Variables to Numeric
Budget_EPA_df$year<-as.numeric(Budget_EPA_df$year)
Budget_EPA_df$num_awards<-as.numeric(Budget_EPA_df$num_awards)
Budget_EPA_df$num_proposal<-as.numeric(Budget_EPA_df$num_proposal)

#Now replace commas, then convert to numeric: 'extramuralbudget'
Budget_EPA_df$extramuralbudget<-replaceCommas(Budget_EPA_df$extramuralbudget)
Budget_EPA_df$extramuralbudget<-as.numeric(Budget_EPA_df$extramuralbudget, length = 1)

#Now replace commas, then convert to numeric: 'obligated_dollars'
Budget_EPA_df$obligated_dollars<-replaceCommas(Budget_EPA_df$obligated_dollars)
Budget_EPA_df$obligated_dollars<-as.numeric(Budget_EPA_df$obligated_dollars)

#Then 'program budget' and 'deficit dollars'
Budget_EPA_df$programbudget<-replaceCommas(Budget_EPA_df$programbudget)
Budget_EPA_df$programbudget<-as.numeric(Budget_EPA_df$programbudget)

Budget_EPA_df$deficits_dollars<-replaceCommas(Budget_EPA_df$deficits_dollars)
Budget_EPA_df$deficits_dollars<-as.numeric(Budget_EPA_df$deficits_dollars)

#Budget: DOC
Agency_DOC<-"DOC"
Budget_DOC_Call<-paste(Budget_Base,Agency_DOC, Budget_Suffix,sep="")
Budget_DOC_Raw<-"Budget_DOC_Raw.xlsx"
download.file(Budget_DOC_Call,Budget_DOC_Raw )
Budget_DOC_data<-read_excel(Budget_DOC_Raw)

#Wrangle 'Budget_DOC_data'
Budget_DOC_df<-Budget_DOC_data

#Add Column Names
Budget_col_names<-c("year", "extramuralbudget", "obligated_dollars", "programbudget","deficits_dollars","extramural_perc","num_proposal","num_awards")
Budget_DOC_df<-Budget_DOC_df[-1,]
colnames(Budget_DOC_df)<-Budget_col_names

#Create Agency Column
Budget_DOC_df$agency<-"DOC"

#Convert Variables to Numeric
Budget_DOC_df$year<-as.numeric(Budget_DOC_df$year)
Budget_DOC_df$num_awards<-as.numeric(Budget_DOC_df$num_awards)
Budget_DOC_df$num_proposal<-as.numeric(Budget_DOC_df$num_proposal)

#Now replace commas, then convert to numeric: 'extramuralbudget'
Budget_DOC_df$extramuralbudget<-replaceCommas(Budget_DOC_df$extramuralbudget)
Budget_DOC_df$extramuralbudget<-as.numeric(Budget_DOC_df$extramuralbudget, length = 1)

#Now replace commas, then convert to numeric: 'obligated_dollars'
Budget_DOC_df$obligated_dollars<-replaceCommas(Budget_DOC_df$obligated_dollars)
Budget_DOC_df$obligated_dollars<-as.numeric(Budget_DOC_df$obligated_dollars)

#Then 'program budget' and 'deficit dollars'
Budget_DOC_df$programbudget<-replaceCommas(Budget_DOC_df$programbudget)
Budget_DOC_df$programbudget<-as.numeric(Budget_DOC_df$programbudget)

Budget_DOC_df$deficits_dollars<-replaceCommas(Budget_DOC_df$deficits_dollars)
Budget_DOC_df$deficits_dollars<-as.numeric(Budget_DOC_df$deficits_dollars)

#Budget: ED
Agency_ED<-"ED"
Budget_ED_Call<-paste(Budget_Base,Agency_ED, Budget_Suffix,sep="")
Budget_ED_Raw<-"Budget_ED_Raw.xlsx"
download.file(Budget_ED_Call,Budget_ED_Raw )
Budget_ED_data<-read_excel(Budget_ED_Raw)

#Wrangle 'Budget_ED_data'
Budget_ED_df<-Budget_ED_data

#Add Column Names
Budget_col_names<-c("year", "extramuralbudget", "obligated_dollars", "programbudget","deficits_dollars","extramural_perc","num_proposal","num_awards")
Budget_ED_df<-Budget_ED_df[-1,]
colnames(Budget_ED_df)<-Budget_col_names

#Create Agency Column
Budget_ED_df$agency<-"ED"

#Convert Variables to Numeric
Budget_ED_df$year<-as.numeric(Budget_ED_df$year)
Budget_ED_df$num_awards<-as.numeric(Budget_ED_df$num_awards)
Budget_ED_df$num_proposal<-as.numeric(Budget_ED_df$num_proposal)

#Now replace commas, then convert to numeric: 'extramuralbudget'
Budget_ED_df$extramuralbudget<-replaceCommas(Budget_ED_df$extramuralbudget)
Budget_ED_df$extramuralbudget<-as.numeric(Budget_ED_df$extramuralbudget, length = 1)

#Now replace commas, then convert to numeric: 'obligated_dollars'
Budget_ED_df$obligated_dollars<-replaceCommas(Budget_ED_df$obligated_dollars)
Budget_ED_df$obligated_dollars<-as.numeric(Budget_ED_df$obligated_dollars)

#Then 'program budget' and 'deficit dollars'
Budget_ED_df$programbudget<-replaceCommas(Budget_ED_df$programbudget)
Budget_ED_df$programbudget<-as.numeric(Budget_ED_df$programbudget)

Budget_ED_df$deficits_dollars<-replaceCommas(Budget_ED_df$deficits_dollars)
Budget_ED_df$deficits_dollars<-as.numeric(Budget_ED_df$deficits_dollars)

#Budget: DOT
Agency_DOT<-"DOT"
Budget_DOT_Call<-paste(Budget_Base,Agency_DOT, Budget_Suffix,sep="")
Budget_DOT_Raw<-"Budget_DOT_Raw.xlsx"
download.file(Budget_DOT_Call,Budget_DOT_Raw )
Budget_DOT_data<-read_excel(Budget_DOT_Raw)

#Wrangle 'Budget_DOT_data'
Budget_DOT_df<-Budget_DOT_data

#Add Column Names
Budget_col_names<-c("year", "extramuralbudget", "obligated_dollars", "programbudget","deficits_dollars","extramural_perc","num_proposal","num_awards")
Budget_DOT_df<-Budget_DOT_df[-1,]
colnames(Budget_DOT_df)<-Budget_col_names

#Create Agency Column
Budget_DOT_df$agency<-"DOT"

#Convert Variables to Numeric
Budget_DOT_df$year<-as.numeric(Budget_DOT_df$year)
Budget_DOT_df$num_awards<-as.numeric(Budget_DOT_df$num_awards)
Budget_DOT_df$num_proposal<-as.numeric(Budget_DOT_df$num_proposal)

#Now replace commas, then convert to numeric: 'extramuralbudget'
Budget_DOT_df$extramuralbudget<-replaceCommas(Budget_DOT_df$extramuralbudget)
Budget_DOT_df$extramuralbudget<-as.numeric(Budget_DOT_df$extramuralbudget, length = 1)

#Now replace commas, then convert to numeric: 'obligated_dollars'
Budget_DOT_df$obligated_dollars<-replaceCommas(Budget_DOT_df$obligated_dollars)
Budget_DOT_df$obligated_dollars<-as.numeric(Budget_DOT_df$obligated_dollars)

#Then 'program budget' and 'deficit dollars'
Budget_DOT_df$programbudget<-replaceCommas(Budget_DOT_df$programbudget)
Budget_DOT_df$programbudget<-as.numeric(Budget_DOT_df$programbudget)

Budget_DOT_df$deficits_dollars<-replaceCommas(Budget_DOT_df$deficits_dollars)
Budget_DOT_df$deficits_dollars<-as.numeric(Budget_DOT_df$deficits_dollars)

#Budget: DHS
Agency_DHS<-"DHS"
Budget_DHS_Call<-paste(Budget_Base,Agency_DHS, Budget_Suffix,sep="")
Budget_DHS_Raw<-"Budget_DHS_Raw.xlsx"
download.file(Budget_DHS_Call,Budget_DHS_Raw )
Budget_DHS_data<-read_excel(Budget_DHS_Raw)

#Wrangle 'Budget_DHS_data'
Budget_DHS_df<-Budget_DHS_data

#Add Column Names
Budget_col_names<-c("year", "extramuralbudget", "obligated_dollars", "programbudget","deficits_dollars","extramural_perc","num_proposal","num_awards")
Budget_DHS_df<-Budget_DHS_df[-1,]
colnames(Budget_DHS_df)<-Budget_col_names

#Create Agency Column
Budget_DHS_df$agency<-"DHS"

#Convert Variables to Numeric
Budget_DHS_df$year<-as.numeric(Budget_DHS_df$year)
Budget_DHS_df$num_awards<-as.numeric(Budget_DHS_df$num_awards)
Budget_DHS_df$num_proposal<-as.numeric(Budget_DHS_df$num_proposal)

#Now replace commas, then convert to numeric: 'extramuralbudget'
Budget_DHS_df$extramuralbudget<-replaceCommas(Budget_DHS_df$extramuralbudget)
Budget_DHS_df$extramuralbudget<-as.numeric(Budget_DHS_df$extramuralbudget, length = 1)

#Now replace commas, then convert to numeric: 'obligated_dollars'
Budget_DHS_df$obligated_dollars<-replaceCommas(Budget_DHS_df$obligated_dollars)
Budget_DHS_df$obligated_dollars<-as.numeric(Budget_DHS_df$obligated_dollars)

#Then 'program budget' and 'deficit dollars'
Budget_DHS_df$programbudget<-replaceCommas(Budget_DHS_df$programbudget)
Budget_DHS_df$programbudget<-as.numeric(Budget_DHS_df$programbudget)

Budget_DHS_df$deficits_dollars<-replaceCommas(Budget_DHS_df$deficits_dollars)
Budget_DHS_df$deficits_dollars<-as.numeric(Budget_DHS_df$deficits_dollars)

#Change values in Agency column to characters from list. Currentlym 'agency' is a list of characters in each dataframe. 
Awards_HHS_df$agency<-"HHS"
Awards_NASA_df$agency<-"NASA"
Awards_NSF_df$agency<-"NSF"
Awards_DOE_df$agency<-"DOE"
Awards_USDA_df$agency<-"USDA"
Awards_EPA_df$agency<-"EPA"
Awards_DOC_df$agency<-"DOC"
Awards_ED_df$agency<-"ED"
Awards_DOT_df$agency<-"DOT"
Awards_DHS_df$agency<-"DHS"

#Define Column Names for 'SBIR_df' by working from existing column names
Awards_SBIR_colnames<-colnames(Awards_HHS_df)
Awards_SBIR_colnames

#Bind Agency_dfs into 'SBIR_Awards_df'
Awards_SBIR_df<-bind_rows(Awards_DOD_df, Awards_HHS_df, Awards_NASA_df, Awards_NSF_df, Awards_DOE_df, Awards_USDA_df, Awards_EPA_df, Awards_DOC_df, Awards_ED_df, Awards_DOT_df, Awards_DHS_df)

#Create Awards_SBIR_df No 'Awards_DoD_df' exists because of issue with DOD data. Try again without it.
Awards_SBIR_df<-bind_rows(Awards_HHS_df, Awards_NASA_df, Awards_NSF_df, Awards_DOE_df, Awards_USDA_df, Awards_EPA_df, Awards_DOC_df, Awards_ED_df, Awards_DOT_df, Awards_DHS_df)

#Check to make sure the data from each agency is in 'Awards_SBIR_df'
CountbyAgency_Awards_SBIR<-Awards_SBIR_df %>% count(agency)
CountbyAgency_Awards_SBIR
sum(CountbyAgency_Awards_SBIR$n)

#Count_Awards_DOD<-Awards_DOD_df %>% count(agency)
#Total_Awards_DOD<-sum(Count_Awards_DOD$n)

Count_Awards_HHS<-Awards_HHS_df %>% count(agency)
Total_Awards_HHS<-sum(Count_Awards_HHS$n)

Count_Awards_NASA<-Awards_NASA_df %>% count(agency)
Total_Awards_NASA<-sum(Count_Awards_NASA$n)

Count_Awards_NSF<-Awards_NSF_df %>% count(agency)
Total_Awards_NSF<-sum(Count_Awards_NSF$n)

Count_Awards_DOE<-Awards_DOE_df %>% count(agency)
Total_Awards_DOE<-sum(Count_Awards_DOE$n)

Count_Awards_USDA<-Awards_USDA_df %>% count(agency)
Total_Awards_USDA<-sum(Count_Awards_USDA$n)

Count_Awards_EPA<-Awards_EPA_df %>% count(agency)
Total_Awards_EPA<-sum(Count_Awards_EPA$n)

Count_Awards_DOC<-Awards_DOC_df %>% count(agency)
Total_Awards_DOC<-sum(Count_Awards_DOC$n)

Count_Awards_ED<-Awards_ED_df %>% count(agency)
Total_Awards_ED<-sum(Count_Awards_ED$n)

Count_Awards_DOT<-Awards_DOT_df %>% count(agency)
Total_Awards_DOT<-sum(Count_Awards_DOT$n)

Count_Awards_DHS<-Awards_DHS_df %>% count(agency)
Total_Awards_DHS<-sum(Count_Awards_DHS$n)

#If result is true, then we can move to our next step. If it is false, we need to figure out what got added to the agencies or didn't make it into the SBIR dataframe. 
sum(CountbyAgency_Awards_SBIR$n)==sum(Total_Awards_HHS,Total_Awards_NASA,Total_Awards_NSF,Total_Awards_DOE,Total_Awards_USDA,Total_Awards_EPA,Total_Awards_DOC,Total_Awards_ED,Total_Awards_DOT,Total_Awards_DHS)

#5) Collect Data from Award Websites 
#Export 'DUNS_tibble' to csv for use in Web Crawl
#DUNS_tibbleTOcsv<-Awards_SBIR_df$link
#filename_DUNS_tibble<-paste("DUNS_tibble",sys_time,".csv", sep="_")
#write.csv(DUNS_tibbleTOcsv, file= filename_DUNS_tibble )
#rm(DUNS_tibbleTOcsv)

#Use python code to conduct webcraw

#Import Web Crawl data 
WebCrawl_Colnames<-c("index", "link", "firm", "address 1", "duns", "info", "contact", "amount")

#Web Crawl 1
WebCrawl1<-read_excel("DUNS_List1.xlsx", skip=0, col_names = FALSE)
colnames(WebCrawl1)<-WebCrawl_Colnames
missing1<-for (Var in names(WebCrawl1)){
  missing<-sum(is.na(WebCrawl1[,]))
  if (missing >0){
    print(c(Var,missing))
  }
}

#Web crawl 2
WebCrawl2<-read_excel("DUNS_List2.xlsx",skip=0, col_names=FALSE)
colnames(WebCrawl2)<-WebCrawl_Colnames

missing2<-for (Var in names(WebCrawl2)){
  missing<-sum(is.na(WebCrawl2[,]))
  if (missing >0){
    print(c(Var,missing))
  }
}

missing2

#Web Crawl 3
WebCrawl3<-read_excel("DUNS_List3.xlsx",skip=0, col_names=FALSE)
colnames(WebCrawl3)<-WebCrawl_Colnames

missing3<-for (Var in names(WebCrawl3)){
  missing<-sum(is.na(WebCrawl3[,]))
  if (missing >0){
    print(c(Var,missing))
  }
}

missing3

#Web crawl 4
WebCrawl4<-read_excel("DUNS_List4.xlsx",skip=0, col_names=FALSE)
colnames(WebCrawl4)<-WebCrawl_Colnames

missing4<-for (Var in names(WebCrawl4)){
  missing<-sum(is.na(WebCrawl4[,]))
  if (missing >0){
    print(c(Var,missing))
  }
}

missing4

#Web Crawl 5
WebCrawl5<-read_excel("DUNS_List5.xlsx",skip=0, col_names=FALSE)
colnames(WebCrawl5)<-WebCrawl_Colnames

missing5<-for (Var in names(WebCrawl5)){
  missing<-sum(is.na(WebCrawl5[,]))
  if (missing >0){
    print(c(Var,missing))
  }
}

missing5

#Web Crawl 6
WebCrawl6<-read_excel("DUNS_List6.xlsx",skip=0, col_names=FALSE)
colnames(WebCrawl6)<-WebCrawl_Colnames

missing6<-for (Var in names(WebCrawl6)){
  missing<-sum(is.na(WebCrawl6[,]))
  if (missing >0){
    print(c(Var,missing))
  }
}

missing6

#Web Crawl 13
WebCrawl13<-read_excel("DUNS_List13.xlsx",skip=0, col_names=FALSE)
colnames(WebCrawl13)<-WebCrawl_Colnames

missing13<-for (Var in names(WebCrawl13)){
  missing<-sum(is.na(WebCrawl6[,]))
  if (missing >0){
    print(c(Var,missing))
  }
}


#Read WebCrawl_Combined (approx 70,000 observations). Must use CSV because Excel row limit
WebCrawlCombined<-read.csv("DUNS_Combined.csv")
colnames(WebCrawlCombined)<-WebCrawl_Colnames


missingCombined<-for (Var in names(WebCrawlCombined)){
  missing<-sum(is.na(WebCrawlCombined[,]))
  if (missing >0){
    print(c(Var,missing))
  }
}

missingCombined


#Clean WebCrawl Data####
#No missing Cases - Merge into 1 Data Frame 
WebCrawl_All_df<-bind_rows(WebCrawl1,WebCrawl2,WebCrawl3,WebCrawl4,WebCrawl5,WebCrawl6,WebCrawl13,WebCrawlCombined)
str(WebCrawl_All_df)

#General Text Field Preparation - Remove Bad Characters and such 
WebCrawl_All_df$link<- WebCrawl_All_df$link %>% str_replace_all("\\[|\\]", "")
WebCrawl_All_df$link<- gsub('"', '', WebCrawl_All_df$link)
WebCrawl_All_df$link<- gsub("'", '', WebCrawl_All_df$link)
WebCrawl_All_df$firm<- WebCrawl_All_df$firm %>% str_replace_all("\\[|\\]", "")
WebCrawl_All_df$firm<- gsub("'", '', WebCrawl_All_df$firm)
WebCrawl_All_df$`address 1`<- WebCrawl_All_df$`address 1` %>% str_replace_all("\\[|\\]", "")
WebCrawl_All_df$`address 1`<- gsub("'", '', WebCrawl_All_df$`address 1`)
WebCrawl_All_df$`address 1`<- trimws(WebCrawl_All_df$`address 1`)
WebCrawl_All_df$duns<- WebCrawl_All_df$duns %>% str_replace_all("\\[|\\]", "")
WebCrawl_All_df$duns<- gsub("'", '', WebCrawl_All_df$duns)
WebCrawl_All_df$duns<- WebCrawl_All_df$duns %>% str_remove_all("[:alpha:]")
WebCrawl_All_df$duns<- WebCrawl_All_df$duns %>% str_remove_all("[:punct:]")
WebCrawl_All_df$info<- WebCrawl_All_df$info %>% str_replace_all("\\[|\\]", "")
WebCrawl_All_df$info<- gsub("'", '', WebCrawl_All_df$info)
WebCrawl_All_df$contact<- WebCrawl_All_df$contact %>% str_replace_all("\\[|\\]", "")
WebCrawl_All_df$contact<- gsub("'", '', WebCrawl_All_df$contact)
WebCrawl_All_df$amount<- WebCrawl_All_df$amount %>% str_replace_all("\\[|\\]", "")
WebCrawl_All_df$amount<- gsub("'", '', WebCrawl_All_df$amount)
WebCrawl_All_df$amount<- WebCrawl_All_df$amount %>% str_remove_all("[:alpha:]")
WebCrawl_All_df$amount<- WebCrawl_All_df$amount %>% str_remove_all("[:punct:]")
WebCrawl_All_df$amount<- WebCrawl_All_df$amount %>% str_remove_all("\\$")


#Address - Extract zipcode and state
#WebCrawl_All_df$zip<-str_extract(WebCrawl_All_df$`address 1`, "\\d{5}-\\d{4}")
#WebCrawl_All_df$zip<-str_extract(WebCrawl_All_df$`address 1`, ", \\d{5}")
WebCrawl_All_df$zip<-str_extract(WebCrawl_All_df$`address 1`, ", [:upper:]{2}, \\d{5}")
WebCrawl_All_df$zip<-str_remove_all(WebCrawl_All_df$zip, "[:punct:]")
WebCrawl_All_df$zip<-str_remove_all(WebCrawl_All_df$zip, "[:alpha:]")
WebCrawl_All_df$zip<-str_remove_all(WebCrawl_All_df$zip, "[:space:]")
WebCrawl_All_df$state<-str_extract(WebCrawl_All_df$`address 1`, ", [:upper:]{2},")
WebCrawl_All_df$state<-str_remove_all(WebCrawl_All_df$state, "[:punct:]")
WebCrawl_All_df$state<-str_remove(WebCrawl_All_df$state, "[:space:]")

#Store Backup of WebCrawl_All_df
WebCrawl_All_backup_df<-WebCrawl_All_df

#Delete duplicates - No duplicates
WebCrawl_All_df[duplicated(WebCrawl_All_df), ]

#Cleaning Zip Code and State
#Clean Zip Codes (Standardize them with database)
WebCrawl_All_df$zip<-clean.zipcodes(WebCrawl_All_df$zip)
names(WebCrawl_All_df)

#Join Zip Code Database to our dataframe 
WebCrawl_All_df<-left_join(WebCrawl_All_df, zipcode, by="zip")

#Review for issues. The main reason for missing data is incorrect zip code due to extract methods. There are also incorrect zip codes (listed as 00000), correct zip codes but are not in zipcode database, or no zip code. 
#I addressed the extract zip code problem. I cannot correct the 00000 so I set these to NA. The correct zip codes but that are not in my database, I am keeping. I will mark incomplete records in the next step. 
Issues<-WebCrawl_All_df[!complete.cases(WebCrawl_All_df),]
str(Issues)

#Set the 00000 zipcodes to NA. 16 rows in total
Wrong<- WebCrawl_All_df$zip[WebCrawl_All_df$zip=="00000"]
table(Wrong)
WebCrawl_All_df$zip[WebCrawl_All_df$zip=="00000"]<-NA

#Update Issues
WebCrawl_All_df[!complete.cases(WebCrawl_All_df$zip),]
#2590 cases without Zip Code, going to mark them as incomplete. 

#Mark Rest of cases as Incomplete 
# I am making the decision to create a column to indicate the record is incomplete. 
WebCrawl_All_df$Incomplete<-0
WebCrawl_All_df[!complete.cases(WebCrawl_All_df),'Incomplete']<-"Yes"
WebCrawl_All_df[complete.cases(WebCrawl_All_df),'Incomplete']<-"No"
WebCrawl_All_df[WebCrawl_All_df$Incomplete=="Yes",]

#Find Issues with State - 554 Issues with State
WebCrawl_All_df$StateIssues<-WebCrawl_All_df$state.x!=WebCrawl_All_df$state.y
StateIssues_df<-WebCrawl_All_df[WebCrawl_All_df$Issues==TRUE,]
StateIssues_df

#Manually figure out what to do with zip/state issues#
nrow(StateIssues_df)
#View(StateIssues_df)

WebCrawl_All_df[WebCrawl_All_df$StateIssues==TRUE & WebCrawl_All_df$Incomplete=="Yes",]

#Create Columns for 'Hubzone Owned', 'Woman Owned', 'Economically Disadvantaged'
#HUBZone:
WebCrawl_All_df$hub<-str_extract(WebCrawl_All_df$info, "HUBZone Owned: (.*?) ")
WebCrawl_All_df$hub<-str_remove(WebCrawl_All_df$hub, "HUBZone Owned:")
WebCrawl_All_df$hub<-str_remove_all(WebCrawl_All_df$hub, "[:space:]")

#Check values 
table(WebCrawl_All_df$hub)
#Issues<-WebCrawl_All_df[WebCrawl_All_df$hub=="Unavailable",]

#Woman:
WebCrawl_All_df$woman<-str_extract(WebCrawl_All_df$info, "Woman Owned: (.*?) ")
WebCrawl_All_df$woman<-str_remove(WebCrawl_All_df$woman, "Woman Owned:")
WebCrawl_All_df$woman<-str_remove_all(WebCrawl_All_df$woman, "[:space:]")

#Check values 
table(WebCrawl_All_df$woman)

#Socially and Economically Disadvantaged:
WebCrawl_All_df$social<-str_extract(WebCrawl_All_df$info, "Socially and Economically Disadvantaged: (.*?) ")
WebCrawl_All_df$social<-str_remove(WebCrawl_All_df$social, "Socially and Economically Disadvantaged:")
WebCrawl_All_df$social<-str_remove_all(WebCrawl_All_df$social, "[:space:]")

#Check values 
table(WebCrawl_All_df$social)

#Check Number of Complete Cases: 11,581
nrow(WebCrawl_All_df[complete.cases(WebCrawl_All_df),])


#In future, harvest phone numbers and emails from data. Not necessary for this analysis.

#Join WebCrawl Data with Awards_SBIR_df####
Awards_SBIR_df<-left_join(Awards_SBIR_df, WebCrawl_All_df, by = "link")
nrow(Awards_SBIR_df)
names(Awards_SBIR_df)

#Check for Duplicates 
Awards_SBIR_df[duplicated(Awards_SBIR_df), ]


#Clean Research Institution
Awards_SBIR_df$`reseach institution`[is.na(Awards_SBIR_df$`reseach institution`)]<-"None"

#Convert columns to numerical
Awards_SBIR_df$amount<-as.numeric(Awards_SBIR_df$amount)
Awards_SBIR_df$amount<-(Awards_SBIR_df$amount*.00001)

#Create agency-year ID
Awards_SBIR_df$ageyr<-paste0(Awards_SBIR_df$agency,Awards_SBIR_df$year)

#For sake of testing and analysis moving forward - create 'Awards_SBIR_df_test'
#Awards_SBIR_df_test<-Awards_SBIR_df 
nrow(Awards_SBIR_df_test[complete.cases(Awards_SBIR_df_test),])

#Clean Research Institution
Awards_SBIR_df$`reseach institution`[is.na(Awards_SBIR_df$`reseach institution`)]<-"None"

#Keep only complete cases - same number of rows as webscraper
Awards_SBIR_df_complete<-Awards_SBIR_df[complete.cases(Awards_SBIR_df_test),]
nrow(Awards_SBIR_df_complete)

#Compare Awards_SBIR_df_complete with Awards_SBIR_df
(nrow(Awards_SBIR_df)-nrow(Awards_SBIR_df_complete))/nrow(Awards_SBIR_df)

Awards_SBIR_df_filter<-Awards_SBIR_df %>% 
  filter(year<2018)

1-(nrow(Awards_SBIR_df_filter)-nrow(Awards_SBIR_df_complete))/nrow(Awards_SBIR_df_filter)

#6) Merge Solicit_Agency Dataframes into Solicit_SBIR dataframe####
#With DOD
Solicit_SBIR_df<-bind_rows(Solicit_DOD_df, Solicit_HHS_df, Solicit_NASA_df, Solicit_NSF_df, Solicit_DOE_df, Solicit_USDA_df, Solicit_EPA_df, Solicit_DOC_df, Solicit_ED_df, Solicit_DOT_df, Solicit_DHS_df)

#Without DOD
Solicit_SBIR_df<-bind_rows(Solicit_HHS_df, Solicit_NASA_df, Solicit_NSF_df, Solicit_DOE_df, Solicit_USDA_df, Solicit_EPA_df, Solicit_DOC_df, Solicit_ED_df, Solicit_DOT_df, Solicit_DHS_df)


#7) Create 'Budget_SBIR_df' ####
#With DoD
rm(Budget_SBIR_df)
Budget_SBIR_df<-bind_rows(Budget_DOD_df, Budget_HHS_df, Budget_NASA_df, Budget_NSF_df, Budget_DOE_df, Budget_USDA_df, Budget_EPA_df, Budget_DOC_df, Budget_ED_df, Budget_DOT_df, Budget_DHS_df)

#Without DoD
#Budget_SBIR_df<-bind_rows(Budget_HHS_df, Budget_NASA_df, Budget_NSF_df, Budget_DOE_df, Budget_USDA_df, Budget_EPA_df, Budget_DOC_df, Budget_ED_df, Budget_DOT_df, Budget_DHS_df)

#Check structure 'Budget_SBIR_df'
str(Budget_SBIR_df)

#Create agency-year identifier 'ageyr'
Budget_SBIR_df$ageyr<-paste(Budget_SBIR_df$agency,sep="",Budget_SBIR_df$year)

#Convert 'extramural_perc' to numeric
Budget_SBIR_df$extramural_perc<-as.numeric(Budget_SBIR_df$extramural_perc)

#Check Data
ggplot(Budget_SBIR_df, aes(x=year))+
  geom_histogram(binwidth=.5)+
  scale_x_continuous(breaks= seq(1990, 2016, by=1))

#Check NAs
anyNA(Budget_SBIR_df)
missing_cases_Budget_SBIR<-Budget_SBIR_df[!complete.cases(Budget_SBIR_df),]
missing_cases_Budget_SBIR[is.na(missing_cases_Budget_SBIR$obligated_dollars),]

#Data frame of all All Missing data
missing_cases_Budget_SBIR

#Fix Missing data: calculate data for fields that can be calculated
#Calculate Number of Awards 93 and 99
Num_Awards93<-Awards_SBIR_df %>% 
  group_by(agency, year) %>% 
  filter(year==1993) %>% 
  count()

Num_Awards93<-as_data_frame(Num_Awards93)
colnames(Num_Awards93)[3]<-"num_awards"
str(Num_Awards93)

Num_Awards99<-Awards_SBIR_df %>% 
  group_by(agency, year) %>% 
  filter(year==1999) %>% 
  count()

Num_Awards99<-as_data_frame(Num_Awards99)
colnames(Num_Awards99)[3]<-"num_awards"
str(Num_Awards99)

Missing_Num_Awards<-bind_rows(Num_Awards93, Num_Awards99)
Missing_Num_Awards$ageyr<-paste(Missing_Num_Awards$agency,sep='',Missing_Num_Awards$year)


#Program Budget calculation
Amount93<-Awards_SBIR_df %>% 
  filter(year==1993) %>% 
  group_by(agency, year) %>% 
  summarise(programbudget=sum(amount))

Amount93<-as_data_frame(Amount93)


Amount99<-Awards_SBIR_df %>% 
  filter(year==1999) %>% 
  group_by(agency, year) %>% 
  summarise(programbudget=sum(amount))

Amount99<-as_data_frame(Amount99)

Missing_Programbudget<-bind_rows(Amount93, Amount99)
Missing_Programbudget$ageyr<-paste(Missing_Programbudget$agency,sep='',Missing_Programbudget$year)

#Create Table of the calculated data
Awards_MissingYears<-left_join(Missing_Programbudget,Missing_Num_Awards, by="ageyr")
Awards_MissingYears$agency.y<-NULL
Awards_MissingYears$year.y<-NULL
Awards_MissingYears$year<-Awards_MissingYears$year.x
Awards_MissingYears$agency<-Awards_MissingYears$agency.x
Awards_MissingYears$year.x<-NULL
Awards_MissingYears$agency.x<-NULL
Awards_MissingYears$n<-NULL


#Create ageyr in Budget_SBIR_df
Budget_SBIR_df$ageyr<-paste(Budget_SBIR_df$agency,sep='',Budget_SBIR_df$year)
table(Budget_SBIR_df$year)

#Back up Budget_SBIR_df
Budget_SBIR_df_backup<-Budget_SBIR_df

#Join Awards_MissingYears into Budget_SBIR_df
Budget_SBIR_df<-full_join(Budget_SBIR_df,Awards_MissingYears)
str(Budget_SBIR_df)

#Missing DoD from Budget....should I just cut it out? 
Budget_SBIR_df<-Budget_SBIR_df[Budget_SBIR_df$agency!= "DOD",]
str(Budget_SBIR_df)
nrow(Budget_SBIR_df)

Budget_SBIR_df[!complete.cases(Budget_SBIR_df),]

#Join Congressional Setaside Data
str(Set_Aside_df)
Budget_SBIR_df<-left_join(Budget_SBIR_df,Set_Aside_df, by = "year" )

#Calculate Program Budget for cases with extramural research number and setaside percent
#View(Budget_SBIR_df[!is.na(Budget_SBIR_df$extramuralbudget) & is.na(Budget_SBIR_df$programbudget),])

Budget_SBIR_df[!is.na(Budget_SBIR_df$extramuralbudget) & is.na(Budget_SBIR_df$programbudget),"programbudget"]<-(Budget_SBIR_df[!is.na(Budget_SBIR_df$extramuralbudget) & is.na(Budget_SBIR_df$programbudget),"extramuralbudget"]*(Budget_SBIR_df[!is.na(Budget_SBIR_df$extramuralbudget) & is.na(Budget_SBIR_df$programbudget),"set_asidepercent"]/100))

#Calculuate Deficit Dolars for cases with program budget and obligated amount
Budget_SBIR_df[!is.na(Budget_SBIR_df$obligated_dollars) & is.na(Budget_SBIR_df$deficits_dollars),"deficits_dollars"]<- (Budget_SBIR_df[!is.na(Budget_SBIR_df$obligated_dollars) & is.na(Budget_SBIR_df$deficits_dollars),"obligated_dollars"]-Budget_SBIR_df[!is.na(Budget_SBIR_df$obligated_dollars) & is.na(Budget_SBIR_df$deficits_dollars),"programbudget"])

#Impute Extramural Research Budgets for the 1993 and 1999 records 
Budget_SBIR_df$year<-as.Date(as.character(Budget_SBIR_df$year), format='%Y')
str(Budget_SBIR_df)

#imputeTS Approach###


#Need to make univariate vectorrs for each agency's extramural budget then use Kalman technique in R

#DHS
Budget_DHS_df_TS<-Budget_SBIR_df %>% 
  filter(agency=="DHS") %>% 
  select(year, ageyr, extramuralbudget) %>% 
  arrange(year)

Budget_DHS_df_TS$extramuralbudget_int<-na.kalman(Budget_DHS_df_TS$extramuralbudget)
Budget_DHS_df_TS$agency<-"DHS"

#DOC
Budget_DOC_df_TS<-Budget_SBIR_df %>% 
  filter(agency=="DOC") %>% 
  select(year, ageyr, extramuralbudget)  %>% 
  arrange(year)

Budget_DOC_df_TS$extramuralbudget_int<-na.kalman(Budget_DOC_df_TS$extramuralbudget)
Budget_DOC_df_TS$agency<-"DOC"

#DOE
Budget_DOE_df_TS<-Budget_SBIR_df %>% 
  filter(agency=="DOE") %>% 
  select(year, ageyr, extramuralbudget)  %>% 
  arrange(year)

Budget_DOE_df_TS$extramuralbudget_int<-na.kalman(Budget_DOE_df_TS$extramuralbudget)
Budget_DOE_df_TS$agency<-"DOE"


#DOT
Budget_DOT_df_TS<-Budget_SBIR_df %>% 
  filter(agency=="DOT") %>% 
  select(year, ageyr, extramuralbudget)  %>% 
  arrange(year)

Budget_DOT_df_TS$extramuralbudget_int<-na.kalman(Budget_DOT_df_TS$extramuralbudget)
Budget_DOT_df_TS$agency<-"DOT"

#ED
Budget_ED_df_TS<-Budget_SBIR_df %>% 
  filter(agency=="ED") %>% 
  select(year, ageyr, extramuralbudget)  %>% 
  arrange(year)

Budget_ED_df_TS$extramuralbudget_int<-na.kalman(Budget_ED_df_TS$extramuralbudget)
Budget_ED_df_TS$agency<-"ED"

#EPA
Budget_EPA_df_TS<-Budget_SBIR_df %>% 
  filter(agency=="EPA") %>% 
  select(year, ageyr, extramuralbudget)  %>% 
  arrange(year)

Budget_EPA_df_TS$extramuralbudget_int<-na.kalman(Budget_EPA_df_TS$extramuralbudget)
Budget_EPA_df_TS$agency<-"EPA"

#HHS
Budget_HHS_df_TS<-Budget_SBIR_df %>% 
  filter(agency=="HHS") %>% 
  select(year, ageyr, extramuralbudget)  %>% 
  arrange(year)

Budget_HHS_df_TS$extramuralbudget_int<-na.kalman(Budget_HHS_df_TS$extramuralbudget)
Budget_HHS_df_TS$agency<-"HHS"

#NASA
Budget_NASA_df_TS<-Budget_SBIR_df %>% 
  filter(agency=="NASA") %>% 
  select(year, ageyr, extramuralbudget)  %>% 
  arrange(year)

Budget_NASA_df_TS$extramuralbudget_int<-na.kalman(Budget_NASA_df_TS$extramuralbudget)
Budget_NASA_df_TS$agency<-"NASA"

#NSF
Budget_NSF_df_TS<-Budget_SBIR_df %>% 
  filter(agency=="NSF") %>% 
  select(year, ageyr, extramuralbudget)  %>% 
  arrange(year)

Budget_NSF_df_TS$extramuralbudget_int<-na.kalman(Budget_NSF_df_TS$extramuralbudget)
Budget_NSF_df_TS$agency<-"NSF"

#USDA
Budget_USDA_df_TS<-Budget_SBIR_df %>% 
  filter(agency=="USDA") %>% 
  select(year, ageyr, extramuralbudget)  %>% 
  arrange(year)

Budget_USDA_df_TS$extramuralbudget_int<-na.kalman(Budget_USDA_df_TS$extramuralbudget)
Budget_USDA_df_TS$agency<-"USDA"

rm(Budget_SBIR_TS)
#Bind Rows
Budget_SBIR_TS<-bind_rows(Budget_USDA_df_TS, Budget_NSF_df_TS,Budget_NASA_df_TS, Budget_HHS_df_TS, Budget_EPA_df_TS, Budget_ED_df_TS, Budget_DOT_df_TS, Budget_DOE_df_TS, Budget_DOC_df_TS, Budget_DHS_df_TS)


#Visualize completed imputed data for extramuralbudget
ggplot(Budget_SBIR_TS, aes(x=year, y=extramuralbudget_int))+
  geom_line(aes(color=agency))


#Join data to Budget_SBIR_df
Budget_SBIR_df<-inner_join(Budget_SBIR_df, Budget_SBIR_TS)

Budget_SBIR_df$extramuralbudget<-Budget_SBIR_df$extramuralbudget_int
Budget_SBIR_df$extramuralbudget_int<-NULL

#Calculate obligated_dollars, deficits_dollars, extramural_perc
#Calculuate obligated dollars for cases with extramuralbudget and set_asidepercent
Budget_SBIR_df[!is.na(Budget_SBIR_df$extramuralbudget) & is.na(Budget_SBIR_df$obligated_dollars),"obligated_dollars"]<- (Budget_SBIR_df[!is.na(Budget_SBIR_df$extramuralbudget) & is.na(Budget_SBIR_df$obligated_dollars),"extramuralbudget"]*(Budget_SBIR_df[!is.na(Budget_SBIR_df$extramuralbudget) & is.na(Budget_SBIR_df$obligated_dollars),"set_asidepercent"]/100))

#Calculuate Deficit Dolars for cases with program budget and obligated amount
Budget_SBIR_df[!is.na(Budget_SBIR_df$obligated_dollars) & is.na(Budget_SBIR_df$deficits_dollars),"deficits_dollars"]<- (Budget_SBIR_df[!is.na(Budget_SBIR_df$obligated_dollars) & is.na(Budget_SBIR_df$deficits_dollars),"obligated_dollars"]-Budget_SBIR_df[!is.na(Budget_SBIR_df$obligated_dollars) & is.na(Budget_SBIR_df$deficits_dollars),"programbudget"])

#Calculate extramural_perc with extramuralbudget and programbudget
Budget_SBIR_df[!is.na(Budget_SBIR_df$programbudget) & is.na(Budget_SBIR_df$extramural_perc),"extramural_perc"]<- (Budget_SBIR_df[!is.na(Budget_SBIR_df$programbudget) & is.na(Budget_SBIR_df$extramural_perc),"programbudget"]/Budget_SBIR_df[!is.na(Budget_SBIR_df$programbudget) & is.na(Budget_SBIR_df$extramural_perc),"extramuralbudget"]*100)

#Impute data for non-calculable fields: number of proposals####
#DHS
Budget_DHS_df_Prop<-Budget_SBIR_df %>% 
  filter(agency=="DHS") %>% 
  select(year, ageyr, num_proposal) %>% 
  arrange(year)

Budget_DHS_df_Prop$num_proposal_int<- na.kalman(Budget_DHS_df_Prop$num_proposal)
Budget_DHS_df_Prop$agency<-"DHS"

#DOC
Budget_DOC_df_Prop<-Budget_SBIR_df %>% 
  filter(agency=="DOC") %>% 
  select(year, ageyr, num_proposal)  %>% 
  arrange(year)

Budget_DOC_df_Prop$num_proposal_int<-na.kalman(Budget_DOC_df_Prop$num_proposal)
Budget_DOC_df_Prop$agency<-"DOC"

#DOE
Budget_DOE_df_Prop<-Budget_SBIR_df %>% 
  filter(agency=="DOE") %>% 
  select(year, ageyr, num_proposal)  %>% 
  arrange(year)

Budget_DOE_df_Prop$num_proposal_int<-na.kalman(Budget_DOE_df_Prop$num_proposal)
Budget_DOE_df_Prop$agency<-"DOE"


#DOT
Budget_DOT_df_Prop<-Budget_SBIR_df %>% 
  filter(agency=="DOT") %>% 
  select(year, ageyr, num_proposal)  %>% 
  arrange(year)

Budget_DOT_df_Prop$num_proposal_int<-na.kalman(Budget_DOT_df_Prop$num_proposal)
Budget_DOT_df_Prop$agency<-"DOT"

#ED
Budget_ED_df_Prop<-Budget_SBIR_df %>% 
  filter(agency=="ED") %>% 
  select(year, ageyr, num_proposal)  %>% 
  arrange(year)

Budget_ED_df_Prop$num_proposal_int<-na.kalman(Budget_ED_df_Prop$num_proposal)
Budget_ED_df_Prop$agency<-"ED"

#EPA
Budget_EPA_df_Prop<-Budget_SBIR_df %>% 
  filter(agency=="EPA") %>% 
  select(year, ageyr, num_proposal)  %>% 
  arrange(year)

Budget_EPA_df_Prop$num_proposal_int<-na.kalman(Budget_EPA_df_Prop$num_proposal)
Budget_EPA_df_Prop$agency<-"EPA"

#HHS
Budget_HHS_df_Prop<-Budget_SBIR_df %>% 
  filter(agency=="HHS") %>% 
  select(year, ageyr, num_proposal)  %>% 
  arrange(year)

Budget_HHS_df_Prop$num_proposal_int<-na.kalman(Budget_HHS_df_Prop$num_proposal)
Budget_HHS_df_Prop$agency<-"HHS"

#NASA
Budget_NASA_df_Prop<-Budget_SBIR_df %>% 
  filter(agency=="NASA") %>% 
  select(year, ageyr, num_proposal)  %>% 
  arrange(year)

Budget_NASA_df_Prop$num_proposal_int<-na.kalman(Budget_NASA_df_Prop$num_proposal)
Budget_NASA_df_Prop$agency<-"NASA"

#NSF
Budget_NSF_df_Prop<-Budget_SBIR_df %>% 
  filter(agency=="NSF") %>% 
  select(year, ageyr, num_proposal)  %>% 
  arrange(year)

Budget_NSF_df_Prop$num_proposal_int<-na.kalman(Budget_NSF_df_Prop$num_proposal)
Budget_NSF_df_Prop$agency<-"NSF"

#USDA
Budget_USDA_df_Prop<-Budget_SBIR_df %>% 
  filter(agency=="USDA") %>% 
  select(year, ageyr, num_proposal)  %>% 
  arrange(year)

Budget_USDA_df_Prop$num_proposal_int<-na.kalman(Budget_USDA_df_Prop$num_proposal)
Budget_USDA_df_Prop$agency<-"USDA"

#Bind Rows
rm(Budget_SBIR_Prop)
Budget_SBIR_Prop<-bind_rows(Budget_USDA_df_Prop, Budget_NSF_df_Prop,Budget_NASA_df_Prop, Budget_HHS_df_Prop, Budget_EPA_df_Prop, Budget_ED_df_Prop, Budget_DOT_df_Prop, Budget_DOE_df_Prop, Budget_DOC_df_Prop, Budget_DHS_df_Prop)
Budget_SBIR_Prop$num_proposal_int<-round(Budget_SBIR_Prop$num_proposal_int, digits=0)


#Visualize completed imputed data for extramuralbudget
ggplot(Budget_SBIR_Prop, aes(x=year, y=num_proposal_int))+
  geom_line(aes(color=agency))

#Join data to Budget_SBIR_df
Budget_SBIR_df<-inner_join(Budget_SBIR_df, Budget_SBIR_Prop)

Budget_SBIR_df$num_proposal<-Budget_SBIR_df$num_proposal_int
Budget_SBIR_df$num_proposal_int<-NULL


#Analysis of Program Budget to Obligated Dollars Relationship - NSF in 2009 is an outlier. Why is program budget so much higher? 
#View(Budget_SBIR_df[Budget_SBIR_df$programbudget>Budget_SBIR_df$obligated_dollars,"deficits_dollars"])

#Create Comply Variable
#If comply with set-aside requirement for year = 1, if not comply = 0
Budget_SBIR_df$Comply <-"0"
Budget_SBIR_df$Comply[Budget_SBIR_df$extramural_perc>=Budget_SBIR_df$set_asidepercent]<-1
Budget_SBIR_df$Comply[Budget_SBIR_df$extramural_perc<Budget_SBIR_df$set_asidepercent]<-0

Budget_SBIR_df_backup2<-Budget_SBIR_df
#Calculate extramural_perc change#

#Calculate ExtramuralPerc Change
Budget_SBIR_df<- Budget_SBIR_df %>% 
  mutate(extramural_perc_chng= extramural_perc - lag(extramural_perc, default=first(extramural_perc)))

#Calculate AwardsYoY Change
Budget_SBIR_df<- Budget_SBIR_df %>% 
  mutate(num_awards_yoy= num_awards - lag(num_awards, default=first(num_awards)))


#Calculate AwardsYoY Change
Budget_SBIR_df<- Budget_SBIR_df %>% 
  mutate(num_awards_yoy= num_awards - lag(num_awards, default=first(num_awards)))


#Calculate ProposalsYoY Change
Budget_SBIR_df<- Budget_SBIR_df %>% 
  mutate(num_proposal_yoy= num_proposal - lag(num_proposal, default=first(num_proposal)))

ggplot(Awards_SBIR_df, aes(x=year))+
  geom_bar()


#Calculate Avg Award per agency-year in tens-of thousands of dollars
age_year<-Awards_SBIR_df %>% 
  group_by(ageyr) %>% 
  summarise(mean(amount))


age_year$avg_award<-age_year$`mean(amount)`
age_year$`mean(amount)`<-NULL

Budget_SBIR_df<-left_join(Budget_SBIR_df,age_year, by="ageyr")


#Calculate Program and Phase per agency-year in tens-of thousands of dollars
#Phase - counts 
age_year<-Awards_SBIR_df %>% 
  group_by(ageyr) %>% 
  count(phase)

age_year<-age_year %>% 
  spread(phase, n)

names(age_year)[2]<-"phase1"
names(age_year)[3]<-"phase2"

age_year[!complete.cases(age_year$phase1), "phase1"]<-0
age_year[!complete.cases(age_year$phase2), "phase2"]<-0

Budget_SBIR_df<-left_join(Budget_SBIR_df,age_year, by="ageyr")

#Program - counts
age_year<-Awards_SBIR_df %>% 
  group_by(ageyr) %>% 
  count(program)

age_year<-age_year %>% 
  spread(program, n)

names(age_year)[2]<-"sbir"
names(age_year)[3]<-"sttr"

age_year[!complete.cases(age_year$sbir), "sbir"]<-0
age_year[!complete.cases(age_year$sttr), "sttr"]<-0

Budget_SBIR_df<-left_join(Budget_SBIR_df,age_year, by="ageyr")

#Verify no more NAs
Budget_SBIR_df[!anyNA(Budget_SBIR_df$avg_award),]

#8) Save Data Frames#### 
#Award_SBIR_df
name<-"Awards_SBIR_df.Rdata"
save(file= name, Awards_SBIR_df)

#Budget_SBIR_df
name<- "Budget_SBIR_df.RData"
save(file=name, Budget_SBIR_df)

#Solicit_SBIR_df
name<-"Solicit_SBIR_df.Rdata"
  save(file=name, Solicit_SBIR_df)







