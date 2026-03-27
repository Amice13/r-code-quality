
###########################
#### Empirical Analysis ###
###########################
library(readr)
library(stringi)
library(lfe)
library(dplyr)

# Original Data in Repository OSS Data
OSS_Contributions <- read_csv("~/YOUR_WORKING_DIRECTORY/OSS_Contributions.csv")
OSS_Contributions$Date=as.Date(OSS_Contributions$Date)
ReplicationDataMain <- read_csv("~/YOUR_WORKING_DIRECTORY/ReplicationDataMain.csv")
ReplicationDataInventors <- read_csv("~/YOUR_WORKING_DIRECTORY/ReplicationDataInventors.csv")


# Consider PN='US10237130' 
patent_id='US10237130'
Example=ReplicationDataMain[ReplicationDataMain$patent_id==patent_id,]

# This patent was filed on the 2015-03-27 and granted on 2019-03-19
# It mentions linux in its description
# We can retrieve all contributions one month prior filing and granting from OSS_Contributions

ExampleContributionsPriorFiling=subset(OSS_Contributions,Date>=(Example$Date[1]-28) & Date<Example$Date[1] & Project=='linux' & WebDomain=='6wind.com')

# There are two commits, that have modified 2 files by means of inserting 21 new lines and deleting 17 existing lines (38 modifications)
# There are no commits prior granting:

ExampleContributionsPriorGranting=subset(OSS_Contributions,Date>=(Example$Date[2]-28) & Date<Example$Date[2] & Project=='Linux' & WebDomain=='6wind.com')

# Also: There are no other patents filed by '6wind.com' in the two weeks preceding the granting date




###########
# Table 5 #
###########

Column1=felm(log(Commits+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=ReplicationDataMain)
summary(Column1)


Column2=felm(log(Modifications+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=ReplicationDataMain)
summary(Column2)


Column3=felm(log(FilesChanged+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=ReplicationDataMain)
summary(Column3)



###########
# Table 9 #
###########

Column1=felm(log(Commits+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=ReplicationDataInventors)
summary(Column1)


Column2=felm(log(Modifications+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=ReplicationDataInventors)
summary(Column2)


Column3=felm(log(FilesChanged+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=ReplicationDataInventors)
summary(Column3)


##############
# Table B.21 #
##############

Column1=felm(log(Commits+1)~Filing|FxY+TxY+Repos|0|WebDomains+CPC+Year+Repos,data=ReplicationDataMain)
summary(Column1)


Column2=felm(log(Modifications+1)~Filing|FxY+TxY+Repos|0|WebDomains+CPC+Year+Repos,data=ReplicationDataMain)
summary(Column2)


Column3=felm(log(FilesChanged+1)~Filing|FxY+TxY+Repos|0|WebDomains+CPC+Year+Repos,data=ReplicationDataMain)
summary(Column3)


##############
# Table B.23 #
##############

# Years to Grant
ReplicationDataMain=ReplicationDataMain %>%
  group_by(patent_id,WebDomains) %>%
  mutate(YTG=max(Date)-min(Date))
ReplicationDataMain$FastGrants=round(as.numeric(ReplicationDataMain$YTG)/365,1)<5
ReplicationDataMain$FastGrants2=round(as.numeric(ReplicationDataMain$YTG)/365,1)<3


Column1=felm(log(Commits+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=subset(ReplicationDataMain,FastGrants))
summary(Column1)


Column2=felm(log(Modifications+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=subset(ReplicationDataMain,FastGrants))
summary(Column2)


Column3=felm(log(FilesChanged+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=subset(ReplicationDataMain,FastGrants))
summary(Column3)



##############
# Table B.25 #
##############
ReplicationDataMain$FxM=paste0(ReplicationDataMain$WebDomains,ReplicationDataMain$Month)

Column1=felm(log(Commits+1)~Filing|FxM+TxY+Repos|0|FxM+TxY+Repos,data=ReplicationDataMain)
summary(Column1)


Column2=felm(log(Modifications+1)~Filing|FxM+TxY+Repos|0|FxM+TxY+Repos,data=ReplicationDataMain)
summary(Column2)


Column3=felm(log(FilesChanged+1)~Filing|FxM+TxY+Repos|0|FxM+TxY+Repos,data=ReplicationDataMain)
summary(Column3)




##############
# Table B.28 #
##############
ReplicationDataMain$Firm=!stri_detect_fixed(ReplicationDataMain$WebDomains,'.edu') & !stri_detect_fixed(ReplicationDataMain$WebDomains,'.gov')  & !stri_detect_fixed(ReplicationDataMain$WebDomains,'.ac.uk')  

Column1=felm(log(Commits+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=subset(ReplicationDataMain,Firm))
summary(Column1)


Column2=felm(log(Modifications+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=subset(ReplicationDataMain,Firm))
summary(Column2)


Column3=felm(log(FilesChanged+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=subset(ReplicationDataMain,Firm))
summary(Column3)




##############
# Table B.30 #
##############

Column1=felm(log(Commits+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=subset(ReplicationDataMain,Year<=2020))
summary(Column1)


Column2=felm(log(Modifications+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=subset(ReplicationDataMain,Year<=2020))
summary(Column2)


Column3=felm(log(FilesChanged+1)~Filing|FxY+TxY+Repos|0|FxY+TxY+Repos,data=subset(ReplicationDataMain,Year<=2020))
summary(Column3)


##############
# Table B.32 #
##############

Column1=felm(log(Commits+1)~Filing|FxY+CPC+Repos|0|FxY+CPC+Repos,data=subset(ReplicationDataInventors,Days<365*2))
summary(Column1)


Column2=felm(log(Modifications+1)~Filing|FxY+CPC+Repos|0|FxY+CPC+Repos,data=subset(ReplicationDataInventors,Days<365*2))
summary(Column2)


Column3=felm(log(FilesChanged+1)~Filing|FxY+CPC+Repos|0|FxY+CPC+Repos,data=subset(ReplicationDataInventors,Days<365*2))
summary(Column3)


