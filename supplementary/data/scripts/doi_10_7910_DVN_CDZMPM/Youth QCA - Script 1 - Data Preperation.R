## WDs

setwd("E:/Fortis/Workspace/Research Article - QCA Youth organizations/Revise 1")
setwd("C:/Users/fe300/Desktop/Research Article - QCA Youth organizations/Revise 1")

library(tidyverse)

# download and read Party Facts mapping table
file_name <- "partyfacts-mapping.csv"
if( ! file_name %in% list.files()) {
  url <- "https://partyfacts.herokuapp.com/download/external-parties-csv/"
  download.file(url, file_name)
}
partyfacts_raw <- read_csv(file_name, guess_max = 50000)
partyfacts <- partyfacts_raw |> filter(! is.na(partyfacts_id))

# link datasets (select only linked parties)
dataset_1 <- partyfacts |> filter(dataset_key == "manifesto")
dataset_2 <- partyfacts |> filter(dataset_key == "parlgov")
link_table <-
  dataset_1 |>
  inner_join(dataset_2, by = c("partyfacts_id" = "partyfacts_id"))

# write results into file with dataset names in file name
file_out <- "partyfacts-linked.csv"
write_csv(link_table, file_out)


## Merging

ARPL_Dataset <- read.csv("E:/Fortis/Workspace/Research Article - QCA Youth organizations/Revise 1/ARPL_Dataset.csv")


### korrekturen pre-merge (E-mail 4. Juli 2023)
ARPL_Dataset[65, 6] <- "53951"
ARPL_Dataset[120, 3] <- "Civic Coalition"
ARPL_Dataset[122, 6] <- "92811"


## merging
ARPL_Dataset_new <- left_join(ARPL_Dataset, dataset_1, by= c("ID.ManifProj"="dataset_party_id"))


# add after merge: Farmer union: 2002 - 2018
ARPL_Dataset_new[83, 27] <- 2002
ARPL_Dataset_new[83, 28] <- 2018
# add after merge: civic coalition: 2001 - 2015
ARPL_Dataset_new[120, 27] <- 2001
ARPL_Dataset_new[120, 28] <- 2015
# add after merge: Democratic Left alliance (LEFT-P): 1991
ARPL_Dataset_new[123, 27] <- 1991

ARPL_Dataset_new[c(1:21, 27)] -> ARPL_Dataset_plus
names(ARPL_Dataset_plus)[names(ARPL_Dataset_plus) == 'country.x'] <- 'country'
names(ARPL_Dataset_plus)[names(ARPL_Dataset_plus) == 'year_first'] <- 'founding.year'
ARPL_Dataset_plus[,c(1:6, 22, 7:21)] -> ARPL_Dataset_plus


### Write ARPL Dataset Vers. July 2023 (no founding year)
write.csv(ARPL_Dataset, "ARPL_Dataset_update_July2023.csv", row.names=FALSE)

## write update with year of party initiation
write.csv(ARPL_Dataset_plus, "ARPL_Dataset_July2023.csv", row.names=FALSE)


library(foreign)

write.dta(ARPL_Dataset_plus, "ARPL_Dataset_July2023.dta")


rm(list=setdiff(ls(), "ARPL.Dataset_plus"))


####

###### MERGE with QCA Conditions Data
######
rm(list=setdiff(ls(), "QCA.Data.complete.2023"))
write.csv(QCA.Data.complete.2023, "QCA_Conditions_2023.csv", row.names=FALSE)

ARPL_Dataset_July2023 <- read.csv("ARPL_Dataset_July2023.csv")

## error checklist:
## change name of civic platform (poland in Dataset QCA.Data.complete.2023) to civic coalition
QCA.Data.complete.2023[74,4] <- "Civic Coalition"
## error in latvia farmers union
QCA.Data.complete.2023[120,4] <- "Farmers' Union & Green Party of Latvia"



QCA.Data.July2023 <- dplyr::full_join(QCA.Data.complete.2023, ARPL_Dataset_July2023, by=c("Country" = "country", "Party.name" = "party.name"))
QCA.Data.July2023[1:151,] -> QCA.Data.July2023

QCA.Data.July2023[,c(1:14,22:39)] -> QCA.Data.July2023

write.csv(QCA.Data.July2023, "Full_QCA_Data_July_2023.csv", row.names=FALSE)

#ä#ü#ö#############ä#ü#ö#####

######## CALIBRATION
filter(QCA.Data.July2023, nr.MPs >= 10) -> QCA.Data.calibrated
na.omit(QCA.Data.calibrated) -> QCA.Data.calibrated



write.csv(QCA.Data.calibrated, "QCA_Calibrated_July2023.csv", row.names=FALSE)



#### 18 07

ARPL_Dataset_July2023 -> ARPL_Dataset_August2023

ARPL_Dataset_August2023[,-7] -> ARPL_Dataset_August2023

colnames(ARPL_Dataset_August2023) -> colnames.1

colnames.1[20] <- "ARI60"

colnames.1

colnames.1 -> colnames(ARPL_Dataset_August2023)

write.csv(ARPL_Dataset_August2023, "ARPL_Dataset_August2023.csv", row.names=FALSE)
rm(list=setdiff(ls(), "ARPL_Dataset_August2023"))


write.dta(ARPL_Dataset_August2023, "ARPL_Dataset_August2023.dta")


## fix errors in Spain (Year) and errors in share40-60 

ARPL_Dataset_August2023[138,7] <- "2019"
ARPL_Dataset_August2023[139,7] <- "2019"
ARPL_Dataset_August2023[140,7] <- "2019"
ARPL_Dataset_August2023[141,7] <- "2019"
ARPL_Dataset_August2023[142,7] <- "2019"
ARPL_Dataset_August2023[143,7] <- "2019"

as.numeric(ARPL_Dataset_August2023$election.year) -> ARPL_Dataset_August2023$election.year

ARPL_Dataset_August2023[93,12] <- 58.33
ARPL_Dataset_August2023[93,19] <- 1.7130690




##### QCA Revise 1 Mit July2023 data (Load workspace Revise1/ARPL_Dataset_July2023)

ARPL_Dataset_July2023[138,8] <- "2019"
ARPL_Dataset_July2023[139,8] <- "2019"
ARPL_Dataset_July2023[140,8] <- "2019"
ARPL_Dataset_July2023[141,8] <- "2019"
ARPL_Dataset_July2023[142,8] <- "2019"
ARPL_Dataset_July2023[143,8] <- "2019"



as.numeric(ARPL_Dataset_July2023$election.year) -> ARPL_Dataset_July2023$election.year

ARPL_Dataset_July2023$party.age <- ARPL_Dataset_July2023$election.year - ARPL_Dataset_July2023$founding.year



setwd("C:/Users/fe300/Desktop/Research Article - QCA Youth organizations")
library(readr)
QCA.Data <- read_csv("QCA_Data_complete_Feb2023.csv")
QCA.Data.July <- read_csv("Data_QCA_calibrated_ver_March2023.csv")

library(QCA)
library(SetMethods)
library(tidyverse)


## Merge_August_2023 <- left_join(ARPL_Dataset_July2023, QCA.Data, by=c("country" = "Country", "party.name" = "Party.name"))
Merge_August_2023 <- left_join(ARPL_Dataset_July2023, QCA.Data.July, by=c("country" = "Country", "party.name" = "Party.name"))



filter(Merge_August_2023, nr.MPs >= 10) -> Merge_August_2023_v2


## fix 3 cases -no automatic code matching- (D66 - Farmers Union - Civic coalition)

## Farmers Union
Merge_August_2023_v2[67,24:52] <- QCA.Data.July[98, c(2:3, 5:31)]

# Civic Plattform / Coalition
Merge_August_2023_v2[92,24:52] <- QCA.Data.July[57, c(2:3, 5:31)]


## Merge Data for D66 - Netherlands

Merge_August_2023_v2[85, 24] <- "NLD"
Merge_August_2023_v2[85, 25] <- 2018
Merge_August_2023_v2[85, 28] <- 9
Merge_August_2023_v2[85, 29] <- 47474.11
Merge_August_2023_v2[85, 32] <- 41.554
Merge_August_2023_v2[85, 33] <- 18

# decentral

Merge_August_2023_v2[85, 34] <- 0.0000000

# Youth Index

Merge_August_2023_v2[85, 27] <- 0.4

# Manifesto rile 

Merge_August_2023_v2[85, 30] <- -6.514
Merge_August_2023_v2[85, 31] <- -19.464

## Check Sein Fein

## error in lituanian labor party share 40-60 eliminated
Merge_August_2023_v2[75, 13] <- 58.33
Merge_August_2023_v2[75, 20] <- 1.7130690

Merge_August_2023_v2[,c(1:23,27:35)] -> QCA.Data.complete.August2023



colnames(QCA.Data.complete.August2023) -> colnames.2

colnames.2
colnames.2[2] <- "ISO3"
colnames.2[21] <- "ARI60"

colnames.2 -> colnames(QCA.Data.complete.August2023)

## Export csv of the dataset
write.csv(QCA.Data.complete.August2023, "QCA_Data_August2023.csv", row.names=FALSE)

rm(list=setdiff(ls(), "QCA.Data.complete.August2023"))


#######


