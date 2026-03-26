########################################
########### CLEAN DATASET ##############
########################################


#This code cleans the dataset and prepares it for the empirical analysis

#INCLUDE YOUR PATH HERE
rm(list=ls())

#warning: in some operating systems, the file needs to be opened in UTF-8
setwd("")


#Load the dataset
#install.packages("readxl")
#install.packages("tibble")
library("readxl")
library("tibble")
mydat <- read_excel("data_analysis.xlsx", sheet="data_final")
glimpse(mydat)

#change names columns to make reshape easier
#table 1 conjoint
colnames(mydat)[37] <- "oilA_t1"
colnames(mydat)[38] <- "allyA_t1"
colnames(mydat)[39] <- "tradeA_t1"
colnames(mydat)[40] <- "yearsA_t1"
colnames(mydat)[41] <- "govA_t1"
colnames(mydat)[42] <- "personA_t1"
colnames(mydat)[43] <- "electionsA_t1"
colnames(mydat)[44] <- "religionA_t1"
colnames(mydat)[45] <- "militarA_t1"

colnames(mydat)[46] <- "oilB_t1"
colnames(mydat)[47] <- "allyB_t1"
colnames(mydat)[48] <- "tradeB_t1"
colnames(mydat)[49] <- "yearsB_t1"
colnames(mydat)[50] <- "govB_t1"
colnames(mydat)[51] <- "personB_t1"
colnames(mydat)[52] <- "electionsB_t1"
colnames(mydat)[53] <- "religionB_t1"
colnames(mydat)[54] <- "militarB_t1"

#table 2 conjoint
colnames(mydat)[64] <- "oilA_t2"
colnames(mydat)[65] <- "allyA_t2"
colnames(mydat)[66] <- "tradeA_t2"
colnames(mydat)[67] <- "yearsA_t2"
colnames(mydat)[68] <- "govA_t2"
colnames(mydat)[69] <- "personA_t2"
colnames(mydat)[70] <- "electionsA_t2"
colnames(mydat)[71] <- "religionA_t2"
colnames(mydat)[72] <- "militarA_t2"

colnames(mydat)[73] <- "oilB_t2"
colnames(mydat)[74] <- "allyB_t2"
colnames(mydat)[75] <- "tradeB_t2"
colnames(mydat)[76] <- "yearsB_t2"
colnames(mydat)[77] <- "govB_t2"
colnames(mydat)[78] <- "personB_t2"
colnames(mydat)[79] <- "electionsB_t2"
colnames(mydat)[80] <- "religionB_t2"
colnames(mydat)[81] <- "militarB_t2"

#table 3 conjoint
colnames(mydat)[92] <- "oilA_t3"
colnames(mydat)[93] <- "allyA_t3"
colnames(mydat)[94] <- "tradeA_t3"
colnames(mydat)[95] <- "yearsA_t3"
colnames(mydat)[96] <- "govA_t3"
colnames(mydat)[97] <- "personA_t3"
colnames(mydat)[98] <- "electionsA_t3"
colnames(mydat)[99] <- "religionA_t3"
colnames(mydat)[100] <- "militarA_t3"

colnames(mydat)[101] <- "oilB_t3"
colnames(mydat)[102] <- "allyB_t3"
colnames(mydat)[103] <- "tradeB_t3"
colnames(mydat)[104] <- "yearsB_t3"
colnames(mydat)[105] <- "govB_t3"
colnames(mydat)[106] <- "personB_t3"
colnames(mydat)[107] <- "electionsB_t3"
colnames(mydat)[108] <- "religionB_t3"
colnames(mydat)[109] <- "militarB_t3"

#table 4 conjoint
colnames(mydat)[120] <- "oilA_t4"
colnames(mydat)[121] <- "allyA_t4"
colnames(mydat)[122] <- "tradeA_t4"
colnames(mydat)[123] <- "yearsA_t4"
colnames(mydat)[124] <- "govA_t4"
colnames(mydat)[125] <- "personA_t4"
colnames(mydat)[126] <- "electionsA_t4"
colnames(mydat)[127] <- "religionA_t4"
colnames(mydat)[128] <- "militarA_t4"

colnames(mydat)[129] <- "oilB_t4"
colnames(mydat)[130] <- "allyB_t4"
colnames(mydat)[131] <- "tradeB_t4"
colnames(mydat)[132] <- "yearsB_t4"
colnames(mydat)[133] <- "govB_t4"
colnames(mydat)[134] <- "personB_t4"
colnames(mydat)[135] <- "electionsB_t4"
colnames(mydat)[136] <- "religionB_t4"
colnames(mydat)[137] <- "militarB_t4"

#table 5 conjoint
colnames(mydat)[150] <- "oilA_t5"
colnames(mydat)[151] <- "allyA_t5"
colnames(mydat)[152] <- "tradeA_t5"
colnames(mydat)[153] <- "yearsA_t5"
colnames(mydat)[154] <- "govA_t5"
colnames(mydat)[155] <- "personA_t5"
colnames(mydat)[156] <- "electionsA_t5"
colnames(mydat)[157] <- "religionA_t5"
colnames(mydat)[158] <- "militarA_t5"

colnames(mydat)[159] <- "oilB_t5"
colnames(mydat)[160] <- "allyB_t5"
colnames(mydat)[161] <- "tradeB_t5"
colnames(mydat)[162] <- "yearsB_t5"
colnames(mydat)[163] <- "govB_t5"
colnames(mydat)[164] <- "personB_t5"
colnames(mydat)[165] <- "electionsB_t5"
colnames(mydat)[166] <- "religionB_t5"
colnames(mydat)[167] <- "militarB_t5"

#table 6 conjoint
colnames(mydat)[179] <- "oilA_t6"
colnames(mydat)[180] <- "allyA_t6"
colnames(mydat)[181] <- "tradeA_t6"
colnames(mydat)[182] <- "yearsA_t6"
colnames(mydat)[183] <- "govA_t6"
colnames(mydat)[184] <- "personA_t6"
colnames(mydat)[185] <- "electionsA_t6"
colnames(mydat)[186] <- "religionA_t6"
colnames(mydat)[187] <- "militarA_t6"

colnames(mydat)[188] <- "oilB_t6"
colnames(mydat)[189] <- "allyB_t6"
colnames(mydat)[190] <- "tradeB_t6"
colnames(mydat)[191] <- "yearsB_t6"
colnames(mydat)[192] <- "govB_t6"
colnames(mydat)[193] <- "personB_t6"
colnames(mydat)[194] <- "electionsB_t6"
colnames(mydat)[195] <- "religionB_t6"
colnames(mydat)[196] <- "militarB_t6"

#Outcomes
#t1
colnames(mydat)[31] <- "attackA_t1"
colnames(mydat)[32] <- "sanctionA_t1"
colnames(mydat)[33] <- "foreignaidA_t1"
colnames(mydat)[34] <- "attackB_t1"
colnames(mydat)[35] <- "sanctionB_t1"
colnames(mydat)[36] <- "foreignaidB_t1"

#t2
colnames(mydat)[58] <- "attackA_t2"
colnames(mydat)[59] <- "sanctionA_t2"
colnames(mydat)[60] <- "foreignaidA_t2"
colnames(mydat)[61] <- "attackB_t2"
colnames(mydat)[62] <- "sanctionB_t2"
colnames(mydat)[63] <- "foreignaidB_t2"

#t3
colnames(mydat)[86] <- "attackA_t3"
colnames(mydat)[87] <- "sanctionA_t3"
colnames(mydat)[88] <- "foreignaidA_t3"
colnames(mydat)[89] <- "attackB_t3"
colnames(mydat)[90] <- "sanctionB_t3"
colnames(mydat)[91] <- "foreignaidB_t3"

#t4
colnames(mydat)[114] <- "attackA_t4"
colnames(mydat)[115] <- "sanctionA_t4"
colnames(mydat)[116] <- "foreignaidA_t4"
colnames(mydat)[117] <- "attackB_t4"
colnames(mydat)[118] <- "sanctionB_t4"
colnames(mydat)[119] <- "foreignaidB_t4"

#t5
colnames(mydat)[142] <- "attackA_t5"
colnames(mydat)[143] <- "sanctionA_t5"
colnames(mydat)[144] <- "foreignaidA_t5"
colnames(mydat)[145] <- "attackB_t5"
colnames(mydat)[146] <- "sanctionB_t5"
colnames(mydat)[147] <- "foreignaidB_t5"

#t6
colnames(mydat)[171] <- "attackA_t6"
colnames(mydat)[172] <- "sanctionA_t6"
colnames(mydat)[173] <- "foreignaidA_t6"
colnames(mydat)[174] <- "attackB_t6"
colnames(mydat)[175] <- "sanctionB_t6"
colnames(mydat)[176] <- "foreignaidB_t6"

#Create unique id
mydat$ID <- seq.int(nrow(mydat))

mydat <- as.data.frame(mydat)

# stack profiles
conj <- reshape(mydat, 
                varying = list(
                  names(mydat)[grep("^oilA", names(mydat))],
                  names(mydat)[grep("^oilB", names(mydat))],
                  names(mydat)[grep("^allyA", names(mydat))],
                  names(mydat)[grep("^allyB", names(mydat))],
                  names(mydat)[grep("^tradeA", names(mydat))],
                  names(mydat)[grep("^tradeB", names(mydat))],
                  names(mydat)[grep("^yearsA", names(mydat))],
                  names(mydat)[grep("^yearsB", names(mydat))],
                  names(mydat)[grep("^govA", names(mydat))],
                  names(mydat)[grep("^govB", names(mydat))],
                  names(mydat)[grep("^personA", names(mydat))],
                  names(mydat)[grep("^personB", names(mydat))],
                  names(mydat)[grep("^electionsA", names(mydat))],
                  names(mydat)[grep("^electionsB", names(mydat))],
                  names(mydat)[grep("^religionA", names(mydat))],
                  names(mydat)[grep("^religionB", names(mydat))],
                  names(mydat)[grep("^militarA", names(mydat))],
                  names(mydat)[grep("^militarB", names(mydat))],
                  names(mydat)[grep("^attackA", names(mydat))],
                  names(mydat)[grep("^attackB", names(mydat))],
                  names(mydat)[grep("^sanctionA", names(mydat))],
                  names(mydat)[grep("^sanctionB", names(mydat))],
                  names(mydat)[grep("^foreignaidA", names(mydat))],
                  names(mydat)[grep("^foreignaidB", names(mydat))]
                ),
                v.names = c("oilA", "oilB", 
                            "allyA", "allyB",
                            "tradeA", "tradeB",
                            "yearsA", "yearsB",
                            "govA", "govB",
                            "personA", "personB",
                            "electionsA", "electionsB",
                            "religionA", "religionB",
                            "militarA", "militarB",
                            "attackA", "attackB",
                            "sanctionA", "sanctionB",
                            "foreignaidA", "foreignaidB"),
                timevar = "pair",
                idvar = "ID",
                direction = "long"
)

## create conjoint profile display pair variable
conj[["id_pair"]] <- paste0(conj[["ID"]], "_", conj[["pair"]])


# stack a/b options
conj <- reshape(conj,
                varying = list(
                  c("oilA", "oilB"), 
                  c("allyA", "allyB"),
                  c("tradeA", "tradeB"),
                  c("yearsA", "yearsB"),
                  c("govA", "govB"),
                  c("personA", "personB"),
                  c("electionsA", "electionsB"),
                  c("religionA", "religionB"),
                  c("militarA", "militarB"),
                  c("attackA", "attackB"),
                  c("sanctionA", "sanctionB"),
                  c("foreignaidA", "foreignaidB")
                ),
                v.names = c("oil", "ally", "trade", "years", 
                            "gov", "person", "elections", "religion",
                            "militar", "attack", "sanction", "foreignaid"),
                timevar = "AB",
                idvar = "id_pair",
                direction = "long"
)

conj[["AB"]] <- c("A", "B")[conj[["AB"]]]


# clean labeled factors
#install.packages("dplyr")
#install.packages("labelled")
library("dplyr")
library("labelled")

#clean outcomes
#1 Yes 0 No (we need to reverse order)
conj[["attack"]] <- ifelse(conj[["attack"]] == 2, 1, 0)
conj[["sanction"]] <- ifelse(conj[["sanction"]] == 2, 1, 0)
conj[["foreignaid"]] <- ifelse(conj[["foreignaid"]] == 2, 1, 0)


#oil
conj$oil <- to_factor(conj$oil)

#ally
conj$ally <- to_factor(conj$ally)

#trade
conj$trade <- to_factor(conj$trade)

#years
conj$years <- to_factor(conj$years)
conj$years <- relevel(conj$years, ref="4 years")

#government
conj$gov <- to_factor(conj$gov)

#person
conj$person <- to_factor(conj$person)
conj$person <- relevel(conj$person, ref="A civilian who heads the regime's official party")

#elections
conj$elections <- to_factor(conj$elections)

#religion
conj$religion <- to_factor(conj$religion)
conj$religion <- relevel(conj$religion, ref="Christian")

#militar
conj$militar <- to_factor(conj$militar)
conj$militar <- relevel(conj$militar, ref="Weak")

#ideology
conj$ideol[conj$left_right_8<4] <- 1
conj$ideol[conj$left_right_8==4] <- 2
conj$ideol[conj$left_right_8>4] <- 3
conj$ideol <- as.factor(conj$ideol)
conj$ideol <- recode_factor(conj$ideol , 
                            `1` = "Conservative", 
                            `2` = "Moderate", 
                            `3` = "Liberal")


# EXPORT DATA
#install.packages("rio")
library("rio")
rio::export(conj, "conjoint-data-stacked.rds")
