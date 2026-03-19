###Q###Q###Q###Q###Q###Q###Q###Q
###Q###Q   Preprocess   ###Q###Q
###Q###Q###Q###Q###Q###Q###Q###Q

## load packages ====
# R version 4.2.3 (2023-03-15)
library(here) # v1.0.1
library(tidyverse) # v2.0.0

## read data ====
dtaDE <- read.csv(here("data", "KSFR23_DEsurvey_raw_narrow.csv"), header=T)
dtaIT <- read.csv(here("data", "KSFR23_ITsurvey_raw_narrow.csv"), header=T)

## filter observations ====
# not spam
dtaDE <- dtaDE[dtaDE$Status == "IP Address",]
dtaIT <- dtaIT[dtaIT$Status == "IP Address",]
c(nrow(dtaDE), nrow(dtaIT))

# finished
dtaDE <- dtaDE[dtaDE$Finished=="True", ]
dtaIT <- dtaIT[dtaIT$Finished=="True", ]
c(nrow(dtaDE), nrow(dtaIT))

# agree to participate
dtaDE <- dtaDE[dtaDE$Q2 == "Zustimmen" , ]
dtaIT <- dtaIT[dtaIT$Q2 == "Acconsento" , ]
c(nrow(dtaDE), nrow(dtaIT))

# between 4min & 4h to complete survey
dtaDE <- dtaDE[between(as.numeric(dtaDE$Duration..in.seconds.), 240, 14400),]
dtaIT <- dtaIT[between(as.numeric(dtaIT$Duration..in.seconds.), 240, 14400),]
c(nrow(dtaDE), nrow(dtaIT))

# pass attention check
dtaDE <- dtaDE[dtaDE$Q53_7 == "Weiß nicht" , ]
dtaIT <- dtaIT[dtaIT$Q50_7 == "Non so" , ]
c(nrow(dtaDE), nrow(dtaIT))

## obtain variables ====
# gender, age, region
dtaDE = dtaDE %>%
  mutate(MaleFemale = ifelse(dtaDE$Q62 == "Männlich" , 1 , 
                             ifelse(dtaDE$Q62 == "Weiblich" , 0 , NA)),
         Age = factor(dtaDE$Q63, 
                      levels = c("18-24 Jahre alt.", "25-34 Jahre alt.", 
                                 "35-49 Jahre alt.", "50-64 Jahre alt.", 
                                 "65+ Jahre alt."),
                      labels = as.character(1:5)),
         Region = na_if(dtaDE$Q64, "-99"))

dtaIT = dtaIT %>%
  mutate(MaleFemale = ifelse(dtaIT$Q66 == "Maschio" , 1 , 
                             ifelse(dtaIT$Q66 == "Femmina" , 0 , NA)),
         Age = factor(dtaIT$Q67, 
                      levels = c("Tra i 18 e i 24 anni.", "Tra i 25 e i 34 anni.", 
                                 "Tra i 35 e i 49 anni.", "Tra i 50 e i 64 anni.", 
                                 "Più di 65 anni."),
                      labels = as.character(1:5)),
         Region = str_replace(dtaIT$Q68, ' \\(.+\\)', '') %>%
           na_if(., "-99"))

# party support (i.e. preferred party)
dtaDE$PartySupport <- NA
dtaDE$PartySupport <- ifelse(dtaDE$Q7_1 == 1, "SPD", dtaDE$PartySupport)
dtaDE$PartySupport <- ifelse(dtaDE$Q7_2 == 1, "AfD", dtaDE$PartySupport)
dtaDE$PartySupport <- ifelse(dtaDE$Q7_3 == 1, "FDP", dtaDE$PartySupport)
dtaDE$PartySupport <- ifelse(dtaDE$Q7_4 == 1, "LIN", dtaDE$PartySupport)
dtaDE$PartySupport <- ifelse(dtaDE$Q7_5 == 1, "GRU", dtaDE$PartySupport)
dtaDE$PartySupport <- ifelse(dtaDE$Q7_6 == 1, "CDU", dtaDE$PartySupport)

dtaIT$PartySupport <- NA
dtaIT$PartySupport <- ifelse(dtaIT$Q6_1 == 1, "M5S", dtaIT$PartySupport)
dtaIT$PartySupport <- ifelse(dtaIT$Q6_2 == 1, "Lega", dtaIT$PartySupport)
dtaIT$PartySupport <- ifelse(dtaIT$Q6_3 == 1, "LeU", dtaIT$PartySupport)
dtaIT$PartySupport <- ifelse(dtaIT$Q6_4 == 1, "PD", dtaIT$PartySupport)
dtaIT$PartySupport <- ifelse(dtaIT$Q6_5 == 1, "FdI", dtaIT$PartySupport)
dtaIT$PartySupport <- ifelse(dtaIT$Q6_6 == 1, "IV", dtaIT$PartySupport)
dtaIT$PartySupport <- ifelse(dtaIT$Q6_7 == 1, "FI", dtaIT$PartySupport)

## pre-treatment attachment to preferred party
dtaDE = dtaDE %>%
  mutate(PartyAttach = na_if(Q8, "-99") %>%
           str_extract(., "\\d{1,2}") %>% as.numeric())

dtaIT = dtaIT %>%
  mutate(PartyAttach = na_if(Q7, "-99") %>%
           str_extract(., "\\d{1,2}") %>% as.numeric())

## post-treatment turnout
dtaDE = dtaDE %>% 
  mutate(turnout = coalesce(na_if(Q50, ""), 
                            na_if(Q38, ""),
                            na_if(Q26, "")) %>%
           str_replace(., '[[:blank:]].*$', '') %>%
           na_if(., '-99') %>% 
           as.numeric())

dtaIT = dtaIT %>% 
  mutate(turnout = coalesce(na_if(Q47, ""), 
                            na_if(Q35, ""),
                            na_if(Q23, "")) %>%
           str_replace(., '[[:blank:]].*$', '') %>%
           na_if(., '-99') %>% 
           as.numeric())

## post-treatment likelihood to vote for pre-treatment preferred party
dtaDE = dtaDE %>% 
  mutate(sticky = coalesce(na_if(Q51, ""), 
                           na_if(Q39, ""),
                           na_if(Q27, "")) %>%
           str_replace(., '[[:blank:]].*$', '') %>%
           na_if(., '-99') %>% 
           as.numeric())

dtaIT = dtaIT %>% #rowwise() %>% 
  mutate(sticky = coalesce(na_if(Q48, ""), 
                           na_if(Q36, ""),
                           na_if(Q24, "")) %>%
           str_replace(., '[[:blank:]].*$', '') %>%
           na_if(., '-99') %>% 
           as.numeric())

# EU knowledge questions
dtaDE = dtaDE %>% rowwise() %>%
  mutate(EUinfo1 = na_if(Q13_1, "-99") %>% {if_else(. == "Falsch", 1 , 0)},
         EUinfo2 = na_if(Q13_2, "-99") %>% {if_else(. == "Falsch", 1 , 0)},
         EUinfo3 = na_if(Q13_3, "-99") %>% {if_else(. == "Wahr"  , 1 , 0)},
         EUinfo4 = na_if(Q13_4, "-99") %>% {if_else(. == "Wahr"  , 1 , 0)},
         EUinfo5 = na_if(Q13_5, "-99") %>% {if_else(. == "Falsch", 1 , 0)},
         EUinfo6 = na_if(Q13_6, "-99") %>% {if_else(. == "Wahr"  , 1 , 0)},
         EUinfo7 = na_if(Q13_7, "-99") %>% {if_else(. == "Falsch", 1 , 0)},
         EUinfo = EUinfo1 + EUinfo2 + EUinfo3 + EUinfo4 + EUinfo5 + EUinfo6 + EUinfo7,
         EUinfo_bin = ifelse(EUinfo > 5, 'High', 'Low'),
         EUinfo_ter = ifelse(EUinfo <= 3, 'Low', 
                             ifelse(EUinfo <= 5, 'Mid', 'High')))
dtaIT = dtaIT %>%
  mutate(EUinfo1 = na_if(Q12_1, "-99") %>% {if_else(. == "Falso", 1 , 0)},
         EUinfo2 = na_if(Q12_2, "-99") %>% {if_else(. == "Falso", 1 , 0)},
         EUinfo3 = na_if(Q12_3, "-99") %>% {if_else(. == "Vero" , 1 , 0)},
         EUinfo4 = na_if(Q12_4, "-99") %>% {if_else(. == "Vero" , 1 , 0)},
         EUinfo5 = na_if(Q12_5, "-99") %>% {if_else(. == "Falso", 1 , 0)},
         EUinfo6 = na_if(Q12_6, "-99") %>% {if_else(. == "Vero" , 1 , 0)},
         EUinfo7 = na_if(Q12_7, "-99") %>% {if_else(. == "Falso", 1 , 0)},
         EUinfo = EUinfo1 + EUinfo2 + EUinfo3 + EUinfo4 + EUinfo5 + EUinfo6 + EUinfo7,
         EUinfo_bin = ifelse(EUinfo > 5, 'High', 'Low'),
         EUinfo_ter = ifelse(EUinfo <= 3, 'Low', 
                              ifelse(EUinfo <= 5, 'Mid', 'High')))


# EU Europarty knowledge
dtaDE$EupartyCorrect <- 'false'
dtaDE$EupartyCorrect <- ifelse(dtaDE$PartySupport=="CDU" &
                                 dtaDE$Q15 == "Europäische Volkspartei" , 'true' , dtaDE$EupartyCorrect )
dtaDE$EupartyCorrect <- ifelse(dtaDE$PartySupport=="SPD" & 
                                 dtaDE$Q15 == "Sozialdemokratische Partei Europas" , 'true' , dtaDE$EupartyCorrect)
dtaDE$EupartyCorrect <- ifelse(dtaDE$PartySupport=="AfD" &
                                 dtaDE$Q15 %in% c("Identität und Demokratie Partei", "Fraktionslos"), 'true' , dtaDE$EupartyCorrect) # technically not member of ID, but member of EP group
dtaDE$EupartyCorrect <- ifelse(dtaDE$PartySupport=="FDP" & 
                                 dtaDE$Q15 == "Allianz der Liberalen und Demokraten für Europa" , 'true' , dtaDE$EupartyCorrect)
dtaDE$EupartyCorrect <- ifelse(dtaDE$PartySupport=="LIN" & 
                                 dtaDE$Q15 == "Partei der Europäischen Linken" , 'true' , dtaDE$EupartyCorrect)
dtaDE$EupartyCorrect <- ifelse(dtaDE$PartySupport=="GRU" & 
                                 dtaDE$Q15 == "Europäische Grüne Partei" , 'true' , dtaDE$EupartyCorrect)
dtaDE$EupartyCorrect <- ifelse(dtaDE$Q15 == "Weiß nicht" , 'DK' , dtaDE$EupartyCorrect)
dtaDE$EupartyCorrect <- ifelse(dtaDE$Q15 == "-99" , NA , dtaDE$EupartyCorrect)

dtaIT$EupartyCorrect <- 'false'
dtaIT$EupartyCorrect <- ifelse(dtaIT$PartySupport=="PD" & 
                                 dtaIT$Q14 == "Partito del Socialismo Europeo (Party of European Socialists)", 'true' , dtaIT$EupartyCorrect)
dtaIT$EupartyCorrect <- ifelse(dtaIT$PartySupport=="M5S" & 
                                 dtaIT$Q14 == "Senza affiliazione " , 'true' , dtaIT$EupartyCorrect)# blank space sic! 
dtaIT$EupartyCorrect <- ifelse(dtaIT$PartySupport=="Lega" & 
                                 dtaIT$Q14 == "Partito Identità e Democrazia (Identity and Democracy Party)", 'true' , dtaIT$EupartyCorrect) 
dtaIT$EupartyCorrect <- ifelse(dtaIT$PartySupport=="FdI" & 
                                 dtaIT$Q14 == "Conservatori e Riformisti Europei (European Conservatives and Reformists Party)", 'true' , dtaIT$EupartyCorrect)
dtaIT$EupartyCorrect <- ifelse(dtaIT$PartySupport=="IV" & 
                                 dtaIT$Q14 %in% c("Partito dell'Alleanza dei Liberali e dei Democratici per l'Europa (Alliance of Liberals and Democrats for Europe Party)", "Senza affiliazione "), 'true' , dtaIT$EupartyCorrect) # technically not member of ALDE, but member of renew group
dtaIT$EupartyCorrect <- ifelse(dtaIT$PartySupport=="FI" & 
                                 dtaIT$Q14 == "Partito Popolare Europeo (European People’s Party)", 'true' , dtaIT$EupartyCorrect)
dtaIT$EupartyCorrect <- ifelse(dtaIT$PartySupport=="LeU" & 
                                 dtaIT$Q14 %in% c("Partito del Socialismo Europeo (Party of European Socialists)", "Partito della Sinistra Europea (Party of the European Left)"), 'true' , dtaIT$EupartyCorrect)
dtaIT$EupartyCorrect <- ifelse(dtaIT$Q14 == "Non so", 'DK' , dtaIT$EupartyCorrect)
dtaIT$EupartyCorrect <- ifelse(dtaIT$Q14 == "-99" , NA , dtaIT$EupartyCorrect)

## treatment-level 
# naming: (1)(2)(3) 
#  1: Party national or foreign, 
#  2: Election national or EP, 
#  3: cooperation with xenophobic, left, autocratic or control party
# all 3*4 = 12 possible combinations
dtaDE$TreatIND <- NA
dtaDE$TreatIND[dtaDE$Scenario1_DO == "Q16|Q18|Q19|Q23|Q24|Q59|Q25|Q26|Q27|Q28"] <- "NatNatXeno"
dtaDE$TreatIND[dtaDE$Scenario1_DO == "Q16|Q18|Q20|Q23|Q24|Q59|Q25|Q26|Q27|Q28"] <- "NatNatLeft"
dtaDE$TreatIND[dtaDE$Scenario1_DO == "Q16|Q18|Q21|Q23|Q24|Q59|Q25|Q26|Q27|Q28"] <- "NatNatAuto"
dtaDE$TreatIND[dtaDE$Scenario1_DO == "Q16|Q18|Q22|Q23|Q24|Q59|Q25|Q26|Q27|Q28"] <- "NatNatCont"
dtaDE$TreatIND[dtaDE$Scenario2_DO == "Q29|Q30|Q31|Q35|Q36|Q60|Q37|Q38|Q39|Q40"] <- "NatEPXeno"
dtaDE$TreatIND[dtaDE$Scenario2_DO == "Q29|Q30|Q32|Q35|Q36|Q60|Q37|Q38|Q39|Q40"] <- "NatEPLeft"
dtaDE$TreatIND[dtaDE$Scenario2_DO == "Q29|Q30|Q33|Q35|Q36|Q60|Q37|Q38|Q39|Q40"] <- "NatEPAuto"
dtaDE$TreatIND[dtaDE$Scenario2_DO == "Q29|Q30|Q34|Q35|Q36|Q60|Q37|Q38|Q39|Q40"] <- "NatEPCont"
dtaDE$TreatIND[dtaDE$Scenario3_DO == "Q41|Q42|Q43|Q47|Q48|Q61|Q49|Q50|Q51|Q52"] <- "ForEPXeno"
dtaDE$TreatIND[dtaDE$Scenario3_DO == "Q41|Q42|Q44|Q47|Q48|Q61|Q49|Q50|Q51|Q52"] <- "ForEPLeft"
dtaDE$TreatIND[dtaDE$Scenario3_DO == "Q41|Q42|Q45|Q47|Q48|Q61|Q49|Q50|Q51|Q52"] <- "ForEPAuto"
dtaDE$TreatIND[dtaDE$Scenario3_DO == "Q41|Q42|Q46|Q47|Q48|Q61|Q49|Q50|Q51|Q52"] <- "ForEPCont"
dtaDE$TreatIND <- as.factor(dtaDE$TreatIND)

dtaIT$TreatIND <- NA
dtaIT$TreatIND[dtaIT$Scenario1_DO == "Q15|Q16|Q21|Q20|Q21|Q22|Q23|Q24|Q25|Q64"] <- "NatNatXeno"
dtaIT$TreatIND[dtaIT$Scenario1_DO == "Q15|Q16|Q17|Q20|Q21|Q22|Q23|Q24|Q25|Q64"] <- "NatNatLeft"
dtaIT$TreatIND[dtaIT$Scenario1_DO == "Q15|Q16|Q18|Q20|Q21|Q22|Q23|Q24|Q25|Q64"] <- "NatNatAuto"
dtaIT$TreatIND[dtaIT$Scenario1_DO == "Q15|Q16|Q19|Q20|Q21|Q22|Q23|Q24|Q25|Q64"] <- "NatNatCont"
dtaIT$TreatIND[dtaIT$Scenario2_DO == "Q26|Q27|Q28|Q32|Q33|Q62|Q34|Q35|Q36|Q37"] <- "NatEPXeno"
dtaIT$TreatIND[dtaIT$Scenario2_DO == "Q26|Q27|Q29|Q32|Q33|Q62|Q34|Q35|Q36|Q37"] <- "NatEPLeft"
dtaIT$TreatIND[dtaIT$Scenario2_DO == "Q26|Q27|Q30|Q32|Q33|Q62|Q34|Q35|Q36|Q37"] <- "NatEPAuto"
dtaIT$TreatIND[dtaIT$Scenario2_DO == "Q26|Q27|Q31|Q32|Q33|Q62|Q34|Q35|Q36|Q37"] <- "NatEPCont"
dtaIT$TreatIND[dtaIT$Scenario3_DO == "Q38|Q39|Q40|Q44|Q45|Q63|Q46|Q47|Q48|Q49"] <- "ForEPXeno"
dtaIT$TreatIND[dtaIT$Scenario3_DO == "Q38|Q39|Q41|Q44|Q45|Q63|Q46|Q47|Q48|Q49"] <- "ForEPLeft"
dtaIT$TreatIND[dtaIT$Scenario3_DO == "Q38|Q39|Q42|Q44|Q45|Q63|Q46|Q47|Q48|Q49"] <- "ForEPAuto"
dtaIT$TreatIND[dtaIT$Scenario3_DO == "Q38|Q39|Q43|Q44|Q45|Q63|Q46|Q47|Q48|Q49"] <- "ForEPCont"
dtaIT$TreatIND <- as.factor(dtaIT$TreatIND)

# split level and treatment variables
dtaDE$level = factor(str_extract(as.character(dtaDE$TreatIND), "NatNat|NatEP|ForEP"))
dtaDE$level = relevel(dtaDE$level, ref = 'NatNat')
dtaDE$treat = factor(str_extract(as.character(dtaDE$TreatIND), 'Xeno|Auto|Left|Cont'))
dtaDE$treat = relevel(dtaDE$treat, ref = 'Cont')

dtaIT$level = factor(str_extract(as.character(dtaIT$TreatIND), "NatNat|NatEP|ForEP"))
dtaIT$level = relevel(dtaIT$level, ref = 'NatNat')
dtaIT$treat = factor(str_extract(as.character(dtaIT$TreatIND), 'Xeno|Auto|Left|Cont'))
dtaIT$treat = relevel(dtaIT$treat, ref = 'Cont')

# support for collaboration & change in party attachment
dtaDE = dtaDE %>% 
  mutate(SuppCollab = coalesce(na_if(Q48, ""), 
                               na_if(Q36, ""),
                               na_if(Q24, "")) %>%
           str_replace(., '[[:blank:]].*$', '') %>%
           na_if(., '-99') %>% 
           as.numeric(),
         PartyAttachOut = coalesce(na_if(Q49, ""), 
                                   na_if(Q37, ""),
                                   na_if(Q25, "")) %>%
           str_replace(., '[[:blank:]].*$', '') %>%
           na_if(., '-99') %>% 
           as.numeric(),
         PartyAttachChange = PartyAttachOut - PartyAttach)

dtaIT = dtaIT %>%
  mutate(SuppCollab = coalesce(na_if(Q45, ""), 
                               na_if(Q33, ""),
                               na_if(Q21, "")) %>%
           str_replace(., '[[:blank:]].*$', '') %>%
           na_if(., '-99') %>% 
           as.numeric(),
         PartyAttachOut = coalesce(na_if(Q46, ""), 
                                   na_if(Q34, ""),
                                   na_if(Q22, "")) %>%
           str_replace(., '[[:blank:]].*$', '') %>%
           na_if(., '-99') %>% 
           as.numeric(),
         PartyAttachChange = PartyAttachOut - PartyAttach)

# resembles real party
dtaDE = dtaDE %>% mutate(reminds = na_if(Q56, "") %>% na_if(., "-99") %>% {if_else(. == "Ja", 1, 0)})

dtaIT = dtaIT %>% mutate(reminds = na_if(Q53, "") %>% na_if(., "-99") %>% {if_else(. == "Sì", 1, 0)})


## Manipulation check variables ----
levsDE = c("Wahrscheinlich", "Weder unwahrscheinlich noch wahrscheinlich",
           "Unwahrscheinlich", "Weiß nicht")
levsIT = c("Probabile", "Né improbabile, né probabile",
           "Improbabile", "Non so")
labs = c("high", "neither",
         "low", "DK")

# policy attribution, LR placement for new party, likelihood of collaboration
dtaDE = dtaDE %>%
  mutate(manip_policy_taxrich = na_if(Q53_1, "-99") %>% factor(., levels = levsDE, labels = labs),
         manip_policy_reportimmigrant = na_if(Q53_2, "-99") %>% factor(., levels = levsDE, labels = labs),
         manip_policy_punishfake = na_if(Q53_3, "-99") %>% factor(., levels = levsDE, labels = labs),
         manip_policy_planepollution = na_if(Q53_4, "-99") %>% factor(., levels = levsDE, labels = labs),
         manip_policy_taxinherit = na_if(Q53_5, "-99") %>% factor(., levels = levsDE, labels = labs),
         manip_policy_school = na_if(Q53_6, "-99") %>% factor(., levels = levsDE, labels = labs),
         manip_policy_naturalization = na_if(Q53_8, "-99") %>% factor(., levels = levsDE, labels = labs),
         manip_policy_punishjournal = na_if(Q53_9, "-99") %>% factor(., levels = levsDE, labels = labs),
         manip_LR = na_if(Q54, "-99") %>% str_extract("\\d{1,2}") %>% as.numeric(),
         manip_collablikely = na_if(Q58, "-99") %>% factor(., 
                                                           levels = c("Sehr wahrscheinlich", "Eher wahrscheinlich", 
                                                                      "Eher unwahrscheinlich",  "Sehr unwahrscheinlich"),
                                                           labels = c('VeryHigh', 'High', 'Low', 'VeryLow')))

dtaIT = dtaIT %>%
  mutate(manip_policy_taxrich = na_if(Q50_1, "-99") %>% factor(., levels = levsIT, labels = labs),
         manip_policy_reportimmigrant = na_if(Q50_2, "-99") %>% factor(., levels = levsIT, labels = labs),
         manip_policy_punishfake = na_if(Q50_3, "-99") %>% factor(., levels = levsIT, labels = labs),
         manip_policy_planepollution = na_if(Q50_4, "-99") %>% factor(., levels = levsIT, labels = labs),
         manip_policy_taxinherit = na_if(Q50_5, "-99") %>% factor(., levels = levsIT, labels = labs),
         manip_policy_school = na_if(Q50_6, "-99") %>% factor(., levels = levsIT, labels = labs),
         manip_policy_naturalization = na_if(Q50_8, "-99") %>% factor(., levels = levsIT, labels = labs),
         manip_policy_punishjournal = na_if(Q50_9, "-99") %>% factor(., levels = levsIT, labels = labs),
         manip_LR = na_if(Q51, "-99") %>% str_extract("\\d{1,2}") %>% as.numeric(),
         manip_collablikely = na_if(Q55, "-99") %>% factor(., 
                                                           levels = c("Molto probabile ", "Abbastanza probabile ",   
                                                                      "Abbastanza improbabile ", "Molto improbabile"),
                                                           labels = c('VeryHigh', 'High', 'Low', 'VeryLow')))
rm(levsDE, levsIT, labs)

# treatment resembles real party?
manip_resemblesDE = dtaDE$Q57
manip_resemblesIT = dtaIT$Q54

# descriptive adjectives
manip_descriptorsDE = dtaDE$Q55
manip_descriptorsIT = dtaIT$Q52

## Combine samples ====
dtaDE = dtaDE %>% dplyr::select("MaleFemale", "Age", "Region" ,"PartySupport",
                                "PartyAttach", "turnout","sticky", "EUinfo1", 
                                "EUinfo2","EUinfo3", "EUinfo4", "EUinfo5", "EUinfo6",
                                "EUinfo7", "EUinfo", "EUinfo_bin", "EUinfo_ter", 
                                "EupartyCorrect", 
                                "TreatIND", "level", "treat",
                                "SuppCollab", "PartyAttachOut", "PartyAttachChange",
                                "reminds", 
                                "manip_policy_taxrich", "manip_policy_reportimmigrant",
                                "manip_policy_punishfake", "manip_policy_planepollution",
                                "manip_policy_taxinherit", "manip_policy_school",
                                "manip_policy_naturalization", "manip_policy_punishjournal",
                                "manip_LR", "manip_collablikely"
                                ) %>%
  mutate(Country = 'DE')
dtaIT = dtaIT %>% dplyr::select("MaleFemale", "Age", "Region" ,"PartySupport",
                                "PartyAttach", "turnout","sticky", "EUinfo1", 
                                "EUinfo2","EUinfo3", "EUinfo4", "EUinfo5", "EUinfo6",
                                "EUinfo7", "EUinfo", "EUinfo_bin", "EUinfo_ter", 
                                "EupartyCorrect", 
                                "TreatIND", "level", "treat",
                                "SuppCollab", "PartyAttachOut", "PartyAttachChange",
                                "reminds",
                                "manip_policy_taxrich", "manip_policy_reportimmigrant",
                                "manip_policy_punishfake", "manip_policy_planepollution",
                                "manip_policy_taxinherit", "manip_policy_school",
                                "manip_policy_naturalization", "manip_policy_punishjournal",
                                "manip_LR", "manip_collablikely"
                                ) %>%
  mutate(Country = 'IT')
dta = bind_rows(dtaDE, dtaIT)

## save cleaned data sets ====
# DOES NOT OVERRIDE EXISTING DATA SET!
if(!file.exists(here("data", "KSFR23_data_clean.Rdata"))){
  save(dta, dtaDE, dtaIT, file = here("data", "KSFR23_data_clean.Rdata"))
}

if(!file.exists(here("data", "KSFR23_manipulation_checks.Rdata"))){
  save(manip_descriptorsDE,manip_descriptorsIT,manip_resemblesDE,manip_resemblesIT, file = here("data", "KSFR23_manipulation_checks.Rdata"))
}


