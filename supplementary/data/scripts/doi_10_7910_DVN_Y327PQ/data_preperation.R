#The following data preperation was carried out using R version 4.3.0 
require(tidyverse) #tidyverse version 2.0.0
#setwd()
rm(list = ls())

electiondates<- c(#General elections dates
as.Date("18-02-2005", format = "%d-%m-%Y"), 
as.Date("13-11-2007", format = "%d-%m-%Y"), 
as.Date("15-09-2011", format = "%d-%m-%Y"),
as.Date("18-06-2015", format = "%d-%m-%Y"),
as.Date("05-06-2019", format = "%d-%m-%Y"),
#Local election dates
as.Date("15-11-2005", format = "%d-%m-%Y"),
as.Date("17-11-2009", format = "%d-%m-%Y"),
as.Date("19-11-2013", format = "%d-%m-%Y"),
as.Date("21-11-2017", format = "%d-%m-%Y"))


#Election data#### 
#Retrived from https://valgdatabase.dst.dk/ 
valgd <- read_csv2("Valgdata.csv", na = c("-"), col_types = paste("d", "c", paste( rep("d", 627), collapse=""), sep =""), locale=locale(encoding = "ISO-8859-1")) 

#Make long format and create identifying names
valgd <- valgd %>% 
  pivot_longer(cols = 6:629, names_to = "variable", values_to = "votes") %>%
  mutate(
    election=str_sub(variable, 1,2),
    year=str_sub(variable, 3,6),
    variable=str_sub(variable, 10, 50),
    komnr=as.numeric(str_sub(Gruppe, 1,3)),
  ) %>% 
  pivot_wider(
    names_from = variable, values_from = votes
  ) 
#Remove regional elections
valgd <- subset(valgd, election != "RV")
#Remove elections prior to 2007
valgd <- subset(valgd, year >=2007)

#Finding the votes for locallists in local elections (changing list and party names)
kmvalg <- subset(valgd, election == "KV") %>% 
  group_by(year, Gruppe) %>%   
  summarise(`D. Centrum-Demokraterne`= sum(ifelse(year==2017, NA, `D. Centrum-Demokraterne`)),
            `I. Liberal Alliance`= sum(ifelse(year<2007, `I. Liberal Alliance`, NA)), 
            J = sum(J),
            N = sum(N),
            G = sum(G),
            S = sum(S),
            H = sum(H),
            U = sum(U),
            Z = sum(Z),
            L = sum(L),
            R = sum(R),
            K2 = sum(K2),
            W = sum(W),
            `T` = sum(`T`),
            K1 = sum(K1),
            Å1 = sum(Å1),
            Æ = sum(Æ),
            Q = sum(Q),
            `M. Minoritetspartiet`= sum(`M. Minoritetspartiet`),
            `Y. Ny Alliance`=sum(`Y. Ny Alliance`),
            `Å. Alternativet`= sum(ifelse(year<2014, `Å. Alternativet`, NA)), 
            `1. Uden for partierne`= sum(`1. Uden for partierne`),
            `2. Uden for partierne`= sum(`2. Uden for partierne`),
            `3. Uden for partierne`= sum(`3. Uden for partierne`),
            `4. Uden for partierne`= sum(`4. Uden for partierne`),
            `5. Uden for partierne`= sum(`5. Uden for partierne`),
            `6. Uden for partierne`= sum(`6. Uden for partierne`),
            `P. Stram Kurs`=sum(`P. Stram Kurs`),
            `E. Klaus Riskær Pedersen`=sum(`E. Klaus Riskær Pedersen`),
            `04`=sum(`04`),
            `05`=sum(`05`),
            `03`=sum(`03`),
            `01`=sum(`01`),
            `02`=sum(`02`),
            `06`=sum(`06`),
            `06`=sum(`06`)) %>% 
  pivot_longer(3:37) %>% 
  group_by(year, Gruppe) %>%   
  summarise(Lokalliste=sum(value, na.rm =T)) %>% 
  mutate(election="KV")

##Cleaning up data for changing party names and list names####
valgd <- valgd %>%
  mutate(`D. Nye Borgerlige` =ifelse(election=="FV" & year==2019,`D. Centrum-Demokraterne`, 
                                     ifelse(election %in% c("KV", "RV") & year==2017, `D. Centrum-Demokraterne`, NA)),
         `D. Nye Borgerlige`=ifelse(is.na(`D. Nye Borgerlige`), 0, `D. Nye Borgerlige`),
         `P. Stram Kurs`=ifelse(election=="FV" & year==2019, `P. Stram Kurs`, NA),
         `P. Stram Kurs`=ifelse(is.na(`P. Stram Kurs`), 0, `P. Stram Kurs`),
         `C. Det Konservative Folkeparti`=ifelse(is.na(`C. Det Konservative Folkeparti`), 0, `C. Det Konservative Folkeparti`),
         `Ø. Enhedslisten - De Rød-Grønne`=ifelse(is.na(`Ø. Enhedslisten - De Rød-Grønne`), 0, `Ø. Enhedslisten - De Rød-Grønne`),
         `F. SF - Socialistisk Folkeparti`=ifelse(election=="KV" & year==2005, `F. Socialistisk Folkeparti`, `F. SF - Socialistisk Folkeparti`),
         `F. SF - Socialistisk Folkeparti`=ifelse(is.na(`F. SF - Socialistisk Folkeparti`), 0, `F. SF - Socialistisk Folkeparti`),
         `B. Det Radikale Venstre`=ifelse(is.na(`B. Det Radikale Venstre`), 0, `B. Det Radikale Venstre`),
         `I. Liberal Alliance`=ifelse(year<2007, 0, ifelse(year==2007, `Y. Ny Alliance`, ifelse(year>2007, `I. Liberal Alliance`, NA))),
         `I. Liberal Alliance`=ifelse(is.na(`I. Liberal Alliance`), 0, `I. Liberal Alliance`),
         `O. Dansk Folkeparti`=ifelse(is.na(`O. Dansk Folkeparti`), 0, `O. Dansk Folkeparti`)
         
  ) %>% 
  left_join(kmvalg, by = c("year" = "year", "Gruppe" = "Gruppe", "election" = "election")) %>% #add local lists votes
  mutate(afstemid=Gruppe, #include government coalitions, change to short english variable names, and select relevant variables.
         komnr=as.numeric(str_sub(Gruppe, 1,3)),
         election=election,
         year=as.numeric(year),
         vk.votes = `V. Venstre, Danmarks Liberale Parti`+`C. Det Konservative Folkeparti`,
         vkla.votes = `V. Venstre, Danmarks Liberale Parti`+`C. Det Konservative Folkeparti` + `I. Liberal Alliance`,
         srsf.votes = `A. Socialdemokratiet`+`B. Det Radikale Venstre`+`F. SF - Socialistisk Folkeparti`,
         rwp.votes=`P. Stram Kurs`+`D. Nye Borgerlige`+`O. Dansk Folkeparti`,
         socdem.votes=`A. Socialdemokratiet`,
         lib.votes=`V. Venstre, Danmarks Liberale Parti`,
         rlib.votes=`B. Det Radikale Venstre`,
         spp.votes=`F. SF - Socialistisk Folkeparti`,
         ul.votes=`Ø. Enhedslisten - De Rød-Grønne`,
         cons.votes=`C. Det Konservative Folkeparti`,
         locl.votes=Lokalliste,
         total.votes=`Afgivne stemmer`,
         blank.votes=`Blanke stemmer`,
         other_invalid.votes=`Andre ugyldige stemmer`,
         eligible.votes=`Stemmeberettigede`,
         valid.votes=`Gyldige stemmer`
         ) %>% 
  select(election, year, komnr, afstemid, vk.votes, vkla.votes, srsf.votes, rwp.votes, socdem.votes, lib.votes, rlib.votes, spp.votes, ul.votes, cons.votes,locl.votes, total.votes, blank.votes, other_invalid.votes, eligible.votes, valid.votes) 
rm(kmvalg)

##Add additional demographic covariates at the zip-code level####
zipdata<- read_csv("zipdata.csv")
valgd <- valgd %>% 
  left_join(zipdata, by = c("afstemid" = "afstemid", "year" = "year")) 
rm(zipdata)

#Make variable that determines which parties are incumbents in the election#
##Incumbents in National elections####
valgd <- valgd %>% 
  mutate(incumbent = ifelse(year==2005 & election=="FV", "VK", 
                            ifelse(year==2007 & election=="FV", "VK",
                                   ifelse(year==2011 & election=="FV", "VK",
                                          ifelse(year==2015 & election=="FV", "SRSF",
                                                 ifelse(year==2019 & election=="FV", "VKLA", NA))))))
         
         

table(valgd$year, valgd$incumbent)

##Incumbents in Local elections####
#Dataset on mayors from: Kjaer, Ulrik, and Niels Opstrup. 2018. "Danske Borgmestre 1970-2018." Kommunalpolitiske Studier 33.
mayor <- read_csv("mayor.csv")

mayor %>% #
  mutate(b2005=ifelse(Periode_Start<=2005 & Periode_Slut>=2005 | Periode_Start<=2005 &  is.na(Periode_Slut), T,F),
         b2009=ifelse(Periode_Start<=2009 & Periode_Slut>=2009 | Periode_Start<=2009 &  is.na(Periode_Slut), T,F),
         b2013=ifelse(Periode_Start<=2013 & Periode_Slut>=2013 | Periode_Start<=2013 &  is.na(Periode_Slut), T,F),
         b2017=ifelse(Periode_Start<=2017 & Periode_Slut>=2017 | Periode_Start<=2017 &  is.na(Periode_Slut), T,F),
         b2021=ifelse(Periode_Start<=2021 & Periode_Slut>=2021 | Periode_Start<=2021 &  is.na(Periode_Slut), T,F),
         w2005=ifelse(Periode_Start<=2006 & Periode_Slut>2006 | Periode_Start<=2006 & is.na(Periode_Slut), T,F),
         w2009=ifelse(Periode_Start<=2010 & Periode_Slut>2010 | Periode_Start<=2010 & is.na(Periode_Slut), T,F),
         w2013=ifelse(Periode_Start<=2014 & Periode_Slut>2014 | Periode_Start<=2014 & is.na(Periode_Slut), T,F),
         w2017=ifelse(Periode_Start<=2018 & Periode_Slut>2018 | Periode_Start<=2018 & is.na(Periode_Slut), T,F)
         )%>%
  {.} -> mayor

mayor %>% 
  subset(b2009==T) %>% 
  group_by(Komnr) %>% 
  mutate(medtages=max(Periode_Start)==Periode_Start,
         Valg="KV2009") %>% 
  ungroup() %>% 
  subset(medtages==T) %>% 
  select(Komnr, Kommunenavn, Parti, Valg , Fornavn, Efternavn) %>% 
  {.} -> b2009

mayor %>% 
  subset(b2013==T) %>% 
  group_by(Komnr) %>% 
  mutate(medtages=max(Periode_Start)==Periode_Start,
         Valg="KV2013") %>% 
  ungroup() %>% 
  subset(medtages==T) %>% 
  select(Komnr, Kommunenavn, Parti, Valg , Fornavn, Efternavn) %>% 
  {.} -> b2013

mayor %>% 
  subset(b2017==T) %>% 
  group_by(Komnr) %>% 
  mutate(medtages=max(Periode_Start)==Periode_Start,
         Valg="KV2017") %>% 
  ungroup() %>% 
  subset(medtages==T) %>% 
  select(Komnr, Kommunenavn, Parti, Valg , Fornavn, Efternavn) %>% 
  {.} -> b2017

b2009 %>% 
  bind_rows(b2013, b2017) %>% 
  mutate(election=str_sub(Valg, 1,2),
         year=as.numeric(str_sub(Valg, 3,6)),
  ) %>% 
  rename_with(.fn = ~ paste0("inc_", .x)) %>% 
  {.} ->inc_mayor 


valgd <- left_join(valgd, inc_mayor, by = c("komnr" = "inc_Komnr", "election" = "inc_election", "year"="inc_year")) %>% 
  mutate(incumbent=ifelse(election=="FV", incumbent, 
                          ifelse(election=="KV", inc_Parti, NA))) 

table(valgd$year, valgd$incumbent)
rm(inc_mayor, mayor, b2009, b2013, b2017)

#Hospital closures####
hospitals <- read_csv("hospital.csv")
##Distance changed to nearest hospital####
hospitaltoafstm <- read_csv("hosptoafstm.csv")
hospitaltoafstm %>% 
  rename_all( ~ paste("a", names(hospitaltoafstm), sep = "_")) %>% 
  pivot_longer(cols = 2:1378, names_to = "afstemid", values_to = "distance") %>%
  mutate(afstemid=as.numeric(str_sub(afstemid, 3, 8)),
         distance=round(distance/1000, 2),
         hospid=a_ID) %>% 
  left_join(hospitals, by = c("hospid"="hospid")) %>% 
  group_by(afstemid) %>% 
  mutate(close=nth(sort(distance), 30)) %>% 
  ungroup() %>% 
  subset(close>distance) %>% 
  {.} -> hospitaltoafstm

dist <- tibble()

for(i in 1:length(unique(valgd$afstemid))){
  dist[i,1] <- unique(valgd$afstemid)[i]
  names(dist)[1]  <- "afstemid"
  dist[i,2] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[1]|
                                 established<electiondates[1] & is.na(closed)|
                                 established<electiondates[1] & closed>electiondates[1])$distance)[1:1])
  names(dist)[2]  <- "d1_FV2005"
  dist[i,3] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[2]|
                                 established<electiondates[2] & is.na(closed)|
                                 established<electiondates[2] & closed>electiondates[2])$distance)[1:1])
  names(dist)[3]  <- "d1_FV2007"
  dist[i,4] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[3]|
                                 established<electiondates[3] & is.na(closed)|
                                 established<electiondates[3] & closed>electiondates[3])$distance)[1:1])
  names(dist)[4]  <- "d1_FV2011"
  dist[i,5] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[4]|
                                 established<electiondates[4] & is.na(closed)|
                                 established<electiondates[4] & closed>electiondates[4])$distance)[1:1])
  names(dist)[5]  <- "d1_FV2015"
  dist[i,6] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[5]|
                                 established<electiondates[5] & is.na(closed)|
                                 established<electiondates[5] & closed>electiondates[5])$distance)[1:1])
  names(dist)[6]  <- "d1_FV2019"
  dist[i,7] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[6]|
                                 established<electiondates[6] & is.na(closed)|
                                 established<electiondates[6] & closed>electiondates[6])$distance)[1:1])
  names(dist)[7]  <- "d1_KV2005"
  dist[i,8] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[7]|
                                 established<electiondates[7] & is.na(closed)|
                                 established<electiondates[7] & closed>electiondates[7])$distance)[1:1])
  names(dist)[8]  <- "d1_KV2009"
  dist[i,9] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[8]|
                                 established<electiondates[8] & is.na(closed)|
                                 established<electiondates[8] & closed>electiondates[8])$distance)[1:1])
  names(dist)[9]  <- "d1_KV2013"
  dist[i,10] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[9]|
                                  established<electiondates[9] & is.na(closed)|
                                  established<electiondates[9] & closed>electiondates[9])$distance)[1:1])
  names(dist)[10] <- "d1_KV2017"
  dist[i,11] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[1]|
                                  established<electiondates[1] & is.na(closed)|
                                  established<electiondates[1] & closed>electiondates[1])$distance)[1:2])
  names(dist)[11]  <- "d2_FV2005"
  dist[i,12] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[2]|
                                  established<electiondates[2] & is.na(closed)|
                                  established<electiondates[2] & closed>electiondates[2])$distance)[1:2])
  names(dist)[12]  <- "d2_FV2007"
  dist[i,13] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[3]|
                                  established<electiondates[3] & is.na(closed)|
                                  established<electiondates[3] & closed>electiondates[3])$distance)[1:2])
  names(dist)[13]  <- "d2_FV2011"
  dist[i,14] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[4]|
                                  established<electiondates[4] & is.na(closed)|
                                  established<electiondates[4] & closed>electiondates[4])$distance)[1:2])
  names(dist)[14]  <- "d2_FV2015"
  dist[i,15] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[5]|
                                  established<electiondates[5] & is.na(closed)|
                                  established<electiondates[5] & closed>electiondates[5])$distance)[1:2])
  names(dist)[15]  <- "d2_FV2019"
  dist[i,16] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[6]|
                                  established<electiondates[6] & is.na(closed)|
                                  established<electiondates[6] & closed>electiondates[6])$distance)[1:2])
  names(dist)[16]  <- "d2_KV2005"
  dist[i,17] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[7]|
                                  established<electiondates[7] & is.na(closed)|
                                  established<electiondates[7] & closed>electiondates[7])$distance)[1:2])
  names(dist)[17]  <- "d2_KV2009"
  dist[i,18] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[8]|
                                  established<electiondates[8] & is.na(closed)|
                                  established<electiondates[8] & closed>electiondates[8])$distance)[1:2])
  names(dist)[18]  <- "d2_KV2013"
  dist[i,19] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[9]|
                                  established<electiondates[9] & is.na(closed)|
                                  established<electiondates[9] & closed>electiondates[9])$distance)[1:2])
  names(dist)[19]  <- "d2_KV2017"
  
  dist[i,20] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[1]|
                                  established<electiondates[1] & is.na(closed)|
                                  established<electiondates[1] & closed>electiondates[1])$distance)[1:3])
  names(dist)[20]  <- "d3_FV2005"
  dist[i,21] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[2]|
                                  established<electiondates[2] & is.na(closed)|
                                  established<electiondates[2] & closed>electiondates[2])$distance)[1:3])
  names(dist)[21]  <- "d3_FV2007"
  dist[i,22] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[3]|
                                  established<electiondates[3] & is.na(closed)|
                                  established<electiondates[3] & closed>electiondates[3])$distance)[1:3])
  names(dist)[22]  <- "d3_FV2011"
  dist[i,23] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[4]|
                                  established<electiondates[4] & is.na(closed)|
                                  established<electiondates[4] & closed>electiondates[4])$distance)[1:3])
  names(dist)[23]  <- "d3_FV2015"
  dist[i,24] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[5]|
                                  established<electiondates[5] & is.na(closed)|
                                  established<electiondates[5] & closed>electiondates[5])$distance)[1:3])
  names(dist)[24]  <- "d3_FV2019"
  dist[i,25] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[6]|
                                  established<electiondates[6] & is.na(closed)|
                                  established<electiondates[6] & closed>electiondates[6])$distance)[1:3])
  names(dist)[25]  <- "d3_KV2005"
  dist[i,26] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[7]|
                                  established<electiondates[7] & is.na(closed)|
                                  established<electiondates[7] & closed>electiondates[7])$distance)[1:3])
  names(dist)[26]  <- "d3_KV2009"
  dist[i,27] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[8]|
                                  established<electiondates[8] & is.na(closed)|
                                  established<electiondates[8] & closed>electiondates[8])$distance)[1:3])
  names(dist)[27]  <- "d3_KV2013"
  dist[i,28] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[9]|
                                  established<electiondates[9] & is.na(closed)|
                                  established<electiondates[9] & closed>electiondates[9])$distance)[1:3])
  names(dist)[28]  <- "d3_KV2017"
  dist[i,29] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[1]|
                                  established<electiondates[1] & is.na(closed)|
                                  established<electiondates[1] & closed>electiondates[1])$distance)[1:4])
  names(dist)[29]  <- "d4_FV2005"
  dist[i,30] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[2]|
                                  established<electiondates[2] & is.na(closed)|
                                  established<electiondates[2] & closed>electiondates[2])$distance)[1:4])
  names(dist)[30]  <- "d4_FV2007"
  dist[i,31] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[3]|
                                  established<electiondates[3] & is.na(closed)|
                                  established<electiondates[3] & closed>electiondates[3])$distance)[1:4])
  names(dist)[31]  <- "d4_FV2011"
  dist[i,32] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[4]|
                                  established<electiondates[4] & is.na(closed)|
                                  established<electiondates[4] & closed>electiondates[4])$distance)[1:4])
  names(dist)[32]  <- "d4_FV2015"
  dist[i,33] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[5]|
                                  established<electiondates[5] & is.na(closed)|
                                  established<electiondates[5] & closed>electiondates[5])$distance)[1:4])
  names(dist)[33]  <- "d4_FV2019"
  dist[i,34] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[6]|
                                  established<electiondates[6] & is.na(closed)|
                                  established<electiondates[6] & closed>electiondates[6])$distance)[1:4])
  names(dist)[34]  <- "d4_KV2005"
  dist[i,35] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[7]|
                                  established<electiondates[7] & is.na(closed)|
                                  established<electiondates[7] & closed>electiondates[7])$distance)[1:4])
  names(dist)[35]  <- "d4_KV2009"
  dist[i,36] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[8]|
                                  established<electiondates[8] & is.na(closed)|
                                  established<electiondates[8] & closed>electiondates[8])$distance)[1:4])
  names(dist)[36]  <- "d4_KV2013"
  dist[i,37] <- sum(sort(subset(subset(hospitaltoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[9]|
                                  established<electiondates[9] & is.na(closed)|
                                  established<electiondates[9] & closed>electiondates[9])$distance)[1:4])
  names(dist)[37]  <- "d4_KV2017"
}

dist %>% mutate(
  treatment_grp_FVd1=ifelse(d1_FV2005!=d1_FV2007, "Treated before first election (no ref)",
                            ifelse(d1_FV2007==d1_FV2011 & d1_FV2007==d1_FV2015 & d1_FV2007==d1_FV2019, "Control",
                                   ifelse(d1_FV2007<d1_FV2011 & d1_FV2011<d1_FV2015 | d1_FV2007<d1_FV2011 & d1_FV2015<d1_FV2019 | d1_FV2011<d1_FV2015 & d1_FV2015<d1_FV2019, "Treated twice",
                                          ifelse(d1_FV2007>d1_FV2011 | d1_FV2011>d1_FV2015 | d1_FV2015>d1_FV2019 , "New hospital opened",
                                                 ifelse(d1_FV2007<d1_FV2011  , "Treat 1st period",
                                                        ifelse(d1_FV2011<d1_FV2015  , "Treat 2nd period",
                                                               ifelse(d1_FV2015<d1_FV2019  , "Treat 3rd period",NA))))))),
  delta_dist_FVd1=ifelse(treatment_grp_FVd1=="Treat 1st period", d1_FV2011-d1_FV2007,
                         ifelse(treatment_grp_FVd1=="Treat 2nd period", d1_FV2015-d1_FV2011,
                                ifelse(treatment_grp_FVd1=="Treat 3rd period", d1_FV2019-d1_FV2015,
                                       ifelse(treatment_grp_FVd1=="Control", 0,NA)))),
  treatment_grp_FVd2=ifelse(d2_FV2005!=d2_FV2007, "Treated before first election (no ref)",
                            ifelse(d2_FV2007==d2_FV2011 & d2_FV2007==d2_FV2015 & d2_FV2007==d2_FV2019, "Control",
                                   ifelse(d2_FV2007<d2_FV2011 & d2_FV2011<d2_FV2015 | d2_FV2007<d2_FV2011 & d2_FV2015<d2_FV2019 | d2_FV2011<d2_FV2015 & d2_FV2015<d2_FV2019, "Treated twice",
                                          ifelse(d2_FV2007>d2_FV2011 | d2_FV2011>d2_FV2015 | d2_FV2015>d2_FV2019 , "New hospital opened",
                                                 ifelse(d2_FV2007<d2_FV2011  , "Treat 1st period",
                                                        ifelse(d2_FV2011<d2_FV2015  , "Treat 2nd period",
                                                               ifelse(d2_FV2015<d2_FV2019  , "Treat 3rd period",NA))))))),
  delta_dist_FVd2=ifelse(treatment_grp_FVd2=="Treat 1st period", d2_FV2011-d2_FV2007,
                         ifelse(treatment_grp_FVd2=="Treat 2nd period", d2_FV2015-d2_FV2011,
                                ifelse(treatment_grp_FVd2=="Treat 3rd period", d2_FV2019-d2_FV2015,
                                       ifelse(treatment_grp_FVd2=="Control", 0,NA)))),
  treatment_grp_FVd3=ifelse(d3_FV2005!=d3_FV2007, "Treated before first election (no ref)",
                            ifelse(d3_FV2007==d3_FV2011 & d3_FV2007==d3_FV2015 & d3_FV2007==d3_FV2019, "Control",
                                   ifelse(d3_FV2007<d3_FV2011 & d3_FV2011<d3_FV2015 | d3_FV2007<d3_FV2011 & d3_FV2015<d3_FV2019 | d3_FV2011<d3_FV2015 & d3_FV2015<d3_FV2019, "Treated twice",
                                          ifelse(d3_FV2007>d3_FV2011 | d3_FV2011>d3_FV2015 | d3_FV2015>d3_FV2019 , "New hospital opened",
                                                 ifelse(d3_FV2007<d3_FV2011  , "Treat 1st period",
                                                        ifelse(d3_FV2011<d3_FV2015  , "Treat 2nd period",
                                                               ifelse(d3_FV2015<d3_FV2019  , "Treat 3rd period",NA))))))),
  delta_dist_FVd3=ifelse(treatment_grp_FVd3=="Treat 1st period", d3_FV2011-d3_FV2007,
                         ifelse(treatment_grp_FVd3=="Treat 2nd period", d3_FV2015-d3_FV2011,
                                ifelse(treatment_grp_FVd3=="Treat 3rd period", d3_FV2019-d3_FV2015,
                                       ifelse(treatment_grp_FVd3=="Control", 0,NA)))),
  treatment_grp_FVd4=ifelse(d4_FV2005!=d4_FV2007, "Treated before first election (no ref)",
                            ifelse(d4_FV2007==d4_FV2011 & d4_FV2007==d4_FV2015 & d4_FV2007==d4_FV2019, "Control",
                                   ifelse(d4_FV2007<d4_FV2011 & d4_FV2011<d4_FV2015 | d4_FV2007<d4_FV2011 & d4_FV2015<d4_FV2019 | d4_FV2011<d4_FV2015 & d4_FV2015<d4_FV2019, "Treated twice",
                                          ifelse(d4_FV2007>d4_FV2011 | d4_FV2011>d4_FV2015 | d4_FV2015>d4_FV2019 , "New hospital opened",
                                                 ifelse(d4_FV2007<d4_FV2011  , "Treat 1st period",
                                                        ifelse(d4_FV2011<d4_FV2015  , "Treat 2nd period",
                                                               ifelse(d4_FV2015<d4_FV2019  , "Treat 3rd period",NA))))))),
  delta_dist_FVd4=ifelse(treatment_grp_FVd4=="Treat 1st period", d4_FV2011-d4_FV2007,
                         ifelse(treatment_grp_FVd4=="Treat 2nd period", d4_FV2015-d4_FV2011,
                                ifelse(treatment_grp_FVd4=="Treat 3rd period", d4_FV2019-d4_FV2015,
                                       ifelse(treatment_grp_FVd4=="Control", 0,NA)))),
  
  treatment_grp_KVd1=ifelse(d1_KV2005!=d1_KV2009, "Treated before first election (no ref)",
                            ifelse(d1_KV2009==d1_KV2013 & d1_KV2009==d1_KV2017, "Control",
                                   ifelse(d1_KV2009<d1_KV2013 & d1_KV2013<d1_KV2017, "Treated twice",
                                          ifelse(d1_KV2009>d1_KV2013 | d1_KV2013>d1_KV2017, "New hospital opened",
                                                 ifelse(d1_KV2009<d1_KV2013, "Treat 1st period",
                                                        ifelse(d1_KV2013<d1_KV2017, "Treat 2nd period",NA)))))),
  delta_dist_KVd1=ifelse(treatment_grp_KVd1=="Treat 1st period", d1_KV2013-d1_KV2009,
                         ifelse(treatment_grp_KVd1=="Treat 2nd period", d1_KV2017-d1_KV2013,
                                ifelse(treatment_grp_KVd1=="Control", 0,NA))),
  treatment_grp_KVd2=ifelse(d2_KV2005!=d2_KV2009, "Treated before first election (no ref)",
                            ifelse(d2_KV2009==d2_KV2013 & d2_KV2009==d2_KV2017, "Control",
                                   ifelse(d2_KV2009<d2_KV2013 & d2_KV2013<d2_KV2017, "Treated twice",
                                          ifelse(d2_KV2009>d2_KV2013 | d2_KV2013>d2_KV2017, "New hospital opened",
                                                 ifelse(d2_KV2009<d2_KV2013, "Treat 1st period",
                                                        ifelse(d2_KV2013<d2_KV2017, "Treat 2nd period",NA)))))),
  delta_dist_KVd2=ifelse(treatment_grp_KVd2=="Treat 1st period", d2_KV2013-d2_KV2009,
                         ifelse(treatment_grp_KVd2=="Treat 2nd period", d2_KV2017-d2_KV2013,
                                ifelse(treatment_grp_KVd2=="Control", 0,NA))),
  treatment_grp_KVd3=ifelse(d3_KV2005!=d3_KV2009, "Treated before first election (no ref)",
                            ifelse(d3_KV2009==d3_KV2013 & d3_KV2009==d3_KV2017, "Control",
                                   ifelse(d3_KV2009<d3_KV2013 & d3_KV2013<d3_KV2017, "Treated twice",
                                          ifelse(d3_KV2009>d3_KV2013 | d3_KV2013>d3_KV2017, "New hospital opened",
                                                 ifelse(d3_KV2009<d3_KV2013, "Treat 1st period",
                                                        ifelse(d3_KV2013<d3_KV2017, "Treat 2nd period",NA)))))),
  delta_dist_KVd3=ifelse(treatment_grp_KVd3=="Treat 1st period", d3_KV2013-d3_KV2009,
                         ifelse(treatment_grp_KVd3=="Treat 2nd period", d3_KV2017-d3_KV2013,
                                ifelse(treatment_grp_KVd3=="Control", 0,NA))),
  treatment_grp_KVd4=ifelse(d4_KV2005!=d4_KV2009, "Treated before first election (no ref)",
                            ifelse(d4_KV2009==d4_KV2013 & d4_KV2009==d4_KV2017, "Control",
                                   ifelse(d4_KV2009<d4_KV2013 & d4_KV2013<d4_KV2017, "Treated twice",
                                          ifelse(d4_KV2009>d4_KV2013 | d4_KV2013>d4_KV2017, "New hospital opened",
                                                 ifelse(d4_KV2009<d4_KV2013, "Treat 1st period",
                                                        ifelse(d4_KV2013<d4_KV2017, "Treat 2nd period",NA)))))),
  delta_dist_KVd4=ifelse(treatment_grp_KVd4=="Treat 1st period", d4_KV2013-d4_KV2009,
                         ifelse(treatment_grp_KVd4=="Treat 2nd period", d4_KV2017-d4_KV2013,
                                ifelse(treatment_grp_KVd4=="Control", 0,NA))),
)    %>% 
  {.}-> dist

dist %>% 
  pivot_longer(cols = 2:37, names_to = "variable", values_to = "value" ) %>% 
  separate(variable, c("number of hospitals", "election_year"), sep = "_") %>% 
  pivot_wider(names_from = `number of hospitals`, values_from = value) %>% 
  mutate(
    delta_dist_d1=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), delta_dist_FVd1, 
                         ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), delta_dist_KVd1, NA)),
    delta_dist_d2=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), delta_dist_FVd2, 
                         ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), delta_dist_KVd2, NA)),
    delta_dist_d3=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), delta_dist_FVd3, 
                         ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), delta_dist_KVd3, NA)),
    delta_dist_d4=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), delta_dist_FVd4, 
                         ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), delta_dist_KVd4, NA)),
    treatment_grp_d1=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), treatment_grp_FVd1, 
                            ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), treatment_grp_KVd1, NA)),
    treatment_grp_d2=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), treatment_grp_FVd2, 
                            ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), treatment_grp_KVd2, NA)),
    treatment_grp_d3=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), treatment_grp_FVd3, 
                            ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), treatment_grp_KVd3, NA)),
    treatment_grp_d4=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), treatment_grp_FVd4, 
                            ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), treatment_grp_KVd4, NA)),
    election=str_sub(election_year, 1,2),
    year=as.numeric(str_sub(election_year, 3, 6))
  ) %>% 
  subset(election_year %in% c("FV2007", "FV2011", "FV2015", "FV2019", "KV2009", "KV2013", "KV2017")) %>% 
  select(afstemid, year, d1, d2, d3, d4, delta_dist_d1, delta_dist_d2, delta_dist_d3, delta_dist_d4, treatment_grp_d1, treatment_grp_d2, treatment_grp_d3, treatment_grp_d4) %>% 
  {.}-> dist


valgd <- left_join(valgd, dist, by = c("afstemid"="afstemid", "year" = "year"))
rm(hospitals, dist, hospitaltoafstm)

#School closures####
schools <- read_csv("school.csv")

schools %>% 
  group_by(afstemid) %>% 
  subset(F==is.na(closed)) %>% 
  mutate(treattwiceFV=sum(afstemid %in% subset(schools, closed>electiondates[1] & closed<electiondates[5])$afstemid),
         treattwiceKV=sum(afstemid %in% subset(schools, closed>electiondates[6] & closed<electiondates[9])$afstemid)) %>% 
  subset(treattwiceFV>1 | treattwiceKV>1) %>% 
  {.}-> treattwice

##Schools closed in precinct####
valgd %>% 
  mutate(treatment_grp=ifelse(F==(afstemid %in% schools$afstemid), "No school in precinct",
                              ifelse(afstemid %in% subset(treattwice, treattwiceFV>1)$afstemid & election =="FV" |
                                       afstemid %in% subset(treattwice, treattwiceKV>1)$afstemid & election =="KV"   , "Treated twice", 
                                     ifelse(afstemid %in% subset(schools, established>electiondates[1])$afstemid, "New school opened in precinct",
                                            ifelse(afstemid %in% subset(schools, closed>electiondates[1] & closed<electiondates[2])$afstemid & election =="FV"|
                                                     afstemid %in% subset(schools, closed>electiondates[6] & closed<electiondates[7])$afstemid & election %in% c("KV", "RV"), "Treated before first election (no ref)",
                                                   ifelse(afstemid %in% subset(schools, closed>electiondates[2] & closed<electiondates[3])$afstemid & election =="FV" |
                                                            afstemid %in% subset(schools, closed>electiondates[7] & closed<electiondates[8])$afstemid & election %in% c("KV","RV"), "Treat 1st period",
                                                          ifelse(afstemid %in% subset(schools, closed>electiondates[3] & closed<electiondates[4])$afstemid & election =="FV" |
                                                                   afstemid %in% subset(schools, closed>electiondates[8] & closed<electiondates[9])$afstemid & election %in% c("KV","RV"), "Treat 2nd period",
                                                                 ifelse(afstemid %in% subset(schools, closed>electiondates[4] & closed<electiondates[5])$afstemid & election =="FV", "Treat 3rd period",
                                                                        ifelse(F==(afstemid %in% subset(schools, closed>electiondates[1] & closed<electiondates[5])$afstemid) & election=="FV" |
                                                                                 F==(afstemid %in% subset(schools, closed>electiondates[6] & closed<electiondates[9])$afstemid) & election %in% c("KV","RV"), "Control", NA)))))))))%>% 
  mutate(
    rel_year=ifelse(treatment_grp %in% c("No school in precinct",  "Treated twice", "New school opened in precinct", "Treated before first election (no ref)"), NA,
                    ifelse(treatment_grp %in% c("Control"), Inf, 
                           ifelse(treatment_grp=="Treat 3rd period" & year =="2007",-3,
                                  ifelse(treatment_grp=="Treat 3rd period" & year =="2011",-2,
                                         ifelse(treatment_grp=="Treat 3rd period" & year =="2015",-1,
                                                ifelse(treatment_grp=="Treat 3rd period" & year =="2019",0,
                                                       ifelse(treatment_grp=="Treat 2nd period" & year =="2007" & election =="FV" |
                                                                treatment_grp=="Treat 2nd period" & year =="2009" & election %in% c("KV","RV") ,-2, 
                                                              ifelse(treatment_grp=="Treat 2nd period" & year =="2011" & election =="FV" |
                                                                       treatment_grp=="Treat 2nd period" & year =="2013" & election %in% c("KV","RV") ,-1, 
                                                                     ifelse(treatment_grp=="Treat 2nd period" & year =="2015" & election =="FV" |
                                                                              treatment_grp=="Treat 2nd period" & year =="2017" & election %in% c("KV","RV") , 0,  
                                                                            ifelse(treatment_grp=="Treat 2nd period" & year =="2019" & election =="FV" , 1,
                                                                                   ifelse(treatment_grp=="Treat 1st period" & year =="2007" & election =="FV" |
                                                                                            treatment_grp=="Treat 1st period" & year =="2009" & election %in% c("KV","RV") ,-1, 
                                                                                          ifelse(treatment_grp=="Treat 1st period" & year =="2011" & election =="FV" |
                                                                                                   treatment_grp=="Treat 1st period" & year =="2013" & election %in% c("KV","RV") , 0, 
                                                                                                 ifelse(treatment_grp=="Treat 1st period" & year =="2015" & election =="FV" |
                                                                                                          treatment_grp=="Treat 1st period" & year =="2017" & election %in% c("KV","RV") , 1,  
                                                                                                        ifelse(treatment_grp=="Treat 1st period" & year =="2019" & election =="FV" , 2,NA)))))))))))))),
    treat=ifelse(is.na(rel_year), NA,
                 ifelse(rel_year==Inf, FALSE, 
                        ifelse(rel_year>-1,TRUE,FALSE))),
    
  )%>% 
  {.}->valgd

##Distance changed to nearest schools####
schoolstoafstm <- read_csv("schooltoafstm.csv")

schoolstoafstm %>% 
  rename_all( ~ paste("a", names(schoolstoafstm), sep = "_")) %>% 
  pivot_longer(cols = 2:1378, names_to = "afstemid", values_to = "distance") %>%
  mutate(afstemid=as.numeric(str_sub(afstemid, 3, 8)),
         distance=round(distance/1000, 2),
         schoolid=a_ID) %>% 
  left_join(select(schools,schoolid, established, closed), by = c("schoolid"="schoolid")) %>% 
  group_by(afstemid) %>% 
  mutate(close=nth(sort(distance), 50)) %>% 
  ungroup() %>% 
  subset(close>distance) %>% 
  {.} -> schoolstoafstm

dist <- tibble()

for(i in 1:length(unique(valgd$afstemid))){
  dist[i,1] <- unique(valgd$afstemid)[i]
  names(dist)[1]  <- "afstemid"
  dist[i,2] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[1]|
                                 established<electiondates[1] & is.na(closed)|
                                 established<electiondates[1] & closed>electiondates[1])$distance)[1:1])
  names(dist)[2]  <- "d1_FV2005"
  dist[i,3] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[2]|
                                 established<electiondates[2] & is.na(closed)|
                                 established<electiondates[2] & closed>electiondates[2])$distance)[1:1])
  names(dist)[3]  <- "d1_FV2007"
  dist[i,4] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[3]|
                                 established<electiondates[3] & is.na(closed)|
                                 established<electiondates[3] & closed>electiondates[3])$distance)[1:1])
  names(dist)[4]  <- "d1_FV2011"
  dist[i,5] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[4]|
                                 established<electiondates[4] & is.na(closed)|
                                 established<electiondates[4] & closed>electiondates[4])$distance)[1:1])
  names(dist)[5]  <- "d1_FV2015"
  dist[i,6] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[5]|
                                 established<electiondates[5] & is.na(closed)|
                                 established<electiondates[5] & closed>electiondates[5])$distance)[1:1])
  names(dist)[6]  <- "d1_FV2019"
  dist[i,7] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[6]|
                                 established<electiondates[6] & is.na(closed)|
                                 established<electiondates[6] & closed>electiondates[6])$distance)[1:1])
  names(dist)[7]  <- "d1_KV2005"
  dist[i,8] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[7]|
                                 established<electiondates[7] & is.na(closed)|
                                 established<electiondates[7] & closed>electiondates[7])$distance)[1:1])
  names(dist)[8]  <- "d1_KV2009"
  dist[i,9] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                               is.na(established) & is.na(closed)|
                                 is.na(established) & closed>electiondates[8]|
                                 established<electiondates[8] & is.na(closed)|
                                 established<electiondates[8] & closed>electiondates[8])$distance)[1:1])
  names(dist)[9]  <- "d1_KV2013"
  dist[i,10] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[9]|
                                  established<electiondates[9] & is.na(closed)|
                                  established<electiondates[9] & closed>electiondates[9])$distance)[1:1])
  names(dist)[10] <- "d1_KV2017"
  
  dist[i,11] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[1]|
                                  established<electiondates[1] & is.na(closed)|
                                  established<electiondates[1] & closed>electiondates[1])$distance)[1:2])
  names(dist)[11]  <- "d2_FV2005"
  dist[i,12] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[2]|
                                  established<electiondates[2] & is.na(closed)|
                                  established<electiondates[2] & closed>electiondates[2])$distance)[1:2])
  names(dist)[12]  <- "d2_FV2007"
  dist[i,13] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[3]|
                                  established<electiondates[3] & is.na(closed)|
                                  established<electiondates[3] & closed>electiondates[3])$distance)[1:2])
  names(dist)[13]  <- "d2_FV2011"
  dist[i,14] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[4]|
                                  established<electiondates[4] & is.na(closed)|
                                  established<electiondates[4] & closed>electiondates[4])$distance)[1:2])
  names(dist)[14]  <- "d2_FV2015"
  dist[i,15] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[5]|
                                  established<electiondates[5] & is.na(closed)|
                                  established<electiondates[5] & closed>electiondates[5])$distance)[1:2])
  names(dist)[15]  <- "d2_FV2019"
  dist[i,16] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[6]|
                                  established<electiondates[6] & is.na(closed)|
                                  established<electiondates[6] & closed>electiondates[6])$distance)[1:2])
  names(dist)[16]  <- "d2_KV2005"
  dist[i,17] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[7]|
                                  established<electiondates[7] & is.na(closed)|
                                  established<electiondates[7] & closed>electiondates[7])$distance)[1:2])
  names(dist)[17]  <- "d2_KV2009"
  dist[i,18] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[8]|
                                  established<electiondates[8] & is.na(closed)|
                                  established<electiondates[8] & closed>electiondates[8])$distance)[1:2])
  names(dist)[18]  <- "d2_KV2013"
  dist[i,19] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[9]|
                                  established<electiondates[9] & is.na(closed)|
                                  established<electiondates[9] & closed>electiondates[9])$distance)[1:2])
  names(dist)[19]  <- "d2_KV2017"
  
  dist[i,20] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[1]|
                                  established<electiondates[1] & is.na(closed)|
                                  established<electiondates[1] & closed>electiondates[1])$distance)[1:3])
  names(dist)[20]  <- "d3_FV2005"
  dist[i,21] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[2]|
                                  established<electiondates[2] & is.na(closed)|
                                  established<electiondates[2] & closed>electiondates[2])$distance)[1:3])
  names(dist)[21]  <- "d3_FV2007"
  dist[i,22] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[3]|
                                  established<electiondates[3] & is.na(closed)|
                                  established<electiondates[3] & closed>electiondates[3])$distance)[1:3])
  names(dist)[22]  <- "d3_FV2011"
  dist[i,23] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[4]|
                                  established<electiondates[4] & is.na(closed)|
                                  established<electiondates[4] & closed>electiondates[4])$distance)[1:3])
  names(dist)[23]  <- "d3_FV2015"
  dist[i,24] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[5]|
                                  established<electiondates[5] & is.na(closed)|
                                  established<electiondates[5] & closed>electiondates[5])$distance)[1:3])
  names(dist)[24]  <- "d3_FV2019"
  dist[i,25] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[6]|
                                  established<electiondates[6] & is.na(closed)|
                                  established<electiondates[6] & closed>electiondates[6])$distance)[1:3])
  names(dist)[25]  <- "d3_KV2005"
  dist[i,26] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[7]|
                                  established<electiondates[7] & is.na(closed)|
                                  established<electiondates[7] & closed>electiondates[7])$distance)[1:3])
  names(dist)[26]  <- "d3_KV2009"
  dist[i,27] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[8]|
                                  established<electiondates[8] & is.na(closed)|
                                  established<electiondates[8] & closed>electiondates[8])$distance)[1:3])
  names(dist)[27]  <- "d3_KV2013"
  dist[i,28] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[9]|
                                  established<electiondates[9] & is.na(closed)|
                                  established<electiondates[9] & closed>electiondates[9])$distance)[1:3])
  names(dist)[28]  <- "d3_KV2017"
  dist[i,29] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[1]|
                                  established<electiondates[1] & is.na(closed)|
                                  established<electiondates[1] & closed>electiondates[1])$distance)[1:4])
  names(dist)[29]  <- "d4_FV2005"
  dist[i,30] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[2]|
                                  established<electiondates[2] & is.na(closed)|
                                  established<electiondates[2] & closed>electiondates[2])$distance)[1:4])
  names(dist)[30]  <- "d4_FV2007"
  dist[i,31] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[3]|
                                  established<electiondates[3] & is.na(closed)|
                                  established<electiondates[3] & closed>electiondates[3])$distance)[1:4])
  names(dist)[31]  <- "d4_FV2011"
  dist[i,32] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[4]|
                                  established<electiondates[4] & is.na(closed)|
                                  established<electiondates[4] & closed>electiondates[4])$distance)[1:4])
  names(dist)[32]  <- "d4_FV2015"
  dist[i,33] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[5]|
                                  established<electiondates[5] & is.na(closed)|
                                  established<electiondates[5] & closed>electiondates[5])$distance)[1:4])
  names(dist)[33]  <- "d4_FV2019"
  dist[i,34] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[6]|
                                  established<electiondates[6] & is.na(closed)|
                                  established<electiondates[6] & closed>electiondates[6])$distance)[1:4])
  names(dist)[34]  <- "d4_KV2005"
  dist[i,35] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[7]|
                                  established<electiondates[7] & is.na(closed)|
                                  established<electiondates[7] & closed>electiondates[7])$distance)[1:4])
  names(dist)[35]  <- "d4_KV2009"
  dist[i,36] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[8]|
                                  established<electiondates[8] & is.na(closed)|
                                  established<electiondates[8] & closed>electiondates[8])$distance)[1:4])
  names(dist)[36]  <- "d4_KV2013"
  dist[i,37] <- sum(sort(subset(subset(schoolstoafstm, afstemid==unique(valgd$afstemid)[i]),
                                is.na(established) & is.na(closed)|
                                  is.na(established) & closed>electiondates[9]|
                                  established<electiondates[9] & is.na(closed)|
                                  established<electiondates[9] & closed>electiondates[9])$distance)[1:4])
  names(dist)[37]  <- "d4_KV2017"
}

dist %>% mutate(
  treatment_grp_FVd1=ifelse(d1_FV2005!=d1_FV2007, "Treated before first election (no ref)",
                            ifelse(d1_FV2007==d1_FV2011 & d1_FV2007==d1_FV2015 & d1_FV2007==d1_FV2019, "Control",
                                   ifelse(d1_FV2007<d1_FV2011 & d1_FV2011<d1_FV2015 | d1_FV2007<d1_FV2011 & d1_FV2015<d1_FV2019 | d1_FV2011<d1_FV2015 & d1_FV2015<d1_FV2019, "Treated twice",
                                          ifelse(d1_FV2007>d1_FV2011 | d1_FV2011>d1_FV2015 | d1_FV2015>d1_FV2019 , "New school opened in precinct",
                                                 ifelse(d1_FV2007<d1_FV2011  , "Treat 1st period",
                                                        ifelse(d1_FV2011<d1_FV2015  , "Treat 2nd period",
                                                               ifelse(d1_FV2015<d1_FV2019  , "Treat 3rd period",NA))))))),
  delta_dist_FVd1=ifelse(treatment_grp_FVd1=="Treat 1st period", d1_FV2011-d1_FV2007,
                         ifelse(treatment_grp_FVd1=="Treat 2nd period", d1_FV2015-d1_FV2011,
                                ifelse(treatment_grp_FVd1=="Treat 3rd period", d1_FV2019-d1_FV2015,
                                       ifelse(treatment_grp_FVd1=="Control", 0,NA)))),
  treatment_grp_FVd2=ifelse(d2_FV2005!=d2_FV2007, "Treated before first election (no ref)",
                            ifelse(d2_FV2007==d2_FV2011 & d2_FV2007==d2_FV2015 & d2_FV2007==d2_FV2019, "Control",
                                   ifelse(d2_FV2007<d2_FV2011 & d2_FV2011<d2_FV2015 | d2_FV2007<d2_FV2011 & d2_FV2015<d2_FV2019 | d2_FV2011<d2_FV2015 & d2_FV2015<d2_FV2019, "Treated twice",
                                          ifelse(d2_FV2007>d2_FV2011 | d2_FV2011>d2_FV2015 | d2_FV2015>d2_FV2019 , "New school opened in precinct",
                                                 ifelse(d2_FV2007<d2_FV2011  , "Treat 1st period",
                                                        ifelse(d2_FV2011<d2_FV2015  , "Treat 2nd period",
                                                               ifelse(d2_FV2015<d2_FV2019  , "Treat 3rd period",NA))))))),
  delta_dist_FVd2=ifelse(treatment_grp_FVd2=="Treat 1st period", d2_FV2011-d2_FV2007,
                         ifelse(treatment_grp_FVd2=="Treat 2nd period", d2_FV2015-d2_FV2011,
                                ifelse(treatment_grp_FVd2=="Treat 3rd period", d2_FV2019-d2_FV2015,
                                       ifelse(treatment_grp_FVd2=="Control", 0,NA)))),
  treatment_grp_FVd3=ifelse(d3_FV2005!=d3_FV2007, "Treated before first election (no ref)",
                            ifelse(d3_FV2007==d3_FV2011 & d3_FV2007==d3_FV2015 & d3_FV2007==d3_FV2019, "Control",
                                   ifelse(d3_FV2007<d3_FV2011 & d3_FV2011<d3_FV2015 | d3_FV2007<d3_FV2011 & d3_FV2015<d3_FV2019 | d3_FV2011<d3_FV2015 & d3_FV2015<d3_FV2019, "Treated twice",
                                          ifelse(d3_FV2007>d3_FV2011 | d3_FV2011>d3_FV2015 | d3_FV2015>d3_FV2019 , "New school opened in precinct",
                                                 ifelse(d3_FV2007<d3_FV2011  , "Treat 1st period",
                                                        ifelse(d3_FV2011<d3_FV2015  , "Treat 2nd period",
                                                               ifelse(d3_FV2015<d3_FV2019  , "Treat 3rd period",NA))))))),
  delta_dist_FVd3=ifelse(treatment_grp_FVd3=="Treat 1st period", d3_FV2011-d3_FV2007,
                         ifelse(treatment_grp_FVd3=="Treat 2nd period", d3_FV2015-d3_FV2011,
                                ifelse(treatment_grp_FVd3=="Treat 3rd period", d3_FV2019-d3_FV2015,
                                       ifelse(treatment_grp_FVd3=="Control", 0,NA)))),
  treatment_grp_FVd4=ifelse(d4_FV2005!=d4_FV2007, "Treated before first election (no ref)",
                            ifelse(d4_FV2007==d4_FV2011 & d4_FV2007==d4_FV2015 & d4_FV2007==d4_FV2019, "Control",
                                   ifelse(d4_FV2007<d4_FV2011 & d4_FV2011<d4_FV2015 | d4_FV2007<d4_FV2011 & d4_FV2015<d4_FV2019 | d4_FV2011<d4_FV2015 & d4_FV2015<d4_FV2019, "Treated twice",
                                          ifelse(d4_FV2007>d4_FV2011 | d4_FV2011>d4_FV2015 | d4_FV2015>d4_FV2019 , "New school opened in precinct",
                                                 ifelse(d4_FV2007<d4_FV2011  , "Treat 1st period",
                                                        ifelse(d4_FV2011<d4_FV2015  , "Treat 2nd period",
                                                               ifelse(d4_FV2015<d4_FV2019  , "Treat 3rd period",NA))))))),
  delta_dist_FVd4=ifelse(treatment_grp_FVd4=="Treat 1st period", d4_FV2011-d4_FV2007,
                         ifelse(treatment_grp_FVd4=="Treat 2nd period", d4_FV2015-d4_FV2011,
                                ifelse(treatment_grp_FVd4=="Treat 3rd period", d4_FV2019-d4_FV2015,
                                       ifelse(treatment_grp_FVd4=="Control", 0,NA)))),
  
  treatment_grp_KVd1=ifelse(d1_KV2005!=d1_KV2009, "Treated before first election (no ref)",
                            ifelse(d1_KV2009==d1_KV2013 & d1_KV2009==d1_KV2017, "Control",
                                   ifelse(d1_KV2009<d1_KV2013 & d1_KV2013<d1_KV2017, "Treated twice",
                                          ifelse(d1_KV2009>d1_KV2013 | d1_KV2013>d1_KV2017, "New school opened in precinct",
                                                 ifelse(d1_KV2009<d1_KV2013, "Treat 1st period",
                                                        ifelse(d1_KV2013<d1_KV2017, "Treat 2nd period",NA)))))),
  delta_dist_KVd1=ifelse(treatment_grp_KVd1=="Treat 1st period", d1_KV2013-d1_KV2009,
                         ifelse(treatment_grp_KVd1=="Treat 2nd period", d1_KV2017-d1_KV2013,
                                ifelse(treatment_grp_KVd1=="Control", 0,NA))),
  treatment_grp_KVd2=ifelse(d2_KV2005!=d2_KV2009, "Treated before first election (no ref)",
                            ifelse(d2_KV2009==d2_KV2013 & d2_KV2009==d2_KV2017, "Control",
                                   ifelse(d2_KV2009<d2_KV2013 & d2_KV2013<d2_KV2017, "Treated twice",
                                          ifelse(d2_KV2009>d2_KV2013 | d2_KV2013>d2_KV2017, "New school opened in precinct",
                                                 ifelse(d2_KV2009<d2_KV2013, "Treat 1st period",
                                                        ifelse(d2_KV2013<d2_KV2017, "Treat 2nd period",NA)))))),
  delta_dist_KVd2=ifelse(treatment_grp_KVd2=="Treat 1st period", d2_KV2013-d2_KV2009,
                         ifelse(treatment_grp_KVd2=="Treat 2nd period", d2_KV2017-d2_KV2013,
                                ifelse(treatment_grp_KVd2=="Control", 0,NA))),
  treatment_grp_KVd3=ifelse(d3_KV2005!=d3_KV2009, "Treated before first election (no ref)",
                            ifelse(d3_KV2009==d3_KV2013 & d3_KV2009==d3_KV2017, "Control",
                                   ifelse(d3_KV2009<d3_KV2013 & d3_KV2013<d3_KV2017, "Treated twice",
                                          ifelse(d3_KV2009>d3_KV2013 | d3_KV2013>d3_KV2017, "New school opened in precinct",
                                                 ifelse(d3_KV2009<d3_KV2013, "Treat 1st period",
                                                        ifelse(d3_KV2013<d3_KV2017, "Treat 2nd period",NA)))))),
  delta_dist_KVd3=ifelse(treatment_grp_KVd3=="Treat 1st period", d3_KV2013-d3_KV2009,
                         ifelse(treatment_grp_KVd3=="Treat 2nd period", d3_KV2017-d3_KV2013,
                                ifelse(treatment_grp_KVd3=="Control", 0,NA))),
  treatment_grp_KVd4=ifelse(d4_KV2005!=d4_KV2009, "Treated before first election (no ref)",
                            ifelse(d4_KV2009==d4_KV2013 & d4_KV2009==d4_KV2017, "Control",
                                   ifelse(d4_KV2009<d4_KV2013 & d4_KV2013<d4_KV2017, "Treated twice",
                                          ifelse(d4_KV2009>d4_KV2013 | d4_KV2013>d4_KV2017, "New school opened in precinct",
                                                 ifelse(d4_KV2009<d4_KV2013, "Treat 1st period",
                                                        ifelse(d4_KV2013<d4_KV2017, "Treat 2nd period",NA)))))),
  delta_dist_KVd4=ifelse(treatment_grp_KVd4=="Treat 1st period", d4_KV2013-d4_KV2009,
                         ifelse(treatment_grp_KVd4=="Treat 2nd period", d4_KV2017-d4_KV2013,
                                ifelse(treatment_grp_KVd4=="Control", 0,NA))),
)    %>% 
  {.}-> dist

dist %>% 
  pivot_longer(cols = 2:37,names_to = "variable", values_to = "value") %>% 
  separate(variable, c("number of schools", "election_year"), sep = "_") %>% 
  pivot_wider(names_from = `number of schools`, values_from = value) %>% 
  mutate(
    delta_dist_d1=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), delta_dist_FVd1, 
                         ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), delta_dist_KVd1, NA)),
    delta_dist_d2=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), delta_dist_FVd2, 
                         ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), delta_dist_KVd2, NA)),
    delta_dist_d3=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), delta_dist_FVd3, 
                         ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), delta_dist_KVd3, NA)),
    delta_dist_d4=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), delta_dist_FVd4, 
                         ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), delta_dist_KVd4, NA)),
    treatment_grp_d1=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), treatment_grp_FVd1, 
                            ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), treatment_grp_KVd1, NA)),
    treatment_grp_d2=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), treatment_grp_FVd2, 
                            ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), treatment_grp_KVd2, NA)),
    treatment_grp_d3=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), treatment_grp_FVd3, 
                            ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), treatment_grp_KVd3, NA)),
    treatment_grp_d4=ifelse(election_year%in% c("FV2007", "FV2011", "FV2015", "FV2019"), treatment_grp_FVd4, 
                            ifelse(election_year%in% c("KV2009", "KV2013", "KV2017"), treatment_grp_KVd4, NA)),
    election=str_sub(election_year, 1,2),
    year=as.numeric(str_sub(election_year, 3, 6))
  ) %>% 
  subset(election_year %in% c("FV2007", "FV2011", "FV2015", "FV2019", "KV2009", "KV2013", "KV2017")) %>% 
  select(afstemid, year, d1, d2, d3, d4, delta_dist_d1, delta_dist_d2, delta_dist_d3, delta_dist_d4, treatment_grp_d1, treatment_grp_d2, treatment_grp_d3, treatment_grp_d4) %>% 
  rename(sch_d1 = d1, 
         sch_d2 = d2,
         sch_d3 = d3,
         sch_d4 = d4, 
         sch_delta_dist_d1 = delta_dist_d1,
         sch_delta_dist_d2 = delta_dist_d2,
         sch_delta_dist_d3 = delta_dist_d3, 
         sch_delta_dist_d4 = delta_dist_d4, 
         sch_treatment_grp_d1 = treatment_grp_d1, 
         sch_treatment_grp_d2 = treatment_grp_d2, 
         sch_treatment_grp_d3 = treatment_grp_d3, 
         sch_treatment_grp_d4 = treatment_grp_d4) %>% 
  {.}-> dist

valgd <- left_join(valgd, dist, by = c("afstemid"="afstemid", "year" = "year"))

rm(schoolstoafstm, schools, treattwice, dist)

#Selection of variables from election data#####
#Renaming og some variables to ensure consistency
valgd %>%
  mutate(
    schp_g=ifelse(treatment_grp=="Treat 1st period" & election=="FV", 2011,
             ifelse(treatment_grp=="Treat 1st period" & election %in% c("KV","RV"), 2013,
                    ifelse(treatment_grp=="Treat 2nd period" & election=="FV", 2015,
                           ifelse(treatment_grp=="Treat 2nd period" & election %in% c("KV","RV"), 2017,
                                  ifelse(treatment_grp=="Treat 3rd period" & election=="FV", 2019,
                                         ifelse(treatment_grp=="Control", 0, NA)))))),
    schd1_g=ifelse(sch_treatment_grp_d1=="Treat 1st period" & election=="FV", 2011,
                   ifelse(sch_treatment_grp_d1=="Treat 1st period" & election %in% c("KV","RV"), 2013,
                          ifelse(sch_treatment_grp_d1=="Treat 2nd period" & election=="FV", 2015,
                                 ifelse(sch_treatment_grp_d1=="Treat 2nd period" & election %in% c("KV","RV"), 2017,
                                        ifelse(sch_treatment_grp_d1=="Treat 3rd period" & election=="FV", 2019,
                                               ifelse(sch_treatment_grp_d1=="Control", 0, NA)))))),
    schd2_g=ifelse(sch_treatment_grp_d2=="Treat 1st period" & election=="FV", 2011,
                   ifelse(sch_treatment_grp_d2=="Treat 1st period" & election %in% c("KV","RV"), 2013,
                          ifelse(sch_treatment_grp_d2=="Treat 2nd period" & election=="FV", 2015,
                                 ifelse(sch_treatment_grp_d2=="Treat 2nd period" & election %in% c("KV","RV"), 2017,
                                        ifelse(sch_treatment_grp_d2=="Treat 3rd period" & election=="FV", 2019,
                                               ifelse(sch_treatment_grp_d2=="Control", 0, NA)))))),
    schd3_g=ifelse(sch_treatment_grp_d3=="Treat 1st period" & election=="FV", 2011,
                   ifelse(sch_treatment_grp_d3=="Treat 1st period" & election %in% c("KV","RV"), 2013,
                          ifelse(sch_treatment_grp_d3=="Treat 2nd period" & election=="FV", 2015,
                                 ifelse(sch_treatment_grp_d3=="Treat 2nd period" & election %in% c("KV","RV"), 2017,
                                        ifelse(sch_treatment_grp_d3=="Treat 3rd period" & election=="FV", 2019,
                                               ifelse(sch_treatment_grp_d3=="Control", 0, NA)))))),
    schd4_g=ifelse(sch_treatment_grp_d4=="Treat 1st period" & election=="FV", 2011,
                   ifelse(sch_treatment_grp_d4=="Treat 1st period" & election %in% c("KV","RV"), 2013,
                          ifelse(sch_treatment_grp_d4=="Treat 2nd period" & election=="FV", 2015,
                                 ifelse(sch_treatment_grp_d4=="Treat 2nd period" & election %in% c("KV","RV"), 2017,
                                        ifelse(sch_treatment_grp_d4=="Treat 3rd period" & election=="FV", 2019,
                                               ifelse(sch_treatment_grp_d4=="Control", 0, NA)))))),
    hosd1_g=ifelse(treatment_grp_d1=="Treat 1st period" & election=="FV", 2011,
                   ifelse(treatment_grp_d1=="Treat 1st period" & election %in% c("KV","RV"), 2013,
                          ifelse(treatment_grp_d1=="Treat 2nd period" & election=="FV", 2015,
                                 ifelse(treatment_grp_d1=="Treat 2nd period" & election %in% c("KV","RV"), 2017,
                                        ifelse(treatment_grp_d1=="Treat 3rd period" & election=="FV", 2019,
                                               ifelse(treatment_grp_d1=="Control", 0, NA)))))),
    hosd2_g=ifelse(treatment_grp_d2=="Treat 1st period" & election=="FV", 2011,
                   ifelse(treatment_grp_d2=="Treat 1st period" & election %in% c("KV","RV"), 2013,
                          ifelse(treatment_grp_d2=="Treat 2nd period" & election=="FV", 2015,
                                 ifelse(treatment_grp_d2=="Treat 2nd period" & election %in% c("KV","RV"), 2017,
                                        ifelse(treatment_grp_d2=="Treat 3rd period" & election=="FV", 2019,
                                               ifelse(treatment_grp_d2=="Control", 0, NA)))))),
    hosd3_g=ifelse(treatment_grp_d3=="Treat 1st period" & election=="FV", 2011,
                   ifelse(treatment_grp_d3=="Treat 1st period" & election %in% c("KV","RV"), 2013,
                          ifelse(treatment_grp_d3=="Treat 2nd period" & election=="FV", 2015,
                                 ifelse(treatment_grp_d3=="Treat 2nd period" & election %in% c("KV","RV"), 2017,
                                        ifelse(treatment_grp_d3=="Treat 3rd period" & election=="FV", 2019,
                                               ifelse(treatment_grp_d3=="Control", 0, NA)))))),
    hosd4_g=ifelse(treatment_grp_d4=="Treat 1st period" & election=="FV", 2011,
                   ifelse(treatment_grp_d4=="Treat 1st period" & election %in% c("KV","RV"), 2013,
                          ifelse(treatment_grp_d4=="Treat 2nd period" & election=="FV", 2015,
                                 ifelse(treatment_grp_d4=="Treat 2nd period" & election %in% c("KV","RV"), 2017,
                                        ifelse(treatment_grp_d4=="Treat 3rd period" & election=="FV", 2019,
                                               ifelse(treatment_grp_d4=="Control", 0, NA)))))),

    schp_treat=ifelse(schp_g==0, F,
                       ifelse(schp_g<=year,T,F)),
    schd1_treat=ifelse(schd1_g==0, F,
                       ifelse(schd1_g<=year,T,F)),
    schd2_treat=ifelse(schd2_g==0, F,
                       ifelse(schd2_g<=year,T,F)),
    schd3_treat=ifelse(schd3_g==0, F,
                       ifelse(schd3_g<=year,T,F)),
    schd4_treat=ifelse(schd4_g==0, F,
                       ifelse(schd4_g<=year,T,F)),
    hosd1_treat=ifelse(hosd1_g==0, F,
                       ifelse(hosd1_g<=year,T,F)),
    hosd2_treat=ifelse(hosd2_g==0, F,
                       ifelse(hosd2_g<=year,T,F)),
    hosd3_treat=ifelse(hosd3_g==0, F,
                       ifelse(hosd3_g<=year,T,F)),
    hosd4_treat=ifelse(hosd4_g==0, F,
                       ifelse(hosd4_g<=year,T,F)),
    schp_relyear=ifelse(treatment_grp %in% c("No school in precinct",  "Treated twice", "New school opened in precinct", "Treated before first election (no ref)"), NA,
                         ifelse(treatment_grp %in% c("Control"), Inf, 
                                ifelse(treatment_grp=="Treat 3rd period" & year =="2007",-3,
                                       ifelse(treatment_grp=="Treat 3rd period" & year =="2011",-2,
                                              ifelse(treatment_grp=="Treat 3rd period" & year =="2015",-1,
                                                     ifelse(treatment_grp=="Treat 3rd period" & year =="2019",0,
                                                            ifelse(treatment_grp=="Treat 2nd period" & year =="2007" & election =="FV" |
                                                                     treatment_grp=="Treat 2nd period" & year =="2009" & election %in% c("KV","RV") ,-2, 
                                                                   ifelse(treatment_grp=="Treat 2nd period" & year =="2011" & election =="FV" |
                                                                            treatment_grp=="Treat 2nd period" & year =="2013" & election %in% c("KV","RV") ,-1, 
                                                                          ifelse(treatment_grp=="Treat 2nd period" & year =="2015" & election =="FV" |
                                                                                   treatment_grp=="Treat 2nd period" & year =="2017" & election %in% c("KV","RV") , 0,  
                                                                                 ifelse(treatment_grp=="Treat 2nd period" & year =="2019" & election =="FV" , 1,
                                                                                        ifelse(treatment_grp=="Treat 1st period" & year =="2007" & election =="FV" |
                                                                                                 treatment_grp=="Treat 1st period" & year =="2009" & election %in% c("KV","RV") ,-1, 
                                                                                               ifelse(treatment_grp=="Treat 1st period" & year =="2011" & election =="FV" |
                                                                                                        treatment_grp=="Treat 1st period" & year =="2013" & election %in% c("KV","RV") , 0, 
                                                                                                      ifelse(treatment_grp=="Treat 1st period" & year =="2015" & election =="FV" |
                                                                                                               treatment_grp=="Treat 1st period" & year =="2017" & election %in% c("KV","RV") , 1,  
                                                                                                             ifelse(treatment_grp=="Treat 1st period" & year =="2019" & election =="FV" , 2,NA)))))))))))))),
    schd1_relyear=ifelse(sch_treatment_grp_d1 %in% c("No school in precinct",  "Treated twice", "New school opened in precinct", "Treated before first election (no ref)"), NA,
                         ifelse(sch_treatment_grp_d1 %in% c("Control"), Inf, 
                                ifelse(sch_treatment_grp_d1=="Treat 3rd period" & year =="2007",-3,
                                       ifelse(sch_treatment_grp_d1=="Treat 3rd period" & year =="2011",-2,
                                              ifelse(sch_treatment_grp_d1=="Treat 3rd period" & year =="2015",-1,
                                                     ifelse(sch_treatment_grp_d1=="Treat 3rd period" & year =="2019",0,
                                                            ifelse(sch_treatment_grp_d1=="Treat 2nd period" & year =="2007" & election =="FV" |
                                                                     sch_treatment_grp_d1=="Treat 2nd period" & year =="2009" & election %in% c("KV","RV") ,-2, 
                                                                   ifelse(sch_treatment_grp_d1=="Treat 2nd period" & year =="2011" & election =="FV" |
                                                                            sch_treatment_grp_d1=="Treat 2nd period" & year =="2013" & election %in% c("KV","RV") ,-1, 
                                                                          ifelse(sch_treatment_grp_d1=="Treat 2nd period" & year =="2015" & election =="FV" |
                                                                                   sch_treatment_grp_d1=="Treat 2nd period" & year =="2017" & election %in% c("KV","RV") , 0,  
                                                                                 ifelse(sch_treatment_grp_d1=="Treat 2nd period" & year =="2019" & election =="FV" , 1,
                                                                                        ifelse(sch_treatment_grp_d1=="Treat 1st period" & year =="2007" & election =="FV" |
                                                                                                 sch_treatment_grp_d1=="Treat 1st period" & year =="2009" & election %in% c("KV","RV") ,-1, 
                                                                                               ifelse(sch_treatment_grp_d1=="Treat 1st period" & year =="2011" & election =="FV" |
                                                                                                        sch_treatment_grp_d1=="Treat 1st period" & year =="2013" & election %in% c("KV","RV") , 0, 
                                                                                                      ifelse(sch_treatment_grp_d1=="Treat 1st period" & year =="2015" & election =="FV" |
                                                                                                               sch_treatment_grp_d1=="Treat 1st period" & year =="2017" & election %in% c("KV","RV") , 1,  
                                                                                                             ifelse(sch_treatment_grp_d1=="Treat 1st period" & year =="2019" & election =="FV" , 2,NA)))))))))))))),
    schd2_relyear=ifelse(sch_treatment_grp_d2 %in% c("No school in precinct",  "Treated twice", "New school opened in precinct", "Treated before first election (no ref)"), NA,
                         ifelse(sch_treatment_grp_d2 %in% c("Control"), Inf, 
                                ifelse(sch_treatment_grp_d2=="Treat 3rd period" & year =="2007",-3,
                                       ifelse(sch_treatment_grp_d2=="Treat 3rd period" & year =="2011",-2,
                                              ifelse(sch_treatment_grp_d2=="Treat 3rd period" & year =="2015",-1,
                                                     ifelse(sch_treatment_grp_d2=="Treat 3rd period" & year =="2019",0,
                                                            ifelse(sch_treatment_grp_d2=="Treat 2nd period" & year =="2007" & election =="FV" |
                                                                     sch_treatment_grp_d2=="Treat 2nd period" & year =="2009" & election %in% c("KV","RV") ,-2, 
                                                                   ifelse(sch_treatment_grp_d2=="Treat 2nd period" & year =="2011" & election =="FV" |
                                                                            sch_treatment_grp_d2=="Treat 2nd period" & year =="2013" & election %in% c("KV","RV") ,-1, 
                                                                          ifelse(sch_treatment_grp_d2=="Treat 2nd period" & year =="2015" & election =="FV" |
                                                                                   sch_treatment_grp_d2=="Treat 2nd period" & year =="2017" & election %in% c("KV","RV") , 0,  
                                                                                 ifelse(sch_treatment_grp_d2=="Treat 2nd period" & year =="2019" & election =="FV" , 1,
                                                                                        ifelse(sch_treatment_grp_d2=="Treat 1st period" & year =="2007" & election =="FV" |
                                                                                                 sch_treatment_grp_d2=="Treat 1st period" & year =="2009" & election %in% c("KV","RV") ,-1, 
                                                                                               ifelse(sch_treatment_grp_d2=="Treat 1st period" & year =="2011" & election =="FV" |
                                                                                                        sch_treatment_grp_d2=="Treat 1st period" & year =="2013" & election %in% c("KV","RV") , 0, 
                                                                                                      ifelse(sch_treatment_grp_d2=="Treat 1st period" & year =="2015" & election =="FV" |
                                                                                                               sch_treatment_grp_d2=="Treat 1st period" & year =="2017" & election %in% c("KV","RV") , 1,  
                                                                                                             ifelse(sch_treatment_grp_d2=="Treat 1st period" & year =="2019" & election =="FV" , 2,NA)))))))))))))),
    schd3_relyear=ifelse(sch_treatment_grp_d3 %in% c("No school in precinct",  "Treated twice", "New school opened in precinct", "Treated before first election (no ref)"), NA,
                         ifelse(sch_treatment_grp_d3 %in% c("Control"), Inf, 
                                ifelse(sch_treatment_grp_d3=="Treat 3rd period" & year =="2007",-3,
                                       ifelse(sch_treatment_grp_d3=="Treat 3rd period" & year =="2011",-2,
                                              ifelse(sch_treatment_grp_d3=="Treat 3rd period" & year =="2015",-1,
                                                     ifelse(sch_treatment_grp_d3=="Treat 3rd period" & year =="2019",0,
                                                            ifelse(sch_treatment_grp_d3=="Treat 2nd period" & year =="2007" & election =="FV" |
                                                                     sch_treatment_grp_d3=="Treat 2nd period" & year =="2009" & election %in% c("KV","RV") ,-2, 
                                                                   ifelse(sch_treatment_grp_d3=="Treat 2nd period" & year =="2011" & election =="FV" |
                                                                            sch_treatment_grp_d3=="Treat 2nd period" & year =="2013" & election %in% c("KV","RV") ,-1, 
                                                                          ifelse(sch_treatment_grp_d3=="Treat 2nd period" & year =="2015" & election =="FV" |
                                                                                   sch_treatment_grp_d3=="Treat 2nd period" & year =="2017" & election %in% c("KV","RV") , 0,  
                                                                                 ifelse(sch_treatment_grp_d3=="Treat 2nd period" & year =="2019" & election =="FV" , 1,
                                                                                        ifelse(sch_treatment_grp_d3=="Treat 1st period" & year =="2007" & election =="FV" |
                                                                                                 sch_treatment_grp_d3=="Treat 1st period" & year =="2009" & election %in% c("KV","RV") ,-1, 
                                                                                               ifelse(sch_treatment_grp_d3=="Treat 1st period" & year =="2011" & election =="FV" |
                                                                                                        sch_treatment_grp_d3=="Treat 1st period" & year =="2013" & election %in% c("KV","RV") , 0, 
                                                                                                      ifelse(sch_treatment_grp_d3=="Treat 1st period" & year =="2015" & election =="FV" |
                                                                                                               sch_treatment_grp_d3=="Treat 1st period" & year =="2017" & election %in% c("KV","RV") , 1,  
                                                                                                             ifelse(sch_treatment_grp_d3=="Treat 1st period" & year =="2019" & election =="FV" , 2,NA)))))))))))))),
    schd4_relyear=ifelse(sch_treatment_grp_d4 %in% c("No school in precinct",  "Treated twice", "New school opened in precinct", "Treated before first election (no ref)"), NA,
                         ifelse(sch_treatment_grp_d4 %in% c("Control"), Inf, 
                                ifelse(sch_treatment_grp_d4=="Treat 3rd period" & year =="2007",-3,
                                       ifelse(sch_treatment_grp_d4=="Treat 3rd period" & year =="2011",-2,
                                              ifelse(sch_treatment_grp_d4=="Treat 3rd period" & year =="2015",-1,
                                                     ifelse(sch_treatment_grp_d4=="Treat 3rd period" & year =="2019",0,
                                                            ifelse(sch_treatment_grp_d4=="Treat 2nd period" & year =="2007" & election =="FV" |
                                                                     sch_treatment_grp_d4=="Treat 2nd period" & year =="2009" & election %in% c("KV","RV") ,-2, 
                                                                   ifelse(sch_treatment_grp_d4=="Treat 2nd period" & year =="2011" & election =="FV" |
                                                                            sch_treatment_grp_d4=="Treat 2nd period" & year =="2013" & election %in% c("KV","RV") ,-1, 
                                                                          ifelse(sch_treatment_grp_d4=="Treat 2nd period" & year =="2015" & election =="FV" |
                                                                                   sch_treatment_grp_d4=="Treat 2nd period" & year =="2017" & election %in% c("KV","RV") , 0,  
                                                                                 ifelse(sch_treatment_grp_d4=="Treat 2nd period" & year =="2019" & election =="FV" , 1,
                                                                                        ifelse(sch_treatment_grp_d4=="Treat 1st period" & year =="2007" & election =="FV" |
                                                                                                 sch_treatment_grp_d4=="Treat 1st period" & year =="2009" & election %in% c("KV","RV") ,-1, 
                                                                                               ifelse(sch_treatment_grp_d4=="Treat 1st period" & year =="2011" & election =="FV" |
                                                                                                        sch_treatment_grp_d4=="Treat 1st period" & year =="2013" & election %in% c("KV","RV") , 0, 
                                                                                                      ifelse(sch_treatment_grp_d4=="Treat 1st period" & year =="2015" & election =="FV" |
                                                                                                               sch_treatment_grp_d4=="Treat 1st period" & year =="2017" & election %in% c("KV","RV") , 1,  
                                                                                                             ifelse(sch_treatment_grp_d4=="Treat 1st period" & year =="2019" & election =="FV" , 2,NA)))))))))))))),
    hosd1_relyear=ifelse(treatment_grp_d1 %in% c("No school in precinct",  "Treated twice", "New hospital opened", "Treated before first election (no ref)"), NA,
                         ifelse(treatment_grp_d1 %in% c("Control"), Inf, 
                                ifelse(treatment_grp_d1=="Treat 3rd period" & year =="2007",-3,
                                       ifelse(treatment_grp_d1=="Treat 3rd period" & year =="2011",-2,
                                              ifelse(treatment_grp_d1=="Treat 3rd period" & year =="2015",-1,
                                                     ifelse(treatment_grp_d1=="Treat 3rd period" & year =="2019",0,
                                                            ifelse(treatment_grp_d1=="Treat 2nd period" & year =="2007" & election =="FV" |
                                                                     treatment_grp_d1=="Treat 2nd period" & year =="2009" & election %in% c("KV","RV") ,-2, 
                                                                   ifelse(treatment_grp_d1=="Treat 2nd period" & year =="2011" & election =="FV" |
                                                                            treatment_grp_d1=="Treat 2nd period" & year =="2013" & election %in% c("KV","RV") ,-1, 
                                                                          ifelse(treatment_grp_d1=="Treat 2nd period" & year =="2015" & election =="FV" |
                                                                                   treatment_grp_d1=="Treat 2nd period" & year =="2017" & election %in% c("KV","RV") , 0,  
                                                                                 ifelse(treatment_grp_d1=="Treat 2nd period" & year =="2019" & election =="FV" , 1,
                                                                                        ifelse(treatment_grp_d1=="Treat 1st period" & year =="2007" & election =="FV" |
                                                                                                 treatment_grp_d1=="Treat 1st period" & year =="2009" & election %in% c("KV","RV") ,-1, 
                                                                                               ifelse(treatment_grp_d1=="Treat 1st period" & year =="2011" & election =="FV" |
                                                                                                        treatment_grp_d1=="Treat 1st period" & year =="2013" & election %in% c("KV","RV") , 0, 
                                                                                                      ifelse(treatment_grp_d1=="Treat 1st period" & year =="2015" & election =="FV" |
                                                                                                               treatment_grp_d1=="Treat 1st period" & year =="2017" & election %in% c("KV","RV") , 1,  
                                                                                                             ifelse(treatment_grp_d1=="Treat 1st period" & year =="2019" & election =="FV" , 2,NA)))))))))))))),
    hosd2_relyear=ifelse(treatment_grp_d2 %in% c("No school in precinct",  "Treated twice", "New hospital opened", "Treated before first election (no ref)"), NA,
                         ifelse(treatment_grp_d2 %in% c("Control"), Inf, 
                                ifelse(treatment_grp_d2=="Treat 3rd period" & year =="2007",-3,
                                       ifelse(treatment_grp_d2=="Treat 3rd period" & year =="2011",-2,
                                              ifelse(treatment_grp_d2=="Treat 3rd period" & year =="2015",-1,
                                                     ifelse(treatment_grp_d2=="Treat 3rd period" & year =="2019",0,
                                                            ifelse(treatment_grp_d2=="Treat 2nd period" & year =="2007" & election =="FV" |
                                                                     treatment_grp_d2=="Treat 2nd period" & year =="2009" & election %in% c("KV","RV") ,-2, 
                                                                   ifelse(treatment_grp_d2=="Treat 2nd period" & year =="2011" & election =="FV" |
                                                                            treatment_grp_d2=="Treat 2nd period" & year =="2013" & election %in% c("KV","RV") ,-1, 
                                                                          ifelse(treatment_grp_d2=="Treat 2nd period" & year =="2015" & election =="FV" |
                                                                                   treatment_grp_d2=="Treat 2nd period" & year =="2017" & election %in% c("KV","RV") , 0,  
                                                                                 ifelse(treatment_grp_d2=="Treat 2nd period" & year =="2019" & election =="FV" , 1,
                                                                                        ifelse(treatment_grp_d2=="Treat 1st period" & year =="2007" & election =="FV" |
                                                                                                 treatment_grp_d2=="Treat 1st period" & year =="2009" & election %in% c("KV","RV") ,-1, 
                                                                                               ifelse(treatment_grp_d2=="Treat 1st period" & year =="2011" & election =="FV" |
                                                                                                        treatment_grp_d2=="Treat 1st period" & year =="2013" & election %in% c("KV","RV") , 0, 
                                                                                                      ifelse(treatment_grp_d2=="Treat 1st period" & year =="2015" & election =="FV" |
                                                                                                               treatment_grp_d2=="Treat 1st period" & year =="2017" & election %in% c("KV","RV") , 1,  
                                                                                                             ifelse(treatment_grp_d2=="Treat 1st period" & year =="2019" & election =="FV" , 2,NA)))))))))))))),
    hosd3_relyear=ifelse(treatment_grp_d3 %in% c("No school in precinct",  "Treated twice", "New hospital opened", "Treated before first election (no ref)"), NA,
                         ifelse(treatment_grp_d3 %in% c("Control"), Inf, 
                                ifelse(treatment_grp_d3=="Treat 3rd period" & year =="2007",-3,
                                       ifelse(treatment_grp_d3=="Treat 3rd period" & year =="2011",-2,
                                              ifelse(treatment_grp_d3=="Treat 3rd period" & year =="2015",-1,
                                                     ifelse(treatment_grp_d3=="Treat 3rd period" & year =="2019",0,
                                                            ifelse(treatment_grp_d3=="Treat 2nd period" & year =="2007" & election =="FV" |
                                                                     treatment_grp_d3=="Treat 2nd period" & year =="2009" & election %in% c("KV","RV") ,-2, 
                                                                   ifelse(treatment_grp_d3=="Treat 2nd period" & year =="2011" & election =="FV" |
                                                                            treatment_grp_d3=="Treat 2nd period" & year =="2013" & election %in% c("KV","RV") ,-1, 
                                                                          ifelse(treatment_grp_d3=="Treat 2nd period" & year =="2015" & election =="FV" |
                                                                                   treatment_grp_d3=="Treat 2nd period" & year =="2017" & election %in% c("KV","RV") , 0,  
                                                                                 ifelse(treatment_grp_d3=="Treat 2nd period" & year =="2019" & election =="FV" , 1,
                                                                                        ifelse(treatment_grp_d3=="Treat 1st period" & year =="2007" & election =="FV" |
                                                                                                 treatment_grp_d3=="Treat 1st period" & year =="2009" & election %in% c("KV","RV") ,-1, 
                                                                                               ifelse(treatment_grp_d3=="Treat 1st period" & year =="2011" & election =="FV" |
                                                                                                        treatment_grp_d3=="Treat 1st period" & year =="2013" & election %in% c("KV","RV") , 0, 
                                                                                                      ifelse(treatment_grp_d3=="Treat 1st period" & year =="2015" & election =="FV" |
                                                                                                               treatment_grp_d3=="Treat 1st period" & year =="2017" & election %in% c("KV","RV") , 1,  
                                                                                                             ifelse(treatment_grp_d3=="Treat 1st period" & year =="2019" & election =="FV" , 2,NA)))))))))))))),
    hosd4_relyear=ifelse(treatment_grp_d4 %in% c("Treated twice", "New hospital opened", "Treated before first election (no ref)"), NA,
                         ifelse(treatment_grp_d4 %in% c("Control"), Inf, 
                                ifelse(treatment_grp_d4=="Treat 3rd period" & year =="2007",-3,
                                       ifelse(treatment_grp_d4=="Treat 3rd period" & year =="2011",-2,
                                              ifelse(treatment_grp_d4=="Treat 3rd period" & year =="2015",-1,
                                                     ifelse(treatment_grp_d4=="Treat 3rd period" & year =="2019",0,
                                                            ifelse(treatment_grp_d4=="Treat 2nd period" & year =="2007" & election =="FV" |
                                                                     treatment_grp_d4=="Treat 2nd period" & year =="2009" & election %in% c("KV","RV") ,-2, 
                                                                   ifelse(treatment_grp_d4=="Treat 2nd period" & year =="2011" & election =="FV" |
                                                                            treatment_grp_d4=="Treat 2nd period" & year =="2013" & election %in% c("KV","RV") ,-1, 
                                                                          ifelse(treatment_grp_d4=="Treat 2nd period" & year =="2015" & election =="FV" |
                                                                                   treatment_grp_d4=="Treat 2nd period" & year =="2017" & election %in% c("KV","RV") , 0,  
                                                                                 ifelse(treatment_grp_d4=="Treat 2nd period" & year =="2019" & election =="FV" , 1,
                                                                                        ifelse(treatment_grp_d4=="Treat 1st period" & year =="2007" & election =="FV" |
                                                                                                 treatment_grp_d4=="Treat 1st period" & year =="2009" & election %in% c("KV","RV") ,-1, 
                                                                                               ifelse(treatment_grp_d4=="Treat 1st period" & year =="2011" & election =="FV" |
                                                                                                        treatment_grp_d4=="Treat 1st period" & year =="2013" & election %in% c("KV","RV") , 0, 
                                                                                                      ifelse(treatment_grp_d4=="Treat 1st period" & year =="2015" & election =="FV" |
                                                                                                               treatment_grp_d4=="Treat 1st period" & year =="2017" & election %in% c("KV","RV") , 1,  
                                                                                                             ifelse(treatment_grp_d4=="Treat 1st period" & year =="2019" & election =="FV" , 2,NA)))))))))))))),
    
    
    
  ) %>% 
{.}-> valgd

#Creation of datasets. I need four different datasets. I need to divide based on local vs national elections. And I need to divide based on dependent: RWP vs INC.
#RWP####

rwpdata <- valgd %>% 
  subset(year!=2005) %>% 
  transmute(
    afstemid,
    komnr,
    election,
    year, 
    ul.votes,
    ul_sup=spp.votes/valid.votes,
    spp.votes,
    spp_sup=spp.votes/valid.votes,
    socdem.votes,
    socdem_sup=socdem.votes/valid.votes,
    rlib.votes,
    rlib_sup=rlib.votes/valid.votes,
    lib.votes,
    lib_sup=lib.votes/valid.votes,
    cons.votes=cons.votes,
    cons_sup=cons.votes/valid.votes,
    rwp.votes,
    rwp_sup=rwp.votes/valid.votes,
    turnout=total.votes/eligible.votes,
    eligible.votes,
    #School closed in presinct (schp)
    schp_group = treatment_grp,
    schp_relyear=schp_relyear,
    schp_treat = schp_treat,
    schp_g = schp_g,
    `schp_relyear[-3]`=ifelse(schp_relyear==-3,1,0),
    `schp_relyear[-2]`=ifelse(schp_relyear==-2,1,0),
    `schp_relyear[-1]`=ifelse(schp_relyear==-1 | schp_relyear==Inf ,1,0),
    `schp_relyear[+0]`=ifelse(schp_relyear==0,1,0),
    `schp_relyear[+1]`=ifelse(schp_relyear==1,1,0),
    `schp_relyear[+2]`=ifelse(schp_relyear==2,1,0),
    #distance to school(s) increased (schd#_)
    schd1_group = sch_treatment_grp_d1,
    schd1_relyear = schd1_relyear,
    schd1_treat = schd1_treat,
    schd1_g = schd1_g,
    schd1_deltadist = sch_delta_dist_d1, 
    `schd1_relyear[-3]`=ifelse(schd1_relyear==-3,1,0),
    `schd1_relyear[-2]`=ifelse(schd1_relyear==-2,1,0),
    `schd1_relyear[-1]`=ifelse(schd1_relyear==-1 | schd1_relyear==Inf ,1,0),
    `schd1_relyear[+0]`=ifelse(schd1_relyear==0,1,0),
    `schd1_relyear[+1]`=ifelse(schd1_relyear==1,1,0),
    `schd1_relyear[+2]`=ifelse(schd1_relyear==2,1,0),
    schd2_group = sch_treatment_grp_d2,
    schd2_relyear = schd2_relyear,
    schd2_treat = schd2_treat,
    schd2_g = schd2_g,
    schd2_deltadist = sch_delta_dist_d2, 
    `schd2_relyear[-3]`=ifelse(schd2_relyear==-3,1,0),
    `schd2_relyear[-2]`=ifelse(schd2_relyear==-2,1,0),
    `schd2_relyear[-1]`=ifelse(schd2_relyear==-1 | schd2_relyear==Inf ,1,0),
    `schd2_relyear[+0]`=ifelse(schd2_relyear==0,1,0),
    `schd2_relyear[+1]`=ifelse(schd2_relyear==1,1,0),
    `schd2_relyear[+2]`=ifelse(schd2_relyear==2,1,0),
    schd3_group = sch_treatment_grp_d3,
    schd3_relyear = schd3_relyear,
    schd3_treat = schd3_treat,
    schd3_g = schd3_g,
    schd3_deltadist = sch_delta_dist_d3, 
    `schd3_relyear[-3]`=ifelse(schd3_relyear==-3,1,0),
    `schd3_relyear[-2]`=ifelse(schd3_relyear==-2,1,0),
    `schd3_relyear[-1]`=ifelse(schd3_relyear==-1 | schd3_relyear==Inf ,1,0),
    `schd3_relyear[+0]`=ifelse(schd3_relyear==0,1,0),
    `schd3_relyear[+1]`=ifelse(schd3_relyear==1,1,0),
    `schd3_relyear[+2]`=ifelse(schd3_relyear==2,1,0),
    schd4_group = sch_treatment_grp_d4,
    schd4_relyear = schd4_relyear,
    schd4_treat = schd4_treat,
    schd4_g = schd4_g,
    schd4_deltadist = sch_delta_dist_d4, 
    `schd4_relyear[-3]`=ifelse(schd4_relyear==-3,1,0),
    `schd4_relyear[-2]`=ifelse(schd4_relyear==-2,1,0),
    `schd4_relyear[-1]`=ifelse(schd4_relyear==-1 | schd4_relyear==Inf ,1,0),
    `schd4_relyear[+0]`=ifelse(schd4_relyear==0,1,0),
    `schd4_relyear[+1]`=ifelse(schd4_relyear==1,1,0),
    `schd4_relyear[+2]`=ifelse(schd4_relyear==2,1,0),
    #distance to hospital(s) increased (hosd#_)
    hosd1_group = treatment_grp_d1,
    hosd1_relyear = hosd1_relyear,
    hosd1_treat = hosd1_treat,
    hosd1_g = hosd1_g,
    hosd1_deltadist = delta_dist_d1, 
    `hosd1_relyear[-3]`=ifelse(hosd1_relyear==-3,1,0),
    `hosd1_relyear[-2]`=ifelse(hosd1_relyear==-2,1,0),
    `hosd1_relyear[-1]`=ifelse(hosd1_relyear==-1 | hosd1_relyear==Inf ,1,0),
    `hosd1_relyear[+0]`=ifelse(hosd1_relyear==0,1,0),
    `hosd1_relyear[+1]`=ifelse(hosd1_relyear==1,1,0),
    `hosd1_relyear[+2]`=ifelse(hosd1_relyear==2,1,0),
    hosd2_group = treatment_grp_d2,
    hosd2_relyear = hosd2_relyear,
    hosd2_treat = hosd2_treat,
    hosd2_g = hosd2_g,
    hosd2_deltadist = delta_dist_d2, 
    `hosd2_relyear[-3]`=ifelse(hosd2_relyear==-3,1,0),
    `hosd2_relyear[-2]`=ifelse(hosd2_relyear==-2,1,0),
    `hosd2_relyear[-1]`=ifelse(hosd2_relyear==-1 | hosd2_relyear==Inf ,1,0),
    `hosd2_relyear[+0]`=ifelse(hosd2_relyear==0,1,0),
    `hosd2_relyear[+1]`=ifelse(hosd2_relyear==1,1,0),
    `hosd2_relyear[+2]`=ifelse(hosd2_relyear==2,1,0),
    hosd3_group = treatment_grp_d3,
    hosd3_relyear = hosd3_relyear,
    hosd3_treat = hosd3_treat,
    hosd3_g = hosd3_g,
    hosd3_deltadist = delta_dist_d3, 
    `hosd3_relyear[-3]`=ifelse(hosd3_relyear==-3,1,0),
    `hosd3_relyear[-2]`=ifelse(hosd3_relyear==-2,1,0),
    `hosd3_relyear[-1]`=ifelse(hosd3_relyear==-1 | hosd3_relyear==Inf ,1,0),
    `hosd3_relyear[+0]`=ifelse(hosd3_relyear==0,1,0),
    `hosd3_relyear[+1]`=ifelse(hosd3_relyear==1,1,0),
    `hosd3_relyear[+2]`=ifelse(hosd3_relyear==2,1,0),
    hosd4_group = treatment_grp_d4,
    hosd4_relyear = hosd4_relyear,
    hosd4_treat = hosd4_treat,
    hosd4_g = hosd4_g,
    hosd4_deltadist = delta_dist_d4, 
    `hosd4_relyear[-3]`=ifelse(hosd4_relyear==-3,1,0),
    `hosd4_relyear[-2]`=ifelse(hosd4_relyear==-2,1,0),
    `hosd4_relyear[-1]`=ifelse(hosd4_relyear==-1 | hosd4_relyear==Inf ,1,0),
    `hosd4_relyear[+0]`=ifelse(hosd4_relyear==0,1,0),
    `hosd4_relyear[+1]`=ifelse(hosd4_relyear==1,1,0),
    `hosd4_relyear[+2]`=ifelse(hosd4_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(rwpdata, "rwpdata.csv")
  
#INC#####
##schp####
incdata_schp <-  valgd %>% 
  subset(year!=2005) %>% 
  subset(treatment_grp %in%  c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
###LE####
incdata_schp_le <- incdata_schp %>% 
  subset(election == "KV") 
####t###
incdata_schp_le_t <- incdata_schp_le %>% 
  subset(treatment_grp %in%  c("Treat 1st period", "Treat 2nd period")) %>% 
  mutate(party=ifelse(treatment_grp=="Treat 1st period" & year %in% c(2013), incumbent,
                      ifelse(treatment_grp=="Treat 2nd period" & year %in% c(2017), incumbent, NA ))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = ifelse(treatment_grp=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(treatment_grp=="Treat 2nd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_schp_le_t_1 <- incdata_schp_le_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_schp_le_t_2 <- incdata_schp_le_t %>% 
  subset(grp_2nd==1)
####c###
incdata_schp_le_c <- incdata_schp_le %>% 
  subset(treatment_grp %in%  c("Control"))
#####1st###
incdata_schp_le_c_1 <- incdata_schp_le_c %>% 
  mutate(party=ifelse(year %in% c(2013), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_schp_le_c_2 <- incdata_schp_le_c %>% 
  mutate(party=ifelse(year %in% c(2017), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_schp_le <- bind_rows(incdata_schp_le_t_1, incdata_schp_le_t_2, incdata_schp_le_c_1, incdata_schp_le_c_2) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    #School closed in presinct (schp)
    schp_group = treatment_grp,
    schp_relyear = schp_relyear,
    schp_treat = schp_treat,
    schp_g = schp_g,
    `schp_relyear[-3]`=ifelse(schp_relyear==-3,1,0),
    `schp_relyear[-2]`=ifelse(schp_relyear==-2,1,0),
    `schp_relyear[-1]`=ifelse(schp_relyear==-1 | schp_relyear==Inf ,1,0),
    `schp_relyear[+0]`=ifelse(schp_relyear==0,1,0),
    `schp_relyear[+1]`=ifelse(schp_relyear==1,1,0),
    `schp_relyear[+2]`=ifelse(schp_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_schp_le, "incdata_schp_le.csv")

###NE####
incdata_schp_ne <- incdata_schp %>% 
  subset(election == "FV") 
####t###
incdata_schp_ne_t <- incdata_schp_ne %>% 
  subset(treatment_grp %in%  c("Treat 1st period", "Treat 2nd period", "Treat 3rd period")) %>% 
  mutate(party=ifelse(treatment_grp=="Treat 1st period" & year %in% c(2011), "VK",
                      ifelse(treatment_grp=="Treat 2nd period" & year %in% c(2015), "SRSF",
                             ifelse(treatment_grp=="Treat 3rd period" & year %in% c(2019), "VKLA", NA)))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(treatment_grp=="Treat 1st period" & year %in% c(2007, 2011, 2019), 1,
                         ifelse(treatment_grp=="Treat 2nd period" & year %in% c(2015), 1,
                                ifelse(treatment_grp=="Treat 3rd period" & year %in% c(2007, 2011, 2019), 1, 0))),
  grp_1st = ifelse(treatment_grp=="Treat 1st period", 1, 0),
  grp_2nd = ifelse(treatment_grp=="Treat 2nd period", 1, 0),
  grp_3rd = ifelse(treatment_grp=="Treat 3rd period", 1, 0),
  afstemid_p=paste0(afstemid, party))
#####1st###
incdata_schp_ne_t_1 <- incdata_schp_ne_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_schp_ne_t_2 <- incdata_schp_ne_t %>% 
  subset(grp_2nd==1)
#####3rd###
incdata_schp_ne_t_3 <- incdata_schp_ne_t %>% 
  subset(grp_3rd==1)
####c###
incdata_schp_ne_c <- incdata_schp_ne %>% 
  subset(treatment_grp %in%  c("Control"))
#####1st###
incdata_schp_ne_c_1 <- incdata_schp_ne_c %>% 
  mutate(party=ifelse(year %in% c(2011), "VK", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_schp_ne_c_2 <-  incdata_schp_ne_c %>% 
  mutate(party=ifelse(year %in% c(2015), "SRSF", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2015), 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####3rd###
incdata_schp_ne_c_3 <-  incdata_schp_ne_c %>% 
  mutate(party=ifelse(year %in% c(2019), "VKLA", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 0,
         grp_2nd = 0,
         grp_3rd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_schp_ne <- bind_rows(incdata_schp_ne_t_1, incdata_schp_ne_t_2, incdata_schp_ne_t_3, incdata_schp_ne_c_1, incdata_schp_ne_c_2, incdata_schp_ne_c_3) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    grp_3rd,
    #School closed in presinct (schp)
    schp_group = treatment_grp,
    schp_relyear = schp_relyear,
    schp_treat = schp_treat,
    schp_g = schp_g,
    `schp_relyear[-3]`=ifelse(schp_relyear==-3,1,0),
    `schp_relyear[-2]`=ifelse(schp_relyear==-2,1,0),
    `schp_relyear[-1]`=ifelse(schp_relyear==-1 | schp_relyear==Inf ,1,0),
    `schp_relyear[+0]`=ifelse(schp_relyear==0,1,0),
    `schp_relyear[+1]`=ifelse(schp_relyear==1,1,0),
    `schp_relyear[+2]`=ifelse(schp_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_schp_ne, "incdata_schp_ne.csv")

##schd1####
incdata_schd1 <-  valgd %>% 
  subset(year!=2005) %>% 
  subset(sch_treatment_grp_d1 %in%  c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
###LE####
incdata_schd1_le <- incdata_schd1 %>% 
  subset(election == "KV") 
####t###
incdata_schd1_le_t <- incdata_schd1_le %>% 
  subset(sch_treatment_grp_d1 %in%  c("Treat 1st period", "Treat 2nd period")) %>% 
  mutate(party=ifelse(sch_treatment_grp_d1=="Treat 1st period" & year %in% c(2013), incumbent,
                      ifelse(sch_treatment_grp_d1=="Treat 2nd period" & year %in% c(2017), incumbent, NA ))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = ifelse(sch_treatment_grp_d1=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(sch_treatment_grp_d1=="Treat 2nd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_schd1_le_t_1 <- incdata_schd1_le_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_schd1_le_t_2 <- incdata_schd1_le_t %>% 
  subset(grp_2nd==1)
####c###
incdata_schd1_le_c <- incdata_schd1_le %>% 
  subset(sch_treatment_grp_d1 %in%  c("Control"))
#####1st###
incdata_schd1_le_c_1 <- incdata_schd1_le_c %>% 
  mutate(party=ifelse(year %in% c(2013), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_schd1_le_c_2 <- incdata_schd1_le_c %>% 
  mutate(party=ifelse(year %in% c(2017), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_schd1_le <- bind_rows(incdata_schd1_le_t_1, incdata_schd1_le_t_2, incdata_schd1_le_c_1, incdata_schd1_le_c_2) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    #distance to school(s) increased (schd#_)
    schd1_group = sch_treatment_grp_d1,
    schd1_relyear = schd1_relyear,
    schd1_treat = schd1_treat,
    schd1_g = schd1_g,
    schd1_deltadist = sch_delta_dist_d1, 
    `schd1_relyear[-3]`=ifelse(schd1_relyear==-3,1,0),
    `schd1_relyear[-2]`=ifelse(schd1_relyear==-2,1,0),
    `schd1_relyear[-1]`=ifelse(schd1_relyear==-1 | schd1_relyear==Inf ,1,0),
    `schd1_relyear[+0]`=ifelse(schd1_relyear==0,1,0),
    `schd1_relyear[+1]`=ifelse(schd1_relyear==1,1,0),
    `schd1_relyear[+2]`=ifelse(schd1_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_schd1_le, "incdata_schd1_le.csv")

###NE####
incdata_schd1_ne <- incdata_schd1 %>% 
  subset(election == "FV") 
####t###
incdata_schd1_ne_t <- incdata_schd1_ne %>% 
  subset(sch_treatment_grp_d1 %in%  c("Treat 1st period", "Treat 2nd period", "Treat 3rd period")) %>% 
  mutate(party=ifelse(sch_treatment_grp_d1=="Treat 1st period" & year %in% c(2011), "VK",
                      ifelse(sch_treatment_grp_d1=="Treat 2nd period" & year %in% c(2015), "SRSF",
                             ifelse(sch_treatment_grp_d1=="Treat 3rd period" & year %in% c(2019), "VKLA", NA)))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(sch_treatment_grp_d1=="Treat 1st period" & year %in% c(2007, 2011, 2019), 1,
                         ifelse(sch_treatment_grp_d1=="Treat 2nd period" & year %in% c(2015), 1,
                                ifelse(sch_treatment_grp_d1=="Treat 3rd period" & year %in% c(2007, 2011, 2019), 1, 0))),
         grp_1st = ifelse(sch_treatment_grp_d1=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(sch_treatment_grp_d1=="Treat 2nd period", 1, 0),
         grp_3rd = ifelse(sch_treatment_grp_d1=="Treat 3rd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_schd1_ne_t_1 <- incdata_schd1_ne_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_schd1_ne_t_2 <- incdata_schd1_ne_t %>% 
  subset(grp_2nd==1)
#####3rd###
incdata_schd1_ne_t_3 <- incdata_schd1_ne_t %>% 
  subset(grp_3rd==1)
####c###
incdata_schd1_ne_c <- incdata_schd1_ne %>% 
  subset(sch_treatment_grp_d1 %in%  c("Control"))
#####1st###
incdata_schd1_ne_c_1 <- incdata_schd1_ne_c %>% 
  mutate(party=ifelse(year %in% c(2011), "VK", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_schd1_ne_c_2 <-  incdata_schd1_ne_c %>% 
  mutate(party=ifelse(year %in% c(2015), "SRSF", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2015), 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####3rd###
incdata_schd1_ne_c_3 <-  incdata_schd1_ne_c %>% 
  mutate(party=ifelse(year %in% c(2019), "VKLA", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 0,
         grp_2nd = 0,
         grp_3rd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_schd1_ne <- bind_rows(incdata_schd1_ne_t_1, incdata_schd1_ne_t_2, incdata_schd1_ne_t_3, incdata_schd1_ne_c_1, incdata_schd1_ne_c_2, incdata_schd1_ne_c_3) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    grp_3rd,
    #distance to school(s) increased (schd#_)
    schd1_group = sch_treatment_grp_d1,
    schd1_relyear = schd1_relyear,
    schd1_treat = schd1_treat,
    schd1_g = schd1_g,
    schd1_deltadist = sch_delta_dist_d1, 
    `schd1_relyear[-3]`=ifelse(schd1_relyear==-3,1,0),
    `schd1_relyear[-2]`=ifelse(schd1_relyear==-2,1,0),
    `schd1_relyear[-1]`=ifelse(schd1_relyear==-1 | schd1_relyear==Inf ,1,0),
    `schd1_relyear[+0]`=ifelse(schd1_relyear==0,1,0),
    `schd1_relyear[+1]`=ifelse(schd1_relyear==1,1,0),
    `schd1_relyear[+2]`=ifelse(schd1_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
    
  )
write_csv2(incdata_schd1_ne, "incdata_schd1_ne.csv")

##schd2####
incdata_schd2 <-  valgd %>% 
  subset(year!=2005) %>% 
  subset(sch_treatment_grp_d2 %in%  c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
###LE####
incdata_schd2_le <- incdata_schd2 %>% 
  subset(election == "KV") 
####t###
incdata_schd2_le_t <- incdata_schd2_le %>% 
  subset(sch_treatment_grp_d2 %in%  c("Treat 1st period", "Treat 2nd period")) %>% 
  mutate(party=ifelse(sch_treatment_grp_d2=="Treat 1st period" & year %in% c(2013), incumbent,
                      ifelse(sch_treatment_grp_d2=="Treat 2nd period" & year %in% c(2017), incumbent, NA ))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = ifelse(sch_treatment_grp_d2=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(sch_treatment_grp_d2=="Treat 2nd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_schd2_le_t_1 <- incdata_schd2_le_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_schd2_le_t_2 <- incdata_schd2_le_t %>% 
  subset(grp_2nd==1)
####c###
incdata_schd2_le_c <- incdata_schd2_le %>% 
  subset(sch_treatment_grp_d2 %in%  c("Control"))
#####1st###
incdata_schd2_le_c_1 <- incdata_schd2_le_c %>% 
  mutate(party=ifelse(year %in% c(2013), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_schd2_le_c_2 <- incdata_schd2_le_c %>% 
  mutate(party=ifelse(year %in% c(2017), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_schd2_le <- bind_rows(incdata_schd2_le_t_1, incdata_schd2_le_t_2, incdata_schd2_le_c_1, incdata_schd2_le_c_2) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    #distance to school(s) increased (schd#_)
    schd2_group = sch_treatment_grp_d2,
    schd2_relyear = schd2_relyear,
    schd2_treat = schd2_treat,
    schd2_g = schd2_g,
    schd2_deltadist = sch_delta_dist_d2, 
    `schd2_relyear[-3]`=ifelse(schd2_relyear==-3,1,0),
    `schd2_relyear[-2]`=ifelse(schd2_relyear==-2,1,0),
    `schd2_relyear[-1]`=ifelse(schd2_relyear==-1 | schd2_relyear==Inf ,1,0),
    `schd2_relyear[+0]`=ifelse(schd2_relyear==0,1,0),
    `schd2_relyear[+1]`=ifelse(schd2_relyear==1,1,0),
    `schd2_relyear[+2]`=ifelse(schd2_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_schd2_le, "incdata_schd2_le.csv")

###NE####
incdata_schd2_ne <- incdata_schd2 %>% 
  subset(election == "FV") 
####t###
incdata_schd2_ne_t <- incdata_schd2_ne %>% 
  subset(sch_treatment_grp_d2 %in%  c("Treat 1st period", "Treat 2nd period", "Treat 3rd period")) %>% 
  mutate(party=ifelse(sch_treatment_grp_d2=="Treat 1st period" & year %in% c(2011), "VK",
                      ifelse(sch_treatment_grp_d2=="Treat 2nd period" & year %in% c(2015), "SRSF",
                             ifelse(sch_treatment_grp_d2=="Treat 3rd period" & year %in% c(2019), "VKLA", NA)))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(sch_treatment_grp_d2=="Treat 1st period" & year %in% c(2007, 2011, 2019), 1,
                         ifelse(sch_treatment_grp_d2=="Treat 2nd period" & year %in% c(2015), 1,
                                ifelse(sch_treatment_grp_d2=="Treat 3rd period" & year %in% c(2007, 2011, 2019), 1, 0))),
         grp_1st = ifelse(sch_treatment_grp_d2=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(sch_treatment_grp_d2=="Treat 2nd period", 1, 0),
         grp_3rd = ifelse(sch_treatment_grp_d2=="Treat 3rd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_schd2_ne_t_1 <- incdata_schd2_ne_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_schd2_ne_t_2 <- incdata_schd2_ne_t %>% 
  subset(grp_2nd==1)
#####3rd###
incdata_schd2_ne_t_3 <- incdata_schd2_ne_t %>% 
  subset(grp_3rd==1)
####c###
incdata_schd2_ne_c <- incdata_schd2_ne %>% 
  subset(sch_treatment_grp_d2 %in%  c("Control"))
#####1st###
incdata_schd2_ne_c_1 <- incdata_schd2_ne_c %>% 
  mutate(party=ifelse(year %in% c(2011), "VK", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_schd2_ne_c_2 <-  incdata_schd2_ne_c %>% 
  mutate(party=ifelse(year %in% c(2015), "SRSF", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2015), 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####3rd###
incdata_schd2_ne_c_3 <-  incdata_schd2_ne_c %>% 
  mutate(party=ifelse(year %in% c(2019), "VKLA", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 0,
         grp_2nd = 0,
         grp_3rd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_schd2_ne <- bind_rows(incdata_schd2_ne_t_1, incdata_schd2_ne_t_2, incdata_schd2_ne_t_3, incdata_schd2_ne_c_1, incdata_schd2_ne_c_2, incdata_schd2_ne_c_3) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    grp_3rd,
    #distance to school(s) increased (schd#_)
    schd2_group = sch_treatment_grp_d2,
    schd2_relyear = schd2_relyear,
    schd2_treat = schd2_treat,
    schd2_g = schd2_g,
    schd2_deltadist = sch_delta_dist_d2, 
    `schd2_relyear[-3]`=ifelse(schd2_relyear==-3,1,0),
    `schd2_relyear[-2]`=ifelse(schd2_relyear==-2,1,0),
    `schd2_relyear[-1]`=ifelse(schd2_relyear==-1 | schd2_relyear==Inf ,1,0),
    `schd2_relyear[+0]`=ifelse(schd2_relyear==0,1,0),
    `schd2_relyear[+1]`=ifelse(schd2_relyear==1,1,0),
    `schd2_relyear[+2]`=ifelse(schd2_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_schd2_ne, "incdata_schd2_ne.csv")

##schd3####
incdata_schd3 <-  valgd %>% 
  subset(year!=2005) %>% 
  subset(sch_treatment_grp_d3 %in%  c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
###LE####
incdata_schd3_le <- incdata_schd3 %>% 
  subset(election == "KV") 
####t###
incdata_schd3_le_t <- incdata_schd3_le %>% 
  subset(sch_treatment_grp_d3 %in%  c("Treat 1st period", "Treat 2nd period")) %>% 
  mutate(party=ifelse(sch_treatment_grp_d3=="Treat 1st period" & year %in% c(2013), incumbent,
                      ifelse(sch_treatment_grp_d3=="Treat 2nd period" & year %in% c(2017), incumbent, NA ))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = ifelse(sch_treatment_grp_d3=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(sch_treatment_grp_d3=="Treat 2nd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_schd3_le_t_1 <- incdata_schd3_le_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_schd3_le_t_2 <- incdata_schd3_le_t %>% 
  subset(grp_2nd==1)
####c###
incdata_schd3_le_c <- incdata_schd3_le %>% 
  subset(sch_treatment_grp_d3 %in%  c("Control"))
#####1st###
incdata_schd3_le_c_1 <- incdata_schd3_le_c %>% 
  mutate(party=ifelse(year %in% c(2013), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_schd3_le_c_2 <- incdata_schd3_le_c %>% 
  mutate(party=ifelse(year %in% c(2017), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_schd3_le <- bind_rows(incdata_schd3_le_t_1, incdata_schd3_le_t_2, incdata_schd3_le_c_1, incdata_schd3_le_c_2) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    #distance to school(s) increased (schd#_)
    schd3_group = sch_treatment_grp_d3,
    schd3_relyear = schd3_relyear,
    schd3_treat = schd3_treat,
    schd3_g = schd3_g,
    schd3_deltadist = sch_delta_dist_d3, 
    `schd3_relyear[-3]`=ifelse(schd3_relyear==-3,1,0),
    `schd3_relyear[-2]`=ifelse(schd3_relyear==-2,1,0),
    `schd3_relyear[-1]`=ifelse(schd3_relyear==-1 | schd3_relyear==Inf ,1,0),
    `schd3_relyear[+0]`=ifelse(schd3_relyear==0,1,0),
    `schd3_relyear[+1]`=ifelse(schd3_relyear==1,1,0),
    `schd3_relyear[+2]`=ifelse(schd3_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_schd3_le, "incdata_schd3_le.csv")

###NE####
incdata_schd3_ne <- incdata_schd3 %>% 
  subset(election == "FV") 
####t###
incdata_schd3_ne_t <- incdata_schd3_ne %>% 
  subset(sch_treatment_grp_d3 %in%  c("Treat 1st period", "Treat 2nd period", "Treat 3rd period")) %>% 
  mutate(party=ifelse(sch_treatment_grp_d3=="Treat 1st period" & year %in% c(2011), "VK",
                      ifelse(sch_treatment_grp_d3=="Treat 2nd period" & year %in% c(2015), "SRSF",
                             ifelse(sch_treatment_grp_d3=="Treat 3rd period" & year %in% c(2019), "VKLA", NA)))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(sch_treatment_grp_d3=="Treat 1st period" & year %in% c(2007, 2011, 2019), 1,
                         ifelse(sch_treatment_grp_d3=="Treat 2nd period" & year %in% c(2015), 1,
                                ifelse(sch_treatment_grp_d3=="Treat 3rd period" & year %in% c(2007, 2011, 2019), 1, 0))),
         grp_1st = ifelse(sch_treatment_grp_d3=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(sch_treatment_grp_d3=="Treat 2nd period", 1, 0),
         grp_3rd = ifelse(sch_treatment_grp_d3=="Treat 3rd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_schd3_ne_t_1 <- incdata_schd3_ne_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_schd3_ne_t_2 <- incdata_schd3_ne_t %>% 
  subset(grp_2nd==1)
#####3rd###
incdata_schd3_ne_t_3 <- incdata_schd3_ne_t %>% 
  subset(grp_3rd==1)
####c###
incdata_schd3_ne_c <- incdata_schd3_ne %>% 
  subset(sch_treatment_grp_d3 %in%  c("Control"))
#####1st###
incdata_schd3_ne_c_1 <- incdata_schd3_ne_c %>% 
  mutate(party=ifelse(year %in% c(2011), "VK", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_schd3_ne_c_2 <-  incdata_schd3_ne_c %>% 
  mutate(party=ifelse(year %in% c(2015), "SRSF", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2015), 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####3rd###
incdata_schd3_ne_c_3 <-  incdata_schd3_ne_c %>% 
  mutate(party=ifelse(year %in% c(2019), "VKLA", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 0,
         grp_2nd = 0,
         grp_3rd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_schd3_ne <- bind_rows(incdata_schd3_ne_t_1, incdata_schd3_ne_t_2, incdata_schd3_ne_t_3, incdata_schd3_ne_c_1, incdata_schd3_ne_c_2, incdata_schd3_ne_c_3) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    grp_3rd,
    #distance to school(s) increased (schd#_)
    schd3_group = sch_treatment_grp_d3,
    schd3_relyear = schd3_relyear,
    schd3_treat = schd3_treat,
    schd3_g = schd3_g,
    schd3_deltadist = sch_delta_dist_d3, 
    `schd3_relyear[-3]`=ifelse(schd3_relyear==-3,1,0),
    `schd3_relyear[-2]`=ifelse(schd3_relyear==-2,1,0),
    `schd3_relyear[-1]`=ifelse(schd3_relyear==-1 | schd3_relyear==Inf ,1,0),
    `schd3_relyear[+0]`=ifelse(schd3_relyear==0,1,0),
    `schd3_relyear[+1]`=ifelse(schd3_relyear==1,1,0),
    `schd3_relyear[+2]`=ifelse(schd3_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
    
  )
write_csv2(incdata_schd3_ne, "incdata_schd3_ne.csv")

##schd4####
incdata_schd4 <-  valgd %>% 
  subset(year!=2005) %>% 
  subset(sch_treatment_grp_d4 %in%  c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
###LE####
incdata_schd4_le <- incdata_schd4 %>% 
  subset(election == "KV") 
####t###
incdata_schd4_le_t <- incdata_schd4_le %>% 
  subset(sch_treatment_grp_d4 %in%  c("Treat 1st period", "Treat 2nd period")) %>% 
  mutate(party=ifelse(sch_treatment_grp_d4=="Treat 1st period" & year %in% c(2013), incumbent,
                      ifelse(sch_treatment_grp_d4=="Treat 2nd period" & year %in% c(2017), incumbent, NA ))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = ifelse(sch_treatment_grp_d4=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(sch_treatment_grp_d4=="Treat 2nd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_schd4_le_t_1 <- incdata_schd4_le_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_schd4_le_t_2 <- incdata_schd4_le_t %>% 
  subset(grp_2nd==1)
####c###
incdata_schd4_le_c <- incdata_schd4_le %>% 
  subset(sch_treatment_grp_d4 %in%  c("Control"))
#####1st###
incdata_schd4_le_c_1 <- incdata_schd4_le_c %>% 
  mutate(party=ifelse(year %in% c(2013), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_schd4_le_c_2 <- incdata_schd4_le_c %>% 
  mutate(party=ifelse(year %in% c(2017), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_schd4_le <- bind_rows(incdata_schd4_le_t_1, incdata_schd4_le_t_2, incdata_schd4_le_c_1, incdata_schd4_le_c_2) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
    transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    #distance to school(s) increased (schd#_)
    schd4_group = sch_treatment_grp_d4,
    schd4_relyear = schd4_relyear,
    schd4_treat = schd4_treat,
    schd4_g = schd4_g,
    schd4_deltadist = sch_delta_dist_d4, 
    `schd4_relyear[-3]`=ifelse(schd4_relyear==-3,1,0),
    `schd4_relyear[-2]`=ifelse(schd4_relyear==-2,1,0),
    `schd4_relyear[-1]`=ifelse(schd4_relyear==-1 | schd4_relyear==Inf ,1,0),
    `schd4_relyear[+0]`=ifelse(schd4_relyear==0,1,0),
    `schd4_relyear[+1]`=ifelse(schd4_relyear==1,1,0),
    `schd4_relyear[+2]`=ifelse(schd4_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
    )
write_csv2(incdata_schd4_le, "incdata_schd4_le.csv")

###NE####
incdata_schd4_ne <- incdata_schd4 %>% 
  subset(election == "FV") 
####t###
incdata_schd4_ne_t <- incdata_schd4_ne %>% 
  subset(sch_treatment_grp_d4 %in%  c("Treat 1st period", "Treat 2nd period", "Treat 3rd period")) %>% 
  mutate(party=ifelse(sch_treatment_grp_d4=="Treat 1st period" & year %in% c(2011), "VK",
                      ifelse(sch_treatment_grp_d4=="Treat 2nd period" & year %in% c(2015), "SRSF",
                             ifelse(sch_treatment_grp_d4=="Treat 3rd period" & year %in% c(2019), "VKLA", NA)))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(sch_treatment_grp_d4=="Treat 1st period" & year %in% c(2007, 2011, 2019), 1,
                         ifelse(sch_treatment_grp_d4=="Treat 2nd period" & year %in% c(2015), 1,
                                ifelse(sch_treatment_grp_d4=="Treat 3rd period" & year %in% c(2007, 2011, 2019), 1, 0))),
         grp_1st = ifelse(sch_treatment_grp_d4=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(sch_treatment_grp_d4=="Treat 2nd period", 1, 0),
         grp_3rd = ifelse(sch_treatment_grp_d4=="Treat 3rd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_schd4_ne_t_1 <- incdata_schd4_ne_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_schd4_ne_t_2 <- incdata_schd4_ne_t %>% 
  subset(grp_2nd==1)
#####3rd###
incdata_schd4_ne_t_3 <- incdata_schd4_ne_t %>% 
  subset(grp_3rd==1)
####c###
incdata_schd4_ne_c <- incdata_schd4_ne %>% 
  subset(sch_treatment_grp_d4 %in%  c("Control"))
#####1st###
incdata_schd4_ne_c_1 <- incdata_schd4_ne_c %>% 
  mutate(party=ifelse(year %in% c(2011), "VK", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_schd4_ne_c_2 <-  incdata_schd4_ne_c %>% 
  mutate(party=ifelse(year %in% c(2015), "SRSF", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2015), 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####3rd###
incdata_schd4_ne_c_3 <-  incdata_schd4_ne_c %>% 
  mutate(party=ifelse(year %in% c(2019), "VKLA", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 0,
         grp_2nd = 0,
         grp_3rd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_schd4_ne <- bind_rows(incdata_schd4_ne_t_1, incdata_schd4_ne_t_2, incdata_schd4_ne_t_3, incdata_schd4_ne_c_1, incdata_schd4_ne_c_2, incdata_schd4_ne_c_3) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    grp_3rd,
    #distance to school(s) increased (schd#_)
    schd4_group = sch_treatment_grp_d4,
    schd4_relyear = schd4_relyear,
    schd4_treat = schd4_treat,
    schd4_g = schd4_g,
    schd4_deltadist = sch_delta_dist_d4, 
    `schd4_relyear[-3]`=ifelse(schd4_relyear==-3,1,0),
    `schd4_relyear[-2]`=ifelse(schd4_relyear==-2,1,0),
    `schd4_relyear[-1]`=ifelse(schd4_relyear==-1 | schd4_relyear==Inf ,1,0),
    `schd4_relyear[+0]`=ifelse(schd4_relyear==0,1,0),
    `schd4_relyear[+1]`=ifelse(schd4_relyear==1,1,0),
    `schd4_relyear[+2]`=ifelse(schd4_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_schd4_ne, "incdata_schd4_ne.csv")

##hosd1####
incdata_hosd1 <-  valgd %>% 
  subset(year!=2005) %>% 
  subset(treatment_grp_d1 %in%  c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
###LE####
incdata_hosd1_le <- incdata_hosd1 %>% 
  subset(election == "KV") 
####t###
incdata_hosd1_le_t <- incdata_hosd1_le %>% 
  subset(treatment_grp_d1 %in%  c("Treat 1st period", "Treat 2nd period")) %>% 
  mutate(party=ifelse(treatment_grp_d1=="Treat 1st period" & year %in% c(2013), incumbent,
                      ifelse(treatment_grp_d1=="Treat 2nd period" & year %in% c(2017), incumbent, NA ))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = ifelse(treatment_grp_d1=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(treatment_grp_d1=="Treat 2nd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_hosd1_le_t_1 <- incdata_hosd1_le_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_hosd1_le_t_2 <- incdata_hosd1_le_t %>% 
  subset(grp_2nd==1)
####c###
incdata_hosd1_le_c <- incdata_hosd1_le %>% 
  subset(treatment_grp_d1 %in%  c("Control"))
#####1st###
incdata_hosd1_le_c_1 <- incdata_hosd1_le_c %>% 
  mutate(party=ifelse(year %in% c(2013), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_hosd1_le_c_2 <- incdata_hosd1_le_c %>% 
  mutate(party=ifelse(year %in% c(2017), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_hosd1_le <- bind_rows(incdata_hosd1_le_t_1, incdata_hosd1_le_t_2, incdata_hosd1_le_c_1, incdata_hosd1_le_c_2) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    #distance to hospital(s) increased (hosd#_)
    hosd1_group = treatment_grp_d1,
    hosd1_relyear = hosd1_relyear,
    hosd1_treat = hosd1_treat,
    hosd1_g = hosd1_g,
    hosd1_deltadist = delta_dist_d1, 
    `hosd1_relyear[-3]`=ifelse(hosd1_relyear==-3,1,0),
    `hosd1_relyear[-2]`=ifelse(hosd1_relyear==-2,1,0),
    `hosd1_relyear[-1]`=ifelse(hosd1_relyear==-1 | hosd1_relyear==Inf ,1,0),
    `hosd1_relyear[+0]`=ifelse(hosd1_relyear==0,1,0),
    `hosd1_relyear[+1]`=ifelse(hosd1_relyear==1,1,0),
    `hosd1_relyear[+2]`=ifelse(hosd1_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_hosd1_le, "incdata_hosd1_le.csv")

###NE####
incdata_hosd1_ne <- incdata_hosd1 %>% 
  subset(election == "FV") 
####t###
incdata_hosd1_ne_t <- incdata_hosd1_ne %>% 
  subset(treatment_grp_d1 %in%  c("Treat 1st period", "Treat 2nd period", "Treat 3rd period")) %>% 
  mutate(party=ifelse(treatment_grp_d1=="Treat 1st period" & year %in% c(2011), "VK",
                      ifelse(treatment_grp_d1=="Treat 2nd period" & year %in% c(2015), "SRSF",
                             ifelse(treatment_grp_d1=="Treat 3rd period" & year %in% c(2019), "VKLA", NA)))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(treatment_grp_d1=="Treat 1st period" & year %in% c(2007, 2011, 2019), 1,
                         ifelse(treatment_grp_d1=="Treat 2nd period" & year %in% c(2015), 1,
                                ifelse(treatment_grp_d1=="Treat 3rd period" & year %in% c(2007, 2011, 2019), 1, 0))),
         grp_1st = ifelse(treatment_grp_d1=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(treatment_grp_d1=="Treat 2nd period", 1, 0),
         grp_3rd = ifelse(treatment_grp_d1=="Treat 3rd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_hosd1_ne_t_1 <- incdata_hosd1_ne_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_hosd1_ne_t_2 <- incdata_hosd1_ne_t %>% 
  subset(grp_2nd==1)
#####3rd###
incdata_hosd1_ne_t_3 <- incdata_hosd1_ne_t %>% 
  subset(grp_3rd==1)
####c###
incdata_hosd1_ne_c <- incdata_hosd1_ne %>% 
  subset(treatment_grp_d1 %in%  c("Control"))
#####1st###
incdata_hosd1_ne_c_1 <- incdata_hosd1_ne_c %>% 
  mutate(party=ifelse(year %in% c(2011), "VK", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_hosd1_ne_c_2 <-  incdata_hosd1_ne_c %>% 
  mutate(party=ifelse(year %in% c(2015), "SRSF", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2015), 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####3rd###
incdata_hosd1_ne_c_3 <-  incdata_hosd1_ne_c %>% 
  mutate(party=ifelse(year %in% c(2019), "VKLA", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 0,
         grp_2nd = 0,
         grp_3rd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_hosd1_ne <- bind_rows(incdata_hosd1_ne_t_1, incdata_hosd1_ne_t_2, incdata_hosd1_ne_t_3, incdata_hosd1_ne_c_1, incdata_hosd1_ne_c_2, incdata_hosd1_ne_c_3) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    grp_3rd,
    #distance to hospital(s) increased (hosd#_)
    hosd1_group = treatment_grp_d1,
    hosd1_relyear = hosd1_relyear,
    hosd1_treat = hosd1_treat,
    hosd1_g = hosd1_g,
    hosd1_deltadist = delta_dist_d1, 
    `hosd1_relyear[-3]`=ifelse(hosd1_relyear==-3,1,0),
    `hosd1_relyear[-2]`=ifelse(hosd1_relyear==-2,1,0),
    `hosd1_relyear[-1]`=ifelse(hosd1_relyear==-1 | hosd1_relyear==Inf ,1,0),
    `hosd1_relyear[+0]`=ifelse(hosd1_relyear==0,1,0),
    `hosd1_relyear[+1]`=ifelse(hosd1_relyear==1,1,0),
    `hosd1_relyear[+2]`=ifelse(hosd1_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_hosd1_ne, "incdata_hosd1_ne.csv")

##hosd2####
incdata_hosd2 <-  valgd %>% 
  subset(year!=2005) %>% 
  subset(treatment_grp_d2 %in%  c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
###LE####
incdata_hosd2_le <- incdata_hosd2 %>% 
  subset(election == "KV") 
####t###
incdata_hosd2_le_t <- incdata_hosd2_le %>% 
  subset(treatment_grp_d2 %in%  c("Treat 1st period", "Treat 2nd period")) %>% 
  mutate(party=ifelse(treatment_grp_d2=="Treat 1st period" & year %in% c(2013), incumbent,
                      ifelse(treatment_grp_d2=="Treat 2nd period" & year %in% c(2017), incumbent, NA ))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = ifelse(treatment_grp_d2=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(treatment_grp_d2=="Treat 2nd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_hosd2_le_t_1 <- incdata_hosd2_le_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_hosd2_le_t_2 <- incdata_hosd2_le_t %>% 
  subset(grp_2nd==1)
####c###
incdata_hosd2_le_c <- incdata_hosd2_le %>% 
  subset(treatment_grp_d2 %in%  c("Control"))
#####1st###
incdata_hosd2_le_c_1 <- incdata_hosd2_le_c %>% 
  mutate(party=ifelse(year %in% c(2013), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_hosd2_le_c_2 <- incdata_hosd2_le_c %>% 
  mutate(party=ifelse(year %in% c(2017), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_hosd2_le <- bind_rows(incdata_hosd2_le_t_1, incdata_hosd2_le_t_2, incdata_hosd2_le_c_1, incdata_hosd2_le_c_2) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    #distance to hospital(s) increased (hosd#_)
    hosd2_group = treatment_grp_d2,
    hosd2_relyear = hosd2_relyear,
    hosd2_treat = hosd2_treat,
    hosd2_g = hosd2_g,
    hosd2_deltadist = delta_dist_d2, 
    `hosd2_relyear[-3]`=ifelse(hosd2_relyear==-3,1,0),
    `hosd2_relyear[-2]`=ifelse(hosd2_relyear==-2,1,0),
    `hosd2_relyear[-1]`=ifelse(hosd2_relyear==-1 | hosd2_relyear==Inf ,1,0),
    `hosd2_relyear[+0]`=ifelse(hosd2_relyear==0,1,0),
    `hosd2_relyear[+1]`=ifelse(hosd2_relyear==1,1,0),
    `hosd2_relyear[+2]`=ifelse(hosd2_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_hosd2_le, "incdata_hosd2_le.csv")

###NE####
incdata_hosd2_ne <- incdata_hosd2 %>% 
  subset(election == "FV") 
####t###
incdata_hosd2_ne_t <- incdata_hosd2_ne %>% 
  subset(treatment_grp_d2 %in%  c("Treat 1st period", "Treat 2nd period", "Treat 3rd period")) %>% 
  mutate(party=ifelse(treatment_grp_d2=="Treat 1st period" & year %in% c(2011), "VK",
                      ifelse(treatment_grp_d2=="Treat 2nd period" & year %in% c(2015), "SRSF",
                             ifelse(treatment_grp_d2=="Treat 3rd period" & year %in% c(2019), "VKLA", NA)))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(treatment_grp_d2=="Treat 1st period" & year %in% c(2007, 2011, 2019), 1,
                         ifelse(treatment_grp_d2=="Treat 2nd period" & year %in% c(2015), 1,
                                ifelse(treatment_grp_d2=="Treat 3rd period" & year %in% c(2007, 2011, 2019), 1, 0))),
         grp_1st = ifelse(treatment_grp_d2=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(treatment_grp_d2=="Treat 2nd period", 1, 0),
         grp_3rd = ifelse(treatment_grp_d2=="Treat 3rd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_hosd2_ne_t_1 <- incdata_hosd2_ne_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_hosd2_ne_t_2 <- incdata_hosd2_ne_t %>% 
  subset(grp_2nd==1)
#####3rd###
incdata_hosd2_ne_t_3 <- incdata_hosd2_ne_t %>% 
  subset(grp_3rd==1)
####c###
incdata_hosd2_ne_c <- incdata_hosd2_ne %>% 
  subset(treatment_grp_d2 %in%  c("Control"))
#####1st###
incdata_hosd2_ne_c_1 <- incdata_hosd2_ne_c %>% 
  mutate(party=ifelse(year %in% c(2011), "VK", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_hosd2_ne_c_2 <-  incdata_hosd2_ne_c %>% 
  mutate(party=ifelse(year %in% c(2015), "SRSF", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2015), 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####3rd###
incdata_hosd2_ne_c_3 <-  incdata_hosd2_ne_c %>% 
  mutate(party=ifelse(year %in% c(2019), "VKLA", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 0,
         grp_2nd = 0,
         grp_3rd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_hosd2_ne <- bind_rows(incdata_hosd2_ne_t_1, incdata_hosd2_ne_t_2, incdata_hosd2_ne_t_3, incdata_hosd2_ne_c_1, incdata_hosd2_ne_c_2, incdata_hosd2_ne_c_3) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    grp_3rd,
    #distance to hospital(s) increased (hosd#_)
    hosd2_group = treatment_grp_d2,
    hosd2_relyear = hosd2_relyear,
    hosd2_treat = hosd2_treat,
    hosd2_g = hosd2_g,
    hosd2_deltadist = delta_dist_d2, 
    `hosd2_relyear[-3]`=ifelse(hosd2_relyear==-3,1,0),
    `hosd2_relyear[-2]`=ifelse(hosd2_relyear==-2,1,0),
    `hosd2_relyear[-1]`=ifelse(hosd2_relyear==-1 | hosd2_relyear==Inf ,1,0),
    `hosd2_relyear[+0]`=ifelse(hosd2_relyear==0,1,0),
    `hosd2_relyear[+1]`=ifelse(hosd2_relyear==1,1,0),
    `hosd2_relyear[+2]`=ifelse(hosd2_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_hosd2_ne, "incdata_hosd2_ne.csv")

##hosd3####
incdata_hosd3 <-  valgd %>% 
  subset(year!=2005) %>% 
  subset(treatment_grp_d3 %in%  c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
###LE####
incdata_hosd3_le <- incdata_hosd3 %>% 
  subset(election == "KV") 
####t###
incdata_hosd3_le_t <- incdata_hosd3_le %>% 
  subset(treatment_grp_d3 %in%  c("Treat 1st period", "Treat 2nd period")) %>% 
  mutate(party=ifelse(treatment_grp_d3=="Treat 1st period" & year %in% c(2013), incumbent,
                      ifelse(treatment_grp_d3=="Treat 2nd period" & year %in% c(2017), incumbent, NA ))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = ifelse(treatment_grp_d3=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(treatment_grp_d3=="Treat 2nd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_hosd3_le_t_1 <- incdata_hosd3_le_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_hosd3_le_t_2 <- incdata_hosd3_le_t %>% 
  subset(grp_2nd==1)
####c###
incdata_hosd3_le_c <- incdata_hosd3_le %>% 
  subset(treatment_grp_d3 %in%  c("Control"))
#####1st###
incdata_hosd3_le_c_1 <- incdata_hosd3_le_c %>% 
  mutate(party=ifelse(year %in% c(2013), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_hosd3_le_c_2 <- incdata_hosd3_le_c %>% 
  mutate(party=ifelse(year %in% c(2017), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_hosd3_le <- bind_rows(incdata_hosd3_le_t_1, incdata_hosd3_le_t_2, incdata_hosd3_le_c_1, incdata_hosd3_le_c_2) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    #distance to hospital(s) increased (hosd#_)
    hosd3_group = treatment_grp_d3,
    hosd3_relyear = hosd3_relyear,
    hosd3_treat = hosd3_treat,
    hosd3_g = hosd3_g,
    hosd3_deltadist = delta_dist_d3, 
    `hosd3_relyear[-3]`=ifelse(hosd3_relyear==-3,1,0),
    `hosd3_relyear[-2]`=ifelse(hosd3_relyear==-2,1,0),
    `hosd3_relyear[-1]`=ifelse(hosd3_relyear==-1 | hosd3_relyear==Inf ,1,0),
    `hosd3_relyear[+0]`=ifelse(hosd3_relyear==0,1,0),
    `hosd3_relyear[+1]`=ifelse(hosd3_relyear==1,1,0),
    `hosd3_relyear[+2]`=ifelse(hosd3_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_hosd3_le, "incdata_hosd3_le.csv")

###NE####
incdata_hosd3_ne <- incdata_hosd3 %>% 
  subset(election == "FV") 
####t###
incdata_hosd3_ne_t <- incdata_hosd3_ne %>% 
  subset(treatment_grp_d3 %in%  c("Treat 1st period", "Treat 2nd period", "Treat 3rd period")) %>% 
  mutate(party=ifelse(treatment_grp_d3=="Treat 1st period" & year %in% c(2011), "VK",
                      ifelse(treatment_grp_d3=="Treat 2nd period" & year %in% c(2015), "SRSF",
                             ifelse(treatment_grp_d3=="Treat 3rd period" & year %in% c(2019), "VKLA", NA)))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(treatment_grp_d3=="Treat 1st period" & year %in% c(2007, 2011, 2019), 1,
                         ifelse(treatment_grp_d3=="Treat 2nd period" & year %in% c(2015), 1,
                                ifelse(treatment_grp_d3=="Treat 3rd period" & year %in% c(2007, 2011, 2019), 1, 0))),
         grp_1st = ifelse(treatment_grp_d3=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(treatment_grp_d3=="Treat 2nd period", 1, 0),
         grp_3rd = ifelse(treatment_grp_d3=="Treat 3rd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_hosd3_ne_t_1 <- incdata_hosd3_ne_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_hosd3_ne_t_2 <- incdata_hosd3_ne_t %>% 
  subset(grp_2nd==1)
#####3rd###
incdata_hosd3_ne_t_3 <- incdata_hosd3_ne_t %>% 
  subset(grp_3rd==1)
####c###
incdata_hosd3_ne_c <- incdata_hosd3_ne %>% 
  subset(treatment_grp_d3 %in%  c("Control"))
#####1st###
incdata_hosd3_ne_c_1 <- incdata_hosd3_ne_c %>% 
  mutate(party=ifelse(year %in% c(2011), "VK", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_hosd3_ne_c_2 <-  incdata_hosd3_ne_c %>% 
  mutate(party=ifelse(year %in% c(2015), "SRSF", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2015), 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####3rd###
incdata_hosd3_ne_c_3 <-  incdata_hosd3_ne_c %>% 
  mutate(party=ifelse(year %in% c(2019), "VKLA", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 0,
         grp_2nd = 0,
         grp_3rd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_hosd3_ne <- bind_rows(incdata_hosd3_ne_t_1, incdata_hosd3_ne_t_2, incdata_hosd3_ne_t_3, incdata_hosd3_ne_c_1, incdata_hosd3_ne_c_2, incdata_hosd3_ne_c_3) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    grp_3rd,
    #distance to hospital(s) increased (hosd#_)
    hosd3_group = treatment_grp_d3,
    hosd3_relyear = hosd3_relyear,
    hosd3_treat = hosd3_treat,
    hosd3_g = hosd3_g,
    hosd3_deltadist = delta_dist_d3, 
    `hosd3_relyear[-3]`=ifelse(hosd3_relyear==-3,1,0),
    `hosd3_relyear[-2]`=ifelse(hosd3_relyear==-2,1,0),
    `hosd3_relyear[-1]`=ifelse(hosd3_relyear==-1 | hosd3_relyear==Inf ,1,0),
    `hosd3_relyear[+0]`=ifelse(hosd3_relyear==0,1,0),
    `hosd3_relyear[+1]`=ifelse(hosd3_relyear==1,1,0),
    `hosd3_relyear[+2]`=ifelse(hosd3_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_hosd3_ne, "incdata_hosd3_ne.csv")

##hosd4####
incdata_hosd4 <-  valgd %>% 
  subset(year!=2005) %>% 
  subset(treatment_grp_d4 %in%  c("Control", "Treat 1st period", "Treat 2nd period", "Treat 3rd period")) 
###LE####
incdata_hosd4_le <- incdata_hosd4 %>% 
  subset(election == "KV") 
####t###
incdata_hosd4_le_t <- incdata_hosd4_le %>% 
  subset(treatment_grp_d4 %in%  c("Treat 1st period", "Treat 2nd period")) %>% 
  mutate(party=ifelse(treatment_grp_d4=="Treat 1st period" & year %in% c(2013), incumbent,
                      ifelse(treatment_grp_d4=="Treat 2nd period" & year %in% c(2017), incumbent, NA ))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = ifelse(treatment_grp_d4=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(treatment_grp_d4=="Treat 2nd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_hosd4_le_t_1 <- incdata_hosd4_le_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_hosd4_le_t_2 <- incdata_hosd4_le_t %>% 
  subset(grp_2nd==1)
####c###
incdata_hosd4_le_c <- incdata_hosd4_le %>% 
  subset(treatment_grp_d4 %in%  c("Control"))
#####1st###
incdata_hosd4_le_c_1 <- incdata_hosd4_le_c %>% 
  mutate(party=ifelse(year %in% c(2013), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_hosd4_le_c_2 <- incdata_hosd4_le_c %>% 
  mutate(party=ifelse(year %in% c(2017), incumbent, NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="Det Konservative Folkeparti", cons.votes,
                            ifelse(party=="Lokalliste", locl.votes,
                                   ifelse(party=="Radikale Venstre", rlib.votes,
                                          ifelse(party=="Socialdemokratiet", socdem.votes,
                                                 ifelse(party=="Socialistisk Folkeparti", spp.votes,
                                                        ifelse(party=="Venstre", lib.votes, NA)))))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(party == incumbent, 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_hosd4_le <- bind_rows(incdata_hosd4_le_t_1, incdata_hosd4_le_t_2, incdata_hosd4_le_c_1, incdata_hosd4_le_c_2) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    #distance to hospital(s) increased (hosd#_)
    hosd4_group = treatment_grp_d4,
    hosd4_relyear = hosd4_relyear,
    hosd4_treat = hosd4_treat,
    hosd4_g = hosd4_g,
    hosd4_deltadist = delta_dist_d4, 
    `hosd4_relyear[-3]`=ifelse(hosd4_relyear==-3,1,0),
    `hosd4_relyear[-2]`=ifelse(hosd4_relyear==-2,1,0),
    `hosd4_relyear[-1]`=ifelse(hosd4_relyear==-1 | hosd4_relyear==Inf ,1,0),
    `hosd4_relyear[+0]`=ifelse(hosd4_relyear==0,1,0),
    `hosd4_relyear[+1]`=ifelse(hosd4_relyear==1,1,0),
    `hosd4_relyear[+2]`=ifelse(hosd4_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_hosd4_le, "incdata_hosd4_le.csv")

###NE####
incdata_hosd4_ne <- incdata_hosd4 %>% 
  subset(election == "FV") 
####t###
incdata_hosd4_ne_t <- incdata_hosd4_ne %>% 
  subset(treatment_grp_d4 %in%  c("Treat 1st period", "Treat 2nd period", "Treat 3rd period")) %>% 
  mutate(party=ifelse(treatment_grp_d4=="Treat 1st period" & year %in% c(2011), "VK",
                      ifelse(treatment_grp_d4=="Treat 2nd period" & year %in% c(2015), "SRSF",
                             ifelse(treatment_grp_d4=="Treat 3rd period" & year %in% c(2019), "VKLA", NA)))) %>%
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(treatment_grp_d4=="Treat 1st period" & year %in% c(2007, 2011, 2019), 1,
                         ifelse(treatment_grp_d4=="Treat 2nd period" & year %in% c(2015), 1,
                                ifelse(treatment_grp_d4=="Treat 3rd period" & year %in% c(2007, 2011, 2019), 1, 0))),
         grp_1st = ifelse(treatment_grp_d4=="Treat 1st period", 1, 0),
         grp_2nd = ifelse(treatment_grp_d4=="Treat 2nd period", 1, 0),
         grp_3rd = ifelse(treatment_grp_d4=="Treat 3rd period", 1, 0),
         afstemid_p=paste0(afstemid, party))
#####1st###
incdata_hosd4_ne_t_1 <- incdata_hosd4_ne_t %>% 
  subset(grp_1st==1)
#####2nd###
incdata_hosd4_ne_t_2 <- incdata_hosd4_ne_t %>% 
  subset(grp_2nd==1)
#####3rd###
incdata_hosd4_ne_t_3 <- incdata_hosd4_ne_t %>% 
  subset(grp_3rd==1)
####c###
incdata_hosd4_ne_c <- incdata_hosd4_ne %>% 
  subset(treatment_grp_d4 %in%  c("Control"))
#####1st###
incdata_hosd4_ne_c_1 <- incdata_hosd4_ne_c %>% 
  mutate(party=ifelse(year %in% c(2011), "VK", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 1,
         grp_2nd = 0,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####2nd###
incdata_hosd4_ne_c_2 <-  incdata_hosd4_ne_c %>% 
  mutate(party=ifelse(year %in% c(2015), "SRSF", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2015), 1, 0),
         grp_1st = 0,
         grp_2nd = 1,
         grp_3rd = 0,
         afstemid_p=paste0(afstemid, party))
#####3rd###
incdata_hosd4_ne_c_3 <-  incdata_hosd4_ne_c %>% 
  mutate(party=ifelse(year %in% c(2019), "VKLA", NA)) %>% 
  group_by(afstemid, election) %>% 
  mutate(party=ifelse(F %in% is.na(party), subset(party, F==is.na(party)), NA)) %>% 
  ungroup() %>% 
  mutate(inc.votes = ifelse(party=="VK", vk.votes,
                            ifelse(party=="SRSF", srsf.votes,
                                   ifelse(party=="VKLA", vkla.votes, NA))),
         inc_sup=inc.votes/valid.votes,
         incumb = ifelse(year %in% c(2007, 2011, 2019), 1, 0),
         grp_1st = 0,
         grp_2nd = 0,
         grp_3rd = 1,
         afstemid_p=paste0(afstemid, party))

incdata_hosd4_ne <- bind_rows(incdata_hosd4_ne_t_1, incdata_hosd4_ne_t_2, incdata_hosd4_ne_t_3, incdata_hosd4_ne_c_1, incdata_hosd4_ne_c_2, incdata_hosd4_ne_c_3) %>% 
  mutate(afstemid_p=as.numeric(factor(afstemid_p)), 
  ) %>% 
  group_by(afstemid_p) %>% 
  mutate(l_afstemid_p=length(afstemid_p)) %>% 
  ungroup() %>% 
  transmute(
    afstemid,
    afstemid_p,
    komnr,
    election,
    year, 
    inc.votes,
    inc_sup, 
    eligible.votes,
    incumb,
    party,
    grp_1st,
    grp_2nd,
    grp_3rd,
    #distance to hospital(s) increased (hosd#_)
    hosd4_group = treatment_grp_d4,
    hosd4_relyear = hosd4_relyear,
    hosd4_treat = hosd4_treat,
    hosd4_g = hosd4_g,
    hosd4_deltadist = delta_dist_d4, 
    `hosd4_relyear[-3]`=ifelse(hosd4_relyear==-3,1,0),
    `hosd4_relyear[-2]`=ifelse(hosd4_relyear==-2,1,0),
    `hosd4_relyear[-1]`=ifelse(hosd4_relyear==-1 | hosd4_relyear==Inf ,1,0),
    `hosd4_relyear[+0]`=ifelse(hosd4_relyear==0,1,0),
    `hosd4_relyear[+1]`=ifelse(hosd4_relyear==1,1,0),
    `hosd4_relyear[+2]`=ifelse(hosd4_relyear==2,1,0),
    #controls
    pop,
    area,
    popdens=pop/area,
    ayoung,
    aold,
    anwimm,
    alowedu,
    ahighedu,
    aunemp,
    minc,
    loginc=log(minc),
    logpop=log(pop)
  )
write_csv2(incdata_hosd4_ne, "incdata_hosd4_ne.csv")

