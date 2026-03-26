#### 1. Read in Iyengar and Westwood's data ####
iw_dat <- import("data/raw/iyengar_westwood_replication/AJPSResponsivenessNegativity.csv")

#### 2. Cleaning ####
study1<-iw_dat %>% 
  ## Keep required variables and observations
  dplyr::select(study1, scholarship, Q51, Q14, gpa1, gpa2, Q52) %>% 
  filter(study1  == 1 & scholarship == "partisan") %>% 
  ## recode/make variables of interest
  mutate(participantPID = case_when(
    Q51 == 1 ~ 1, 
    Q51 == 2 ~ 5, 
    Q52 == 1 ~ 2, 
    Q52 == 3 ~ 3, 
    Q52 == 2 ~ 4, 
    TRUE ~ NA_real_), 
    participantPID = factor(participantPID, levels=1:5, 
                            labels = c("Democrat","Lean Democrat","Independent",
                                       "Lean Republican","Republican")),
    participantPID2 = factor(as.character(participantPID), 
                             levels = c("Independent","Democrat","Lean Democrat",
                                        "Lean Republican","Republican")),
    partisanSelection = case_when(
      Q14 == 1 ~ 1, 
      Q14 == 2 ~ 0, 
      TRUE~ NA_real_), 
    across(c(gpa1, gpa2), as.numeric), 
    mostQualified = case_when(gpa1 == gpa2 ~ 0, 
                              gpa1 < gpa2 ~ 1, 
                              gpa2 < gpa1 ~ 2), 
    mostQualifiedPerson = factor(mostQualified, levels=c(0,1,2), 
                                 labels=c("Equally Qualified", "Republican More Qualified",
                                          "Democrat More Qualified")))

#### 3. Subset for export ####
iw_dat <- study1 %>% 
  dplyr::select(partisanSelection, participantPID, participantPID2, 
         mostQualifiedPerson, scholarship)

#### 4. Save data to analysis folder ####
save(iw_dat, file="data/analysis/iyengar_westwood_replication/iw_dat.rda")
export(iw_dat, file="data/analysis/iyengar_westwood_replication/iw_dat.dta")

