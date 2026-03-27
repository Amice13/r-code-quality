#######
#######
####### Replication Data for: The Differential Impact of the Hong Kong National Security Law 
####### on Political Sensitivity Bias in Opinion Polls.
####### This file cleans the raw data used in other files.
####### Last Updated: May. 2023
#######
#######

# Check system and installs packages user doesn't have, load needed packages
#remotes::install_github("grantmcdermott/ggiplot") #install ggiplot
need <- c("dplyr", "tidyr") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

#setup
rm(list = ls()) # clear workspace
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
list.files() #confirm the right directory is set

#clean popularity of govt (7)
popularity_name_list <- c("govttrust", "govtsat", "govsatecon", "govsatlive",
                     "govsatdem", "govsatfree","govsatcent") #list of popularity variables# read raw data
sensitivity <- c("medium", "low","low","low",
                "medium","high","medium")
popularity_list<- lapply(popularity_name_list, function(name) {
    read.csv(paste0(name, ".csv"))
  }) #read the datasets into a large list
names(popularity_list) <- popularity_name_list
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
for (name in popularity_name_list) {
  popularity_list[[name]]<- rename(popularity_list[[name]], Date= "調查開始日期Survey.Start.Date")
  popularity_list[[name]]$Date <- as.Date(popularity_list[[name]]$Date)
  popularity_list[[name]]<- rename(popularity_list[[name]], response="回應率Response.rate")
  popularity_list[[name]]<- rename(popularity_list[[name]], any_of(c(proBeijing="淨值Net.Value", proBeijing="淨值Netvalue")))
  popularity_list[[name]]<- subset(popularity_list[[name]], select=c(Date,response,proBeijing))
  popularity_list[[name]]$proBeijing <- as.numeric(sub("%","",popularity_list[[name]]$proBeijing))
  popularity_list[[name]]$response <- as.numeric(sub("%","",popularity_list[[name]]$response))
  popularity_list[[name]]$sensitivity <- sensitivity[which(popularity_name_list==name)]
  a <- popularity_list[[name]]
  save(a,file=paste(name, "Rdata", sep="."))
  }

#clean People’s Appraisal of Society’s Current Conditions (6) #problematic
rm(list = ls()) # clear workspace
name_list <- c("conpol","conecon","conlive","satpol", "satecon","satlive")
anti_Beijing <- c()
sensitivity <- c("low","low","low","medium","low","low")
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
list<- lapply(name_list, function(name) {
  read.csv(paste0(name, ".csv"))
}) #read the datasets into a large list
names(list) <- name_list
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
for (name in name_list) {
  list[[name]]<- rename(list[[name]], Date= "調查開始日期Survey.Start.Date")
  list[[name]]$Date <- as.Date(list[[name]]$Date)
  if ("淨值Net.Value" %in% names(list[[name]]) | "淨值Netvalue" %in% names(list[[name]]) | "淨值Net.value" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(proBeijing="淨值Net.Value", proBeijing="淨值Netvalue", proBeijing="淨值Net.value")))
    list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
  } else { if("評分Rating" %in% names(list[[name]])) {
    list[[name]]$proBeijing <- (as.numeric(sub("%","",list[[name]]$評分Rating))-5)*20
  } else { if("支持度Support.rating" %in% names(list[[name]])) {
    list[[name]]$proBeijing <- (list[[name]]$支持度Support.rating-50)*2
  } else{print(colnames(list[[name]])[6])
    if("唔知.難講DK.HS" %in% names(list[[name]])){
      colnames(list[[name]])[6]<- "proBeijing"
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
    } else{print(paste0("ALERT: ",name," Variable:",colnames(list[[name]])[6]))
      colnames(list[[name]])[6]<- "proBeijing"
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))}
  }
  }
  }
  if(name %in% anti_Beijing) {
    print(paste0(name," is an anti-Beijing poll"))
    list[[name]]$proBeijing=-list[[name]]$proBeijing}
  if ("認知率Recognition.Rate" %in% names(list[[name]]) |"認知率Recognition.rate" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(recognition="認知率Recognition.Rate",recognition="認知率Recognition.rate")))
    list[[name]]$recognition <- as.numeric(sub("%","",list[[name]]$recognition))
  } 
  if ("回應率Response.rate" %in% names(list[[name]]) |"回應率Response.Rate" %in% names(list[[name]])|"回應比率Response.rate" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(response="回應率Response.rate",response="回應率Response.Rate",response="回應比率Response.rate")))
    list[[name]]$response <- as.numeric(sub("%","",list[[name]]$response))
  } 
  list[[name]]$sensitivity <- sensitivity[which(name_list==name)]
  a <- list[[name]] %>% select(any_of(c("Date","response","recognition","proBeijing","sensitivity")))
  print(colnames(a))
  save(a,file=paste(name, "Rdata", sep="."))
}
#clean Ethnic Identity (6)
rm(list = ls()) # clear workspace
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
citizenPRC <- read.csv("citizenPRC.csv")
ethHKer<- read.csv("ethHKer.csv")
ethCHN <- read.csv("ethCHN.csv")
ethCHNrace <- read.csv("ethCHNrace.csv")
ethAsian <- read.csv("ethAsian.csv")
ethGlobal <- read.csv("ethGlobal.csv")

citizenPRC<- rename(citizenPRC, Date= "調查開始日期Survey.Start.Date")
ethHKer<- rename(ethHKer, Date= "調查開始日期Survey.Start.Date")
ethCHN<- rename(ethCHN, Date= "調查開始日期Survey.Start.Date")
ethCHNrace<- rename(ethCHNrace, Date= "調查開始日期Survey.Start.Date")
ethAsian<- rename(ethAsian, Date= "調查開始日期Survey.Start.Date")
ethGlobal<- rename(ethGlobal, Date= "調查開始日期Survey.Start.Date")
citizenPRC$proBeijing <- (citizenPRC$評分Rating-5)*20
ethHKer$proBeijing <- -(ethHKer$評分Rating-5)*20
ethCHN$proBeijing <- (ethCHN$評分Rating-5)*20
ethCHNrace$proBeijing <- (ethCHNrace$評分Rating-5)*20
ethAsian$proBeijing <- (ethAsian$評分Rating-5)*20
ethGlobal$proBeijing <- -(ethGlobal$評分Rating-5)*20
eth_name_list <- c("citizenPRC", "ethHKer", "ethCHN", "ethCHNrace","ethAsian","ethGlobal")
eth_list <- list(citizenPRC,ethHKer,  ethCHN, ethCHNrace, ethAsian, ethGlobal)
names(eth_list) <- eth_name_list
sensitivity=c("high", "low", "high", "low", "low", "low")
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
for (name in eth_name_list) {
    eth_list[[name]]$Date <- as.Date(eth_list[[name]]$Date)
    eth_list[[name]]<- rename(eth_list[[name]], response="回應率Response.rate")
    eth_list[[name]]<- rename(eth_list[[name]], any_of(c(recognition="認知率Recognition.rate")))
    eth_list[[name]]<- subset(eth_list[[name]], select=c(Date,response, recognition, proBeijing))
    eth_list[[name]]$response <- as.numeric(sub("%","",eth_list[[name]]$response))
  eth_list[[name]]$recognition <- as.numeric(sub("%","",eth_list[[name]]$recognition))
    eth_list[[name]]$sensitivity <- sensitivity[which(eth_name_list==name)]
    a <- eth_list[[name]]
    save(a,file=paste(name, "Rdata", sep="."))
}

#clean Social Indicators (11)
rm(list = ls()) # clear workspace
social_name_list <- c("appraisal_democracy","appraisal_freedom","appraisal_prosperity","appraisal_stability",
                      "appraisal_civilization","appraisal_corrup_free","appraisal_equality","appraisal_efficiency",
                      "appraisal_welfare","appraisal_order","appraisal_fairness")
sensitivity <- c("high","high","low","low","low","low",
                 "low","low","low","low","low")
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
social_list<- lapply(social_name_list, function(name) {
  read.csv(paste0(name, ".csv"))
}) #read the datasets into a large list
names(social_list) <- social_name_list
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
for (name in social_name_list) {
    social_list[[name]]<- rename(social_list[[name]], Date= "調查開始日期Survey.Start.Date")
    social_list[[name]]$Date <- as.Date(social_list[[name]]$Date)
    social_list[[name]]<- rename(social_list[[name]], response="回應率Response.Rate")
    social_list[[name]]<- rename(social_list[[name]], any_of(c(recognition="認知率Recognition.Rate")))
    colnames(social_list[[name]])[6]<- "proBeijing"
    social_list[[name]]<- subset(social_list[[name]], select=c(Date,response,recognition,proBeijing))
    social_list[[name]]$proBeijing <- (social_list[[name]]$proBeijing-5)*20 #for the first three
    social_list[[name]]$response <- as.numeric(sub("%","",social_list[[name]]$response))
    social_list[[name]]$recognition <- as.numeric(sub("%","",social_list[[name]]$recognition))
    social_list[[name]]$sensitivity <- sensitivity[which(social_name_list==name)]
    a <- social_list[[name]]
   save(a,file=paste(name, "Rdata", sep="."))
}

#clean Freedom Indicators (10)
rm(list = ls()) # clear workspace
name_list <- c("free_speech","free_press","free_relig","free_publication",
               "free_art","free_demonstrate", "free_academ","free_asso",
               "free_move","free_strike")
anti_Beijing <- c()
sensitivity <- c("high","medium","low","medium","low",
                 "high","low","high","medium","high")
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
list<- lapply(name_list, function(name) {
  read.csv(paste0(name, ".csv"))
}) #read the datasets into a large list
names(list) <- name_list
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
for (name in name_list) {
  list[[name]]<- rename(list[[name]], Date= "調查開始日期Survey.Start.Date")
  list[[name]]$Date <- as.Date(list[[name]]$Date)
  if ("淨值Net.Value" %in% names(list[[name]]) | "淨值Netvalue" %in% names(list[[name]]) | "淨值Net.value" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(proBeijing="淨值Net.Value", proBeijing="淨值Netvalue", proBeijing="淨值Net.value")))
    list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
  } else { if("評分Rating" %in% names(list[[name]])) {
    list[[name]]$proBeijing <- (as.numeric(sub("%","",list[[name]]$評分Rating))-5)*20
  } else { if("支持度Support.rating" %in% names(list[[name]])) {
    list[[name]]$proBeijing <- (list[[name]]$支持度Support.rating-50)*2
  } else{print(colnames(list[[name]])[6])
    if("唔知.難講DK.HS" %in% names(list[[name]])){
      colnames(list[[name]])[6]<- "proBeijing"
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
    } else{print(paste0("ALERT: ",name," Variable:",colnames(list[[name]])[6]))
      colnames(list[[name]])[6]<- "proBeijing"
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))}
  }
  }
  }
  if(name %in% anti_Beijing) {
    print(paste0(name," is an anti-Beijing poll"))
    list[[name]]$proBeijing=-list[[name]]$proBeijing}
  if ("認知率Recognition.Rate" %in% names(list[[name]]) |"認知率Recognition.rate" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(recognition="認知率Recognition.Rate",recognition="認知率Recognition.rate")))
    list[[name]]$recognition <- as.numeric(sub("%","",list[[name]]$recognition))
  } 
  if ("回應率Response.rate" %in% names(list[[name]]) |"回應率Response.Rate" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(response="回應率Response.rate",response="回應率Response.Rate")))
    list[[name]]$response <- as.numeric(sub("%","",list[[name]]$response))
  } 
  list[[name]]$sensitivity <- sensitivity[which(name_list==name)]
  a <- list[[name]] %>% select(any_of(c("Date","response","recognition","proBeijing","sensitivity")))
  print(colnames(a))
  save(a,file=paste(name, "Rdata", sep="."))
}
#clean Rule of Law Indicators (6)
rm(list = ls()) # clear workspace
law_name_list <- c("rule_of_law","fairness_judicial","impartiality_courts")
sensitivity <- c("low","low","medium")
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
law_list<- lapply(law_name_list, function(name) {
  read.csv(paste0(name, ".csv"))
}) #read the datasets into a large list
names(law_list) <- law_name_list
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
for (name in law_name_list) {
        law_list[[name]]<- rename(law_list[[name]], Date= "調查開始日期Survey.Start.Date")
        law_list[[name]]$Date <- as.Date(law_list[[name]]$Date)
        law_list[[name]]<- rename(law_list[[name]], response="回應率Response.Rate")
        law_list[[name]]<- rename(law_list[[name]], any_of(c(recognition="認知率Recognition.Rate")))
    colnames(law_list[[name]])[6]<- "proBeijing"
    law_list[[name]]$proBeijing <- (law_list[[name]]$proBeijing-5)*20
    law_list[[name]]<- subset(law_list[[name]], select=c(Date,response,recognition,proBeijing))
        law_list[[name]]$response <- as.numeric(sub("%","",law_list[[name]]$response))
        law_list[[name]]$recognition <- as.numeric(sub("%","",law_list[[name]]$recognition))
        law_list[[name]]$sensitivity <- sensitivity[which(law_name_list==name)]
        a <- law_list[[name]]
        save(a,file=paste(name, "Rdata", sep="."))
}

#clean Trust and Confidence Indicators (5)
rm(list = ls()) # clear workspace
trust_name_list <- c("trust_Beijing","trust_Taiwan","conf_HK_future",
                     "conf_China_future","conf_OCTS")
sensitivity <- c("high","high","low","low","high")
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
trust_list<- lapply(trust_name_list, function(name) {
  read.csv(paste0(name, ".csv"))
}) #read the datasets into a large list
names(trust_list) <- trust_name_list
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
for (name in trust_name_list) {
  trust_list[[name]]<- rename(trust_list[[name]], Date= "調查開始日期Survey.Start.Date")
  trust_list[[name]]$Date <- as.Date(trust_list[[name]]$Date)
  trust_list[[name]]<- rename(trust_list[[name]], response="回應率Response.rate")
    trust_list[[name]]<- rename(trust_list[[name]], any_of(c(proBeijing="淨值Net.Value", proBeijing="淨值Netvalue")))
    trust_list[[name]]$proBeijing <- as.numeric(sub("%","",trust_list[[name]]$proBeijing))
  trust_list[[name]]<- subset(trust_list[[name]], select=c(Date,response,proBeijing))
  trust_list[[name]]$response <- as.numeric(sub("%","",trust_list[[name]]$response))
  trust_list[[name]]$sensitivity <- sensitivity[which(trust_name_list==name)]
  a <- trust_list[[name]]
  save(a,file=paste(name, "Rdata", sep="."))
}
a <- trust_list$trust_Taiwan
a$proBeijing=-a$proBeijing
save(a,file="trust_Taiwan.Rdata")

#clean Taiwan and Tibetan Issues (4) automatic now
rm(list = ls()) # clear workspace
name_list <- c("indep_Taiwan","indep_Tibet","conf_reunif", "Taiwan_UN")
anti_Beijing <- c("indep_Taiwan","indep_Tibet","Taiwan_UN")
sensitivity <- c("extremely high","extremely high","high","high")
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
list<- lapply(name_list, function(name) {
  read.csv(paste0(name, ".csv"))
}) #read the datasets into a large list
names(list) <- name_list
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
for (name in name_list) {
    list[[name]]<- rename(list[[name]], Date= "調查開始日期Survey.Start.Date")
    list[[name]]$Date <- as.Date(list[[name]]$Date)
    list[[name]]<- rename(list[[name]], any_of(c(response="回應率Response.rate",response="回應率Response.Rate")))  #should be good
  if ("淨值Net.Value" %in% names(list[[name]]) | "淨值Netvalue" %in% names(list[[name]]) | "淨值Net.value" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(proBeijing="淨值Net.Value", proBeijing="淨值Netvalue", proBeijing="淨值Net.value")))
    list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
  } else { if("評分Rating" %in% names(list[[name]])) {
    print (2)
    } else {
    print(colnames(list[[name]])[6])
  }
  }
  if(name %in% anti_Beijing) {list[[name]]$proBeijing=-list[[name]]$proBeijing}
   if ("認知率Recognition.Rate" %in% names(list[[name]]) |"認知率Recognition.rate" %in% names(list[[name]])) {
     list[[name]]<- rename(list[[name]], any_of(c(recognition="認知率Recognition.Rate",recognition="認知率Recognition.rate")))
     list[[name]]$recognition <- as.numeric(sub("%","",list[[name]]$recognition))
  } 
  if ("回應率Response.rate" %in% names(list[[name]]) |"回應率Response.Rate" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(recognition="回應率Response.rate",recognition="回應率Response.Rate")))
    list[[name]]$response <- as.numeric(sub("%","",list[[name]]$response))
  } 
    list[[name]]$sensitivity <- sensitivity[which(name_list==name)]
    a <- list[[name]] %>% select(any_of(c("Date","response","recognition","proBeijing","sensitivity")))
    print(colnames(a))
    save(a,file=paste(name, "Rdata", sep="."))
}

#clean June Fourth Incident (11)
rm(list = ls()) # clear workspace
name_list <- c('64_students','64_CHN_gov','64_reverse','64_post','CHN_human3y',
               'effort_econ_demo','need_econ_demo')
anti_Beijing <- c('64_students','64_reverse')
sensitivity <- c('extremely high','extremely high','extremely high','high',
                 'high','high','high')
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
list<- lapply(name_list, function(name) {
  read.csv(paste0(name, ".csv"))
}) #read the datasets into a large list
names(list) <- name_list
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
for (name in name_list) {
    list[[name]]<- rename(list[[name]], Date= "調查開始日期Survey.Start.Date")
    list[[name]]$Date <- as.Date(list[[name]]$Date)
 if ("淨值Net.Value" %in% names(list[[name]]) | "淨值Netvalue" %in% names(list[[name]]) | "淨值Net.value" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(proBeijing="淨值Net.Value", proBeijing="淨值Netvalue", proBeijing="淨值Net.value")))
    list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
  } else { if("評分Rating" %in% names(list[[name]])) {
    list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$評分Rating))
    } else {
     print(colnames(list[[name]])[6])
    if("唔知.難講DK.HS" %in% names(list[[name]])){
      colnames(list[[name]])[6]<- "proBeijing"
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
    } else{print("other")}
  }
  }
  if(name %in% anti_Beijing) {
    print(paste0(name," is an anti-Beijing poll"))
    list[[name]]$proBeijing=-list[[name]]$proBeijing}
   if ("認知率Recognition.Rate" %in% names(list[[name]]) |"認知率Recognition.rate" %in% names(list[[name]])) {
     list[[name]]<- rename(list[[name]], any_of(c(recognition="認知率Recognition.Rate",recognition="認知率Recognition.rate")))
     list[[name]]$recognition <- as.numeric(sub("%","",list[[name]]$recognition))
 } 
  if ("回應率Response.rate" %in% names(list[[name]]) |"回應率Response.Rate" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(response="回應率Response.rate",response="回應率Response.Rate")))
    list[[name]]$response <- as.numeric(sub("%","",list[[name]]$response))
  }
    list[[name]]$sensitivity <- sensitivity[which(name_list==name)]
    a <- list[[name]] %>% select(any_of(c("Date","response","recognition","proBeijing","sensitivity")))
    print(colnames(a))
    save(a,file=paste(name, "Rdata", sep="."))
}

#clean Feeling towards different governments (21)
rm(list = ls()) # clear workspace
name_list <- c('gov_HK','gov_CHN','gov_TW','gov_Macau','gov_USA','gov_GB','gov_JPN','gov_FRA')
anti_Beijing <- c('gov_TW','gov_USA','gov_GB','gov_JPN','gov_FRA')
sensitivity <- c('medium','high','medium','low', 'medium','medium','low','low')
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
list<- lapply(name_list, function(name) {
  read.csv(paste0(name, ".csv"))
}) #read the datasets into a large list
names(list) <- name_list
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
for (name in name_list) {
     list[[name]]<- rename(list[[name]], Date= "調查開始日期Survey.Start.Date")
     list[[name]]$Date <- as.Date(list[[name]]$Date)
     if ("淨值Net.Value" %in% names(list[[name]]) | "淨值Netvalue" %in% names(list[[name]]) | "淨值Net.value" %in% names(list[[name]])) {
     list[[name]]<- rename(list[[name]], any_of(c(proBeijing="淨值Net.Value", proBeijing="淨值Netvalue", proBeijing="淨值Net.value")))
     list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
   } else { if("評分Rating" %in% names(list[[name]])) {
     list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$評分Rating))
     } else {
     print(colnames(list[[name]])[6])
     if("唔知.難講DK.HS" %in% names(list[[name]])){
       colnames(list[[name]])[6]<- "proBeijing"
       list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
    } else{print("other")}
  }
   }
  if(name %in% anti_Beijing) {
    print(paste0(name," is an anti-Beijing poll"))
    list[[name]]$proBeijing=-list[[name]]$proBeijing}
  if ("認知率Recognition.Rate" %in% names(list[[name]]) |"認知率Recognition.rate" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(recognition="認知率Recognition.Rate",recognition="認知率Recognition.rate")))
    list[[name]]$recognition <- as.numeric(sub("%","",list[[name]]$recognition))
  } 
  if ("回應率Response.rate" %in% names(list[[name]]) |"回應率Response.Rate" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(response="回應率Response.rate",response="回應率Response.Rate")))
    list[[name]]$response <- as.numeric(sub("%","",list[[name]]$response))
  } 
  list[[name]]$sensitivity <- sensitivity[which(name_list==name)]
  a <- list[[name]] %>% select(any_of(c("Date","response","recognition","proBeijing","sensitivity")))
  print(colnames(a))
  save(a,file=paste(name, "Rdata", sep="."))
}

#clean Feeling towards different People (18)
rm(list = ls()) # clear workspace
name_list <- c('ppl_HK','ppl_CHN','ppl_TW','ppl_Macau','ppl_USA','ppl_GB','ppl_JPN','ppl_FRA')
anti_Beijing <- c('ppl_TW','ppl_USA','ppl_GB','ppl_JPN','ppl_FRA')
sensitivity <- c('low','low','low','low','low','low','low','low')
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
list<- lapply(name_list, function(name) {
  read.csv(paste0(name, ".csv"))
}) #read the datasets into a large list
names(list) <- name_list
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
for (name in name_list) {
  list[[name]]<- rename(list[[name]], Date= "調查開始日期Survey.Start.Date")
  list[[name]]$Date <- as.Date(list[[name]]$Date)
  if ("淨值Net.Value" %in% names(list[[name]]) | "淨值Netvalue" %in% names(list[[name]]) | "淨值Net.value" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(proBeijing="淨值Net.Value", proBeijing="淨值Netvalue", proBeijing="淨值Net.value")))
    list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
  } else { if("評分Rating" %in% names(list[[name]])) {
    list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$評分Rating))
  } else {
    print(colnames(list[[name]])[6])
    if("唔知.難講DK.HS" %in% names(list[[name]])){
      colnames(list[[name]])[6]<- "proBeijing"
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
    } else{print("other")}
  }
  }
  if(name %in% anti_Beijing) {
    print(paste0(name," is an anti-Beijing poll"))
    list[[name]]$proBeijing=-list[[name]]$proBeijing}
  if ("認知率Recognition.Rate" %in% names(list[[name]]) |"認知率Recognition.rate" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(recognition="認知率Recognition.Rate",recognition="認知率Recognition.rate")))
    list[[name]]$recognition <- as.numeric(sub("%","",list[[name]]$recognition))
  } 
  if ("回應率Response.rate" %in% names(list[[name]]) |"回應率Response.Rate" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(response="回應率Response.rate",response="回應率Response.Rate")))
    list[[name]]$response <- as.numeric(sub("%","",list[[name]]$response))
  } 
  list[[name]]$sensitivity <- sensitivity[which(name_list==name)]
  a <- list[[name]] %>% select(any_of(c("Date","response","recognition","proBeijing","sensitivity")))
  print(colnames(a))
  save(a,file=paste(name, "Rdata", sep="."))
}

#clean Appraisal of the Local News Media (18)

rm(list = ls()) # clear workspace
name_list <- c('sat_free_press','cred_news','news_freespeech','news_abuse',
               'news_selfcensor','News_scrupleHKgovt','News_scrupleBeijing')
anti_Beijing <- c('news_selfcensor','News_scrupleHKgovt','News_scrupleBeijing')
sensitivity <- c('medium','low','medium', 'medium','medium','high','high')
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
list<- lapply(name_list, function(name) {
  read.csv(paste0(name, ".csv"))
}) #read the datasets into a large list
names(list) <- name_list
setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
for (name in name_list) {
  list[[name]]<- rename(list[[name]], Date= "調查開始日期Survey.Start.Date")
  list[[name]]$Date <- as.Date(list[[name]]$Date)
  if ("淨值Net.Value" %in% names(list[[name]]) | "淨值Netvalue" %in% names(list[[name]]) | "淨值Net.value" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(proBeijing="淨值Net.Value", proBeijing="淨值Netvalue", proBeijing="淨值Net.value")))
    list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
  } else { if("評分Rating" %in% names(list[[name]])) {
    list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$評分Rating))
  } else {
    print(colnames(list[[name]])[6])
    if("唔知.難講DK.HS" %in% names(list[[name]])){
      colnames(list[[name]])[6]<- "proBeijing"
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
    } else{print(paste0("ALERT: ",name," Variable:",colnames(list[[name]])[6]))
      colnames(list[[name]])[6]<- "proBeijing"
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))}
  }
  }
  if(name %in% anti_Beijing) {
    print(paste0(name," is an anti-Beijing poll"))
    list[[name]]$proBeijing=-list[[name]]$proBeijing}
  if ("認知率Recognition.Rate" %in% names(list[[name]]) |"認知率Recognition.rate" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(recognition="認知率Recognition.Rate",recognition="認知率Recognition.rate")))
    list[[name]]$recognition <- as.numeric(sub("%","",list[[name]]$recognition))
  } 
  if ("回應率Response.rate" %in% names(list[[name]]) |"回應率Response.Rate" %in% names(list[[name]])) {
    list[[name]]<- rename(list[[name]], any_of(c(response="回應率Response.rate",response="回應率Response.Rate")))
    list[[name]]$response <- as.numeric(sub("%","",list[[name]]$response))
  } 
  list[[name]]$sensitivity <- sensitivity[which(name_list==name)]
  a <- list[[name]] %>% select(any_of(c("Date","response","recognition","proBeijing","sensitivity")))
  print(colnames(a))
  save(a,file=paste(name, "Rdata", sep="."))
  }
  
#clean Popularity of Disciplinary Forces (9) and Popularity of the PLA Hong Kong Garrison
  rm(list = ls()) # clear workspace
  name_list <- c('pop_HKpolice','pop_ICAC','pop_PLA'  )
  anti_Beijing <- c()
  sensitivity <- c('high', 'low','high' )
  setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
  list<- lapply(name_list, function(name) {
    read.csv(paste0(name, ".csv"))
  }) #read the datasets into a large list
  names(list) <- name_list
  setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
  for (name in name_list) {
    list[[name]]<- rename(list[[name]], Date= "調查開始日期Survey.Start.Date")
    list[[name]]$Date <- as.Date(list[[name]]$Date)
    if ("淨值Net.Value" %in% names(list[[name]]) | "淨值Netvalue" %in% names(list[[name]]) | "淨值Net.value" %in% names(list[[name]])) {
      list[[name]]<- rename(list[[name]], any_of(c(proBeijing="淨值Net.Value", proBeijing="淨值Netvalue", proBeijing="淨值Net.value")))
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
    } else { if("評分Rating" %in% names(list[[name]])) {
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$評分Rating))
    } else {
      print(colnames(list[[name]])[6])
      if("唔知.難講DK.HS" %in% names(list[[name]])){
        colnames(list[[name]])[6]<- "proBeijing"
        list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
      } else{print(paste0("ALERT: ",name," Variable:",colnames(list[[name]])[6]))
        colnames(list[[name]])[6]<- "proBeijing"
        list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))}
    }
    }
    if(name %in% anti_Beijing) {
      print(paste0(name," is an anti-Beijing poll"))
      list[[name]]$proBeijing=-list[[name]]$proBeijing}
    if ("認知率Recognition.Rate" %in% names(list[[name]]) |"認知率Recognition.rate" %in% names(list[[name]])) {
      list[[name]]<- rename(list[[name]], any_of(c(recognition="認知率Recognition.Rate",recognition="認知率Recognition.rate")))
      list[[name]]$recognition <- as.numeric(sub("%","",list[[name]]$recognition))
    } 
    if ("回應率Response.rate" %in% names(list[[name]]) |"回應率Response.Rate" %in% names(list[[name]])) {
      list[[name]]<- rename(list[[name]], any_of(c(response="回應率Response.rate",response="回應率Response.Rate")))
      list[[name]]$response <- as.numeric(sub("%","",list[[name]]$response))
    } 
    list[[name]]$sensitivity <- sensitivity[which(name_list==name)]
    a <- list[[name]] %>% select(any_of(c("Date","response","recognition","proBeijing","sensitivity")))
    print(colnames(a))
    save(a,file=paste(name, "Rdata", sep="."))
  }

#clean Popularity of Political Groups (17)
  rm(list = ls()) # clear workspace
  name_list <- c('pop_DAB','pop_HKFTU', 'pop_NPP','pop_ADPL','pop_Labour','pop_Democrat','pop_Civic')
  anti_Beijing <- c('pop_ADPL','pop_Labour','pop_Democrat','pop_Civic')
  sensitivity <- c('medium','medium','medium','high','medium','high','high')
  setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
  list<- lapply(name_list, function(name) {
    read.csv(paste0(name, ".csv"))
  }) #read the datasets into a large list
  names(list) <- name_list
  setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
  for (name in name_list) {
    list[[name]]<- rename(list[[name]], Date= "調查開始日期Survey.Start.Date")
    list[[name]]$Date <- as.Date(list[[name]]$Date)
    if ("淨值Net.Value" %in% names(list[[name]]) | "淨值Netvalue" %in% names(list[[name]]) | "淨值Net.value" %in% names(list[[name]])) {
      list[[name]]<- rename(list[[name]], any_of(c(proBeijing="淨值Net.Value", proBeijing="淨值Netvalue", proBeijing="淨值Net.value")))
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
    } else { if("評分Rating" %in% names(list[[name]])) {
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$評分Rating))
    } else { if("支持度Support.rating" %in% names(list[[name]])) {
      list[[name]]$proBeijing <- (list[[name]]$支持度Support.rating-50)*2
    } else{print(colnames(list[[name]])[6])
      if("唔知.難講DK.HS" %in% names(list[[name]])){
        colnames(list[[name]])[6]<- "proBeijing"
        list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
      } else{print(paste0("ALERT: ",name," Variable:",colnames(list[[name]])[6]))
        colnames(list[[name]])[6]<- "proBeijing"
        list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))}
    }
    }
    }
    if(name %in% anti_Beijing) {
      print(paste0(name," is an anti-Beijing poll"))
      list[[name]]$proBeijing=-list[[name]]$proBeijing}
    if ("認知率Recognition.Rate" %in% names(list[[name]]) |"認知率Recognition.rate" %in% names(list[[name]])) {
      list[[name]]<- rename(list[[name]], any_of(c(recognition="認知率Recognition.Rate",recognition="認知率Recognition.rate")))
      list[[name]]$recognition <- as.numeric(sub("%","",list[[name]]$recognition))
    } 
    if ("回應率Response.rate" %in% names(list[[name]]) |"回應率Response.Rate" %in% names(list[[name]])) {
      list[[name]]<- rename(list[[name]], any_of(c(response="回應率Response.rate",response="回應率Response.Rate")))
      list[[name]]$response <- as.numeric(sub("%","",list[[name]]$response))
    } 
    list[[name]]$sensitivity <- sensitivity[which(name_list==name)]
    a <- list[[name]] %>% select(any_of(c("Date","response","recognition","proBeijing","sensitivity")))
    print(colnames(a))
    save(a,file=paste(name, "Rdata", sep="."))
  }
  
#clean Popularity of Cross-Strait Political Figures (17)
  rm(list = ls()) # clear workspace
  name_list <- c('pop_Xi','pop_Tsai')
  anti_Beijing <- c('pop_Tsai')
  sensitivity <- c('high','medium')
  setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
  list<- lapply(name_list, function(name) {
    read.csv(paste0(name, ".csv"))
  }) #read the datasets into a large list
  names(list) <- name_list
  setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
  for (name in name_list) {
    list[[name]]<- rename(list[[name]], Date= "調查開始日期Survey.Start.Date")
    list[[name]]$Date <- as.Date(list[[name]]$Date)
    if ("淨值Net.Value" %in% names(list[[name]]) | "淨值Netvalue" %in% names(list[[name]]) | "淨值Net.value" %in% names(list[[name]])) {
      list[[name]]<- rename(list[[name]], any_of(c(proBeijing="淨值Net.Value", proBeijing="淨值Netvalue", proBeijing="淨值Net.value")))
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
    } else { if("評分Rating" %in% names(list[[name]])) {
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$評分Rating))
    } else { if("支持度Support.rating" %in% names(list[[name]])) {
      list[[name]]$proBeijing <- (list[[name]]$支持度Support.rating-50)*2
    } else{print(colnames(list[[name]])[6])
      if("唔知.難講DK.HS" %in% names(list[[name]])){
        colnames(list[[name]])[6]<- "proBeijing"
        list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
      } else{print(paste0("ALERT: ",name," Variable:",colnames(list[[name]])[6]))
        colnames(list[[name]])[6]<- "proBeijing"
        list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))}
    }
    }
    }
    if(name %in% anti_Beijing) {
      print(paste0(name," is an anti-Beijing poll"))
      list[[name]]$proBeijing=-list[[name]]$proBeijing}
    if ("認知率Recognition.Rate" %in% names(list[[name]]) |"認知率Recognition.rate" %in% names(list[[name]])) {
      list[[name]]<- rename(list[[name]], any_of(c(recognition="認知率Recognition.Rate",recognition="認知率Recognition.rate")))
      list[[name]]$recognition <- as.numeric(sub("%","",list[[name]]$recognition))
    } 
    if ("回應率Response.rate" %in% names(list[[name]]) |"回應率Response.Rate" %in% names(list[[name]])) {
      list[[name]]<- rename(list[[name]], any_of(c(response="回應率Response.rate",response="回應率Response.Rate")))
      list[[name]]$response <- as.numeric(sub("%","",list[[name]]$response))
    } 
    list[[name]]$sensitivity <- sensitivity[which(name_list==name)]
    a <- list[[name]] %>% select(any_of(c("Date","response","recognition","proBeijing","sensitivity")))
    print(colnames(a))
    save(a,file=paste(name, "Rdata", sep="."))
  }
  
#clean Surveys on Financial Budget (8)
  rm(list = ls()) # clear workspace
  name_list <- c('budgetrate','satbudget')
  anti_Beijing <- c()
  sensitivity <- c('low','low')
  setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "raw data", sep = "/")) #select raw data directory
  list<- lapply(name_list, function(name) {
    read.csv(paste0(name, ".csv"))
  }) #read the datasets into a large list
  names(list) <- name_list
  setwd(paste (dirname(rstudioapi::getSourceEditorContext()$path), "cleaned data", sep = "/")) #select raw data directory
  for (name in name_list) {
    list[[name]]<- rename(list[[name]], Date= "調查開始日期Survey.Start.Date")
    list[[name]]$Date <- as.Date(list[[name]]$Date)
    if ("淨值Net.Value" %in% names(list[[name]]) | "淨值Netvalue" %in% names(list[[name]]) | "淨值Net.value" %in% names(list[[name]])) {
      list[[name]]<- rename(list[[name]], any_of(c(proBeijing="淨值Net.Value", proBeijing="淨值Netvalue", proBeijing="淨值Net.value")))
      list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
    } else { if("評分Rating" %in% names(list[[name]])) {
      list[[name]]$proBeijing <- (as.numeric(sub("%","",list[[name]]$評分Rating))-50)*2
    } else { if("支持度Support.rating" %in% names(list[[name]])) {
      list[[name]]$proBeijing <- (list[[name]]$支持度Support.rating-50)*2
    } else{print(colnames(list[[name]])[6])
      if("唔知.難講DK.HS" %in% names(list[[name]])){
        colnames(list[[name]])[6]<- "proBeijing"
        list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))
      } else{print(paste0("ALERT: ",name," Variable:",colnames(list[[name]])[6]))
        colnames(list[[name]])[6]<- "proBeijing"
        list[[name]]$proBeijing <- as.numeric(sub("%","",list[[name]]$proBeijing))}
    }
    }
    }
    if(name %in% anti_Beijing) {
      print(paste0(name," is an anti-Beijing poll"))
      list[[name]]$proBeijing=-list[[name]]$proBeijing}
    if ("認知率Recognition.Rate" %in% names(list[[name]]) |"認知率Recognition.rate" %in% names(list[[name]])) {
      list[[name]]<- rename(list[[name]], any_of(c(recognition="認知率Recognition.Rate",recognition="認知率Recognition.rate")))
      list[[name]]$recognition <- as.numeric(sub("%","",list[[name]]$recognition))
    } 
    if ("回應率Response.rate" %in% names(list[[name]]) |"回應率Response.Rate" %in% names(list[[name]])) {
      list[[name]]<- rename(list[[name]], any_of(c(response="回應率Response.rate",response="回應率Response.Rate")))
      list[[name]]$response <- as.numeric(sub("%","",list[[name]]$response))
    } 
    list[[name]]$sensitivity <- sensitivity[which(name_list==name)]
    a <- list[[name]] %>% select(any_of(c("Date","response","recognition","proBeijing","sensitivity")))
    print(colnames(a))
    save(a,file=paste(name, "Rdata", sep="."))
  }