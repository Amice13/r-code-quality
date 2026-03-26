clear_and_match_inc_le <- function(data, closuretype){
  nameA <- paste0(closuretype, "_t")
  nameB <- paste0(closuretype, "_t2")
  
  data <- data %>% 
    subset(afstemid!=101056)  %>% #I remove the area 101056. It is a newly build up area in the port of Copenhagen, and thus does not exist in the early periods.
    mutate(A = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf, -3, -2 ,-1), 0, 1),
           B = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf), 0, 1),
           afstemid_p=paste0(afstemid, party))
  names(data)[36] <- nameA
  names(data)[37] <- nameB
  data <- data %>% 
    group_by(afstemid) %>% 
    mutate(na.incov=ifelse(T %in% is.na(aold)|
                             T %in% is.na(ayoung)|
                             T %in% is.na(anwimm)|
                             T %in% is.na(ahighedu)|
                             T %in% is.na(alowedu)|
                             T %in% is.na(aunemp)|
                             T %in% is.na(loginc)|
                             T %in% is.na(popdens), 1,0)) %>%
    subset(na.incov != 1) 
  data_1 <- data %>% 
    subset(grp_1st==1)
  
  data_2 <- data %>% 
    subset(grp_2nd==1)
  
  # retain only pre period data for matching
  data_1_m <- data_1[which(data_1[,36] == 0),]
  data_2_m <- data_2[which(data_2[,36] == 0),]
  # match for each of the different pretreatment periods.
  data_1_m <- pivot_wider(data_1_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                          names_from = "year",
                          values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                           "loginc" , "popdens"))
  
  data_2_m <- pivot_wider(data_2_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                          names_from = "year",
                          values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                           "loginc" , "popdens"))
  m.le_schp_1 <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2009 + ayoung_2009 + anwimm_2009 + ahighedu_2009 + alowedu_2009 + aunemp_2009 + loginc_2009 + popdens_2009, 
                         data = data_1_m , exact = ~party, 
                         method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  m.le_schp_2 <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2009 + ayoung_2009 + anwimm_2009 + ahighedu_2009 + alowedu_2009 + aunemp_2009 + loginc_2009 + popdens_2009, 
                         data = data_2_m , exact = ~party, 
                         method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  v.le <- data.frame(old = c("distance", "aold_2009", "ayoung_2009", "anwimm_2009", "ahighedu_2009", "alowedu_2009", "aunemp_2009", "loginc_2009", "popdens_2009"),
                     new = c("Propensity Score", "Share 55+ years old", "Share 0-30 years old", "Share Non-western immigrants", "Share with long education",  "Share with short education", "Share unemployed", "Log median household income", "Population density") )
  
  data_1_m <- match.data(m.le_schp_1)
  data_1 <- data_1 %>% 
    mutate(match_s=ifelse(afstemid %in% data_1_m$afstemid, 1, 0))
  
  data_2_m <- match.data(m.le_schp_2)
  data_2 <- data_2 %>% 
    mutate(match_s=ifelse(afstemid%in% data_2_m$afstemid, 1, 0))
  
  return(list(data_1, data_2))
}
clear_and_match_inc_ne <- function(data, closuretype){
  nameA <- paste0(closuretype, "_t")
  nameB <- paste0(closuretype, "_t2")
  data <- data %>% 
    subset(afstemid!=101056)  %>% #I remove the area 101056. It is a newly build up area in the port of Copenhagen, and thus does not exist in the early periods.
    mutate(A = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf, -3, -2 ,-1), 0, 1),
           B = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf), 0, 1),
           afstemid_p=paste0(afstemid, party))
  names(data)[37] <- nameA
  names(data)[38] <- nameB
  data <- data %>% 
    group_by(afstemid) %>% 
    mutate(na.incov=ifelse(T %in% is.na(aold)|
                             T %in% is.na(ayoung)|
                             T %in% is.na(anwimm)|
                             T %in% is.na(ahighedu)|
                             T %in% is.na(alowedu)|
                             T %in% is.na(aunemp)|
                             T %in% is.na(loginc)|
                             T %in% is.na(popdens), 1,0)) %>%
    subset(na.incov != 1) 
  
  data_1 <- data %>% 
    subset(grp_1st==1)
  
  data_2 <- data %>% 
    subset(grp_2nd==1)
  
  data_3 <- data %>% 
    subset(grp_3rd==1)
  
  # retain only pre period data for matching
  data_1_m <- data_1[which(data_1[,37] == 0),]
  data_2_m <- data_2[which(data_2[,37] == 0),]
  data_3_m <- data_3[which(data_3[,37] == 0),]
  
  # match for each of the different pretreatment periods.
  data_1_m <- pivot_wider(data_1_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                          names_from = "year",
                          values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                           "loginc" , "popdens"))
  
  data_2_m <- pivot_wider(data_2_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                          names_from = "year",
                          values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                           "loginc" , "popdens"))
  
  data_3_m <- pivot_wider(data_3_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                          names_from = "year",
                          values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                           "loginc" , "popdens"))
  
  m.ne_schp_1 <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2007 + ayoung_2007 + anwimm_2007 + ahighedu_2007 + alowedu_2007 + aunemp_2007 + loginc_2007 + popdens_2007, 
                         data = data_1_m , exact = ~party, 
                         method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  m.ne_schp_2 <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2007 + ayoung_2007 + anwimm_2007 + ahighedu_2007 + alowedu_2007 + aunemp_2007 + loginc_2007 + popdens_2007, 
                         data = data_2_m , exact = ~party, 
                         method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  m.ne_schp_3 <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2007 + ayoung_2007 + anwimm_2007 + ahighedu_2007 + alowedu_2007 + aunemp_2007+ loginc_2007 + popdens_2007, 
                         data = data_3_m , exact = ~party, 
                         method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  v.ne <- data.frame(old = c("distance", "aold_2007", "ayoung_2007", "anwimm_2007", "ahighedu_2007", "alowedu_2007", "aunemp_2007", "loginc_2007", "popdens_2007"),
                     new = c("Propensity Score", "Share 55+ years old", "Share 0-30 years old", "Share Non-western immigrants", "Share with long education",  "Share with short education", "Share unemployed", "Log median household income", "Population density") )
  
  
  data_1_m <- match.data(m.ne_schp_1)
  data_1 <- data_1 %>% 
    mutate(match_s=ifelse(afstemid %in% data_1_m$afstemid, 1, 0))
  
  data_2_m <- match.data(m.ne_schp_2)
  data_2 <- data_2 %>% 
    mutate(match_s=ifelse(afstemid %in% data_2_m$afstemid, 1, 0))
  
  data_3_m <- match.data(m.ne_schp_3)
  data_3 <- data_3 %>% 
    mutate(match_s=ifelse(afstemid %in% data_3_m$afstemid, 1, 0))
  
  return(list(data_1, data_2, data_3))
}
clear_and_match_rwp_le <- function(data, closuretype){
  nameA <- paste0(closuretype, "_t")
  nameB <- paste0(closuretype, "_t2")
  
  data <- data %>% 
    mutate(A = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf, -3, -2 ,-1), 0, 1),
           B = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf), 0, 1)
    )
  names(data)[131] <- nameA
  names(data)[132] <- nameB
  data <- data %>% 
    group_by(afstemid) %>% 
    mutate(na.incov=ifelse(T %in% is.na(aold)|
                             T %in% is.na(ayoung)|
                             T %in% is.na(anwimm)|
                             T %in% is.na(ahighedu)|
                             T %in% is.na(alowedu)|
                             T %in% is.na(aunemp)|
                             T %in% is.na(loginc)|
                             T %in% is.na(popdens), 1,0)) %>%
    subset(na.incov != 1) 
  
  # retain only pre period data for matching
  data_m <- data[which(data[,131] == 0),]
  # match for each of the different pretreatment periods.
  data_m <- pivot_wider(data_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g")),
                        names_from = "year",
                        values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                         "loginc" , "popdens"))
  
  m.le_1 <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2009 + ayoung_2009 + anwimm_2009 + ahighedu_2009 + alowedu_2009 + aunemp_2009 + loginc_2009 + popdens_2009, 
                    data = data_m , 
                    method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  v.le <- data.frame(old = c("distance", "aold_2009", "ayoung_2009", "anwimm_2009", "ahighedu_2009", "alowedu_2009", "aunemp_2009", "loginc_2009", "popdens_2009"),
                     new = c("Propensity Score", "Share 55+ years old", "Share 0-30 years old", "Share Non-western immigrants", "Share with long education",  "Share with short education", "Share unemployed", "Log median household income", "Population density") )
  
  data_m <- match.data(m.le_1)
  data <- data %>% 
    mutate(match_s=ifelse(afstemid %in% data_m$afstemid, 1, 0))
  
  return(list(data, m.le_1))
}
clear_and_match_rwp_ne <- function(data, closuretype){
  nameA <- paste0(closuretype, "_t")
  nameB <- paste0(closuretype, "_t2")
  
  data <- data %>% 
    mutate(A = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf, -3, -2 ,-1), 0, 1),
           B = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf), 0, 1)
    )
  names(data)[131] <- nameA
  names(data)[132] <- nameB
  data <- data %>% 
    group_by(afstemid) %>% 
    mutate(na.incov=ifelse(T %in% is.na(aold)|
                             T %in% is.na(ayoung)|
                             T %in% is.na(anwimm)|
                             T %in% is.na(ahighedu)|
                             T %in% is.na(alowedu)|
                             T %in% is.na(aunemp)|
                             T %in% is.na(loginc)|
                             T %in% is.na(popdens), 1,0)) %>%
    subset(na.incov != 1) 
  
  # retain only pre period data for matching
  data_m <- data[which(data[,131] == 0),]
  # match for each of the different pretreatment periods.
  data_m <- pivot_wider(data_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g")),
                        names_from = "year",
                        values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                         "loginc" , "popdens"))
  
  m.ne_1 <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2007 + ayoung_2007 + anwimm_2007 + ahighedu_2007 + alowedu_2007 + aunemp_2007 + loginc_2007 + popdens_2007, 
                    data = data_m , 
                    method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  v.ne <- data.frame(old = c("distance", "aold_2007", "ayoung_2007", "anwimm_2007", "ahighedu_2007", "alowedu_2007", "aunemp_2007", "loginc_2007", "popdens_2007"),
                     new = c("Propensity Score", "Share 55+ years old", "Share 0-30 years old", "Share Non-western immigrants", "Share with long education",  "Share with short education", "Share unemployed", "Log median household income", "Population density") )
  
  data_m <- match.data(m.ne_1)
  data <- data %>% 
    mutate(match_s=ifelse(afstemid %in% data_m$afstemid, 1, 0))
  
  return(list(data, m.ne_1))
}
extract.inc.le <- function(Model1.1, Model1.2) {
  out <- tibble()
  out[1:4,1] <- factor(c("p-2", "p-1", "p", "p+1"), levels = c("p-2", "p-1", "p", "p+1"))
  names(out)[1] <- "time"
  
  metacomb <- tibble()
  
  metacomb[1,1] <- Model1.1$group[1]
  metacomb[2,1] <- Model1.2$group[1]
  names(metacomb)[1] <- "study"
  metacomb[1,2] <- Model1.1$att[2]
  metacomb[2,2] <- Model1.2$att[3]
  names(metacomb)[2] <- "att"
  metacomb[1,3] <- Model1.1$se[2]
  metacomb[2,3] <- Model1.2$se[3]
  names(metacomb)[3] <- "att.sd"
  sa <- meta.summaries(att, att.sd, method= "fixed", data = metacomb)
  
  out[1,2] <- Model1.2$att[1]*100
  out[2,2] <- NA
  out[3,2] <- sa$summary*100
  out[4,2] <- Model1.1$att[3]*100
  names(out)[2] <- "att"
  out[1,3] <- (Model1.2$att[1]- Model1.2$se[1]*Model1.2$c)*100
  out[2,3] <- NA
  out[3,3] <- summary(sa)$summci[1]*100
  out[4,3] <- (Model1.1$att[3]- Model1.1$se[3]*Model1.1$c)*100
  names(out)[3] <- "conf.low"
  out[1,4] <- (Model1.2$att[1]+ Model1.2$se[1]*Model1.2$c)*100
  out[2,4] <- NA
  out[3,4] <- summary(sa)$summci[3]*100
  out[4,4] <- (Model1.1$att[3]+ Model1.1$se[3]*Model1.1$c)*100
  names(out)[4] <- "conf.high"
  out[1,5] <- NA
  out[2,5] <- NA
  out[3,5] <- "meta"
  out[4,5] <- NA
  names(out)[5] <- "meta"
  out[1,6] <- Model1.2$se[1]*100
  out[2,6] <- NA
  out[3,6] <- sa$se.summary*100
  out[4,6] <- Model1.1$se[3]*100
  names(out)[6] <- "se"
  
  return(out)
}
extract.inc.ne <- function(Model2.1, Model2.2, Model2.3) {
  out <- tibble()
  out[1:6,1] <- factor(c("p-3", "p-2", "p-1", "p", "p+1", "p+2"), levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2"))
  names(out)[1] <- "time"
  
  metacomb1 <- tibble()
  metacomb1[1,1] <- Model2.1$group[1]
  metacomb1[2,1] <- Model2.2$group[1]
  metacomb1[3,1] <- Model2.3$group[1]
  names(metacomb1)[1] <- "study"
  metacomb1[1,2] <- Model2.1$att[2]
  metacomb1[2,2] <- Model2.2$att[3]
  metacomb1[3,2] <- Model2.3$att[4]
  names(metacomb1)[2] <- "att"
  metacomb1[1,3] <- Model2.1$se[2]
  metacomb1[2,3] <- Model2.2$se[3]
  metacomb1[3,3] <- Model2.3$se[4]
  names(metacomb1)[3] <- "att.sd"
  sa1 <- meta.summaries(att, att.sd, method= "fixed", data = metacomb1)
  
  metacomb2 <- tibble()
  metacomb2[1,1] <- Model2.3$group[1]
  metacomb2[2,1] <- Model2.2$group[1]
  names(metacomb2)[1] <- "study"
  metacomb2[1,2] <- Model2.3$att[2]
  metacomb2[2,2] <- Model2.2$att[1]
  names(metacomb2)[2] <- "att"
  metacomb2[1,3] <- Model2.3$se[2]
  metacomb2[2,3] <- Model2.2$se[1]
  names(metacomb2)[3] <- "att.sd"
  sa2 <- meta.summaries(att, att.sd, method= "fixed", data = metacomb2)
  
  metacomb3 <- tibble()
  metacomb3[1,1] <- Model2.2$group[1]
  metacomb3[2,1] <- Model2.1$group[1]
  names(metacomb3)[1] <- "study"
  metacomb3[1,2] <- Model2.2$att[4]
  metacomb3[2,2] <- Model2.1$att[3]
  names(metacomb3)[2] <- "att"
  metacomb3[1,3] <- Model2.2$se[4]
  metacomb3[2,3] <- Model2.1$se[3]
  names(metacomb3)[3] <- "att.sd"
  sa3 <- meta.summaries(att, att.sd, method= "fixed", data = metacomb3)
  
  out[1,2] <- Model2.3$att[1]*100
  out[2,2] <- sa2$summary*100
  out[3,2] <- NA
  out[4,2] <- sa1$summary*100
  out[5,2] <- sa3$summary*100
  out[6,2] <- Model2.1$att[4]*100
  names(out)[2] <- "att"
  out[1,3] <- (Model2.3$att[1] - Model2.3$se[1]*Model2.3$c)*100
  out[2,3] <- summary(sa2)$summci[1]*100
  out[3,3] <- NA
  out[4,3] <- summary(sa1)$summci[1]*100
  out[5,3] <- summary(sa3)$summci[1]*100
  out[6,3] <- (Model2.1$att[4] - Model2.1$se[3]*Model2.1$c)*100
  names(out)[3] <- "conf.low"
  out[1,4] <- (Model2.3$att[1] + Model2.3$se[1]*Model2.3$c)*100
  out[2,4] <- summary(sa2)$summci[3]*100
  out[3,4] <- NA
  out[4,4] <- summary(sa1)$summci[3]*100
  out[5,4] <- summary(sa3)$summci[3]*100
  out[6,4] <- (Model2.1$att[4]+ Model2.1$se[3]*Model2.1$c)*100
  names(out)[4] <- "conf.high"
  out[1,5] <- NA
  out[2,5] <- "meta"
  out[3,5] <- NA
  out[4,5] <- "meta"
  out[5,5] <- "meta"
  out[6,5] <- NA
  names(out)[5] <- "meta"
  out[1,6] <- Model2.3$se[1]*100
  out[2,6] <- sa2$se.summary*100
  out[3,6] <- NA
  out[4,6] <- sa1$se.summary*100
  out[5,6] <- sa3$se.summary*100
  out[6,6] <- Model2.1$se[4]*100
  names(out)[6] <- "se"
  return(out)
}

clear_and_match_inc_le_dist <- function(data_EN, closuretype, treshhold){
  nameA <- paste0(closuretype, "_t")
  nameB <- paste0(closuretype, "_t2")
  nameC <- paste0(closuretype, "_deltadistg")

    data_EN <- data_EN %>% 
    subset(afstemid!=101056)  %>% #I remove the area 101056. It is a newly build up area in the port of Copenhagen, and thus does not exist in the early periods.
    mutate(A = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf, -3, -2 ,-1), 0, 1),
           B = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf), 0, 1),
           C = ifelse(get(paste0(closuretype, "_deltadist"))>0 & get(paste0(closuretype, "_deltadist")) < treshhold,  paste0("Less than ",treshhold, " km"),
                      ifelse(get(paste0(closuretype, "_deltadist"))>= treshhold ,  paste0("More than or equal to",treshhold, " km"), "Control")),
           afstemid_p=paste0(afstemid, party))
  
  names(data_EN)[36] <- nameA
  names(data_EN)[37] <- nameB
  names(data_EN)[38] <- nameC
  
  data_EN <- data_EN %>% 
    group_by(afstemid) %>% 
    mutate(na.incov=ifelse(T %in% is.na(aold)|
                             T %in% is.na(ayoung)|
                             T %in% is.na(anwimm)|
                             T %in% is.na(ahighedu)|
                             T %in% is.na(alowedu)|
                             T %in% is.na(aunemp)|
                             T %in% is.na(loginc)|
                             T %in% is.na(popdens), 1,0)) %>%
    subset(na.incov != 1) 
  data_1_low <- data_EN %>% 
    subset(grp_1st==1 & get(nameC)==paste0("Less than ",treshhold, " km")|
             grp_1st==1 & get(nameC)=="Control")
  data_1_high <- data_EN %>% 
    subset(grp_1st==1 & get(nameC)==paste0("More than or equal to",treshhold, " km")|
             grp_1st==1 & get(nameC)=="Control")
  
  data_2_low <- data_EN %>% 
    subset(grp_2nd==1 & get(nameC)==paste0("Less than ",treshhold, " km")|
             grp_2nd==1 & get(nameC)=="Control")
  data_2_high <- data_EN %>% 
    subset(grp_2nd==1 & get(nameC)==paste0("More than or equal to",treshhold, " km")|
             grp_2nd==1 & get(nameC)=="Control")
  
  # retain only pre period data for matching
  data_1_low_m <- data_1_low[which(data_1_low[,36] == 0),]
  data_1_high_m <- data_1_high[which(data_1_high[,36] == 0),]
  
  data_2_low_m <- data_2_low[which(data_2_low[,36] == 0),]
  data_2_high_m <- data_2_high[which(data_2_high[,36] == 0),]
  # match for each of the different pretreatment periods.
  data_1_low_m <- pivot_wider(data_1_low_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                              names_from = "year",
                              values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                               "loginc" , "popdens"))
  
  data_1_high_m <- pivot_wider(data_1_high_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                               names_from = "year",
                               values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                                "loginc" , "popdens"))
  
  data_2_low_m <- pivot_wider(data_2_low_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                              names_from = "year",
                              values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                               "loginc" , "popdens"))
  data_2_high_m <- pivot_wider(data_2_high_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                               names_from = "year",
                               values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                                "loginc" , "popdens"))
  
  m.le_schp_1_l <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2009 + ayoung_2009 + anwimm_2009 + ahighedu_2009 + alowedu_2009 + aunemp_2009 + loginc_2009 + popdens_2009, 
                           data = data_1_low_m , exact = ~party, 
                           method = "nearest", distance = "glm", link = "logit", ratio = 1)
  m.le_schp_1_h <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2009 + ayoung_2009 + anwimm_2009 + ahighedu_2009 + alowedu_2009 + aunemp_2009 + loginc_2009 + popdens_2009, 
                           data = data_1_high_m , exact = ~party, 
                           method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  m.le_schp_2_l <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2009 + ayoung_2009 + anwimm_2009 + ahighedu_2009 + alowedu_2009 + aunemp_2009 + loginc_2009 + popdens_2009, 
                           data = data_2_low_m , exact = ~party, 
                           method = "nearest", distance = "glm", link = "logit", ratio = 1)
  m.le_schp_2_h <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2009 + ayoung_2009 + anwimm_2009 + ahighedu_2009 + alowedu_2009 + aunemp_2009 + loginc_2009 + popdens_2009, 
                           data = data_2_high_m , exact = ~party, 
                           method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  v.le <- data.frame(old = c("distance", "aold_2009", "ayoung_2009", "anwimm_2009", "ahighedu_2009", "alowedu_2009", "aunemp_2009", "loginc_2009", "popdens_2009"),
                     new = c("Propensity Score", "Share 55+ years old", "Share 0-30 years old", "Share Non-western immigrants", "Share with long education",  "Share with short education", "Share unemployed", "Log median household income", "Population density") )
  
  data_1_m_l <- match.data(m.le_schp_1_l)
  data_1_low <- data_1_low %>% 
    mutate(match_s=ifelse(afstemid %in% data_1_m_l$afstemid, 1, 0))
  
  data_1_m_h <- match.data(m.le_schp_1_h)
  data_1_high <- data_1_high %>% 
    mutate(match_s=ifelse(afstemid %in% data_1_m_h$afstemid, 1, 0))
  
  data_2_m_l <- match.data(m.le_schp_2_l)
  data_2_low <- data_2_low %>% 
    mutate(match_s=ifelse(afstemid %in% data_2_m_l$afstemid, 1, 0))
  
  data_2_m_h <- match.data(m.le_schp_2_h)
  data_2_high <- data_2_high %>% 
    mutate(match_s=ifelse(afstemid %in% data_2_m_h$afstemid, 1, 0))
  
  
  return(list(data_1_low, data_1_high, data_2_low, data_2_high))
}
clear_and_match_inc_ne_dist <- function(data_EN, closuretype, treshhold){
  nameA <- paste0(closuretype, "_t")
  nameB <- paste0(closuretype, "_t2")
  nameC <- paste0(closuretype, "_deltadistg")
  
  data_EN <- data_EN %>% 
    subset(afstemid!=101056)  %>% #I remove the area 101056. It is a newly build up area in the port of Copenhagen, and thus does not exist in the early periods.
    mutate(A = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf, -3, -2 ,-1), 0, 1),
           B = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf), 0, 1),
           C = ifelse(get(paste0(closuretype, "_deltadist"))>0 & get(paste0(closuretype, "_deltadist")) < treshhold,  paste0("Less than ",treshhold, " km"),
                      ifelse(get(paste0(closuretype, "_deltadist"))>= treshhold ,  paste0("More than or equal to",treshhold, " km"), "Control")),
           
           afstemid_p=paste0(afstemid, party))
  names(data_EN)[37] <- nameA
  names(data_EN)[38] <- nameB
  names(data_EN)[39] <- nameC
  data_EN <- data_EN %>% 
    group_by(afstemid) %>% 
    mutate(na.incov=ifelse(T %in% is.na(aold)|
                             T %in% is.na(ayoung)|
                             T %in% is.na(anwimm)|
                             T %in% is.na(ahighedu)|
                             T %in% is.na(alowedu)|
                             T %in% is.na(aunemp)|
                             T %in% is.na(loginc)|
                             T %in% is.na(popdens), 1,0)) %>%
    subset(na.incov != 1) 
  
  data_1_low <- data_EN %>% 
    subset(grp_1st==1 & get(nameC)==paste0("Less than ",treshhold, " km")|
             grp_1st==1 & get(nameC)=="Control")
  data_1_high <- data_EN %>% 
    subset(grp_1st==1 & get(nameC)==paste0("More than or equal to",treshhold, " km")|
             grp_1st==1 & get(nameC)=="Control")
  data_2_low <- data_EN %>% 
    subset(grp_2nd==1 & get(nameC)==paste0("Less than ",treshhold, " km")|
             grp_2nd==1 & get(nameC)=="Control")
  data_2_high <- data_EN %>% 
    subset(grp_2nd==1 & get(nameC)==paste0("More than or equal to",treshhold, " km")|
             grp_2nd==1 & get(nameC)=="Control")
  data_3_low <- data_EN %>% 
    subset(grp_3rd==1 & get(nameC)==paste0("Less than ",treshhold, " km")|
             grp_3rd==1 & get(nameC)=="Control")
  data_3_high <- data_EN %>% 
    subset(grp_3rd==1 & get(nameC)==paste0("More than or equal to",treshhold, " km")|
             grp_3rd==1 & get(nameC)=="Control")
  
  
  # retain only pre period data for matching
  data_1_low_m <- data_1_low[which(data_1_low[,37] == 0),]
  data_1_high_m <- data_1_high[which(data_1_high[,37] == 0),]
  data_2_low_m <- data_2_low[which(data_2_low[,37] == 0),]
  data_2_high_m <- data_2_high[which(data_2_high[,37] == 0),]
  data_3_low_m <- data_3_low[which(data_3_low[,37] == 0),]
  data_3_high_m <- data_3_high[which(data_3_high[,37] == 0),]
  # match for each of the different pretreatment periods.
  data_1_low_m <- pivot_wider(data_1_low_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                              names_from = "year",
                              values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                               "loginc" , "popdens"))
  data_1_high_m <- pivot_wider(data_1_high_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                               names_from = "year",
                               values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                                "loginc" , "popdens"))
  data_2_low_m <- pivot_wider(data_2_low_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                              names_from = "year",
                              values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                               "loginc" , "popdens"))
  data_2_high_m <- pivot_wider(data_2_high_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                               names_from = "year",
                               values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                                "loginc" , "popdens"))
  data_3_low_m <- pivot_wider(data_3_low_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                              names_from = "year",
                              values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                               "loginc" , "popdens"))
  data_3_high_m <- pivot_wider(data_3_high_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g"), "party"),
                               names_from = "year",
                               values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                                "loginc" , "popdens"))
  
  m.ne_schp_1.l <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2007 + ayoung_2007 + anwimm_2007 + ahighedu_2007 + alowedu_2007 + aunemp_2007 + loginc_2007 + popdens_2007, 
                           data = data_1_low_m , exact = ~party, 
                           method = "nearest", distance = "glm", link = "logit", ratio = 1)
  m.ne_schp_1.h <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2007 + ayoung_2007 + anwimm_2007 + ahighedu_2007 + alowedu_2007 + aunemp_2007 + loginc_2007 + popdens_2007, 
                           data = data_1_high_m , exact = ~party, 
                           method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  m.ne_schp_2.l <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2007 + ayoung_2007 + anwimm_2007 + ahighedu_2007 + alowedu_2007 + aunemp_2007 + loginc_2007 + popdens_2007, 
                           data = data_2_low_m , exact = ~party, 
                           method = "nearest", distance = "glm", link = "logit", ratio = 1)
  m.ne_schp_2.h <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2007 + ayoung_2007 + anwimm_2007 + ahighedu_2007 + alowedu_2007 + aunemp_2007 + loginc_2007 + popdens_2007, 
                           data = data_2_high_m , exact = ~party, 
                           method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  m.ne_schp_3.l <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2007 + ayoung_2007 + anwimm_2007 + ahighedu_2007 + alowedu_2007 + aunemp_2007 + loginc_2007 + popdens_2007, 
                           data = data_3_low_m , exact = ~party, 
                           method = "nearest", distance = "glm", link = "logit", ratio = 1)
  m.ne_schp_3.h <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2007 + ayoung_2007 + anwimm_2007 + ahighedu_2007 + alowedu_2007 + aunemp_2007 + loginc_2007 + popdens_2007, 
                           data = data_3_high_m , exact = ~party, 
                           method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  v.ne <- data.frame(old = c("distance", "aold_2007", "ayoung_2007", "anwimm_2007", "ahighedu_2007", "alowedu_2007", "aunemp_2007", "loginc_2007", "popdens_2007"),
                     new = c("Propensity Score", "Share 55+ years old", "Share 0-30 years old", "Share Non-western immigrants", "Share with long education",  "Share with short education", "Share unemployed", "Log median household income", "Population density") )
  
  data_1_m_l <- match.data(m.ne_schp_1.l)
  data_1_low <- data_1_low %>% 
    mutate(match_s=ifelse(afstemid %in% data_1_m_l$afstemid, 1, 0))
  data_1_m_h <- match.data(m.ne_schp_1.h)
  data_1_high <- data_1_high %>% 
    mutate(match_s=ifelse(afstemid %in% data_1_m_h$afstemid, 1, 0))
  
  data_2_m_l <- match.data(m.ne_schp_2.l)
  data_2_low <- data_2_low %>% 
    mutate(match_s=ifelse(afstemid %in% data_2_m_l$afstemid, 1, 0))
  data_2_m_h <- match.data(m.ne_schp_2.h)
  data_2_high <- data_2_high %>% 
    mutate(match_s=ifelse(afstemid %in% data_2_m_h$afstemid, 1, 0))
  
  data_3_m_l <- match.data(m.ne_schp_3.l)
  data_3_low <- data_3_low %>% 
    mutate(match_s=ifelse(afstemid %in% data_3_m_l$afstemid, 1, 0))
  data_3_m_h <- match.data(m.ne_schp_3.h)
  data_3_high <- data_3_high %>% 
    mutate(match_s=ifelse(afstemid %in% data_3_m_h$afstemid, 1, 0))
  
  return(list(data_1_low, data_1_high, data_2_low, data_2_high, data_3_low, data_3_high))
}
extract.rwp.le <- function(rwp_le_schp_1u, rwp_le_schp_1m) {
  out <- tibble()
  sa <- aggte(rwp_le_schp_1u, type = "dynamic", clustervars = "afstemid",  cband = T,  bstrap = T)
  
  out[1:4,1] <- factor(c("p-2", "p-1", "p", "p+1"), levels = c("p-2", "p-1", "p", "p+1"))
  names(out)[1] <- "time"
  out[1:4,2] <- c(sa$att[1], NA, sa$att[3:4]) *100
  names(out)[2] <- "att"
  out[1:4,3] <- (sa$att[1:4] - sa$se[1:4]* sa$crit.val.egt) *100
  names(out)[3] <- "conf.low"
  out[1:4,4] <- (sa$att[1:4] + sa$se[1:4]* sa$crit.val.egt) *100
  names(out)[4] <- "conf.high"
  out[1:4,5] <- "Unmatched"
  names(out)[5] <- "Model"
  out[1:4,6] <- "Local Election:\n School Closure"
  names(out)[6] <- "elec"
  out[1:4,7] <- sa$se*100
  names(out)[7] <- "se"
  
  out[1:4,8] <- ifelse((sa$att[1:4] - sa$se[1:4]* sa$crit.val.egt)>0 &
                         (sa$att[1:4] + sa$se[1:4]* sa$crit.val.egt)>0 |
                         (sa$att[1:4] - sa$se[1:4]* sa$crit.val.egt)<0 &
                         (sa$att[1:4] + sa$se[1:4]* sa$crit.val.egt)<0, "*", "")
  names(out)[8] <- "sig"
  
  
  
  sb <- aggte(rwp_le_schp_1m, type = "dynamic", clustervars = "afstemid",  cband = T,  bstrap = T)
  out[5:8,1] <- factor(c("p-2", "p-1", "p", "p+1"), levels = c("p-2", "p-1", "p", "p+1"))
  names(out)[1] <- "time"
  out[5:8,2] <- c(sb$att[1], NA, sb$att[3:4])  *100
  names(out)[2] <- "att"
  out[5:8,3] <- (sb$att[1:4] - sb$se[1:4]* sb$crit.val.egt) *100
  names(out)[3] <- "conf.low"
  out[5:8,4] <- (sb$att[1:4] + sb$se[1:4]* sb$crit.val.egt) *100
  names(out)[4] <- "conf.high"
  out[5:8,5] <- "Matched"
  names(out)[5] <- "Model"
  out[5:8,6] <- "Local Election:\n School Closure"
  names(out)[6] <- "elec"
  out[5:8,7] <- sb$se*100
  names(out)[7] <- "se"
  out[5:8,8] <- ifelse((sb$att[1:4] - sb$se[1:4]* sb$crit.val.egt)>0 &
                         (sb$att[1:4] + sb$se[1:4]* sb$crit.val.egt)>0 |
                         (sb$att[1:4] - sb$se[1:4]* sb$crit.val.egt)<0 &
                         (sb$att[1:4] + sb$se[1:4]* sb$crit.val.egt)<0, "*", "")
  names(out)[8] <- "sig"
  
  return(out)
}
extract.rwp.ne <- function(rwp_ne_hosd1_1u, rwp_ne_hosd1_1m) {
  out <- tibble()
  sa <- aggte(rwp_ne_hosd1_1u, type = "dynamic", clustervars = "afstemid",  cband = T,  bstrap = T)
  
  out[1:6,1] <- factor(c("p-3", "p-2", "p-1", "p", "p+1", "p+2"), levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2"))
  names(out)[1] <- "time"
  out[1:6,2] <- c(sa$att[1:2], NA, sa$att[4:6]) *100
  names(out)[2] <- "att"
  out[1:6,3] <- (sa$att[1:6]  - sa$se[1:6]* sa$crit.val.egt) *100
  names(out)[3] <- "conf.low"
  out[1:6,4] <- (sa$att[1:6] + sa$se[1:6]* sa$crit.val.egt) *100
  names(out)[4] <- "conf.high"
  out[1:6,5] <- "Unmatched"
  names(out)[5] <- "Model"
  out[1:6,6] <- "National Election:\n Hospital Closure"
  names(out)[6] <- "elec"
  out[1:6,7] <- sa$se*100
  names(out)[7] <- "se"
  out[1:6,8] <- ifelse((sa$att[1:6] - sa$se[1:6]* sa$crit.val.egt)>0 &
                         (sa$att[1:6] + sa$se[1:6]* sa$crit.val.egt)>0 |
                         (sa$att[1:6] - sa$se[1:6]* sa$crit.val.egt)<0 &
                         (sa$att[1:6] + sa$se[1:6]* sa$crit.val.egt)<0, "*", "")
  names(out)[8] <- "sig"
  
  
  
  sb <- aggte(rwp_ne_hosd1_1m, type = "dynamic", clustervars = "afstemid",  cband = T,  bstrap = T)
  out[7:12,1] <- factor(c("p-3", "p-2", "p-1", "p", "p+1", "p+2"), levels = c("p-3", "p-2", "p-1", "p", "p+1", "p+2"))
  names(out)[1] <- "time"
  out[7:12,2] <- c(sb$att[1:2], NA, sb$att[4:6])  *100
  names(out)[2] <- "att"
  out[7:12,3] <- (sb$att[1:6] - sb$se[1:6]* sb$crit.val.egt) *100
  names(out)[3] <- "conf.low"
  out[7:12,4] <- (sb$att[1:6] + sb$se[1:6]* sb$crit.val.egt) *100
  names(out)[4] <- "conf.high"
  out[7:12,5] <- "Matched"
  names(out)[5] <- "Model"
  out[7:12,6] <- "National Election:\n Hospital Closure"
  names(out)[6] <- "elec"
  out[7:12,7] <- sb$se*100
  names(out)[7] <- "se"
  out[7:12,8] <- ifelse((sb$att[1:6] - sb$se[1:6]* sb$crit.val.egt)>0 &
                          (sb$att[1:6] + sb$se[1:6]* sb$crit.val.egt)>0 |
                          (sb$att[1:6] - sb$se[1:6]* sb$crit.val.egt)<0 &
                          (sb$att[1:6] + sb$se[1:6]* sb$crit.val.egt)<0, "*", "")
  names(out)[8] <- "sig"
  
  
  return(out)
}
clear_and_match_rwp_le_dist <- function(data_EN, closuretype, treshhold){
  nameA <- paste0(closuretype, "_t")
  nameB <- paste0(closuretype, "_t2")
  nameC <- paste0(closuretype, "_deltadistg")
  
  data_EN <- data_EN %>% 
    subset(afstemid!=101056)  %>% #I remove the area 101056. It is a newly build up area in the port of Copenhagen, and thus does not exist in the early periods.
    mutate(A = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf, -3, -2 ,-1), 0, 1),
           B = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf), 0, 1),
           C = ifelse(get(paste0(closuretype, "_deltadist"))>0 & get(paste0(closuretype, "_deltadist")) < treshhold,  paste0("Less than ",treshhold, " km"),
                      ifelse(get(paste0(closuretype, "_deltadist"))>= treshhold ,  paste0("More than or equal to",treshhold, " km"), "Control")))
  
  
  names(data_EN)[131] <- nameA
  names(data_EN)[132] <- nameB
  names(data_EN)[133] <- nameC
  
  
  
  data_EN <- data_EN %>% 
    group_by(afstemid) %>% 
    mutate(na.incov=ifelse(T %in% is.na(aold)|
                             T %in% is.na(ayoung)|
                             T %in% is.na(anwimm)|
                             T %in% is.na(ahighedu)|
                             T %in% is.na(alowedu)|
                             T %in% is.na(aunemp)|
                             T %in% is.na(loginc)|
                             T %in% is.na(popdens), 1,0)) %>%
    subset(na.incov != 1)
  
  data_low <- data_EN %>% 
    subset(get(nameC)==paste0("Less than ",treshhold, " km")| get(nameC)=="Control")
  data_high <- data_EN %>% 
    subset(get(nameC)==paste0("More than or equal to",treshhold, " km")| get(nameC)=="Control")
  
  # retain only pre period data for matching
  data_low_m <- data_low[which(data_low[,131] == 0),]
  data_high_m <- data_high[which(data_high[,131] == 0),]
  # match for each of the different pretreatment periods.
  data_1_low_m <- pivot_wider(data_low_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g")),
                              names_from = "year",
                              values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                               "loginc" , "popdens"))
  data_1_high_m <- pivot_wider(data_high_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g")),
                               names_from = "year",
                               values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                                "loginc" , "popdens"))
  
  m.le_schp_l <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2009 + ayoung_2009 + anwimm_2009 + ahighedu_2009 + alowedu_2009 + aunemp_2009 + loginc_2009 + popdens_2009, 
                         data = data_1_low_m , 
                         method = "nearest", distance = "glm", link = "logit", ratio = 1)
  m.le_schp_h <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2009 + ayoung_2009 + anwimm_2009 + ahighedu_2009 + alowedu_2009 + aunemp_2009 + loginc_2009 + popdens_2009, 
                         data = data_1_high_m , 
                         method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  
  v.le <- data.frame(old = c("distance", "aold_2009", "ayoung_2009", "anwimm_2009", "ahighedu_2009", "alowedu_2009", "aunemp_2009", "loginc_2009", "popdens_2009"),
                     new = c("Propensity Score", "Share 55+ years old", "Share 0-30 years old", "Share Non-western immigrants", "Share with long education",  "Share with short education", "Share unemployed", "Log median household income", "Population density") )
  
  
  data_m_l <- match.data(m.le_schp_l)
  data_low <- data_low %>% 
    mutate(match_s=ifelse(afstemid %in% data_m_l$afstemid, 1, 0))
  data_m_h <- match.data(m.le_schp_h)
  data_high <- data_high %>% 
    mutate(match_s=ifelse(afstemid %in% data_m_h$afstemid, 1, 0))
  
  
  return(list(data_low, data_high, m.le_schp_l, m.le_schp_h))
}
clear_and_match_rwp_ne_dist <- function(data_EN, closuretype, treshhold){
  nameA <- paste0(closuretype, "_t")
  nameB <- paste0(closuretype, "_t2")
  nameC <- paste0(closuretype, "_deltadistg")
  
  data_EN <- data_EN %>% 
    subset(afstemid!=101056)  %>% #I remove the area 101056. It is a newly build up area in the port of Copenhagen, and thus does not exist in the early periods.
    mutate(A = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf, -3, -2 ,-1), 0, 1),
           B = ifelse(get(paste0(closuretype, "_relyear")) %in% c(Inf), 0, 1),
           C = ifelse(get(paste0(closuretype, "_deltadist"))>0 & get(paste0(closuretype, "_deltadist")) < treshhold,  paste0("Less than ",treshhold, " km"),
                      ifelse(get(paste0(closuretype, "_deltadist"))>= treshhold ,  paste0("More than or equal to",treshhold, " km"), "Control")))
  
  names(data_EN)[131] <- nameA
  names(data_EN)[132] <- nameB
  names(data_EN)[133] <- nameC
  
  data_EN <- data_EN %>% 
    group_by(afstemid) %>% 
    mutate(na.incov=ifelse(T %in% is.na(aold)|
                             T %in% is.na(ayoung)|
                             T %in% is.na(anwimm)|
                             T %in% is.na(ahighedu)|
                             T %in% is.na(alowedu)|
                             T %in% is.na(aunemp)|
                             T %in% is.na(loginc)|
                             T %in% is.na(popdens), 1,0)) %>%
    subset(na.incov != 1)
  
  data_low <- data_EN %>% 
    subset(get(nameC)==paste0("Less than ",treshhold, " km")| get(nameC)=="Control")
  data_high <- data_EN %>% 
    subset(get(nameC)==paste0("More than or equal to",treshhold, " km")| get(nameC)=="Control")
  
  
  # retain only pre period data for matching
  data_low_m <- data_low[which(data_low[,131] == 0),]
  data_high_m <- data_high[which(data_high[,131] == 0),]
  # match for each of the different pretreatment periods.
  
  data_1_low_m <- pivot_wider(data_low_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g")),
                              names_from = "year",
                              values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                               "loginc" , "popdens"))
  data_1_high_m <- pivot_wider(data_high_m, id_cols = c("afstemid", paste0(closuretype, "_t"), paste0(closuretype, "_t2"), paste0(closuretype, "_g")),
                               names_from = "year",
                               values_from = c( "aold" , "ayoung", "anwimm" , "ahighedu" , "alowedu" , "aunemp" , 
                                                "loginc" , "popdens"))
  
  m.ne_low <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2007 + ayoung_2007 + anwimm_2007 + ahighedu_2007 + alowedu_2007 + aunemp_2007 + loginc_2007 + popdens_2007, 
                      data = data_1_low_m , 
                      method = "nearest", distance = "glm", link = "logit", ratio = 1)
  m.ne_high <- matchit(get(paste0(closuretype, "_t2")) ~ aold_2007 + ayoung_2007 + anwimm_2007 + ahighedu_2007 + alowedu_2007 + aunemp_2007 + loginc_2007 + popdens_2007, 
                       data = data_1_high_m , 
                       method = "nearest", distance = "glm", link = "logit", ratio = 1)
  
  
  data_m_l <- match.data(m.ne_low)
  data_low <- data_low %>% 
    mutate(match_s=ifelse(afstemid %in% data_m_l$afstemid, 1, 0))
  data_m_h <- match.data(m.ne_high)
  data_high <- data_high %>% 
    mutate(match_s=ifelse(afstemid %in% data_m_h$afstemid, 1, 0))
  
  return(list(data_low, data_high, m.ne_low, m.ne_high))
}
