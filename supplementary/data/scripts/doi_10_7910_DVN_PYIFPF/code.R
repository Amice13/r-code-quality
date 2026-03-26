rm(list = ls())

save(list = ls(all = TRUE), file= "code.RData")

install.packages("")

library(xlsx)
library(Zelig)
library(MASS)
library(stargazer)





####################################
########## LOAD DATA ###############
####################################

psp <- read.csv("mps_czech_republic.csv",fileEncoding = "Windows-1250")
psp <- psp[psp$id_obdobi<=171,]
psp$pohlavi <- ifelse(psp$pohlavi=="muž",0,1)
colnames(psp)[colnames(psp)=="pohlavi"] <- "zena"

psp <- psp[ , !(names(psp) %in% c("ulice","obec","psc","web","email","telefon","psp_telefon","facebook"))]

psp$prihlasen <- psp$hlasoval + psp$zdrzel_se

psp$psp1 <- ifelse(psp$obdobi=="PSP1",1,0)
psp$psp2 <- ifelse(psp$obdobi=="PSP2",1,0)
psp$psp3 <- ifelse(psp$obdobi=="PSP3",1,0)
psp$psp4 <- ifelse(psp$obdobi=="PSP4",1,0)
psp$psp5 <- ifelse(psp$obdobi=="PSP5",1,0)
psp$psp6 <- ifelse(psp$obdobi=="PSP6",1,0)

psp$geo <- 0
psp$geo <- ifelse(psp$kraj_cz=="Hlavní město Praha" | psp$kraj_cz=="Středočeský", 1, psp$geo)
psp$geo <- ifelse(psp$kraj_cz=="Jihočeský" | psp$kraj_cz=="Západočeský" | psp$kraj_cz=="Severočeský" | psp$kraj_cz=="Východočeský", 2, psp$geo)
psp$geo <- ifelse(psp$kraj_cz=="Jihomoravský" | psp$kraj_cz=="Olomoucký", 3, psp$geo)
psp$geo <- ifelse(psp$kraj_cz=="Severomoravský", 4, psp$geo)
psp$geo <- ifelse(psp$kraj_cz=="Moravskoslezský" | psp$kraj_cz=="Zlínský", 4, psp$geo)
psp$geo <- ifelse(psp$kraj_cz=="Liberecký" | psp$kraj_cz=="Královéhradecký" | psp$kraj_cz=="Pardubický" | psp$kraj_cz=="Vysočina" | psp$kraj_cz=="Jihočeský" | psp$kraj_cz=="Plzeňský" | psp$kraj_cz=="Karlovarský" | psp$kraj_cz=="Ústecký", 2, psp$geo)


parties <- unique(psp$kandidatka_zkratka)
psp$ano <- ifelse(psp$kandidatka_zkratka=="ANO2011",1,0)
psp$hsdsms <- ifelse(psp$kandidatka_zkratka=="HSD-SMS",1,0)
psp$koalice <- ifelse(psp$kandidatka_zkratka=="K",1,0)
psp$kducsl <- ifelse(psp$kandidatka_zkratka=="KDU-CSL",1,0)
psp$kscm <- ifelse(psp$kandidatka_zkratka=="KSCM",1,0)
psp$lb <- ifelse(psp$kandidatka_zkratka=="LB",1,0)
psp$lsu <- ifelse(psp$kandidatka_zkratka=="LSU",1,0)
psp$oda <- ifelse(psp$kandidatka_zkratka=="ODA",1,0)
psp$ods <- ifelse(psp$kandidatka_zkratka=="ODS",1,0)
psp$odskds <- ifelse(psp$kandidatka_zkratka=="ODS-KDS",1,0)
psp$sprrsc <- ifelse(psp$kandidatka_zkratka=="SPR-RSC",1,0)
psp$sz <- ifelse(psp$kandidatka_zkratka=="SZ",1,0)
psp$top09 <- ifelse(psp$kandidatka_zkratka=="TOP09",1,0)
psp$us <- ifelse(psp$kandidatka_zkratka=="US",1,0)
psp$usvit <- ifelse(psp$kandidatka_zkratka=="USVIT",1,0)
psp$vv <- ifelse(psp$kandidatka_zkratka=="VV",1,0)

psp$zakony_rok <- psp$zakony/psp$delka_mandatu*365
psp$interpelace_rok <- psp$interpelace/psp$delka_mandatu*365
psp$promluvy_rok <- psp$promluvy/psp$delka_mandatu*365

psp$vs <- ifelse(is.na(psp$pred) & is.na(psp$za),0,1)

psp$clenstvi <- psp$vybory + psp$komise + psp$podvybory

psp$clenstvi_specific <- psp$vybory_specific + psp$komise_specific + psp$podvybory_specific

psp$predsed <- psp$vybory_predsednictvo + psp$komise_predsednictvo + psp$podvybory_predsednictvo

psp$predsed_specific <- psp$vybory_predsed_specific + psp$komise_predsed_specific + psp$podvybory_predsed_specific

psp_orig <- psp 

psp <- psp[psp$id_poslanec!=202,] # Ivana Janů vymazána, protože odešla před zavedením elektronického systému

psp <- psp[psp$delka_mandatu>=180,]

psp$vlada <- ifelse(psp$vlada==0, 0, 1)

psp$kluby_predsednictvo <- ifelse(psp$kluby_predsednictvo==0, 0, 1)

psp_cl <- psp[psp$id_obdobi>=165,]
psp_cl <- psp_cl[!(psp_cl$id_obdobi==165 & !(as.Date(psp_cl$poslanec_od) <= as.Date("1993-12-01") & as.Date(psp_cl$poslanec_do) >= as.Date("1993-12-01"))),]
psp_cl <- psp_cl[!(psp_cl$id_obdobi==166 & !(as.Date(psp_cl$poslanec_od) <= as.Date("1996-12-01") & as.Date(psp_cl$poslanec_do) >= as.Date("1996-12-01"))),]
psp_cl <- psp_cl[!(psp_cl$id_obdobi==167 & !(as.Date(psp_cl$poslanec_od) <= as.Date("1998-12-20") & as.Date(psp_cl$poslanec_do) >= as.Date("1998-12-20"))),]
psp_cl <- psp_cl[!(psp_cl$id_obdobi==168 & !(as.Date(psp_cl$poslanec_od) <= as.Date("2002-12-15") & as.Date(psp_cl$poslanec_do) >= as.Date("2002-12-15"))),]
psp_cl <- psp_cl[!(psp_cl$id_obdobi==169 & !(as.Date(psp_cl$poslanec_od) <= as.Date("2006-12-03") & as.Date(psp_cl$poslanec_do) >= as.Date("2006-12-03"))),]
psp_cl <- psp_cl[!(psp_cl$id_obdobi==170 & !(as.Date(psp_cl$poslanec_od) <= as.Date("2010-11-29") & as.Date(psp_cl$poslanec_do) >= as.Date("2010-11-29"))),]
psp_cl <- psp_cl[!(psp_cl$id_obdobi==171 & !(as.Date(psp_cl$poslanec_od) <= as.Date("2014-04-26") & as.Date(psp_cl$poslanec_do) >= as.Date("2014-04-26"))),]



####################################
##### Descriptive Information ######
####################################

gender <- as.data.frame(matrix(0,1,4))
colnames(gender) <- c("Term","Total MPs","Total Female MPs","Share of female MPs (percentage)")
gender[1,] <- c("1993",length(psp$zena[psp$obdobi=="PSP1"]),length(psp$zena[psp$obdobi=="PSP1" & psp$zena==1]),round(table(psp$zena[psp$obdobi=="PSP1"])[2]/length(psp$zena[psp$obdobi=="PSP1"])*100,2))
gender[2,] <- c("1996",length(psp$zena[psp$obdobi=="PSP2"]),length(psp$zena[psp$obdobi=="PSP2" & psp$zena==1]),round(table(psp$zena[psp$obdobi=="PSP2"])[2]/length(psp$zena[psp$obdobi=="PSP2"])*100,2))
gender[3,] <- c("1998",length(psp$zena[psp$obdobi=="PSP3"]),length(psp$zena[psp$obdobi=="PSP3" & psp$zena==1]),round(table(psp$zena[psp$obdobi=="PSP3"])[2]/length(psp$zena[psp$obdobi=="PSP3"])*100,2))
gender[4,] <- c("2002",length(psp$zena[psp$obdobi=="PSP4"]),length(psp$zena[psp$obdobi=="PSP4" & psp$zena==1]),round(table(psp$zena[psp$obdobi=="PSP4"])[2]/length(psp$zena[psp$obdobi=="PSP4"])*100,2))
gender[5,] <- c("2006",length(psp$zena[psp$obdobi=="PSP5"]),length(psp$zena[psp$obdobi=="PSP5" & psp$zena==1]),round(table(psp$zena[psp$obdobi=="PSP5"])[2]/length(psp$zena[psp$obdobi=="PSP5"])*100,2))
gender[6,] <- c("2010",length(psp$zena[psp$obdobi=="PSP6"]),length(psp$zena[psp$obdobi=="PSP6" & psp$zena==1]),round(table(psp$zena[psp$obdobi=="PSP6"])[2]/length(psp$zena[psp$obdobi=="PSP6"])*100,2))
gender[7,] <- c("2013",length(psp$zena[psp$obdobi=="PSP7"]),length(psp$zena[psp$obdobi=="PSP7" & psp$zena==1]),round(table(psp$zena[psp$obdobi=="PSP7"])[2]/length(psp$zena[psp$obdobi=="PSP7"])*100,2))
gender[8,] <- c("Total",length(psp$zena),length(psp$zena[psp$zena==1]),round(length(psp$zena[psp$zena==1])/length(psp$zena)*100,2))
write.xlsx(gender, "gender.xlsx")

descriptive <- function(variable){
  round(c(length(variable),mean(variable),sd(variable),min(variable),max(variable)),2)
}
desc <- as.data.frame(matrix(0,1,6))
colnames(desc) <- c("Variable","N","Mean","Standard Deviation","Minimum","Maximum")
desc[1,] <- c("Female",descriptive(psp$zena))
desc[2,] <- c("Age",descriptive(psp$vek))
desc[3,] <- c("Tenure",descriptive(psp$zkusenost))
desc[4,] <- c("Degree Holder",descriptive(psp$vs))
desc[5,] <- c("Geographic Area",descriptive(psp$geo))
desc[6,] <- c("PPG Presidium",descriptive(psp$kluby_predsednictvo))
desc[7,] <- c("Minister",descriptive(psp$vlada))
desc[8,] <- c("Chamber's Presidency",descriptive(psp$predsednictvo))
desc[9,] <- c("Memberships in (Sub)Committees and Commissions",descriptive(psp$clenstvi))
desc[10,] <- c("Voting Attendance (%)",descriptive(psp$prihlasen))
desc[11,] <- c("Voting Activity (%)",descriptive(psp$aktivne))
desc[12,] <- c("Proposed Bills per Year",descriptive(psp$zakony_rok))
desc[13,] <- c("Interpellations per Year",descriptive(psp$interpelace_rok))
desc[14,] <- c("Speeches per Year",descriptive(psp$promluvy_rok))
desc[15,] <- c("Memberships in (Sub)Committees and Commissions",descriptive(psp_cl$clenstvi_specific))
desc[16,] <- c("Presidency Posts in (Sub)Committees and Commissions",descriptive(psp_cl$predsed_specific))
write.xlsx(desc, "descriptivestat.xlsx")



#######################################
########## Voting Attendance ##########
#######################################

hist(psp$prihlasen, breaks = 100)
hist(psp$zena, breaks = 100)
plot(psp$zena,psp$prihlasen)


reg_vote_att_1 <- lm(prihlasen ~ zena
                   , data = psp)
summary(reg_vote_att_1)

reg_vote_att_2 <- lm(prihlasen ~ zena
                     + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                     + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
               , data = psp)
summary(reg_vote_att_2)
par(mfrow=c(2,2))
plot(reg_vote_att_2)


#######################################
########## Voting Activity ############
#######################################

hist(psp$aktivne, breaks = 100)


reg_vote_act_1 <- lm(aktivne ~ zena
                   , data = psp)
summary(reg_vote_act_1)

reg_vote_act_2 <- lm(aktivne ~ zena
                     + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                     + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                     , data = psp)
summary(reg_vote_act_2)
par(mfrow=c(2,2)) 
plot(reg_vote_act_2)


z_vote_act <- zelig(aktivne ~ zena
                    + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                    + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                    , data = psp, model = "ls")
z_vote_act$setrange(zena=0:1,
                    vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi=mean(psp$clenstvi),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                    ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_vote_act$sim(num=100000)
plot_vote_act_gender <- as.data.frame(z_vote_act$get_qi(qi="ev", xvalue="range"))
colnames(plot_vote_act_gender) <- c("muz","zena")
plot_vote_act_gender <- rbind(apply(plot_vote_act_gender,2,mean),apply(plot_vote_act_gender,2,quantile, probs = c(.025,.975)))



###########################################################
############## OLS Figures ##################
###########################################################

stargazer(list(reg_vote_att_1,reg_vote_att_2,reg_vote_act_1,reg_vote_act_2),
          out="model_ols.txt",intercept.bottom=TRUE, digits=3, type = "text")





#######################################
######### Proposed Bills ##############
#######################################

hist(psp$zakony_rok, breaks = 100)

nb_bill_1 <- glm.nb(zakony_rok ~ zena
                  ,data = psp)
summary(nb_bill_1)

nb_bill_2 <- glm.nb(zakony_rok ~ zena
                    + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                    + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                       ,data = psp)
summary(nb_bill_2)


#######################################
######### Interpellations #############
#######################################

hist(psp$interpelace_rok, breaks = 100)

nb_int_1 <- glm.nb(interpelace_rok ~ zena
                  ,data = psp)
summary(nb_int_1)

nb_int_2 <- glm.nb(interpelace_rok ~ zena
                   + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                   + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                  ,data = psp)
summary(nb_int_2)






#######################################
######### Speeches ####################
#######################################

hist(psp$promluvy_rok, breaks = 100)

nb_sp_1 <- glm.nb(promluvy_rok ~ zena
                 ,data = psp)
summary(nb_sp_1)

nb_sp_2 <- glm.nb(promluvy_rok ~ zena
                  + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                 ,data = psp)
summary(nb_sp_2)



z_nb_sp <- zelig(promluvy_rok ~ zena
                 + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                 + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                   ,model ="negbin",data = psp)
z_nb_sp$setrange(zena=0:1,
                    vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi=mean(psp$clenstvi),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                    ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_nb_sp$sim(num=100000)
plot_speech <- as.data.frame(z_nb_sp$get_qi(qi="ev", xvalue="range"))
colnames(plot_speech) <- c("muz","zena")
plot_speech <- rbind(apply(plot_speech,2,mean),apply(plot_speech,2,quantile, probs = c(.025,.975)))


###########################################################
############## Negative Binomial Figures ##################
###########################################################

stargazer(list(nb_bill_1,nb_bill_2,nb_int_1,nb_int_2,nb_sp_1,nb_sp_2),
          out="model_nb.txt",intercept.bottom=TRUE, digits=3, type = "text")





#######################################
######### Memberships #################
#######################################

hist(psp$clenstvi_specific, breaks = 100)

p_cl_1 <- glm(clenstvi_specific ~ zena
                 ,family="poisson",data = psp_cl)
summary(p_cl_1)

p_cl_2 <- glm(clenstvi_specific ~ zena
              + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                 + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                 ,family="poisson",data = psp_cl)
summary(p_cl_2)




z_p_cl <- zelig(clenstvi_specific ~ zena
                 + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                 + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                   ,model ="poisson",data = psp_cl)
z_p_cl$setrange(zena=0:1,
                 vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                 ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_p_cl$sim(num=100000)
plot_cl_gender <- as.data.frame(z_p_cl$get_qi(qi="ev", xvalue="range"))
colnames(plot_cl_gender) <- c("muz","zena")
plot_cl_gender <- rbind(apply(plot_cl_gender,2,mean),apply(plot_cl_gender,2,quantile, probs = c(.025,.975)))


#######################################
######### Presidencies ################
#######################################
 
hist(psp$predsed_specific, breaks = 100)

p_pres_1 <- glm(predsed_specific ~ zena
               ,family="poisson",data = psp_cl)
summary(p_pres_1)

p_pres_2 <- glm(predsed_specific ~ zena
                + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
               ,family="poisson",data = psp_cl)
summary(p_pres_2)


z_p_pres <- zelig(predsed_specific ~ zena
                + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                ,model ="poisson",data = psp_cl)
z_p_pres$setrange(zena=0:1,
                vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_p_pres$sim(num=100000)
plot_pres_gender <- as.data.frame(z_p_pres$get_qi(qi="ev", xvalue="range"))
colnames(plot_pres_gender) <- c("muz","zena")
plot_pres_gender <- rbind(apply(plot_pres_gender,2,mean),apply(plot_pres_gender,2,quantile, probs = c(.025,.975)))





###########################################################
############## Poisson Figures ############################
###########################################################

stargazer(list(p_cl_1,p_cl_2,p_pres_1,p_pres_2),
          out="model_poisson.txt",intercept.bottom=TRUE, digits=3, type = "text")





###########################################################
############### Particular Committee Memberships ##########
###########################################################

# socialni
socialni <- zelig(v_spec_socialni ~ zena
                    + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                    + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                    ,model ="logit",data = psp_cl)
socialni$setrange(zena=0:1,
                    vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                    ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
socialni$sim(num=10000)
plot_socialni <- as.data.frame(socialni$get_qi(qi="ev", xvalue="range"))
colnames(plot_socialni) <- c("muz","zena")
cert_socialni <- quantile(plot_socialni$muz - plot_socialni$zena, probs = c(0.005,.025,0.05,0.95,.975,0.995))
plot_socialni <- rbind(apply(plot_socialni,2,mean),apply(plot_socialni,2,quantile, probs = c(.025,.975)))
plot_socialni <- c(plot_socialni[,1],plot_socialni[,2])

# organizacni
organizacni <- zelig(v_spec_organizacni ~ zena
                  + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                  ,model ="logit",data = psp_cl)
organizacni$setrange(zena=0:1,
                  vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
organizacni$sim(num=10000)
plot_organizacni <- as.data.frame(organizacni$get_qi(qi="ev", xvalue="range"))
colnames(plot_organizacni) <- c("muz","zena")
cert_organizacni <- quantile(plot_organizacni$muz - plot_organizacni$zena, probs = c(0.005,.025,0.05,0.95,.975,0.995))
plot_organizacni <- rbind(apply(plot_organizacni,2,mean),apply(plot_organizacni,2,quantile, probs = c(.025,.975)))
plot_organizacni <- c(plot_organizacni[,1],plot_organizacni[,2])

# peticni
peticni <- zelig(v_spec_peticni ~ zena
                  + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                  ,model ="logit",data = psp_cl)
peticni$setrange(zena=0:1,
                  vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
peticni$sim(num=10000)
plot_peticni <- as.data.frame(peticni$get_qi(qi="ev", xvalue="range"))
colnames(plot_peticni) <- c("muz","zena")
cert_peticni <- quantile(plot_peticni$muz - plot_peticni$zena, probs = c(0.005,.025,0.05,0.95,.975,0.995))
plot_peticni <- rbind(apply(plot_peticni,2,mean),apply(plot_peticni,2,quantile, probs = c(.025,.975)))
plot_peticni <- c(plot_peticni[,1],plot_peticni[,2])

# vedu_vzdelani
vedu_vzdelani <- zelig(v_spec_vedu_vzdelani ~ zena
                  + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                  ,model ="logit",data = psp_cl)
vedu_vzdelani$setrange(zena=0:1,
                  vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
vedu_vzdelani$sim(num=10000)
plot_vedu_vzdelani <- as.data.frame(vedu_vzdelani$get_qi(qi="ev", xvalue="range"))
colnames(plot_vedu_vzdelani) <- c("muz","zena")
cert_vedu_vzdelani <- quantile(plot_vedu_vzdelani$muz - plot_vedu_vzdelani$zena, probs = c(0.005,.025,0.05,0.95,.975,0.995))
plot_vedu_vzdelani <- rbind(apply(plot_vedu_vzdelani,2,mean),apply(plot_vedu_vzdelani,2,quantile, probs = c(.025,.975)))
plot_vedu_vzdelani <- c(plot_vedu_vzdelani[,1],plot_vedu_vzdelani[,2])

# zivotni_prostredi
zivotni_prostredi <- zelig(v_spec_zivotni_prostredi ~ zena
                  + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                  ,model ="logit",data = psp_cl)
zivotni_prostredi$setrange(zena=0:1,
                  vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
zivotni_prostredi$sim(num=10000)
plot_zivotni_prostredi <- as.data.frame(zivotni_prostredi$get_qi(qi="ev", xvalue="range"))
colnames(plot_zivotni_prostredi) <- c("muz","zena")
cert_zivotni_prostredi <- quantile(plot_zivotni_prostredi$muz - plot_zivotni_prostredi$zena, probs = c(0.005,.025,0.05,0.95,.975,0.995))
plot_zivotni_prostredi <- rbind(apply(plot_zivotni_prostredi,2,mean),apply(plot_zivotni_prostredi,2,quantile, probs = c(.025,.975)))
plot_zivotni_prostredi <- c(plot_zivotni_prostredi[,1],plot_zivotni_prostredi[,2])

# zdravotnicky
zdravotnicky <- zelig(v_spec_zdravotnicky ~ zena
                  + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                  ,model ="logit",data = psp_cl)
zdravotnicky$setrange(zena=0:1,
                  vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
zdravotnicky$sim(num=10000)
plot_zdravotnicky <- as.data.frame(zdravotnicky$get_qi(qi="ev", xvalue="range"))
colnames(plot_zdravotnicky) <- c("muz","zena")
cert_zdravotnicky <- quantile(plot_zdravotnicky$muz - plot_zdravotnicky$zena, probs = c(0.005,.025,0.05,0.95,.975,0.995))
plot_zdravotnicky <- rbind(apply(plot_zdravotnicky,2,mean),apply(plot_zdravotnicky,2,quantile, probs = c(.025,.975)))
plot_zdravotnicky <- c(plot_zdravotnicky[,1],plot_zdravotnicky[,2])

# mandatovy
mandatovy <- zelig(v_spec_mandatovy ~ zena
                  + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                  ,model ="logit",data = psp_cl)
mandatovy$setrange(zena=0:1,
                  vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
mandatovy$sim(num=10000)
plot_mandatovy <- as.data.frame(mandatovy$get_qi(qi="ev", xvalue="range"))
colnames(plot_mandatovy) <- c("muz","zena")
cert_mandatovy <- quantile(plot_mandatovy$muz - plot_mandatovy$zena, probs = c(0.005,.025,0.05,0.95,.975,0.995))
plot_mandatovy <- rbind(apply(plot_mandatovy,2,mean),apply(plot_mandatovy,2,quantile, probs = c(.025,.975)))
plot_mandatovy <- c(plot_mandatovy[,1],plot_mandatovy[,2])

# bezpecnostni
bezpecnostni <- zelig(v_spec_bezpecnostni ~ zena
                  + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                  ,model ="logit",data = psp_cl)
bezpecnostni$setrange(zena=0:1,
                  vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
bezpecnostni$sim(num=10000)
plot_bezpecnostni <- as.data.frame(bezpecnostni$get_qi(qi="ev", xvalue="range"))
colnames(plot_bezpecnostni) <- c("muz","zena")
cert_bezpecnostni <- quantile(plot_bezpecnostni$muz - plot_bezpecnostni$zena, probs = c(0.005,.025,0.05,0.95,.975,0.995))
plot_bezpecnostni <- rbind(apply(plot_bezpecnostni,2,mean),apply(plot_bezpecnostni,2,quantile, probs = c(.025,.975)))
plot_bezpecnostni <- c(plot_bezpecnostni[,1],plot_bezpecnostni[,2])

# ustavne_pravni
ustavne_pravni <- zelig(v_spec_ustavne_pravni ~ zena
                  + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                  ,model ="logit",data = psp_cl)
ustavne_pravni$setrange(zena=0:1,
                  vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
ustavne_pravni$sim(num=10000)
plot_ustavne_pravni <- as.data.frame(ustavne_pravni$get_qi(qi="ev", xvalue="range"))
colnames(plot_ustavne_pravni) <- c("muz","zena")
cert_ustavne_pravni <- quantile(plot_ustavne_pravni$muz - plot_ustavne_pravni$zena, probs = c(0.005,.025,0.05,0.95,.975,0.995))
plot_ustavne_pravni <- rbind(apply(plot_ustavne_pravni,2,mean),apply(plot_ustavne_pravni,2,quantile, probs = c(.025,.975)))
plot_ustavne_pravni <- c(plot_ustavne_pravni[,1],plot_ustavne_pravni[,2])

# hospodarsky
hospodarsky <- zelig(v_spec_hospodarsky ~ zena
                  + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                  ,model ="logit",data = psp_cl)
hospodarsky$setrange(zena=0:1,
                  vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
hospodarsky$sim(num=10000)
plot_hospodarsky <- as.data.frame(hospodarsky$get_qi(qi="ev", xvalue="range"))
colnames(plot_hospodarsky) <- c("muz","zena")
cert_hospodarsky <- quantile(plot_hospodarsky$muz - plot_hospodarsky$zena, probs = c(0.005,.025,0.05,0.95,.975,0.995))
plot_hospodarsky <- rbind(apply(plot_hospodarsky,2,mean),apply(plot_hospodarsky,2,quantile, probs = c(.025,.975)))
plot_hospodarsky <- c(plot_hospodarsky[,1],plot_hospodarsky[,2])

# zahranicni
zahranicni <- zelig(v_spec_zahranicni ~ zena
                  + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                  ,model ="logit",data = psp_cl)
zahranicni$setrange(zena=0:1,
                  vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
zahranicni$sim(num=10000)
plot_zahranicni <- as.data.frame(zahranicni$get_qi(qi="ev", xvalue="range"))
colnames(plot_zahranicni) <- c("muz","zena")
cert_zahranicni <- quantile(plot_zahranicni$muz - plot_zahranicni$zena, probs = c(0.005,.025,0.05,0.95,.975,0.995))
plot_zahranicni <- rbind(apply(plot_zahranicni,2,mean),apply(plot_zahranicni,2,quantile, probs = c(.025,.975)))
plot_zahranicni <- c(plot_zahranicni[,1],plot_zahranicni[,2])

# zemedelsky
zemedelsky <- zelig(v_spec_zemedelsky ~ zena
                  + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                  ,model ="logit",data = psp_cl)
zemedelsky$setrange(zena=0:1,
                  vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
zemedelsky$sim(num=10000)
plot_zemedelsky <- as.data.frame(zemedelsky$get_qi(qi="ev", xvalue="range"))
colnames(plot_zemedelsky) <- c("muz","zena")
cert_zemedelsky <- quantile(plot_zemedelsky$muz - plot_zemedelsky$zena, probs = c(0.005,.025,0.05,0.95,.975,0.995))
plot_zemedelsky <- rbind(apply(plot_zemedelsky,2,mean),apply(plot_zemedelsky,2,quantile, probs = c(.025,.975)))
plot_zemedelsky <- c(plot_zemedelsky[,1],plot_zemedelsky[,2])

# rozpoctovy
rozpoctovy <- zelig(v_spec_rozpoctovy ~ zena
                  + vek + zkusenost + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi_specific + psp1 + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + hsdsms + koalice + kducsl + kscm + lb + lsu + oda + ods + odskds + sprrsc + sz + top09 + us + usvit + vv
                  ,model ="logit",data = psp_cl)
rozpoctovy$setrange(zena=0:1,
                  vek=mean(psp$vek),zkusenost=1,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi_specific=mean(psp_cl$clenstvi_specific),psp1=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,hsdsms=0,koalice=0,kducsl=0,kscm=0,lb=0,lsu=0,oda=0,ods=0,odskds=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
rozpoctovy$sim(num=10000)
plot_rozpoctovy <- as.data.frame(rozpoctovy$get_qi(qi="ev", xvalue="range"))
colnames(plot_rozpoctovy) <- c("muz","zena")
cert_rozpoctovy <- quantile(plot_rozpoctovy$muz - plot_rozpoctovy$zena, probs = c(0.005,.025,0.05,0.95,.975,0.995))
plot_rozpoctovy <- rbind(apply(plot_rozpoctovy,2,mean),apply(plot_rozpoctovy,2,quantile, probs = c(.025,.975)))
plot_rozpoctovy <- c(plot_rozpoctovy[,1],plot_rozpoctovy[,2])



###################################
############ Plots ################
###################################

# Voting Activity

pdf("figures/voting_activity.pdf", width=10, height=3)
par(mfrow=c(1,1),family="Helvetica",oma = c(1,1,0,0) + 0.1,mar = c(4,0,2,1) + 0.1) 
plot(plot_vote_act_gender[1,],c(0:1), col="white", type = "l", lwd = 2, xlim = c(80,85), yaxt='n', ylab = "", xlab = "Voting Activity (%)", las=1, main = "", font.main = 1)
grid(NULL, NA)
lines(plot_vote_act_gender[2:3,1],c(0.3,0.3), lt = "solid", lwd = 2)
lines(plot_vote_act_gender[2:3,2],c(0.7,0.7), lt = "solid", lwd = 2)
for(i in 2:3){
  lines(rep(plot_vote_act_gender[i,1],2),c(0.26,0.34), lwd = 2)
}
lines(rep(plot_vote_act_gender[1,1],2),c(0.28,0.32), lwd = 2)
for(i in 2:3){
  lines(rep(plot_vote_act_gender[i,2],2),c(0.66,0.74), lwd = 2)
}
lines(rep(plot_vote_act_gender[1,2],2),c(0.68,0.72), lwd = 2)
text(plot_vote_act_gender[1,],c(0.35,0.75),c("Male","Female"), pos = 3)
dev.off()


# Speeches

pdf("figures/speeches.pdf", width=10, height=3)
par(mfrow=c(1,1),family="Helvetica",oma = c(1,1,0,0) + 0.1,mar = c(4,0,2,1) + 0.1) 
plot(plot_speech[1,],c(0:1), col="white", type = "l", lwd = 2, xlim = c(10,30), yaxt='n', ylab = "", xlab = "Speeches per Year (N)", las=1, main = "", font.main = 1)
grid(NULL, NA)
lines(plot_speech[2:3,1],c(0.3,0.3), lt = "solid", lwd = 2)
lines(plot_speech[2:3,2],c(0.7,0.7), lt = "solid", lwd = 2)
for(i in 2:3){
  lines(rep(plot_speech[i,1],2),c(0.26,0.34), lwd = 2)
}
lines(rep(plot_speech[1,1],2),c(0.28,0.32), lwd = 2)
for(i in 2:3){
  lines(rep(plot_speech[i,2],2),c(0.66,0.74), lwd = 2)
}
lines(rep(plot_speech[1,2],2),c(0.68,0.72), lwd = 2)
text(plot_speech[1,],c(0.35,0.75),c("Male","Female"), pos = 3)
dev.off()


# Memberships and Presidency

pdf("figures/memberships.pdf", width=10, height=3)
par(mfrow=c(1,2),family="Helvetica",oma = c(1,1,0,0) + 0.1,mar = c(4,0,2,1) + 0.1) 
plot(plot_cl_gender[1,],c(0:1), col="white", type = "l", lwd = 2, xlim = c(3,6), yaxt='n', ylab = "", xlab = "Post-Election Memberships (N)", las=1, main = "a)", font.main = 1)
grid(NULL, NA)
lines(plot_cl_gender[2:3,1],c(0.3,0.3), lt = "solid", lwd = 2)
lines(plot_cl_gender[2:3,2],c(0.7,0.7), lt = "solid", lwd = 2)
for(i in 2:3){
  lines(rep(plot_cl_gender[i,1],2),c(0.26,0.34), lwd = 2)
}
lines(rep(plot_cl_gender[1,1],2),c(0.28,0.32), lwd = 2)
for(i in 2:3){
  lines(rep(plot_cl_gender[i,2],2),c(0.66,0.74), lwd = 2)
}
lines(rep(plot_cl_gender[1,2],2),c(0.68,0.72), lwd = 2)
text(plot_cl_gender[1,],c(0.35,0.75),c("Male","Female"), pos = 3)

plot(plot_pres_gender[1,],c(0:1), col="white", type = "l", lwd = 2, xlim = c(0,1.5), yaxt='n', ylab = "", xlab = "Post-Election Presidium Posts (N)", las=1, main = "b)", font.main = 1)
grid(NULL, NA)
lines(plot_pres_gender[2:3,1],c(0.3,0.3), lt = "solid", lwd = 2)
lines(plot_pres_gender[2:3,2],c(0.7,0.7), lt = "solid", lwd = 2)
for(i in 2:3){
  lines(rep(plot_pres_gender[i,1],2),c(0.26,0.34), lwd = 2)
}
lines(rep(plot_pres_gender[1,1],2),c(0.28,0.32), lwd = 2)
for(i in 2:3){
  lines(rep(plot_pres_gender[i,2],2),c(0.66,0.74), lwd = 2)
}
lines(rep(plot_pres_gender[1,2],2),c(0.68,0.72), lwd = 2)
text(plot_pres_gender[1,],c(0.35,0.75),c("Male","Female"), pos = 3)
dev.off()





### particular committees

pdf("figures/committee_probs.pdf", width=10, height=4)
par(mfrow=c(1,1),family="Helvetica",oma = c(0,1,0,0) + 0.1,mar = c(4,16.5,0,0) + 0.1)
plot(c(0:1),c(1,13), col="white", type = "l", lwd = 2, xlim = c(0,0.6), ylim = c(0.5,13.5), yaxt='n', ylab = "", xlab = "Predicted Probabilities of Being a Committee Member (P)", las=1, main = "", font.main = 1)
grid(26, NA)
spec_com_fig <- function(committee, vertical, cert){
  points(c(committee[1]),c(vertical-0.1), pch = "+")
  points(c(committee[4]),c(vertical+0.1), pch = "+", col = "grey60")
  lines(c(committee[2:3]),rep(vertical-0.1,2), lwd = 2)
  lines(c(committee[5:6]),rep(vertical+0.1,2), lwd = 2, col = "grey60")
  text(0.009,vertical-0.25,paste0(rep("*",sum(as.numeric(c(cert[1]*cert[6]>0,cert[2]*cert[5]>0,cert[3]*cert[4]>0)))), collapse = ""), pos = 2)
}
spec_com_fig(plot_zemedelsky,1,cert_zemedelsky)
spec_com_fig(plot_rozpoctovy,2,cert_rozpoctovy)
spec_com_fig(plot_ustavne_pravni,3,cert_ustavne_pravni)
spec_com_fig(plot_hospodarsky,4,cert_hospodarsky)
spec_com_fig(plot_zivotni_prostredi,5,cert_zivotni_prostredi)
spec_com_fig(plot_zahranicni,6,cert_zahranicni)
spec_com_fig(plot_zdravotnicky,7,cert_zdravotnicky)
spec_com_fig(plot_mandatovy,8,cert_mandatovy)
spec_com_fig(plot_peticni,9,cert_peticni)
spec_com_fig(plot_vedu_vzdelani,10,cert_vedu_vzdelani)
spec_com_fig(plot_bezpecnostni,11,cert_bezpecnostni)
spec_com_fig(plot_socialni,12,cert_socialni)
spec_com_fig(plot_organizacni,13,cert_organizacni)
axis(2, at=1:13, labels=c("Agriculture","Budget","Constitutional and Legal Affairs","Economic Affairs","Environment","Foreign Affairs","Health Care","Mandate and Immunity","Petitions","Science, Education, Culture, Youth and Sport","Security","Social Policy","Steering"),las=1)
legend("bottomright", legend=c("Female", "Male"),
       col=c("grey60", "black"), lwd = 2.5, bg = "white")
dev.off()












