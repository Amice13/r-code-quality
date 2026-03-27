rm(list = ls())
load("age.RData")

save(list = ls(all = TRUE), file= "age.RData")

install.packages("Zelig")

library(xlsx)
library(Zelig)
library(MASS)
library(stargazer)
library(openxlsx)
library(car)





####################################
########## LOAD DATA ###############
####################################

psp <- read.csv("mps_czech_republic.csv",fileEncoding = "Windows-1250")
psp <- psp[psp$id_obdobi<=171,]

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

psp$clenstvi <- psp$vybory + psp$komise + psp$podvybory

psp$clenstvi_specific <- psp$vybory_specific + psp$komise_specific + psp$podvybory_specific

psp$predsed <- psp$vybory_predsednictvo + psp$komise_predsednictvo + psp$podvybory_predsednictvo

psp$predsed_specific <- psp$vybory_predsed_specific + psp$komise_predsed_specific + psp$podvybory_predsed_specific

psp_orig <- psp

# exclude the very first term
psp <- psp[psp$obdobi!="PSP1",]

psp <- psp[order(psp$id_osoba, -psp$id_poslanec), ]
psp <- psp[ !duplicated(psp$id_osoba), ]  

psp <- psp[psp$id_poslanec!=202,] # Ivana Janů excluded because she had left the chamber before the electronic system of recording was installed

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

psp_min <- psp[psp$vlada==0,]

psp_cl_min <- psp_cl[psp_cl$vlada==0,]


####################################
##### Descriptive Statistics #######
####################################

cor(psp$vek,psp$zkusenost)

pdf("figures/descriptive.pdf", width = 10, height = 4)
par(mfrow=c(1,2),family="Times",ps=14,oma = c(1,1,0,0) + 0.1,mar = c(4,4,2,1) + 0.1) 
hist(psp$vek, xlab = "Age", main = "a)", las = 1, ylim = c(0,140), breaks = 20, font.main = 1)
grid(nx = NA)
hist(psp$zkusenost, xlab = "Tenure", main = "b)", breaks = 7, las = 1, xlim = c(1,8), ylim = c(0,1400), font.main = 1)
grid(nx = NA)
dev.off()

agetenure <- as.data.frame(matrix(0,1,3))
colnames(agetenure) <- c("Age/Tenure","Frequency","Share (percentage)")
agetenure[1,] <- c("21-30",table(psp$vek<31)[2],round(table(psp$vek<31)[2]/length(psp$vek)*100,2))
agetenure[2,] <- c("31-40",table(psp$vek>=31 & psp$vek<41)[2],round(table(psp$vek>=31 & psp$vek<41)[2]/length(psp$vek)*100,2))
agetenure[3,] <- c("41-50",table(psp$vek>=41 & psp$vek<51)[2],round(table(psp$vek>=41 & psp$vek<51)[2]/length(psp$vek)*100,2))
agetenure[4,] <- c("51-60",table(psp$vek>=51 & psp$vek<61)[2],round(table(psp$vek>=51 & psp$vek<61)[2]/length(psp$vek)*100,2))
agetenure[5,] <- c("61-70",table(psp$vek>=61 & psp$vek<71)[2],round(table(psp$vek>=61 & psp$vek<71)[2]/length(psp$vek)*100,2))
agetenure[6,] <- c("71-80",table(psp$vek>=71 & psp$vek<81)[2],round(table(psp$vek>=71 & psp$vek<81)[2]/length(psp$vek)*100,2))
agetenure[7,] <- c("Total",table(psp$vek>=21 & psp$vek<81)[1],round(table(psp$vek>=21 & psp$vek<81)[1]/length(psp$vek)*100,2))
agetenure[8:14,] <- cbind(as.data.frame(t(table(psp$zkusenost))),round(table(psp$zkusenost)/length(psp$zkusenost)*100,2))[,c(2,3,5)]
agetenure[15,] <- c("Total",length(psp$zkusenost),sum(as.numeric(agetenure[8:14,3])))
write.xlsx(agetenure, "agetenure.xlsx")


descriptive <- function(variable){
  round(c(length(variable),mean(variable),sd(variable),min(variable),max(variable)),2)
}

desc <- as.data.frame(matrix(0,1,6))
colnames(desc) <- c("Variable","N","Mean","Standard Deviation","Minimum","Maximum")
desc[1,] <- c("Age",descriptive(psp$vek))
desc[2,] <- c("Tenure",descriptive(psp$zkusenost))
desc[3,] <- c("Female",descriptive(psp$zena))
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

mean(psp$vek)
median(psp$vek)



#######################################
########## Voting Attendance ##########
#######################################

hist(psp$prihlasen, breaks = 100)
hist(psp$vek, breaks = 100)
plot(psp$vek,psp$prihlasen)
plot(psp$zkusenost,psp$prihlasen)

reg_vote_att_1 <- lm(prihlasen ~ vek + zkusenost
                   , data = psp)
summary(reg_vote_att_1)

reg_vote_att_2 <- lm(prihlasen ~ vek + zkusenost
               + zena + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp2 + psp3 + psp4 + psp5 + psp6
               + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
               , data = psp)
summary(reg_vote_att_2)
par(mfrow=c(2,2))
plot(reg_vote_att_2)
vif(reg_vote_att_2)


z_vote_att <- zelig(prihlasen ~ vek + zkusenost
                  + zena + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
                  , data = psp, model = "ls")
z_vote_att$setrange(vek=20:80,zkusenost=1,
                  zena=0,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi=mean(psp$clenstvi),psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_vote_att$sim(num=100000)
plot_vote_att_age <- as.data.frame(z_vote_att$get_qi(qi="ev", xvalue="range"))
colnames(plot_vote_att_age) <- c(20:80)
plot_vote_att_age <- rbind(apply(plot_vote_att_age,2,mean),apply(plot_vote_att_age,2,quantile, probs = c(.025,.975)))
z_vote_att$setrange(vek=mean(psp$vek),zkusenost=1:7,
                  zena=0,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi=mean(psp$clenstvi),psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                  ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_vote_att$sim(num=100000)
plot_vote_att_exp <- as.data.frame(z_vote_att$get_qi(qi="ev", xvalue="range"))
colnames(plot_vote_att_exp) <- c(1:7)
plot_vote_att_exp <- rbind(apply(plot_vote_att_exp,2,mean),apply(plot_vote_att_exp,2,quantile, probs = c(.025,.975)))



#######################################
########## Voting Activity ############
#######################################

hist(psp$aktivne, breaks = 100)
hist(psp$vek, breaks = 100)
plot(psp$vek,psp$aktivne)
plot(psp$zkusenost,psp$vek)

reg_vote_act_1 <- lm(aktivne ~ vek + zkusenost
                   , data = psp)
summary(reg_vote_act_1)

reg_vote_act_2 <- lm(aktivne ~ vek + zkusenost
                   + zena + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp2 + psp3 + psp4 + psp5 + psp6
                   + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
                   , data = psp)
summary(reg_vote_act_2)
par(mfrow=c(2,2)) 
plot(reg_vote_act_2)
vif(reg_vote_act_2)


z_vote_act <- zelig(aktivne ~ vek + zkusenost
                    + zena + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp2 + psp3 + psp4 + psp5 + psp6
                    + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
                    , data = psp, model = "ls")
z_vote_act$setrange(vek=20:80,zkusenost=1,
                    zena=0,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi=mean(psp$clenstvi),psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                    ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_vote_act$sim(num=100000)
plot_vote_act_age <- as.data.frame(z_vote_act$get_qi(qi="ev", xvalue="range"))
colnames(plot_vote_act_age) <- c(20:80)
plot_vote_act_age <- rbind(apply(plot_vote_act_age,2,mean),apply(plot_vote_act_age,2,quantile, probs = c(.025,.975)))
z_vote_act$setrange(vek=mean(psp$vek),zkusenost=1:7,
                    zena=0,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi=mean(psp$clenstvi),psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                    ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_vote_act$sim(num=100000)
plot_vote_act_exp <- as.data.frame(z_vote_act$get_qi(qi="ev", xvalue="range"))
colnames(plot_vote_act_exp) <- c(1:7)
plot_vote_act_exp <- rbind(apply(plot_vote_act_exp,2,mean),apply(plot_vote_act_exp,2,quantile, probs = c(.025,.975)))
z_vote_act <- zelig(aktivne ~ vek*zkusenost
                    + zena + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp2 + psp3 + psp4 + psp5 + psp6
                    + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
                    , data = psp, model = "ls")
z_vote_act$setrange(vek=21,zkusenost=1:7,
                    zena=0,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi=mean(psp$clenstvi),psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                    ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_vote_act$sim(num=100000)
plot_vote_act_exp_21 <- as.data.frame(z_vote_act$get_qi(qi="ev", xvalue="range"))
colnames(plot_vote_act_exp_21) <- c(1:7)
plot_vote_act_exp_21 <- rbind(apply(plot_vote_act_exp_21,2,mean),apply(plot_vote_act_exp_21,2,quantile, probs = c(.025,.975)))
z_vote_act$setrange(vek=40,zkusenost=1:7,
                    zena=0,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi=mean(psp$clenstvi),psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                    ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_vote_act$sim(num=100000)
plot_vote_act_exp_40 <- as.data.frame(z_vote_act$get_qi(qi="ev", xvalue="range"))
colnames(plot_vote_act_exp_40) <- c(1:7)
plot_vote_act_exp_40 <- rbind(apply(plot_vote_act_exp_40,2,mean),apply(plot_vote_act_exp_40,2,quantile, probs = c(.025,.975)))
z_vote_act$setrange(vek=60,zkusenost=1:7,
                    zena=0,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi=mean(psp$clenstvi),psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                    ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_vote_act$sim(num=100000)
plot_vote_act_exp_60 <- as.data.frame(z_vote_act$get_qi(qi="ev", xvalue="range"))
colnames(plot_vote_act_exp_60) <- c(1:7)
plot_vote_act_exp_60 <- rbind(apply(plot_vote_act_exp_60,2,mean),apply(plot_vote_act_exp_60,2,quantile, probs = c(.025,.975)))



################################################
############## OLS Figures #####################
################################################

stargazer(list(reg_vote_att_1,reg_vote_att_2,reg_vote_act_1,reg_vote_act_2),
          out="model_ols.txt",intercept.bottom=TRUE, digits=3, type = "text")





#######################################
######### Proposed Bills ##############
#######################################

hist(psp$zakony_rok, breaks = 100)

nb_bill_1 <- glm.nb(zakony_rok ~ vek + zkusenost
                  ,data = psp)
summary(nb_bill_1)

nb_bill_2 <- glm.nb(zakony_rok ~ vek + zkusenost
                       + zena + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp2 + psp3 + psp4 + psp5 + psp6
                       + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
                       ,data = psp)
summary(nb_bill_2)
vif(nb_bill_2)


z_nb_bill <- zelig(zakony_rok ~ vek + zkusenost
                 + zena + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp2 + psp3 + psp4 + psp5 + psp6
                 + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
                 ,model ="negbin",data = psp)
z_nb_bill$setrange(vek=20:80,zkusenost=1,
                 zena=0,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi=mean(psp$clenstvi),psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                 ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_nb_bill$sim(num=100000)
plot_bill_age <- as.data.frame(z_nb_bill$get_qi(qi="ev", xvalue="range"))
colnames(plot_bill_age) <- c(20:80)
plot_bill_age <- rbind(apply(plot_bill_age,2,mean),apply(plot_bill_age,2,quantile, probs = c(.025,.975)))




#######################################
######### Interpellations #############
#######################################

hist(psp$interpelace_rok, breaks = 100)

nb_int_1 <- glm.nb(interpelace_rok ~ vek + zkusenost
                  ,data = psp_min)
summary(nb_int_1)

nb_int_2 <- glm.nb(interpelace_rok ~ vek + zkusenost
                  + zena + vs + geo + kluby_predsednictvo + predsednictvo + clenstvi + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
                  ,data = psp_min)
summary(nb_int_2)
vif(nb_int_2)



table(psp_min$interpelace)


#######################################
######### Speeches ####################
#######################################

hist(psp$promluvy_rok, breaks = 100)

nb_sp_1 <- glm.nb(promluvy_rok ~ vek + zkusenost
                 ,data = psp)
summary(nb_sp_1)

nb_sp_2 <- glm.nb(promluvy_rok ~ vek + zkusenost
                 + zena + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp2 + psp3 + psp4 + psp5 + psp6
                 + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
                 ,data = psp)
summary(nb_sp_2)
vif(nb_sp_2)



z_nb_sp <- zelig(promluvy_rok ~ vek + zkusenost
                   + zena + vs + geo + kluby_predsednictvo + vlada + predsednictvo + clenstvi + psp2 + psp3 + psp4 + psp5 + psp6
                   + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
                   ,model ="negbin",data = psp)
z_nb_sp$setrange(vek=mean(psp$vek),zkusenost=1:7,
                   zena=0,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi=mean(psp$clenstvi),psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                   ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_nb_sp$sim(num=100000)
plot_sp_exp <- as.data.frame(z_nb_sp$get_qi(qi="ev", xvalue="range"))
colnames(plot_sp_exp) <- c(1:7)
plot_sp_exp <- rbind(apply(plot_sp_exp,2,mean),apply(plot_sp_exp,2,quantile, probs = c(.025,.975)))
z_nb_sp$setrange(vek=20:80,zkusenost=1,
                 zena=0,vs=1,geo=1,kluby_predsednictvo=0,vlada=0,predsednictvo=0,clenstvi=mean(psp$clenstvi),psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                 ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_nb_sp$sim(num=100000)
plot_sp_age <- as.data.frame(z_nb_sp$get_qi(qi="ev", xvalue="range"))
colnames(plot_sp_age) <- c(20:80)
plot_sp_age <- rbind(apply(plot_sp_age,2,mean),apply(plot_sp_age,2,quantile, probs = c(.025,.975)))







###########################################################
############## Negative Binomial Figures ##################
###########################################################

stargazer(list(nb_bill_1,nb_bill_2,nb_int_1,nb_int_2,nb_sp_1,nb_sp_2),
          out="model_nb.txt",intercept.bottom=TRUE, digits=3, type = "text")





#######################################
######### Memberships #################
#######################################

hist(psp$clenstvi_specific, breaks = 100)

poi_cl_1 <- glm(clenstvi_specific ~ vek + zkusenost
                 ,family="poisson",data = psp_cl_min)
summary(poi_cl_1)


poi_cl_2 <- glm(clenstvi_specific ~ vek + zkusenost
                + zena + vs + geo + kluby_predsednictvo + predsednictvo + psp2 + psp3 + psp4 + psp5 + psp6
                + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
                ,family="poisson",data = psp_cl_min)
summary(poi_cl_2)
vif(poi_cl_2)


z_poi_mem <- zelig(clenstvi_specific ~ vek + zkusenost
                   + zena + vs + geo + kluby_predsednictvo + predsednictvo + psp2 + psp3 + psp4 + psp5 + psp6
                   + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
                   ,model ="poisson",data = psp_cl_min)
z_poi_mem$setrange(vek=20:80,zkusenost=1,
                   zena=0,vs=1,geo=1,kluby_predsednictvo=0,predsednictvo=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                   ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_poi_mem$sim(num=100000)
plot_mem_age <- as.data.frame(z_poi_mem$get_qi(qi="ev", xvalue="range"))
colnames(plot_mem_age) <- c(20:80)
plot_mem_age <- rbind(apply(plot_mem_age,2,mean),apply(plot_mem_age,2,quantile, probs = c(.025,.975)))
z_poi_mem$setrange(vek=mean(psp_cl_min$vek),zkusenost=1:7,
                    zena=0,vs=1,geo=1,kluby_predsednictvo=0,predsednictvo=0,psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                   ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_poi_mem$sim(num=100000)
plot_mem_exp <- as.data.frame(z_poi_mem$get_qi(qi="ev", xvalue="range"))
colnames(plot_mem_exp) <- c(1:7)
plot_mem_exp <- rbind(apply(plot_mem_exp,2,mean),apply(plot_mem_exp,2,quantile, probs = c(.025,.975)))


#######################################
######### Presidencies ################
#######################################

hist(psp$predsed_specific, breaks = 100)

poi_pres_1 <- glm(predsed_specific ~ vek + zkusenost
               ,family="poisson",data = psp_cl_min)
summary(poi_pres_1)

poi_pres_2 <- glm(predsed_specific ~ vek + zkusenost
                  + zena + vs + geo + kluby_predsednictvo + predsednictvo + clenstvi_specific + psp2 + psp3 + psp4 + psp5 + psp6
                  + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
               ,family="poisson",data = psp_cl_min)
summary(poi_pres_2)
vif(poi_pres_2)

nb_pres_2 <- glm.nb(predsed_specific ~ vek + zkusenost
                 + zena + vs + geo + kluby_predsednictvo + predsednictvo + clenstvi_specific + psp2 + psp3 + psp4 + psp5 + psp6
                 + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
                 ,data = psp_cl_min)

(-2*logLik(poi_pres_2) + 2*logLik(nb_pres_2)) > qchisq(0.95,df=1) # TRUE value supports negative binomial model (otherwise Poisson)


z_poi_pres <- zelig(predsed_specific ~ vek + zkusenost
                    + zena + vs + geo + kluby_predsednictvo + predsednictvo + clenstvi_specific + psp2 + psp3 + psp4 + psp5 + psp6
                    + ano + koalice + kducsl + kscm + oda + ods + sprrsc + sz + top09 + us + usvit + vv
                    ,model ="poisson",data = psp_cl_min)
z_poi_pres$setrange(vek=20:80,zkusenost=1,
                    zena=0,vs=1,geo=1,kluby_predsednictvo=0,predsednictvo=0,clenstvi_specific=mean(psp_cl_min$clenstvi_specific),psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                    ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_poi_pres$sim(num=100000)
plot_pres_age <- as.data.frame(z_poi_pres$get_qi(qi="ev", xvalue="range"))
colnames(plot_pres_age) <- c(1:7)
plot_pres_age <- rbind(apply(plot_pres_age,2,mean),apply(plot_pres_age,2,quantile, probs = c(.025,.975)))
z_poi_pres$setrange(vek=mean(psp_cl_min$vek),zkusenost=1:7,
                 zena=0,vs=1,geo=1,kluby_predsednictvo=0,predsednictvo=0,clenstvi_specific=mean(psp_cl_min$clenstvi_specific),psp2=0,psp3=0,psp4=0,psp5=0,psp6=0,
                 ano=0,koalice=0,kducsl=0,kscm=0,oda=0,ods=0,sprrsc=0,sz=0,top09=0,us=0,usvit=0,vv=0)
z_poi_pres$sim(num=100000)
plot_pres_exp <- as.data.frame(z_poi_pres$get_qi(qi="ev", xvalue="range"))
colnames(plot_pres_exp) <- c(1:7)
plot_pres_exp <- rbind(apply(plot_pres_exp,2,mean),apply(plot_pres_exp,2,quantile, probs = c(.025,.975)))





###########################################################
############## Poisson Figures ############################
###########################################################

stargazer(list(poi_cl_1,poi_cl_2,poi_pres_1,poi_pres_2),
          out="model_poisson.txt",intercept.bottom=TRUE, digits=3, type = "text")







###################################
############ Plots ################
###################################

# Voting Attendance

pdf("figures/attendance.pdf", width = 10, height = 4)
par(mfrow=c(1,2),family="Helvetica",ps=14,oma = c(1,1,0,0) + 0.1,mar = c(4,4,2,0) + 0.1) 
plot(c(20:80),plot_vote_att_age[1,], col="white", type = "l", lwd = 2, ylim = c(60,85), xlab = "Age", ylab = "Voting Attendance (%)", las=1, main = "a)", font.main = 1)
grid()
lines(c(21:80),plot_vote_att_age[1,c(-1)], lt = "solid", lwd = 1)
polygon(x = c(c(21:80),rev(c(21:80))), y = c(plot_vote_att_age[2,c(-1)],rev(plot_vote_att_age[3,c(-1)])),
        col = adjustcolor("grey30", alpha = 0.1), border = FALSE)
lines(c(21:80),plot_vote_att_age[2,c(-1)], lt = "dashed", lwd = 1)
lines(c(21:80),plot_vote_att_age[3,c(-1)], lt = "dashed", lwd = 1)
plot(1:7,plot_vote_att_exp[1,], ylim = c(60,85), pch = "-", las=1, xlab = "Tenure", ylab = "", main = "b)", font.main = 1, cex = 1)
grid()
for(i in 1:7){
  lines(c(i,i),plot_vote_att_exp[2:3,i], lt = "solid")
}
dev.off()



# Voting Activity

pdf("figures/activity.pdf", width = 10, height = 4)
par(mfrow=c(1,2),family="Helvetica",ps=14,oma = c(1,1,0,0) + 0.1,mar = c(4,4,2,0) + 0.1) 
plot(c(20:80),plot_vote_act_age[1,], col="white", type = "l", lwd = 2, ylim = c(72,86), xlab = "Age", ylab = "Voting Activity (%)", las=1, main = "a)", font.main = 1)
grid()
lines(c(21:80),plot_vote_act_age[1,c(-1)], lt = "solid", lwd = 1)
polygon(x = c(c(21:80),rev(c(21:80))), y = c(plot_vote_act_age[2,c(-1)],rev(plot_vote_act_age[3,c(-1)])),
        col = adjustcolor("grey30", alpha = 0.1), border = FALSE)
lines(c(21:80),plot_vote_act_age[2,c(-1)], lt = "dashed", lwd = 1)
lines(c(21:80),plot_vote_act_age[3,c(-1)], lt = "dashed", lwd = 1)
plot(1:7,plot_vote_act_exp[1,], ylim = c(72,86), pch = "-", las=1, xlab = "Tenure", ylab = "", main = "b)", font.main = 1, cex = 1)
grid()
for(i in 1:7){
  lines(c(i,i),plot_vote_act_exp[2:3,i], lt = "solid")
}
dev.off()

# Combined Voting

pdf("figures/combined.pdf", width = 10, height = 8)
par(mfrow=c(2,2),family="Helvetica",ps=14,oma = c(1,1,0,0) + 0.1,mar = c(4,4,2,0) + 0.1) 
plot(c(20:80),plot_vote_att_age[1,], col="white", type = "l", lwd = 2, ylim = c(55,90), xlab = "Age", ylab = "Voting Attendance (%)", las=1, main = "a)", font.main = 1)
grid()
lines(c(21:80),plot_vote_att_age[1,c(-1)], lt = "solid", lwd = 1)
polygon(x = c(c(21:80),rev(c(21:80))), y = c(plot_vote_att_age[2,c(-1)],rev(plot_vote_att_age[3,c(-1)])),
        col = adjustcolor("grey30", alpha = 0.1), border = FALSE)
lines(c(21:80),plot_vote_att_age[2,c(-1)], lt = "dashed", lwd = 1)
lines(c(21:80),plot_vote_att_age[3,c(-1)], lt = "dashed", lwd = 1)
plot(1:7,plot_vote_att_exp[1,], ylim = c(55,90), pch = "-", las=1, xlab = "Tenure", ylab = "", main = "b)", font.main = 1, cex = 1)
grid()
for(i in 1:7){
  lines(c(i,i),plot_vote_att_exp[2:3,i], lt = "solid")
}
plot(c(20:80),plot_vote_act_age[1,], col="white", type = "l", lwd = 2, ylim = c(72,86), xlab = "Age", ylab = "Voting Activity (%)", las=1, main = "c)", font.main = 1)
grid()
lines(c(21:80),plot_vote_act_age[1,c(-1)], lt = "solid", lwd = 1)
polygon(x = c(c(21:80),rev(c(21:80))), y = c(plot_vote_act_age[2,c(-1)],rev(plot_vote_act_age[3,c(-1)])),
        col = adjustcolor("grey30", alpha = 0.1), border = FALSE)
lines(c(21:80),plot_vote_act_age[2,c(-1)], lt = "dashed", lwd = 1)
lines(c(21:80),plot_vote_act_age[3,c(-1)], lt = "dashed", lwd = 1)
plot(1:7,plot_vote_act_exp[1,], ylim = c(72,86), pch = "-", las=1, xlab = "Tenure", ylab = "", main = "d)", font.main = 1, cex = 1)
grid()
for(i in 1:7){
  lines(c(i,i),plot_vote_act_exp[2:3,i], lt = "solid")
}
dev.off()


# Bills

pdf("figures/bills.pdf", width = 5, height = 4)
par(mfrow=c(1,1)) 
plot(c(1:7),plot_bill_exp[1,], col="black", pch = 3, ylim = c(4,8), xlab = "Tenure", ylab = "Proposed Bills Per Year", las=1)
grid()
for(i in 1:7){
  lines(c(i,i),plot_bill_exp[2:3,i], lt = "solid")
}
dev.off()


# Speeches

pdf("figures/speeches,bills.pdf", width = 10, height = 4)
par(mfrow=c(1,2),family="Helvetica",ps=14,oma = c(1,0,0,0) + 0.1,mar = c(4,5,2,0) + 0.1) 
plot(1:7,plot_sp_exp[1,], ylim = c(0,150), pch = "-", xlab = "Tenure", ylab = "Speeches Per Year", las=1, main = "a)", font.main = 1, cex = 1)
grid()
for(i in 1:7){
  lines(c(i,i),plot_sp_exp[2:3,i], lt = "solid")
}
plot(c(20:80),plot_bill_age[1,], col="white", type = "l", lwd = 2, ylim=c(0,7), xlab = "Age", ylab = "Bill Proposals Per Year", las=1, main = "b)", font.main = 1)
grid()
lines(c(21:80),plot_bill_age[1,c(-1)], lt = "solid", lwd = 1)
polygon(x = c(c(21:80),rev(c(21:80))), y = c(plot_bill_age[2,c(-1)],rev(plot_bill_age[3,c(-1)])),
        col = adjustcolor("grey30", alpha = 0.1), border = FALSE)
lines(c(21:80),plot_bill_age[2,c(-1)], lt = "dashed", lwd = 1)
lines(c(21:80),plot_bill_age[3,c(-1)], lt = "dashed", lwd = 1)
dev.off()


# Posts

pdf("figures/posts.pdf", width = 10, height = 4)
par(mfrow=c(1,2),family="Helvetica",ps=14,oma = c(1,1,0,0) + 0.1,mar = c(4,4,2,1) + 0.1) 
plot(1:7,plot_mem_exp[1,], ylim = c(0,6), pch = "-", xlab = "Tenure", ylab = "Memberships", las=1, main = "a)", font.main = 1, cex = 1)
grid()
for(i in 1:7){
  lines(c(i,i),plot_mem_exp[2:3,i], lt = "solid")
}
plot(1:7,plot_pres_exp[1,], ylim = c(0,6), pch = "-", xlab = "Tenure", ylab = "Presidency Posts", las=1, main = "b)", font.main = 1, cex = 1)
grid()
for(i in 1:7){
  lines(c(i,i),plot_pres_exp[2:3,i], lt = "solid")
}
dev.off()


pdf("figures/posts.pdf", width = 10, height = 4)
par(mfrow=c(1,2),family="Helvetica",ps=14,oma = c(1,1,0,0) + 0.1,mar = c(4,4,2,1) + 0.1) 
plot(c(20:80),plot_pres_age[1,], col="white", type = "l", lwd = 2, ylim=c(0,3), xlab = "Age", ylab = "Post-Election Presidium Posts", las=1, main = "a)", font.main = 1)
grid()
lines(c(21:80),plot_pres_age[1,c(-1)], lt = "solid", lwd = 1)
polygon(x = c(c(21:80),rev(c(21:80))), y = c(plot_pres_age[2,c(-1)],rev(plot_pres_age[3,c(-1)])),
        col = adjustcolor("grey30", alpha = 0.1), border = FALSE)
lines(c(21:80),plot_pres_age[2,c(-1)], lt = "dashed", lwd = 1)
lines(c(21:80),plot_pres_age[3,c(-1)], lt = "dashed", lwd = 1)
plot(1:7,plot_pres_exp[1,], ylim = c(0,3), pch = "-", xlab = "Tenure", ylab = "", las=1, main = "b)", font.main = 1, cex = 1)
grid()
for(i in 1:7){
  lines(c(i,i),plot_pres_exp[2:3,i], lt = "solid")
}
dev.off()













