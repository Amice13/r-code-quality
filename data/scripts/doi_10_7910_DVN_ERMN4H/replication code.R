### Replication code for "The effect of party identification and candidate opinions on trust in elections"
### Salvador Vázquez del Mercado
### saved and uploaded on April 7, 2020

# 0- packages and locale -------------------------------------------
library(pacman)
#install.packages("survey")
p_load(haven, rio, srvyr, survey, foreign, MASS, zeligverse, sjmisc, sjlabelled, VGAM, pscl, tidyverse, broom, lmtest, interplot)

#importantísimo para manejar nombres en espaniol
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

##stp

# 1- load bases -----------------------------------------------------------
base06_1 <- read_sav("~/Google Drive/01 Proyectos/CNEP/Analisis/CNEP06.sav")
base12_1 <- read_sav("~/Google Drive/01 Proyectos/CNEP/Analisis/CNEPMX2012.sav")
base18_1 <- read_sav("~/Google Drive/01 Proyectos/CNEP/Analisis/CNEP_MEXICO2018.sav")


# 2- select and recode bases ----------------------------------------------
b06 <- base06_1 %>% 
  dplyr::select( 
    #identificación
    Z.Me.C.Identify,
    #opinión de candidatos
    C.ThermCalderon, C.ThermMadrazo, C.ThermObrador,
    #satisfacción con la democracia
    Z.Me.B.DemSatPost, Z.Me.K.Confidence,
    #opinión de protesta
    Z.Me.H.ObradorAccuse, Z.Me.H.PRDProtests,
    #sociodemo
    L.Gender, L.Age, L.Education
  ) %>% 
  mutate(
    idp= rec(Z.Me.C.Identify, rec= "2=1 [PRI]; 4=3 [PAN]; 6=5 [PRD]; 7:9= 10 [Ning]; else=copy"),    
    opiFCH= rec(C.ThermCalderon, rec= "11= NA; else=copy", val.labels= get_labels(fill_labels(C.ThermCalderon))),
    opiRMP= rec(C.ThermMadrazo, rec= "11= NA; else=copy", val.labels= get_labels(fill_labels(C.ThermMadrazo))),
    opiAMLO= rec(C.ThermObrador, rec= "11= NA; else=copy", val.labels= get_labels(fill_labels(C.ThermObrador))),
    satDem= rec(Z.Me.B.DemSatPost, rec= "5= NA; else=copy", val.labels= get_labels(Z.Me.B.DemSatPost)),
    satDem= rec(satDem, rec= "rev", val.labels= rev(get_labels(satDem ))),
    eleCnf= rec(Z.Me.K.Confidence, rec= "1=2 [Confident]; 2=1 [Doubt]; 3= NA"),
    sexo= L.Gender,
    edad= L.Age,
    edu= rec(L.Education, rec= "10= NA; else=copy", val.labels= get_labels(L.Education)),
    idp2= rec(Z.Me.C.Identify, rec= "7:9= 1; 1=2; 2=2; 3=4; 4=4; 5=6; 6=6", val.labels= c("Ning", "PRI", "PAN", "PRD"), as.num=F),
    eleCnf2= rec(Z.Me.K.Confidence, rec= "1=1 [Confident]; 2=0 [Doubt]; 3= NA", as.num= F),    jusProt= rec(Z.Me.H.PRDProtests, rec= "1=2 [Justifi]; 2=1 [NoJustif]; 3= NA", as.num= F),
    jusAcus= rec(Z.Me.H.ObradorAccuse, rec= "1=2 [Justifi]; 2=1 [NoJustif]; 3= NA", as.num= F),
  )


b12 <- base12_1 %>% 
  dplyr::select(
    #identificación
    p71,
    #opi candidatos
    p97a, p97b, p97c,
    #satisfacción con la democracia, confianza en las elecciones
    p79, p109,
    #sociodemo
    pb, pc, p148, p3
  ) %>% 
  mutate(
    idp= rec(p71, rec= "2=1 [PRI]; 4=3 [PAN]; 6=5 [PRD]; 7:9= 10 [Ning]; else=copy"),    
    opiJVM= rec(p97a, rec= "11= NA; else=copy", val.labels= get_labels(fill_labels(p97a))), #las etiquetas de p97a-c están raras, dejo la escala como queda
    opiEPN= rec(p97b, rec= "11= NA; else=copy", val.labels= get_labels(fill_labels(p97b))),
    opiAMLO= rec(p97c, rec= "11= NA; else=copy", val.labels= get_labels(fill_labels(p97c))),
    satDem= rec(p79, rec= "5= NA; else=copy", val.labels= get_labels(p79)),
    satDem= rec(satDem, rec= "rev", val.labels= rev(get_labels(satDem ))),
    eleLib= rec(p81a, rec= "5= NA; else=copy", val.labels= get_labels(p81a)),
    eleLib= rec(eleLib, rec= "rev", val.labels= rev(get_labels(eleLib ))),  
    sexo= pb,
    edad= pc,
    edu= rec(p148, rec= "10= NA; else=copy", val.labels= get_labels(p148)),
    eleCnf= rec(p109, rec= "1=2 [Confiar]; 2=1 [Dudas]; 3= NA"),
    polInt= rec(p3, rec= "4=1 [Nada]; 3=2 [Poco]; 2=3 [Algo]; 1=4 [Mucho]; 5=NA"),
    idp2= rec(p71, rec= "7:9= 1; 1=2; 2=2; 3=4; 4=4; 5=6; 6=6", val.labels= c("Ning", "PRI", "PAN", "PRD"), as.num=F),
    eleCnf2= rec(p109, rec= "1=1 [Confident]; 2=0 [Doubt]; 3= NA", as.num=F)
  )



b18 <- base18_1 %>% 
  dplyr::select(
    #idp
    p94,
    #sociodemográficas
    pB, pC, p144, 
    #políticas
    p2, p3, p4, p106, #p111e no está en la base
    #opiniones 
    p9a:p9d,
    #democracia
    p100,
    #elecciones
    p107,p2,
    #ponderador
    WEIGHTTOT
  ) %>% ##manipulación de variables
  mutate(
    idp= rec(p94, rec= "2=1 [PRI]; 4=3 [PAN]; 6=5 [PRD]; 8=7 [MOR]; 10=10 [Ning]; 11=NA ; else=copy"),
    sexo= to_label(pB),
    edad= pC,
    edu= rec(p144, rec= "10= NA; else=copy", val.labels= get_labels(p144)),
    intPol= rec(p2, rec= "5= NA; else= copy", val.labels= get_labels(p2)),
    intPol= rec(intPol, rec= "rev", val.labels= rev(get_labels(intPol))),
    opiRAC= rec(p9a, rec= "11= NA; else=copy", val.labels= get_labels(fill_labels(p9a))),
    opiJAM= rec(p9b, rec= "11= NA; else=copy", val.labels= get_labels(fill_labels(p9b))),
    opiAMLO= rec(p9c, rec= "11= NA; else=copy", val.labels= get_labels(fill_labels(p9c))),
    opiJRC= rec(p9d, rec= "11= NA; else=copy", val.labels= get_labels(fill_labels(p9d))),
    satDem= rec(p100, rec= "5:6= NA; else=copy", val.labels= get_labels(p100)),
    satDem= rec(satDem, rec= "rev", val.labels= rev(get_labels(satDem ))),
    eleCon= rec(p107, rec= "2=1 [dudar]; 1=2 [confiar]; 3=NA"),
    polInt= rec(p2, rec= "4=1 [Nada]; 3=2 [Poco]; 2=3 [Algo]; 1=4 [Mucho]; 5=NA"),
    idp2= rec(p94, rec= "10:11= 1; 1=2; 2=2; 3=4; 4=4; 5=6; 6=6; 7=8; 8=8", val.labels= c("Ning", "PRI", "PAN", "PRD", "MOR"), as.num=F),
    eleCnf2= rec(p107, rec= "1=1 [Confident]; 2=0 [Doubt]; 3= NA", as.num=F)
    
    
  ) %>% dplyr::select(-starts_with("p"))


b18d <- svydesign(
  ids= ~0,
  data= b18,
  weights= ~WEIGHTTOT
)


# 3- center variables -----------------------------------------------------
#creación de variables centradas, 2006:
b06$opiAMLOc <- b06$opiAMLO - mean(b06$opiAMLO, na.rm= T)
b06$opiFCHc <-  b06$opiFCH - mean(b06$opiFCH, na.rm= T)
b06$opiRMPc <-  b06$opiRMP - mean(b06$opiRMP, na.rm= T)

b06$satDemc <- b06$satDem - median(b06$satDem, na.rm= T)
b06$educ <- b06$edu - median(b06$edu, na.rm= T)

#creación de variables centradas, 2012:
b12$opiAMLOc <- b12$opiAMLO - mean(b12$opiAMLO, na.rm= T)
b12$opiJVMc <-  b12$opiJVM - mean(b12$opiJVM, na.rm= T)
b12$opiEPNc <-  b12$opiEPN - mean(b12$opiEPN, na.rm= T)

b12$satDemc <- b12$satDem - median(b12$satDem, na.rm= T)
b12$educ <- b12$edu - median(b12$edu, na.rm= T)

#creación de variables centradas, 2018:
b18$opiAMLOc <- b18$opiAMLO - mean(b18$opiAMLO, na.rm= T)
b18$opiRACc <-  b18$opiRAC - mean(b18$opiRAC, na.rm= T)
b18$opiJAMc <-  b18$opiJAM - mean(b18$opiJAM, na.rm= T)

b18$satDemc <- b18$satDem - median(b18$satDem, na.rm= T)
b18$educ <- b18$edu - median(b18$edu, na.rm= T)

#para svydesign en 18
update(b18d, 
       b18$opiAMLOc <- b18$opiAMLO - mean(b18$opiAMLO, na.rm= T),
       b18$opiRACc <-  b18$opiRAC - mean(b18$opiRAC, na.rm= T),
       b18$opiJAMc <-  b18$opiJAM - mean(b18$opiJAM, na.rm= T),
       b18$satDemc <- b18$satDem - median(b18$satDem, na.rm= T),
       b18$educ <- b18$edu - median(b18$edu, na.rm= T)
       )

# 4- models ---------------------------------------------------------------


#2006

mod06 <- zelig(eleCnf2 ~ idp2*(opiRMPc + opiFCHc + opiAMLOc) + educ + satDemc, model= "logit", data= b06, cite=F) %>% from_zelig_model()

mod06d <- glm(formula = eleCnf2 ~ idp2 * (opiRMPc + opiFCHc + opiAMLOc) + educ + satDemc, family = binomial("logit"), data = b06)

lrt06 <- lrtest(mod06d, glm(eleCnf2~1, family= binomial("logit"), data=b06[ as.numeric(-mod06d$na.action),]))

mod06.di <- data.frame(n= length(mod06d$y), pr2= pR2(mod06d)[4], llh= pR2(mod06d)[1], chi2=lrt06$Chisq[2], pval=lrt06$`Pr(>Chisq)`[2])

write.csv(tidy(mod06), "final/mod06.df.csv")

#2012

mod12 <- zelig(eleCnf2 ~ idp2*(opiJVMc + opiEPNc + opiAMLOc) + educ + satDemc, model= "logit", data= b12, cite=F) %>% from_zelig_model()

mod12d <- glm(formula = eleCnf2 ~ idp2 * (opiJVMc + opiEPNc + opiAMLOc) + educ + satDemc, family = binomial("logit"), data = b12)

lrt12 <- lrtest(mod12d, glm(eleCnf2~1, family= binomial("logit"), data=b12[ as.numeric(-mod12d$na.action),]))

mod12.di <- data.frame(n= length(mod12d$y), pr2= pR2(mod12d)[4], llh= pR2(mod12d)[1], chi2=lrt12$Chisq[2], pval=lrt12$`Pr(>Chisq)`[2])

write.csv(tidy(mod12), "final/mod12.df.csv")

#2018

mod18 <- zelig(eleCnf2 ~ idp2*(opiRACc + opiJAMc + opiAMLOc) + educ + satDemc, model= "logit", data= b18, cite=F) %>% from_zelig_model()

mod18d <- glm(formula = eleCnf2 ~ idp2 * (opiRACc + opiJAMc + opiAMLOc) + educ + satDemc, family = binomial("logit"), data = b18)

lrt18 <- lrtest(mod18d, glm(eleCnf2~1, family= binomial("logit"), data=b18[ as.numeric(-mod18d$na.action),]))

mod18.di <- data.frame(n= length(mod18d$y), pr2= pR2(mod18d)[4], llh= pR2(mod18d)[1], chi2=lrt18$Chisq[2], pval=lrt18$`Pr(>Chisq)`[2])


write.csv(tidy(mod18), "final/mod18.df.csv")

write.csv(rbind(d06= mod06.di, d12= mod12.di, d18= mod18.di), "final/modsdi.csv")


# 5.1- first differences with zelig 2006------------------------------------------

## 2006
##creación de tabla de primeras diferencias para *idp*
parts <- c(2, 4, 6)
tparts <- c("PRI", "PAN", "PRD")
fd.idp06 <- data.frame(NA, NA, NA, NA)

pruz <- zelig(eleCnf2 ~ idp2*(opiRMPc + opiFCHc + opiAMLOc) + educ + satDemc, model= "logit", data= b06, cite=F)
set.seed(12345)
for(i in 1:length(parts)){
  simp <- pruz %>% 
    setx( idp2= 1) %>% 
    setx1(idp2= parts[i]) %>% 
    Zelig::sim()
  #extracción de vector de primeras diferencias y sus valores: 
  fds <- unlist(simp$sim.out$x1$fd)
  fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
  fd
  colnames(fd.idp06) <- c("party", names(fd) )
  fd.idp06[i, ] <- c(tparts[i], fd)
}
fd.idp06 <- data.frame(year= 2006, fd.idp06)
fd.idp06

##creación de tabla de primeras diferencias para rango intercuartil de *cands*

cands <- c("RMP", "FCH", "AMLO")
fd.can06 <- data.frame(NA, NA, NA, NA)

pruz <- zelig(eleCnf2 ~ idp2*(opiRMPc + opiFCHc + opiAMLOc) + educ + satDemc, model= "logit", data= b06, cite=F) 

#RMP
pers <- quantile(b06$opiRMPc, probs= c(.25, .75), na.rm=T)
set.seed(12345)
simp <- pruz %>% 
  setx( idp2= 1, opiRMPc= pers[1]) %>% 
  setx1(idp2= 1, opiRMPc= pers[2]) %>% 
  Zelig::sim()
#extracción de vector de primeras diferencias y sus valores: 
fds <- unlist(simp$sim.out$x1$fd)
fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
fd
colnames(fd.can06) <- c("candidate", names(fd) )
fd.can06[1, ] <- c(cands[1], fd)

#FCH
pers <- quantile(b06$opiFCHc, probs= c(.25, .75), na.rm=T)
set.seed(12345)

simp <- pruz %>% 
  setx( idp2= 1, opiFCHc= pers[1]) %>% 
  setx1(idp2= 1, opiFCHc= pers[2]) %>% 
  Zelig::sim()
#extracción de vector de primeras diferencias y sus valores: 
fds <- unlist(simp$sim.out$x1$fd)
fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
fd
fd.can06[2, ] <- c(cands[2], fd)

#AMLO
pers <- quantile(b06$opiAMLOc, probs= c(.25, .75), na.rm=T)
set.seed(12345)

simp <- pruz %>% 
  setx( idp2= 1, opiAMLOc= pers[1]) %>% 
  setx1(idp2= 1, opiAMLOc= pers[2]) %>% 
  Zelig::sim()
#extracción de vector de primeras diferencias y sus valores: 
fds <- unlist(simp$sim.out$x1$fd)
fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
fd
fd.can06[3, ] <- c(cands[3], fd)

fd.can06 <- data.frame(year= 2006, fd.can06)
fd.can06

## primeras diferencias para rango intercuartil de cands, para cada identidad partidista
parts <- c(1, 2, 4, 6)
tparts <- c("ning", "PRI", "PAN", "PRD")
cands <- c("RMP", "FCH", "AMLO")
fd.int06 <- data.frame(NA, NA, NA, NA, NA)

pruz <- zelig(eleCnf2 ~ idp2*(opiRMPc + opiFCHc + opiAMLOc) + educ + satDemc, model= "logit", data= b06, cite=F) 
nums <- length(parts)-1

for(i in 1:length(parts)){
  #RMP
  pers <- quantile(b06$opiRMPc, probs= c(.25, .75), na.rm=T)
  set.seed(12345)
  
  simp <- pruz %>% 
    setx( idp2= parts[i], opiRMPc= pers[1]) %>% 
    setx1(idp2= parts[i], opiRMPc= pers[2]) %>% 
    Zelig::sim()
  #extracción de vector de primeras diferencias y sus valores: 
  fds <- unlist(simp$sim.out$x1$fd)
  fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
  fd
  colnames(fd.int06) <- c("candidate", "party", names(fd) )
  fd.int06[1 + (i-1)*nums, ] <- c(cands[1], tparts[i], fd)
  
  #FCH
  pers <- quantile(b06$opiFCHc, probs= c(.25, .75), na.rm=T)
  set.seed(12345)
  
  simp <- pruz %>% 
    setx( idp2= parts[i], opiFCHc= pers[1]) %>% 
    setx1(idp2= parts[i], opiFCHc= pers[2]) %>% 
    Zelig::sim()
  #extracción de vector de primeras diferencias y sus valores: 
  fds <- unlist(simp$sim.out$x1$fd)
  fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
  fd
  fd.int06[2+ (i-1)*nums, ] <- c(cands[2], tparts[i], fd)
  
  #AMLO
  pers <- quantile(b06$opiAMLOc, probs= c(.25, .75), na.rm=T)
  set.seed(12345)
  
  simp <- pruz %>% 
    setx( idp2= parts[i], opiAMLOc= pers[1]) %>% 
    setx1(idp2= parts[i], opiAMLOc= pers[2]) %>% 
    Zelig::sim()
  #extracción de vector de primeras diferencias y sus valores: 
  fds <- unlist(simp$sim.out$x1$fd)
  fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
  fd
  fd.int06[3+ (i-1)*nums, ] <- c(cands[3], tparts[i], fd)
}

fd.int06 <- data.frame(year= 2006, fd.int06)
fd.int06

# 5.2- first differences with zelig 2012------------------------------------------

## 2012
##creación de tabla de primeras diferencias para *idp*
parts <- c(2, 4, 6)
tparts <- c("PRI", "PAN", "PRD")
fd.idp12 <- data.frame(NA, NA, NA, NA)

pruz <- zelig(eleCnf2 ~ idp2*(opiJVMc + opiEPNc + opiAMLOc) + educ + satDemc, model= "logit", data= b12, cite=F)

for(i in 1:length(parts)){
  set.seed(12345)
  
  simp <- pruz %>% 
    setx( idp2= 1) %>% 
    setx1(idp2= parts[i]) %>% 
    Zelig::sim()
  #extracción de vector de primeras diferencias y sus valores: 
  fds <- unlist(simp$sim.out$x1$fd)
  fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
  fd
  colnames(fd.idp12) <- c("party", names(fd) )
  fd.idp12[i, ] <- c(tparts[i], fd)
}
fd.idp12 <- data.frame(year= 2012, fd.idp12)
fd.idp12

##creación de tabla de primeras diferencias para rango intercuartil de *cands*

cands <- c("EPN", "JVM", "AMLO")
fd.can12 <- data.frame(NA, NA, NA, NA)

pruz <- zelig(eleCnf2 ~ idp2*(opiJVMc + opiEPNc + opiAMLOc) + educ + satDemc, model= "logit", data= b12, cite=F)

#EPN
pers <- quantile(b12$opiEPNc, probs= c(.25, .75), na.rm=T)
set.seed(12345)

simp <- pruz %>% 
  setx( idp2= 1, opiEPNc= pers[1]) %>% 
  setx1(idp2= 1, opiEPNc= pers[2]) %>% 
  Zelig::sim()
#extracción de vector de primeras diferencias y sus valores: 
fds <- unlist(simp$sim.out$x1$fd)
fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
fd
colnames(fd.can12) <- c("candidate", names(fd) )
fd.can12[1, ] <- c(cands[1], fd)

#JVM
pers <- quantile(b12$opiJVMc, probs= c(.25, .75), na.rm=T)
set.seed(12345)

simp <- pruz %>% 
  setx( idp2= 1, opiJVMc= pers[1]) %>% 
  setx1(idp2= 1, opiJVMc= pers[2]) %>% 
  Zelig::sim()
#extracción de vector de primeras diferencias y sus valores: 
fds <- unlist(simp$sim.out$x1$fd)
fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
fd
fd.can12[2, ] <- c(cands[2], fd)

#AMLO
pers <- quantile(b12$opiAMLOc, probs= c(.25, .75), na.rm=T)
set.seed(12345)

simp <- pruz %>% 
  setx( idp2= 1, opiAMLOc= pers[1]) %>% 
  setx1(idp2= 1, opiAMLOc= pers[2]) %>% 
  Zelig::sim()
#extracción de vector de primeras diferencias y sus valores: 
fds <- unlist(simp$sim.out$x1$fd)
fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
fd
fd.can12[3, ] <- c(cands[3], fd)

fd.can12 <- data.frame(year= 2012, fd.can12)
fd.can12

## primeras diferencias para rango intercuartil de cands, para cada identidad partidista
parts <- c(1, 2, 4, 6)
tparts <- c("ning", "PRI", "PAN", "PRD")
cands <- c("EPN", "JVM", "AMLO")
fd.int12 <- data.frame(NA, NA, NA, NA, NA)

pruz <- zelig(eleCnf2 ~ idp2*(opiJVMc + opiEPNc + opiAMLOc) + educ + satDemc, model= "logit", data= b12, cite=F)
nums <- length(parts)-1

for(i in 1:length(parts)){
  #EPN
  pers <- quantile(b12$opiEPNc, probs= c(.25, .75), na.rm=T)
  simp <- pruz %>% 
    setx( idp2= parts[i], opiEPNc= pers[1]) %>% 
    setx1(idp2= parts[i], opiEPNc= pers[2]) %>% 
    Zelig::sim()
  #extracción de vector de primeras diferencias y sus valores: 
  fds <- unlist(simp$sim.out$x1$fd)
  fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
  fd
  colnames(fd.int12) <- c("candidate", "party", names(fd) )
  fd.int12[1 + (i-1)*nums, ] <- c(cands[1], tparts[i], fd)
  
  #JVM
  pers <- quantile(b12$opiJVMc, probs= c(.25, .75), na.rm=T)
  simp <- pruz %>% 
    setx( idp2= parts[i], opiJVMc= pers[1]) %>% 
    setx1(idp2= parts[i], opiJVMc= pers[2]) %>% 
    Zelig::sim()
  #extracción de vector de primeras diferencias y sus valores: 
  fds <- unlist(simp$sim.out$x1$fd)
  fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
  fd
  fd.int12[2+ (i-1)*nums, ] <- c(cands[2], tparts[i], fd)
  
  #AMLO
  pers <- quantile(b12$opiAMLOc, probs= c(.25, .75), na.rm=T)
  simp <- pruz %>% 
    setx( idp2= parts[i], opiAMLOc= pers[1]) %>% 
    setx1(idp2= parts[i], opiAMLOc= pers[2]) %>% 
    Zelig::sim()
  #extracción de vector de primeras diferencias y sus valores: 
  fds <- unlist(simp$sim.out$x1$fd)
  fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
  fd
  fd.int12[3+ (i-1)*nums, ] <- c(cands[3], tparts[i], fd)
}

fd.int12 <- data.frame(year= 2012, fd.int12)
fd.int12

# 5.3- first differences with zelig 2018------------------------------------------

## 2018
##creación de tabla de primeras diferencias para *idp*
parts <- c(2, 4, 6, 8)
tparts <- c("PRI", "PAN", "PRD", "MOR")
fd.idp18 <- data.frame(NA, NA, NA, NA)

pruz <- zelig(eleCnf2 ~ idp2*(opiRACc + opiJAMc + opiAMLOc) + educ + satDemc, model= "logit", data= b18, cite=F)
i <- 1
for(i in 1:length(parts)){
  set.seed(12345)
  
  simp <- pruz %>% 
    setx( idp2= 1) %>% 
    setx1(idp2= parts[i]) %>% 
    Zelig::sim()
  #extracción de vector de primeras diferencias y sus valores: 
  fds <- unlist(simp$sim.out$x1$fd)
  fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
  #fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
  #rownames(fd) <- NULL
  colnames(fd.idp18) <- c("party", names(fd) )
  fd.idp18[i, ] <- c(tparts[i], fd)
}
fd.idp18 <- data.frame(year= 2018, fd.idp18)
fd.idp18

##creación de tabla de primeras diferencias para rango intercuartil de *cands*

cands <- c("JAM", "RAC", "AMLO")
fd.can18 <- data.frame(NA, NA, NA, NA)

pruz <- zelig(eleCnf2 ~ idp2*(opiRACc + opiJAMc + opiAMLOc) + educ + satDemc, model= "logit", data= b18, cite=F)

#JAM
pers <- quantile(b18$opiJAMc, probs= c(.25, .75), na.rm=T)
set.seed(12345)

simp <- pruz %>% 
  setx( idp2= 1, opiJAMc= pers[1]) %>% 
  setx1(idp2= 1, opiJAMc= pers[2]) %>% 
  Zelig::sim()
#extracción de vector de primeras diferencias y sus valores: 
fds <- unlist(simp$sim.out$x1$fd)
fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
fd
colnames(fd.can18) <- c("candidate", names(fd) )
fd.can18[1, ] <- c(cands[1], fd)

#RAC
pers <- quantile(b18$opiRACc, probs= c(.25, .75), na.rm=T)  
set.seed(12345)

simp <- pruz %>% 
  setx( idp2= 1, opiRACc= pers[1]) %>% 
  setx1(idp2= 1, opiRACc= pers[2]) %>% 
  Zelig::sim()
#extracción de vector de primeras diferencias y sus valores: 
fds <- unlist(simp$sim.out$x1$fd)
fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
fd
fd.can18[2, ] <- c(cands[2], fd)

#AMLO
pers <- quantile(b18$opiAMLOc, probs= c(.25, .75), na.rm=T)
set.seed(12345)

simp <- pruz %>% 
  setx( idp2= 1, opiAMLOc= pers[1]) %>% 
  setx1(idp2= 1, opiAMLOc= pers[2]) %>% 
  Zelig::sim()
#extracción de vector de primeras diferencias y sus valores: 
fds <- unlist(simp$sim.out$x1$fd)
fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
fd
fd.can18[3, ] <- c(cands[3], fd)

fd.can18 <- data.frame(year= 2018, fd.can18)
fd.can18

## primeras diferencias para rango intercuartil de cands, para cada identidad partidista
parts <- c(1, 2, 4, 6, 8)
tparts <- c("ning", "PRI", "PAN", "PRD", "MOR")
cands <- c("JAM", "RAC", "AMLO")
fd.int18 <- data.frame(NA, NA, NA, NA, NA)

pruz <- zelig(eleCnf2 ~ idp2*(opiRACc + opiJAMc + opiAMLOc) + educ + satDemc, model= "logit", data= b18, cite=F)

nums <- length(parts)-2

for(i in 1:length(parts)){
  #JAM
  pers <- quantile(b18$opiJAMc, probs= c(.25, .75), na.rm=T)
  set.seed(12345)
  
  simp <- pruz %>% 
    setx( idp2= parts[i], opiJAMc= pers[1]) %>% 
    setx1(idp2= parts[i], opiJAMc= pers[2]) %>% 
    Zelig::sim()
  #extracción de vector de primeras diferencias y sus valores: 
  fds <- unlist(simp$sim.out$x1$fd)
  fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
  fd
  colnames(fd.int18) <- c("candidate", "party", names(fd) )
  fd.int18[1 + (i-1)*nums, ] <- c(cands[1], tparts[i], fd)
  
  #RAC
  pers <- quantile(b18$opiRACc, probs= c(.25, .75), na.rm=T)
  set.seed(12345)
  
  simp <- pruz %>% 
    setx( idp2= parts[i], opiRACc= pers[1]) %>% 
    setx1(idp2= parts[i], opiRACc= pers[2]) %>% 
    Zelig::sim()
  #extracción de vector de primeras diferencias y sus valores: 
  fds <- unlist(simp$sim.out$x1$fd)
  fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
  fd
  fd.int18[2+ (i-1)*nums, ] <- c(cands[2], tparts[i], fd)
  
  #AMLO
  pers <- quantile(b18$opiAMLOc, probs= c(.25, .75), na.rm=T)
  set.seed(12345)
  
  simp <- pruz %>% 
    setx( idp2= parts[i], opiAMLOc= pers[1]) %>% 
    setx1(idp2= parts[i], opiAMLOc= pers[2]) %>% 
    Zelig::sim()
  #extracción de vector de primeras diferencias y sus valores: 
  fds <- unlist(simp$sim.out$x1$fd)
  fd <- data.frame(mean= mean(fds), min= quantile(fds, probs= c(.025)), max= quantile(fds, probs= c(0.975)))
  fd
  fd.int18[3+ (i-1)*nums, ] <- c(cands[3], tparts[i], fd)
}

fd.int18<- data.frame(year= 2018, fd.int18)
fd.int18


# 6- tables and plots of first differences----------------------------------------------------------
### tablas completas
fd.idp <- rbind(fd.idp06, fd.idp12, fd.idp18)
fd.can <- rbind(fd.can06, fd.can12, fd.can18)
fd.int <- rbind(fd.int06, fd.int12, fd.int18)

write.csv(fd.idp, "final/fd.idp.csv")
write.csv(fd.can, "final/fd.can.csv")
write.csv(fd.int, "final/fd.int.csv")

##Graf 1.
# graf. de ipd

#status
# 1=win, 2=lose, 3= win/accuse, 4= lose/accuse
status.pid <- c(2,1,4, 1,2,4, 2,2,4,3) #qué hacer con MOR? ganador o acusador? hay que hacer una clasif de 4... 

tcols <- c("#FF0000",  "#3366FF", "#FFCC00","#FF0000",  "#3366FF", "#FFCC00","#FF0000",  "#3366FF", "#FFCC00", "#993333")


# plot fd de idp

par(mfrow=c(2,1))

plot(fd.idp$mean, ylim= c(-.6, .8),
     pch=ifelse(status.pid==1 | status.pid==3, 24, 25),
     col= tcols, cex=1,
     bg= ifelse(status.pid==1 | status.pid==2, "gray95", "gray40"), 
     main= "A) First differences of partisanship\n(all other variables at means)",
     xlab= "Party", ylab="Change in prob.", xaxt= "n")
abline(h=0, col= "gray45", lty= "dashed")
abline(v= c(3.5, 6.5), col= "gray65")
text(c(2, 5, 8.5), .8, labels= c("2006", "2012", "2018"), cex= .8)

m <- dim(fd.idp)[1]
for(i in 1:m){
  lines(c(i, i), c(fd.idp$min[i], fd.idp$max[i]), col= tcols[i])
}
axis(side= 1, at= 1:length(fd.idp$party), labels= fd.idp$party, cex.axis= 0.7 )
legend(x= 8, y= 0.9, legend= c("win", "lose", "accept", "accuse"),
       bty= "n",
       pt.bg= c("white", "white", "gray95", "gray40"), 
       cex= .75, pch= c(24,25,22,22), y.intersp = .2)

#legend(x= c(8.0, 9.5), y= c(0.7, 0.3), legend= c("win", "lose", "accept", "accuse"),
 #        bty= "n",
  #       pt.bg= c("white", "white", "gray95", "gray40"), 
  #       cex= .65, pch= c(24,25,22,22), y.intersp = .3)


# graf. de ipd
#status
# 1=win, 2=lose, 3= win/accuse, 4= lose/accuse
status.can <- c(2,1,4, 1,2,4, 2,2,3) #qué hacer con AMLO? ganador o acusador? hay que hacer una clasif de 4...

tcols <- c("#FF0000",  "#3366FF", "#FFCC00","#FF0000",  "#3366FF", "#FFCC00","#FF0000",  "#3366FF", "#993333")


# plot fd de can
# min(fd.can$min), max(fd.can$max)*1.4
plot(fd.can$mean, ylim= c(-.6, .8),
     pch=ifelse(status.can==1 | status.can==3, 24, 25),  cex=1,
     col= tcols, bg= ifelse(status.can==1 | status.can==2, "gray95", "gray40"),
     main= "B) First differences of candidate opinion\n(all other variables at means)",
     xlab= "Candidate", ylab="Change in prob.", xaxt= "n")
abline(h=0, col= "gray45", lty= "dashed")
abline(v= c(3.5, 6.5), col= "gray65")
text(c(2, 5, 8), 0.8, labels= c("2006", "2012", "2018"), cex= 0.8)

m <- dim(fd.can)[1]
for(i in 1:m){
  lines(c(i, i), c(fd.can$min[i], fd.can$max[i]), col= tcols[i])
}
axis(side= 1, at= 1:length(fd.can$candidate), labels= fd.can$candidate , cex.axis= 0.7)
legend(x= 7.3, y= 0.9, legend= c("win", "lose", "accept", "accuse"), 
       pt.bg= c("white", "white", "gray90", "black"), 
       bty= "n", 
       cex= .75, pch= c(24,25,22,22), y.intersp = .2)



# graf de interacciones

# 7.1- simulations of interactions 2006 -------------------------------------------------------
let <- 1
lets <- c("a", "b", "c", "d", "e", "f", "g", "h", "i")
parts <- c(2, 4, 6) # FALTA MORENA
tparts <- c("PRI", "PAN", "PRD", "MOR")
tcands <- c("RMP", "FCH", "AMLO")
tcols <- c("#FF0000",  "#3366FF", "#FFCC00", "#993333")
tcols2 <- c("#FF00001A",  "#3366FF1A", "#FFCC001A", "#9933331A")
ltys <- c(1, 2, 3, 4, 6)

par(mfrow=c(3, 3), oma = c(4, 1, 1, 1), mar = c(3, 4, 2, 1))
for(k in 1:length(tcands)){
  simis <- data.frame(year= NA, party= NA, cand= NA, val= NA, inter=NA, mean= NA, min= NA, med= NA, max= NA, stringsAsFactors = F)
  psimis <- simis
  set.seed(12345)
  for(j in 1:length(parts)){
    switch(k, 
           simi <- zelig(eleCnf2 ~ idp2*(opiRMPc + opiFCHc + opiAMLOc) + educ + satDemc, model= "logit", data= b06, cite=F) %>% 
             setx( idp2= 1, opiRMPc= seq(from= -5, to= 5, by= 1)) %>%  
             setx1( idp2= parts[j], opiRMPc= seq(from= -5, to= 5, by= 1)) %>% 
             Zelig::sim()
           ,
           simi <- zelig(eleCnf2 ~ idp2*(opiRMPc + opiFCHc + opiAMLOc) + educ + satDemc, model= "logit", data= b06, cite=F) %>% 
             setx( idp2= 1, opiFCHc= seq(from= -5, to= 5, by= 1)) %>%  
             setx1( idp2= parts[j], opiFCHc= seq(from= -5, to= 5, by= 1)) %>% 
             Zelig::sim()
           ,
           simi <- zelig(eleCnf2 ~ idp2*(opiRMPc + opiFCHc + opiAMLOc) + educ + satDemc, model= "logit", data= b06, cite=F) %>% 
             setx( idp2= 1, opiAMLOc= seq(from= -5, to= 5, by= 1)) %>%  
             setx1( idp2= parts[j], opiAMLOc= seq(from= -5, to= 5, by= 1)) %>% 
             Zelig::sim()
    )
    
    for(i in 1:length(seq(from= -5, to= 5, by= 1))){
      qs <- quantile((unlist(simi$sim.out$range[[i]][[1]])), probs= c(.025, .5, .975)); names(qs) <- NULL
      ms <- mean(unlist(simi$sim.out$range[[i]][[1]]))
      vals <- simi$setx.out$range[[i]][[2]][[1]][4 + k]#k?
      ivals <- 0
      cvar <- attributes(simi$setx.out$range[[i]][[2]][[1]])$dimnames[[2]][4 + k] #k?
      
      qs1 <- quantile((unlist(simi$sim.out$range1[[i]][[1]])), probs= c(.025, .5, .975)); names(qs1) <- NULL
      ms1 <- mean(unlist(simi$sim.out$range1[[i]][[1]]))
      vals1 <- simi$setx.out$range1[[i]][[2]][[1]][4 + k] #k?
      ivals1 <- 1
      cvar1 <- attributes(simi$setx.out$range1[[i]][[2]][[1]])$dimnames[[2]][4 + k]  #k?
      
      psimis[i,] <- data.frame(year= 2006, party= tparts[j], cand= cvar, val= vals, inter=ivals, mean= ms, min= qs[1], med= qs[2], max= qs[3], stringsAsFactors = F)
      psimis[i + length(seq(from= -5, to= 5, by= 1)), ] <- data.frame(year= 2006, party= tparts[j], cand= cvar1, val= vals1, inter=ivals1, mean= ms1, min= qs1[1], med= qs1[2], max= qs1[3], stringsAsFactors = F)
    }
    simis <- rbind(psimis, simis)
  }
  simis <- simis[-dim(simis)[1],]
  #simis
  
  plot(simis$val[simis$inter==0 & simis$party=="PRD"]+5, simis$mean[simis$inter==0 & simis$party=="PRD"], type= "l", ylim= c(0,1), 
       main= paste0(lets[let],") 2006 - Opinion of ", tcands[k]), cex.main=1.1,
       xlab= paste("Opinion of", tcands[k]), 
       ylab= "Prob. of trust"); let= let+1 
  
  polygon(c(rev(secu + 5) , secu + 5), c(rev(simis$max[simis$inter==0 & simis$party=="PRD"]), simis$min[simis$inter==0 & simis$party=="PRD"]), col= rgb(0,0,0,.1), border=F)
  
  for(j in 1:length(parts)){
    lines(simis$val[simis$inter==1 & simis$party==tparts[j]]+5, simis$mean[simis$inter==1 & simis$party==tparts[j]], simis$val, type= "l", col= tcols[j], lty= ltys[j+1])
    
    polygon(c(rev(secu + 5) , secu + 5), c(rev(simis$max[simis$inter==1 & simis$party==tparts[j]]), simis$min[simis$inter==1 & simis$party==tparts[j]]), col= tcols2[j], border=F)
  }
}


# 7.2- simulations of interactions 2012 -------------------------------------------------------

parts <- c(2, 4, 6) # FALTA MORENA
tparts <- c("PRI", "PAN", "PRD", "MOR")
tcands <- c("EPN", "JVM", "AMLO")
tcols <- c("#FF0000",  "#3366FF", "#FFCC00", "#993333")
tcols2 <- c("#FF00001A",  "#3366FF1A", "#FFCC001A", "#9933331A")

for(k in 1:length(tcands)){
  simis <- data.frame(year= NA, party= NA, cand= NA, val= NA, inter=NA, mean= NA, min= NA, med= NA, max= NA, stringsAsFactors = F)
  psimis <- simis
  set.seed(12345)
  for(j in 1:length(parts)){
    switch(k, 
           simi <- zelig(eleCnf2 ~ idp2*(opiEPNc + opiJVMc + opiAMLOc) + educ + satDemc, model= "logit", data= b12, cite=F) %>% 
             setx( idp2= 1, opiEPNc= seq(from= -5, to= 5, by= 1)) %>%  
             setx1( idp2= parts[j], opiEPNc= seq(from= -5, to= 5, by= 1)) %>% 
             Zelig::sim()
           ,
           simi <- zelig(eleCnf2 ~ idp2*(opiEPNc + opiJVMc + opiAMLOc) + educ + satDemc, model= "logit", data= b12, cite=F) %>% 
             setx( idp2= 1, opiJVMc= seq(from= -5, to= 5, by= 1)) %>%  
             setx1( idp2= parts[j], opiJVMc= seq(from= -5, to= 5, by= 1)) %>% 
             Zelig::sim()
           ,
           simi <- zelig(eleCnf2 ~ idp2*(opiEPNc + opiJVMc + opiAMLOc) + educ + satDemc, model= "logit", data= b12, cite=F) %>% 
             setx( idp2= 1, opiAMLOc= seq(from= -5, to= 5, by= 1)) %>%  
             setx1( idp2= parts[j], opiAMLOc= seq(from= -5, to= 5, by= 1)) %>% 
             Zelig::sim()
    )
    
    for(i in 1:length(seq(from= -5, to= 5, by= 1))){
      qs <- quantile((unlist(simi$sim.out$range[[i]][[1]])), probs= c(.025, .5, .975)); names(qs) <- NULL
      ms <- mean(unlist(simi$sim.out$range[[i]][[1]]))
      vals <- simi$setx.out$range[[i]][[2]][[1]][4 + k]#k?
      ivals <- 0
      cvar <- attributes(simi$setx.out$range[[i]][[2]][[1]])$dimnames[[2]][4 + k] #k?
      
      qs1 <- quantile((unlist(simi$sim.out$range1[[i]][[1]])), probs= c(.025, .5, .975)); names(qs1) <- NULL
      ms1 <- mean(unlist(simi$sim.out$range1[[i]][[1]]))
      vals1 <- simi$setx.out$range1[[i]][[2]][[1]][4 + k] #k?
      ivals1 <- 1
      cvar1 <- attributes(simi$setx.out$range1[[i]][[2]][[1]])$dimnames[[2]][4 + k]  #k?
      
      psimis[i,] <- data.frame(year= 2012, party= tparts[j], cand= cvar, val= vals, inter=ivals, mean= ms, min= qs[1], med= qs[2], max= qs[3], stringsAsFactors = F)
      psimis[i + length(seq(from= -5, to= 5, by= 1)), ] <- data.frame(year= 2012, party= tparts[j], cand= cvar1, val= vals1, inter=ivals1, mean= ms1, min= qs1[1], med= qs1[2], max= qs1[3], stringsAsFactors = F)
    }
    simis <- rbind(psimis, simis)
  }
  simis <- simis[-dim(simis)[1],]
  #simis
  
  plot(simis$val[simis$inter==0 & simis$party=="PRD"]+5, simis$mean[simis$inter==0 & simis$party=="PRD"], type= "l", ylim= c(0,1), 
       main= paste0(lets[let],") 2012 - Opinion of ", tcands[k]), cex.main=1.1,
       xlab= paste("Opinion of", tcands[k]), 
       ylab= "Prob. of trust")
  let= let+1 
  
  polygon(c(rev(secu + 5) , secu + 5), c(rev(simis$max[simis$inter==0 & simis$party=="PRD"]), simis$min[simis$inter==0 & simis$party=="PRD"]), col= rgb(0,0,0,.1), border=F)
  
  for(j in 1:length(parts)){
    lines(simis$val[simis$inter==1 & simis$party==tparts[j]]+5, simis$mean[simis$inter==1 & simis$party==tparts[j]], simis$val, type= "l", col= tcols[j], lty= ltys[j+1])
    
    polygon(c(rev(secu + 5) , secu + 5), c(rev(simis$max[simis$inter==1 & simis$party==tparts[j]]), simis$min[simis$inter==1 & simis$party==tparts[j]]), col= tcols2[j], border=F)
  }
}

# 7.3- simulations of interactions 2018 -------------------------------------------------------

parts <- c(2, 4, 6, 8) 
tparts <- c("PRI", "PAN", "PRD", "MOR")
tcands <- c("JAM", "RAC", "AMLO")
tcols <- c("#FF0000",  "#3366FF", "#FFCC00", "#993333")
tcols2 <- c("#FF00000D",  "#3366FF0D", "#FFCC000D", "#9933330D")

for(k in 1:length(tcands)){
  simis <- data.frame(year= NA, party= NA, cand= NA, val= NA, inter=NA, mean= NA, min= NA, med= NA, max= NA, stringsAsFactors = F)
  psimis <- simis
  set.seed(12345)
  for(j in 1:length(parts)){
    switch(k, 
           simi <- zelig(eleCnf2 ~ idp2*(opiJAMc + opiRACc + opiAMLOc) + educ + satDemc, model= "logit", data= b18, cite=F) %>% 
             setx( idp2= 1, opiJAMc= seq(from= -5, to= 5, by= 1)) %>%  
             setx1( idp2= parts[j], opiJAMc= seq(from= -5, to= 5, by= 1)) %>% 
             Zelig::sim()
           ,
           simi <- zelig(eleCnf2 ~ idp2*(opiJAMc + opiRACc + opiAMLOc) + educ + satDemc, model= "logit", data= b18, cite=F) %>% 
             setx( idp2= 1, opiRACc= seq(from= -5, to= 5, by= 1)) %>%  
             setx1( idp2= parts[j], opiRACc= seq(from= -5, to= 5, by= 1)) %>% 
             Zelig::sim()
           ,
           simi <- zelig(eleCnf2 ~ idp2*(opiJAMc + opiRACc + opiAMLOc) + educ + satDemc, model= "logit", data= b18, cite=F) %>% 
             setx( idp2= 1, opiAMLOc= seq(from= -5, to= 5, by= 1)) %>%  
             setx1( idp2= parts[j], opiAMLOc= seq(from= -5, to= 5, by= 1)) %>% 
             Zelig::sim()
    )
    
    for(i in 1:length(seq(from= -5, to= 5, by= 1))){
      qs <- quantile((unlist(simi$sim.out$range[[i]][[1]])), probs= c(.025, .5, .975)); names(qs) <- NULL
      ms <- mean(unlist(simi$sim.out$range[[i]][[1]]))
      vals <- simi$setx.out$range[[i]][[2]][[1]][5 + k]#k?
      ivals <- 0
      cvar <- attributes(simi$setx.out$range[[i]][[2]][[1]])$dimnames[[2]][5 + k] #k?
      
      qs1 <- quantile((unlist(simi$sim.out$range1[[i]][[1]])), probs= c(.025, .5, .975)); names(qs1) <- NULL
      ms1 <- mean(unlist(simi$sim.out$range1[[i]][[1]]))
      vals1 <- simi$setx.out$range1[[i]][[2]][[1]][5 + k] #k?
      ivals1 <- 1
      cvar1 <- attributes(simi$setx.out$range1[[i]][[2]][[1]])$dimnames[[2]][5 + k]  #k?
      
      psimis[i,] <- data.frame(year= 2018, party= tparts[j], cand= cvar, val= vals, inter=ivals, mean= ms, min= qs[1], med= qs[2], max= qs[3], stringsAsFactors = F)
      psimis[i + length(seq(from= -5, to= 5, by= 1)), ] <- data.frame(year= 2018, party= tparts[j], cand= cvar1, val= vals1, inter=ivals1, mean= ms1, min= qs1[1], med= qs1[2], max= qs1[3], stringsAsFactors = F)
    }
    simis <- rbind(psimis, simis)
  }
  simis <- simis[-dim(simis)[1],]
  #simis
  
  plot(simis$val[simis$inter==0 & simis$party=="PRD"]+5, simis$mean[simis$inter==0 & simis$party=="PRD"], type= "l", ylim= c(0,1), 
       main= paste0(lets[let],") 2018 - Opinion of ", tcands[k]), cex.main=1.1,
       xlab= paste("Opinion of", tcands[k]), 
       ylab= "Prob. of trust")
  let= let+1 
  
  polygon(c(rev(secu + 5) , secu + 5), c(rev(simis$max[simis$inter==0 & simis$party=="PRD"]), simis$min[simis$inter==0 & simis$party=="PRD"]), col= rgb(0,0,0,.1), border=F)
  
  for(j in 1:length(parts)){
    lines(simis$val[simis$inter==1 & simis$party==tparts[j]]+5, simis$mean[simis$inter==1 & simis$party==tparts[j]], simis$val, type= "l", col= tcols[j], lty= ltys[j+1])
    
    polygon(c(rev(secu + 5) , secu + 5), c(rev(simis$max[simis$inter==1 & simis$party==tparts[j]]), simis$min[simis$inter==1 & simis$party==tparts[j]]), col= tcols2[j], border=F)
  }
}


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

legend("bottom", legend= c("Indep.", "PRI", "PAN", "PRD", "MORENA"), xpd = TRUE, horiz = TRUE, inset = c(0,0), lwd= 2, seg.len= 2.5, col = c("black", tcols), cex= 1, lty= ltys)



