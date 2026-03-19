################################################################################
###################             RECODIFICATION              #################### 
################################################################################
library(panelr)
library(plm)
library(psych)
library(dplyr)
library(car)
library(polycor)
elw=elsoc_wide_2016_2021
dim(elw)

#Select sample 1
elw <- subset(elw, muestra==1)
#Select type of attrition: 1
elw=filter(elw, tipo_atricion==1)


####################      SOCIODEMOGRAPHIC VARIABLES        ####################

#Sexo
elw$hombre_w01 <- as.numeric(elw$m0_sexo_w01==1)
elw$hombre_w02 <- as.numeric(elw$m0_sexo_w02==1)
elw$hombre_w03 <- as.numeric(elw$m0_sexo_w03==1)
elw$hombre_w04 <- as.numeric(elw$m0_sexo_w04==1)
elw$hombre_w05 <- as.numeric(elw$m0_sexo_w05==1)

with(elw, cor(cbind(hombre_w01, hombre_w02, hombre_w03, hombre_w04, hombre_w05),
             use="pairwise.complete.obs"))
#Edad
elw$edad_w01 <- elw$m0_edad_w01
elw$edad_w02 <- elw$m0_edad_w02
elw$edad_w03 <- elw$m0_edad_w03
elw$edad_w04 <- elw$m0_edad_w04
elw$edad_w05 <- elw$m0_edad_w05

with(elw, cor(cbind(edad_w01, edad_w02, edad_w03, edad_w04, edad_w05),
             use="pairwise.complete.obs"))

#Edad en tramos 10 años
age_str <- "0:17='17 o -'; 18:24='18-24'; 25:34='25-34'; 35:44='35-44'; 45:54='45-54'; 
55:64='55-64'; 65:hi='65 o +'; -999:-888=NA"
elw$edadR_w01 <- recode(elw$edad_w01, age_str, as.factor=T) 
elw$edadR_w02 <- recode(elw$edad_w02, age_str, as.factor=T) 
elw$edadR_w03 <- recode(elw$edad_w03, age_str, as.factor=T) 
elw$edadR_w04 <- recode(elw$edad_w04, age_str, as.factor=T)
elw$edadR_w05 <- recode(elw$edad_w05, age_str, as.factor=T) 

#Educacion recodificada, ego alter
edulabs=c("Ed. Basica o menos", "Ed. Media Inc.", "Ed. Media Com.", "Ed. Tec. Sup.", "Ed. Univer.")
edurecode <- "1:3=1; 4=2; 5=3; 6:7=4; 8:10=5; -999:-888=NA"
elw$educF_w01 <- factor(recode(elw$m01_w01, edurecode), labels=edulabs)
elw$educF_w02 <- factor(recode(elw$m01_w02, edurecode), labels=edulabs)
elw$educF_w03 <- factor(recode(elw$m01_w03, edurecode), labels=edulabs)
elw$educF_w04 <- factor(recode(elw$m01_w04, edurecode), labels=edulabs)
elw$educF_w05 <- factor(recode(elw$m01_w05, edurecode), labels=edulabs)

#Educacion continua
elw$educC_w01 <- recode(elw$m01_w01, "-999:-888=NA")
elw$educC_w02 <- recode(elw$m01_w02, "-999:-888=NA")
elw$educC_w03 <- recode(elw$m01_w03, "-999:-888=NA")
elw$educC_w04 <- recode(elw$m01_w04, "-999:-888=NA")
elw$educC_w05 <- recode(elw$m01_w05, "-999:-888=NA")
with(elw, cor(cbind(educC_w01, educC_w02, educC_w03, educC_w04, educC_w05),
             use="pairwise.complete.obs"))

#Religion
rel_str <- "1='Catolico'; 2='Evangelico'; c(3,4,6)='Otra religion'; 5='Creyente, No adherente';
-999:-888='Ninguna o Ateo'; 7:9='Ninguna o Ateo'"
elw$religid_w01 <- recode(elw$m38_w01, rel_str, as.factor=TRUE)
elw$religid_w02 <- recode(elw$m38_w02, rel_str, as.factor=TRUE)
elw$religid_w03 <- recode(elw$m38_w03, rel_str, as.factor=TRUE)
elw$religid_w04 <- recode(elw$m38_w04, rel_str, as.factor=TRUE)
elw$religid_w05 <- recode(elw$m38_w05, rel_str, as.factor=TRUE)
prop.table(table(elw$religid_w05, elw$religid_w01),1)

#Practicancia religiosa
elw$practica_w01 <- 8 - elw$m39_w01
elw$practica_w01[elw$practica_w01>800] <- 1
elw$practica_w01[is.na(elw$practica_w01)] <- 1
elw$practica_w01[elw$religid_w01=="Ninguna / Ateo"] <- 1

elw$practica_w02 <- 8 - elw$m39_w02
elw$practica_w02[elw$practica_w02>800] <- 1
elw$practica_w02[is.na(elw$practica_w02)] <- 1
elw$practica_w02[elw$religid_w02=="Ninguna / Ateo"] <- 1

elw$practica_w03 <- 8 - elw$m39_w03
elw$practica_w03[elw$practica_w03>800] <- 1
elw$practica_w03[is.na(elw$practica_w03)] <- 1
elw$practica_w03[elw$religid_w03=="Ninguna / Ateo"] <- 1

elw$practica_w04 <- 8 - elw$m39_w04
elw$practica_w04[elw$practica_w04>800] <- 1
elw$practica_w04[is.na(elw$practica_w04)] <- 1
elw$practica_w04[elw$religid_w04=="Ninguna / Ateo"] <- 1

elw$practica_w05 <- 8 - elw$m39_w05
elw$practica_w05[elw$practica_w05>800] <- 1
elw$practica_w05[is.na(elw$practica_w05)] <- 1
elw$practica_w05[elw$religid_w05=="Ninguna / Ateo"] <- 1

cor(with(elw, cbind(practica_w01, practica_w02, practica_w03, practica_w04, practica_w05)),
    use = "complete.obs")
summary(elw$practica_w06)

#Satisfaccion con ingreso
elw$SatIngreso_w01 <- elw$m16_w01
elw$SatIngreso_w02 <- elw$m16_w02
elw$SatIngreso_w03 <- elw$m16_w03
elw$SatIngreso_w04 <- elw$m16_w04
elw$SatIngreso_w05 <- elw$m16_w05
elw$SatIngreso_w01[elw$SatIngreso_w01<0] <- NA
elw$SatIngreso_w02[elw$SatIngreso_w02<0] <- NA
elw$SatIngreso_w03[elw$SatIngreso_w03<0] <- NA
elw$SatIngreso_w04[elw$SatIngreso_w04<0] <- NA
elw$SatIngreso_w05[elw$SatIngreso_w05<0] <- NA

#Cantidad de vehiculos por hogar  (ola 1, 3, 4,5)
elw$autos_w01 <- elw$m32_w01
elw$autos_w02 <- NA
elw$autos_w03 <- elw$m32_w03
elw$autos_w04 <- elw$m32_w04
elw$autos_w05 <- elw$m32_w05
elw$autos_w01[elw$autos_w01<0] <- NA
elw$autos_w03[elw$autos_w03<0] <- NA
elw$autos_w04[elw$autos_w04<0] <- NA
elw$autos_w05[elw$autos_w05<0] <- NA

#Tiempo residencia en la ciudad
elw$tciudad_w01 <- elw$m34_01_w01
elw$tciudad_w02 <- NA
elw$tciudad_w03 <- elw$m34_01_w03
elw$tciudad_w04 <- NA
elw$tciudad_w05 <- NA
elw$tciudad_w01[elw$tciudad_w01<0] <- NA
elw$tciudad_w03[elw$tciudad_w03<0] <- NA

#Estado civil (ola 1, 3, 4)
ecivilabs=c("Casado", "Conviviente", "Soltero", "Viudo", "Separado o Divorciado")
ecivilrec <- "1=1; 2:3=2; 4=3; 5=4; 6:8=5; else=NA"
elw$ecivil_w01 <- factor(recode(elw$m36_w01, ecivilrec), labels=ecivilabs)
elw$ecivil_w02 <- NA
elw$ecivil_w03 <- factor(recode(elw$m36_w03, ecivilrec), labels=ecivilabs)
elw$ecivil_w04 <- factor(recode(elw$m36_w04, ecivilrec), labels=ecivilabs)
elw$ecivil_w05 <- NA

#Nacionalidad
nacionlabs=c("Chileno/a", "Otro")
nacionrec <- "1=1; 2:8=2; else=NA"
elw$nacion_w01 <- factor(recode(elw$m45_w01, nacionrec), labels=nacionlabs)
elw$nacion_w02 <- NA
elw$nacion_w03 <- factor(recode(elw$m45_w03, nacionrec), labels=nacionlabs)
elw$nacion_w04 <- NA
elw$nacion_w05 <- NA

#Condicion indigena
elw$indigena_w01 <-NA
elw$indigena_w02 <- recode(elw$m53_w02, "1:9=1; 10=0; 11=1; -999:-888=NA")
elw$indigena_w03 <- recode(elw$m53_w03, "1:9=1; 10=0; 11=1; -999:-888=NA")
elw$indigena_w04 <- recode(elw$m53_w04, "1:9=1; 10=0; 11=1; -999:-888=NA")
elw$indigena_w05 <-NA
table(elw$indigena_w02, elw$indigena_w04)

#Transicion a Desempleo
elw$desempleo_w01 <- as.numeric(elw$m02_w01==6)
elw$desempleo_w02 <- as.numeric(elw$m02_w02==6)
elw$desempleo_w03 <- as.numeric(elw$m02_w03==6)
elw$desempleo_w04 <- as.numeric(elw$m02_w04==6)
elw$desempleo_w05 <- as.numeric(elw$m02_w05==6)

with(elw, cor(cbind(desempleo_w01, desempleo_w02, desempleo_w03, desempleo_w04,
                   desempleo_w05),use="pairwise.complete.obs"))

#Transici?n a Empleo
elw$empleo_w01 <- as.numeric(elw$m02_w01>=1 & elw$m02_w01<=2)
elw$empleo_w02 <- as.numeric(elw$m02_w02>=1 & elw$m02_w02<=2)
elw$empleo_w03 <- as.numeric(elw$m02_w03>=1 & elw$m02_w03<=2)
elw$empleo_w04 <- as.numeric(elw$m02_w04>=1 & elw$m02_w04<=2)
elw$empleo_w05 <- as.numeric(elw$m02_w05>=1 & elw$m02_w05<=2)
with(elw, cor(cbind(empleo_w01, empleo_w02, empleo_w03, empleo_w04, empleo_w05),
             use="pairwise.complete.obs"))

#Check inter items correlation
with(elw, cor(empleo_w01, desempleo_w01, use="complete.obs"))
with(elw, cor(empleo_w03, desempleo_w03, use="complete.obs"))

#Medio de transporte al trabjo o lugar de estudios: vehiculo particular vs lo demas
elw$auto_laburo_w01 <- as.numeric(elw$m41_w01==2)
elw$auto_laburo_w02 <- as.numeric(elw$m41_w02==2)
elw$auto_laburo_w03 <- as.numeric(elw$m41_w03==2)
elw$auto_laburo_w04 <- as.numeric(elw$m41_w04==2)
elw$auto_laburo_w05 <- as.numeric(elw$m41_w05==2)

table(elw$m02_w05)


#########################   HEALTH AND WELL-BEING   ############################

##Satisfaccion con la vida##
elw$SatisVida_w01 <- recode(elw$s01_w01, "-999:-888=NA")
elw$SatisVida_w02 <- recode(elw$s01_w02, "-999:-888=NA")
elw$SatisVida_w03 <- recode(elw$s01_w03, "-999:-888=NA")
elw$SatisVida_w04 <- recode(elw$s01_w04, "-999:-888=NA")
elw$SatisVida_w05 <- recode(elw$s01_w05, "-999:-888=NA")

##Life Ideal
elw$VidaIdeal_w01 <- recode(elw$s02_w01, "-999:-888=NA")
elw$VidaIdeal_w02 <- recode(elw$s02_w02, "-999:-888=NA")
elw$VidaIdeal_w03 <- recode(elw$s02_w03, "-999:-888=NA")
elw$VidaIdeal_w04 <- recode(elw$s02_w04, "-999:-888=NA")
elw$VidaIdeal_w05 <- recode(elw$s02_w05, "-999:-888=NA")

##Overall Life Evaluation
elw$LifeEval_w01 <- (elw$SatisVida_w01 + elw$VidaIdeal_w01) / 2
elw$LifeEval_w02 <- (elw$SatisVida_w02 + elw$VidaIdeal_w02) / 2
elw$LifeEval_w03 <- (elw$SatisVida_w03 + elw$VidaIdeal_w03) / 2
elw$LifeEval_w04 <- (elw$SatisVida_w04 + elw$VidaIdeal_w04) / 2
elw$LifeEval_w05 <- (elw$SatisVida_w05 + elw$VidaIdeal_w05) / 2

##Health Auto-report
elw$AutoSalud_w01 <- recode(elw$s03_w01, "-999:-888=NA")
elw$AutoSalud_w02 <- recode(elw$s03_w02, "-999:-888=NA")
elw$AutoSalud_w03 <- recode(elw$s03_w03, "-999:-888=NA")
elw$AutoSalud_w04 <- recode(elw$s03_w04, "-999:-888=NA")
elw$AutoSalud_w05 <- recode(elw$s03_w05, "-999:-888=NA")


#Check Correlation
cor(with(elw, cbind(SatisVida_w01, VidaIdeal_w01, AutoSalud_w01)), 
    use = "pairwise.complete.obs")
cor(with(elw, cbind(SatisVida_w05, VidaIdeal_w05, AutoSalud_w05)), 
    use = "pairwise.complete.obs")


#######################    CITIZENHIP AND DEMOCRACY     ########################

####Satisaccion con la Democracia##
elw$satisdemo_w01 <- recode(elw$c01_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$satisdemo_w02 <- recode(elw$c01_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$satisdemo_w03 <- recode(elw$c01_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$satisdemo_w04 <- recode(elw$c01_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$satisdemo_w05 <- recode(elw$c01_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-666=NA")

### CONFIANZA SOCIAL ###
#Confianza Social 1 - Confianza Social Generalizada
elw$confsoc1_w01 <- recode(elw$c02_w01, "1=3; 2=1; 3=2; -999:-888=NA")
elw$confsoc1_w02 <- recode(elw$c02_w02, "1=3; 2=1; 3=2; -999:-888=NA")
elw$confsoc1_w03 <- recode(elw$c02_w03, "1=3; 2=1; 3=2; -999:-888=NA")
elw$confsoc1_w04 <- recode(elw$c02_w04, "1=3; 2=1; 3=2; -999:-888=NA")
elw$confsoc1_w05 <- recode(elw$c02_w05, "1=3; 2=1; 3=2; -999:-666=NA")
#Confianza Social 2 - Altruismo Social Generalizado
elw$confsoc2_w01 <- recode(elw$c03_w01, "1=3; 2=1; 3=2; -999:-888=NA")
elw$confsoc2_w02 <- recode(elw$c03_w02, "1=3; 2=1; 3=2; -999:-888=NA")
elw$confsoc2_w03 <- recode(elw$c03_w03, "1=3; 2=1; 3=2; -999:-888=NA")
elw$confsoc2_w04 <- recode(elw$c03_w04, "1=3; 2=1; 3=2; -999:-888=NA")
elw$confsoc2_w05 <- recode(elw$c03_w05, "1=3; 2=1; 3=2; -999:-666=NA")
#Confianza Social 3 - Mayoria de la gente trata de ser justa
elw$confsoc3_w01 <- recode(elw$c04_w01, "1=1; 2=3; 3=2; -999:-888=NA")
elw$confsoc3_w02 <- recode(elw$c04_w02, "1=1; 2=3; 3=2; -999:-888=NA")
elw$confsoc3_w03 <- recode(elw$c04_w03, "1=1; 2=3; 3=2; -999:-888=NA")
elw$confsoc3_w04 <- recode(elw$c04_w04, "1=1; 2=3; 3=2; -999:-888=NA")
elw$confsoc3_w05 <- recode(elw$c04_w05, "1=1; 2=3; 3=2; -999:-666=NA")

###### Confianza social sin NA ######
#Confianza Social 1 - Confianza Social Generalizada
elw$confsoc1r_w01 <- recode(elw$c02_w01, "1=3; 2=1; 3=2; -999:-888=2")
elw$confsoc1r_w02 <- recode(elw$c02_w02, "1=3; 2=1; 3=2; -999:-888=2")
elw$confsoc1r_w03 <- recode(elw$c02_w03, "1=3; 2=1; 3=2; -999:-888=2")
elw$confsoc1r_w04 <- recode(elw$c02_w04, "1=3; 2=1; 3=2; -999:-888=2")
elw$confsoc1r_w05 <- recode(elw$c02_w05, "1=3; 2=1; 3=2; -999:-666=2")
#Confianza Social 2 - Altruismo Social Generalizado
elw$confsoc2r_w01 <- recode(elw$c03_w01, "1=3; 2=1; 3=2; -999:-888=2")
elw$confsoc2r_w02 <- recode(elw$c03_w02, "1=3; 2=1; 3=2; -999:-888=2")
elw$confsoc2r_w03 <- recode(elw$c03_w03, "1=3; 2=1; 3=2; -999:-888=2")
elw$confsoc2r_w04 <- recode(elw$c03_w04, "1=3; 2=1; 3=2; -999:-888=2")
elw$confsoc2r_w05 <- recode(elw$c03_w05, "1=3; 2=1; 3=2; -999:-666=2")
#Confianza Social 3 - Mayoria de la gente trata de ser justa
elw$confsoc3r_w01 <- recode(elw$c04_w01, "1=1; 2=3; 3=2; -999:-888=2")
elw$confsoc3r_w02 <- recode(elw$c04_w02, "1=1; 2=3; 3=2; -999:-888=2")
elw$confsoc3r_w03 <- recode(elw$c04_w03, "1=1; 2=3; 3=2; -999:-888=2")
elw$confsoc3r_w04 <- recode(elw$c04_w04, "1=1; 2=3; 3=2; -999:-888=2")
elw$confsoc3r_w05 <- recode(elw$c04_w05, "1=1; 2=3; 3=2; -999:-666=2")

#Compare
with(elw, cor(cbind(confsoc3r_w01, confsoc3r_w02, confsoc3r_w03, confsoc3r_w04, 
                    confsoc3r_w05),use="pairwise.complete.obs"))
with(elw, cor(cbind(confsoc3_w01, confsoc3_w02, confsoc3_w03, confsoc3_w04,
                    confsoc3_w05), use="pairwise.complete.obs"))


###---CONFIANZA INSTITUCIONAL---###
#Confianza en el Gobierno
elw$confGob_w01 <- recode(elw$c05_01_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confGob_w02 <- recode(elw$c05_01_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confGob_w03 <- recode(elw$c05_01_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confGob_w04 <- recode(elw$c05_01_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confGob_w05 <- recode(elw$c05_01_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-666=NA")
#Confianza en Partidos politicos
elw$confPart_w01 <- recode(elw$c05_02_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confPart_w02 <- recode(elw$c05_02_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confPart_w03 <- recode(elw$c05_02_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confPart_w04 <- recode(elw$c05_02_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confPart_w05 <- recode(elw$c05_02_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-666=NA")
#Confianza en Carabineros
elw$confCara_w01 <- recode(elw$c05_03_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")#NA codificados con 3
elw$confCara_w02 <- recode(elw$c05_03_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confCara_w03 <- recode(elw$c05_03_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confCara_w04 <- recode(elw$c05_03_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confCara_w05 <- recode(elw$c05_03_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-666=3")
#Confianza en Sindicatos
elw$confSind_w01 <- recode(elw$c05_04_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confSind_w02 <- recode(elw$c05_04_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confSind_w03 <- recode(elw$c05_04_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confSind_w04 <- recode(elw$c05_04_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confSind_w05 <- NA
table(elw$c05_04_w03)
#Confianza en Poder Judicial
elw$confJudi_w01 <- recode(elw$c05_05_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confJudi_w02 <- recode(elw$c05_05_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confJudi_w03 <- recode(elw$c05_05_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confJudi_w04 <- recode(elw$c05_05_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confJudi_w05 <- recode(elw$c05_05_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-666=NA")
#Confianza en Empresas Privadas
elw$confEmpr_w01 <- recode(elw$c05_06_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confEmpr_w02 <- recode(elw$c05_06_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confEmpr_w03 <- recode(elw$c05_06_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confEmpr_w04 <- recode(elw$c05_06_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confEmpr_w05 <- NA
#Confianza en Congreso
elw$confCong_w01 <- recode(elw$c05_07_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confCong_w02 <- recode(elw$c05_07_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confCong_w03 <- recode(elw$c05_07_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confCong_w04 <- recode(elw$c05_07_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confCong_w05 <- recode(elw$c05_07_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-666=NA")
#Confianza en Presidente
elw$confPres_w01 <- recode(elw$c05_08_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confPres_w02 <- recode(elw$c05_08_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confPres_w03 <- recode(elw$c05_08_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confPres_w04 <- recode(elw$c05_08_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confPres_w05 <- recode(elw$c05_08_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
#Confianza en Fiscalia
elw$confFisc_w01 <- NA
elw$confFisc_w02 <- recode(elw$c05_09_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confFisc_w03 <- recode(elw$c05_09_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$confFisc_w04 <- NA
elw$confFisc_w05 <- NA

######### Confianza en instituciones sin NA ###########
#Confianza en el Gobierno
elw$confGobr_w01 <- recode(elw$c05_01_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confGobr_w02 <- recode(elw$c05_01_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confGobr_w03 <- recode(elw$c05_01_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confGobr_w04 <- recode(elw$c05_01_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confGobr_w05 <- recode(elw$c05_01_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-666=3")
#Confianza en Partidos politicos
elw$confPartr_w01 <- recode(elw$c05_02_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confPartr_w02 <- recode(elw$c05_02_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confPartr_w03 <- recode(elw$c05_02_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confPartr_w04 <- recode(elw$c05_02_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confPartr_w05 <- recode(elw$c05_02_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-666=3")
#Confianza en Carabineros
elw$confCarar_w01 <- recode(elw$c05_03_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confCarar_w02 <- recode(elw$c05_03_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confCarar_w03 <- recode(elw$c05_03_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confCarar_w04 <- recode(elw$c05_03_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confCarar_w05 <- recode(elw$c05_03_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-666=3")
#Confianza en Sindicatos
elw$confSindr_w01 <- recode(elw$c05_04_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confSindr_w02 <- recode(elw$c05_04_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confSindr_w03 <- recode(elw$c05_04_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confSindr_w04 <- recode(elw$c05_04_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confSindr_w05 <- NA
#Confianza en Poder Judicial
elw$confJudir_w01 <- recode(elw$c05_05_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confJudir_w02 <- recode(elw$c05_05_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confJudir_w03 <- recode(elw$c05_05_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confJudir_w04 <- recode(elw$c05_05_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confJudir_w05 <- recode(elw$c05_05_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-666=3")
#Confianza en Empresas Privadas
elw$confEmprr_w01 <- recode(elw$c05_06_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confEmprr_w02 <- recode(elw$c05_06_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confEmprr_w03 <- recode(elw$c05_06_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confEmprr_w04 <- recode(elw$c05_06_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confEmprr_w05 <- NA
#Confianza en Congreso
elw$confCongr_w01 <- recode(elw$c05_07_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confCongr_w02 <- recode(elw$c05_07_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confCongr_w03 <- recode(elw$c05_07_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confCongr_w04 <- recode(elw$c05_07_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confCongr_w05 <- recode(elw$c05_07_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-666=3")
#Confianza en Presidente
elw$confPresr_w01 <- recode(elw$c05_08_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confPresr_w02 <- recode(elw$c05_08_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confPresr_w03 <- recode(elw$c05_08_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confPresr_w04 <- recode(elw$c05_08_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confPresr_w05 <- recode(elw$c05_08_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
#Confianza en Fiscalia
elw$confFiscr_w02 <- recode(elw$c05_09_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confFiscr_w03 <- recode(elw$c05_09_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=3")
elw$confFiscr_w04 <- NA
elw$confFiscr_w05 <- NA


###---CONFIANZA EN GRUPOS SOCIOPOLITICOS---###
#Confianza en Militantes de la UDI
elw$confUdi_w01 <- recode(elw$c06_01_w01, "-999:-888=NA")
elw$confUdi_w02 <- NA
elw$confUdi_w03 <- recode(elw$c06_01_w03, "-999:-888=NA")
elw$confUdi_w04 <- NA
elw$confUdi_w05 <- NA
#Confianza en Militantes de la DC
elw$confDc_w01 <- recode(elw$c06_02_w01, "-999:-888=NA")
elw$confDc_w02 <- NA
elw$confDc_w03 <- recode(elw$c06_02_w03, "-999:-888=NA")
elw$confDc_w04 <- NA
elw$confDc_w05 <- NA
#Confianza en Militantes de la PC
elw$confPc_w01 <- recode(elw$c06_03_w01, "-999:-888=NA")
elw$confPc_w02 <- NA
elw$confPc_w03 <- recode(elw$c06_03_w03, "-999:-888=NA")
elw$confPc_w04 <- NA
elw$confPc_w05 <- NA
#Confianza en Homosexuales
elw$confHomo_w01 <- recode(elw$c06_04_w01, "-999:-888=NA")
elw$confHomo_w02 <- NA
elw$confHomo_w03 <- recode(elw$c06_04_w03, "-999:-888=NA")
elw$confHomo_w04 <- NA
elw$confHomo_w05 <- NA
#Confianza en Mapuches
elw$confMapu_w01 <- recode(elw$c06_05_w01, "-999:-888=NA")
elw$confMapu_w02 <- NA
elw$confMapu_w03 <- recode(elw$c06_05_w03, "-999:-888=NA")
elw$confMapu_w04 <- NA
elw$confMapu_w05 <- NA
#Confianza en Inmigrantes peruanos
elw$confPeru_w01 <- recode(elw$c06_06_w01, "-999:-888=NA")
elw$confPeru_w02 <- NA
elw$confPeru_w03 <- recode(elw$c06_06_w03, "-999:-888=NA")
elw$confPeru_w04 <- NA
elw$confPeru_w05 <- NA


###PARTICIPACION POLITICA NO FORMAL
# (1=Nunca; 2=Casi Nunca/Ocasionalmente; 3=Frecuentemente/Muy Frecuentemente)
#Frecuencia: Firma carta o peticion apoyando causa
elw$FirmCart_w01 <- recode(elw$c08_01_w01, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$FirmCart_w02 <- recode(elw$c08_01_w02, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$FirmCart_w03 <- recode(elw$c08_01_w03, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$FirmCart_w04 <- recode(elw$c08_01_w04, "1=1; 2:3=2; 4:5=3; -999:-666=NA")
elw$FirmCart_w05 <- NA #No hay ?tem
#Frecuencia: Asiste a marcha o manifestacion pacifica
elw$AsisMarc_w01 <- recode(elw$c08_02_w01, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$AsisMarc_w02 <- recode(elw$c08_02_w02, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$AsisMarc_w03 <- recode(elw$c08_02_w03, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$AsisMarc_w04 <- recode(elw$c08_02_w04, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$AsisMarc_w05 <- recode(elw$c08_02_w05, "1=1; 2:3=2; 4:5=3; -999:-666=NA")
#Frecuencia: Participo en huelga
elw$PartHuel_w01 <- recode(elw$c08_03_w01, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$PartHuel_w02 <- recode(elw$c08_03_w02, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$PartHuel_w03 <- recode(elw$c08_03_w03, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$PartHuel_w04 <- recode(elw$c08_03_w04, "1=1; 2:3=2; 4:5=3; -999:-666=NA")
elw$PartHuel_w05 <- NA
#Frecuencia: Usa redes sociales para opinar en temas publicos
elw$RedeOpin_w01 <- recode(elw$c08_04_w01, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$RedeOpin_w02 <- recode(elw$c08_04_w02, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$RedeOpin_w03 <- recode(elw$c08_04_w03, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$RedeOpin_w04 <- recode(elw$c08_04_w04, "1=1; 2:3=2; 4:5=3; -999:-888=NA")
elw$RedeOpin_w05 <- recode(elw$c08_04_w05, "1=1; 2:3=2; 4:5=3; -999:-666=NA")
# with(el, polychor(FirmCart_w01, FirmCart_w04)) 
# with(el, polychor(FirmCart_w02, FirmCart_w04)) 
# with(el, polychor(FirmCart_w03, FirmCart_w04)) 
# with(el, polychor(AsisMarc_w01, AsisMarc_w02)) 
# with(el, polychor(AsisMarc_w01, AsisMarc_w03)) 
# with(el, polychor(AsisMarc_w02, AsisMarc_w03)) 


###DISPOSICION A PARTICIPACION POLITICA NO FORMAL (Ola 1 y 3)
#(1=Nada/Casi nada dispuesto; 2=Algo dispuesto; 3=Bastante/Totalmente dispuesto )

#Disposicion: Firma carta o peticion apoyando causa
elw$DisFirmCart_w01 <- recode(elw$c09_01_w01, "1:2=1; 3=2; 4:5=3; -999:-888=NA")
elw$DisFirmCart_w02 <- NA 
elw$DisFirmCart_w03 <- recode(elw$c09_01_w03, "1:2=1; 3=2; 4:5=3; -999:-888=NA")
elw$DisFirmCart_w04 <- NA
elw$DisFirmCart_w05 <- NA
#Disposicion: Asiste a marcha o manifestacion pacifica
elw$DisAsisMarc_w01 <- recode(elw$c09_02_w01, "1:2=1; 3=2; 4:5=3; -999:-888=NA")
elw$DisAsisMarc_w02 <- NA
elw$DisAsisMarc_w03 <- recode(elw$c09_02_w03, "1:2=1; 3=2; 4:5=3; -999:-888=NA")
elw$DisAsisMarc_w04 <- NA
elw$DisAsisMarc_w05 <- NA
#Disposicion: Participo en huelga
elw$DisPartHuel_w01 <- recode(elw$c09_03_w01, "1:2=1; 3=2; 4:5=3; -999:-888=NA")
elw$DisPartHuel_w02 <- NA
elw$DisPartHuel_w03 <- recode(elw$c09_03_w03, "1:2=1; 3=2; 4:5=3; -999:-888=NA")
elw$DisPartHuel_w04 <- NA
elw$DisPartHuel_w05 <- NA
#Disposicion: Usa redes sociales para opinar en temas publicos
elw$DisRedeOpin_w01 <- recode(elw$c09_04_w01, "1:2=1; 3=2; 4:5=3; -999:-888=NA")
elw$DisRedeOpin_w02 <- NA
elw$DisRedeOpin_w03 <- recode(elw$c09_04_w03, "1:2=1; 3=2; 4:5=3; -999:-888=NA")
elw$DisRedeOpin_w04 <- NA
elw$DisRedeOpin_w05 <- NA



###########   VALORACION VOTO   ###########
#Votar es mi deber como ciudadano
elw$VotaDebe_w01 <- recode(elw$c10_01_w01, "-999:-888=NA")
elw$VotaDebe_w02 <- recode(elw$c10_01_w02, "-999:-888=NA")
elw$VotaDebe_w03 <- recode(elw$c10_01_w03, "-999:-888=NA")
elw$VotaDebe_w04 <- recode(elw$c10_01_w04, "-999:-888=NA")
elw$VotaDebe_w05 <- recode(elw$c10_01_w05, "-999:-666=NA")
#Mi voto influye en el resultado
elw$VotoInfl_w01 <- recode(elw$c10_02_w01, "-999:-888=NA")
elw$VotoInfl_w02 <- recode(elw$c10_02_w02, "-999:-888=NA")
elw$VotoInfl_w03 <- recode(elw$c10_02_w03, "-999:-888=NA")
elw$VotoInfl_w04 <- recode(elw$c10_02_w04, "-999:-888=NA")
elw$VotoInfl_w05 <- recode(elw$c10_02_w05, "-999:-666=NA")
#Votar permite expresar mis ideas
elw$VotaExpr_w01 <- recode(elw$c10_03_w01, "-999:-888=NA")
elw$VotaExpr_w02 <- recode(elw$c10_03_w02, "-999:-888=NA")
elw$VotaExpr_w03 <- recode(elw$c10_03_w03, "-999:-888=NA")
elw$VotaExpr_w04 <- recode(elw$c10_03_w04, "-999:-888=NA")
elw$VotaExpr_w05 <- recode(elw$c10_03_w05, "-999:-666=NA")

#Indice valoraci?n del voto
elw$ValorVotoIn_w01 <- apply(with(elw, cbind(VotaDebe_w01,VotoInfl_w01,
                                              VotaExpr_w01)),1, mean, na.rm=T)
elw$ValorVotoIn_w02 <- apply(with(elw, cbind(VotaDebe_w02,VotoInfl_w02,
                                              VotaExpr_w02)),1, mean, na.rm=T)
elw$ValorVotoIn_w03 <- apply(with(elw, cbind(VotaDebe_w03,VotoInfl_w03,
                                             VotaExpr_w03)),1, mean, na.rm=T)
elw$ValorVotoIn_w04 <- apply(with(elw, cbind(VotaDebe_w04,VotoInfl_w04,
                                             VotaExpr_w04)),1, mean, na.rm=T)
elw$ValorVotoIn_w05 <- apply(with(elw, cbind(VotaDebe_w05,VotoInfl_w05,
                                             VotaExpr_w05)),1, mean, na.rm=T)

##Interes en la politica (Ns/NR recodificados con 1)
elw$InteresPoli_w01 <- recode(elw$c13_w01, "-999:-888=1")
elw$InteresPoli_w02 <- recode(elw$c13_w02, "-999:-888=1")
elw$InteresPoli_w03 <- recode(elw$c13_w03, "-999:-888=1")
elw$InteresPoli_w04 <- recode(elw$c13_w04, "-999:-888=1")
elw$InteresPoli_w05 <- recode(elw$c13_w05, "-999:-666=1")
table(elw$InteresPoli_w01)

#Frecuencia Habla de politica con familiares o amigos
elw$HablaPoli_w01 <- recode(elw$c14_01_w01, "-999:-888=NA")
elw$HablaPoli_w02 <- recode(elw$c14_01_w02, "-999:-888=NA")
elw$HablaPoli_w03 <- recode(elw$c14_01_w03, "-999:-888=NA")
elw$HablaPoli_w04 <- recode(elw$c14_01_w04, "-999:-888=NA")
elw$HablaPoli_w05 <- recode(elw$c14_01_w05, "-999:-666=NA")

#Frecuencia Se informa sobre politica en medios de comunicacion
elw$MediosPoli_w01 <- recode(elw$c14_02_w01, "-999:-888=NA")
elw$MediosPoli_w02 <- recode(elw$c14_02_w02, "-999:-888=NA")
elw$MediosPoli_w03 <- recode(elw$c14_02_w03, "-999:-888=NA")
elw$MediosPoli_w04 <- recode(elw$c14_02_w04, "-999:-888=NA")
elw$MediosPoli_w05 <- recode(elw$c14_02_w05, "-999:-666=NA")

elw$polengage_w01 <- with(elw, apply(cbind(HablaPoli_w01, MediosPoli_w01, InteresPoli_w01),
                                     1, mean, na.rm=T))
elw$polengage_w02 <- with(elw, apply(cbind(HablaPoli_w02, MediosPoli_w02, InteresPoli_w02),
                                     1, mean, na.rm=T))
elw$polengage_w03 <- with(elw, apply(cbind(HablaPoli_w03, MediosPoli_w03, InteresPoli_w03),
                                     1, mean, na.rm=T))
elw$polengage_w04 <- with(elw, apply(cbind(HablaPoli_w03, MediosPoli_w04, InteresPoli_w04),
                                     1, mean, na.rm=T))
elw$polengage_w05 <- with(elw, apply(cbind(HablaPoli_w05, MediosPoli_w05, InteresPoli_w05),
                                     1, mean, na.rm=T))

##############    Posición ideológica     ###############

#Menciona posicion ideologica
elw$pos_ideo_w01 <- recode(elw$c15_w01, "0:10=1; 11:12=0;-999:-888=0")
elw$pos_ideo_w02 <- recode(elw$c15_w02, "0:10=1; 11:12=0;-999:-888=0")
elw$pos_ideo_w03 <- recode(elw$c15_w03, "0:10=1; 11:12=0;-999:-888=0")
elw$pos_ideo_w04 <- recode(elw$c15_w04, "0:10=1; 11:12=0;-999:-888=0")
elw$pos_ideo_w05 <- recode(elw$c15_w05, "0:10=1; 11:12=0;-999:-666=0")

#Posicion ideologica (continua)
elw$izqder_w01 <- ifelse(elw$pos_ideo_w01==1, elw$c15_w01, NA)
elw$izqder_w02 <- ifelse(elw$pos_ideo_w02==1, elw$c15_w02, NA)
elw$izqder_w03 <- ifelse(elw$pos_ideo_w03==1, elw$c15_w03, NA)
elw$izqder_w04 <- ifelse(elw$pos_ideo_w04==1, elw$c15_w04, NA)
elw$izqder_w05 <- ifelse(elw$pos_ideo_w05==1, elw$c15_w05, NA)
cor(cbind(elw$izqder_w01, elw$izqder_w02, elw$izqder_w03, 
          elw$izqder_w04, elw$izqder_w05), use="pairwise.complete.obs")

#Identificacion Partidaria
elw$partyid_w01 <- recode(elw$c16_w01, "1:14=1; 15=0; -999:-888=0")
elw$partyid_w02 <- recode(elw$c16_w02, "1:14=1; 15=0; -999:-888=0")
elw$partyid_w03 <- recode(elw$c16_w03, "1:14=1; 15=0; -999:-888=0")
elw$partyid_w04 <- recode(elw$c16_w04, "1:14=1; 15=0; -999:-888=0")
elw$partyid_w05 <- NA

#Posicion ideologica recodificada
elw$izqderF_w01 <- factor(car::recode(elw$c15_w01, "0:4=1; 5=2; 6:10=3; 11:12=4;-999:-888=4"),
                          labels=c("Izquierda","Centro", "Derecha", "Ninguna/Ind"))
elw$izqderF_w02 <- factor(car::recode(elw$c15_w02, "0:4=1; 5=2; 6:10=3;11:12=4;-999:-888=4"),
                          labels=c("Izquierda","Centro", "Derecha", "Ninguna/Ind"))
elw$izqderF_w03 <- factor(car::recode(elw$c15_w03, "0:4=1; 5=2; 6:10=3;11:12=4;-999:-888=4"),
                          labels=c("Izquierda","Centro", "Derecha", "Ninguna/Ind"))
elw$izqderF_w04 <- factor(car::recode(elw$c15_w04, "0:4=1; 5=2; 6:10=3;11:12=4;-999:-888=4"),
                          labels=c("Izquierda","Centro", "Derecha", "Ninguna/Ind"))
elw$izqderF_w05 <- factor(car::recode(elw$c15_w05, "0:4=1; 5=2; 6:10=3;11:12=4;-999:-666=4"),
                          labels=c("Izquierda","Centro", "Derecha", "Ninguna/Ind"))

#Dummies para izq-der
elw$izq_w01 <- as.numeric(elw$izqderF_w01=="Izquierda")
elw$izq_w02 <- as.numeric(elw$izqderF_w02=="Izquierda")
elw$izq_w03 <- as.numeric(elw$izqderF_w03=="Izquierda")
elw$izq_w04 <- as.numeric(elw$izqderF_w04=="Izquierda")
elw$izq_w05 <- as.numeric(elw$izqderF_w05=="Izquierda")
elw$centro_w01 <- as.numeric(elw$izqderF_w01=="Centro")
elw$centro_w02 <- as.numeric(elw$izqderF_w02=="Centro")
elw$centro_w03 <- as.numeric(elw$izqderF_w03=="Centro")
elw$centro_w04 <- as.numeric(elw$izqderF_w04=="Centro")
elw$centro_w05 <- as.numeric(elw$izqderF_w05=="Centro")
elw$derecha_w01 <- as.numeric(elw$izqderF_w01=="Derecha")
elw$derecha_w02 <- as.numeric(elw$izqderF_w02=="Derecha")
elw$derecha_w03 <- as.numeric(elw$izqderF_w03=="Derecha")
elw$derecha_w04 <- as.numeric(elw$izqderF_w04=="Derecha")
elw$derecha_w05 <- as.numeric(elw$izqderF_w05=="Derecha")
elw$ninguna_ind_w01 <- as.numeric(elw$izqderF_w01=="Ninguna/Ind")
elw$ninguna_ind_w02 <- as.numeric(elw$izqderF_w02=="Ninguna/Ind")
elw$ninguna_ind_w03 <- as.numeric(elw$izqderF_w03=="Ninguna/Ind")
elw$ninguna_ind_w04 <- as.numeric(elw$izqderF_w04=="Ninguna/Ind")
elw$ninguna_ind_w05 <- as.numeric(elw$izqderF_w05=="Ninguna/Ind")

#Polarizacion ideologica
pp <- "5=1; 4=2; 6=2; 3=3; 7=3; 8=4; 2=4; 9=5; 1=5; 0=6; 10=6; 11:12= NA;-999:-666=NA"
elw$polariza_w01 <- car::recode(elw$c15_w01, pp)
elw$polariza_w02 <- car::recode(elw$c15_w02, pp)
elw$polariza_w03 <- car::recode(elw$c15_w03, pp)
elw$polariza_w04 <- car::recode(elw$c15_w04, pp)
elw$polariza_w05 <- car::recode(elw$c15_w05, pp)

###Partido Preferido - 3 tercios y medio###
recode_party <- "1:4=1; c(6,12,13)=2; 8=3; c(5,7,9,10,11)=4; 14=NA; 15=NA; -999:-666=NA"
elw$partypref_w01 <- factor(recode(elw$c16_w01, recode_party),
                            labels=c("Izq", "Cen-Izq", "DC", "Der"))
elw$partypref_w02 <- factor(recode(elw$c16_w02, recode_party),
                            labels=c("Izq", "Cen-Izq", "DC", "Der"))
elw$partypref_w03 <- factor(recode(elw$c16_w03, recode_party),
                            labels=c("Izq", "Cen-Izq", "DC", "Der"))
elw$partypref_w04 <- factor(recode(elw$c16_w04, recode_party),
                            labels=c("Izq", "Cen-Izq", "DC", "Der"))
elw$partypref_w05<-NA

###Identificación con coaliciones

elw$idcoalicion_w01 <- factor(car::recode(elw$c17_w01, "1=1; 2=2; 4=4; 5=5; -999:-888=NA"),
                              labels=c("Chile Vamos","Nueva Mayoría", "Otra", "Ninguna"))
elw$idcoalicion_w02 <- factor(car::recode(elw$c17_w02, "1=1; 2=2; 3=3; 4=4; 5=5; -999:-888=NA"),
                              labels=c("Chile Vamos","Nueva Mayoría", "Frente Amplio", "Otra", "Ninguna"))
elw$idcoalicion_w03 <- factor(car::recode(elw$c17_w03, "1=1; 2=2; 3=3; 4=4; 5=5; -999:-888=NA"),
                              labels=c("Chile Vamos","Nueva Mayoría", "Frente Amplio", "Otra", "Ninguna"))
elw$idcoalicion_w04 <- factor(car::recode(elw$c17_w04, "1=1; 2=2; 3=3; 4=4; 5=5; -999:-888=NA"),
                              labels=c("Chile Vamos","Nueva Mayoría", "Frente Amplio", "Otra", "Ninguna"))
elw$idcoalicion_w05 <- NA

elw$chivamos_w01 <- as.numeric(elw$idcoalicion_w01=="Chile Vamos")
elw$chivamos_w02 <- as.numeric(elw$idcoalicion_w02=="Chile Vamos")
elw$chivamos_w03 <- as.numeric(elw$idcoalicion_w03=="Chile Vamos")
elw$chivamos_w04 <- as.numeric(elw$idcoalicion_w04=="Chile Vamos")
elw$chivamos_w05 <- NA
elw$nuevmayor_w01 <- as.numeric(elw$idcoalicion_w01=="Nueva Mayoría")
elw$nuevmayor_w02 <- as.numeric(elw$idcoalicion_w02=="Nueva Mayoría")
elw$nuevmayor_w03 <- as.numeric(elw$idcoalicion_w03=="Nueva Mayoría")
elw$nuevmayor_w04 <- as.numeric(elw$idcoalicion_w04=="Nueva Mayoría")
elw$nuevmayor_w05 <- NA
elw$famplio_w01 <- as.numeric(elw$idcoalicion_w01=="Frente Amplio")
elw$famplio_w02 <- as.numeric(elw$idcoalicion_w02=="Frente Amplio")
elw$famplio_w03 <- as.numeric(elw$idcoalicion_w03=="Frente Amplio")
elw$famplio_w04 <- as.numeric(elw$idcoalicion_w04=="Frente Amplio")
elw$famplio_w05 <- NA
elw$nocoalic_w01 <- as.numeric(elw$idcoalicion_w01=="Ninguna")
elw$nocoalic_w02 <- as.numeric(elw$idcoalicion_w02=="Ninguna")
elw$nocoalic_w03 <- as.numeric(elw$idcoalicion_w03=="Ninguna")
elw$nocoalic_w04 <- as.numeric(elw$idcoalicion_w04=="Ninguna")
elw$nocoalic_w05 <- NA


###Identificacion con coalicion de gobierno
table(elw$c17_w02)
elw$idgobierno_w01 <- recode(elw$c17_w01, "1=0; 2=1; 5=0; -999:-888=NA")
elw$idgobierno_w02 <- recode(elw$c17_w02, "1=1; 2=0; 3:5=0; -999:-888=NA")
elw$idgobierno_w03 <- recode(elw$c17_w03, "1=1; 2=0; 3:5=0; -999:-888=NA")
elw$idgobierno_w04 <- recode(elw$c17_w04, "1=1; 2=0; 3:5=0; -999:-888=NA")
elw$idgobierno_w05 <- NA

##Participación y voto en elecciones presidenciales

###Participacion Electoral Retrospectiva### -------------
# (1=Si; 0=No; NA=No tenia 18 anos/NSNR)

table(elw$c11_w03)

votorec <- "1=1; 2=0; 3=NA; -888=NA; -999=NA"
elw$part_elect2013_w01 <- as.numeric(car::recode(elw$c11_w01, votorec))
elw$part_elect2017_w03 <- as.numeric(car::recode(elw$c11_w03, votorec))

table(elw$part_elect2013_w01)
table(elw$part_elect2017_w03)

#Intención de voto en elección presidencial 2017
table(elw$c36_w02)
elw$intvoto_2017_w02 <- factor(car::recode(elw$c36_w02, "8:10=8; -999:-888=9"),
                               labels=c("Guillier", "Piñera", "Sanchez", "Goic", 
                                        "MEO", "Parisi", "Otro", "No sabe/No vota", "NS/NR"))
table(elw$c36_w02)
table(elw$intvoto_2017_w02)

elw$int_Guillier02 <- as.numeric(elw$intvoto_2017_w02=="Guillier")
elw$int_Pinera02 <- as.numeric(elw$intvoto_2017_w02=="Piñera")
elw$int_Sanchez02 <- as.numeric(elw$intvoto_2017_w02=="Sanchez")
elw$int_Goic02 <- as.numeric(elw$intvoto_2017_w02=="Goic")
elw$int_Otros02 <- as.numeric(elw$intvoto_2017_w02=="MEO"|elw$intvoto_2017_w02=="Parisi"|elw$intvoto_2017_w02=="Otro")
elw$int_NSNV02 <- as.numeric(elw$intvoto_2017_w02=="No sabe/No vota")
elw$int_NA02 <- as.numeric(elw$intvoto_2017_w02=="NS/NR")

table(elw$intvoto_2017_w02)
table(elw$int_Otros_w02)

#Voto retrospectivo en elección presidencial 2017
table(elw$c39_w03)
elw$voto_2017_03 <-factor(car::recode(elw$c39_w03, "9:10=9; -999:-888=10"),
                          labels=c("Goic", "Kast", "Pinera", "Guillier", "Sanchez", 
                                   "Meo", "Artes","Navarro","Blanco/Nulo", "NS/NR"))
table(elw$voto_2017_03)

elw$voto_Goic03 <- as.numeric(elw$voto_2017_03=="Goic")
elw$voto_Kast03 <- as.numeric(elw$voto_2017_03=="Kast")
elw$voto_Pinera03 <- as.numeric(elw$voto_2017_03=="Pinera")
elw$voto_Guillier03 <- as.numeric(elw$voto_2017_03=="Guillier")
elw$voto_Sanchez03 <- as.numeric(elw$voto_2017_03=="Sanchez")
elw$voto_Meo03 <- as.numeric(elw$voto_2017_03=="Meo")
elw$voto_Otros03 <- as.numeric(elw$voto_2017_03=="Artes"|elw$voto_2017_03=="Navarro")
elw$voto_ByN03 <- as.numeric(elw$voto_2017_03=="Blanco/Nulo")
elw$voto_NA03 <- as.numeric(elw$voto_2017_03=="NS/NR")

table(elw$c11_w03)

###---GRADO DE ACUERDO O EN DESACUERDO CON---###
#Una sociedad ideal requiere que algunos grupos estar en una posicion superior y 
#otros en una posicion inferior
elw$desigualdad_w01 <- recode(elw$c18_01_w01, "-999:-888=NA")
elw$desigualdad_w02 <- recode(elw$c18_01_w02, "-999:-888=NA")
elw$desigualdad_w03 <- recode(elw$c18_01_w03, "-999:-888=NA")
elw$desigualdad_w04 <- recode(elw$c18_01_w04, "-999:-888=NA")
elw$desigualdad_w05 <- NA
#Deberiamos	trabajar para dar a todos los grupos la misma oportunidad	de tener exito
elw$oportunidades_w01 <- recode(elw$c18_02_w01, "-999:-888=NA")
elw$oportunidades_w02 <- recode(elw$c18_02_w02, "-999:-888=NA")
elw$oportunidades_w03 <- recode(elw$c18_02_w03, "-999:-888=NA")
elw$oportunidades_w04 <- recode(elw$c18_02_w04, "-999:-888=NA")
elw$oportunidades_w05 <- NA
#Deberiamos hacer todo lo posible por igualar las condiciones de diferentes grupos
elw$igualitarismo_w01 <- recode(elw$c18_03_w01, "-999:-888=NA")
elw$igualitarismo_w02 <- recode(elw$c18_03_w02, "-999:-888=NA")
elw$igualitarismo_w03 <- recode(elw$c18_03_w03, "-999:-888=NA")
elw$igualitarismo_w04 <- recode(elw$c18_03_w04, "-999:-888=NA")
elw$igualitarismo_w05 <- NA
#En vez de tanta preocupacion por los derechos de las personas, lo que este 
#pais necesita es un gobierno firme
elw$gobfirme_w01 <- recode(elw$c18_04_w01, "-999:-888=NA")
elw$gobfirme_w02 <- recode(elw$c18_04_w02, "-999:-888=NA")
elw$gobfirme_w03 <- recode(elw$c18_04_w03, "-999:-888=NA")
elw$gobfirme_w04 <- recode(elw$c18_04_w04, "-999:-888=NA")
elw$gobfirme_w05 <- recode(elw$c18_04_w05, "-999:-666=NA")
table(elw$c18_04_w01)
#Lo	que	nuestro	pais necesita	es un	mandatario/a fuerte con la determinacion para 
#llevarnos por el camino correcto
elw$mandfuerte_w01 <- recode(elw$c18_05_w01, "-999:-888=NA")
elw$mandfuerte_w02 <- recode(elw$c18_05_w02, "-999:-888=NA")
elw$mandfuerte_w03 <- recode(elw$c18_05_w03, "-999:-888=NA")
elw$mandfuerte_w04 <- recode(elw$c18_05_w04, "-999:-888=NA")
elw$mandfuerte_w05 <- recode(elw$c18_05_w05, "-999:-666=NA")
#La	obediencia y el respeto por la autoridad son los valores mas importantes que los
#ninios debieran aprender
elw$obediencia_w01 <- recode(elw$c18_06_w01, "-999:-888=NA")
elw$obediencia_w02 <- recode(elw$c18_06_w02, "-999:-888=NA")
elw$obediencia_w03 <- recode(elw$c18_06_w03, "-999:-888=NA")
elw$obediencia_w04 <- recode(elw$c18_06_w04, "-999:-888=NA")
elw$obediencia_w05 <- recode(elw$c18_06_w05, "-999:-666=NA")
#Las verdaderas claves para tener	una	buena	vida son la obediencia y la disciplina
elw$disciplina_w01 <- recode(elw$c18_07_w01, "-999:-888=NA")
elw$disciplina_w02 <- recode(elw$c18_07_w02, "-999:-888=NA")
elw$disciplina_w03 <- recode(elw$c18_07_w03, "-999:-888=NA")
elw$disciplina_w04 <- recode(elw$c18_07_w04, "-999:-888=NA")
elw$disciplina_w05 <- recode(elw$c18_07_w05, "-999:-666=NA")

#High inter item correlation
cor(with(elw, cbind(gobfirme_w04, mandfuerte_w04, obediencia_w04, disciplina_w04)),
    use = "pairwise.complete.obs")
alpha(with(elw, cbind(gobfirme_w01, mandfuerte_w01, obediencia_w01, disciplina_w01)))
alpha(with(elw, cbind(gobfirme_w04, mandfuerte_w04, obediencia_w04, disciplina_w04)))

#Composite measures
elw$autori_w01 <- apply(with(elw, cbind(gobfirme_w01, mandfuerte_w01,
                                        obediencia_w01, disciplina_w01)),1, mean, na.rm=T)
elw$autori_w02 <- apply(with(elw, cbind(gobfirme_w02, mandfuerte_w02,
                                        obediencia_w02, disciplina_w02)),1, mean, na.rm=T)
elw$autori_w03 <- apply(with(elw, cbind(gobfirme_w03, mandfuerte_w03,
                                        obediencia_w03, disciplina_w03)),1, mean, na.rm=T)
elw$autori_w04 <- apply(with(elw, cbind(gobfirme_w04, mandfuerte_w04,
                                        obediencia_w04, disciplina_w04)),1, mean, na.rm=T)
elw$autori_w05 <- apply(with(elw, cbind(gobfirme_w05, mandfuerte_w05,
                                        obediencia_w05, disciplina_w05)),1, mean, na.rm=T)


#Creo	que	el cambio social es posible
elw$cambiosocial_w01 <- recode(elw$c18_08_w01, "-999:-888=NA")
elw$cambiosocial_w02 <- recode(elw$c18_08_w02, "-999:-888=NA")
elw$cambiosocial_w03 <- recode(elw$c18_08_w03, "-999:-888=NA")
elw$cambiosocial_w04 <- recode(elw$c18_08_w04, "-999:-888=NA")
elw$cambiosocial_w05 <- recode(elw$c18_08_w05, "-999:-666=NA")
#En Chile las personas son recompensadas por sus esfuerzos
elw$merito_w01 <- recode(elw$c18_09_w01, "-999:-888=NA")
elw$merito_w02 <- recode(elw$c18_09_w02, "-999:-888=NA")
elw$merito_w03 <- recode(elw$c18_09_w03, "-999:-888=NA")
elw$merito_w04 <- recode(elw$c18_09_w04, "-999:-888=NA")
elw$merito_w05 <- recode(elw$c18_09_w05, "-999:-666=NA")
#En	Chile las personas son recompensadas por su inteligencia y habilidades
elw$merito2_w01 <- recode(elw$c18_10_w01, "-999:-888=NA")
elw$merito2_w02 <- recode(elw$c18_10_w02, "-999:-888=NA")
elw$merito2_w03 <- recode(elw$c18_10_w03, "-999:-888=NA")
elw$merito2_w04 <- recode(elw$c18_10_w04, "-999:-888=NA")
elw$merito2_w05 <- recode(elw$c18_10_w05, "-999:-666=NA")
#En	Chile	las	diferencias	de	ingreso	son	demasiado	grandes
elw$difingreso_w01 <- recode(elw$c18_11_w01, "-999:-888=NA")
elw$difingreso_w02 <- recode(elw$c18_11_w02, "-999:-888=NA")
elw$difingreso_w03 <- recode(elw$c18_11_w03, "-999:-888=NA")
elw$difingreso_w04 <- recode(elw$c18_11_w04, "-999:-888=NA")
elw$difingreso_w05 <- recode(elw$c18_11_w05, "-999:-666=NA")

cor(with(elw, cbind(merito_w01, merito2_w01,
                    difingreso_w01)),use = "pairwise.complete.obs")

#Indice Meritocracia
elw$IndMerito_w01 <- apply(with(elw, cbind(merito_w01, merito2_w01)),1, mean, na.rm=T)
elw$IndMerito_w02 <- apply(with(elw, cbind(merito_w02, merito2_w02)),1, mean, na.rm=T)
elw$IndMerito_w03 <- apply(with(elw, cbind(merito_w03, merito2_w03)),1, mean, na.rm=T)
elw$IndMerito_w04 <- apply(with(elw, cbind(merito_w04, merito2_w04)),1, mean, na.rm=T)
elw$IndMerito_w05 <- apply(with(elw, cbind(merito_w05, merito2_w05)),1, mean, na.rm=T)

###Preferencia entre autoritarismo y democracia###
ld <- c("Democracia", "Gob. Autoritario", "Da lo mismo", "Ninguna")
elw$demoyauto_w01 <- factor(recode(elw$c25_w01, " -999:-888=NA"), labels=ld)
elw$demoyauto_w02 <- factor(recode(elw$c25_w02, " -999:-888=NA"), labels=ld)
elw$demoyauto_w03 <- factor(recode(elw$c25_w03, " -999:-888=NA"), labels=ld)
elw$demoyauto_w04 <- factor(recode(elw$c25_w04, " -999:-888=NA"), labels=ld)
elw$demoyauto_w05 <- factor(recode(elw$c25_w05, " -999:-666=NA"), labels=ld)

cor(elw$autori_w01, elw$demoyauto_w01=="Democracia", use="complete.obs")
cor(elw$autori_w02, elw$demoyauto_w02=="Gob. Autoritario", use="complete.obs")
cor(elw$autori_w03, elw$demoyauto_w03=="Gob. Autoritario", use="complete.obs")
cor(elw$autori_w04, elw$demoyauto_w04=="Gob. Autoritario", use="complete.obs")


###################    Valoración Movimientos Sociales   #######################

#Valoración movimientos sociales: Si/No
elw$val_movsoc_w01 <- car::recode(elw$c20_w01, "1:7=1; 11=1; 12=0; -999:-888=NA")
elw$val_movsoc_w02 <- car::recode(elw$c20_w02, "1:7=1; 11=1; 12=0; -999:-888=NA")
elw$val_movsoc_w03 <- car::recode(elw$c20_w03, "1:9=1; 11=1; 12=0; -999:-888=NA")
elw$val_movsoc_w04 <- car::recode(elw$c20_w04, "1:10=1; 11=1; 12=0; -999:-888=NA")
elw$val_movsoc_w05 <- car::recode(elw$c20_w05, "1:10=1; 11=1; 12=0; -999:-666=NA")

#Valoración mov sociales: Izquierda/Derecha
elw$mov_ideo_w01 <- car::recode(elw$c20_w01, "1:5='Movimientos Izquierda'; 6:7='Movimientos Derecha';
                                   11='Otros'; 12='Ninguno'; -999:-888='Ninguno'")
elw$mov_ideo_w02 <- car::recode(elw$c20_w02, "1:5='Movimientos Izquierda'; 6:7='Movimientos Derecha';
                                   11='Otros'; 12='Ninguno'; -999:-888='Ninguno'")
elw$mov_ideo_w03 <- car::recode(elw$c20_w03, "1:5='Movimientos Izquierda'; 6:7='Movimientos Derecha';
                                   8:9='Movimientos Izquierda'; 11='Otros'; 12='Ninguno'; -999:-888='Ninguno'")
elw$mov_ideo_w04 <- car::recode(elw$c20_w04, "1:5='Movimientos Izquierda'; 6:7='Movimientos Derecha';
                                   8:10='Movimientos Izquierda';11='Otros'; 12='Ninguno'; -999:-888='Ninguno'")
elw$mov_ideo_w05 <- car::recode(elw$c20_w05, "1:5='Movimientos Izquierda'; 6:7='Movimientos Derecha';
                                   8:10='Movimientos Izquierda'; 11='Otros'; 12='Ninguno'; -999:-666='Ninguno'")
#Codificación "Otros" en mov_ideo
elw$mov_otros_w01<- car::recode(elw$c20_otro_w01, "'Aborto'=1; 'Contra las afp'=1; 'De la igual de genero'=1;
                            'De los pensionados'=1; 'La afp'=1; ' Mejor salud'=1; 'Movimiento anti afp'=1;
                            'Movimiento derechos de las mujeres'=1; 'Movimiento mejora de sistema previsional'=1;
                            'Movimiento no mas afp'=1; 'Movimiento por aborto'=1; 
                            'Movimiento por causa de las pensiones de vejez'=1; 'Movimiento por eliminacion de afp'=1;
                            'Movimiento social anti afp'=1; 'Movimiento social no mas afp'=1; 
                            'Movimiento social por el respeto e igualdad a la mujer'=1; 'Ni una menos'=1;
                            'Ni una menos, antifemicidio'=1; 'Ni una menos, antifeminicidio'=1; 'No +afp'=1;
                            'No a las afp'=1; 'No mas afp'=1; 'Pensiones afp'=1; 'Pro aborto'=1; 
                            'Pro aborto, apoya al aborto de las 3 causas del parlamento'=1; 'Salud'=1; 
                            'Violencia de genero'=1; else=0")

elw$mov_otros_w02<- car::recode(elw$c20_otro_w02, "'Acceso a salud igualitaria'=1; 'Movimiento por tener el derecho a elegir'=1;
                            'Moviminto feminista'=1; 'Ni una menos'=1; 'No mas afp'=1; 
                            'Sistema salud publica mala atencion demasiada demanda'=1; else=0")

elw$mov_otros_w03<- car::recode(elw$c20_otro_w03, "'Cambio en la salud publica'=1; 
                                'Colegio de profesores de chile y metropolitano.'=1; 'Salud'=1; 
                                'Salud atencion y enfermedades raras'=1; 'Violencia a la mujer'=1; 'Indigenas'=1;
                                else=0")

elw$mov_otros_w04<- car::recode(elw$c20_otro_w04, "'Defensa del rio cautin, por la instalacion de represa.'=1;
                                'Movimiento cambio al sistema de salud'=1; 'Movimiento cuidadores'=1;
                                'Movimiento de mejora en la salud'=1; 'Movimiento social por el cambio del sistema de salud'=1;
                                'Movimiento social por la salud'=1; 'Por la salud'=1; 'Salud'=1; else=0")

elw$mov_otros_w05<- car::recode(elw$c20_otro_w05, "'Cambio constitucional'=1; 'Cambios en la salud, educaci?n y en las pensiones'=1;
                                'Comit?s de allegados por una vivienda digna'=1; 
                                'Movimiento de salud, que los tens ingresen al c?digo sanitario .'=1;
                                'Por la salud'=1; else=0")

elw$mov_ideo_w01=ifelse(elw$mov_otros_w01==1, 'Movimientos Izquierda', elw$mov_ideo_w01)
elw$mov_ideo_w02=ifelse(elw$mov_otros_w02==1, 'Movimientos Izquierda', elw$mov_ideo_w02)
elw$mov_ideo_w03=ifelse(elw$mov_otros_w03==1, 'Movimientos Izquierda', elw$mov_ideo_w03)
elw$mov_ideo_w04=ifelse(elw$mov_otros_w04==1, 'Movimientos Izquierda', elw$mov_ideo_w04)
elw$mov_ideo_w05=ifelse(elw$mov_otros_w05==1, 'Movimientos Izquierda', elw$mov_ideo_w05)

prop.table(table(elw$mov_ideo_w01))
prop.table(table(elw$mov_ideo_w02))
prop.table(table(elw$mov_ideo_w03))
prop.table(table(elw$mov_ideo_w04))
prop.table(table(elw$mov_ideo_w05))

#Valoracion mov sociales: solo de Izquierda
elw$mov_izq_w01 <- car::recode(elw$mov_ideo_w01, "'Movimientos Izquierda'=1; 'Movimientos Derecha'=0; 
                               'Otros'=0; 'Ninguno'=0")
elw$mov_izq_w02 <- car::recode(elw$mov_ideo_w02, "'Movimientos Izquierda'=1; 'Movimientos Derecha'=0; 
                               'Otros'=0; 'Ninguno'=0")
elw$mov_izq_w03 <- car::recode(elw$mov_ideo_w03, "'Movimientos Izquierda'=1; 'Movimientos Derecha'=0; 
                               'Otros'=0; 'Ninguno'=0")
elw$mov_izq_w04 <- car::recode(elw$mov_ideo_w04, "'Movimientos Izquierda'=1; 'Movimientos Derecha'=0; 
                               'Otros'=0; 'Ninguno'=0")
elw$mov_izq_w05 <- car::recode(elw$mov_ideo_w05, "'Movimientos Izquierda'=1; 'Movimientos Derecha'=0; 
                               'Otros'=0; 'Ninguno'=0")


## Pensando en el movimiento social que m?s valora (1=Totalmente desacuerdo, 5=Totalmente de acuerdo):
#Siento un compromiso con este movimiento
elw$compro_mov01_w01 <- car::recode(elw$c21_01_w01, "-999:-888=NA")
elw$compro_mov01_w02 <- car::recode(elw$c21_01_w02, "-999:-888=NA")
elw$compro_mov01_w03 <- car::recode(elw$c21_01_w03, "-999:-888=NA")
elw$compro_mov01_w04 <- car::recode(elw$c21_01_w04, "-999:-888=NA")
elw$compro_mov01_w05 <- NA
#Me identifico con este movimiento
elw$compro_mov02_w01 <- car::recode(elw$c21_02_w01, "-999:-888=NA")
elw$compro_mov02_w02 <- car::recode(elw$c21_02_w02, "-999:-888=NA")
elw$compro_mov02_w03 <- car::recode(elw$c21_02_w03, "-999:-888=NA")
elw$compro_mov02_w04 <- car::recode(elw$c21_02_w04, "-999:-888=NA")
elw$compro_mov02_w05 <- NA
#Estoy de acuerdo con las acciones de este movimiento
elw$compro_mov03_w01 <- car::recode(elw$c21_03_w01, "-999:-888=NA")
elw$compro_mov03_w02 <- car::recode(elw$c21_03_w02, "-999:-888=NA")
elw$compro_mov03_w03 <- car::recode(elw$c21_03_w03, "-999:-888=NA")
elw$compro_mov03_w04 <- car::recode(elw$c21_03_w04, "-999:-888=NA")
elw$compro_mov03_w05 <- NA
# Pensar acerca del futuro de este movimiento me hace sentir esperanzado
elw$compro_mov04_w01 <- car::recode(elw$c21_04_w01, "-999:-888=NA")
elw$compro_mov04_w02 <- car::recode(elw$c21_04_w02, "-999:-888=NA")
elw$compro_mov04_w03 <- car::recode(elw$c21_04_w03, "-999:-888=NA")
elw$compro_mov04_w04 <- car::recode(elw$c21_04_w04, "-999:-888=NA")
elw$compro_mov04_w05 <- NA
#Las acciones y protestas de este movimiento pueden generar un cambio social
elw$compro_mov05_w01 <- car::recode(elw$c21_05_w01, "-999:-888=NA")
elw$compro_mov05_w02 <- car::recode(elw$c21_05_w02, "-999:-888=NA")
elw$compro_mov05_w03 <- car::recode(elw$c21_05_w03, "-999:-888=NA")
elw$compro_mov05_w04 <- car::recode(elw$c21_05_w04, "-999:-888=NA")
elw$compro_mov05_w05 <- NA
#Los participantes de este movimiento me ven como un miembro m?s
elw$compro_mov06_w01 <- car::recode(elw$c21_06_w01, "-999:-888=NA")
elw$compro_mov06_w02 <- car::recode(elw$c21_06_w02, "-999:-888=NA")
elw$compro_mov06_w03 <- car::recode(elw$c21_06_w03, "-999:-888=NA")
elw$compro_mov06_w04 <- car::recode(elw$c21_06_w04, "-999:-888=NA")
elw$compro_mov06_w05 <- NA
#El prop?sito de este movimiento est? alineado con mis valores
elw$compro_mov07_w01 <- car::recode(elw$c21_07_w01, "-999:-888=NA")
elw$compro_mov07_w02 <- car::recode(elw$c21_07_w02, "-999:-888=NA")
elw$compro_mov07_w03 <- car::recode(elw$c21_07_w03, "-999:-888=NA")
elw$compro_mov07_w04 <- car::recode(elw$c21_07_w04, "-999:-888=NA")
elw$compro_mov07_w05 <- NA
#La gente en general y quienes pertenecen al movimiento tienen posiciones similares
elw$compro_mov08_w01 <- car::recode(elw$c21_08_w01, "-999:-888=NA")
elw$compro_mov08_w02 <- car::recode(elw$c21_08_w02, "-999:-888=NA")
elw$compro_mov08_w03 <- car::recode(elw$c21_08_w03, "-999:-888=NA")
elw$compro_mov08_w04 <- car::recode(elw$c21_08_w04, "-999:-888=NA")
elw$compro_mov08_w05 <- NA
#Las autoridades y el movimiento tienen posiciones opuestas
elw$compro_mov09_w01 <- car::recode(elw$c21_09_w01, "-999:-888=NA")
elw$compro_mov09_w02 <- car::recode(elw$c21_09_w02, "-999:-888=NA")
elw$compro_mov09_w03 <- car::recode(elw$c21_09_w03, "-999:-888=NA")
elw$compro_mov09_w04 <- car::recode(elw$c21_09_w04, "-999:-888=NA")
elw$compro_mov09_w05 <- NA
#Los participantes de este movimiento me apoyan
elw$compro_mov10_w01 <- car::recode(elw$c21_10_w01, "-999:-888=NA")
elw$compro_mov10_w02 <- car::recode(elw$c21_10_w02, "-999:-888=NA")
elw$compro_mov10_w03 <- car::recode(elw$c21_10_w03, "-999:-888=NA")
elw$compro_mov10_w04 <- car::recode(elw$c21_10_w04, "-999:-888=NA")
elw$compro_mov10_w05 <- NA
#Las pol?ticas de gobierno relacionadas con la causa de este movimiento son injustas
elw$compro_mov11_w01 <- car::recode(elw$c21_11_w01, "-999:-888=NA")
elw$compro_mov11_w02 <- car::recode(elw$c21_11_w02, "-999:-888=NA")
elw$compro_mov11_w03 <- car::recode(elw$c21_11_w03, "-999:-888=NA")
elw$compro_mov11_w04 <- car::recode(elw$c21_11_w04, "-999:-888=NA")
elw$compro_mov11_w05 <- NA

#Ahora,	pensando en los ùltimos 12 meses, cuán	frecuentemente ha participado USTED en
#[EL MOVIMIENTO	QUE	EL ENTREVISTADO M?S	VALORA]? (1=Nunca, 5=Muy Frecuentemente)
elw$mov_particip_w01 <-car::recode(elw$c22_w01, "-999:-888=NA")
elw$mov_particip_w02 <-car::recode(elw$c22_w02, "-999:-888=NA")
elw$mov_particip_w03 <-car::recode(elw$c22_w03, "-999:-888=NA")
elw$mov_particip_w04 <-car::recode(elw$c22_w04, "-999:-888=NA")
elw$mov_particip_w05 <-car::recode(elw$c22_w05, "-999:-888=NA")

#Pensando en los ?ltimos 12	meses, ?cu?n frecuentemente	han	participado	miembros de	
#SU	FAMILIA	en [EL MOVIMIENTO QUE EL ENTREVISTADO M?S VALORA]?
elw$mov_fam_w01 <-car::recode(elw$c23_w01, "-999:-888=NA")
elw$mov_fam_w02 <-car::recode(elw$c23_w02, "-999:-888=NA")
elw$mov_fam_w03 <-car::recode(elw$c23_w03, "-999:-888=NA")
elw$mov_fam_w04 <-car::recode(elw$c23_w04, "-999:-888=NA")
elw$mov_fam_w05 <-NA

#Pensando en los ?ltimos 12	meses, ?cu?n frecuentemente	han	participado	miembros de	
#SUS AMIGOS en [EL MOVIMIENTO QUE EL ENTREVISTADO M?S VALORA]?
elw$mov_amig_w01 <-car::recode(elw$c24_w01, "-999:-888=NA")
elw$mov_amig_w02 <-car::recode(elw$c24_w02, "-999:-888=NA")
elw$mov_amig_w03 <-car::recode(elw$c24_w03, "-999:-888=NA")
elw$mov_amig_w04 <-car::recode(elw$c24_w04, "-999:-888=NA")
elw$mov_amig_w05 <-NA

##########      NUEVA CONSTITUCION      ########## 
#Conformidad con la actual constitucion #solo ola 1 y 2
elw$conformconst_w01 <- recode(elw$c26_w01, "-999:-888=NA")
elw$conformconst_w02 <- recode(elw$c26_w02, "-999:-888=NA")
elw$conformconst_w03 <- NA
elw$conformconst_w04 <- recode(elw$c26_w04, "-999:-888=NA")
elw$conformconst_w05 <- NA
#Importancia del cambio constitucional #solo ola 1 y 2
elw$importcambio_w01 <- recode(elw$c27_w01, "-999:-888=NA")
elw$importcambio_w02 <- recode(elw$c27_w02, "-999:-888=NA")
elw$importcambio_w03 <- NA
elw$importcambio_w04 <- recode(elw$c27_w04, "-999:-888=NA")
elw$importcambio_w05 <- NA
#Grado de acuerdo con cambiar la const
elw$cambioconst_w01 <- recode(elw$c28_w01, "-999:-888=NA")
elw$cambioconst_w02 <- recode(elw$c28_w02, "-999:-888=NA")
elw$cambioconst_w03 <- NA
elw$cambioconst_w04 <- recode(elw$c28_w04, "-999:-888=NA")
elw$cambioconst_w05 <- NA
#Mecanismo de cambio de la Constitucion #solo ola 1 y 2
elw$mecanismoNC_w01 <- recode(elw$c29_w01, "-999:-888=NA")
elw$mecanismoNC_w02 <- recode(elw$c29_w02, "-999:-888=NA")
elw$mecanismoNC_w03 <- NA
elw$mecanismoNC_w04 <- recode(elw$c29_w04, "-999:-888=NA")
elw$mecanismoNC_w05 <- NA

######    IDENTIDAD NACIONAL    ######
#Me siento orgulloso de ser chileno
elw$orgullochileno_w01 <- recode(elw$c32_01_w01, "-999:-888=NA")
elw$orgullochileno_w02 <- recode(elw$c32_01_w02, "-999:-888=NA")
elw$orgullochileno_w03 <- recode(elw$c32_01_w03, "-999:-888=NA")
elw$orgullochileno_w04 <- recode(elw$c32_01_w04, "-999:-888=NA")
elw$orgullochileno_w05 <- recode(elw$c32_01_w05, "-999:-666=NA")
#Me identifico como chileno
elw$identichileno_w01 <- recode(elw$c32_02_w01, "-999:-888=NA")
elw$identichileno_w02 <- recode(elw$c32_02_w02, "-999:-888=NA")
elw$identichileno_w03 <- recode(elw$c32_02_w03, "-999:-888=NA")
elw$identichileno_w04 <- recode(elw$c32_02_w04, "-999:-888=NA")
elw$identichileno_w05 <- recode(elw$c32_02_w05, "-999:-666=NA")

cor(with(elw, cbind(orgullochileno_w01, identichileno_w01)),
    use = "pairwise.complete.obs")

#Indice Nacionalismo
elw$IdNacional_w01 <- apply(with(elw, cbind(orgullochileno_w01, 
                                            identichileno_w01)),1, mean, na.rm=T)
elw$IdNacional_w02 <- apply(with(elw, cbind(orgullochileno_w02, 
                                            identichileno_w02)),1, mean, na.rm=T)
elw$IdNacional_w03 <- apply(with(elw, cbind(orgullochileno_w03, 
                                            identichileno_w03)),1, mean, na.rm=T)
elw$IdNacional_w04 <- apply(with(elw, cbind(orgullochileno_w04, 
                                            identichileno_w04)),1, mean, na.rm=T)
elw$IdNacional_w05 <- apply(with(elw, cbind(orgullochileno_w05, 
                                            identichileno_w05)),1, mean, na.rm=T)

#######   Percepci?n subjetiva de clase social   ########
elw$clasesubj_w01 <- recode(elw$c33_w01, "-999:-888=NA")
elw$clasesubj_w02 <- NA
elw$clasesubj_w03 <- recode(elw$c33_w03, "-999:-888=NA")
elw$clasesubj_w04 <- recode(elw$c33_w04, "-999:-888=NA")
elw$clasesubj_w05 <- recode(elw$c33_w05, "-999:-666=NA")


############    Preferencias sobre ISSUES    #############

#Las parejas homosexuales deber?an poder adoptar hijos
elw$issue_homo_w01  <- NA
elw$issue_homo_w02  <- NA
elw$issue_homo_w03  <- recode(elw$c37_01_w03, "-999:-888=NA")
elw$issue_homo_w04  <- recode(elw$c37_01_w04, "-999:-888=NA")
elw$issue_homo_w05  <- NA
#El aborto debe ser legal bajo cualquier circunstancia
elw$issue_aborto_w01  <- NA
elw$issue_aborto_w02  <- NA
elw$issue_aborto_w03  <- recode(elw$c37_02_w03, "-999:-888=NA")
elw$issue_aborto_w04  <- recode(elw$c37_02_w04, "-999:-888=NA")
elw$issue_aborto_w05  <- recode(elw$c37_02_w05, "-999:-888=NA")
#El Estado de Chile, mas que los privados, deberia ser el principal proveedor de educacion
elw$issue_educ_w01  <- NA
elw$issue_educ_w02  <- NA
elw$issue_educ_w03  <- recode(elw$c37_03_w03, "-999:-888=NA")
elw$issue_educ_w04  <- recode(elw$c37_03_w04, "-999:-888=NA")
elw$issue_educ_w05  <- NA
#Cada persona debiera asegurarse por s? mismo su futura pension para la tercera edad
elw$issue_pension_w01  <- NA
elw$issue_pension_w02  <- NA
elw$issue_pension_w03  <- recode(elw$c37_04_w03, "-999:-888=NA")
elw$issue_pension_w04  <- recode(elw$c37_04_w04, "-999:-888=NA")
elw$issue_pension_w05  <- recode(elw$c37_04_w05, "-999:-666=NA")
#Chile deberia tomar medidas mas drasticas para impedir el ingreso de inmigrantes al pais
elw$issue_migra_w01  <- NA
elw$issue_migra_w02  <- NA
elw$issue_migra_w03  <- recode(elw$c37_05_w03, "-999:-888=NA")
elw$issue_migra_w04  <- recode(elw$c37_05_w04, "-999:-888=NA")
elw$issue_migra_w05  <- recode(elw$c37_05_w05, "-999:-666=NA")
#La educaci?n sexual de los ni?os deber?a ser responsabilidad exclusiva de los padres
elw$issue_edsex_w01  <- NA
elw$issue_edsex_w02  <- NA
elw$issue_edsex_w03  <- NA
elw$issue_edsex_w04  <- recode(elw$c37_06_w04, "-999:-888=NA")
elw$issue_edsex_w05  <- NA
#Se deber?an clausurar empresas contaminantes, incluso si esto implica un aumento en el desempleo
elw$issue_contam_w01  <- NA
elw$issue_contam_w02  <- NA
elw$issue_contam_w03  <- NA
elw$issue_contam_w04  <- recode(elw$c37_07_w04, "-999:-888=NA")
elw$issue_contam_w05  <- NA
#El gasto social debe destinarse unicamente a los mas pobres y vulnerables
elw$issue_gasto_w01  <- NA
elw$issue_gasto_w02  <- NA
elw$issue_gasto_w03  <- NA
elw$issue_gasto_w04  <- recode(elw$c37_08_w04, "-999:-888=NA")
elw$issue_gasto_w05  <- NA

#Percepcion de Corrupcion
elw$corrupcion_w01 <- NA
elw$corrupcion_w02 <- NA
elw$corrupcion_w03 <- recode(elw$c38_w03, "-999:-888=NA")
elw$corrupcion_w04 <- recode(elw$c38_w04, "-999:-888=NA")
elw$corrupcion_w05 <- recode(elw$c38_w05, "-999:-666=NA")
cor(with(elw, cbind(corrupcion_w03, corrupcion_w04, corrupcion_w05)), use="complete.obs")

###Percepcion del Estado
#Acuerdo con: El Estado trabaja por el bienestar de las personas
elw$estado_bien_w01 <- NA
elw$estado_bien_w02 <- NA
elw$estado_bien_w03 <- NA
elw$estado_bien_w04 <- recode(elw$c40_01_w04, "-999:-888=NA")
elw$estado_bien_w05 <- recode(elw$c40_01_w05, "-999:-666=NA")
#Acuerdo con: Las autoridades protegen a los vulnerables y débiles
elw$estado_vulve_w01 <- NA
elw$estado_vulve_w02 <- NA
elw$estado_vulve_w03 <- NA
elw$estado_vulve_w04 <- recode(elw$c40_06_w04, "-999:-888=NA")
elw$estado_vulve_w05 <- recode(elw$c40_06_w05, "-999:-666=NA")


########### SOCIABILITY ###########
#Frecuencia: Visito la casa de vecino
elw$VisiVeci_w01 <- recode(elw$c07_01_w01, "-999:-888=NA")
elw$VisiVeci_w02 <- recode(elw$c07_01_w02, "-999:-888=NA")
elw$VisiVeci_w03 <- recode(elw$c07_01_w03, "-999:-888=NA")
elw$VisiVeci_w04 <- recode(elw$c07_01_w04, "-999:-888=NA")
elw$VisiVeci_w05 <- recode(elw$c07_01_w05, "-999:-666=NA")
#Frecuencia: Asistio a reunion sobre temas de interes publico/comunitario
elw$ReunPubl_w01 <- recode(elw$c07_02_w01, "-999:-888=NA")
elw$ReunPubl_w02 <- recode(elw$c07_02_w02, "-999:-888=NA")
elw$ReunPubl_w03 <- recode(elw$c07_02_w03, "-999:-888=NA")
elw$ReunPubl_w04 <- recode(elw$c07_02_w04, "-999:-888=NA")
elw$ReunPubl_w05 <- recode(elw$c07_02_w05, "-999:-666=NA")
#Frecuencia: Amigos visitaron en su casa
elw$VisiAmig_w01 <- recode(elw$c07_03_w01, "-999:-888=NA")
elw$VisiAmig_w02 <- recode(elw$c07_03_w02, "-999:-888=NA")
elw$VisiAmig_w03 <- recode(elw$c07_03_w03, "-999:-888=NA")
elw$VisiAmig_w04 <- recode(elw$c07_03_w04, "-999:-888=NA")
elw$VisiAmig_w05 <- recode(elw$c07_03_w05, "-999:-666=NA")
#Frecuencia: Hizo voluntariado
elw$Volun_w01 <- recode(elw$c07_04_w01, "-999:-888=NA")
elw$Volun_w02 <- recode(elw$c07_04_w02, "-999:-888=NA")
elw$Volun_w03 <- recode(elw$c07_04_w03, "-999:-888=NA")
elw$Volun_w04 <- recode(elw$c07_04_w04, "-999:-888=NA")
elw$Volun_w05 <- recode(elw$c07_04_w05, "-999:-666=NA")
#Frecuencia: Dono dinero a caridad (2017)
elw$DonoDine_w01 <- recode(elw$c07_05_w01, "-999:-888=NA")
elw$DonoDine_w02 <- recode(elw$c07_05_w02, "-999:-888=NA")
elw$DonoDine_w03 <- recode(elw$c07_05_w03, "-999:-888=NA")
elw$DonoDine_w04 <- recode(elw$c07_05_w04, "-999:-888=NA")
elw$DonoDine_w05 <- recode(elw$c07_05_w05, "-999:-666=NA")
#Frecuencia: Presto 10,000 o mas
elw$PresLuca_w01 <- recode(elw$c07_06_w01, "-999:-888=NA")
elw$PresLuca_w02 <- recode(elw$c07_06_w02, "-999:-888=NA")
elw$PresLuca_w03 <- recode(elw$c07_06_w03, "-999:-888=NA")
elw$PresLuca_w04 <- recode(elw$c07_06_w04, "-999:-888=NA")
elw$PresLuca_w05 <- recode(elw$c07_06_w05, "-999:-666=NA")
#Frecuencia: Converso con persona en problemas o deprimida
elw$ConvDepr_w01 <- recode(elw$c07_07_w01, "-999:-888=NA")
elw$ConvDepr_w02 <- recode(elw$c07_07_w02, "-999:-888=NA")
elw$ConvDepr_w03 <- recode(elw$c07_07_w03, "-999:-888=NA")
elw$ConvDepr_w04 <- recode(elw$c07_07_w04, "-999:-888=NA")
elw$ConvDepr_w05 <- recode(elw$c07_07_w05, "-999:-666=NA")
#Frecuencia: Ayudo a alguien a conseguir trabajo
elw$AyudPega_w01 <- recode(elw$c07_08_w01, "-999:-888=NA")
elw$AyudPega_w02 <- recode(elw$c07_08_w02, "-999:-888=NA")
elw$AyudPega_w03 <- recode(elw$c07_08_w03, "-999:-888=NA")
elw$AyudPega_w04 <- recode(elw$c07_08_w04, "-999:-888=NA")
elw$AyudPega_w05 <- recode(elw$c07_08_w05, "-999:-666=NA")
#Vida Social
elw$sociabi_w01 <- apply(cbind(elw$VisiAmig_w01, elw$VisiVeci_w01), 1, mean, na.rm=T)
elw$sociabi_w02 <- apply(cbind(elw$VisiAmig_w02, elw$VisiVeci_w02), 1, mean, na.rm=T)
elw$sociabi_w03 <- apply(cbind(elw$VisiAmig_w03, elw$VisiVeci_w03), 1, mean, na.rm=T)
elw$sociabi_w04 <- apply(cbind(elw$VisiAmig_w04, elw$VisiVeci_w04), 1, mean, na.rm=T)
elw$sociabi_w05 <- apply(cbind(elw$VisiAmig_w05, elw$VisiVeci_w04), 1, mean, na.rm=T)


#Participacion: Junta de vecinos
elw$part_jjvv_w01 <- recode(elw$c12_01_w01, "1=0; 2:3=1; -999:-888=NA")
elw$part_jjvv_w02 <- NA
elw$part_jjvv_w03 <- recode(elw$c12_01_w03, "1=0; 2:3=1; -999:-888=NA")
elw$part_jjvv_w04 <- NA
elw$part_jjvv_w05 <- NA
#Participacion: Organizacion religiosa
elw$part_relig_w01 <- recode(elw$c12_02_w01, "1=0; 2:3=1; -999:-888=NA")
elw$part_relig_w02 <- NA
elw$part_relig_w03 <- recode(elw$c12_02_w03, "1=0; 2:3=1; -999:-888=NA")
elw$part_relig_w04 <- NA
elw$part_relig_w05 <- NA
#Participacion: Sindicato
elw$part_sindic_w01 <- recode(elw$c12_04_w01, "1=0; 2:3=1; -999:-888=NA")
elw$part_sindic_w02 <- NA
elw$part_sindic_w03 <- recode(elw$c12_04_w03, "1=0; 2:3=1; -999:-888=NA")
elw$part_sindic_w04 <- NA
elw$part_sindic_w05 <- NA
#Participacion: Asociacion gremial
elw$part_gremial_w01 <- recode(elw$c12_05_w01, "1=0; 2:3=1; -999:-888=NA")
elw$part_gremial_w02 <- NA
elw$part_gremial_w03 <- recode(elw$c12_05_w03, "1=0; 2:3=1; -999:-888=NA")
elw$part_gremial_w04 <- NA
elw$part_gremial_w05 <- NA
#Participacion: Beneficencia
elw$part_benefic_w01 <- recode(elw$c12_06_w01, "1=0; 2:3=1; -999:-888=NA")
elw$part_benefic_w02 <- NA
elw$part_benefic_w03 <- recode(elw$c12_06_w03, "1=0; 2:3=1; -999:-888=NA")
elw$part_benefic_w04 <- NA
elw$part_benefic_w05 <- NA
#Participacion: Deportiva
elw$part_deport_w01 <- recode(elw$c12_07_w01, "1=0; 2:3=1; -999:-888=NA")
elw$part_deport_w02 <- NA
elw$part_deport_w03 <- recode(elw$c12_07_w03, "1=0; 2:3=1; -999:-888=NA")
elw$part_deport_w04 <- NA
elw$part_deport_w05 <- NA
#Participacion: Estudiantil
elw$part_estud_w01 <- recode(elw$c12_08_w01, "1=0; 2:3=1; -999:-888=NA")
elw$part_estud_w02 <- NA
elw$part_estud_w03 <- recode(elw$c12_08_w03, "1=0; 2:3=1; -999:-888=NA")
elw$part_estud_w04 <- NA
elw$part_estud_w05 <- NA
#Participacion: Otra
elw$part_otra_w01 <- recode(elw$c12_09_w01, "1=0; 2:3=1; -999:-888=NA")
elw$part_otra_w02 <- NA
elw$part_otra_w03 <- recode(elw$c12_09_w03, "1=0; 2:3=1; -999:-888=NA")
elw$part_otra_w04 <- NA
elw$part_otra_w05 <- NA


#################     INEQUALITY AND LEGITIMACY    ##########################

###ESTATUS SOCIAL SUBJETIVO###
elw$ess_w01 <- recode(elw$d01_01_w01, "-999:-888=NA")
elw$ess_w02 <- recode(elw$d01_01_w02, "-999:-888=NA")
elw$ess_w03 <- recode(elw$d01_01_w03, "-999:-888=NA")
elw$ess_w04 <- recode(elw$d01_01_w04, "-999:-888=NA")
elw$ess_w05 <- recode(elw$d01_01_w05, "-999:-666=NA")

#Justicia distributiva en pensiones
elw$jdpensiones_w01 <- recode(elw$d02_01_w01, "-999:-888=NA")
elw$jdpensiones_w02 <- recode(elw$d02_01_w02, "-999:-888=NA")
elw$jdpensiones_w03 <- recode(elw$d02_01_w03, "-999:-888=NA")
elw$jdpensiones_w04 <- recode(elw$d02_01_w04, "-999:-888=NA")
elw$jdpensiones_w05 <- NA 
#Justicia distributiva en educacion
elw$jdeducacion_w01 <- recode(elw$d02_02_w01, "-999:-888=NA")
elw$jdeducacion_w02 <- recode(elw$d02_02_w02, "-999:-888=NA")
elw$jdeducacion_w03 <- recode(elw$d02_02_w03, "-999:-888=NA")
elw$jdeducacion_w04 <- recode(elw$d02_02_w04, "-999:-888=NA")
elw$jdeducacion_w05 <- NA
#Justicia distributiva en salud
elw$jdsalud_w01 <- recode(elw$d02_03_w01, "-999:-888=NA")
elw$jdsalud_w02 <- recode(elw$d02_03_w02, "-999:-888=NA")
elw$jdsalud_w03 <- recode(elw$d02_03_w03, "-999:-888=NA")
elw$jdsalud_w04 <- recode(elw$d02_03_w04, "-999:-888=NA")
elw$jdsalud_w05 <- NA
alpha(with(elw, cbind(jdpensiones_w01, jdeducacion_w01, jdsalud_w01)))
alpha(with(elw, cbind(jdpensiones_w02, jdeducacion_w02, jdsalud_w02)))
alpha(with(elw, cbind(jdpensiones_w03, jdeducacion_w03, jdsalud_w03)))
alpha(with(elw, cbind(jdpensiones_w04, jdeducacion_w04, jdsalud_w04)))

#Indice Justicia Distributiva
elw$justdist_w01 <- apply(with(elw, cbind(jdpensiones_w01, jdeducacion_w01, jdsalud_w01)), 
                          1, mean, na.rm=T)
elw$justdist_w02 <- apply(with(elw, cbind(jdpensiones_w02, jdeducacion_w02, jdsalud_w02)), 
                          1, mean, na.rm=T)
elw$justdist_w03 <- apply(with(elw, cbind(jdpensiones_w03, jdeducacion_w03, jdsalud_w03)), 
                          1, mean, na.rm=T)
elw$justdist_w04 <- apply(with(elw, cbind(jdpensiones_w04, jdeducacion_w04, jdsalud_w04)), 
                          1, mean, na.rm=T)

##### Actualmente en Chile, ?cu?n importante es para surgir en la vida? #####
#Provenir de una familia rica o con muchos recursos
elw$famrica_w01 <- recode(elw$d05_01_w01, "-999:-888=NA")
elw$famrica_w02 <- recode(elw$d05_01_w02, "-999:-888=NA")
elw$famrica_w03 <- recode(elw$d05_01_w03, "-999:-888=NA")
elw$famrica_w04 <- recode(elw$d05_01_w04, "-999:-888=NA")
elw$famrica_w05 <- recode(elw$d05_01_w05, "-999:-666=NA")
#Tener un buen nivel de educaci?n
elw$bueneduc_w01 <- recode(elw$d05_02_w01, "-999:-888=NA")
elw$bueneduc_w02 <- recode(elw$d05_02_w02, "-999:-888=NA")
elw$bueneduc_w03 <- recode(elw$d05_02_w03, "-999:-888=NA")
elw$bueneduc_w04 <- recode(elw$d05_02_w04, "-999:-888=NA")
elw$bueneduc_w05 <- recode(elw$d05_02_w05, "-999:-666=NA")
#Tener ambici?n
elw$ambicion_w01 <- recode(elw$d05_03_w01, "-999:-888=NA")
elw$ambicion_w02 <- recode(elw$d05_03_w02, "-999:-888=NA")
elw$ambicion_w03 <- recode(elw$d05_03_w03, "-999:-888=NA")
elw$ambicion_w04 <- recode(elw$d05_03_w04, "-999:-888=NA")
elw$ambicion_w05 <- recode(elw$d05_03_w05, "-999:-666=NA")
#El	trabajo	duro
elw$trabduro_w01 <- recode(elw$d05_04_w01, "-999:-888=NA")
elw$trabduro_w02 <- recode(elw$d05_04_w02, "-999:-888=NA")
elw$trabduro_w03 <- recode(elw$d05_04_w03, "-999:-888=NA")
elw$trabduro_w04 <- recode(elw$d05_04_w04, "-999:-888=NA")
elw$trabduro_w05 <- recode(elw$d05_04_w05, "-999:-666=NA")

#########   Pago poco o muchos Impuestos    ##########
elw$paga_imp_w01 <- recode(elw$d06_w01, "-999:-888=NA")
elw$paga_imp_w02 <- recode(elw$d06_w02, "-999:-888=NA")
elw$paga_imp_w03 <- recode(elw$d06_w03, "-999:-888=NA")
elw$paga_imp_w04 <- recode(elw$d06_w04, "-999:-888=NA")
elw$paga_imp_w05 <- recode(elw$d06_w05, "-999:-666=NA")


#######    Trato respuetuoso   ########
#Trato respetuoso a su clase en salud
elw$tratosalud_w01 <- recode(elw$c35_01_w01, "-999:-888=NA")
elw$tratosalud_w02 <- NA
elw$tratosalud_w03 <- recode(elw$c35_01_w03, "-999:-888=NA")
elw$tratosalud_w04 <- recode(elw$c35_01_w04, "-999:-888=NA")
elw$tratosalud_w05 <- recode(elw$c35_01_w05, "-999:-888=NA")
#Trato respetuoso a su clase en trabajo
elw$tratotrabajo_w01 <- recode(elw$c35_02_w01, "-999:-888=NA")
elw$tratotrabajo_w02 <- NA
elw$tratotrabajo_w03 <- recode(elw$c35_02_w03, "-999:-888=NA")
elw$tratotrabajo_w04 <- recode(elw$c35_02_w04, "-999:-888=NA")
elw$tratotrabajo_w05 <- recode(elw$c35_02_w05, "-999:-888=NA")
#Trato respetuoso a su clase por Carabineros
elw$tratocarabin_w01 <- recode(elw$c35_03_w01, "-999:-888=NA")
elw$tratocarabin_w02 <- NA
elw$tratocarabin_w03 <- recode(elw$c35_03_w03, "-999:-888=NA")
elw$tratocarabin_w04 <- recode(elw$c35_03_w04, "-999:-888=NA")
elw$tratocarabin_w05 <- recode(elw$c35_03_w05, "-999:-888=NA")
#Trato respetuoso a su clase por clase alta
elw$tratoclasealta_w01 <- recode(elw$c35_04_w01, "-999:-888=NA")
elw$tratoclasealta_w02 <- NA
elw$tratoclasealta_w03 <- recode(elw$c35_04_w03, "-999:-888=NA")
elw$tratoclasealta_w04 <- recode(elw$c35_04_w04, "-999:-888=NA")
elw$tratoclasealta_w05 <- recode(elw$c35_04_w05, "-999:-888=NA")


############################   SOCIAL CONFLICT  ###############################

############   Justificacion de la violencia   ###########
#Perseguir y golpear delincuentes
elw$ViolenDelin_w01 <- recode(elw$f05_01_w01, "-999:-888=NA")
elw$ViolenDelin_w02 <- recode(elw$f05_01_w02, "-999:-888=NA")
elw$ViolenDelin_w03 <- recode(elw$f05_01_w03, "-999:-888=NA")
elw$ViolenDelin_w04 <- recode(elw$f05_01_w04, "-999:-888=NA")
elw$ViolenDelin_w04 <- recode(elw$f05_01_w04, "-999:-888=NA")
elw$ViolenDelin_w05 <- NA
#Amarrar delincuente en la calle
elw$AmarrarDelin_w01 <- recode(elw$f05_02_w01, "-999:-888=NA")
elw$AmarrarDelin_w02 <- recode(elw$f05_02_w02, "-999:-888=NA")
elw$AmarrarDelin_w03 <- recode(elw$f05_02_w03, "-999:-888=NA")
elw$AmarrarDelin_w04 <- recode(elw$f05_02_w04, "-999:-888=NA")
elw$AmarrarDelin_w05 <- NA
#Carabineros reprima marchas                                    
elw$RepresionCarab_w01 <- recode(elw$f05_03_w01, "-999:-888=3")#NA codificados con 3
elw$RepresionCarab_w02 <- recode(elw$f05_03_w02, "-999:-888=3")
elw$RepresionCarab_w03 <- recode(elw$f05_03_w03, "-999:-888=3")
elw$RepresionCarab_w04 <- recode(elw$f05_03_w04, "-999:-888=3")
elw$RepresionCarab_w05 <- recode(elw$f05_03_w05, "-999:-888=3")
table(elw$f05_03_w05)
#Carabineros desaloje tomas liceos                             
elw$DesalojoCarab_w01 <- recode(elw$f05_04_w01, "-999:-888=3")#NA codificados con 3
elw$DesalojoCarab_w02 <- recode(elw$f05_04_w02, "-999:-888=3")
elw$DesalojoCarab_w03 <- recode(elw$f05_04_w03, "-999:-888=3")
elw$DesalojoCarab_w04 <- recode(elw$f05_04_w04, "-999:-888=3")
elw$DesalojoCarab_w05 <- recode(elw$f05_04_w05, "-999:-888=3")
#Marido golpee a esposa 
elw$MaridoGolpes_w01 <- recode(elw$f05_05_w01, "-999:-888=NA")
elw$MaridoGolpes_w02 <- recode(elw$f05_05_w02, "-999:-888=NA")
elw$MaridoGolpes_w03 <- recode(elw$f05_05_w03, "-999:-888=NA")
elw$MaridoGolpes_w04 <- recode(elw$f05_05_w04, "-999:-888=NA")
elw$MaridoGolpes_w05 <- NA
#Trabajadores bloqueen calle                                    #todas olas
elw$TrabajBloqueo_w01 <- recode(elw$f05_06_w01, "-999:-888=NA") 
elw$TrabajBloqueo_w02 <- recode(elw$f05_06_w02, "-999:-888=NA")
elw$TrabajBloqueo_w03 <- recode(elw$f05_06_w03, "-999:-888=NA")
elw$TrabajBloqueo_w04 <- recode(elw$f05_06_w04, "-999:-888=NA")
elw$TrabajBloqueo_w05 <- recode(elw$f05_06_w05, "-999:-888=NA")
#Estudiantes apedreen Carabineros                               #todas olas
elw$PiedrasCarab_w01 <- recode(elw$f05_07_w01, "-999:-888=NA") 
elw$PiedrasCarab_w02 <- recode(elw$f05_07_w02, "-999:-888=NA")
elw$PiedrasCarab_w03 <- recode(elw$f05_07_w03, "-999:-888=NA")
elw$PiedrasCarab_w04 <- recode(elw$f05_07_w04, "-999:-888=NA")
elw$PiedrasCarab_w05 <- recode(elw$f05_07_w05, "-999:-888=NA")

cor(with(elw, data.frame(PiedrasCarab_w01, PiedrasCarab_w02, 
                        PiedrasCarab_w03, PiedrasCarab_w04,
                        PiedrasCarab_w05)), use="complete.obs")

#Estudiantes apedreen Carabineros recode
elw$PiedrasCarabR_w01 <- recode(elw$f05_07_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$PiedrasCarabR_w02 <- recode(elw$f05_07_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$PiedrasCarabR_w03 <- recode(elw$f05_07_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$PiedrasCarabR_w04 <- recode(elw$f05_07_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
elw$PiedrasCarabR_w05 <- recode(elw$f05_07_w05, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
cor(with(elw, data.frame(PiedrasCarab_w01, PiedrasCarab_w02, PiedrasCarab_w03,
                        PiedrasCarab_w04, PiedrasCarab_w05)), use="complete.obs")

#Atacar a travestis
elw$AtaqueTraves_w01 <- recode(elw$f05_08_w01, "-999:-888=NA")
elw$AtaqueTraves_w02 <- recode(elw$f05_08_w02, "-999:-888=NA")
elw$AtaqueTraves_w03 <- NA
elw$AtaqueTraves_w04 <- NA
elw$AtaqueTraves_w05 <- NA

#Indice Justificación violencia de Manifestantes
elw$ViolManif_w01 <- apply(with(elw, cbind(TrabajBloqueo_w01, PiedrasCarab_w01)), 
                          1, mean, na.rm=T)
elw$ViolManif_w02 <- apply(with(elw, cbind(TrabajBloqueo_w02, PiedrasCarab_w02)), 
                           1, mean, na.rm=T)
elw$ViolManif_w03 <- apply(with(elw, cbind(TrabajBloqueo_w03, PiedrasCarab_w03)), 
                           1, mean, na.rm=T)
elw$ViolManif_w04 <- apply(with(elw, cbind(TrabajBloqueo_w04, PiedrasCarab_w04)), 
                           1, mean, na.rm=T)
elw$ViolManif_w05 <- apply(with(elw, cbind(TrabajBloqueo_w05, PiedrasCarab_w05)), 
                           1, mean, na.rm=T)
cor(with(elw, data.frame(ViolManif_w01, ViolManif_w02, ViolManif_w03,
                         ViolManif_w04, ViolManif_w05)), use="complete.obs")

#Indice Justificación violencia de Carabineros
elw$ViolCarab_w01 <- apply(with(elw, cbind(RepresionCarab_w01, DesalojoCarab_w01)), 
                           1, mean, na.rm=T)
elw$ViolCarab_w02 <- apply(with(elw, cbind(RepresionCarab_w02, DesalojoCarab_w02)), 
                           1, mean, na.rm=T)
elw$ViolCarab_w03 <- apply(with(elw, cbind(RepresionCarab_w03, DesalojoCarab_w03)), 
                           1, mean, na.rm=T)
elw$ViolCarab_w04 <- apply(with(elw, cbind(RepresionCarab_w04, DesalojoCarab_w04)), 
                           1, mean, na.rm=T)
elw$ViolCarab_w05 <- apply(with(elw, cbind(RepresionCarab_w05, DesalojoCarab_w05)), 
                           1, mean, na.rm=T)
cor(with(elw, data.frame(ViolCarab_w01, ViolCarab_w02, ViolCarab_w03,
                         ViolCarab_w04, ViolCarab_w05)), use="complete.obs")


##############################    TERRITORY    ################################

#En términos generales, cuánto confía usted en sus vecinos? 
elw$confvecinos_w01 <- recode(elw$t01_w01, "-999:-888=NA")
elw$confvecinos_w02 <- recode(elw$t01_w02, "-999:-888=NA")
elw$confvecinos_w03 <- recode(elw$t01_w03, "-999:-888=NA")
elw$confvecinos_w04 <- recode(elw$t01_w04, "-999:-888=NA")
elw$confvecinos_w05 <- recode(elw$t01_w05, "-999:-666=NA")

##Apego al barrio##
#Grado de acuerdo con: Este barrio es ideal para m?
elw$barrioideal_w01 <- recode(elw$t02_01_w01, "-999:-888=NA")
elw$barrioideal_w02 <- recode(elw$t02_01_w02, "-999:-888=NA")
elw$barrioideal_w03 <- recode(elw$t02_01_w03, "-999:-888=NA")
elw$barrioideal_w04 <- recode(elw$t02_01_w04, "-999:-888=NA")
elw$barrioideal_w05 <- NA
#Grado de acuerdo: Me siento integrado/a en este barrio
elw$integbarrio_w01 <- recode(elw$t02_02_w01, "-999:-888=NA")
elw$integbarrio_w02 <- recode(elw$t02_02_w02, "-999:-888=NA")
elw$integbarrio_w03 <- recode(elw$t02_02_w03, "-999:-888=NA")
elw$integbarrio_w04 <- recode(elw$t02_02_w04, "-999:-888=NA")
elw$integbarrio_w05 <- NA
#Grado de acuerdo: Me identifico con la gente de este barrio
elw$identbarrio_w01 <- recode(elw$t02_03_w01, "-999:-888=NA")
elw$identbarrio_w02 <- recode(elw$t02_03_w02, "-999:-888=NA")
elw$identbarrio_w03 <- recode(elw$t02_03_w03, "-999:-888=NA")
elw$identbarrio_w04 <- recode(elw$t02_03_w04, "-999:-888=NA")
elw$identbarrio_w05 <- NA
#Grado de acuerdo: Este barrio es parte de mi
elw$partebarrio_w01 <- recode(elw$t02_04_w01, "-999:-888=NA")
elw$partebarrio_w02 <- recode(elw$t02_04_w02, "-999:-888=NA")
elw$partebarrio_w03 <- recode(elw$t02_04_w03, "-999:-888=NA")
elw$partebarrio_w04 <- recode(elw$t02_04_w04, "-999:-888=NA")
elw$partebarrio_w05 <- NA

elw$apego_barrio_w01 <-(elw$barrioideal_w01 + elw$integbarrio_w01 + 
                           elw$identbarrio_w01 + elw$partebarrio_w01)
elw$apego_barrio_w02 <-(elw$barrioideal_w02 + elw$integbarrio_w02 + 
                           elw$identbarrio_w02 + elw$partebarrio_w02)
elw$apego_barrio_w03 <-(elw$barrioideal_w03 + elw$integbarrio_w03 + 
                           elw$identbarrio_w03 + elw$partebarrio_w03)
elw$apego_barrio_w04 <-(elw$barrioideal_w04 + elw$integbarrio_w04 + 
                           elw$identbarrio_w04 + elw$partebarrio_w04)

#####  Satisfaccion con el barrio   #####
#Seguridad en el barrio
elw$segurobarrio_w01 <- recode(elw$t06_01_w01, "-999:-888=NA")
elw$segurobarrio_w02 <- recode(elw$t06_01_w02, "-999:-888=NA")
elw$segurobarrio_w03 <- recode(elw$t06_01_w03, "-999:-888=NA")
elw$segurobarrio_w04 <- recode(elw$t06_01_w04, "-999:-888=NA")
elw$segurobarrio_w05 <- NA
#Conectividad del barrio
elw$conectbarrio_w01 <- recode(elw$t06_02_w01, "-999:-888=NA")
elw$conectbarrio_w02 <- recode(elw$t06_02_w02, "-999:-888=NA")
elw$conectbarrio_w03 <- recode(elw$t06_02_w03, "-999:-888=NA")
elw$conectbarrio_w04 <- recode(elw$t06_02_w04, "-999:-888=NA")
elw$conectbarrio_w05 <- NA
#Areas verdes y de recreacion disponibles
elw$recreabarrio_w01 <- recode(elw$t06_03_w01, "-999:-888=NA")
elw$recreabarrio_w02 <- recode(elw$t06_03_w02, "-999:-888=NA")
elw$recreabarrio_w03 <- recode(elw$t06_03_w03, "-999:-888=NA")
elw$recreabarrio_w04 <- recode(elw$t06_03_w04, "-999:-888=NA")
elw$recreabarrio_w05 <- NA
#Limpieza y belleza del barrio
elw$limpiezabarrio_w01 <- recode(elw$t06_04_w01, "-999:-888=NA")
elw$limpiezabarrio_w02 <- recode(elw$t06_04_w02, "-999:-888=NA")
elw$limpiezabarrio_w03 <- recode(elw$t06_04_w03, "-999:-888=NA")
elw$limpiezabarrio_w04 <- recode(elw$t06_04_w04, "-999:-888=NA")
elw$limpiezabarrio_w05 <- NA
#Proximidad al lugar de actividad principal
elw$proximbarrio_w01 <- recode(elw$t06_05_w01, "-999:-888=NA")
elw$proximbarrio_w02 <- recode(elw$t06_05_w02, "-999:-888=NA")
elw$proximbarrio_w03 <- recode(elw$t06_05_w03, "-999:-888=NA")
elw$proximbarrio_w04 <- recode(elw$t06_05_w04, "-999:-888=NA")
elw$proximbarrio_w05 <- NA
#Proximidad a colegios de buena calidad
elw$colegiobarrio_w01 <- recode(elw$t06_06_w01, "-999:-888=NA")
elw$colegiobarrio_w02 <- recode(elw$t06_06_w02, "-999:-888=NA")
elw$colegiobarrio_w03 <- recode(elw$t06_06_w03, "-999:-888=NA")
elw$colegiobarrio_w04 <- recode(elw$t06_06_w04, "-999:-888=NA")
elw$colegiobarrio_w05 <- NA 
#Proximidad a comercios
elw$comerciobarrio_w01 <- recode(elw$t06_07_w01, "-999:-888=NA")
elw$comerciobarrio_w02 <- recode(elw$t06_07_w02, "-999:-888=NA")
elw$comerciobarrio_w03 <- recode(elw$t06_07_w03, "-999:-888=NA")
elw$comerciobarrio_w04 <- recode(elw$t06_07_w04, "-999:-888=NA")
elw$comerciobarrio_w05 <- NA
#Proximidad a amigos y familiares
elw$redesbarrio_w01 <- recode(elw$t06_08_w01, "-999:-888=NA")
elw$redesbarrio_w02 <- recode(elw$t06_08_w02, "-999:-888=NA")
elw$redesbarrio_w03 <- recode(elw$t06_08_w03, "-999:-888=NA")
elw$redesbarrio_w04 <- recode(elw$t06_08_w04, "-999:-888=NA")
elw$redesbarrio_w05 <- NA

#Crear variables de fechas de entrevistas
elw$fecha_w01 <- as.Date(as.character(paste(elw$dia_entr_w01, elw$mes_entr_w01, 
                                           elw$annio_entr_w01, sep = "/")),
                        "%d/%m/%Y")
elw$fecha_w02 <- as.Date(as.character(paste(elw$dia_entr_w02, elw$mes_entr_w02, 
                                           elw$annio_entr_w02, sep = "/")),
                        "%d/%m/%Y")
elw$fecha_w03 <- as.Date(as.character(paste(elw$dia_entr_w03, elw$mes_entr_w03, 
                                           elw$annio_entr_w03, sep = "/")),
                        "%d/%m/%Y")
elw$fecha_w04 <- as.Date(as.character(paste(elw$dia_entr_w04, elw$mes_entr_w04, 
                                           elw$annio_entr_w04, sep = "/")),
                        "%d/%m/%Y")
elw$fecha_w05 <- as.Date(as.character(paste(elw$dia_entr_w05, elw$mes_entr_w05, 
                                            elw$annio_entr_w05, sep = "/")),
                         "%d/%m/%Y")



##------------ Save in wide format ----------------##
save(elw, file="Elsoc_Wide_2016_2021_REC.Rdata")
   

##----------- Save in long format -----------------##

#Preparativos - extraer nombres de variables y borrar 
#variables que no tienen 5 olas.
keep_long <- c(names(elw)[which(names(elw)=="hombre_w01"):dim(elw)[2]],
               paste0("region_w0", 1:5), paste0("region_cod_w0", 1:5),
               paste0("comuna_w0", 1:5), paste0("comuna_cod_w0", 1:5),
               paste0("estrato_w0", 1:5), paste0("segmento_w0", 1:5), 
               paste0("ponderador01_w0", 1:5), paste0("ponderador02_w0", 1:5),
               paste0("dia_entr_w0", 1:5), paste0("mes_entr_w0", 1:5),
               paste0("annio_entr_w0", 1:5))
keep_long
keep_long <- data.frame(keep_long)
keep_long$varnames <- substr(keep_long$keep_long, 1, 
                             nchar(as.character(keep_long$keep_long))-4)
keep_long$len <- tapply(keep_long$varnames, keep_long$varnames, 
                        length)[keep_long$varnames]
keep_long <- subset(keep_long, len>=1)
keep_long$keep_long <- as.character(factor(keep_long$keep_long, exclude=NULL))
keep_long$varnames <- as.character(factor(keep_long$varnames, exclude=NULL))

##Reshape Data into Long format
adm_vars <- c("idencuesta", "muestra", "tipo_atricion", "tipo_caso")
tmp <- elw[, names(elw) %in% c(adm_vars, keep_long$keep_long)]

#ell <- reshape(tmp, direction="long",
#               varying=split(as.character(keep_long$keep_long), keep_long$varnames),
#               v.names = sort(unique(as.character(keep_long$varnames))))  #no me funciona este c?digo

ell <-long_panel(data = tmp, prefix = "_w0", begin = 1, end = 5, label_location = "end",
                  id = "idencuesta", wave = "time")

dim(ell)
ell <- ell[order(ell$idencuesta), ]
head(ell, 10)
names(ell)
elw$int
#Save
save(ell, file="Elsoc_Long_2016_2021_REC.Rdata")