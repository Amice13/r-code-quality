library(readstata13)
library(car)
library(polycor)
library(psych)
library(plm)

#Define Working Directory
setwd("C:/Users/Matias/Dropbox/Proyectos/Confianza social y politica/Replication_File")

#Open Data from ELSOC harvard dataverse
#load(url("https://dataverse.harvard.edu/api/access/datafile/4606527"))
#el <- elsoc_wide_2016_2019
#elsoc_wide_2016_2019 <- NULL

#Open Data from loadl drive
el <- read.dta13("ELSOC_Wide_2016_2019_v1.00_Stata14.dta", convert.factors = FALSE)


##############################################
###CODIFICACIÓN VARIABLES SOCIODEMOGRoFICAS###
##############################################

#Sexo
el$hombre_w01 <- as.numeric(el$m0_sexo_w01==1)
el$hombre_w02 <- as.numeric(el$m0_sexo_w02==1)
el$hombre_w03 <- as.numeric(el$m0_sexo_w03==1)
el$hombre_w04 <- as.numeric(el$m0_sexo_w04==1)
with(el, cor(cbind(hombre_w01, hombre_w02, hombre_w03, hombre_w04),
             use="pairwise.complete.obs"))

#Edad
el$edad_w01 <- el$m0_edad_w01
el$edad_w02 <- el$m0_edad_w02
el$edad_w03 <- el$m0_edad_w03
el$edad_w04 <- el$m0_edad_w04
with(el, cor(cbind(edad_w01, edad_w02, edad_w03, edad_w04),
             use="pairwise.complete.obs"))

#Educacion recodificada, ego alter
edulabs=c("Ed. Básica o menos", "Ed. Media Inc.",
          "Ed. Media Com.", "Ed. Tec. Sup.", "Ed. Univer.")
edurecode <- "1:3=1; 4=2; 5=3; 6:7=4; 8:10=5; else=NA"
el$educaR_w01 <- factor(recode(el$m01_w01, edurecode), labels=edulabs)
el$educaR_w02 <- factor(recode(el$m01_w02, edurecode), labels=edulabs)
el$educaR_w03 <- factor(recode(el$m01_w03, edurecode), labels=edulabs)
el$educaR_w04 <- factor(recode(el$m01_w04, edurecode), labels=edulabs)

#Religion
rel_str <- "1='Catolico'; 2='Evangelico'; c(3,4,6)='Otra religion'; 5='Creyente, No adherente';
-999:-888=NA; 7:9='Ninguna / Ateo'"
el$religid_w01 <- recode(el$m38_w01, rel_str, as.factor=TRUE)
el$religid_w02 <- recode(el$m38_w02, rel_str, as.factor=TRUE)
el$religid_w03 <- recode(el$m38_w03, rel_str, as.factor=TRUE)
el$religid_w04 <- recode(el$m38_w04, rel_str, as.factor=TRUE)
prop.table(table(el$religid_w04, el$religid_w01),1)

#Practicancia religiosa
el$practica_w01 <- 8 - el$m39_w01
el$practica_w01[el$practica_w01>800] <- NA
el$practica_w01[el$religid_w01=="Ninguna / Ateo"] <- 1
el$practica_w02 <- 8 - el$m39_w02
el$practica_w02[el$practica_w02>800] <- NA
el$practica_w02[el$religid_w02=="Ninguna / Ateo"] <- 1
el$practica_w03 <- 8 - el$m39_w03
el$practica_w03[el$practica_w03>800] <- NA
el$practica_w03[el$religid_w03=="Ninguna / Ateo"] <- 1
el$practica_w04 <- 8 - el$m39_w04
el$practica_w04[el$practica_w04>800] <- NA
el$practica_w04[el$religid_w04=="Ninguna / Ateo"] <- 1
cor(with(el, cbind(practica_w01, practica_w02, practica_w03, practica_w04)),
    use = "complete.obs")

#Transicion a Desempleo
el$desempleo_w01 <- as.numeric(el$m02_w01==6)
el$desempleo_w02 <- as.numeric(el$m02_w02==6)
el$desempleo_w03 <- as.numeric(el$m02_w03==6)
el$desempleo_w04 <- as.numeric(el$m02_w04==6)
with(el, cor(cbind(desempleo_w01, desempleo_w02, desempleo_w03, desempleo_w04),
             use="pairwise.complete.obs"))

#Transición a Empleo
el$empleo_w01 <- as.numeric(el$m02_w01>=1 & el$m02_w01<=2)
el$empleo_w02 <- as.numeric(el$m02_w02>=1 & el$m02_w02<=2)
el$empleo_w03 <- as.numeric(el$m02_w03>=1 & el$m02_w03<=2)
el$empleo_w04 <- as.numeric(el$m02_w04>=1 & el$m02_w04<=2)
with(el, cor(cbind(empleo_w01, empleo_w02, empleo_w03, empleo_w04),
             use="pairwise.complete.obs"))

##############################
##### SALUD Y BIENESTAR ######
##############################

##Satisfaccion con la vida##
el$SatisVida_w01 <- recode(el$s01_w01, "-999:-888=NA")
el$SatisVida_w02 <- recode(el$s01_w02, "-999:-888=NA")
el$SatisVida_w03 <- recode(el$s01_w03, "-999:-888=NA")
el$SatisVida_w04 <- recode(el$s01_w04, "-999:-888=NA")

##Life Ideal
el$VidaIdeal_w01 <- recode(el$s02_w01, "-999:-888=NA")
el$VidaIdeal_w02 <- recode(el$s02_w02, "-999:-888=NA")
el$VidaIdeal_w03 <- recode(el$s02_w03, "-999:-888=NA")
el$VidaIdeal_w04 <- recode(el$s02_w04, "-999:-888=NA")

##Overall Life Evaluation
el$LifeEval_w01 <- (el$SatisVida_w01 + el$VidaIdeal_w01) / 2
el$LifeEval_w02 <- (el$SatisVida_w02 + el$VidaIdeal_w02) / 2
el$LifeEval_w03 <- (el$SatisVida_w03 + el$VidaIdeal_w03) / 2
el$LifeEval_w04 <- (el$SatisVida_w04 + el$VidaIdeal_w04) / 2

#################################
#####CIUDADANIA Y DEMOCRACIA#####
#################################

#Menciona posicion ideologica
el$ideo_w01 <- recode(el$c15_w01, "0:10=1; 11:12=0;-999:-888=0")
el$ideo_w02 <- recode(el$c15_w02, "0:10=1; 11:12=0;-999:-888=0")
el$ideo_w03 <- recode(el$c15_w03, "0:10=1; 11:12=0;-999:-888=0")
el$ideo_w04 <- recode(el$c15_w04, "0:10=1; 11:12=0;-999:-888=0")

#Posicion ideologica
el$izqder_w01 <- ifelse(el$ideo_w01==1, el$c15_w01, NA)
el$izqder_w02 <- ifelse(el$ideo_w02==1, el$c15_w02, NA)
el$izqder_w03 <- ifelse(el$ideo_w03==1, el$c15_w03, NA)
el$izqder_w04 <- ifelse(el$ideo_w04==1, el$c15_w04, NA)
cor(cbind(el$izqder_w01, el$izqder_w02, el$izqder_w03, 
          el$izqder_w04), use="pairwise.complete.obs")

#Posicion ideologica recodificada
el$izqderR_w01 <- factor(car::recode(el$c15_w01, "0:4=1; 5=2; 6:10=3; 11:12=4;-999:-888=4"),
                         labels=c("Izquierda","Centro", "Derecha", "Ninguna/Ind"))
el$izqderR_w02 <- factor(car::recode(el$c15_w02, "0:4=1; 5=2; 6:10=3;11:12=4;-999:-888=4"),
                         labels=c("Izquierda","Centro", "Derecha", "Ninguna/Ind"))
el$izqderR_w03 <- factor(car::recode(el$c15_w03, "0:4=1; 5=2; 6:10=3;11:12=4;-999:-888=4"),
                         labels=c("Izquierda","Centro", "Derecha", "Ninguna/Ind"))
el$izqderR_w04 <- factor(car::recode(el$c15_w04, "0:4=1; 5=2; 6:10=3;11:12=4;-999:-888=4"),
                         labels=c("Izquierda","Centro", "Derecha", "Ninguna/Ind"))
prop.table(table(el$izqderR_w03))

#Dummies para izq-der
el$izq_w01 <- as.numeric(el$izqderR_w01=="Izquierda")
el$izq_w02 <- as.numeric(el$izqderR_w02=="Izquierda")
el$izq_w03 <- as.numeric(el$izqderR_w03=="Izquierda")
el$izq_w04 <- as.numeric(el$izqderR_w04=="Izquierda")
el$centro_w01 <- as.numeric(el$izqderR_w01=="Centro")
el$centro_w02 <- as.numeric(el$izqderR_w02=="Centro")
el$centro_w03 <- as.numeric(el$izqderR_w03=="Centro")
el$centro_w04 <- as.numeric(el$izqderR_w04=="Centro")
el$derecha_w01 <- as.numeric(el$izqderR_w01=="Derecha")
el$derecha_w02 <- as.numeric(el$izqderR_w02=="Derecha")
el$derecha_w03 <- as.numeric(el$izqderR_w03=="Derecha")
el$derecha_w04 <- as.numeric(el$izqderR_w04=="Derecha")
el$ninguna_ind_w01 <- as.numeric(el$izqderR_w01=="Ninguna/Ind")
el$ninguna_ind_w02 <- as.numeric(el$izqderR_w02=="Ninguna/Ind")
el$ninguna_ind_w03 <- as.numeric(el$izqderR_w03=="Ninguna/Ind")
el$ninguna_ind_w04 <- as.numeric(el$izqderR_w04=="Ninguna/Ind")

### CONFIANZA SOCIAL ###

#Confianza Social 1 - Confianza Social Generalizada
el$confsoc1_w01 <- recode(el$c02_w01, "1=3; 2=1; 3=2; -999:-888=NA")
el$confsoc1_w02 <- recode(el$c02_w02, "1=3; 2=1; 3=2; -999:-888=NA")
el$confsoc1_w03 <- recode(el$c02_w03, "1=3; 2=1; 3=2; -999:-888=NA")
el$confsoc1_w04 <- recode(el$c02_w04, "1=3; 2=1; 3=2; -999:-888=NA")
#Confianza Social 2 - Altruismo Social Generalizado
el$confsoc2_w01 <- recode(el$c03_w01, "1=3; 2=1; 3=2; -999:-888=NA")
el$confsoc2_w02 <- recode(el$c03_w02, "1=3; 2=1; 3=2; -999:-888=NA")
el$confsoc2_w03 <- recode(el$c03_w03, "1=3; 2=1; 3=2; -999:-888=NA")
el$confsoc2_w04 <- recode(el$c03_w04, "1=3; 2=1; 3=2; -999:-888=NA")
#Confianza Social 3 - Mayoria de la gente trata de ser justa
el$confsoc3_w01 <- recode(el$c04_w01, "1=1; 2=3; 3=2; -999:-888=NA")
el$confsoc3_w02 <- recode(el$c04_w02, "1=1; 2=3; 3=2; -999:-888=NA")
el$confsoc3_w03 <- recode(el$c04_w03, "1=1; 2=3; 3=2; -999:-888=NA")
el$confsoc3_w04 <- recode(el$c04_w04, "1=1; 2=3; 3=2; -999:-888=NA")


###---CONFIANZA POLITICA---###

#Confianza en el Gobierno
el$confGob_w01 <- recode(el$c05_01_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confGob_w02 <- recode(el$c05_01_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confGob_w03 <- recode(el$c05_01_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confGob_w04 <- recode(el$c05_01_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
#Confianza en Partidos politicos
el$confPart_w01 <- recode(el$c05_02_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confPart_w02 <- recode(el$c05_02_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confPart_w03 <- recode(el$c05_02_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confPart_w04 <- recode(el$c05_02_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
#Confianza en Carabineros
el$confCara_w01 <- recode(el$c05_03_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confCara_w02 <- recode(el$c05_03_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confCara_w03 <- recode(el$c05_03_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confCara_w04 <- recode(el$c05_03_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
#Confianza en Sindicatos
el$confSind_w01 <- recode(el$c05_04_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confSind_w02 <- recode(el$c05_04_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confSind_w03 <- recode(el$c05_04_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confSind_w04 <- recode(el$c05_04_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
#Confianza en Poder Judicial
el$confJudi_w01 <- recode(el$c05_05_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confJudi_w02 <- recode(el$c05_05_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confJudi_w03 <- recode(el$c05_05_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confJudi_w04 <- recode(el$c05_05_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
#Confianza en Empresas Privadas
el$confEmpr_w01 <- recode(el$c05_06_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confEmpr_w02 <- recode(el$c05_06_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confEmpr_w03 <- recode(el$c05_06_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confEmpr_w04 <- recode(el$c05_06_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
#Confianza en Congreso
el$confCong_w01 <- recode(el$c05_07_w01, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confCong_w02 <- recode(el$c05_07_w02, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confCong_w03 <- recode(el$c05_07_w03, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")
el$confCong_w04 <- recode(el$c05_07_w04, "1=1; 2=2; 3=3; 4:5=4; -999:-888=NA")

##Guardar base de datos con variables recodificadas
save(el, file="Elsoc_recode_2016-2019_wide.Rdata")


##---Process data into long format----##

#Preparativos - extraer nombres de variables y borrar 
#variables que no tienen 3 olas.
keep_long <- c(names(el)[which(names(el)=="hombre_w01"):dim(el)[2]],
               paste0("region_w0", 1:4), paste0("region_cod_w0", 1:4),
               paste0("comuna_w0", 1:4), paste0("comuna_cod_w0", 1:4),
               paste0("estrato_w0", 1:4), paste0("segmento_w0", 1:4), 
               paste0("ponderador01_w0", 1:4), paste0("ponderador02_w0", 1:4),
               paste0("dia_entr_w0", 1:4), paste0("mes_entr_w0", 1:4),
               paste0("annio_entr_w0", 1:4))

keep_long <- data.frame(keep_long)
keep_long$varnames <- substr(keep_long$keep_long, 1, 
                             nchar(as.character(keep_long$keep_long))-4)
keep_long$len <- tapply(keep_long$varnames, keep_long$varnames, 
                        length)[keep_long$varnames]
keep_long <- subset(keep_long, len>=4)
keep_long$keep_long <- as.character(factor(keep_long$keep_long, exclude=NULL))
keep_long$varnames <- as.character(factor(keep_long$varnames, exclude=NULL))

##Reshape Data into Long format
adm_vars <- c("idencuesta", "muestra", "tipo_atricion", "tipo_caso")
tmp <- el[, names(el) %in% c(adm_vars, keep_long$keep_long)]

ell <- reshape(tmp, direction="long",
               varying=split(as.character(keep_long$keep_long), keep_long$varnames),
               v.names = sort(unique(as.character(keep_long$varnames))))
dim(ell)
ell <- ell[order(ell$id), ]
head(ell, 10)
names(ell)

#Date
ell$fecha <- as.Date(as.character(paste(ell$dia_entr, ell$mes_entr,
                                        ell$annio_entr, sep = "/")),
                     "%d/%m/%Y")

#Save
save(ell, file="Elsoc_2016-2019_long.Rdata")
