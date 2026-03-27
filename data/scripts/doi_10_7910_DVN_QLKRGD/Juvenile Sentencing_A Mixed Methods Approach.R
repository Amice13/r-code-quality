rm(list=ls())

####################################################
################# Replication code #################
## Juvenile Sentencing: A Mixed-Methods Approach" ##
###### Brazilian Political Science Review, 2019 ####
################ Thiago R. Oliveira ################
####################################################

library(foreign)
library(ggplot2)
library(tcltk) 
library(gridExtra)
library(grid)
library(xtable)
library(dplyr)
library(lme4)

# Set Working Directory
setwd(tk_choose.dir()) # Select directory where you wish to save this script

# Read original dataset:
dataset <- read.spss("C:/Users/Paulo/Desktop/BD_FCASA.sav",
                     to.data.frame=T)

# Exclude missings on the dependent variable
dataset$teste <- ifelse(dataset$p17a == "999", c("missing"), c("ok"))
dataset <- subset(dataset,dataset$teste=="ok")

# Exclude fifth and further entries
dataset <- dataset[dataset$p2< 5,]

### Coding variables to be used

# Disposition
dataset$medida <- ifelse(dataset$p17a == "6", c("Internação"), c("Não Internação"))
attach(dataset)
dataset$int[medida=="Internação"] <- TRUE
dataset$int[is.na(dataset$int)] <- FALSE

# Offense:
dataset$ato[p14cod1 =="8" | p14cod1 == "9"] <- "Roubo"
dataset$ato[p14cod1 == "2" | p14cod1 == "1" | p14cod1 == "49"] <- "Drogas"
dataset$ato[p14cod1 == "4" | p14cod1 == "6" | p14cod1 == "7" 
            | p14cod1 == "23"] <- "Furto e outros crimes contra a propriedade"
dataset$ato[p14cod1 == "5" | p14cod1 == "10" | p14cod1 == "12" | p14cod1 == "13" 
            | p14cod1 == "24" | p14cod1 == "25" | p14cod1 == "30" 
            | p14cod1 == "34"] <- "Homicídio e outros crimes contra a vida"
dataset$ato[p14cod1 == "16" | p14cod1 == "26" | p14cod1 == "27" | p14cod1 == "37" 
            | p14cod1 == "40" | p14cod1 == "41"] <- "Menorismos"
dataset$ato[is.na(dataset$ato)] <- "Outros atos infracionais"
dataset$ato <- as.factor(dataset$ato)


# Use of drugs
dataset$drogas[p25a=="1"] <- "Não usuário"
dataset$drogas[p25a=="7" | p25a=="2" | p25a=="5"] <- "Maconha/cocaína/crack"
dataset$drogas[p25a=="0" | p25a=="999"] <- "Sem informações"
dataset$drogas[is.na(dataset$drogas)] <- "Outras drogas"
dataset$drogas <- as.factor(dataset$drogas)
dataset <- within(dataset, drogas <- relevel(drogas, ref = "Não usuário"))
dataset$drogas.dumm <- ifelse(dataset$drogas=="Não usuário", c("Não usuário"), c("Usuário"))
dataset$drogas.dummy[dataset$drogas.dumm=="Usuário"] <- T
dataset$drogas.dummy[dataset$drogas.dumm=="Não usuário"] <- F

# Ethnicity
dataset$cor[p8=="0"] <- "???"
dataset$cor[p8=="1"] <- "Branca"
dataset$cor[p8=="2"] <- "Parda"
dataset$cor[p8=="3"] <- "Preta"
dataset$cor[p8=="4"] <- "Amarela"
dataset$cor[p8=="5"] <- "Indígena"
dataset$cor[p8=="9"] <- "Morena"
dataset$cor[p8=="999"] <- "Sem informações"
dataset$cor[is.na(dataset$p8)] <- "missing"
dataset$cordumm <- ifelse(dataset$cor=="Branca", c("Branca"), c("Não branca"))
dataset$cordummy[dataset$cordumm=="Branca"] <- FALSE
dataset$cordummy[is.na(dataset$cordummy)] <- TRUE

# Gender
dataset$sexo[p6=="1"] <- "Masculino"
dataset$sexo[p6=="2"] <- "Feminino"
dataset$sexo[p6=="999"] <- NA
dataset$sexo[dataset$sexo=="Masculino"] <- TRUE
dataset$sexo[dataset$sexo=="Feminino"] <- FALSE

# Occupation 
dataset$ocup[p13a=="154"] <- "Só Estuda"
dataset$ocup[p13a=="0" | p13a=="388" | p13a=="390" | p13a=="999"] <- "Não estuda nem trabalha"
dataset$ocup[is.na(dataset$ocup)] <- "Trabalha"
dataset$ocup <- as.factor(dataset$ocup)
dataset$school <- ifelse(dataset$ocup=="Só Estuda",T,F)
dataset$school[dataset$ocup=="Só estuda"] <- TRUE
dataset$school[is.na(dataset$school)] <- FALSE

# Contact with family
dataset$familia[p20=="1" & p22=="1"] <- "Convive com os pais"
dataset$familia[p22=="1" & p20=="2" | p22=="1" & p20=="3" | p22=="1" & p20=="4" | p22=="1" & p20=="5" | p22=="1" & p20=="999"] <- "Convive só com a mãe"
dataset$familia[p20=="1" & p22=="2" | p20=="1" & p22=="3" | p20=="1" & p22=="4" | p20=="1" & p22=="5" | p20=="1" & p22=="999"] <- "Convive só com o pai"
dataset$familia[p20=="999" & p22=="999"] <- "Sem informações"
dataset$familia[is.na(dataset$familia)] <- "Não convive com os pais"
dataset$familia <- as.factor(dataset$familia)


detach(dataset)
##########
dataset <- within(dataset, drogas <- relevel(drogas, ref = "Não usuário"))
dataset <- within(dataset, familia <- relevel(familia, ref = "Convive com os pais"))
dataset <- within(dataset, ocup <- relevel(ocup, ref = "Só Estuda"))

#####################################################################################

# Dataset with variables used in the study
df <- dataset[,c("p1", "p2", "int", "ato", "drogas.dumm", "cordummy", "sexo", "familia", "school")]

# Drop group that predicts failure perfectly
df <- df[df$ato!="Menorismos",]

# Define reference group
df <- within(df, ato <- relevel(ato, ref = "Drogas"))

# Table 02
m1 <- glmer(int ~ p2 + ato + drogas.dumm + cordummy + sexo + familia + school + (1 | p1),
            data = df, family = binomial, control = glmerControl(optimizer = "bobyqa",
                                                                 optCtrl = list(maxfun = 2e5)),
            nAGQ = 10)
summary(m1, corr = F)

# Table 03:
df <- within(df, ato <- relevel(ato, ref = "Drogas"))
  m4.d <- glmer(int ~ p2 + ato + cordummy + ato*school + drogas.dumm + sexo + familia + 
                  school + (1 | p1),
                data = df, family = binomial, control = glmerControl(optimizer = "bobyqa",
                                                                     optCtrl = list(maxfun = 2e5)),
                nAGQ = 10)
  summary(m4.d, corr = F)

df <- within(df, ato <- relevel(ato, ref = "Furto e outros crimes contra a propriedade"))
  m4.f <- glmer(int ~ p2 + ato + cordummy + ato*school + drogas.dumm + sexo + familia + 
                  school + (1 | p1),
                data = df, family = binomial, control = glmerControl(optimizer = "bobyqa",
                                                                     optCtrl = list(maxfun = 2e5)),
                nAGQ = 10)
  summary(m4.f, corr = F)
  
df <- within(df, ato <- relevel(ato, ref = "Outros atos infracionais"))
  m4.o <- glmer(int ~ p2 + ato + cordummy + ato*school + drogas.dumm + sexo + familia + 
                  school + (1 | p1),
                data = df, family = binomial, control = glmerControl(optimizer = "bobyqa",
                                                                     optCtrl = list(maxfun = 2e5)),
                nAGQ = 10)
  summary(m4.o, corr = F)
  
df <- within(df, ato <- relevel(ato, ref = "Roubo"))
  m4.r <- glmer(int ~ p2 + ato + cordummy + ato*school + drogas.dumm + sexo + familia + 
                  school + (1 | p1),
                data = df, family = binomial, control = glmerControl(optimizer = "bobyqa",
                                                                     optCtrl = list(maxfun = 2e5)),
                nAGQ = 10)
  summary(m4.r, corr = F)

df <- within(df, ato <- relevel(ato, ref = "Homicídio e outros crimes contra a vida"))
  m4.h <- glmer(int ~ p2 + ato + cordummy + ato*school + drogas.dumm + sexo + familia + 
                  school + (1 | p1),
                data = df, family = binomial, control = glmerControl(optimizer = "bobyqa",
                                                                     optCtrl = list(maxfun = 2e5)),
                nAGQ = 10)

# Table 04:
df <- within(df, ato <- relevel(ato, ref = "Drogas"))
  m5.d <- glmer(int ~ p2 + ato + cordummy + ato*drogas.dumm + drogas.dumm + sexo + familia + 
                  school + (1 | p1),
                data = df, family = binomial, control = glmerControl(optimizer = "bobyqa",
                                                                     optCtrl = list(maxfun = 2e5)),
                nAGQ = 10)
  summary(m5.d, corr = F)
  
df <- within(df, ato <- relevel(ato, ref = "Furto e outros crimes contra a propriedade"))
  m5.f <- glmer(int ~ p2 + ato + cordummy + ato*drogas.dumm + drogas.dumm + sexo + familia + 
                  school + (1 | p1),
                data = df, family = binomial, control = glmerControl(optimizer = "bobyqa",
                                                                     optCtrl = list(maxfun = 2e5)),
                nAGQ = 10)
  summary(m5.f, corr = F)
  
df <- within(df, ato <- relevel(ato, ref = "Outros atos infracionais"))
  m5.o <- glmer(int ~ p2 + ato + cordummy + ato*drogas.dumm + drogas.dumm + sexo + familia + 
                  school + (1 | p1),
                data = df, family = binomial, control = glmerControl(optimizer = "bobyqa",
                                                                     optCtrl = list(maxfun = 2e5)),
                nAGQ = 10)
  summary(m5.o, corr = F)
  
df <- within(df, ato <- relevel(ato, ref = "Roubo"))
  m5.r <- glmer(int ~ p2 + ato + cordummy + ato*drogas.dumm + drogas.dumm + sexo + familia + 
                  school + (1 | p1),
                data = df, family = binomial, control = glmerControl(optimizer = "bobyqa",
                                                                     optCtrl = list(maxfun = 2e5)),
                nAGQ = 10)
  summary(m5.r, corr = F)
  
df <- within(df, ato <- relevel(ato, ref = "Homicídio e outros crimes contra a vida"))
  m5.h <- glmer(int ~ p2 + ato + cordummy + ato*drogas.dumm + drogas.dumm + sexo + familia + 
                  school + (1 | p1),
                data = df, family = binomial, control = glmerControl(optimizer = "bobyqa",
                                                                     optCtrl = list(maxfun = 2e5)),
                nAGQ = 10)
  summary(m5.h, corr = F)