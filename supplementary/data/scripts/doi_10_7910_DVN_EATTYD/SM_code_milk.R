################################################################################
################################################################################
##### ASSESSMENT LUMINOMETRY, TRIPLATE AND PETRIFILM TO DETECT IMI IN DAIRY ####
######################### COWS WITH NO CLINICAL SIGNS ##########################
################################################################################
############################ Analyzed October 2023 ##############################
################################################################################
################################################################################


library(R2jags)
library(coda)
library(mcmcplots)
library(epiR)
library(ggplot2)
library(Hmisc)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(tidyverse)

#data ####

#import dataset df.csv#

df3 <- df %>% 
  filter(is.na(ultra_frais)) 

#raw value lumino
describe(df$ultra_frais)

#raw and dichotomized laboratory culture
describe(df$cfu)
describe(df$bacterio)

#raw and dichotomized petrifilm
describe(df$petri_ac_48)
describe(df$petrifilm)

#raw and dichotomized triplate
describe(df$tp_sang_48)
describe(df$tp_choc_48)
describe(df$tp_mac_48)
df %>% mutate(tp_total = tp_sang_48+tp_choc_48+tp_mac_48) %>%
  select(tp_total) %>%
  describe()

describe(df$tp)


################################################################################
# all data - 4 tests, 2 pop, w/ dependence #####################################
################################################################################
#2x2 tables (4 tests)####

table(df$tp,df$bacterio,df$petrifilm,df$lumino50, 
      deparse.level = 2)

table(df$tp,df$bacterio,df$petrifilm,df$lumino100, 
      deparse.level = 2)

table(df$tp,df$bacterio,df$petrifilm,df$lumino125, 
      deparse.level = 2)

table(df$tp,df$bacterio,df$petrifilm,df$lumino150, 
      deparse.level = 2)

table(df$tp,df$bacterio,df$petrifilm,df$lumino175, 
      deparse.level = 2)

table(df$tp,df$bacterio,df$petrifilm,df$lumino200,
      deparse.level = 2)

table(df$tp,df$bacterio,df$petrifilm,df$lumino250, 
      deparse.level = 2)

table(df$tp,df$bacterio,df$petrifilm,df$lumino300, 
      deparse.level = 2)

table(df$tp,df$bacterio,df$petrifilm,df$lumino350, 
      deparse.level = 2)

table(df$tp,df$bacterio,df$petrifilm,df$lumino400, 
      deparse.level = 2)

#2x2 tables (3 tests)####

table(df3$tp,df3$bacterio,df3$petrifilm,
      deparse.level = 2)

#n ####
#population 1 (4 tests)
#is of the form : 
#(TestA+ TestB+ TestC+ TestD+), (TestA+ TestB- TestC+ TestD+), 
#(TestA- TestB+ TestC+ TestD+), (TestA- TestB- TestC+ TestD+), 
#(TestA+ TestB+ TestC- TestD+), (TestA+ TestB- TestC- TestD+), 
#(TestA- TestB+ TestC- TestD+), (TestA- TestB- TestC- TestD+),
#(TestA+ TestB+ TestC+ TestD-), (TestA+ TestB- TestC+ TestD-), 
#(TestA- TestB+ TestC+ TestD-), (TestA- TestB- TestC+ TestD-), 
#(TestA+ TestB+ TestC- TestD-), (TestA+ TestB- TestC- TestD-), 
#(TestA- TestB+ TestC- TestD-), (TestA- TestB- TestC- TestD-)

#population 2 (3 tests)
#(TestA+ TestB+ TestC+), #(TestA+ TestB+ TestC-), 
#(TestA- TestB+ TestC+), #(TestA- TestB+ TestC-),
#(TestA+ TestB- TestC+), #(TestA+ TestB- TestC-), 
#(TestA- TestB- TestC+), #(TestA- TestB- TestC-)


#50
datalist50 <- list(Pop1=c(97,60,6,20,4,22,6,34,
                        26,10,1,2,0,0,0,6),
                 Pop2=c(5,8,1,3,2,6,0,14))

#100
datalist100 <- list(Pop1=c(73,49,5,15,4,18,6,29,
                        50,21,2,7,0,4,0,11),
                 Pop2=c(5,8,1,3,2,6,0,14))

#125
datalist125 <- list(Pop1=c(66,46,4,14,4,15,5,26,
                           57,24,3,8,0,7,1,14),
                    Pop2=c(5,8,1,3,2,6,0,14))

#150
datalist150 <- list(Pop1=c(57,43,4,13,4,11,5,24,
                           66,27,3,9,0,11,1,16),
                    Pop2=c(5,8,1,3,2,6,0,14))

#175
datalist175 <- list(Pop1=c(47,34,2,12,1,10,4,22,
                           76,36,5,10,3,12,2,18),
                    Pop2=c(5,8,1,3,2,6,0,14))


#200
datalist200 <- list(Pop1=c(43,25,2,8,1,9,3,20,
                        80,45,5,14,3,13,3,20),
                 Pop2=c(5,8,1,3,2,6,0,14))

#250
datalist250 <- list(Pop1=c(33,17,2,5,1,8,1,15,
                           90,53,5,17,3,14,5,25),
                    Pop2=c(5,8,1,3,2,6,0,14))

#300
datalist300 <- list(Pop1=c(27,10,2,5,1,6,1,10,
                           96,60,5,17,3,16,5,30),
                    Pop2=c(5,8,1,3,2,6,0,14))

#350
datalist350 <- list(Pop1=c(23,8,2,4,1,3,1,5,
                           100,62,5,18,3,19,5,35),
                    Pop2=c(5,8,1,3,2,6,0,14))

#400
datalist400 <- list(Pop1=c(19,6,2,2,1,2,1,4,
                        104,64,5,20,3,20,5,36),
                 Pop2=c(5,8,1,3,2,6,0,14))


#Labels for TestA, TestB, and TestC####
TestA <- "bacterio"
TestB <- "triplate"
TestC <- "petrifilm"
TestD <- "lumino"


#Provide information for the prior distributions (all beta distributions) for the 9 unknown parameters 
####priors####
Prev.shapea <- 2.3        #a shape parameter for Prev     
Prev.shapeb <- 2.5         #b shape parameter for Prev 

Se.TestA.shapea <- 24.3     #a shape parameter for Se of TestA
Se.TestA.shapeb <- 3      #b shape parameter for Se of TestA
Sp.TestA.shapea <- 9.9     #a shape parameter for Sp of TestA
Sp.TestA.shapeb <- 3.2     #b shape parameter for Sp of TestA

Se.TestB.shapea <- 7.6   #a shape parameter for Se of TestB
Se.TestB.shapeb <- 2.7     #b shape parameter for Se of TestB
Sp.TestB.shapea <- 6.5    #a shape parameter for Sp of TestB
Sp.TestB.shapeb <- 2     #b shape parameter for Sp of TestB

Se.TestC.shapea <- 9.7     #a shape parameter for Se of TestC
Se.TestC.shapeb <- 1.8     #b shape parameter for Se of TestC
Sp.TestC.shapea <- 4.8     #a shape parameter for Sp of TestC
Sp.TestC.shapeb <- 2.6     #b shape parameter for Sp of TestC

Se.TestD.shapea <- 1     #a shape parameter for Se of TestD
Se.TestD.shapeb <- 1     #b shape parameter for Se of TestD
Sp.TestD.shapea <- 1     #a shape parameter for Sp of TestD
Sp.TestD.shapeb <- 1     #b shape parameter for Sp of TestD

####perturbed diffuse priors####
Prev.shapea <- 1         #a shape parameter for Prev     
Prev.shapeb <- 1         #b shape parameter for Prev 

Se.TestA.shapea <- 4     #a shape parameter for Se of TestA
Se.TestA.shapeb <- 1.1     #b shape parameter for Se of TestA
Sp.TestA.shapea <- 3.2     #a shape parameter for Sp of TestA
Sp.TestA.shapeb <- 1.6     #b shape parameter for Sp of TestA

Se.TestB.shapea <- 2.1   #a shape parameter for Se of TestB
Se.TestB.shapeb <- 1.3     #b shape parameter for Se of TestB
Sp.TestB.shapea <- 2.1     #a shape parameter for Sp of TestB
Sp.TestB.shapeb <- 1.2     #b shape parameter for Sp of TestB

Se.TestC.shapea <- 3    #a shape parameter for Se of TestC
Se.TestC.shapeb <- 1.1     #b shape parameter for Se of TestC
Sp.TestC.shapea <- 2.3    #a shape parameter for Sp of TestC
Sp.TestC.shapeb <- 1.6    #b shape parameter for Sp of TestC

Se.TestD.shapea <- 1     #a shape parameter for Se of TestD
Se.TestD.shapeb <- 1     #b shape parameter for Se of TestD
Sp.TestD.shapea <- 1     #a shape parameter for Sp of TestD
Sp.TestD.shapeb <- 1     #b shape parameter for Sp of TestD

#plot priors####
#prevalence
curve(dbeta(x, shape1 = Prev.shapea, shape2 = Prev.shapeb), from = 0, to = 1,
      main="Prior for prevalence of IMI at dry-off", xlab = "Prevalence", ylab = "Density")


#Se
curve(dbeta(x, shape1 = Se.TestA.shapea, shape2 = Se.TestA.shapeb), from = 0, to = 1,
      col = "blue",
      main="Prior for sensitivity to detect of IMI at dry-off", xlab = "Sensitivity", ylab = "Density")

curve(dbeta(x, shape1 = Se.TestB.shapea, shape2 = Se.TestB.shapeb), from = 0, to = 1,
      lty = 3,
      add = TRUE)

curve(dbeta(x, shape1 = Se.TestC.shapea, shape2 = Se.TestC.shapeb), from = 0, to = 1,
      col = "darkgreen", lty = 2,
      add = TRUE)

curve(dbeta(x, shape1 = Se.TestD.shapea, shape2 = Se.TestD.shapeb), from = 0, to = 1,
      col = "darkred",
      add = TRUE)

#Sp
curve(dbeta(x, shape1 = Sp.TestA.shapea, shape2 = Sp.TestA.shapeb), from = 0, to = 1,
      col = "blue",
      main="Prior for specificity to detect of IMI at dry-off", xlab = "Specificity", ylab = "Density")

curve(dbeta(x, shape1 = Sp.TestD.shapea, shape2 = Sp.TestD.shapeb), from = 0, to = 1,
      col = "darkred",
      add = TRUE)

curve(dbeta(x, shape1 = Sp.TestB.shapea, shape2 = Sp.TestB.shapeb), from = 0, to = 1,
      lty = 3,
      add = TRUE)

curve(dbeta(x, shape1 = Sp.TestC.shapea, shape2 = Sp.TestC.shapeb), from = 0, to = 1,
      col = "darkgreen", lty = 2,
      add = TRUE)



#cov
qcauchy(p = c(0.025, 0.975), location = 0, scale = 0.04)
curve(dcauchy(x,location = 0, scale = 0.039), from = -1, to = 1)


#total n####
#I will also need the total number of individuals tested 
n <- sapply(datalist150, sum) #the npop is the same in all datalists (regardless of the threshold)
nPop1 <- n[1]
nPop2 <- n[2]

#Create the JAGS text file####
#used Lurier et al. 2021####
model_4tests_2pop_dep <- paste0("model{

#=== LIKELIHOOD ===#

  #=== POPULATION 1 ===#

  Pop1[1:16] ~ dmulti(p1[1:16], ",nPop1,")
  p1[1] <- Prev*(Se_", TestA, "*Se_", TestB, "*Se_", TestC, " + covse_111)*Se_", TestD, " + (1-Prev)*((1-Sp_", TestA, ")*(1-Sp_", TestB, ")*(1-Sp_", TestC,") + covsp_111)*(1-Sp_", TestD,")
  p1[2] <- Prev*(Se_", TestA, "*(1-Se_", TestB, ")*Se_", TestC, " + covse_101)*Se_", TestD, " + (1-Prev)*((1-Sp_", TestA, ")*Sp_", TestB, "*(1-Sp_", TestC,") + covsp_101)*(1-Sp_", TestD,")
  p1[3] <- Prev*((1-Se_", TestA, ")*Se_", TestB, "*Se_", TestC, " + covse_011)*Se_", TestD, " + (1-Prev)*(Sp_", TestA, "*(1-Sp_", TestB, ")*(1-Sp_", TestC,") + covsp_011)*(1-Sp_", TestD,")
  p1[4] <- Prev*((1-Se_", TestA, ")*(1-Se_", TestB, ")*Se_", TestC, " + covse_001)*Se_", TestD, " + (1-Prev)*(Sp_", TestA, "*Sp_", TestB, "*(1-Sp_", TestC,") + covsp_001)*(1-Sp_", TestD,")
  
  p1[5] <- Prev*(Se_", TestA, "*Se_", TestB, "*(1-Se_", TestC,") + covse_110)*Se_", TestD, " + (1-Prev)*((1-Sp_", TestA, ")*(1-Sp_", TestB, ")*Sp_", TestC," + covsp_110)*(1-Sp_", TestD,")
  p1[6] <- Prev*(Se_", TestA, "*(1-Se_", TestB, ")*(1-Se_", TestC,") + covse_100)*Se_", TestD, " + (1-Prev)*((1-Sp_", TestA, ")*Sp_", TestB, "*Sp_", TestC," + covsp_100)*(1-Sp_", TestD,")
  p1[7] <- Prev*((1-Se_", TestA, ")*Se_", TestB, "*(1-Se_", TestC,") + covse_010)*Se_", TestD, " + (1-Prev)*(Sp_", TestA, "*(1-Sp_", TestB, ")*Sp_", TestC," + covsp_010)*(1-Sp_", TestD,")
  p1[8] <- Prev*((1-Se_", TestA, ")*(1-Se_", TestB, ")*(1-Se_", TestC,") + covse_000)*Se_", TestD, " + (1-Prev)*(Sp_", TestA, "*Sp_", TestB, "*Sp_", TestC," + covsp_000)*(1-Sp_", TestD,")
  
  p1[9] <- Prev*(Se_", TestA, "*Se_", TestB, "*Se_", TestC, " + covse_111)*(1-Se_", TestD, ") + (1-Prev)*((1-Sp_", TestA, ")*(1-Sp_", TestB, ")*(1-Sp_", TestC,") + covsp_111)*Sp_", TestD, "
  p1[10] <- Prev*(Se_", TestA, "*(1-Se_", TestB, ")*Se_", TestC, " + covse_101)*(1-Se_", TestD, ") + (1-Prev)*((1-Sp_", TestA, ")*Sp_", TestB, "*(1-Sp_", TestC,") + covsp_101)*Sp_", TestD, "
  p1[11] <- Prev*((1-Se_", TestA, ")*Se_", TestB, "*Se_", TestC, " + covse_011)*(1-Se_", TestD, ") + (1-Prev)*(Sp_", TestA, "*(1-Sp_", TestB, ")*(1-Sp_", TestC,") + covsp_011)*Sp_", TestD, "
  p1[12] <- Prev*((1-Se_", TestA, ")*(1-Se_", TestB, ")*Se_", TestC, " + covse_001)*(1-Se_", TestD, ") + (1-Prev)*(Sp_", TestA, "*Sp_", TestB, "*(1-Sp_", TestC,") + covsp_001)*Sp_", TestD, "
  
  p1[13] <- Prev*(Se_", TestA, "*Se_", TestB, "*(1-Se_", TestC,") + covse_110)*(1-Se_", TestD, ") + (1-Prev)*((1-Sp_", TestA, ")*(1-Sp_", TestB, ")*Sp_", TestC," + covsp_110)*Sp_", TestD, "
  p1[14] <- Prev*(Se_", TestA, "*(1-Se_", TestB, ")*(1-Se_", TestC,") + covse_100)*(1-Se_", TestD, ") + (1-Prev)*((1-Sp_", TestA, ")*Sp_", TestB, "*Sp_", TestC," + covsp_100)*Sp_", TestD, "
  p1[15] <- Prev*((1-Se_", TestA, ")*Se_", TestB, "*(1-Se_", TestC,") + covse_010)*(1-Se_", TestD, ") + (1-Prev)*(Sp_", TestA, "*(1-Sp_", TestB, ")*Sp_", TestC," + covsp_010)*Sp_", TestD, "
  p1[16] <- Prev*((1-Se_", TestA, ")*(1-Se_", TestB, ")*(1-Se_", TestC,") +covse_000)*(1-Se_", TestD, ") + (1-Prev)*(Sp_", TestA, "*Sp_", TestB, "*Sp_", TestC," + covsp_000)*Sp_", TestD, "
  
  #=== POPULATION 2 ===#
    
  Pop2[1:8] ~ dmulti(p2[1:8], ",nPop2,")
  p2[1] <- Prev*(Se_", TestA, "*Se_", TestB, "*Se_", TestC, " + covse_111) + (1-Prev)*((1-Sp_", TestA, ")*(1-Sp_", TestB,")*(1-Sp_", TestC,") + covsp_111)
  p2[2] <- Prev*(Se_", TestA, "*Se_", TestB, "*(1-Se_", TestC, ") + covse_110) + (1-Prev)*((1-Sp_", TestA, ")*(1-Sp_", TestB,")*Sp_", TestC," + covsp_110)
  p2[3] <- Prev*((1-Se_", TestA, ")*Se_", TestB, "*Se_", TestC, " + covse_011) + (1-Prev)*(Sp_", TestA, "*(1-Sp_", TestB,")*(1-Sp_", TestC,") + covsp_011)
  p2[4] <- Prev*((1-Se_", TestA, ")*Se_", TestB, "*(1-Se_", TestC, ") + covse_010) + (1-Prev)*(Sp_", TestA, "*(1-Sp_", TestB,")*Sp_", TestC," + covsp_010)
  
  p2[5] <- Prev*(Se_", TestA, "*(1-Se_", TestB,")*Se_", TestC, " + covse_101) + (1-Prev)*((1-Sp_", TestA, ")*Sp_", TestB,"*(1-Sp_", TestC,") + covsp_101)
  p2[6] <- Prev*(Se_", TestA, "*(1-Se_", TestB,")*(1-Se_", TestC, ") + covse_100) + (1-Prev)*((1-Sp_", TestA, ")*Sp_", TestB,"*Sp_", TestC," + covsp_100)
  p2[7] <- Prev*((1-Se_", TestA, ")*(1-Se_", TestB,")*Se_", TestC, " + covse_001) + (1-Prev)*(Sp_", TestA, "*Sp_", TestB,"*(1-Sp_", TestC,") + covsp_001)
  p2[8] <- Prev*((1-Se_", TestA, ")*(1-Se_", TestB,")*(1-Se_", TestC, ") + covse_000) + (1-Prev)*(Sp_", TestA, "*Sp_", TestB,"*Sp_", TestC," + covsp_000)
 
  # Calculation of the covariance terms
  covse_100 <- covse_011 + covse_111 - covse_000
	covse_101 <- -(covse_001 + covse_011 + covse_111)
	covse_110 <- covse_000 + covse_001 - covse_111
	covse_010 <- -(covse_000 + covse_001 + covse_011)
	
	covsp_100 <- covsp_011 + covsp_111 - covsp_000
	covsp_101 <- -(covsp_001 + covsp_011 + covsp_111)
	covsp_110 <- covsp_000 + covsp_001 - covsp_111
	covsp_010 <- -(covsp_000 + covsp_001 + covsp_011)

#=== PRIOR  ===#

  Prev ~ dbeta(",Prev.shapea,", ",Prev.shapeb,")    ## Prior for Prevalence
  Se_", TestA, " ~ dbeta(",Se.TestA.shapea,", ",Se.TestA.shapeb,")  ## Prior for Se of Test A
  Sp_", TestA, " ~ dbeta(",Sp.TestA.shapea,", ",Sp.TestA.shapeb,")  ## Prior for Sp of Test A
  Se_", TestB, " ~ dbeta(",Se.TestB.shapea,", ",Se.TestB.shapeb,")  ## Prior for Se of Test B
  Sp_", TestB, " ~ dbeta(",Sp.TestB.shapea,", ",Sp.TestB.shapeb,")  ## Prior for Sp of Test B
  Se_", TestC, " ~ dbeta(",Se.TestC.shapea,", ",Se.TestC.shapeb,")  ## Prior for Se of Test C
  Sp_", TestC, " ~ dbeta(",Sp.TestC.shapea,", ",Sp.TestC.shapeb,")  ## Prior for Sp of Test C
  Se_", TestD, " ~ dbeta(",Se.TestD.shapea,", ",Se.TestD.shapeb,")  ## Prior for Se of Test D
  Sp_", TestD, " ~ dbeta(",Sp.TestD.shapea,", ",Sp.TestD.shapeb,")  ## Prior for Sp of Test D
  
  
  covse_111 ~ dt(0, 1/0.04^2, 1) #JAGS doesn't have cauchy distribution - dt(mu, tau, k) where k = 1 works / cauchy(0,0.039) has 2.5-97.5 percentiles between -0.5-0.5
  covse_011 ~ dt(0, 1/0.04^2, 1)
  covse_000 ~ dt(0, 1/0.04^2, 1)
  covse_001 ~ dt(0, 1/0.04^2, 1)
  
  covsp_111 ~ dt(0, 1/0.04^2, 1)
  covsp_011 ~ dt(0, 1/0.04^2, 1)
  covsp_000 ~ dt(0, 1/0.04^2, 1)
  covsp_001 ~ dt(0, 1/0.04^2, 1)

  #==== calculation PVs ====#
  
  PPV_lumino <- (Prev*Se_lumino)/(Prev*Se_lumino+(1-Prev)*(1-Sp_lumino))
  NPV_lumino <- ((1-Prev)*Sp_lumino)/((Prev*(1-Se_lumino))+((1-Prev)*Sp_lumino))

  PPV_lumino_30 <- (0.3*Se_lumino)/(0.3*Se_lumino+(1-0.3)*(1-Sp_lumino))
  NPV_lumino_30 <- ((1-0.3)*Sp_lumino)/((0.3*(1-Se_lumino))+((1-0.3)*Sp_lumino))

  PPV_lumino_50 <- (0.5*Se_lumino)/(0.5*Se_lumino+(1-0.5)*(1-Sp_lumino))
  NPV_lumino_50 <- ((1-0.5)*Sp_lumino)/((0.5*(1-Se_lumino))+((1-0.5)*Sp_lumino))

  PPV_lumino_80 <- (0.8*Se_lumino)/(0.8*Se_lumino+(1-0.8)*(1-Sp_lumino))
  NPV_lumino_80 <- ((1-0.8)*Sp_lumino)/((0.8*(1-Se_lumino))+((1-0.8)*Sp_lumino))

  PPV_bacterio_30 <- (0.3*Se_bacterio)/(0.3*Se_bacterio+(1-0.3)*(1-Sp_bacterio))
  NPV_bacterio_30 <- ((1-0.3)*Sp_bacterio)/((0.3*(1-Se_bacterio))+((1-0.3)*Sp_bacterio))

  PPV_bacterio_50 <- (0.5*Se_bacterio)/(0.5*Se_bacterio+(1-0.5)*(1-Sp_bacterio))
  NPV_bacterio_50 <- ((1-0.5)*Sp_bacterio)/((0.5*(1-Se_bacterio))+((1-0.5)*Sp_bacterio))

  PPV_bacterio_80 <- (0.8*Se_bacterio)/(0.8*Se_bacterio+(1-0.8)*(1-Sp_bacterio))
  NPV_bacterio_80 <- ((1-0.8)*Sp_bacterio)/((0.8*(1-Se_bacterio))+((1-0.8)*Sp_bacterio))
  
  PPV_petrifilm_30 <- (0.3*Se_petrifilm)/(0.3*Se_petrifilm+(1-0.3)*(1-Sp_petrifilm))
  NPV_petrifilm_30 <- ((1-0.3)*Sp_petrifilm)/((0.3*(1-Se_petrifilm))+((1-0.3)*Sp_petrifilm))

  PPV_petrifilm_50 <- (0.5*Se_petrifilm)/(0.5*Se_petrifilm+(1-0.5)*(1-Sp_petrifilm))
  NPV_petrifilm_50 <- ((1-0.5)*Sp_petrifilm)/((0.5*(1-Se_petrifilm))+((1-0.5)*Sp_petrifilm))

  PPV_petrifilm_80 <- (0.8*Se_petrifilm)/(0.8*Se_petrifilm+(1-0.8)*(1-Sp_petrifilm))
  NPV_petrifilm_80 <- ((1-0.8)*Sp_petrifilm)/((0.8*(1-Se_petrifilm))+((1-0.8)*Sp_petrifilm))

  PPV_triplate_30 <- (0.3*Se_triplate)/(0.3*Se_triplate+(1-0.3)*(1-Sp_triplate))
  NPV_triplate_30 <- ((1-0.3)*Sp_triplate)/((0.3*(1-Se_triplate))+((1-0.3)*Sp_triplate))

  PPV_triplate_50 <- (0.5*Se_triplate)/(0.5*Se_triplate+(1-0.5)*(1-Sp_triplate))
  NPV_triplate_50 <- ((1-0.5)*Sp_triplate)/((0.5*(1-Se_triplate))+((1-0.5)*Sp_triplate))

  PPV_triplate_80 <- (0.8*Se_triplate)/(0.8*Se_triplate+(1-0.8)*(1-Sp_triplate))
  NPV_triplate_80 <- ((1-0.8)*Sp_triplate)/((0.8*(1-Se_triplate))+((1-0.8)*Sp_triplate))

  MCT_13 <- ((1/3)*Prev*(1-Se_lumino))+((1-Prev)*(1-Sp_lumino))
  MCT_12 <- ((1/2)*Prev*(1-Se_lumino))+((1-Prev)*(1-Sp_lumino))
  MCT_11 <- (Prev*(1-Se_lumino))+((1-Prev)*(1-Sp_lumino))
  MCT_21 <- ((2)*Prev*(1-Se_lumino))+((1-Prev)*(1-Sp_lumino))
  MCT_31 <- ((3)*Prev*(1-Se_lumino))+((1-Prev)*(1-Sp_lumino))

  MCT_13_30 <- ((1/3)*0.30*(1-Se_lumino))+((1-0.30)*(1-Sp_lumino))
  MCT_12_30 <- ((1/2)*0.30*(1-Se_lumino))+((1-0.30)*(1-Sp_lumino))
  MCT_11_30 <- (0.30*(1-Se_lumino))+((1-0.30)*(1-Sp_lumino))
  MCT_21_30 <- ((2)*0.30*(1-Se_lumino))+((1-0.30)*(1-Sp_lumino))
  MCT_31_30 <- ((3)*0.30*(1-Se_lumino))+((1-0.30)*(1-Sp_lumino))
  
  MCT_13_80 <- ((1/3)*0.80*(1-Se_lumino))+((1-0.80)*(1-Sp_lumino))
  MCT_12_80 <- ((1/2)*0.80*(1-Se_lumino))+((1-0.80)*(1-Sp_lumino))
  MCT_11_80 <- (0.80*(1-Se_lumino))+((1-0.80)*(1-Sp_lumino))
  MCT_21_80 <- ((2)*0.80*(1-Se_lumino))+((1-0.80)*(1-Sp_lumino))
  MCT_31_80 <- ((3)*0.80*(1-Se_lumino))+((1-0.80)*(1-Sp_lumino))
  
  MCT_13_50 <- ((1/3)*0.50*(1-Se_lumino))+((1-0.50)*(1-Sp_lumino))
  MCT_12_50 <- ((1/2)*0.50*(1-Se_lumino))+((1-0.50)*(1-Sp_lumino))
  MCT_11_50 <- (0.50*(1-Se_lumino))+((1-0.50)*(1-Sp_lumino))
  MCT_21_50 <- ((2)*0.50*(1-Se_lumino))+((1-0.50)*(1-Sp_lumino))
  MCT_31_50 <- ((3)*0.50*(1-Se_lumino))+((1-0.50)*(1-Sp_lumino))
  
  MCT_13b <- ((1/3)*Prev*(1-Se_bacterio))+((1-Prev)*(1-Sp_bacterio))
  MCT_12b <- ((1/2)*Prev*(1-Se_bacterio))+((1-Prev)*(1-Sp_bacterio))
  MCT_11b <- (Prev*(1-Se_bacterio))+((1-Prev)*(1-Sp_bacterio))
  MCT_21b <- ((2)*Prev*(1-Se_bacterio))+((1-Prev)*(1-Sp_bacterio))
  MCT_31b <- ((3)*Prev*(1-Se_bacterio))+((1-Prev)*(1-Sp_bacterio))

  MCT_13b_30 <- ((1/3)*0.30*(1-Se_bacterio))+((1-0.30)*(1-Sp_bacterio))
  MCT_12b_30 <- ((1/2)*0.30*(1-Se_bacterio))+((1-0.30)*(1-Sp_bacterio))
  MCT_11b_30 <- (0.30*(1-Se_bacterio))+((1-0.30)*(1-Sp_bacterio))
  MCT_21b_30 <- ((2)*0.30*(1-Se_bacterio))+((1-0.30)*(1-Sp_bacterio))
  MCT_31b_30 <- ((3)*0.30*(1-Se_bacterio))+((1-0.30)*(1-Sp_bacterio))
  
  MCT_13b_50 <- ((1/3)*0.50*(1-Se_bacterio))+((1-0.50)*(1-Sp_bacterio))
  MCT_12b_50 <- ((1/2)*0.50*(1-Se_bacterio))+((1-0.50)*(1-Sp_bacterio))
  MCT_11b_50 <- (0.50*(1-Se_bacterio))+((1-0.50)*(1-Sp_bacterio))
  MCT_21b_50 <- ((2)*0.50*(1-Se_bacterio))+((1-0.50)*(1-Sp_bacterio))
  MCT_31b_50 <- ((3)*0.50*(1-Se_bacterio))+((1-0.50)*(1-Sp_bacterio))
  
  MCT_13b_80 <- ((1/3)*0.80*(1-Se_bacterio))+((1-0.80)*(1-Sp_bacterio))
  MCT_12b_80 <- ((1/2)*0.80*(1-Se_bacterio))+((1-0.80)*(1-Sp_bacterio))
  MCT_11b_80 <- (0.80*(1-Se_bacterio))+((1-0.80)*(1-Sp_bacterio))
  MCT_21b_80 <- ((2)*0.80*(1-Se_bacterio))+((1-0.80)*(1-Sp_bacterio))
  MCT_31b_80 <- ((3)*0.80*(1-Se_bacterio))+((1-0.80)*(1-Sp_bacterio))

  MCT_13p <- ((1/3)*Prev*(1-Se_petrifilm))+((1-Prev)*(1-Sp_petrifilm))
  MCT_12p <- ((1/2)*Prev*(1-Se_petrifilm))+((1-Prev)*(1-Sp_petrifilm))
  MCT_11p <- (Prev*(1-Se_petrifilm))+((1-Prev)*(1-Sp_petrifilm))
  MCT_21p <- ((2)*Prev*(1-Se_petrifilm))+((1-Prev)*(1-Sp_petrifilm))
  MCT_31p <- ((3)*Prev*(1-Se_petrifilm))+((1-Prev)*(1-Sp_petrifilm))

  MCT_13p_30 <- ((1/3)*0.30*(1-Se_petrifilm))+((1-0.30)*(1-Sp_petrifilm))
  MCT_12p_30 <- ((1/2)*0.30*(1-Se_petrifilm))+((1-0.30)*(1-Sp_petrifilm))
  MCT_11p_30 <- (0.30*(1-Se_petrifilm))+((1-0.30)*(1-Sp_petrifilm))
  MCT_21p_30 <- ((2)*0.30*(1-Se_petrifilm))+((1-0.30)*(1-Sp_petrifilm))
  MCT_31p_30 <- ((3)*0.30*(1-Se_petrifilm))+((1-0.30)*(1-Sp_petrifilm))

  MCT_13p_80 <- ((1/3)*0.80*(1-Se_petrifilm))+((1-0.80)*(1-Sp_petrifilm))
  MCT_12p_80 <- ((1/2)*0.80*(1-Se_petrifilm))+((1-0.80)*(1-Sp_petrifilm))
  MCT_11p_80 <- (0.80*(1-Se_petrifilm))+((1-0.80)*(1-Sp_petrifilm))
  MCT_21p_80 <- ((2)*0.80*(1-Se_petrifilm))+((1-0.80)*(1-Sp_petrifilm))
  MCT_31p_80 <- ((3)*0.80*(1-Se_petrifilm))+((1-0.80)*(1-Sp_petrifilm))

  MCT_13p_50 <- ((1/3)*0.50*(1-Se_petrifilm))+((1-0.50)*(1-Sp_petrifilm))
  MCT_12p_50 <- ((1/2)*0.50*(1-Se_petrifilm))+((1-0.50)*(1-Sp_petrifilm))
  MCT_11p_50 <- (0.50*(1-Se_petrifilm))+((1-0.50)*(1-Sp_petrifilm))
  MCT_21p_50 <- ((2)*0.50*(1-Se_petrifilm))+((1-0.50)*(1-Sp_petrifilm))
  MCT_31p_50 <- ((3)*0.50*(1-Se_petrifilm))+((1-0.50)*(1-Sp_petrifilm))

  MCT_13t <- ((1/3)*Prev*(1-Se_triplate))+((1-Prev)*(1-Sp_triplate))
  MCT_12t <- ((1/2)*Prev*(1-Se_triplate))+((1-Prev)*(1-Sp_triplate))
  MCT_11t <- (Prev*(1-Se_triplate))+((1-Prev)*(1-Sp_triplate))
  MCT_21t <- ((2)*Prev*(1-Se_triplate))+((1-Prev)*(1-Sp_triplate))
  MCT_31t <- ((3)*Prev*(1-Se_triplate))+((1-Prev)*(1-Sp_triplate))

  MCT_13t_30 <- ((1/3)*0.30*(1-Se_triplate))+((1-0.30)*(1-Sp_triplate))
  MCT_12t_30 <- ((1/2)*0.30*(1-Se_triplate))+((1-0.30)*(1-Sp_triplate))
  MCT_11t_30 <- (0.30*(1-Se_triplate))+((1-0.30)*(1-Sp_triplate))
  MCT_21t_30 <- ((2)*0.30*(1-Se_triplate))+((1-0.30)*(1-Sp_triplate))
  MCT_31t_30 <- ((3)*0.30*(1-Se_triplate))+((1-0.30)*(1-Sp_triplate))

  MCT_13t_50 <- ((1/3)*0.50*(1-Se_triplate))+((1-0.50)*(1-Sp_triplate))
  MCT_12t_50 <- ((1/2)*0.50*(1-Se_triplate))+((1-0.50)*(1-Sp_triplate))
  MCT_11t_50 <- (0.50*(1-Se_triplate))+((1-0.50)*(1-Sp_triplate))
  MCT_21t_50 <- ((2)*0.50*(1-Se_triplate))+((1-0.50)*(1-Sp_triplate))
  MCT_31t_50 <- ((3)*0.50*(1-Se_triplate))+((1-0.50)*(1-Sp_triplate))

  MCT_13t_80 <- ((1/3)*0.80*(1-Se_triplate))+((1-0.80)*(1-Sp_triplate))
  MCT_12t_80 <- ((1/2)*0.80*(1-Se_triplate))+((1-0.80)*(1-Sp_triplate))
  MCT_11t_80 <- (0.80*(1-Se_triplate))+((1-0.80)*(1-Sp_triplate))
  MCT_21t_80 <- ((2)*0.80*(1-Se_triplate))+((1-0.80)*(1-Sp_triplate))
  MCT_31t_80 <- ((3)*0.80*(1-Se_triplate))+((1-0.80)*(1-Sp_triplate))
  
}")

###conditional independence####
model_4tests_2pop_indep <- paste0("model{

#=== LIKELIHOOD ===#

  #=== POPULATION 1 ===#

  Pop1[1:16] ~ dmulti(p1[1:16], ",nPop1,")
  p1[1] <- Prev*Se_", TestA, "*Se_", TestB, "*Se_", TestC, "*Se_", TestD, " + (1-Prev)*(1-Sp_", TestA, ")*(1-Sp_", TestB, ")*(1-Sp_", TestC,")*(1-Sp_", TestD,")
  p1[2] <- Prev*Se_", TestA, "*(1-Se_", TestB, ")*Se_", TestC, "*Se_", TestD, " + (1-Prev)*(1-Sp_", TestA, ")*Sp_", TestB, "*(1-Sp_", TestC,")*(1-Sp_", TestD,")
  p1[3] <- Prev*(1-Se_", TestA, ")*Se_", TestB, "*Se_", TestC, "*Se_", TestD, " + (1-Prev)*Sp_", TestA, "*(1-Sp_", TestB, ")*(1-Sp_", TestC,")*(1-Sp_", TestD,")
  p1[4] <- Prev*(1-Se_", TestA, ")*(1-Se_", TestB, ")*Se_", TestC, "*Se_", TestD, " + (1-Prev)*Sp_", TestA, "*Sp_", TestB, "*(1-Sp_", TestC,")*(1-Sp_", TestD,")
  
  p1[5] <- Prev*Se_", TestA, "*Se_", TestB, "*(1-Se_", TestC,")*Se_", TestD, " + (1-Prev)*(1-Sp_", TestA, ")*(1-Sp_", TestB, ")*Sp_", TestC,"*(1-Sp_", TestD,")
  p1[6] <- Prev*Se_", TestA, "*(1-Se_", TestB, ")*(1-Se_", TestC,")*Se_", TestD, " + (1-Prev)*(1-Sp_", TestA, ")*Sp_", TestB, "*Sp_", TestC,"*(1-Sp_", TestD,")
  p1[7] <- Prev*(1-Se_", TestA, ")*Se_", TestB, "*(1-Se_", TestC,")*Se_", TestD, " + (1-Prev)*Sp_", TestA, "*(1-Sp_", TestB, ")*Sp_", TestC,"*(1-Sp_", TestD,")
  p1[8] <- Prev*(1-Se_", TestA, ")*(1-Se_", TestB, ")*(1-Se_", TestC,")*Se_", TestD, " + (1-Prev)*Sp_", TestA, "*Sp_", TestB, "*Sp_", TestC,"*(1-Sp_", TestD,")
  
  p1[9] <- Prev*Se_", TestA, "*Se_", TestB, "*Se_", TestC, "*(1-Se_", TestD, ") + (1-Prev)*(1-Sp_", TestA, ")*(1-Sp_", TestB, ")*(1-Sp_", TestC,")*Sp_", TestD, "
  p1[10] <- Prev*Se_", TestA, "*(1-Se_", TestB, ")*Se_", TestC, "*(1-Se_", TestD, ") + (1-Prev)*(1-Sp_", TestA, ")*Sp_", TestB, "*(1-Sp_", TestC,")*Sp_", TestD, "
  p1[11] <- Prev*(1-Se_", TestA, ")*Se_", TestB, "*Se_", TestC, "*(1-Se_", TestD, ") + (1-Prev)*Sp_", TestA, "*(1-Sp_", TestB, ")*(1-Sp_", TestC,")*Sp_", TestD, "
  p1[12] <- Prev*(1-Se_", TestA, ")*(1-Se_", TestB, ")*Se_", TestC, "*(1-Se_", TestD, ") + (1-Prev)*Sp_", TestA, "*Sp_", TestB, "*(1-Sp_", TestC,")*Sp_", TestD, "
  
  p1[13] <- Prev*Se_", TestA, "*Se_", TestB, "*(1-Se_", TestC,")*(1-Se_", TestD, ") + (1-Prev)*(1-Sp_", TestA, ")*(1-Sp_", TestB, ")*Sp_", TestC,"*Sp_", TestD, "
  p1[14] <- Prev*Se_", TestA, "*(1-Se_", TestB, ")*(1-Se_", TestC,")*(1-Se_", TestD, ") + (1-Prev)*(1-Sp_", TestA, ")*Sp_", TestB, "*Sp_", TestC,"*Sp_", TestD, "
  p1[15] <- Prev*(1-Se_", TestA, ")*Se_", TestB, "*(1-Se_", TestC,")*(1-Se_", TestD, ") + (1-Prev)*Sp_", TestA, "*(1-Sp_", TestB, ")*Sp_", TestC,"*Sp_", TestD, "
  p1[16] <- Prev*(1-Se_", TestA, ")*(1-Se_", TestB, ")*(1-Se_", TestC,")*(1-Se_", TestD, ") + (1-Prev)*Sp_", TestA, "*Sp_", TestB, "*Sp_", TestC,"*Sp_", TestD, "
  
  #=== POPULATION 2 ===#
    
  Pop2[1:8] ~ dmulti(p2[1:8], ",nPop2,")
  p2[1] <- Prev*Se_", TestA, "*Se_", TestB, "*Se_", TestC, " + (1-Prev)*(1-Sp_", TestA, ")*(1-Sp_", TestB,")*(1-Sp_", TestC,") 
  p2[2] <- Prev*Se_", TestA, "*Se_", TestB, "*(1-Se_", TestC, ")  + (1-Prev)*(1-Sp_", TestA, ")*(1-Sp_", TestB,")*Sp_", TestC,"
  p2[3] <- Prev*(1-Se_", TestA, ")*Se_", TestB, "*Se_", TestC, "  + (1-Prev)*Sp_", TestA, "*(1-Sp_", TestB,")*(1-Sp_", TestC,")
  p2[4] <- Prev*(1-Se_", TestA, ")*Se_", TestB, "*(1-Se_", TestC, ")  + (1-Prev)*Sp_", TestA, "*(1-Sp_", TestB,")*Sp_", TestC,"
  
  p2[5] <- Prev*Se_", TestA, "*(1-Se_", TestB,")*Se_", TestC, "  + (1-Prev)*(1-Sp_", TestA, ")*Sp_", TestB,"*(1-Sp_", TestC,") 
  p2[6] <- Prev*Se_", TestA, "*(1-Se_", TestB,")*(1-Se_", TestC, ")  + (1-Prev)*(1-Sp_", TestA, ")*Sp_", TestB,"*Sp_", TestC," 
  p2[7] <- Prev*(1-Se_", TestA, ")*(1-Se_", TestB,")*Se_", TestC, " + (1-Prev)*Sp_", TestA, "*Sp_", TestB,"*(1-Sp_", TestC,") 
  p2[8] <- Prev*(1-Se_", TestA, ")*(1-Se_", TestB,")*(1-Se_", TestC, ")  + (1-Prev)*Sp_", TestA, "*Sp_", TestB,"*Sp_", TestC," 
 


#=== PRIOR  ===#

  Prev ~ dbeta(",Prev.shapea,", ",Prev.shapeb,")    ## Prior for Prevalence
  Se_", TestA, " ~ dbeta(",Se.TestA.shapea,", ",Se.TestA.shapeb,")  ## Prior for Se of Test A
  Sp_", TestA, " ~ dbeta(",Sp.TestA.shapea,", ",Sp.TestA.shapeb,")  ## Prior for Sp of Test A
  Se_", TestB, " ~ dbeta(",Se.TestB.shapea,", ",Se.TestB.shapeb,")  ## Prior for Se of Test B
  Sp_", TestB, " ~ dbeta(",Sp.TestB.shapea,", ",Sp.TestB.shapeb,")  ## Prior for Sp of Test B
  Se_", TestC, " ~ dbeta(",Se.TestC.shapea,", ",Se.TestC.shapeb,")  ## Prior for Se of Test C
  Sp_", TestC, " ~ dbeta(",Sp.TestC.shapea,", ",Sp.TestC.shapeb,")  ## Prior for Sp of Test C
  Se_", TestD, " ~ dbeta(",Se.TestD.shapea,", ",Se.TestD.shapeb,")  ## Prior for Se of Test D
  Sp_", TestD, " ~ dbeta(",Sp.TestD.shapea,", ",Sp.TestD.shapeb,")  ## Prior for Sp of Test D
  

  #==== calculation PVs ====#
  
  PPV_lumino <- (Prev*Se_lumino)/(Prev*Se_lumino+(1-Prev)*(1-Sp_lumino))
  NPV_lumino <- ((1-Prev)*Sp_lumino)/((Prev*(1-Se_lumino))+((1-Prev)*Sp_lumino))
  
  MCT_13 <- ((1/3)*Prev*(1-Se_lumino))+((1-Prev)*(1-Sp_lumino))
  MCT_12 <- ((1/2)*Prev*(1-Se_lumino))+((1-Prev)*(1-Sp_lumino))
  MCT_11 <- (Prev*(1-Se_lumino))+((1-Prev)*(1-Sp_lumino))
  MCT_21 <- ((2)*Prev*(1-Se_lumino))+((1-Prev)*(1-Sp_lumino))
  MCT_31 <- ((3)*Prev*(1-Se_lumino))+((1-Prev)*(1-Sp_lumino))
  
}")

#write as a text (.txt) file####
write.table(model_4tests_2pop_dep, 
            file="model_4tests_2pop_dep.txt", 
            quote=FALSE, 
            sep="", 
            row.names=FALSE,
            col.names=FALSE)

inits <- list(list(Prev=0.40,
                   Se_bacterio=0.90,
                   Sp_bacterio=0.90,
                   Se_lumino=0.90,
                   Sp_lumino=0.90,
                   Se_petrifilm=0.90,
                   Sp_petrifilm=0.90,
                   Se_triplate=0.90,
                   Sp_triplate=0.90),
              
              list(Prev=0.30,
                   Se_bacterio=0.80,
                   Sp_bacterio=0.80,
                   Se_lumino=0.80,
                   Sp_lumino=0.80,
                   Se_petrifilm=0.80,
                   Sp_petrifilm=0.80,
                   Se_triplate=0.80,
                   Sp_triplate=0.80),
              
              list(Prev=0.20,
                   Se_bacterio=0.70,
                   Sp_bacterio=0.70,
                   Se_lumino=0.70,
                   Sp_lumino=0.70,
                   Se_petrifilm=0.70,
                   Sp_petrifilm=0.70,
                   Se_triplate=0.70,
                   Sp_triplate=0.70)
)

#Run the Bayesian model####
bug.out50 <- jags(data=datalist50,     #change the threshold in 'bug.out50'and 'datalist50                       
                         model.file="model_4tests_2pop_dep.txt",     
                         parameters.to.save=c("Prev", "Se_bacterio", "Sp_bacterio", 
                                              "Se_lumino", "Sp_lumino", "Se_petrifilm", 
                                              "Sp_petrifilm", "Se_triplate", "Sp_triplate",
                                              "PPV_lumino", "NPV_lumino",
                                              "PPV_lumino_30", "NPV_lumino_30",
                                              "PPV_lumino_80", "NPV_lumino_80",
                                              "PPV_lumino_50", "NPV_lumino_50",
                                              "PPV_bacterio_30", "NPV_bacterio_30",
                                              "PPV_bacterio_80", "NPV_bacterio_80",
                                              "PPV_bacterio_50", "NPV_bacterio_50",
                                              "PPV_petrifilm_30", "NPV_petrifilm_30",
                                              "PPV_petrifilm_80", "NPV_petrifilm_80",
                                              "PPV_petrifilm_50", "NPV_petrifilm_50",
                                              "PPV_triplate_30", "NPV_triplate_30",
                                              "PPV_triplate_80", "NPV_triplate_80",
                                              "PPV_triplate_50", "NPV_triplate_50",
                                              "MCT_13", "MCT_12", "MCT_11", "MCT_21", "MCT_31",
                                              "MCT_13_30", "MCT_12_30", "MCT_11_30", "MCT_21_30", "MCT_31_30",
                                              "MCT_13_50", "MCT_12_50", "MCT_11_50", "MCT_21_50", "MCT_31_50",
                                              "MCT_13_80", "MCT_12_80", "MCT_11_80", "MCT_21_80", "MCT_31_80",
                                              "MCT_13b", "MCT_12b", "MCT_11b", "MCT_21b", "MCT_31b",
                                              "MCT_13b_30", "MCT_12b_30", "MCT_11b_30", "MCT_21b_30", "MCT_31b_30",
                                              "MCT_13b_50", "MCT_12b_50", "MCT_11b_50", "MCT_21b_50", "MCT_31b_50",
                                              "MCT_13b_80", "MCT_12b_80", "MCT_11b_80", "MCT_21b_80", "MCT_31b_80",
                                              "MCT_13p", "MCT_12p", "MCT_11p", "MCT_21p", "MCT_31p",
                                              "MCT_13p_30", "MCT_12p_30", "MCT_11p_30", "MCT_21p_30", "MCT_31p_30",
                                              "MCT_13p_50", "MCT_12p_50", "MCT_11p_50", "MCT_21p_50", "MCT_31p_50",
                                              "MCT_13p_80", "MCT_12p_80", "MCT_11p_80", "MCT_21p_80", "MCT_31p_80",
                                              "MCT_13t", "MCT_12t", "MCT_11t", "MCT_21t", "MCT_31t",
                                              "MCT_13t_30", "MCT_12t_30", "MCT_11t_30", "MCT_21t_30", "MCT_31t_30",
                                              "MCT_13t_50", "MCT_12t_50", "MCT_11t_50", "MCT_21t_50", "MCT_31t_50",
                                              "MCT_13t_80", "MCT_12t_80", "MCT_11t_80", "MCT_21t_80", "MCT_31t_80",
                                              "covse_111", "covsp_111",
                                              "covse_011", "covsp_011",
                                              "covse_110", "covsp_110",
                                              "covse_101", "covsp_101",
                                              "covse_001", "covsp_001",
                                              "covse_010", "covsp_010",
                                              "covse_100", "covsp_100",
                                              "covse_000", "covsp_000"),               
                         n.chains=3,                                 
                         inits=inits,                                
                         n.iter=120000,                                
                         n.burnin=20000,                              
                         n.thin=20,                                   
                         DIC=TRUE)    

#sensitivity analyses####
bug.out150_vague <- jags(data=datalist150,                             
                model.file="model_4tests_2pop_dep_vague.txt",     
                parameters.to.save=c("Prev", "Se_bacterio", "Sp_bacterio", 
                                     "Se_lumino", "Sp_lumino", "Se_petrifilm", 
                                     "Sp_petrifilm", "Se_triplate", "Sp_triplate",
                                     "PPV_lumino", "NPV_lumino",
                                     "MCT_13", "MCT_12", "MCT_11", "MCT_21", "MCT_31",
                                     "covse_111", "covsp_111",
                                     "covse_011", "covsp_011",
                                     "covse_110", "covsp_110",
                                     "covse_101", "covsp_101",
                                     "covse_001", "covsp_001",
                                     "covse_010", "covsp_010",
                                     "covse_100", "covsp_100",
                                     "covse_000", "covsp_000"),               
                n.chains=3,                                 
                inits=inits,                                
                n.iter=120000,                                
                n.burnin=20000,                              
                n.thin=20,                                   
                DIC=TRUE)    

bug.out150_indep <- jags(data=datalist150,                             
                         model.file="model_4tests_2pop_indep.txt",     
                         parameters.to.save=c("Prev", "Se_bacterio", "Sp_bacterio", 
                                              "Se_lumino", "Sp_lumino", "Se_petrifilm", 
                                              "Sp_petrifilm", "Se_triplate", "Sp_triplate",
                                              "PPV_lumino", "NPV_lumino",
                                              "MCT_13", "MCT_12", "MCT_11", "MCT_21", "MCT_31"),               
                         n.chains=3,                                 
                         inits=inits,                                
                         n.iter=110000,                                
                         n.burnin=10000,                              
                         n.thin=20,                                   
                         DIC=TRUE)    

#diagnostics####
bug.mcmc <- as.mcmc(bug.out400)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

#dataframes####
bug50 <- print(bug.out50, digits.summary=2) 
write.csv(bug50$sims.matrix,"bug50.csv", row.names = FALSE)

bug50 <- bug50$summary %>%
  data.frame() %>%
  select(X50.,X2.5.,X97.5.) %>%
  mutate(threshold = 50,
         element = bug50[["root.short"]])
  
bug100 <- print(bug.out100, digits.summary=2) 
write.csv(bug100$sims.matrix,"bug100.csv", row.names = FALSE)

bug100 <- bug100$summary %>%
  data.frame() %>%
  select(X50.,X2.5.,X97.5.) %>%
  mutate(threshold = 100,
         element = bug100[["root.short"]])

bug150 <- print(bug.out150, digits.summary=2) 
write.csv(bug150$sims.matrix,"bug150.csv", row.names = FALSE)

bug150 <- bug150$summary %>%
  data.frame() %>%
  select(X50.,X2.5.,X97.5.) %>%
  mutate(threshold = 150,
         element = bug150[["root.short"]])

bug150 %>%
  filter(grepl("MCT", element) & grepl("t", element))

bug150_vague %>%
  filter(grepl("cov",element))

bug150_vague <- print(bug.out150_vague, digits.summary=2) 
bug150_vague <- bug150_vague$summary %>%
  data.frame() %>%
  select(X50.,X2.5.,X97.5.) %>%
  mutate(threshold = 150,
         element = bug150_vague[["root.short"]])

bug150_indep <- print(bug.out150_indep, digits.summary=2) 
bug150_indep <- bug150_indep$summary %>%
  data.frame() %>%
  select(X50.,X2.5.,X97.5.) %>%
  mutate(threshold = 150,
         element = bug150[["root.short"]])


bug200 <- print(bug.out200, digits.summary=2) 
write.csv(bug200$sims.matrix,"bug200.csv", row.names = FALSE)

bug200 <- bug200$summary %>%
  data.frame() %>%
  select(X50.,X2.5.,X97.5.) %>%
  mutate(threshold = 200,
         element = bug200[["root.short"]])

bug250 <- print(bug.out250, digits.summary=2) 
write.csv(bug250$sims.matrix,"bug250.csv", row.names = FALSE)

bug250 <- bug250$summary %>%
  data.frame() %>%
  select(X50.,X2.5.,X97.5.) %>%
  mutate(threshold = 250,
         element = bug250[["root.short"]])

bug300 <- print(bug.out300, digits.summary=2) 
write.csv(bug300$sims.matrix,"bug300.csv", row.names = FALSE)

bug300 <- bug300$summary %>%
  data.frame() %>%
  select(X50.,X2.5.,X97.5.) %>%
  mutate(threshold = 300,
         element = bug300[["root.short"]])

bug350 <- print(bug.out350, digits.summary=2) 
write.csv(bug350$sims.matrix,"bug350.csv", row.names = FALSE)

bug350 <- bug350$summary %>%
  data.frame() %>%
  select(X50.,X2.5.,X97.5.) %>%
  mutate(threshold = 350,
         element = bug350[["root.short"]])

bug400 <- print(bug.out400, digits.summary=2) 
write.csv(bug400$sims.matrix,"bug400.csv", row.names = FALSE)

bug400 <- bug400$summary %>%
  data.frame() %>%
  select(X50.,X2.5.,X97.5.) %>%
  mutate(threshold = 400,
         element = bug400[["root.short"]])


#### additional plots####
BM.l50 <- read.csv("bug50.csv")
BM.l100 <- read.csv("bug100.csv")
BM.l150 <- read.csv("bug150.csv")
BM.l200 <- read.csv("bug200.csv")
BM.l250 <- read.csv("bug250.csv")
BM.l300 <- read.csv("bug300.csv")
BM.l350 <- read.csv("bug350.csv")
BM.l400 <- read.csv("bug400.csv")



#Strategy 1: lumino, cp = 50####
data <- BM.l50
data$se <- data$Se_lumino
data$sp <- data$Sp_lumino
#PV @30% infection
data$ppv30 <- (0.30*data$se)/(0.30*data$se+(1-0.30)*(1-data$sp))
data$npv30 <- ((1-0.30)*data$sp)/((1-0.30)*data$sp+0.30*(1-data$se))
#PV @50% infection
data$ppv50 <- (0.50*data$se)/(0.50*data$se+(1-0.50)*(1-data$sp))
data$npv50 <- ((1-0.50)*data$sp)/((1-0.50)*data$sp+0.50*(1-data$se))
#PV @70% infection
data$ppv70 <- (0.70*data$se)/(0.70*data$se+(1-0.70)*(1-data$sp))
data$npv70 <- ((1-0.70)*data$sp)/((1-0.70)*data$sp+0.70*(1-data$se))

#dataframe
se <- quantile(data$se, c(0.025, 0.5, 0.975))
sp <- quantile(data$sp, c(0.025, 0.5, 0.975))
ppv30 <- quantile(data$ppv30, c(0.025, 0.5, 0.975))
npv30 <- quantile(data$npv30, c(0.025, 0.5, 0.975))
ppv50 <- quantile(data$ppv50, c(0.025, 0.5, 0.975))
npv50 <- quantile(data$npv50, c(0.025, 0.5, 0.975))
ppv70 <- quantile(data$ppv70, c(0.025, 0.5, 0.975))
npv70 <- quantile(data$npv70, c(0.025, 0.5, 0.975))

df1 <- data.frame(50, "Luminometry", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv30[2], ppv30[1], ppv30[3],
                  npv30[2], npv30[1], npv30[3],
                  ppv50[2], ppv50[1], ppv50[3],
                  npv50[2], npv50[1], npv50[3],
                  ppv70[2], ppv70[1], ppv70[3],
                  npv70[2], npv70[1], npv70[3]
                  
)
rownames(df1) <- NULL
colnames(df1) <- c("threshold", 
                   "test","se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv30", "ppv30_low", "ppv30_hi",
                   "npv30", "npv30_low", "npv30_hi",
                   "ppv50", "ppv50_low", "ppv50_hi",
                   "npv50", "npv50_low", "npv50_hi",
                   "ppv70", "ppv70_low", "ppv70_hi",
                   "npv70", "npv70_low", "npv70_hi"
)  



#Strategy 2: lumino, cp = 100####
data <- BM.l100
data$se <- data$Se_lumino
data$sp <- data$Sp_lumino
#PV @30% infection
data$ppv30 <- (0.30*data$se)/(0.30*data$se+(1-0.30)*(1-data$sp))
data$npv30 <- ((1-0.30)*data$sp)/((1-0.30)*data$sp+0.30*(1-data$se))
#PV @50% infection
data$ppv50 <- (0.50*data$se)/(0.50*data$se+(1-0.50)*(1-data$sp))
data$npv50 <- ((1-0.50)*data$sp)/((1-0.50)*data$sp+0.50*(1-data$se))
#PV @70% infection
data$ppv70 <- (0.70*data$se)/(0.70*data$se+(1-0.70)*(1-data$sp))
data$npv70 <- ((1-0.70)*data$sp)/((1-0.70)*data$sp+0.70*(1-data$se))

#dataframe
se <- quantile(data$se, c(0.025, 0.5, 0.975))
sp <- quantile(data$sp, c(0.025, 0.5, 0.975))
ppv30 <- quantile(data$ppv30, c(0.025, 0.5, 0.975))
npv30 <- quantile(data$npv30, c(0.025, 0.5, 0.975))
ppv50 <- quantile(data$ppv50, c(0.025, 0.5, 0.975))
npv50 <- quantile(data$npv50, c(0.025, 0.5, 0.975))
ppv70 <- quantile(data$ppv70, c(0.025, 0.5, 0.975))
npv70 <- quantile(data$npv70, c(0.025, 0.5, 0.975))

df2 <- data.frame(100, "Luminometry", 
                  se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv30[2], ppv30[1], ppv30[3],
                  npv30[2], npv30[1], npv30[3],
                  ppv50[2], ppv50[1], ppv50[3],
                  npv50[2], npv50[1], npv50[3],
                  ppv70[2], ppv70[1], ppv70[3],
                  npv70[2], npv70[1], npv70[3]
                  
)
rownames(df2) <- NULL
colnames(df2) <- c("threshold", 
                   "test","se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv30", "ppv30_low", "ppv30_hi",
                   "npv30", "npv30_low", "npv30_hi",
                   "ppv50", "ppv50_low", "ppv50_hi",
                   "npv50", "npv50_low", "npv50_hi",
                   "ppv70", "ppv70_low", "ppv70_hi",
                   "npv70", "npv70_low", "npv70_hi"
)  


#Strategy 3: lumino, cp = 150####
data <- BM.l150
data$se <- data$Se_lumino
data$sp <- data$Sp_lumino
#PV @30% infection
data$ppv30 <- (0.30*data$se)/(0.30*data$se+(1-0.30)*(1-data$sp))
data$npv30 <- ((1-0.30)*data$sp)/((1-0.30)*data$sp+0.30*(1-data$se))
#PV @50% infection
data$ppv50 <- (0.50*data$se)/(0.50*data$se+(1-0.50)*(1-data$sp))
data$npv50 <- ((1-0.50)*data$sp)/((1-0.50)*data$sp+0.50*(1-data$se))
#PV @70% infection
data$ppv70 <- (0.70*data$se)/(0.70*data$se+(1-0.70)*(1-data$sp))
data$npv70 <- ((1-0.70)*data$sp)/((1-0.70)*data$sp+0.70*(1-data$se))

#dataframe
se <- quantile(data$se, c(0.025, 0.5, 0.975))
sp <- quantile(data$sp, c(0.025, 0.5, 0.975))
ppv30 <- quantile(data$ppv30, c(0.025, 0.5, 0.975))
npv30 <- quantile(data$npv30, c(0.025, 0.5, 0.975))
ppv50 <- quantile(data$ppv50, c(0.025, 0.5, 0.975))
npv50 <- quantile(data$npv50, c(0.025, 0.5, 0.975))
ppv70 <- quantile(data$ppv70, c(0.025, 0.5, 0.975))
npv70 <- quantile(data$npv70, c(0.025, 0.5, 0.975))

df3 <- data.frame(150, "Luminometry", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv30[2], ppv30[1], ppv30[3],
                  npv30[2], npv30[1], npv30[3],
                  ppv50[2], ppv50[1], ppv50[3],
                  npv50[2], npv50[1], npv50[3],
                  ppv70[2], ppv70[1], ppv70[3],
                  npv70[2], npv70[1], npv70[3]
                  
)
rownames(df3) <- NULL
colnames(df3) <- c("threshold", 
                   "test","se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv30", "ppv30_low", "ppv30_hi",
                   "npv30", "npv30_low", "npv30_hi",
                   "ppv50", "ppv50_low", "ppv50_hi",
                   "npv50", "npv50_low", "npv50_hi",
                   "ppv70", "ppv70_low", "ppv70_hi",
                   "npv70", "npv70_low", "npv70_hi"
)  


#Strategy 4: lumino, cp = 200####
data <- BM.l200
data$se <- data$Se_lumino
data$sp <- data$Sp_lumino
#PV @30% infection
data$ppv30 <- (0.30*data$se)/(0.30*data$se+(1-0.30)*(1-data$sp))
data$npv30 <- ((1-0.30)*data$sp)/((1-0.30)*data$sp+0.30*(1-data$se))
#PV @50% infection
data$ppv50 <- (0.50*data$se)/(0.50*data$se+(1-0.50)*(1-data$sp))
data$npv50 <- ((1-0.50)*data$sp)/((1-0.50)*data$sp+0.50*(1-data$se))
#PV @70% infection
data$ppv70 <- (0.70*data$se)/(0.70*data$se+(1-0.70)*(1-data$sp))
data$npv70 <- ((1-0.70)*data$sp)/((1-0.70)*data$sp+0.70*(1-data$se))

#dataframe
se <- quantile(data$se, c(0.025, 0.5, 0.975))
sp <- quantile(data$sp, c(0.025, 0.5, 0.975))
ppv30 <- quantile(data$ppv30, c(0.025, 0.5, 0.975))
npv30 <- quantile(data$npv30, c(0.025, 0.5, 0.975))
ppv50 <- quantile(data$ppv50, c(0.025, 0.5, 0.975))
npv50 <- quantile(data$npv50, c(0.025, 0.5, 0.975))
ppv70 <- quantile(data$ppv70, c(0.025, 0.5, 0.975))
npv70 <- quantile(data$npv70, c(0.025, 0.5, 0.975))

df4 <- data.frame(200, "Luminometry", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv30[2], ppv30[1], ppv30[3],
                  npv30[2], npv30[1], npv30[3],
                  ppv50[2], ppv50[1], ppv50[3],
                  npv50[2], npv50[1], npv50[3],
                  ppv70[2], ppv70[1], ppv70[3],
                  npv70[2], npv70[1], npv70[3]
                  
)
rownames(df4) <- NULL
colnames(df4) <- c("threshold", 
                   "test","se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv30", "ppv30_low", "ppv30_hi",
                   "npv30", "npv30_low", "npv30_hi",
                   "ppv50", "ppv50_low", "ppv50_hi",
                   "npv50", "npv50_low", "npv50_hi",
                   "ppv70", "ppv70_low", "ppv70_hi",
                   "npv70", "npv70_low", "npv70_hi"
)  


#Strategy 5: lumino, cp = 250####
data <- BM.l250
data$se <- data$Se_lumino
data$sp <- data$Sp_lumino
#PV @30% infection
data$ppv30 <- (0.30*data$se)/(0.30*data$se+(1-0.30)*(1-data$sp))
data$npv30 <- ((1-0.30)*data$sp)/((1-0.30)*data$sp+0.30*(1-data$se))
#PV @50% infection
data$ppv50 <- (0.50*data$se)/(0.50*data$se+(1-0.50)*(1-data$sp))
data$npv50 <- ((1-0.50)*data$sp)/((1-0.50)*data$sp+0.50*(1-data$se))
#PV @70% infection
data$ppv70 <- (0.70*data$se)/(0.70*data$se+(1-0.70)*(1-data$sp))
data$npv70 <- ((1-0.70)*data$sp)/((1-0.70)*data$sp+0.70*(1-data$se))

#dataframe
se <- quantile(data$se, c(0.025, 0.5, 0.975))
sp <- quantile(data$sp, c(0.025, 0.5, 0.975))
ppv30 <- quantile(data$ppv30, c(0.025, 0.5, 0.975))
npv30 <- quantile(data$npv30, c(0.025, 0.5, 0.975))
ppv50 <- quantile(data$ppv50, c(0.025, 0.5, 0.975))
npv50 <- quantile(data$npv50, c(0.025, 0.5, 0.975))
ppv70 <- quantile(data$ppv70, c(0.025, 0.5, 0.975))
npv70 <- quantile(data$npv70, c(0.025, 0.5, 0.975))

df5 <- data.frame(250, "Luminometry", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv30[2], ppv30[1], ppv30[3],
                  npv30[2], npv30[1], npv30[3],
                  ppv50[2], ppv50[1], ppv50[3],
                  npv50[2], npv50[1], npv50[3],
                  ppv70[2], ppv70[1], ppv70[3],
                  npv70[2], npv70[1], npv70[3]
                  
)
rownames(df5) <- NULL
colnames(df5) <- c("threshold", 
                   "test","se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv30", "ppv30_low", "ppv30_hi",
                   "npv30", "npv30_low", "npv30_hi",
                   "ppv50", "ppv50_low", "ppv50_hi",
                   "npv50", "npv50_low", "npv50_hi",
                   "ppv70", "ppv70_low", "ppv70_hi",
                   "npv70", "npv70_low", "npv70_hi"
)  


#Strategy 6: lumino, cp = 300####
data <- BM.l300
data$se <- data$Se_lumino
data$sp <- data$Sp_lumino
#PV @30% infection
data$ppv30 <- (0.30*data$se)/(0.30*data$se+(1-0.30)*(1-data$sp))
data$npv30 <- ((1-0.30)*data$sp)/((1-0.30)*data$sp+0.30*(1-data$se))
#PV @50% infection
data$ppv50 <- (0.50*data$se)/(0.50*data$se+(1-0.50)*(1-data$sp))
data$npv50 <- ((1-0.50)*data$sp)/((1-0.50)*data$sp+0.50*(1-data$se))
#PV @70% infection
data$ppv70 <- (0.70*data$se)/(0.70*data$se+(1-0.70)*(1-data$sp))
data$npv70 <- ((1-0.70)*data$sp)/((1-0.70)*data$sp+0.70*(1-data$se))

#dataframe
se <- quantile(data$se, c(0.025, 0.5, 0.975))
sp <- quantile(data$sp, c(0.025, 0.5, 0.975))
ppv30 <- quantile(data$ppv30, c(0.025, 0.5, 0.975))
npv30 <- quantile(data$npv30, c(0.025, 0.5, 0.975))
ppv50 <- quantile(data$ppv50, c(0.025, 0.5, 0.975))
npv50 <- quantile(data$npv50, c(0.025, 0.5, 0.975))
ppv70 <- quantile(data$ppv70, c(0.025, 0.5, 0.975))
npv70 <- quantile(data$npv70, c(0.025, 0.5, 0.975))

df6 <- data.frame(300, "Luminometry", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv30[2], ppv30[1], ppv30[3],
                  npv30[2], npv30[1], npv30[3],
                  ppv50[2], ppv50[1], ppv50[3],
                  npv50[2], npv50[1], npv50[3],
                  ppv70[2], ppv70[1], ppv70[3],
                  npv70[2], npv70[1], npv70[3]
                  
)
rownames(df6) <- NULL
colnames(df6) <- c("threshold", 
                   "test","se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv30", "ppv30_low", "ppv30_hi",
                   "npv30", "npv30_low", "npv30_hi",
                   "ppv50", "ppv50_low", "ppv50_hi",
                   "npv50", "npv50_low", "npv50_hi",
                   "ppv70", "ppv70_low", "ppv70_hi",
                   "npv70", "npv70_low", "npv70_hi"
)  


#Strategy 7: lumino, cp = 350####
data <- BM.l350
data$se <- data$Se_lumino
data$sp <- data$Sp_lumino
#PV @30% infection
data$ppv30 <- (0.30*data$se)/(0.30*data$se+(1-0.30)*(1-data$sp))
data$npv30 <- ((1-0.30)*data$sp)/((1-0.30)*data$sp+0.30*(1-data$se))
#PV @50% infection
data$ppv50 <- (0.50*data$se)/(0.50*data$se+(1-0.50)*(1-data$sp))
data$npv50 <- ((1-0.50)*data$sp)/((1-0.50)*data$sp+0.50*(1-data$se))
#PV @70% infection
data$ppv70 <- (0.70*data$se)/(0.70*data$se+(1-0.70)*(1-data$sp))
data$npv70 <- ((1-0.70)*data$sp)/((1-0.70)*data$sp+0.70*(1-data$se))

#dataframe
se <- quantile(data$se, c(0.025, 0.5, 0.975))
sp <- quantile(data$sp, c(0.025, 0.5, 0.975))
ppv30 <- quantile(data$ppv30, c(0.025, 0.5, 0.975))
npv30 <- quantile(data$npv30, c(0.025, 0.5, 0.975))
ppv50 <- quantile(data$ppv50, c(0.025, 0.5, 0.975))
npv50 <- quantile(data$npv50, c(0.025, 0.5, 0.975))
ppv70 <- quantile(data$ppv70, c(0.025, 0.5, 0.975))
npv70 <- quantile(data$npv70, c(0.025, 0.5, 0.975))

df7 <- data.frame(350, "Luminometry", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv30[2], ppv30[1], ppv30[3],
                  npv30[2], npv30[1], npv30[3],
                  ppv50[2], ppv50[1], ppv50[3],
                  npv50[2], npv50[1], npv50[3],
                  ppv70[2], ppv70[1], ppv70[3],
                  npv70[2], npv70[1], npv70[3]
                  
)
rownames(df7) <- NULL
colnames(df7) <- c("threshold", 
                   "test","se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv30", "ppv30_low", "ppv30_hi",
                   "npv30", "npv30_low", "npv30_hi",
                   "ppv50", "ppv50_low", "ppv50_hi",
                   "npv50", "npv50_low", "npv50_hi",
                   "ppv70", "ppv70_low", "ppv70_hi",
                   "npv70", "npv70_low", "npv70_hi"
)  


#Strategy 8: lumino, cp = 400####
data <- BM.l400
data$se <- data$Se_lumino
data$sp <- data$Sp_lumino
#PV @30% infection
data$ppv30 <- (0.30*data$se)/(0.30*data$se+(1-0.30)*(1-data$sp))
data$npv30 <- ((1-0.30)*data$sp)/((1-0.30)*data$sp+0.30*(1-data$se))
#PV @50% infection
data$ppv50 <- (0.50*data$se)/(0.50*data$se+(1-0.50)*(1-data$sp))
data$npv50 <- ((1-0.50)*data$sp)/((1-0.50)*data$sp+0.50*(1-data$se))
#PV @70% infection
data$ppv70 <- (0.70*data$se)/(0.70*data$se+(1-0.70)*(1-data$sp))
data$npv70 <- ((1-0.70)*data$sp)/((1-0.70)*data$sp+0.70*(1-data$se))

#dataframe
se <- quantile(data$se, c(0.025, 0.5, 0.975))
sp <- quantile(data$sp, c(0.025, 0.5, 0.975))
ppv30 <- quantile(data$ppv30, c(0.025, 0.5, 0.975))
npv30 <- quantile(data$npv30, c(0.025, 0.5, 0.975))
ppv50 <- quantile(data$ppv50, c(0.025, 0.5, 0.975))
npv50 <- quantile(data$npv50, c(0.025, 0.5, 0.975))
ppv70 <- quantile(data$ppv70, c(0.025, 0.5, 0.975))
npv70 <- quantile(data$npv70, c(0.025, 0.5, 0.975))

df8 <- data.frame(400, "Luminometry", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv30[2], ppv30[1], ppv30[3],
                  npv30[2], npv30[1], npv30[3],
                  ppv50[2], ppv50[1], ppv50[3],
                  npv50[2], npv50[1], npv50[3],
                  ppv70[2], ppv70[1], ppv70[3],
                  npv70[2], npv70[1], npv70[3]
                  
)
rownames(df8) <- NULL
colnames(df8) <- c("threshold", 
                   "test","se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv30", "ppv30_low", "ppv30_hi",
                   "npv30", "npv30_low", "npv30_hi",
                   "ppv50", "ppv50_low", "ppv50_hi",
                   "npv50", "npv50_low", "npv50_hi",
                   "ppv70", "ppv70_low", "ppv70_hi",
                   "npv70", "npv70_low", "npv70_hi"
)  



#Strategy 9: Petrifilm#### 
data <- BM.l150
data$se <- data$Se_petrifilm
data$sp <- data$Sp_petrifilm
#PV @30% infection
data$ppv30 <- (0.30*data$se)/(0.30*data$se+(1-0.30)*(1-data$sp))
data$npv30 <- ((1-0.30)*data$sp)/((1-0.30)*data$sp+0.30*(1-data$se))
#PV @50% infection
data$ppv50 <- (0.50*data$se)/(0.50*data$se+(1-0.50)*(1-data$sp))
data$npv50 <- ((1-0.50)*data$sp)/((1-0.50)*data$sp+0.50*(1-data$se))
#PV @70% infection
data$ppv70 <- (0.70*data$se)/(0.70*data$se+(1-0.70)*(1-data$sp))
data$npv70 <- ((1-0.70)*data$sp)/((1-0.70)*data$sp+0.70*(1-data$se))
#MCT @30% infection
data$mct30_5_1 <- (1-0.30)*(1-data$sp) + (5*0.30)*(1-data$se)
data$mct30_3_1 <- (1-0.30)*(1-data$sp) + (3*0.30)*(1-data$se)
data$mct30_1_1 <- (1-0.30)*(1-data$sp) + (0.30)*(1-data$se)
data$mct30_1_3 <- (1-0.30)*(1-data$sp) + (0.333*0.30)*(1-data$se)
data$mct30_1_5 <- (1-0.30)*(1-data$sp) + (0.2*0.30)*(1-data$se)
#MCT @50% infection
data$mct50_5_1 <- (1-0.50)*(1-data$sp) + (5*0.50)*(1-data$se)
data$mct50_3_1 <- (1-0.50)*(1-data$sp) + (3*0.50)*(1-data$se)
data$mct50_1_1 <- (1-0.50)*(1-data$sp) + (0.50)*(1-data$se)
data$mct50_1_3 <- (1-0.50)*(1-data$sp) + (0.333*0.50)*(1-data$se)
data$mct50_1_5 <- (1-0.50)*(1-data$sp) + (0.2*0.50)*(1-data$se)
#MCT @70% infection
data$mct70_5_1 <- (1-0.70)*(1-data$sp) + (5*0.70)*(1-data$se)
data$mct70_3_1 <- (1-0.70)*(1-data$sp) + (3*0.70)*(1-data$se)
data$mct70_1_1 <- (1-0.70)*(1-data$sp) + (0.70)*(1-data$se)
data$mct70_1_3 <- (1-0.70)*(1-data$sp) + (0.333*0.70)*(1-data$se)
data$mct70_1_5 <- (1-0.70)*(1-data$sp) + (0.2*0.70)*(1-data$se)

#dataframe
se <- quantile(data$se, c(0.025, 0.5, 0.975))
sp <- quantile(data$sp, c(0.025, 0.5, 0.975))
ppv30 <- quantile(data$ppv30, c(0.025, 0.5, 0.975))
npv30 <- quantile(data$npv30, c(0.025, 0.5, 0.975))
ppv50 <- quantile(data$ppv50, c(0.025, 0.5, 0.975))
npv50 <- quantile(data$npv50, c(0.025, 0.5, 0.975))
ppv70 <- quantile(data$ppv70, c(0.025, 0.5, 0.975))
npv70 <- quantile(data$npv70, c(0.025, 0.5, 0.975))
mct30_5_1 <- quantile(data$mct30_5_1, c(0.025, 0.5, 0.975))
mct30_3_1 <- quantile(data$mct30_3_1, c(0.025, 0.5, 0.975))
mct30_1_1 <- quantile(data$mct30_1_1, c(0.025, 0.5, 0.975))
mct30_1_3 <- quantile(data$mct30_1_3, c(0.025, 0.5, 0.975))
mct30_1_5 <- quantile(data$mct30_1_5, c(0.025, 0.5, 0.975))
mct50_5_1 <- quantile(data$mct50_5_1, c(0.025, 0.5, 0.975))
mct50_3_1 <- quantile(data$mct50_3_1, c(0.025, 0.5, 0.975))
mct50_1_1 <- quantile(data$mct50_1_1, c(0.025, 0.5, 0.975))
mct50_1_3 <- quantile(data$mct50_1_3, c(0.025, 0.5, 0.975))
mct50_1_5 <- quantile(data$mct50_1_5, c(0.025, 0.5, 0.975))
mct70_5_1 <- quantile(data$mct70_5_1, c(0.025, 0.5, 0.975))
mct70_3_1 <- quantile(data$mct70_3_1, c(0.025, 0.5, 0.975))
mct70_1_1 <- quantile(data$mct70_1_1, c(0.025, 0.5, 0.975))
mct70_1_3 <- quantile(data$mct70_1_3, c(0.025, 0.5, 0.975))
mct70_1_5 <- quantile(data$mct70_1_5, c(0.025, 0.5, 0.975))

df9 <- data.frame("P","Petrifilm (P)", 
                  se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv30[2], ppv30[1], ppv30[3],
                  npv30[2], npv30[1], npv30[3],
                  ppv50[2], ppv50[1], ppv50[3],
                  npv50[2], npv50[1], npv50[3],
                  ppv70[2], ppv70[1], ppv70[3],
                  npv70[2], npv70[1], npv70[3],
                  
                  mct30_5_1[2], mct30_5_1[1], mct30_5_1[3],
                  mct30_3_1[2], mct30_3_1[1], mct30_3_1[3],
                  mct30_1_1[2], mct30_1_1[1], mct30_1_1[3],
                  mct30_1_3[2], mct30_1_3[1], mct30_1_3[3],
                  mct30_1_5[2], mct30_1_5[1], mct30_1_5[3],
                  
                  mct50_5_1[2], mct50_5_1[1], mct50_5_1[3],
                  mct50_3_1[2], mct50_3_1[1], mct50_3_1[3],
                  mct50_1_1[2], mct50_1_1[1], mct50_1_1[3],
                  mct50_1_3[2], mct50_1_3[1], mct50_1_3[3],
                  mct50_1_5[2], mct50_1_5[1], mct50_1_5[3],
                  
                  mct70_5_1[2], mct70_5_1[1], mct70_5_1[3],
                  mct70_3_1[2], mct70_3_1[1], mct70_3_1[3],
                  mct70_1_1[2], mct70_1_1[1], mct70_1_1[3],
                  mct70_1_3[2], mct70_1_3[1], mct70_1_3[3],
                  mct70_1_5[2], mct70_1_5[1], mct70_1_5[3]
)
rownames(df9) <- NULL
colnames(df9) <- c("threshold", 
                   "test", "se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv30", "ppv30_low", "ppv30_hi",
                   "npv30", "npv30_low", "npv30_hi",
                   "ppv50", "ppv50_low", "ppv50_hi",
                   "npv50", "npv50_low", "npv50_hi",
                   "ppv70", "ppv70_low", "ppv70_hi",
                   "npv70", "npv70_low", "npv70_hi",
                   
                   "mct30_5_1", "mct30_5_1_low", "mct30_5_1_hi",
                   "mct30_3_1", "mct30_3_1_low", "mct30_3_1_hi",
                   "mct30_1_1", "mct30_1_1_low", "mct30_1_1_hi",
                   "mct30_1_3", "mct30_1_3_low", "mct30_1_3_hi",
                   "mct30_1_5", "mct30_1_5_low", "mct30_1_5_hi",
                   
                   "mct50_5_1", "mct50_5_1_low", "mct50_5_1_hi",
                   "mct50_3_1", "mct50_3_1_low", "mct50_3_1_hi",
                   "mct50_1_1", "mct50_1_1_low", "mct50_1_1_hi",
                   "mct50_1_3", "mct50_1_3_low", "mct50_1_3_hi",
                   "mct50_1_5", "mct50_1_5_low", "mct50_1_5_hi",
                   
                   "mct70_5_1", "mct70_5_1_low", "mct70_5_1_hi",
                   "mct70_3_1", "mct70_3_1_low", "mct70_3_1_hi",
                   "mct70_1_1", "mct70_1_1_low", "mct70_1_1_hi",
                   "mct70_1_3", "mct70_1_3_low", "mct70_1_3_hi",
                   "mct70_1_5", "mct70_1_5_low", "mct70_1_5_hi"
)  

#Strategy 10: Triplate#### 
data <- BM.l150
data$se <- data$Se_triplate
data$sp <- data$Sp_triplate
#PV @30% infection
data$ppv30 <- (0.30*data$se)/(0.30*data$se+(1-0.30)*(1-data$sp))
data$npv30 <- ((1-0.30)*data$sp)/((1-0.30)*data$sp+0.30*(1-data$se))
#PV @50% infection
data$ppv50 <- (0.50*data$se)/(0.50*data$se+(1-0.50)*(1-data$sp))
data$npv50 <- ((1-0.50)*data$sp)/((1-0.50)*data$sp+0.50*(1-data$se))
#PV @70% infection
data$ppv70 <- (0.70*data$se)/(0.70*data$se+(1-0.70)*(1-data$sp))
data$npv70 <- ((1-0.70)*data$sp)/((1-0.70)*data$sp+0.70*(1-data$se))
#MCT @30% infection
data$mct30_5_1 <- (1-0.30)*(1-data$sp) + (5*0.30)*(1-data$se)
data$mct30_3_1 <- (1-0.30)*(1-data$sp) + (3*0.30)*(1-data$se)
data$mct30_1_1 <- (1-0.30)*(1-data$sp) + (0.30)*(1-data$se)
data$mct30_1_3 <- (1-0.30)*(1-data$sp) + (0.333*0.30)*(1-data$se)
data$mct30_1_5 <- (1-0.30)*(1-data$sp) + (0.2*0.30)*(1-data$se)
#MCT @50% infection
data$mct50_5_1 <- (1-0.50)*(1-data$sp) + (5*0.50)*(1-data$se)
data$mct50_3_1 <- (1-0.50)*(1-data$sp) + (3*0.50)*(1-data$se)
data$mct50_1_1 <- (1-0.50)*(1-data$sp) + (0.50)*(1-data$se)
data$mct50_1_3 <- (1-0.50)*(1-data$sp) + (0.333*0.50)*(1-data$se)
data$mct50_1_5 <- (1-0.50)*(1-data$sp) + (0.2*0.50)*(1-data$se)
#MCT @70% infection
data$mct70_5_1 <- (1-0.70)*(1-data$sp) + (5*0.70)*(1-data$se)
data$mct70_3_1 <- (1-0.70)*(1-data$sp) + (3*0.70)*(1-data$se)
data$mct70_1_1 <- (1-0.70)*(1-data$sp) + (0.70)*(1-data$se)
data$mct70_1_3 <- (1-0.70)*(1-data$sp) + (0.333*0.70)*(1-data$se)
data$mct70_1_5 <- (1-0.70)*(1-data$sp) + (0.2*0.70)*(1-data$se)

#dataframe
se <- quantile(data$se, c(0.025, 0.5, 0.975))
sp <- quantile(data$sp, c(0.025, 0.5, 0.975))
ppv30 <- quantile(data$ppv30, c(0.025, 0.5, 0.975))
npv30 <- quantile(data$npv30, c(0.025, 0.5, 0.975))
ppv50 <- quantile(data$ppv50, c(0.025, 0.5, 0.975))
npv50 <- quantile(data$npv50, c(0.025, 0.5, 0.975))
ppv70 <- quantile(data$ppv70, c(0.025, 0.5, 0.975))
npv70 <- quantile(data$npv70, c(0.025, 0.5, 0.975))
mct30_5_1 <- quantile(data$mct30_5_1, c(0.025, 0.5, 0.975))
mct30_3_1 <- quantile(data$mct30_3_1, c(0.025, 0.5, 0.975))
mct30_1_1 <- quantile(data$mct30_1_1, c(0.025, 0.5, 0.975))
mct30_1_3 <- quantile(data$mct30_1_3, c(0.025, 0.5, 0.975))
mct30_1_5 <- quantile(data$mct30_1_5, c(0.025, 0.5, 0.975))
mct50_5_1 <- quantile(data$mct50_5_1, c(0.025, 0.5, 0.975))
mct50_3_1 <- quantile(data$mct50_3_1, c(0.025, 0.5, 0.975))
mct50_1_1 <- quantile(data$mct50_1_1, c(0.025, 0.5, 0.975))
mct50_1_3 <- quantile(data$mct50_1_3, c(0.025, 0.5, 0.975))
mct50_1_5 <- quantile(data$mct50_1_5, c(0.025, 0.5, 0.975))
mct70_5_1 <- quantile(data$mct70_5_1, c(0.025, 0.5, 0.975))
mct70_3_1 <- quantile(data$mct70_3_1, c(0.025, 0.5, 0.975))
mct70_1_1 <- quantile(data$mct70_1_1, c(0.025, 0.5, 0.975))
mct70_1_3 <- quantile(data$mct70_1_3, c(0.025, 0.5, 0.975))
mct70_1_5 <- quantile(data$mct70_1_5, c(0.025, 0.5, 0.975))

df10 <- data.frame("T","Tri-Plate (T)", 
                  se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv30[2], ppv30[1], ppv30[3],
                  npv30[2], npv30[1], npv30[3],
                  ppv50[2], ppv50[1], ppv50[3],
                  npv50[2], npv50[1], npv50[3],
                  ppv70[2], ppv70[1], ppv70[3],
                  npv70[2], npv70[1], npv70[3],
                  
                  mct30_5_1[2], mct30_5_1[1], mct30_5_1[3],
                  mct30_3_1[2], mct30_3_1[1], mct30_3_1[3],
                  mct30_1_1[2], mct30_1_1[1], mct30_1_1[3],
                  mct30_1_3[2], mct30_1_3[1], mct30_1_3[3],
                  mct30_1_5[2], mct30_1_5[1], mct30_1_5[3],
                  
                  mct50_5_1[2], mct50_5_1[1], mct50_5_1[3],
                  mct50_3_1[2], mct50_3_1[1], mct50_3_1[3],
                  mct50_1_1[2], mct50_1_1[1], mct50_1_1[3],
                  mct50_1_3[2], mct50_1_3[1], mct50_1_3[3],
                  mct50_1_5[2], mct50_1_5[1], mct50_1_5[3],
                  
                  mct70_5_1[2], mct70_5_1[1], mct70_5_1[3],
                  mct70_3_1[2], mct70_3_1[1], mct70_3_1[3],
                  mct70_1_1[2], mct70_1_1[1], mct70_1_1[3],
                  mct70_1_3[2], mct70_1_3[1], mct70_1_3[3],
                  mct70_1_5[2], mct70_1_5[1], mct70_1_5[3]
)
rownames(df10) <- NULL
colnames(df10) <- c("threshold", 
                   "test", "se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv30", "ppv30_low", "ppv30_hi",
                   "npv30", "npv30_low", "npv30_hi",
                   "ppv50", "ppv50_low", "ppv50_hi",
                   "npv50", "npv50_low", "npv50_hi",
                   "ppv70", "ppv70_low", "ppv70_hi",
                   "npv70", "npv70_low", "npv70_hi",
                   
                   "mct30_5_1", "mct30_5_1_low", "mct30_5_1_hi",
                   "mct30_3_1", "mct30_3_1_low", "mct30_3_1_hi",
                   "mct30_1_1", "mct30_1_1_low", "mct30_1_1_hi",
                   "mct30_1_3", "mct30_1_3_low", "mct30_1_3_hi",
                   "mct30_1_5", "mct30_1_5_low", "mct30_1_5_hi",
                   
                   "mct50_5_1", "mct50_5_1_low", "mct50_5_1_hi",
                   "mct50_3_1", "mct50_3_1_low", "mct50_3_1_hi",
                   "mct50_1_1", "mct50_1_1_low", "mct50_1_1_hi",
                   "mct50_1_3", "mct50_1_3_low", "mct50_1_3_hi",
                   "mct50_1_5", "mct50_1_5_low", "mct50_1_5_hi",
                   
                   "mct70_5_1", "mct70_5_1_low", "mct70_5_1_hi",
                   "mct70_3_1", "mct70_3_1_low", "mct70_3_1_hi",
                   "mct70_1_1", "mct70_1_1_low", "mct70_1_1_hi",
                   "mct70_1_3", "mct70_1_3_low", "mct70_1_3_hi",
                   "mct70_1_5", "mct70_1_5_low", "mct70_1_5_hi"
)  

#Strategy 11: Labo#### 
data <- BM.l150
data$se <- data$Se_bacterio
data$sp <- data$Sp_bacterio
#PV @30% infection
data$ppv30 <- (0.30*data$se)/(0.30*data$se+(1-0.30)*(1-data$sp))
data$npv30 <- ((1-0.30)*data$sp)/((1-0.30)*data$sp+0.30*(1-data$se))
#PV @50% infection
data$ppv50 <- (0.50*data$se)/(0.50*data$se+(1-0.50)*(1-data$sp))
data$npv50 <- ((1-0.50)*data$sp)/((1-0.50)*data$sp+0.50*(1-data$se))
#PV @70% infection
data$ppv70 <- (0.70*data$se)/(0.70*data$se+(1-0.70)*(1-data$sp))
data$npv70 <- ((1-0.70)*data$sp)/((1-0.70)*data$sp+0.70*(1-data$se))
#MCT @30% infection
data$mct30_5_1 <- (1-0.30)*(1-data$sp) + (5*0.30)*(1-data$se)
data$mct30_3_1 <- (1-0.30)*(1-data$sp) + (3*0.30)*(1-data$se)
data$mct30_1_1 <- (1-0.30)*(1-data$sp) + (0.30)*(1-data$se)
data$mct30_1_3 <- (1-0.30)*(1-data$sp) + (0.333*0.30)*(1-data$se)
data$mct30_1_5 <- (1-0.30)*(1-data$sp) + (0.2*0.30)*(1-data$se)
#MCT @50% infection
data$mct50_5_1 <- (1-0.50)*(1-data$sp) + (5*0.50)*(1-data$se)
data$mct50_3_1 <- (1-0.50)*(1-data$sp) + (3*0.50)*(1-data$se)
data$mct50_1_1 <- (1-0.50)*(1-data$sp) + (0.50)*(1-data$se)
data$mct50_1_3 <- (1-0.50)*(1-data$sp) + (0.333*0.50)*(1-data$se)
data$mct50_1_5 <- (1-0.50)*(1-data$sp) + (0.2*0.50)*(1-data$se)
#MCT @70% infection
data$mct70_5_1 <- (1-0.70)*(1-data$sp) + (5*0.70)*(1-data$se)
data$mct70_3_1 <- (1-0.70)*(1-data$sp) + (3*0.70)*(1-data$se)
data$mct70_1_1 <- (1-0.70)*(1-data$sp) + (0.70)*(1-data$se)
data$mct70_1_3 <- (1-0.70)*(1-data$sp) + (0.333*0.70)*(1-data$se)
data$mct70_1_5 <- (1-0.70)*(1-data$sp) + (0.2*0.70)*(1-data$se)

#dataframe
se <- quantile(data$se, c(0.025, 0.5, 0.975))
sp <- quantile(data$sp, c(0.025, 0.5, 0.975))
ppv30 <- quantile(data$ppv30, c(0.025, 0.5, 0.975))
npv30 <- quantile(data$npv30, c(0.025, 0.5, 0.975))
ppv50 <- quantile(data$ppv50, c(0.025, 0.5, 0.975))
npv50 <- quantile(data$npv50, c(0.025, 0.5, 0.975))
ppv70 <- quantile(data$ppv70, c(0.025, 0.5, 0.975))
npv70 <- quantile(data$npv70, c(0.025, 0.5, 0.975))
mct30_5_1 <- quantile(data$mct30_5_1, c(0.025, 0.5, 0.975))
mct30_3_1 <- quantile(data$mct30_3_1, c(0.025, 0.5, 0.975))
mct30_1_1 <- quantile(data$mct30_1_1, c(0.025, 0.5, 0.975))
mct30_1_3 <- quantile(data$mct30_1_3, c(0.025, 0.5, 0.975))
mct30_1_5 <- quantile(data$mct30_1_5, c(0.025, 0.5, 0.975))
mct50_5_1 <- quantile(data$mct50_5_1, c(0.025, 0.5, 0.975))
mct50_3_1 <- quantile(data$mct50_3_1, c(0.025, 0.5, 0.975))
mct50_1_1 <- quantile(data$mct50_1_1, c(0.025, 0.5, 0.975))
mct50_1_3 <- quantile(data$mct50_1_3, c(0.025, 0.5, 0.975))
mct50_1_5 <- quantile(data$mct50_1_5, c(0.025, 0.5, 0.975))
mct70_5_1 <- quantile(data$mct70_5_1, c(0.025, 0.5, 0.975))
mct70_3_1 <- quantile(data$mct70_3_1, c(0.025, 0.5, 0.975))
mct70_1_1 <- quantile(data$mct70_1_1, c(0.025, 0.5, 0.975))
mct70_1_3 <- quantile(data$mct70_1_3, c(0.025, 0.5, 0.975))
mct70_1_5 <- quantile(data$mct70_1_5, c(0.025, 0.5, 0.975))

df11 <- data.frame("L","Laboratory (L)", 
                  se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv30[2], ppv30[1], ppv30[3],
                  npv30[2], npv30[1], npv30[3],
                  ppv50[2], ppv50[1], ppv50[3],
                  npv50[2], npv50[1], npv50[3],
                  ppv70[2], ppv70[1], ppv70[3],
                  npv70[2], npv70[1], npv70[3],
                  
                  mct30_5_1[2], mct30_5_1[1], mct30_5_1[3],
                  mct30_3_1[2], mct30_3_1[1], mct30_3_1[3],
                  mct30_1_1[2], mct30_1_1[1], mct30_1_1[3],
                  mct30_1_3[2], mct30_1_3[1], mct30_1_3[3],
                  mct30_1_5[2], mct30_1_5[1], mct30_1_5[3],
                  
                  mct50_5_1[2], mct50_5_1[1], mct50_5_1[3],
                  mct50_3_1[2], mct50_3_1[1], mct50_3_1[3],
                  mct50_1_1[2], mct50_1_1[1], mct50_1_1[3],
                  mct50_1_3[2], mct50_1_3[1], mct50_1_3[3],
                  mct50_1_5[2], mct50_1_5[1], mct50_1_5[3],
                  
                  mct70_5_1[2], mct70_5_1[1], mct70_5_1[3],
                  mct70_3_1[2], mct70_3_1[1], mct70_3_1[3],
                  mct70_1_1[2], mct70_1_1[1], mct70_1_1[3],
                  mct70_1_3[2], mct70_1_3[1], mct70_1_3[3],
                  mct70_1_5[2], mct70_1_5[1], mct70_1_5[3]
)
rownames(df11) <- NULL
colnames(df11) <- c("threshold", 
                   "test", "se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv30", "ppv30_low", "ppv30_hi",
                   "npv30", "npv30_low", "npv30_hi",
                   "ppv50", "ppv50_low", "ppv50_hi",
                   "npv50", "npv50_low", "npv50_hi",
                   "ppv70", "ppv70_low", "ppv70_hi",
                   "npv70", "npv70_low", "npv70_hi",
                   
                   "mct30_5_1", "mct30_5_1_low", "mct30_5_1_hi",
                   "mct30_3_1", "mct30_3_1_low", "mct30_3_1_hi",
                   "mct30_1_1", "mct30_1_1_low", "mct30_1_1_hi",
                   "mct30_1_3", "mct30_1_3_low", "mct30_1_3_hi",
                   "mct30_1_5", "mct30_1_5_low", "mct30_1_5_hi",
                   
                   "mct50_5_1", "mct50_5_1_low", "mct50_5_1_hi",
                   "mct50_3_1", "mct50_3_1_low", "mct50_3_1_hi",
                   "mct50_1_1", "mct50_1_1_low", "mct50_1_1_hi",
                   "mct50_1_3", "mct50_1_3_low", "mct50_1_3_hi",
                   "mct50_1_5", "mct50_1_5_low", "mct50_1_5_hi",
                   
                   "mct70_5_1", "mct70_5_1_low", "mct70_5_1_hi",
                   "mct70_3_1", "mct70_3_1_low", "mct70_3_1_hi",
                   "mct70_1_1", "mct70_1_1_low", "mct70_1_1_hi",
                   "mct70_1_3", "mct70_1_3_low", "mct70_1_3_hi",
                   "mct70_1_5", "mct70_1_5_low", "mct70_1_5_hi"
)  

#combining data####
data_pv <- rbind.data.frame(df1, df2,df3,df4,df5,df6,df7,df8,
                            df9[,1:26],df10[,1:26],df11[,1:26])

data_mct <- rbind.data.frame(df9,df10,df11)

str(data_pv)

##plots####
Fig1 <- data_pv %>% select("threshold", "test",
                   "se", "se_low", "se_hi",
                   "sp", "sp_low", "sp_hi") %>%
  pivot_longer(cols = c("se", "se_low", "se_hi",
                        "sp", "sp_low", "sp_hi")
  ) %>%
  mutate(element = ifelse(grepl("se", name), "Sensitivity",
                          ifelse(grepl("sp", name), "Specificity",NA)),
         names = ifelse(grepl("low",name),"lci",
                        ifelse(grepl("hi", name),"hci", "estimate")),
         threshold = factor(threshold,
                            levels = c("L",'P',"T","50","100","150",
                                       "200","250","300","350","400")),
         test = factor(test,
                       levels = c("Laboratory (L)", "Petrifilm (P)", 
                                  "Tri-Plate (T)","Luminometry")),
         group = paste(test,element,sep="")
                                   ) %>%
  pivot_wider(id_cols = c(threshold,
                          test,element,group),values_from = value, names_from = names) %>%
ggplot(aes(x = threshold))  +
  geom_point(aes(y = estimate, 
                      shape = test, color = element),
                  position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lci, ymax = hci, 
                 linetype = test, color = element),
             position = position_dodge(width = 0.5),
             width = 0.1) +
  geom_path(aes(y = estimate, group = group, 
                      linetype = test, color = element),
                  position = position_dodge(width = 0.5)) +
  ylab("Accuracy") +
  theme_classic(base_family = "Times",base_size = 10) +
  xlab("\nBacteriological tests and luminometry thresholds") +
  ylim(c(0,1)) +
  theme(legend.position = "bottom",legend.title = element_blank(),
        legend.direction = "vertical")

jpeg("milkFig1.jpeg", width = 8.9, 
     height = 10, 
     units = 'cm',res = 600)
Fig1
dev.off()


Fig2 <- data_pv %>% select("threshold", "test",
                           "ppv30", "ppv30_low", "ppv30_hi",
                           "npv30", "npv30_low", "npv30_hi",
                           "ppv50", "ppv50_low", "ppv50_hi",
                           "npv50", "npv50_low", "npv50_hi",
                           "ppv70", "ppv70_low", "ppv70_hi",
                           "npv70", "npv70_low", "npv70_hi") %>%
  pivot_longer(cols = c("ppv30", "ppv30_low", "ppv30_hi",
                        "npv30", "npv30_low", "npv30_hi",
                        "ppv50", "ppv50_low", "ppv50_hi",
                        "npv50", "npv50_low", "npv50_hi",
                        "ppv70", "ppv70_low", "ppv70_hi",
                        "npv70", "npv70_low", "npv70_hi")
  ) %>%
  mutate(element = ifelse(grepl("ppv", name), "PPV",
                          ifelse(grepl("npv", name), "NPV",NA)),
         prev = factor(ifelse(grepl("30",name), 30, 
                       ifelse(grepl("50",name), 50,
                              ifelse(grepl("70",name), 70, NA))),
                       labels = c("0.30","0.50","0.70")),
         names = ifelse(grepl("low",name),"lci",
                        ifelse(grepl("hi", name),"hci", "estimate")),
         threshold = factor(threshold,
                            levels = c("L",'P',"T","50","100","150",
                                       "200","250","300","350","400")),
         test = factor(test,
                       levels = c("Laboratory (L)", "Petrifilm (P)", 
                                  "Tri-Plate (T)","Luminometry")),
         group = paste(test,element,sep="")
  ) %>%
  pivot_wider(id_cols = c(threshold,prev,
                          test,element,group),values_from = value, names_from = names) %>%
  ggplot(aes(x = threshold))  +
  geom_point(aes(y = estimate, 
                 shape = test, color = element),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lci, ymax = hci, 
                    linetype = test, color = element),
                position = position_dodge(width = 0.5),
                width = 0.1) +
  geom_path(aes(y = estimate, group = group, 
                linetype = test, color = element),
            position = position_dodge(width = 0.5)) +
  ylab("Predictive values") +
  theme_classic(base_family = "Times",base_size = 10) +
  scale_linetype_discrete(name = "")+
  scale_shape_discrete(name = "")+
  facet_wrap(~prev, nrow = 3)+
  xlab("\nBacteriological tests and luminometry thresholds") +
  ylim(c(0,1)) +
  theme(legend.position = "bottom",legend.title = element_blank(),
        legend.direction = "vertical")

jpeg("milkFig2.jpeg", width = 8.9, 
     height = 21, 
     units = 'cm',res = 600)
Fig2
dev.off()



Fig3 <- data_mct %>% select("threshold", "test",
                            "mct30_5_1", "mct30_5_1_low", "mct30_5_1_hi",
                            "mct30_3_1", "mct30_3_1_low", "mct30_3_1_hi",
                            "mct30_1_1", "mct30_1_1_low", "mct30_1_1_hi",
                            "mct30_1_3", "mct30_1_3_low", "mct30_1_3_hi",
                            "mct30_1_5", "mct30_1_5_low", "mct30_1_5_hi",
                            
                            "mct50_5_1", "mct50_5_1_low", "mct50_5_1_hi",
                            "mct50_3_1", "mct50_3_1_low", "mct50_3_1_hi",
                            "mct50_1_1", "mct50_1_1_low", "mct50_1_1_hi",
                            "mct50_1_3", "mct50_1_3_low", "mct50_1_3_hi",
                            "mct50_1_5", "mct50_1_5_low", "mct50_1_5_hi",
                            
                            "mct70_5_1", "mct70_5_1_low", "mct70_5_1_hi",
                            "mct70_3_1", "mct70_3_1_low", "mct70_3_1_hi",
                            "mct70_1_1", "mct70_1_1_low", "mct70_1_1_hi",
                            "mct70_1_3", "mct70_1_3_low", "mct70_1_3_hi",
                            "mct70_1_5", "mct70_1_5_low", "mct70_1_5_hi") %>%
  pivot_longer(cols = c("mct30_5_1", "mct30_5_1_low", "mct30_5_1_hi",
                        "mct30_3_1", "mct30_3_1_low", "mct30_3_1_hi",
                        "mct30_1_1", "mct30_1_1_low", "mct30_1_1_hi",
                        "mct30_1_3", "mct30_1_3_low", "mct30_1_3_hi",
                        "mct30_1_5", "mct30_1_5_low", "mct30_1_5_hi",
                        
                        "mct50_5_1", "mct50_5_1_low", "mct50_5_1_hi",
                        "mct50_3_1", "mct50_3_1_low", "mct50_3_1_hi",
                        "mct50_1_1", "mct50_1_1_low", "mct50_1_1_hi",
                        "mct50_1_3", "mct50_1_3_low", "mct50_1_3_hi",
                        "mct50_1_5", "mct50_1_5_low", "mct50_1_5_hi",
                        
                        "mct70_5_1", "mct70_5_1_low", "mct70_5_1_hi",
                        "mct70_3_1", "mct70_3_1_low", "mct70_3_1_hi",
                        "mct70_1_1", "mct70_1_1_low", "mct70_1_1_hi",
                        "mct70_1_3", "mct70_1_3_low", "mct70_1_3_hi",
                        "mct70_1_5", "mct70_1_5_low", "mct70_1_5_hi")
  ) %>%
  mutate(ratio = ifelse(grepl("3_1", name), "3:1",
                               ifelse(grepl("1_1", name), "1:1",
                                      ifelse(grepl("1_3", name), "1:3",NA))),
         prev = factor(ifelse(grepl("30",name), 30, 
                              ifelse(grepl("50",name), 50,
                                     ifelse(grepl("70",name), 70, NA))),
                       labels = c("0.30","0.50","0.70")),
         names = ifelse(grepl("low",name),"lci",
                        ifelse(grepl("hi", name),"hci", "estimate")),
         threshold = factor(threshold,
                            levels = c("L",'P',"T","50","100","150",
                                       "200","250","300","350","400")),
         test = factor(test,
                       levels = c("Laboratory (L)", "Petrifilm (P)", 
                                  "Tri-Plate (T)","Luminometry")),
         group = paste(test,ratio,sep="")
  ) %>%
  filter(!is.na(ratio))%>%
  pivot_wider(id_cols = c(threshold,prev,
                          test,ratio,group),values_from = value, names_from = names) %>%
  ggplot(aes(x = threshold))  +
  geom_point(aes(y = estimate, color = ratio, 
                 shape = test),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lci, ymax = hci, color = ratio, 
                    linetype = test),
                position = position_dodge(width = 0.5),
                width = 0.1) +
  ylab("Misclassification cost terms") +
  theme_classic(base_family = "Times",base_size = 10) +
  scale_color_discrete(name = "FN:FP")+
  scale_linetype_discrete(name = "Test")+
  scale_shape_discrete(name = "Test")+
  facet_wrap(~prev, nrow = 1)+
  xlab("\nBacteriological tests") +
  theme(legend.position = "bottom",legend.title = element_blank(),
        legend.direction = "vertical")+ 
  guides(shape = guide_legend(order = 2),
         linetype = guide_legend(order = 2),
         color = guide_legend(order = 1))

jpeg("milkFig3.jpeg", width = 14, 
     height = 9, 
     units = 'cm',res = 600)
Fig3
dev.off()



