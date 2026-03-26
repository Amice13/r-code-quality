### Chapter 3

### In this chapter, based on the technique of principal components analysis, 
### I will examine how associated the policy stances of legislators in contemporary Brazil. 
### I have compiled a dataset with the Brazilian Legislative Surveys (1997 - 2013).

### TASK 1: Data Preparation
### TASK 2: Principal Components Analysis

rm(list=ls(all=TRUE))

### TASK 1: Data Preparation
library(psych)
library(psychTools)
library(factoextra)
library(FactoMineR)
library(Hmisc)
library(ggplot2)
library(sjPlot)
library(ggpubr)

MyData <- subset(`Chapter.3.Principal.Components.Analysis.(Dataset)`, 
                 select = c(yearcase, uniqueid, caseid, wave, party_elected, region, age, region, terms,
                            fidelit, believe, efforts, needmps_all, ffaaint, ppswitch_all,                                        #Political Dimension
                            econlmr,                                                                                              #Economic Dimension
                            family, vereador, depest, senador, depfed, prefvice, govvice, ministro, secest, clients_all, ppvsreg, #Traditional Dimension
                            lrclass                                                                                               #Left-Right         
                            ))

MyData$party_elected[ MyData$party_elected == "99"] <- NA
MyData$age[ MyData$age == "99"] <- NA
MyData$lrclass[ MyData$lrclass == "99"] <- NA
MyData$terms[ MyData$terms == "99"] <- NA
MyData$fidelit[ MyData$fidelit == "9"] <- NA
MyData$needmps_all[ MyData$needmps_all == "9"] <- NA
MyData$believe[ MyData$believe == "9"] <- NA
MyData$believe[ MyData$believe == "8"] <- NA
MyData$efforts[ MyData$efforts == "9"] <- NA
MyData$efforts[ MyData$efforts == "8"] <- NA
MyData$ppswitch_all[ MyData$ppswitch_all == "9"] <- NA
MyData$ffaaint[ MyData$ffaaint == "9"] <- NA
MyData$econlmr[ MyData$econlmr == "9"] <- NA
MyData$clients_all[ MyData$clients_all == "9"] <- NA
MyData$ppvsreg[ MyData$ppvsreg == "3"] <- NA
MyData$ppvsreg[ MyData$ppvsreg == "9"] <- NA
MyData$family[ MyData$family == "9"] <- NA
MyData$vereador[ MyData$vereador == "9"] <- NA
MyData$depest[ MyData$depest == "9"] <- NA
MyData$senador[ MyData$senador == "9"] <- NA
MyData$depfed[ MyData$depfed == "9"] <- NA
MyData$prefvice[ MyData$prefvice == "9"] <- NA
MyData$govvice[ MyData$govvice == "9"] <- NA
MyData$ministro[ MyData$ministro == "9"] <- NA
MyData$secest[ MyData$secest == "9"] <- NA
MyData$region[ MyData$region == "9"] <- NA
MyData$lrclass[ MyData$lrclass == "3.5"] <- 3
MyData$lrclass[ MyData$lrclass == "5.5"] <- 5

MyData2 <- na.omit(MyData)



### TASK 2: Principal Components Analysis
MyData3 <- subset(MyData2, select = c(fidelit, believe, efforts, needmps_all, ffaaint, ppswitch_all,
                                      econlmr,
                                      clients_all, ppvsreg, family, vereador, depest, senador, depfed, prefvice, govvice, ministro, secest,
                                      lrclass))

colnames(MyData3) <- c("FIDELITY", "BELIEVE", "EFFORTS", "DECREE", "MILITARY", "SWITCH",
                       "ECONOMY",
                       "CLIENTELISM", "LOCAL INTERESTS", "FAMILY", "COUNCILLOR", "STATE DEPUTY", "SENATOR", "FEDERAL DEPUTY", "MAYOR", "GOVERNOR", "MINISTER", "STATE SECRETARY",
                       "LEFT RIGHT")

res.mfa <- MFA(MyData3, 
               group = c(6, 1, 11, 1),
               type = c("s", "s", "s", "s"),
               name.group = c("Political", 
                              "Economic",
                              "Traditional", 
                              "Left-Right"))

# The Size of Eigenvalues
eig.val <- get_eigenvalue(res.mfa)

#Scree Test (Figure 3.2)
p <- fviz_screeplot(res.mfa, geom = "line")
p + labs(title = "",
         x = "Factors", y = "% of variances") +
  theme_classic() +
  theme(text = element_text(family = "serif"))

# Factor Loadings (Figure 3.1)
r <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 1, top = 20,
                  palette = "jco")
rr <- r + labs(title = "Factor 1",
               x = "") +
  set_theme(base = theme_classic(),
            axis.angle.x = 90,
            axis.textsize.x = 0.8
  ) +
  theme(text = element_text(family = "serif"))

s <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 2, top = 20,
                  palette = "jco")
ss <- s + labs(title = "Factor 2",
               x = "") +
  set_theme(base = theme_classic(),
            axis.angle.x = 90,
            axis.textsize.x = 0.8
  ) +
  theme(text = element_text(family = "serif"))

t <- fviz_contrib(res.mfa, choice = "quanti.var", axes = 3, top = 20,
                  palette = "jco")
tt <- t + labs(title = "Factor 3",
               x = "") +
  set_theme(base = theme_classic(),
            axis.angle.x = 90,
            axis.textsize.x = 0.8
  ) +
  theme(text = element_text(family = "serif")) 

ggarrange(rr, ss, tt,
          ncol = 2, nrow = 2) 