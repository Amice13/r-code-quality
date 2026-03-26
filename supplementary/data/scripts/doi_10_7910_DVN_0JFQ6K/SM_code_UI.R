################################################################################
################################################################################
##### ASSESSMENT LUMINOMETRY, TRIPLATE, PETRIFILM, LABO, EST, and PVD ##########
################################### TO DETECT UI IN DAIRY COWS #################
################################################################################
############################ Analyzed November 2024 ############################
########################## code from Wang et al. 2020 ##########################
################################################################################
################################################################################

#the codes for the .bug models are at the end of the document###################
#librairies#####################################################################

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



##datalist####
#import data.csv
farm1 <- data %>%
  filter(farm == 1)
farm2 <- data %>%
  filter(farm == 2)
farm3 <- data %>%
  filter(farm == 3)
farm4 <- data %>%
  filter(farm == 4)
farm5 <- data %>%
  filter(farm == 5)
farm6 <- data %>%
  filter(farm == 6)
farm7 <- data %>%
  filter(farm == 7)
farmnot5 <- data %>%
  filter(farm != 5)



#n is the sample size,
n1 <- nrow(farm1)
n2 <- nrow(farm2)
n3 <- nrow(farm3)
n4 <- nrow(farm4)
n5 <- nrow(farm5)
n6 <- nrow(farm6)
n7 <- nrow(farm7)
#K is the number of raters (tests),
K <- 6

y1_p20l150 <- farm1 %>%
  select(bacterio, tp, petrifilm20, lumino150, pvd, es)
y2_p20l150 <- farm2 %>%
  select(bacterio, tp, petrifilm20, lumino150, pvd, es)
y3_p20l150 <- farm3 %>%
  select(bacterio, tp, petrifilm20, lumino150, pvd, es)
y4_p20l150 <- farm4 %>%
  select(bacterio, tp, petrifilm20, lumino150, pvd, es)
y5_p20l150 <- farm5 %>%
  select(bacterio, tp, petrifilm20, lumino150, pvd, es)
y6_p20l150 <- farm6 %>%
  select(bacterio, tp, petrifilm20, lumino150, pvd, es)
y7_p20l150 <- farm7 %>%
  select(bacterio, tp, petrifilm20, lumino150, pvd, es)

y1_p20l50 <- farm1 %>%
  select(bacterio, tp, petrifilm20, lumino50, pvd, es)
y2_p20l50 <- farm2 %>%
  select(bacterio, tp, petrifilm20, lumino50, pvd, es)
y3_p20l50 <- farm3 %>%
  select(bacterio, tp, petrifilm20, lumino50, pvd, es)
y4_p20l50 <- farm4 %>%
  select(bacterio, tp, petrifilm20, lumino50, pvd, es)
y5_p20l50 <- farm5 %>%
  select(bacterio, tp, petrifilm20, lumino50, pvd, es)
y6_p20l50 <- farm6 %>%
  select(bacterio, tp, petrifilm20, lumino50, pvd, es)
y7_p20l50 <- farm7 %>%
  select(bacterio, tp, petrifilm20, lumino50, pvd, es)

y1_p20l100 <- farm1 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y2_p20l100 <- farm2 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y3_p20l100 <- farm3 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y4_p20l100 <- farm4 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y5_p20l100 <- farm5 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y6_p20l100 <- farm6 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y7_p20l100 <- farm7 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)

y1_p20l200 <- farm1 %>%
  select(bacterio, tp, petrifilm20, lumino200, pvd, es)
y2_p20l200 <- farm2 %>%
  select(bacterio, tp, petrifilm20, lumino200, pvd, es)
y3_p20l200 <- farm3 %>%
  select(bacterio, tp, petrifilm20, lumino200, pvd, es)
y4_p20l200 <- farm4 %>%
  select(bacterio, tp, petrifilm20, lumino200, pvd, es)
y5_p20l200 <- farm5 %>%
  select(bacterio, tp, petrifilm20, lumino200, pvd, es)
y6_p20l200 <- farm6 %>%
  select(bacterio, tp, petrifilm20, lumino200, pvd, es)
y7_p20l200 <- farm7 %>%
  select(bacterio, tp, petrifilm20, lumino200, pvd, es)

y1_p20l250 <- farm1 %>%
  select(bacterio, tp, petrifilm20, lumino250, pvd, es)
y2_p20l250 <- farm2 %>%
  select(bacterio, tp, petrifilm20, lumino250, pvd, es)
y3_p20l250 <- farm3 %>%
  select(bacterio, tp, petrifilm20, lumino250, pvd, es)
y4_p20l250 <- farm4 %>%
  select(bacterio, tp, petrifilm20, lumino250, pvd, es)
y5_p20l250 <- farm5 %>%
  select(bacterio, tp, petrifilm20, lumino250, pvd, es)
y6_p20l250 <- farm6 %>%
  select(bacterio, tp, petrifilm20, lumino250, pvd, es)
y7_p20l250 <- farm7 %>%
  select(bacterio, tp, petrifilm20, lumino250, pvd, es)

y1_p20l300 <- farm1 %>%
  select(bacterio, tp, petrifilm20, lumino300, pvd, es)
y2_p20l300 <- farm2 %>%
  select(bacterio, tp, petrifilm20, lumino300, pvd, es)
y3_p20l300 <- farm3 %>%
  select(bacterio, tp, petrifilm20, lumino300, pvd, es)
y4_p20l300 <- farm4 %>%
  select(bacterio, tp, petrifilm20, lumino300, pvd, es)
y5_p20l300 <- farm5 %>%
  select(bacterio, tp, petrifilm20, lumino300, pvd, es)
y6_p20l300 <- farm6 %>%
  select(bacterio, tp, petrifilm20, lumino300, pvd, es)
y7_p20l300 <- farm7 %>%
  select(bacterio, tp, petrifilm20, lumino300, pvd, es)

y1_p10l100 <- farm1 %>%
  select(bacterio, tp, petrifilm10, lumino100, pvd, es)
y2_p10l100 <- farm2 %>%
  select(bacterio, tp, petrifilm10, lumino100, pvd, es)
y3_p10l100 <- farm3 %>%
  select(bacterio, tp, petrifilm10, lumino100, pvd, es)
y4_p10l100 <- farm4 %>%
  select(bacterio, tp, petrifilm10, lumino100, pvd, es)
y5_p10l100 <- farm5 %>%
  select(bacterio, tp, petrifilm10, lumino100, pvd, es)
y6_p10l100 <- farm6 %>%
  select(bacterio, tp, petrifilm10, lumino100, pvd, es)
y7_p10l100 <- farm7 %>%
  select(bacterio, tp, petrifilm10, lumino100, pvd, es)

y1_p15l100 <- farm1 %>%
  select(bacterio, tp, petrifilm15, lumino100, pvd, es)
y2_p15l100 <- farm2 %>%
  select(bacterio, tp, petrifilm15, lumino100, pvd, es)
y3_p15l100 <- farm3 %>%
  select(bacterio, tp, petrifilm15, lumino100, pvd, es)
y4_p15l100 <- farm4 %>%
  select(bacterio, tp, petrifilm15, lumino100, pvd, es)
y5_p15l100 <- farm5 %>%
  select(bacterio, tp, petrifilm15, lumino100, pvd, es)
y6_p15l100 <- farm6 %>%
  select(bacterio, tp, petrifilm15, lumino100, pvd, es)
y7_p15l100 <- farm7 %>%
  select(bacterio, tp, petrifilm15, lumino100, pvd, es)

y1_p25l100 <- farm1 %>%
  select(bacterio, tp, petrifilm25, lumino100, pvd, es)
y2_p25l100 <- farm2 %>%
  select(bacterio, tp, petrifilm25, lumino100, pvd, es)
y3_p25l100 <- farm3 %>%
  select(bacterio, tp, petrifilm25, lumino100, pvd, es)
y4_p25l100 <- farm4 %>%
  select(bacterio, tp, petrifilm25, lumino100, pvd, es)
y5_p25l100 <- farm5 %>%
  select(bacterio, tp, petrifilm25, lumino100, pvd, es)
y6_p25l100 <- farm6 %>%
  select(bacterio, tp, petrifilm25, lumino100, pvd, es)
y7_p25l100 <- farm7 %>%
  select(bacterio, tp, petrifilm25, lumino100, pvd, es)

y1_p30l100 <- farm1 %>%
  select(bacterio, tp, petrifilm30, lumino100, pvd, es)
y2_p30l100 <- farm2 %>%
  select(bacterio, tp, petrifilm30, lumino100, pvd, es)
y3_p30l100 <- farm3 %>%
  select(bacterio, tp, petrifilm30, lumino100, pvd, es)
y4_p30l100 <- farm4 %>%
  select(bacterio, tp, petrifilm30, lumino100, pvd, es)
y5_p30l100 <- farm5 %>%
  select(bacterio, tp, petrifilm30, lumino100, pvd, es)
y6_p30l100 <- farm6 %>%
  select(bacterio, tp, petrifilm30, lumino100, pvd, es)
y7_p30l100 <- farm7 %>%
  select(bacterio, tp, petrifilm30, lumino100, pvd, es)

y1_p35l100 <- farm1 %>%
  select(bacterio, tp, petrifilm35, lumino100, pvd, es)
y2_p35l100 <- farm2 %>%
  select(bacterio, tp, petrifilm35, lumino100, pvd, es)
y3_p35l100 <- farm3 %>%
  select(bacterio, tp, petrifilm35, lumino100, pvd, es)
y4_p35l100 <- farm4 %>%
  select(bacterio, tp, petrifilm35, lumino100, pvd, es)
y5_p35l100 <- farm5 %>%
  select(bacterio, tp, petrifilm35, lumino100, pvd, es)
y6_p35l100 <- farm6 %>%
  select(bacterio, tp, petrifilm35, lumino100, pvd, es)
y7_p35l100 <- farm7 %>%
  select(bacterio, tp, petrifilm35, lumino100, pvd, es)


#m is the original dataset, an n by K matrix.
m1_p20l150 <- as.matrix(y1_p20l150)
m2_p20l150 <- as.matrix(y2_p20l150)
m3_p20l150 <- as.matrix(y3_p20l150)
m4_p20l150 <- as.matrix(y4_p20l150)
m5_p20l150 <- as.matrix(y5_p20l150)
m6_p20l150 <- as.matrix(y6_p20l150)
m7_p20l150 <- as.matrix(y7_p20l150)

m1_p20l50 <- as.matrix(y1_p20l50)
m2_p20l50 <- as.matrix(y2_p20l50)
m3_p20l50 <- as.matrix(y3_p20l50)
m4_p20l50 <- as.matrix(y4_p20l50)
m5_p20l50 <- as.matrix(y5_p20l50)
m6_p20l50 <- as.matrix(y6_p20l50)
m7_p20l50 <- as.matrix(y7_p20l50)

m1_p20l100 <- as.matrix(y1_p20l100)
m2_p20l100 <- as.matrix(y2_p20l100)
m3_p20l100 <- as.matrix(y3_p20l100)
m4_p20l100 <- as.matrix(y4_p20l100)
m5_p20l100 <- as.matrix(y5_p20l100)
m6_p20l100 <- as.matrix(y6_p20l100)
m7_p20l100 <- as.matrix(y7_p20l100)

m1_p20l200 <- as.matrix(y1_p20l200)
m2_p20l200 <- as.matrix(y2_p20l200)
m3_p20l200 <- as.matrix(y3_p20l200)
m4_p20l200 <- as.matrix(y4_p20l200)
m5_p20l200 <- as.matrix(y5_p20l200)
m6_p20l200 <- as.matrix(y6_p20l200)
m7_p20l200 <- as.matrix(y7_p20l200)

m1_p20l250 <- as.matrix(y1_p20l250)
m2_p20l250 <- as.matrix(y2_p20l250)
m3_p20l250 <- as.matrix(y3_p20l250)
m4_p20l250 <- as.matrix(y4_p20l250)
m5_p20l250 <- as.matrix(y5_p20l250)
m6_p20l250 <- as.matrix(y6_p20l250)
m7_p20l250 <- as.matrix(y7_p20l250)

m1_p20l300 <- as.matrix(y1_p20l300)
m2_p20l300 <- as.matrix(y2_p20l300)
m3_p20l300 <- as.matrix(y3_p20l300)
m4_p20l300 <- as.matrix(y4_p20l300)
m5_p20l300 <- as.matrix(y5_p20l300)
m6_p20l300 <- as.matrix(y6_p20l300)
m7_p20l300 <- as.matrix(y7_p20l300)

m1_p10l100 <- as.matrix(y1_p10l100)
m2_p10l100 <- as.matrix(y2_p10l100)
m3_p10l100 <- as.matrix(y3_p10l100)
m4_p10l100 <- as.matrix(y4_p10l100)
m5_p10l100 <- as.matrix(y5_p10l100)
m6_p10l100 <- as.matrix(y6_p10l100)
m7_p10l100 <- as.matrix(y7_p10l100)

m1_p15l100 <- as.matrix(y1_p15l100)
m2_p15l100 <- as.matrix(y2_p15l100)
m3_p15l100 <- as.matrix(y3_p15l100)
m4_p15l100 <- as.matrix(y4_p15l100)
m5_p15l100 <- as.matrix(y5_p15l100)
m6_p15l100 <- as.matrix(y6_p15l100)
m7_p15l100 <- as.matrix(y7_p15l100)

m1_p25l100 <- as.matrix(y1_p25l100)
m2_p25l100 <- as.matrix(y2_p25l100)
m3_p25l100 <- as.matrix(y3_p25l100)
m4_p25l100 <- as.matrix(y4_p25l100)
m5_p25l100 <- as.matrix(y5_p25l100)
m6_p25l100 <- as.matrix(y6_p25l100)
m7_p25l100 <- as.matrix(y7_p25l100)

m1_p30l100 <- as.matrix(y1_p30l100)
m2_p30l100 <- as.matrix(y2_p30l100)
m3_p30l100 <- as.matrix(y3_p30l100)
m4_p30l100 <- as.matrix(y4_p30l100)
m5_p30l100 <- as.matrix(y5_p30l100)
m6_p30l100 <- as.matrix(y6_p30l100)
m7_p30l100 <- as.matrix(y7_p30l100)

m1_p35l100 <- as.matrix(y1_p35l100)
m2_p35l100 <- as.matrix(y2_p35l100)
m3_p35l100 <- as.matrix(y3_p35l100)
m4_p35l100 <- as.matrix(y4_p35l100)
m5_p35l100 <- as.matrix(y5_p35l100)
m6_p35l100 <- as.matrix(y6_p35l100)
m7_p35l100 <- as.matrix(y7_p35l100)

x1_p20l150<- structure(m1_p20l150, .Dim=c(n1,K))
x2_p20l150<- structure(m2_p20l150, .Dim=c(n2,K))
x3_p20l150<- structure(m3_p20l150, .Dim=c(n3,K))
x4_p20l150<- structure(m4_p20l150, .Dim=c(n4,K))
x5_p20l150<- structure(m5_p20l150, .Dim=c(n5,K))
x6_p20l150<- structure(m6_p20l150, .Dim=c(n6,K))
x7_p20l150<- structure(m7_p20l150, .Dim=c(n7,K))

x1_p20l50<- structure(m1_p20l50, .Dim=c(n1,K))
x2_p20l50<- structure(m2_p20l50, .Dim=c(n2,K))
x3_p20l50<- structure(m3_p20l50, .Dim=c(n3,K))
x4_p20l50<- structure(m4_p20l50, .Dim=c(n4,K))
x5_p20l50<- structure(m5_p20l50, .Dim=c(n5,K))
x6_p20l50<- structure(m6_p20l50, .Dim=c(n6,K))
x7_p20l50<- structure(m7_p20l50, .Dim=c(n7,K))

x1_p20l100<- structure(m1_p20l100, .Dim=c(n1,K))
x2_p20l100<- structure(m2_p20l100, .Dim=c(n2,K))
x3_p20l100<- structure(m3_p20l100, .Dim=c(n3,K))
x4_p20l100<- structure(m4_p20l100, .Dim=c(n4,K))
x5_p20l100<- structure(m5_p20l100, .Dim=c(n5,K))
x6_p20l100<- structure(m6_p20l100, .Dim=c(n6,K))
x7_p20l100<- structure(m7_p20l100, .Dim=c(n7,K))

x1_p20l200<- structure(m1_p20l200, .Dim=c(n1,K))
x2_p20l200<- structure(m2_p20l200, .Dim=c(n2,K))
x3_p20l200<- structure(m3_p20l200, .Dim=c(n3,K))
x4_p20l200<- structure(m4_p20l200, .Dim=c(n4,K))
x5_p20l200<- structure(m5_p20l200, .Dim=c(n5,K))
x6_p20l200<- structure(m6_p20l200, .Dim=c(n6,K))
x7_p20l200<- structure(m7_p20l200, .Dim=c(n7,K))

x1_p20l250<- structure(m1_p20l250, .Dim=c(n1,K))
x2_p20l250<- structure(m2_p20l250, .Dim=c(n2,K))
x3_p20l250<- structure(m3_p20l250, .Dim=c(n3,K))
x4_p20l250<- structure(m4_p20l250, .Dim=c(n4,K))
x5_p20l250<- structure(m5_p20l250, .Dim=c(n5,K))
x6_p20l250<- structure(m6_p20l250, .Dim=c(n6,K))
x7_p20l250<- structure(m7_p20l250, .Dim=c(n7,K))

x1_p20l300<- structure(m1_p20l300, .Dim=c(n1,K))
x2_p20l300<- structure(m2_p20l300, .Dim=c(n2,K))
x3_p20l300<- structure(m3_p20l300, .Dim=c(n3,K))
x4_p20l300<- structure(m4_p20l300, .Dim=c(n4,K))
x5_p20l300<- structure(m5_p20l300, .Dim=c(n5,K))
x6_p20l300<- structure(m6_p20l300, .Dim=c(n6,K))
x7_p20l300<- structure(m7_p20l300, .Dim=c(n7,K))

x1_p10l100<- structure(m1_p10l100, .Dim=c(n1,K))
x2_p10l100<- structure(m2_p10l100, .Dim=c(n2,K))
x3_p10l100<- structure(m3_p10l100, .Dim=c(n3,K))
x4_p10l100<- structure(m4_p10l100, .Dim=c(n4,K))
x5_p10l100<- structure(m5_p10l100, .Dim=c(n5,K))
x6_p10l100<- structure(m6_p10l100, .Dim=c(n6,K))
x7_p10l100<- structure(m7_p10l100, .Dim=c(n7,K))

x1_p15l100<- structure(m1_p15l100, .Dim=c(n1,K))
x2_p15l100<- structure(m2_p15l100, .Dim=c(n2,K))
x3_p15l100<- structure(m3_p15l100, .Dim=c(n3,K))
x4_p15l100<- structure(m4_p15l100, .Dim=c(n4,K))
x5_p15l100<- structure(m5_p15l100, .Dim=c(n5,K))
x6_p15l100<- structure(m6_p15l100, .Dim=c(n6,K))
x7_p15l100<- structure(m7_p15l100, .Dim=c(n7,K))

x1_p25l100<- structure(m1_p25l100, .Dim=c(n1,K))
x2_p25l100<- structure(m2_p25l100, .Dim=c(n2,K))
x3_p25l100<- structure(m3_p25l100, .Dim=c(n3,K))
x4_p25l100<- structure(m4_p25l100, .Dim=c(n4,K))
x5_p25l100<- structure(m5_p25l100, .Dim=c(n5,K))
x6_p25l100<- structure(m6_p25l100, .Dim=c(n6,K))
x7_p25l100<- structure(m7_p25l100, .Dim=c(n7,K))

x1_p30l100<- structure(m1_p30l100, .Dim=c(n1,K))
x2_p30l100<- structure(m2_p30l100, .Dim=c(n2,K))
x3_p30l100<- structure(m3_p30l100, .Dim=c(n3,K))
x4_p30l100<- structure(m4_p30l100, .Dim=c(n4,K))
x5_p30l100<- structure(m5_p30l100, .Dim=c(n5,K))
x6_p30l100<- structure(m6_p30l100, .Dim=c(n6,K))
x7_p30l100<- structure(m7_p30l100, .Dim=c(n7,K))

x1_p35l100<- structure(m1_p35l100, .Dim=c(n1,K))
x2_p35l100<- structure(m2_p35l100, .Dim=c(n2,K))
x3_p35l100<- structure(m3_p35l100, .Dim=c(n3,K))
x4_p35l100<- structure(m4_p35l100, .Dim=c(n4,K))
x5_p35l100<- structure(m5_p35l100, .Dim=c(n5,K))
x6_p35l100<- structure(m6_p35l100, .Dim=c(n6,K))
x7_p35l100<- structure(m7_p35l100, .Dim=c(n7,K))


z1<-rep(0,n1)
z2<-rep(0,n2)
z3<-rep(0,n3)
z4<-rep(0,n4)
z5<-rep(0,n5)
z6<-rep(0,n6)
z7<-rep(0,n7)

dat_p20l150=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, n7=n7,
                 x1=x1_p20l150,
                 x2=x2_p20l150,
                 x3=x3_p20l150,
                 x4=x4_p20l150,
                 x5=x5_p20l150,
                 x6=x6_p20l150,
                 x7=x7_p20l150,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6, z7=z7) #prepare the data list

dat_p20l50=list(K=K,
                n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, n7=n7,
                x1=x1_p20l50,
                x2=x2_p20l50,
                x3=x3_p20l50,
                x4=x4_p20l50,
                x5=x5_p20l50,
                x6=x6_p20l50,
                x7=x7_p20l50,
                z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6, z7=z7) #prepare the data list

dat_p20l100=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, n7=n7,
                 x1=x1_p20l100,
                 x2=x2_p20l100,
                 x3=x3_p20l100,
                 x4=x4_p20l100,
                 x5=x5_p20l100,
                 x6=x6_p20l100,
                 x7=x7_p20l100,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6, z7=z7) #prepare the data list

dat_p20l200=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, n7=n7,
                 x1=x1_p20l200,
                 x2=x2_p20l200,
                 x3=x3_p20l200,
                 x4=x4_p20l200,
                 x5=x5_p20l200,
                 x6=x6_p20l200,
                 x7=x7_p20l200,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6, z7=z7) #prepare the data list

dat_p20l250=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, n7=n7,
                 x1=x1_p20l250,
                 x2=x2_p20l250,
                 x3=x3_p20l250,
                 x4=x4_p20l250,
                 x5=x5_p20l250,
                 x6=x6_p20l250,
                 x7=x7_p20l250,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6, z7=z7) #prepare the data list

dat_p20l300=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, n7=n7,
                 x1=x1_p20l300,
                 x2=x2_p20l300,
                 x3=x3_p20l300,
                 x4=x4_p20l300,
                 x5=x5_p20l300,
                 x6=x6_p20l300,
                 x7=x7_p20l300,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6, z7=z7) #prepare the data list

dat_p10l100=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, n7=n7,
                 x1=x1_p10l100,
                 x2=x2_p10l100,
                 x3=x3_p10l100,
                 x4=x4_p10l100,
                 x5=x5_p10l100,
                 x6=x6_p10l100,
                 x7=x7_p10l100,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6, z7=z7) #prepare the data list

dat_p15l100=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, n7=n7,
                 x1=x1_p15l100,
                 x2=x2_p15l100,
                 x3=x3_p15l100,
                 x4=x4_p15l100,
                 x5=x5_p15l100,
                 x6=x6_p15l100,
                 x7=x7_p15l100,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6, z7=z7) #prepare the data list

dat_p25l100=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, n7=n7,
                 x1=x1_p25l100,
                 x2=x2_p25l100,
                 x3=x3_p25l100,
                 x4=x4_p25l100,
                 x5=x5_p25l100,
                 x6=x6_p25l100,
                 x7=x7_p25l100,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6, z7=z7) #prepare the data list

dat_p30l100=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, n7=n7,
                 x1=x1_p30l100,
                 x2=x2_p30l100,
                 x3=x3_p30l100,
                 x4=x4_p30l100,
                 x5=x5_p30l100,
                 x6=x6_p30l100,
                 x7=x7_p30l100,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6, z7=z7) #prepare the data list

dat_p35l100=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, n7=n7,
                 x1=x1_p35l100,
                 x2=x2_p35l100,
                 x3=x3_p35l100,
                 x4=x4_p35l100,
                 x5=x5_p35l100,
                 x6=x6_p35l100,
                 x7=x7_p35l100,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6, z7=z7) #prepare the data list



#
################################################################################
#Analyses - incl. dependence####
################################################################################
par4=c("se","sp", "c1", "c2")
par5=c("se","sp", "c1", "c2",
       "pi1","pi2","pi3","pi4",
       "pi5","pi6","pi7")
##model - dependence p20l150####
out_p20l150_dep_7pop <- jags(model ="TEMPmodel_ntests_dep_7pop.bug", 
                             parameters.to.save=par4, 
                             data=dat_p20l150, n.chains=3,
                             n.iter=50000, n.burnin=10000, n.thin = 2,
                             #inits = inits,
                             DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l150_dep_7pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l150####
bug_p20l150_d <- print(out_p20l150_dep_7pop, digits.summary=2) 
bug_p20l150_d <- bug_p20l150_d$summary %>%
  data.frame(element = row.names(bug_p20l150_d$summary)) %>%
  mutate(cp_p = 20, 
         cp_l = 150) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)

##model - dependence p20l50####
out_p20l50_dep_7pop <- jags(model ="TEMPmodel_ntests_dep_7pop.bug", 
                            parameters.to.save=par4, 
                            data=dat_p20l50, n.chains=3,
                            n.iter=50000, n.burnin=10000, n.thin = 2,
                            #inits = inits,
                            DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l50_dep_7pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l50####
bug_p20l50_d <- print(out_p20l50_dep_7pop, digits.summary=2) 
bug_p20l50_d <- bug_p20l50_d$summary %>%
  data.frame(element = row.names(bug_p20l50_d$summary)) %>%
  mutate(cp_p = 20, 
         cp_l = 50) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)

##model - dependence p20l100####
out_p20l100_dep_7pop <- jags(model ="TEMPmodel_ntests_dep_7pop.bug", 
                             parameters.to.save=par5, 
                             data=dat_p20l100, n.chains=3,
                             n.iter=50000, n.burnin=10000, n.thin = 2,
                             #inits = inits,
                             DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l100_dep_7pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l100####
bug_p20l100_d <- print(out_p20l100_dep_7pop, digits.summary=2) 
write.csv(bug_p20l100_d$sims.matrix,"bug_p20l100_d.csv", row.names = FALSE)

bug_p20l100_d <- bug_p20l100_d$summary %>%
  data.frame(element = row.names(bug_p20l100_d$summary)) %>%
  mutate(cp_p = 20, 
         cp_l = 100) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)

##model - dependence p20l200####
out_p20l200_dep_7pop <- jags(model ="TEMPmodel_ntests_dep_7pop.bug", 
                             parameters.to.save=par4, 
                             data=dat_p20l200, n.chains=3,
                             n.iter=50000, n.burnin=10000, n.thin = 2,
                             #inits = inits,
                             DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l200_dep_7pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l200####
bug_p20l200_d <- print(out_p20l200_dep_7pop, digits.summary=2) 
bug_p20l200_d <- bug_p20l200_d$summary %>%
  data.frame(element = row.names(bug_p20l200_d$summary)) %>%
  mutate(cp_p = 20, 
         cp_l = 200) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)

##model - dependence p20l250####
out_p20l250_dep_7pop <- jags(model ="TEMPmodel_ntests_dep_7pop.bug", 
                             parameters.to.save=par5, 
                             data=dat_p20l250, n.chains=3,
                             n.iter=50000, n.burnin=10000, n.thin = 2,
                             #inits = inits,
                             DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l250_dep_7pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l250####
bug_p20l250_d <- print(out_p20l250_dep_7pop, digits.summary=2) 
write.csv(bug_p20l250_d$sims.matrix,"bug_p20l250_d.csv", row.names = FALSE)

bug_p20l250_d <- bug_p20l250_d$summary %>%
  data.frame(element = row.names(bug_p20l250_d$summary)) %>%
  mutate(cp_p = 20, 
         cp_l = 250) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)

bug_p20l250_d %>%
  filter(grepl("pi", element))

##model - dependence p20l300####
out_p20l300_dep_7pop <- jags(model ="TEMPmodel_ntests_dep_7pop.bug", 
                             parameters.to.save=par4, 
                             data=dat_p20l300, n.chains=3,
                             n.iter=50000, n.burnin=10000, n.thin = 2,
                             #inits = inits,
                             DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l300_dep_7pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l300####
bug_p20l300_d <- print(out_p20l300_dep_7pop, digits.summary=2) 
bug_p20l300_d <- bug_p20l300_d$summary %>%
  data.frame(element = row.names(bug_p20l300_d$summary)) %>%
  mutate(cp_p = 20, 
         cp_l = 300) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)

##model - dependence p10l100####
out_p10l100_dep_7pop <- jags(model ="TEMPmodel_ntests_dep_7pop.bug", 
                             parameters.to.save=par4, 
                             data=dat_p10l100, n.chains=3,
                             n.iter=50000, n.burnin=10000, n.thin = 2,
                             #inits = inits,
                             DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p10l100_dep_7pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p10l100####
bug_p10l100_d <- print(out_p10l100_dep_7pop, digits.summary=2) 
bug_p10l100_d <- bug_p10l100_d$summary %>%
  data.frame(element = row.names(bug_p10l100_d$summary)) %>%
  mutate(cp_p = 10, 
         cp_l = 100) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)

##model - dependence p15l100####
out_p15l100_dep_7pop <- jags(model ="TEMPmodel_ntests_dep_7pop.bug", 
                             parameters.to.save=par4, 
                             data=dat_p15l100, n.chains=3,
                             n.iter=50000, n.burnin=10000, n.thin = 2,
                             #inits = inits,
                             DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p15l100_dep_7pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p15l100####
bug_p15l100_d <- print(out_p15l100_dep_7pop, digits.summary=2) 
bug_p15l100_d <- bug_p15l100_d$summary %>%
  data.frame(element = row.names(bug_p15l100_d$summary)) %>%
  mutate(cp_p = 15, 
         cp_l = 100) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)

##model - dependence p25l100####
out_p25l100_dep_7pop <- jags(model ="TEMPmodel_ntests_dep_7pop.bug", 
                             parameters.to.save=par4, 
                             data=dat_p25l100, n.chains=3,
                             n.iter=50000, n.burnin=10000, n.thin = 2,
                             #inits = inits,
                             DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p25l100_dep_7pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p25l100####
bug_p25l100_d <- print(out_p25l100_dep_7pop, digits.summary=2) 
bug_p25l100_d <- bug_p25l100_d$summary %>%
  data.frame(element = row.names(bug_p25l100_d$summary)) %>%
  mutate(cp_p = 25, 
         cp_l = 100) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)

##model - dependence p30l100####
out_p30l100_dep_7pop <- jags(model ="TEMPmodel_ntests_dep_7pop.bug", 
                             parameters.to.save=par4, 
                             data=dat_p30l100, n.chains=3,
                             n.iter=50000, n.burnin=10000, n.thin = 2,
                             #inits = inits,
                             DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p30l100_dep_7pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p30l100####
bug_p30l100_d <- print(out_p30l100_dep_7pop, digits.summary=2) 
bug_p30l100_d <- bug_p30l100_d$summary %>%
  data.frame(element = row.names(bug_p30l100_d$summary)) %>%
  mutate(cp_p = 30, 
         cp_l = 100) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)

##model - dependence p35l100####
out_p35l100_dep_7pop <- jags(model ="TEMPmodel_ntests_dep_7pop.bug", 
                             parameters.to.save=par4, 
                             data=dat_p35l100, n.chains=3,
                             n.iter=50000, n.burnin=10000, n.thin = 2,
                             #inits = inits,
                             DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p35l100_dep_7pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p35l100####
bug_p35l100_d <- print(out_p35l100_dep_7pop, digits.summary=2) 
bug_p35l100_d <- bug_p35l100_d$summary %>%
  data.frame(element = row.names(bug_p35l100_d$summary)) %>%
  mutate(cp_p = 35, 
         cp_l = 100) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)




#plot luminometer accuracy####

bug_lumino <- rbind(bug_p20l50_d,
                    bug_p20l100_d,
                    bug_p20l150_d,
                    bug_p20l200_d,
                    bug_p20l250_d, 
                    bug_p20l300_d
)%>%
  filter(grepl("se", element) | grepl("sp", element)) %>%
  mutate(test = ifelse(grepl("1",element), "Laboratory",
                       ifelse(grepl("2", element), "Tri-Plate",
                              ifelse(grepl("3", element),"Petrifilm",
                                     ifelse(grepl("4", element),"Luminometry",
                                            ifelse(grepl("5", element),"PVD",
                                                   ifelse(grepl("6", element), "Esterase", NA)))))),
         element = ifelse(grepl("se",element), "Se",
                          ifelse(grepl("sp", element), "Sp", NA))) %>%
  filter(test == "Luminometry")

bug_labo <- bug_p20l100_d %>%
  filter(grepl("se", element) | grepl("sp", element)) %>%
  mutate(test = ifelse(grepl("1",element), "Laboratory",
                       ifelse(grepl("2", element), "Tri-Plate",
                              ifelse(grepl("3", element),"Petrifilm",
                                     ifelse(grepl("4", element),"Luminometry",
                                            ifelse(grepl("5", element),"PVD",
                                                   ifelse(grepl("6", element), "Esterase", NA)))))),
         element = ifelse(grepl("se",element), "Se",
                          ifelse(grepl("sp", element), "Sp", NA))) %>%
  filter(test == "Laboratory")

AC_lumino <- ggplot(data = bug_lumino, aes(x = cp_l, y = X50., 
                                           color = element)) +
  geom_point(position = position_dodge(width = 10)) +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.),
                position = position_dodge(width = 10),
                width = 3) +
  geom_path(aes(linetype = element), position = position_dodge(width = 10)) +
  ylab("") +
  theme_bw(base_family = "Times",base_size = 10) +
  xlab("\nThresholds (RLU)") +
  ylim(c(0,1)) +
  scale_x_continuous(n.breaks = 6)+
  theme(legend.position = "none",legend.title = element_blank())
AC_lumino


ROC_lumino <- bug_lumino %>%
  mutate(value = ifelse(element =="Sp", 1-X50., X50.)
  ) %>%
  select(element, value, cp_l)%>%
  pivot_wider(names_from = element, values_from = value) %>%
  ggplot() +
  geom_point( aes(x = Sp, y = Se, 
                  color = 1)) +
  geom_text(aes(x = Sp, y = Se,label = cp_l),
            nudge_y = 0.02) +
  geom_line( aes(x = Sp, y = Se, 
                 color = 1)) +
  geom_abline( intercept = 0, slope = 1, linetype = 3 ) +
  theme_bw(base_family = "Times",base_size = 10) +
  xlab("1 - specificity") +
  ylab("Sensitivity") +
  ylim(c(0,1)) + 
  xlim(c(0,1)) +
  theme(legend.position = "none")


#plot petrifilm accuracy####
bug_petrifilm <- rbind(bug_p10l100_d,
                       bug_p15l100_d,
                       bug_p20l100_d,
                       bug_p25l100_d,
                       bug_p30l100_d,
                       bug_p35l100_d)%>%
  filter(grepl("se", element) | grepl("sp", element)) %>%
  mutate(test = ifelse(grepl("1",element), "Laboratory",
                       ifelse(grepl("2", element), "Tri-Plate",
                              ifelse(grepl("3", element),"Petrifilm",
                                     ifelse(grepl("4", element),"Luminometry",
                                            ifelse(grepl("5", element),"PVD",
                                                   ifelse(grepl("6", element), "Esterase", NA)))))),
         element = ifelse(grepl("se",element), "Se",
                          ifelse(grepl("sp", element), "Sp", NA)),
         cp_p = cp_p*10) %>%
  filter(test == "Petrifilm")

bug_lumino %>% 
  pivot_wider(names_from = element, values_from = c(X50., X2.5., X97.5.)) %>%
  mutate(accuracy = X50._Se+X50._Sp)
bug_petrifilm %>% 
  pivot_wider(names_from = element, values_from = c(X50., X2.5., X97.5.)) %>%
  mutate(accuracy = X50._Se+X50._Sp)

AC_petrifilm <- ggplot(data = bug_petrifilm, aes(x = cp_p, y = X50., 
                                                 color = element)) +
  geom_point(position = position_dodge(width = 10)) +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.),
                position = position_dodge(width = 10),
                width = 3) +
  geom_path(aes(linetype = element), position = position_dodge(width = 10)) +
  ylab("") +
  theme_bw(base_family = "Times",base_size = 10) +
  xlab("\nThresholds (cfu/mL)") +
  ylim(c(0,1)) +
  scale_x_continuous(n.breaks = 6)+
  theme(legend.position = "right",legend.title = element_blank(),
        legend.direction = "vertical")
AC_petrifilm

ROC_petrifilm <- bug_petrifilm %>%
  mutate(value = ifelse(element =="Sp", 1-X50., X50.)
  ) %>%
  select(element, value, cp_p)%>%
  pivot_wider(names_from = element, values_from = value) %>%
  ggplot() +
  geom_point( aes(x = Sp, y = Se, 
                  color = 1)) +
  geom_text(aes(x = Sp, y = Se,label = cp_p),
            nudge_y = 0.02) +
  geom_line( aes(x = Sp, y = Se, 
                 color = 1)) +
  geom_abline( intercept = 0, slope = 1, linetype = 3 ) +
  theme_bw(base_family = "Times",base_size = 10) +
  xlab("1 - specificity") +
  ylab("Sensitivity") +
  ylim(c(0,1)) + 
  xlim(c(0,1)) +
  theme(legend.position = "none")


Fig1_endo.dm <- arrangeGrob(AC_lumino, AC_petrifilm,
                            nrow = 1,
                            layout_matrix = rbind(c(1,1,1,2,2,2,2)))

Fig1_endo.wlables <- as_ggplot(Fig1_endo.dm)+
  draw_plot_label(label = c("A","B"), 
                  size = 10,
                  x = c(0,3/7), 
                  y = c(1,1))

jpeg("Fig1_endo.jpeg", width = 14, 
     height = 7, 
     units = 'cm',res = 600)
Fig1_endo.wlables
dev.off()


#PV and MCT for scenarios####

#PV and MCT for combinations

BM.p20l100_d <- read.csv("bug_p20l100_d.csv")
data0_100 <- subset(BM.p20l100_d, select = c("se.3.", "sp.3.", #petrifilm
                                             "se.4.", "sp.4.", #lumino
                                             "se.5.", "sp.5.", #pvd
                                             "se.6.", "sp.6.", #esterase
                                             "se.1.", "sp.1.", #bacterio
                                             "se.2.", "sp.2.")) #tp
data_100 <- data0_100

BM.p20l250_d <- read.csv("bug_p20l250_d.csv")
data0_250 <- subset(BM.p20l250_d, select = c("se.3.", "sp.3.", #petrifilm
                                             "se.4.", "sp.4.", #lumino
                                             "se.5.", "sp.5.", #pvd
                                             "se.6.", "sp.6.", #esterase
                                             "se.1.", "sp.1.", #bacterio
                                             "se.2.", "sp.2.")) #tp
data_250 <- data0_250

#Strategy 1: labo only####
data_100$se <- data_100$se.1.
data_100$sp <- data_100$sp.1.
#Youden Index
data_100$youden <- data_100$se + data_100$sp -1
youden <- quantile(data_100$youden, c(0.025, 0.5, 0.975))

#PV @20% infection
data_100$ppv20 <- (0.20*data_100$se)/(0.20*data_100$se+(1-0.20)*(1-data_100$sp))
data_100$npv20 <- ((1-0.20)*data_100$sp)/((1-0.20)*data_100$sp+0.20*(1-data_100$se))
#PV @40% infection
data_100$ppv40 <- (0.40*data_100$se)/(0.40*data_100$se+(1-0.40)*(1-data_100$sp))
data_100$npv40 <- ((1-0.40)*data_100$sp)/((1-0.40)*data_100$sp+0.40*(1-data_100$se))
#PV @60% infection
data_100$ppv60 <- (0.60*data_100$se)/(0.60*data_100$se+(1-0.60)*(1-data_100$sp))
data_100$npv60 <- ((1-0.60)*data_100$sp)/((1-0.60)*data_100$sp+0.60*(1-data_100$se))
#MCT @20% infection
data_100$mct20_3_1 <- (1-0.20)*(1-data_100$sp) + (3*0.20)*(1-data_100$se)
data_100$mct20_1_1 <- (1-0.20)*(1-data_100$sp) + (0.20)*(1-data_100$se)
data_100$mct20_1_3 <- (1-0.20)*(1-data_100$sp) + ((1/3)*0.20)*(1-data_100$se)
#MCT @40% infection
data_100$mct40_3_1 <- (1-0.40)*(1-data_100$sp) + (3*0.40)*(1-data_100$se)
data_100$mct40_1_1 <- (1-0.40)*(1-data_100$sp) + (0.40)*(1-data_100$se)
data_100$mct40_1_3 <- (1-0.40)*(1-data_100$sp) + ((1/3)*0.40)*(1-data_100$se)
#MCT @60% infection
data_100$mct60_3_1 <- (1-0.60)*(1-data_100$sp) + (3*0.60)*(1-data_100$se)
data_100$mct60_1_1 <- (1-0.60)*(1-data_100$sp) + (0.60)*(1-data_100$se)
data_100$mct60_1_3 <- (1-0.60)*(1-data_100$sp) + ((1/3)*0.60)*(1-data_100$se)


#dataframe
se <- quantile(data_100$se, c(0.025, 0.5, 0.975))
sp <- quantile(data_100$sp, c(0.025, 0.5, 0.975))
ppv20 <- quantile(data_100$ppv20, c(0.025, 0.5, 0.975))
npv20 <- quantile(data_100$npv20, c(0.025, 0.5, 0.975))
ppv40 <- quantile(data_100$ppv40, c(0.025, 0.5, 0.975))
npv40 <- quantile(data_100$npv40, c(0.025, 0.5, 0.975))
ppv60 <- quantile(data_100$ppv60, c(0.025, 0.5, 0.975))
npv60 <- quantile(data_100$npv60, c(0.025, 0.5, 0.975))
mct20_3_1 <- quantile(data_100$mct20_3_1, c(0.025, 0.5, 0.975))
mct20_1_1 <- quantile(data_100$mct20_1_1, c(0.025, 0.5, 0.975))
mct20_1_3 <- quantile(data_100$mct20_1_3, c(0.025, 0.5, 0.975))
mct40_3_1 <- quantile(data_100$mct40_3_1, c(0.025, 0.5, 0.975))
mct40_1_1 <- quantile(data_100$mct40_1_1, c(0.025, 0.5, 0.975))
mct40_1_3 <- quantile(data_100$mct40_1_3, c(0.025, 0.5, 0.975))
mct60_3_1 <- quantile(data_100$mct60_3_1, c(0.025, 0.5, 0.975))
mct60_1_1 <- quantile(data_100$mct60_1_1, c(0.025, 0.5, 0.975))
mct60_1_3 <- quantile(data_100$mct60_1_3, c(0.025, 0.5, 0.975))

df1 <- data.frame("Labo only", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv20[2], ppv20[1], ppv20[3],
                  npv20[2], npv20[1], npv20[3],
                  ppv40[2], ppv40[1], ppv40[3],
                  npv40[2], npv40[1], npv40[3],
                  ppv60[2], ppv60[1], ppv60[3],
                  npv60[2], npv60[1], npv60[3],
                  
                  mct20_3_1[2], mct20_3_1[1], mct20_3_1[3],
                  mct20_1_1[2], mct20_1_1[1], mct20_1_1[3],
                  mct20_1_3[2], mct20_1_3[1], mct20_1_3[3],
                  
                  mct40_3_1[2], mct40_3_1[1], mct40_3_1[3],
                  mct40_1_1[2], mct40_1_1[1], mct40_1_1[3],
                  mct40_1_3[2], mct40_1_3[1], mct40_1_3[3],
                  
                  mct60_3_1[2], mct60_3_1[1], mct60_3_1[3],
                  mct60_1_1[2], mct60_1_1[1], mct60_1_1[3],
                  mct60_1_3[2], mct60_1_3[1], mct60_1_3[3]
)
rownames(df1) <- NULL
colnames(df1) <- c("Scenario", "se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv20", "ppv20_low", "ppv20_hi",
                   "npv20", "npv20_low", "npv20_hi",
                   "ppv40", "ppv40_low", "ppv40_hi",
                   "npv40", "npv40_low", "npv40_hi",
                   "ppv60", "ppv60_low", "ppv60_hi",
                   "npv60", "npv60_low", "npv60_hi",
                   
                   "mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                   "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                   "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                   
                   "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                   "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                   "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                   
                   "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                   "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                   "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi"
)  

#Strategy 2: PVD only####
data_100$se <- data_100$se.5.
data_100$sp <- data_100$sp.5.
#Youden Index
data_100$youden <- data_100$se + data_100$sp -1
youden <- quantile(data_100$youden, c(0.025, 0.5, 0.975))

#PV @20% infection
data_100$ppv20 <- (0.20*data_100$se)/(0.20*data_100$se+(1-0.20)*(1-data_100$sp))
data_100$npv20 <- ((1-0.20)*data_100$sp)/((1-0.20)*data_100$sp+0.20*(1-data_100$se))
#PV @40% infection
data_100$ppv40 <- (0.40*data_100$se)/(0.40*data_100$se+(1-0.40)*(1-data_100$sp))
data_100$npv40 <- ((1-0.40)*data_100$sp)/((1-0.40)*data_100$sp+0.40*(1-data_100$se))
#PV @60% infection
data_100$ppv60 <- (0.60*data_100$se)/(0.60*data_100$se+(1-0.60)*(1-data_100$sp))
data_100$npv60 <- ((1-0.60)*data_100$sp)/((1-0.60)*data_100$sp+0.60*(1-data_100$se))
#MCT @20% infection
data_100$mct20_3_1 <- (1-0.20)*(1-data_100$sp) + (3*0.20)*(1-data_100$se)
data_100$mct20_1_1 <- (1-0.20)*(1-data_100$sp) + (0.20)*(1-data_100$se)
data_100$mct20_1_3 <- (1-0.20)*(1-data_100$sp) + ((1/3)*0.20)*(1-data_100$se)
#MCT @40% infection
data_100$mct40_3_1 <- (1-0.40)*(1-data_100$sp) + (3*0.40)*(1-data_100$se)
data_100$mct40_1_1 <- (1-0.40)*(1-data_100$sp) + (0.40)*(1-data_100$se)
data_100$mct40_1_3 <- (1-0.40)*(1-data_100$sp) + ((1/3)*0.40)*(1-data_100$se)
#MCT @60% infection
data_100$mct60_3_1 <- (1-0.60)*(1-data_100$sp) + (3*0.60)*(1-data_100$se)
data_100$mct60_1_1 <- (1-0.60)*(1-data_100$sp) + (0.60)*(1-data_100$se)
data_100$mct60_1_3 <- (1-0.60)*(1-data_100$sp) + ((1/3)*0.60)*(1-data_100$se)


#dataframe
se <- quantile(data_100$se, c(0.025, 0.5, 0.975))
sp <- quantile(data_100$sp, c(0.025, 0.5, 0.975))
ppv20 <- quantile(data_100$ppv20, c(0.025, 0.5, 0.975))
npv20 <- quantile(data_100$npv20, c(0.025, 0.5, 0.975))
ppv40 <- quantile(data_100$ppv40, c(0.025, 0.5, 0.975))
npv40 <- quantile(data_100$npv40, c(0.025, 0.5, 0.975))
ppv60 <- quantile(data_100$ppv60, c(0.025, 0.5, 0.975))
npv60 <- quantile(data_100$npv60, c(0.025, 0.5, 0.975))
mct20_3_1 <- quantile(data_100$mct20_3_1, c(0.025, 0.5, 0.975))
mct20_1_1 <- quantile(data_100$mct20_1_1, c(0.025, 0.5, 0.975))
mct20_1_3 <- quantile(data_100$mct20_1_3, c(0.025, 0.5, 0.975))
mct40_3_1 <- quantile(data_100$mct40_3_1, c(0.025, 0.5, 0.975))
mct40_1_1 <- quantile(data_100$mct40_1_1, c(0.025, 0.5, 0.975))
mct40_1_3 <- quantile(data_100$mct40_1_3, c(0.025, 0.5, 0.975))
mct60_3_1 <- quantile(data_100$mct60_3_1, c(0.025, 0.5, 0.975))
mct60_1_1 <- quantile(data_100$mct60_1_1, c(0.025, 0.5, 0.975))
mct60_1_3 <- quantile(data_100$mct60_1_3, c(0.025, 0.5, 0.975))

df2 <- data.frame("PVD only", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv20[2], ppv20[1], ppv20[3],
                  npv20[2], npv20[1], npv20[3],
                  ppv40[2], ppv40[1], ppv40[3],
                  npv40[2], npv40[1], npv40[3],
                  ppv60[2], ppv60[1], ppv60[3],
                  npv60[2], npv60[1], npv60[3],
                  
                  mct20_3_1[2], mct20_3_1[1], mct20_3_1[3],
                  mct20_1_1[2], mct20_1_1[1], mct20_1_1[3],
                  mct20_1_3[2], mct20_1_3[1], mct20_1_3[3],
                  
                  mct40_3_1[2], mct40_3_1[1], mct40_3_1[3],
                  mct40_1_1[2], mct40_1_1[1], mct40_1_1[3],
                  mct40_1_3[2], mct40_1_3[1], mct40_1_3[3],
                  
                  mct60_3_1[2], mct60_3_1[1], mct60_3_1[3],
                  mct60_1_1[2], mct60_1_1[1], mct60_1_1[3],
                  mct60_1_3[2], mct60_1_3[1], mct60_1_3[3]
)
rownames(df2) <- NULL
colnames(df2) <- c("Scenario", "se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv20", "ppv20_low", "ppv20_hi",
                   "npv20", "npv20_low", "npv20_hi",
                   "ppv40", "ppv40_low", "ppv40_hi",
                   "npv40", "npv40_low", "npv40_hi",
                   "ppv60", "ppv60_low", "ppv60_hi",
                   "npv60", "npv60_low", "npv60_hi",
                   
                   "mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                   "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                   "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                   
                   "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                   "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                   "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                   
                   "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                   "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                   "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi"
)  

#Strategy 3: At least one + between PVD and EST####
data_100$se <- data_100$se.5.+data_100$se.6.-(data_100$se.5.*data_100$se.6.)
data_100$sp <- data_100$sp.5. * data_100$sp.6.

#Youden Index
data_100$youden <- data_100$se + data_100$sp -1
youden <- quantile(data_100$youden, c(0.025, 0.5, 0.975))

#series
#data_100$se <- data_100$se.5. * data_100$se.6.
#data_100$sp <- data_100$sp.5. + data_100$sp.6. - (data_100$sp.5. * data_100$sp.6.)
#PV @20% infection
data_100$ppv20 <- (0.20*data_100$se)/(0.20*data_100$se+(1-0.20)*(1-data_100$sp))
data_100$npv20 <- ((1-0.20)*data_100$sp)/((1-0.20)*data_100$sp+0.20*(1-data_100$se))
#PV @40% infection
data_100$ppv40 <- (0.40*data_100$se)/(0.40*data_100$se+(1-0.40)*(1-data_100$sp))
data_100$npv40 <- ((1-0.40)*data_100$sp)/((1-0.40)*data_100$sp+0.40*(1-data_100$se))
#PV @60% infection
data_100$ppv60 <- (0.60*data_100$se)/(0.60*data_100$se+(1-0.60)*(1-data_100$sp))
data_100$npv60 <- ((1-0.60)*data_100$sp)/((1-0.60)*data_100$sp+0.60*(1-data_100$se))
#MCT @20% infection
data_100$mct20_3_1 <- (1-0.20)*(1-data_100$sp) + (3*0.20)*(1-data_100$se)
data_100$mct20_1_1 <- (1-0.20)*(1-data_100$sp) + (0.20)*(1-data_100$se)
data_100$mct20_1_3 <- (1-0.20)*(1-data_100$sp) + ((1/3)*0.20)*(1-data_100$se)
#MCT @40% infection
data_100$mct40_3_1 <- (1-0.40)*(1-data_100$sp) + (3*0.40)*(1-data_100$se)
data_100$mct40_1_1 <- (1-0.40)*(1-data_100$sp) + (0.40)*(1-data_100$se)
data_100$mct40_1_3 <- (1-0.40)*(1-data_100$sp) + ((1/3)*0.40)*(1-data_100$se)
#MCT @60% infection
data_100$mct60_3_1 <- (1-0.60)*(1-data_100$sp) + (3*0.60)*(1-data_100$se)
data_100$mct60_1_1 <- (1-0.60)*(1-data_100$sp) + (0.60)*(1-data_100$se)
data_100$mct60_1_3 <- (1-0.60)*(1-data_100$sp) + ((1/3)*0.60)*(1-data_100$se)


#dataframe
se <- quantile(data_100$se, c(0.025, 0.5, 0.975))
sp <- quantile(data_100$sp, c(0.025, 0.5, 0.975))
ppv20 <- quantile(data_100$ppv20, c(0.025, 0.5, 0.975))
npv20 <- quantile(data_100$npv20, c(0.025, 0.5, 0.975))
ppv40 <- quantile(data_100$ppv40, c(0.025, 0.5, 0.975))
npv40 <- quantile(data_100$npv40, c(0.025, 0.5, 0.975))
ppv60 <- quantile(data_100$ppv60, c(0.025, 0.5, 0.975))
npv60 <- quantile(data_100$npv60, c(0.025, 0.5, 0.975))
mct20_3_1 <- quantile(data_100$mct20_3_1, c(0.025, 0.5, 0.975))
mct20_1_1 <- quantile(data_100$mct20_1_1, c(0.025, 0.5, 0.975))
mct20_1_3 <- quantile(data_100$mct20_1_3, c(0.025, 0.5, 0.975))
mct40_3_1 <- quantile(data_100$mct40_3_1, c(0.025, 0.5, 0.975))
mct40_1_1 <- quantile(data_100$mct40_1_1, c(0.025, 0.5, 0.975))
mct40_1_3 <- quantile(data_100$mct40_1_3, c(0.025, 0.5, 0.975))
mct60_3_1 <- quantile(data_100$mct60_3_1, c(0.025, 0.5, 0.975))
mct60_1_1 <- quantile(data_100$mct60_1_1, c(0.025, 0.5, 0.975))
mct60_1_3 <- quantile(data_100$mct60_1_3, c(0.025, 0.5, 0.975))

df3 <- data.frame("PVD or esterase +", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv20[2], ppv20[1], ppv20[3],
                  npv20[2], npv20[1], npv20[3],
                  ppv40[2], ppv40[1], ppv40[3],
                  npv40[2], npv40[1], npv40[3],
                  ppv60[2], ppv60[1], ppv60[3],
                  npv60[2], npv60[1], npv60[3],
                  
                  mct20_3_1[2], mct20_3_1[1], mct20_3_1[3],
                  mct20_1_1[2], mct20_1_1[1], mct20_1_1[3],
                  mct20_1_3[2], mct20_1_3[1], mct20_1_3[3],
                  
                  mct40_3_1[2], mct40_3_1[1], mct40_3_1[3],
                  mct40_1_1[2], mct40_1_1[1], mct40_1_1[3],
                  mct40_1_3[2], mct40_1_3[1], mct40_1_3[3],
                  
                  mct60_3_1[2], mct60_3_1[1], mct60_3_1[3],
                  mct60_1_1[2], mct60_1_1[1], mct60_1_1[3],
                  mct60_1_3[2], mct60_1_3[1], mct60_1_3[3]
)
rownames(df3) <- NULL
colnames(df3) <- c("Scenario", "se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv20", "ppv20_low", "ppv20_hi",
                   "npv20", "npv20_low", "npv20_hi",
                   "ppv40", "ppv40_low", "ppv40_hi",
                   "npv40", "npv40_low", "npv40_hi",
                   "ppv60", "ppv60_low", "ppv60_hi",
                   "npv60", "npv60_low", "npv60_hi",
                   
                   "mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                   "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                   "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                   
                   "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                   "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                   "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                   
                   "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                   "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                   "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi"
)  




#Strategy 4: lumino only (100 RLU)####
data_100$se <- data_100$se.4.
data_100$sp <- data_100$sp.4.
#Youden Index
data_100$youden <- data_100$se + data_100$sp -1
youden <- quantile(data_100$youden, c(0.025, 0.5, 0.975))

#PV @20% infection
data_100$ppv20 <- (0.20*data_100$se)/(0.20*data_100$se+(1-0.20)*(1-data_100$sp))
data_100$npv20 <- ((1-0.20)*data_100$sp)/((1-0.20)*data_100$sp+0.20*(1-data_100$se))
#PV @40% infection
data_100$ppv40 <- (0.40*data_100$se)/(0.40*data_100$se+(1-0.40)*(1-data_100$sp))
data_100$npv40 <- ((1-0.40)*data_100$sp)/((1-0.40)*data_100$sp+0.40*(1-data_100$se))
#PV @60% infection
data_100$ppv60 <- (0.60*data_100$se)/(0.60*data_100$se+(1-0.60)*(1-data_100$sp))
data_100$npv60 <- ((1-0.60)*data_100$sp)/((1-0.60)*data_100$sp+0.60*(1-data_100$se))
#MCT @20% infection
data_100$mct20_3_1 <- (1-0.20)*(1-data_100$sp) + (3*0.20)*(1-data_100$se)
data_100$mct20_1_1 <- (1-0.20)*(1-data_100$sp) + (0.20)*(1-data_100$se)
data_100$mct20_1_3 <- (1-0.20)*(1-data_100$sp) + ((1/3)*0.20)*(1-data_100$se)
#MCT @40% infection
data_100$mct40_3_1 <- (1-0.40)*(1-data_100$sp) + (3*0.40)*(1-data_100$se)
data_100$mct40_1_1 <- (1-0.40)*(1-data_100$sp) + (0.40)*(1-data_100$se)
data_100$mct40_1_3 <- (1-0.40)*(1-data_100$sp) + ((1/3)*0.40)*(1-data_100$se)
#MCT @60% infection
data_100$mct60_3_1 <- (1-0.60)*(1-data_100$sp) + (3*0.60)*(1-data_100$se)
data_100$mct60_1_1 <- (1-0.60)*(1-data_100$sp) + (0.60)*(1-data_100$se)
data_100$mct60_1_3 <- (1-0.60)*(1-data_100$sp) + ((1/3)*0.60)*(1-data_100$se)


#dataframe
se <- quantile(data_100$se, c(0.025, 0.5, 0.975))
sp <- quantile(data_100$sp, c(0.025, 0.5, 0.975))
ppv20 <- quantile(data_100$ppv20, c(0.025, 0.5, 0.975))
npv20 <- quantile(data_100$npv20, c(0.025, 0.5, 0.975))
ppv40 <- quantile(data_100$ppv40, c(0.025, 0.5, 0.975))
npv40 <- quantile(data_100$npv40, c(0.025, 0.5, 0.975))
ppv60 <- quantile(data_100$ppv60, c(0.025, 0.5, 0.975))
npv60 <- quantile(data_100$npv60, c(0.025, 0.5, 0.975))
mct20_3_1 <- quantile(data_100$mct20_3_1, c(0.025, 0.5, 0.975))
mct20_1_1 <- quantile(data_100$mct20_1_1, c(0.025, 0.5, 0.975))
mct20_1_3 <- quantile(data_100$mct20_1_3, c(0.025, 0.5, 0.975))
mct40_3_1 <- quantile(data_100$mct40_3_1, c(0.025, 0.5, 0.975))
mct40_1_1 <- quantile(data_100$mct40_1_1, c(0.025, 0.5, 0.975))
mct40_1_3 <- quantile(data_100$mct40_1_3, c(0.025, 0.5, 0.975))
mct60_3_1 <- quantile(data_100$mct60_3_1, c(0.025, 0.5, 0.975))
mct60_1_1 <- quantile(data_100$mct60_1_1, c(0.025, 0.5, 0.975))
mct60_1_3 <- quantile(data_100$mct60_1_3, c(0.025, 0.5, 0.975))

df4 <- data.frame("Luminometry only (100 RLU)", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv20[2], ppv20[1], ppv20[3],
                  npv20[2], npv20[1], npv20[3],
                  ppv40[2], ppv40[1], ppv40[3],
                  npv40[2], npv40[1], npv40[3],
                  ppv60[2], ppv60[1], ppv60[3],
                  npv60[2], npv60[1], npv60[3],
                  
                  mct20_3_1[2], mct20_3_1[1], mct20_3_1[3],
                  mct20_1_1[2], mct20_1_1[1], mct20_1_1[3],
                  mct20_1_3[2], mct20_1_3[1], mct20_1_3[3],
                  
                  mct40_3_1[2], mct40_3_1[1], mct40_3_1[3],
                  mct40_1_1[2], mct40_1_1[1], mct40_1_1[3],
                  mct40_1_3[2], mct40_1_3[1], mct40_1_3[3],
                  
                  mct60_3_1[2], mct60_3_1[1], mct60_3_1[3],
                  mct60_1_1[2], mct60_1_1[1], mct60_1_1[3],
                  mct60_1_3[2], mct60_1_3[1], mct60_1_3[3]
)
rownames(df4) <- NULL
colnames(df4) <- c("Scenario", "se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv20", "ppv20_low", "ppv20_hi",
                   "npv20", "npv20_low", "npv20_hi",
                   "ppv40", "ppv40_low", "ppv40_hi",
                   "npv40", "npv40_low", "npv40_hi",
                   "ppv60", "ppv60_low", "ppv60_hi",
                   "npv60", "npv60_low", "npv60_hi",
                   
                   "mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                   "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                   "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                   
                   "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                   "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                   "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                   
                   "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                   "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                   "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi"
)  

#Strategy 5: lumino only (250 RLU)####
data_250$se <- data_250$se.4.
data_250$sp <- data_250$sp.4.
#Youden Index
data_250$youden <- data_250$se + data_250$sp -1
youden <- quantile(data_250$youden, c(0.025, 0.5, 0.975))

#PV @20% infection
data_250$ppv20 <- (0.20*data_250$se)/(0.20*data_250$se+(1-0.20)*(1-data_250$sp))
data_250$npv20 <- ((1-0.20)*data_250$sp)/((1-0.20)*data_250$sp+0.20*(1-data_250$se))
#PV @40% infection
data_250$ppv40 <- (0.40*data_250$se)/(0.40*data_250$se+(1-0.40)*(1-data_250$sp))
data_250$npv40 <- ((1-0.40)*data_250$sp)/((1-0.40)*data_250$sp+0.40*(1-data_250$se))
#PV @60% infection
data_250$ppv60 <- (0.60*data_250$se)/(0.60*data_250$se+(1-0.60)*(1-data_250$sp))
data_250$npv60 <- ((1-0.60)*data_250$sp)/((1-0.60)*data_250$sp+0.60*(1-data_250$se))
#MCT @20% infection
data_250$mct20_3_1 <- (1-0.20)*(1-data_250$sp) + (3*0.20)*(1-data_250$se)
data_250$mct20_1_1 <- (1-0.20)*(1-data_250$sp) + (0.20)*(1-data_250$se)
data_250$mct20_1_3 <- (1-0.20)*(1-data_250$sp) + ((1/3)*0.20)*(1-data_250$se)
#MCT @40% infection
data_250$mct40_3_1 <- (1-0.40)*(1-data_250$sp) + (3*0.40)*(1-data_250$se)
data_250$mct40_1_1 <- (1-0.40)*(1-data_250$sp) + (0.40)*(1-data_250$se)
data_250$mct40_1_3 <- (1-0.40)*(1-data_250$sp) + ((1/3)*0.40)*(1-data_250$se)
#MCT @60% infection
data_250$mct60_3_1 <- (1-0.60)*(1-data_250$sp) + (3*0.60)*(1-data_250$se)
data_250$mct60_1_1 <- (1-0.60)*(1-data_250$sp) + (0.60)*(1-data_250$se)
data_250$mct60_1_3 <- (1-0.60)*(1-data_250$sp) + ((1/3)*0.60)*(1-data_250$se)


#dataframe
se <- quantile(data_250$se, c(0.025, 0.5, 0.975))
sp <- quantile(data_250$sp, c(0.025, 0.5, 0.975))
ppv20 <- quantile(data_250$ppv20, c(0.025, 0.5, 0.975))
npv20 <- quantile(data_250$npv20, c(0.025, 0.5, 0.975))
ppv40 <- quantile(data_250$ppv40, c(0.025, 0.5, 0.975))
npv40 <- quantile(data_250$npv40, c(0.025, 0.5, 0.975))
ppv60 <- quantile(data_250$ppv60, c(0.025, 0.5, 0.975))
npv60 <- quantile(data_250$npv60, c(0.025, 0.5, 0.975))
mct20_3_1 <- quantile(data_250$mct20_3_1, c(0.025, 0.5, 0.975))
mct20_1_1 <- quantile(data_250$mct20_1_1, c(0.025, 0.5, 0.975))
mct20_1_3 <- quantile(data_250$mct20_1_3, c(0.025, 0.5, 0.975))
mct40_3_1 <- quantile(data_250$mct40_3_1, c(0.025, 0.5, 0.975))
mct40_1_1 <- quantile(data_250$mct40_1_1, c(0.025, 0.5, 0.975))
mct40_1_3 <- quantile(data_250$mct40_1_3, c(0.025, 0.5, 0.975))
mct60_3_1 <- quantile(data_250$mct60_3_1, c(0.025, 0.5, 0.975))
mct60_1_1 <- quantile(data_250$mct60_1_1, c(0.025, 0.5, 0.975))
mct60_1_3 <- quantile(data_250$mct60_1_3, c(0.025, 0.5, 0.975))

df5 <- data.frame("Luminometry only (250 RLU)", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv20[2], ppv20[1], ppv20[3],
                  npv20[2], npv20[1], npv20[3],
                  ppv40[2], ppv40[1], ppv40[3],
                  npv40[2], npv40[1], npv40[3],
                  ppv60[2], ppv60[1], ppv60[3],
                  npv60[2], npv60[1], npv60[3],
                  
                  mct20_3_1[2], mct20_3_1[1], mct20_3_1[3],
                  mct20_1_1[2], mct20_1_1[1], mct20_1_1[3],
                  mct20_1_3[2], mct20_1_3[1], mct20_1_3[3],
                  
                  mct40_3_1[2], mct40_3_1[1], mct40_3_1[3],
                  mct40_1_1[2], mct40_1_1[1], mct40_1_1[3],
                  mct40_1_3[2], mct40_1_3[1], mct40_1_3[3],
                  
                  mct60_3_1[2], mct60_3_1[1], mct60_3_1[3],
                  mct60_1_1[2], mct60_1_1[1], mct60_1_1[3],
                  mct60_1_3[2], mct60_1_3[1], mct60_1_3[3]
)
rownames(df5) <- NULL
colnames(df5) <- c("Scenario", "se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv20", "ppv20_low", "ppv20_hi",
                   "npv20", "npv20_low", "npv20_hi",
                   "ppv40", "ppv40_low", "ppv40_hi",
                   "npv40", "npv40_low", "npv40_hi",
                   "ppv60", "ppv60_low", "ppv60_hi",
                   "npv60", "npv60_low", "npv60_hi",
                   
                   "mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                   "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                   "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                   
                   "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                   "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                   "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                   
                   "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                   "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                   "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi"
)  

#Strategy 6: Both EST and lumino (100 RLU) + ####
data_100$se <- data_100$se.4. * data_100$se.6.
data_100$sp <- data_100$sp.4. + data_100$sp.6. - (data_100$sp.4. * data_100$sp.6.)
#Youden Index
data_100$youden <- data_100$se + data_100$sp -1
youden <- quantile(data_100$youden, c(0.025, 0.5, 0.975))

#PV @20% infection
data_100$ppv20 <- (0.20*data_100$se)/(0.20*data_100$se+(1-0.20)*(1-data_100$sp))
data_100$npv20 <- ((1-0.20)*data_100$sp)/((1-0.20)*data_100$sp+0.20*(1-data_100$se))
#PV @40% infection
data_100$ppv40 <- (0.40*data_100$se)/(0.40*data_100$se+(1-0.40)*(1-data_100$sp))
data_100$npv40 <- ((1-0.40)*data_100$sp)/((1-0.40)*data_100$sp+0.40*(1-data_100$se))
#PV @60% infection
data_100$ppv60 <- (0.60*data_100$se)/(0.60*data_100$se+(1-0.60)*(1-data_100$sp))
data_100$npv60 <- ((1-0.60)*data_100$sp)/((1-0.60)*data_100$sp+0.60*(1-data_100$se))
#MCT @20% infection
data_100$mct20_3_1 <- (1-0.20)*(1-data_100$sp) + (3*0.20)*(1-data_100$se)
data_100$mct20_1_1 <- (1-0.20)*(1-data_100$sp) + (0.20)*(1-data_100$se)
data_100$mct20_1_3 <- (1-0.20)*(1-data_100$sp) + ((1/3)*0.20)*(1-data_100$se)
#MCT @40% infection
data_100$mct40_3_1 <- (1-0.40)*(1-data_100$sp) + (3*0.40)*(1-data_100$se)
data_100$mct40_1_1 <- (1-0.40)*(1-data_100$sp) + (0.40)*(1-data_100$se)
data_100$mct40_1_3 <- (1-0.40)*(1-data_100$sp) + ((1/3)*0.40)*(1-data_100$se)
#MCT @60% infection
data_100$mct60_3_1 <- (1-0.60)*(1-data_100$sp) + (3*0.60)*(1-data_100$se)
data_100$mct60_1_1 <- (1-0.60)*(1-data_100$sp) + (0.60)*(1-data_100$se)
data_100$mct60_1_3 <- (1-0.60)*(1-data_100$sp) + ((1/3)*0.60)*(1-data_100$se)


#dataframe
se <- quantile(data_100$se, c(0.025, 0.5, 0.975))
sp <- quantile(data_100$sp, c(0.025, 0.5, 0.975))
ppv20 <- quantile(data_100$ppv20, c(0.025, 0.5, 0.975))
npv20 <- quantile(data_100$npv20, c(0.025, 0.5, 0.975))
ppv40 <- quantile(data_100$ppv40, c(0.025, 0.5, 0.975))
npv40 <- quantile(data_100$npv40, c(0.025, 0.5, 0.975))
ppv60 <- quantile(data_100$ppv60, c(0.025, 0.5, 0.975))
npv60 <- quantile(data_100$npv60, c(0.025, 0.5, 0.975))
mct20_3_1 <- quantile(data_100$mct20_3_1, c(0.025, 0.5, 0.975))
mct20_1_1 <- quantile(data_100$mct20_1_1, c(0.025, 0.5, 0.975))
mct20_1_3 <- quantile(data_100$mct20_1_3, c(0.025, 0.5, 0.975))
mct40_3_1 <- quantile(data_100$mct40_3_1, c(0.025, 0.5, 0.975))
mct40_1_1 <- quantile(data_100$mct40_1_1, c(0.025, 0.5, 0.975))
mct40_1_3 <- quantile(data_100$mct40_1_3, c(0.025, 0.5, 0.975))
mct60_3_1 <- quantile(data_100$mct60_3_1, c(0.025, 0.5, 0.975))
mct60_1_1 <- quantile(data_100$mct60_1_1, c(0.025, 0.5, 0.975))
mct60_1_3 <- quantile(data_100$mct60_1_3, c(0.025, 0.5, 0.975))

df6 <- data.frame("Luminometry (100 RLU) and esterase +", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv20[2], ppv20[1], ppv20[3],
                  npv20[2], npv20[1], npv20[3],
                  ppv40[2], ppv40[1], ppv40[3],
                  npv40[2], npv40[1], npv40[3],
                  ppv60[2], ppv60[1], ppv60[3],
                  npv60[2], npv60[1], npv60[3],
                  
                  mct20_3_1[2], mct20_3_1[1], mct20_3_1[3],
                  mct20_1_1[2], mct20_1_1[1], mct20_1_1[3],
                  mct20_1_3[2], mct20_1_3[1], mct20_1_3[3],
                  
                  mct40_3_1[2], mct40_3_1[1], mct40_3_1[3],
                  mct40_1_1[2], mct40_1_1[1], mct40_1_1[3],
                  mct40_1_3[2], mct40_1_3[1], mct40_1_3[3],
                  
                  mct60_3_1[2], mct60_3_1[1], mct60_3_1[3],
                  mct60_1_1[2], mct60_1_1[1], mct60_1_1[3],
                  mct60_1_3[2], mct60_1_3[1], mct60_1_3[3]
)
rownames(df6) <- NULL
colnames(df6) <- c("Scenario", "se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv20", "ppv20_low", "ppv20_hi",
                   "npv20", "npv20_low", "npv20_hi",
                   "ppv40", "ppv40_low", "ppv40_hi",
                   "npv40", "npv40_low", "npv40_hi",
                   "ppv60", "ppv60_low", "ppv60_hi",
                   "npv60", "npv60_low", "npv60_hi",
                   
                   "mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                   "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                   "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                   
                   "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                   "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                   "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                   
                   "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                   "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                   "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi"
)  


#Strategy 7: Both EST and lumino (250 RLU) + ####
data_250$se <- data_250$se.4. * data_250$se.6.
data_250$sp <- data_250$sp.4. + data_250$sp.6. - (data_250$sp.4. * data_250$sp.6.)
#Youden Index
data_250$youden <- data_250$se + data_250$sp -1
youden <- quantile(data_250$youden, c(0.025, 0.5, 0.975))

#PV @20% infection
data_250$ppv20 <- (0.20*data_250$se)/(0.20*data_250$se+(1-0.20)*(1-data_250$sp))
data_250$npv20 <- ((1-0.20)*data_250$sp)/((1-0.20)*data_250$sp+0.20*(1-data_250$se))
#PV @40% infection
data_250$ppv40 <- (0.40*data_250$se)/(0.40*data_250$se+(1-0.40)*(1-data_250$sp))
data_250$npv40 <- ((1-0.40)*data_250$sp)/((1-0.40)*data_250$sp+0.40*(1-data_250$se))
#PV @60% infection
data_250$ppv60 <- (0.60*data_250$se)/(0.60*data_250$se+(1-0.60)*(1-data_250$sp))
data_250$npv60 <- ((1-0.60)*data_250$sp)/((1-0.60)*data_250$sp+0.60*(1-data_250$se))
#MCT @20% infection
data_250$mct20_3_1 <- (1-0.20)*(1-data_250$sp) + (3*0.20)*(1-data_250$se)
data_250$mct20_1_1 <- (1-0.20)*(1-data_250$sp) + (0.20)*(1-data_250$se)
data_250$mct20_1_3 <- (1-0.20)*(1-data_250$sp) + ((1/3)*0.20)*(1-data_250$se)
#MCT @40% infection
data_250$mct40_3_1 <- (1-0.40)*(1-data_250$sp) + (3*0.40)*(1-data_250$se)
data_250$mct40_1_1 <- (1-0.40)*(1-data_250$sp) + (0.40)*(1-data_250$se)
data_250$mct40_1_3 <- (1-0.40)*(1-data_250$sp) + ((1/3)*0.40)*(1-data_250$se)
#MCT @60% infection
data_250$mct60_3_1 <- (1-0.60)*(1-data_250$sp) + (3*0.60)*(1-data_250$se)
data_250$mct60_1_1 <- (1-0.60)*(1-data_250$sp) + (0.60)*(1-data_250$se)
data_250$mct60_1_3 <- (1-0.60)*(1-data_250$sp) + ((1/3)*0.60)*(1-data_250$se)


#dataframe
se <- quantile(data_250$se, c(0.025, 0.5, 0.975))
sp <- quantile(data_250$sp, c(0.025, 0.5, 0.975))
ppv20 <- quantile(data_250$ppv20, c(0.025, 0.5, 0.975))
npv20 <- quantile(data_250$npv20, c(0.025, 0.5, 0.975))
ppv40 <- quantile(data_250$ppv40, c(0.025, 0.5, 0.975))
npv40 <- quantile(data_250$npv40, c(0.025, 0.5, 0.975))
ppv60 <- quantile(data_250$ppv60, c(0.025, 0.5, 0.975))
npv60 <- quantile(data_250$npv60, c(0.025, 0.5, 0.975))
mct20_3_1 <- quantile(data_250$mct20_3_1, c(0.025, 0.5, 0.975))
mct20_1_1 <- quantile(data_250$mct20_1_1, c(0.025, 0.5, 0.975))
mct20_1_3 <- quantile(data_250$mct20_1_3, c(0.025, 0.5, 0.975))
mct40_3_1 <- quantile(data_250$mct40_3_1, c(0.025, 0.5, 0.975))
mct40_1_1 <- quantile(data_250$mct40_1_1, c(0.025, 0.5, 0.975))
mct40_1_3 <- quantile(data_250$mct40_1_3, c(0.025, 0.5, 0.975))
mct60_3_1 <- quantile(data_250$mct60_3_1, c(0.025, 0.5, 0.975))
mct60_1_1 <- quantile(data_250$mct60_1_1, c(0.025, 0.5, 0.975))
mct60_1_3 <- quantile(data_250$mct60_1_3, c(0.025, 0.5, 0.975))

df7 <- data.frame("Luminometry (250 RLU) and esterase +", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv20[2], ppv20[1], ppv20[3],
                  npv20[2], npv20[1], npv20[3],
                  ppv40[2], ppv40[1], ppv40[3],
                  npv40[2], npv40[1], npv40[3],
                  ppv60[2], ppv60[1], ppv60[3],
                  npv60[2], npv60[1], npv60[3],
                  
                  mct20_3_1[2], mct20_3_1[1], mct20_3_1[3],
                  mct20_1_1[2], mct20_1_1[1], mct20_1_1[3],
                  mct20_1_3[2], mct20_1_3[1], mct20_1_3[3],
                  
                  mct40_3_1[2], mct40_3_1[1], mct40_3_1[3],
                  mct40_1_1[2], mct40_1_1[1], mct40_1_1[3],
                  mct40_1_3[2], mct40_1_3[1], mct40_1_3[3],
                  
                  mct60_3_1[2], mct60_3_1[1], mct60_3_1[3],
                  mct60_1_1[2], mct60_1_1[1], mct60_1_1[3],
                  mct60_1_3[2], mct60_1_3[1], mct60_1_3[3]
)
rownames(df7) <- NULL
colnames(df7) <- c("Scenario", "se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv20", "ppv20_low", "ppv20_hi",
                   "npv20", "npv20_low", "npv20_hi",
                   "ppv40", "ppv40_low", "ppv40_hi",
                   "npv40", "npv40_low", "npv40_hi",
                   "ppv60", "ppv60_low", "ppv60_hi",
                   "npv60", "npv60_low", "npv60_hi",
                   
                   "mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                   "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                   "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                   
                   "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                   "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                   "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                   
                   "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                   "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                   "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi"
)  


#Strategy 8: petrifilm only####
data_100$se <- data_100$se.3.
data_100$sp <- data_100$sp.3.
#Youden Index
data_100$youden <- data_100$se + data_100$sp -1
youden <- quantile(data_100$youden, c(0.025, 0.5, 0.975))

#PV @20% infection
data_100$ppv20 <- (0.20*data_100$se)/(0.20*data_100$se+(1-0.20)*(1-data_100$sp))
data_100$npv20 <- ((1-0.20)*data_100$sp)/((1-0.20)*data_100$sp+0.20*(1-data_100$se))
#PV @40% infection
data_100$ppv40 <- (0.40*data_100$se)/(0.40*data_100$se+(1-0.40)*(1-data_100$sp))
data_100$npv40 <- ((1-0.40)*data_100$sp)/((1-0.40)*data_100$sp+0.40*(1-data_100$se))
#PV @60% infection
data_100$ppv60 <- (0.60*data_100$se)/(0.60*data_100$se+(1-0.60)*(1-data_100$sp))
data_100$npv60 <- ((1-0.60)*data_100$sp)/((1-0.60)*data_100$sp+0.60*(1-data_100$se))
#MCT @20% infection
data_100$mct20_3_1 <- (1-0.20)*(1-data_100$sp) + (3*0.20)*(1-data_100$se)
data_100$mct20_1_1 <- (1-0.20)*(1-data_100$sp) + (0.20)*(1-data_100$se)
data_100$mct20_1_3 <- (1-0.20)*(1-data_100$sp) + ((1/3)*0.20)*(1-data_100$se)
#MCT @40% infection
data_100$mct40_3_1 <- (1-0.40)*(1-data_100$sp) + (3*0.40)*(1-data_100$se)
data_100$mct40_1_1 <- (1-0.40)*(1-data_100$sp) + (0.40)*(1-data_100$se)
data_100$mct40_1_3 <- (1-0.40)*(1-data_100$sp) + ((1/3)*0.40)*(1-data_100$se)
#MCT @60% infection
data_100$mct60_3_1 <- (1-0.60)*(1-data_100$sp) + (3*0.60)*(1-data_100$se)
data_100$mct60_1_1 <- (1-0.60)*(1-data_100$sp) + (0.60)*(1-data_100$se)
data_100$mct60_1_3 <- (1-0.60)*(1-data_100$sp) + ((1/3)*0.60)*(1-data_100$se)


#dataframe
se <- quantile(data_100$se, c(0.025, 0.5, 0.975))
sp <- quantile(data_100$sp, c(0.025, 0.5, 0.975))
ppv20 <- quantile(data_100$ppv20, c(0.025, 0.5, 0.975))
npv20 <- quantile(data_100$npv20, c(0.025, 0.5, 0.975))
ppv40 <- quantile(data_100$ppv40, c(0.025, 0.5, 0.975))
npv40 <- quantile(data_100$npv40, c(0.025, 0.5, 0.975))
ppv60 <- quantile(data_100$ppv60, c(0.025, 0.5, 0.975))
npv60 <- quantile(data_100$npv60, c(0.025, 0.5, 0.975))
mct20_3_1 <- quantile(data_100$mct20_3_1, c(0.025, 0.5, 0.975))
mct20_1_1 <- quantile(data_100$mct20_1_1, c(0.025, 0.5, 0.975))
mct20_1_3 <- quantile(data_100$mct20_1_3, c(0.025, 0.5, 0.975))
mct40_3_1 <- quantile(data_100$mct40_3_1, c(0.025, 0.5, 0.975))
mct40_1_1 <- quantile(data_100$mct40_1_1, c(0.025, 0.5, 0.975))
mct40_1_3 <- quantile(data_100$mct40_1_3, c(0.025, 0.5, 0.975))
mct60_3_1 <- quantile(data_100$mct60_3_1, c(0.025, 0.5, 0.975))
mct60_1_1 <- quantile(data_100$mct60_1_1, c(0.025, 0.5, 0.975))
mct60_1_3 <- quantile(data_100$mct60_1_3, c(0.025, 0.5, 0.975))

df8 <- data.frame("Petrifilm only", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv20[2], ppv20[1], ppv20[3],
                  npv20[2], npv20[1], npv20[3],
                  ppv40[2], ppv40[1], ppv40[3],
                  npv40[2], npv40[1], npv40[3],
                  ppv60[2], ppv60[1], ppv60[3],
                  npv60[2], npv60[1], npv60[3],
                  
                  mct20_3_1[2], mct20_3_1[1], mct20_3_1[3],
                  mct20_1_1[2], mct20_1_1[1], mct20_1_1[3],
                  mct20_1_3[2], mct20_1_3[1], mct20_1_3[3],
                  
                  mct40_3_1[2], mct40_3_1[1], mct40_3_1[3],
                  mct40_1_1[2], mct40_1_1[1], mct40_1_1[3],
                  mct40_1_3[2], mct40_1_3[1], mct40_1_3[3],
                  
                  mct60_3_1[2], mct60_3_1[1], mct60_3_1[3],
                  mct60_1_1[2], mct60_1_1[1], mct60_1_1[3],
                  mct60_1_3[2], mct60_1_3[1], mct60_1_3[3]
)
rownames(df8) <- NULL
colnames(df8) <- c("Scenario", "se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv20", "ppv20_low", "ppv20_hi",
                   "npv20", "npv20_low", "npv20_hi",
                   "ppv40", "ppv40_low", "ppv40_hi",
                   "npv40", "npv40_low", "npv40_hi",
                   "ppv60", "ppv60_low", "ppv60_hi",
                   "npv60", "npv60_low", "npv60_hi",
                   
                   "mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                   "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                   "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                   
                   "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                   "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                   "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                   
                   "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                   "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                   "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi"
)  

#Strategy 9: esterase only####
data_100$se <- data_100$se.6.
data_100$sp <- data_100$sp.6.
#Youden Index
data_100$youden <- data_100$se + data_100$sp -1
youden <- quantile(data_100$youden, c(0.025, 0.5, 0.975))

#PV @20% infection
data_100$ppv20 <- (0.20*data_100$se)/(0.20*data_100$se+(1-0.20)*(1-data_100$sp))
data_100$npv20 <- ((1-0.20)*data_100$sp)/((1-0.20)*data_100$sp+0.20*(1-data_100$se))
#PV @40% infection
data_100$ppv40 <- (0.40*data_100$se)/(0.40*data_100$se+(1-0.40)*(1-data_100$sp))
data_100$npv40 <- ((1-0.40)*data_100$sp)/((1-0.40)*data_100$sp+0.40*(1-data_100$se))
#PV @60% infection
data_100$ppv60 <- (0.60*data_100$se)/(0.60*data_100$se+(1-0.60)*(1-data_100$sp))
data_100$npv60 <- ((1-0.60)*data_100$sp)/((1-0.60)*data_100$sp+0.60*(1-data_100$se))
#MCT @20% infection
data_100$mct20_3_1 <- (1-0.20)*(1-data_100$sp) + (3*0.20)*(1-data_100$se)
data_100$mct20_1_1 <- (1-0.20)*(1-data_100$sp) + (0.20)*(1-data_100$se)
data_100$mct20_1_3 <- (1-0.20)*(1-data_100$sp) + ((1/3)*0.20)*(1-data_100$se)
#MCT @40% infection
data_100$mct40_3_1 <- (1-0.40)*(1-data_100$sp) + (3*0.40)*(1-data_100$se)
data_100$mct40_1_1 <- (1-0.40)*(1-data_100$sp) + (0.40)*(1-data_100$se)
data_100$mct40_1_3 <- (1-0.40)*(1-data_100$sp) + ((1/3)*0.40)*(1-data_100$se)
#MCT @60% infection
data_100$mct60_3_1 <- (1-0.60)*(1-data_100$sp) + (3*0.60)*(1-data_100$se)
data_100$mct60_1_1 <- (1-0.60)*(1-data_100$sp) + (0.60)*(1-data_100$se)
data_100$mct60_1_3 <- (1-0.60)*(1-data_100$sp) + ((1/3)*0.60)*(1-data_100$se)


#dataframe
se <- quantile(data_100$se, c(0.025, 0.5, 0.975))
sp <- quantile(data_100$sp, c(0.025, 0.5, 0.975))
ppv20 <- quantile(data_100$ppv20, c(0.025, 0.5, 0.975))
npv20 <- quantile(data_100$npv20, c(0.025, 0.5, 0.975))
ppv40 <- quantile(data_100$ppv40, c(0.025, 0.5, 0.975))
npv40 <- quantile(data_100$npv40, c(0.025, 0.5, 0.975))
ppv60 <- quantile(data_100$ppv60, c(0.025, 0.5, 0.975))
npv60 <- quantile(data_100$npv60, c(0.025, 0.5, 0.975))
mct20_3_1 <- quantile(data_100$mct20_3_1, c(0.025, 0.5, 0.975))
mct20_1_1 <- quantile(data_100$mct20_1_1, c(0.025, 0.5, 0.975))
mct20_1_3 <- quantile(data_100$mct20_1_3, c(0.025, 0.5, 0.975))
mct40_3_1 <- quantile(data_100$mct40_3_1, c(0.025, 0.5, 0.975))
mct40_1_1 <- quantile(data_100$mct40_1_1, c(0.025, 0.5, 0.975))
mct40_1_3 <- quantile(data_100$mct40_1_3, c(0.025, 0.5, 0.975))
mct60_3_1 <- quantile(data_100$mct60_3_1, c(0.025, 0.5, 0.975))
mct60_1_1 <- quantile(data_100$mct60_1_1, c(0.025, 0.5, 0.975))
mct60_1_3 <- quantile(data_100$mct60_1_3, c(0.025, 0.5, 0.975))

df9 <- data.frame("ENDO only", se[2], se[1], se[3], 
                  sp[2], sp[1], sp[3],
                  
                  ppv20[2], ppv20[1], ppv20[3],
                  npv20[2], npv20[1], npv20[3],
                  ppv40[2], ppv40[1], ppv40[3],
                  npv40[2], npv40[1], npv40[3],
                  ppv60[2], ppv60[1], ppv60[3],
                  npv60[2], npv60[1], npv60[3],
                  
                  mct20_3_1[2], mct20_3_1[1], mct20_3_1[3],
                  mct20_1_1[2], mct20_1_1[1], mct20_1_1[3],
                  mct20_1_3[2], mct20_1_3[1], mct20_1_3[3],
                  
                  mct40_3_1[2], mct40_3_1[1], mct40_3_1[3],
                  mct40_1_1[2], mct40_1_1[1], mct40_1_1[3],
                  mct40_1_3[2], mct40_1_3[1], mct40_1_3[3],
                  
                  mct60_3_1[2], mct60_3_1[1], mct60_3_1[3],
                  mct60_1_1[2], mct60_1_1[1], mct60_1_1[3],
                  mct60_1_3[2], mct60_1_3[1], mct60_1_3[3]
)
rownames(df9) <- NULL
colnames(df9) <- c("Scenario", "se", "se_low", "se_hi", 
                   "sp", "sp_low", "sp_hi",
                   
                   "ppv20", "ppv20_low", "ppv20_hi",
                   "npv20", "npv20_low", "npv20_hi",
                   "ppv40", "ppv40_low", "ppv40_hi",
                   "npv40", "npv40_low", "npv40_hi",
                   "ppv60", "ppv60_low", "ppv60_hi",
                   "npv60", "npv60_low", "npv60_hi",
                   
                   "mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                   "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                   "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                   
                   "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                   "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                   "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                   
                   "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                   "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                   "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi"
)  

#Strategy 10: TP only####
data_100$se <- data_100$se.2.
data_100$sp <- data_100$sp.2.
#Youden Index
data_100$youden <- data_100$se + data_100$sp -1
youden <- quantile(data_100$youden, c(0.025, 0.5, 0.975))

#PV @20% infection
data_100$ppv20 <- (0.20*data_100$se)/(0.20*data_100$se+(1-0.20)*(1-data_100$sp))
data_100$npv20 <- ((1-0.20)*data_100$sp)/((1-0.20)*data_100$sp+0.20*(1-data_100$se))
#PV @40% infection
data_100$ppv40 <- (0.40*data_100$se)/(0.40*data_100$se+(1-0.40)*(1-data_100$sp))
data_100$npv40 <- ((1-0.40)*data_100$sp)/((1-0.40)*data_100$sp+0.40*(1-data_100$se))
#PV @60% infection
data_100$ppv60 <- (0.60*data_100$se)/(0.60*data_100$se+(1-0.60)*(1-data_100$sp))
data_100$npv60 <- ((1-0.60)*data_100$sp)/((1-0.60)*data_100$sp+0.60*(1-data_100$se))
#MCT @20% infection
data_100$mct20_3_1 <- (1-0.20)*(1-data_100$sp) + (3*0.20)*(1-data_100$se)
data_100$mct20_1_1 <- (1-0.20)*(1-data_100$sp) + (0.20)*(1-data_100$se)
data_100$mct20_1_3 <- (1-0.20)*(1-data_100$sp) + ((1/3)*0.20)*(1-data_100$se)
#MCT @40% infection
data_100$mct40_3_1 <- (1-0.40)*(1-data_100$sp) + (3*0.40)*(1-data_100$se)
data_100$mct40_1_1 <- (1-0.40)*(1-data_100$sp) + (0.40)*(1-data_100$se)
data_100$mct40_1_3 <- (1-0.40)*(1-data_100$sp) + ((1/3)*0.40)*(1-data_100$se)
#MCT @60% infection
data_100$mct60_3_1 <- (1-0.60)*(1-data_100$sp) + (3*0.60)*(1-data_100$se)
data_100$mct60_1_1 <- (1-0.60)*(1-data_100$sp) + (0.60)*(1-data_100$se)
data_100$mct60_1_3 <- (1-0.60)*(1-data_100$sp) + ((1/3)*0.60)*(1-data_100$se)


#dataframe
se <- quantile(data_100$se, c(0.025, 0.5, 0.975))
sp <- quantile(data_100$sp, c(0.025, 0.5, 0.975))
ppv20 <- quantile(data_100$ppv20, c(0.025, 0.5, 0.975))
npv20 <- quantile(data_100$npv20, c(0.025, 0.5, 0.975))
ppv40 <- quantile(data_100$ppv40, c(0.025, 0.5, 0.975))
npv40 <- quantile(data_100$npv40, c(0.025, 0.5, 0.975))
ppv60 <- quantile(data_100$ppv60, c(0.025, 0.5, 0.975))
npv60 <- quantile(data_100$npv60, c(0.025, 0.5, 0.975))
mct20_3_1 <- quantile(data_100$mct20_3_1, c(0.025, 0.5, 0.975))
mct20_1_1 <- quantile(data_100$mct20_1_1, c(0.025, 0.5, 0.975))
mct20_1_3 <- quantile(data_100$mct20_1_3, c(0.025, 0.5, 0.975))
mct40_3_1 <- quantile(data_100$mct40_3_1, c(0.025, 0.5, 0.975))
mct40_1_1 <- quantile(data_100$mct40_1_1, c(0.025, 0.5, 0.975))
mct40_1_3 <- quantile(data_100$mct40_1_3, c(0.025, 0.5, 0.975))
mct60_3_1 <- quantile(data_100$mct60_3_1, c(0.025, 0.5, 0.975))
mct60_1_1 <- quantile(data_100$mct60_1_1, c(0.025, 0.5, 0.975))
mct60_1_3 <- quantile(data_100$mct60_1_3, c(0.025, 0.5, 0.975))

df10 <- data.frame("TP only", se[2], se[1], se[3], 
                   sp[2], sp[1], sp[3],
                   
                   ppv20[2], ppv20[1], ppv20[3],
                   npv20[2], npv20[1], npv20[3],
                   ppv40[2], ppv40[1], ppv40[3],
                   npv40[2], npv40[1], npv40[3],
                   ppv60[2], ppv60[1], ppv60[3],
                   npv60[2], npv60[1], npv60[3],
                   
                   mct20_3_1[2], mct20_3_1[1], mct20_3_1[3],
                   mct20_1_1[2], mct20_1_1[1], mct20_1_1[3],
                   mct20_1_3[2], mct20_1_3[1], mct20_1_3[3],
                   
                   mct40_3_1[2], mct40_3_1[1], mct40_3_1[3],
                   mct40_1_1[2], mct40_1_1[1], mct40_1_1[3],
                   mct40_1_3[2], mct40_1_3[1], mct40_1_3[3],
                   
                   mct60_3_1[2], mct60_3_1[1], mct60_3_1[3],
                   mct60_1_1[2], mct60_1_1[1], mct60_1_1[3],
                   mct60_1_3[2], mct60_1_3[1], mct60_1_3[3]
)
rownames(df10) <- NULL
colnames(df10) <- c("Scenario", "se", "se_low", "se_hi", 
                    "sp", "sp_low", "sp_hi",
                    
                    "ppv20", "ppv20_low", "ppv20_hi",
                    "npv20", "npv20_low", "npv20_hi",
                    "ppv40", "ppv40_low", "ppv40_hi",
                    "npv40", "npv40_low", "npv40_hi",
                    "ppv60", "ppv60_low", "ppv60_hi",
                    "npv60", "npv60_low", "npv60_hi",
                    
                    "mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                    "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                    "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                    
                    "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                    "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                    "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                    
                    "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                    "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                    "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi"
)  

#combining data####
data_scenarios <- rbind.data.frame(df1, df2, df3, df4, df5, 
                                   df6, df7, df8, df9, df10)
print(data_scenarios)


##plot accuracy####
ACscenarios_endo <-data_scenarios %>%
  select("Scenario","se", "se_low", "se_hi", 
         "sp", "sp_low", "sp_hi") %>%
  pivot_longer(cols = c("se", "se_low", "se_hi", 
                        "sp", "sp_low", "sp_hi")
  ) %>%
  mutate(element = ifelse(grepl("se", name), "Se",
                          ifelse(grepl("sp", name), "Sp",NA)),
         names = ifelse(grepl("low",name),"lci",
                        ifelse(grepl("hi", name),"hci", "estimate"))) %>%
  pivot_wider(id_cols = c(Scenario,element,),values_from = value, names_from = names) %>%
  mutate(Scenario = factor(Scenario,
                           levels = c(
                             "Labo only","PVD only",
                             "PVD or esterase +", "ENDO only",
                             "Luminometry only (100 RLU)",
                             "Luminometry only (250 RLU)", 
                             "Luminometry (100 RLU) and esterase +",
                             "Luminometry (250 RLU) and esterase +",
                             "Petrifilm only","TP only"
                           ))) %>%
  ggplot(aes(x = Scenario))  +
  geom_point(aes(y = estimate,color = element),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lci, ymax = hci, 
                    color = element),
                position = position_dodge(width = 0.5),
                width = 0.1) +
  ylab("Accuracy") +
  theme_classic(base_family = "Times",base_size = 10) +
  #facet_wrap(~prev, nrow = 1)+
  xlab("\nScenarios")+
  ylim(c(0,1)) +
  theme(legend.position = "bottom",legend.title = element_blank(),
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

jpeg("ACscenarios_endo.jpeg", width = 30, 
     height = 15, 
     units = 'cm',res = 600)
ACscenarios_endo
dev.off()

#plot PV####

data_scenarios %>%
  select("Scenario","ppv20", "ppv20_low", "ppv20_hi",
         "npv20", "npv20_low", "npv20_hi",
         "ppv40", "ppv40_low", "ppv40_hi",
         "npv40", "npv40_low", "npv40_hi",
         "ppv60", "ppv60_low", "ppv60_hi",
         "npv60", "npv60_low", "npv60_hi") %>%
  pivot_longer(cols = c("ppv20", "ppv20_low", "ppv20_hi",
                        "npv20", "npv20_low", "npv20_hi",
                        "ppv40", "ppv40_low", "ppv40_hi",
                        "npv40", "npv40_low", "npv40_hi",
                        "ppv60", "ppv60_low", "ppv60_hi",
                        "npv60", "npv60_low", "npv60_hi")
  ) %>%
  mutate(element = ifelse(grepl("ppv", name), "PPV",
                          ifelse(grepl("npv", name), "NPV",NA)),
         prev = ifelse(grepl("20",name), "0.20", 
                       ifelse(grepl("40",name), "0.40",
                              ifelse(grepl("60",name), "0.60", NA))),
         names = ifelse(grepl("low",name),"lci",
                        ifelse(grepl("hi", name),"hci", "estimate"))) %>%
  pivot_wider(id_cols = c(Scenario,element,prev),values_from = value, names_from = names) %>%
  mutate(Scenario = factor(Scenario,
                           levels = c(
                             "Labo only","PVD only",
                             "PVD or esterase +","ENDO only",
                             "Luminometry only (100 RLU)",
                             "Luminometry only (250 RLU)", 
                             "Luminometry (100 RLU) and esterase +",
                             "Luminometry (250 RLU) and esterase +",
                             "Petrifilm only", "TP only"
                           ),
                           labels = c("LABO only",
                                      "PVD only",
                                      "PVD-ENDO in parallel",
                                      "ENDO only",
                                      "LUM100 only",
                                      "LUM250 only",
                                      "LUM100-ENDO in series",
                                      "LUM250-ENDO in series",
                                      "PETRI only", "TP only"))) %>%
  filter(prev=="0.60") %>%
  filter(element=="PPV")
print(n = 20)

data_scenarios %>%
  select("Scenario","mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
         "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
         "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
         
         "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
         "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
         "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
         
         "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
         "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
         "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi") %>%
  pivot_longer(cols = c("mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                        "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                        "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                        
                        "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                        "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                        "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                        
                        "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                        "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                        "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi")
  ) %>%
  mutate(ratio = factor(ifelse(grepl("3_1", name), "3:1",
                               ifelse(grepl("1_1", name), "1:1",
                                      ifelse(grepl("1_3", name), "1:3",NA))),
                        levels = c("1:3", "1:1", "3:1")),
         prev = ifelse(grepl("20",name), "0.20", 
                       ifelse(grepl("40",name), "0.40",
                              ifelse(grepl("60",name), "0.60", NA))),
         names = ifelse(grepl("low",name),"lci",
                        ifelse(grepl("hi", name),"hci", "estimate"))) %>%
  pivot_wider(id_cols = c(Scenario,ratio,prev),values_from = value, names_from = names) %>%
  mutate(Scenario = factor(Scenario,
                           levels = c(
                             "Labo only","PVD only",
                             "PVD or esterase +", "ENDO only",
                             "Luminometry only (100 RLU)",
                             "Luminometry only (250 RLU)", 
                             "Luminometry (100 RLU) and esterase +",
                             "Luminometry (250 RLU) and esterase +",
                             "Petrifilm only", "TP only"
                           ),
                           labels = c("LABO only",
                                      "PVD only",
                                      "PVD-ENDO in parallel",
                                      "ENDO only",
                                      "LUM100 only",
                                      "LUM250 only",
                                      "LUM100-ENDO in series",
                                      "LUM250-ENDO in series",
                                      "PETRI only", "TP only"))) %>%
  filter(prev=="0.40")%>%
  filter(ratio=="3:1")%>%
  print(n=30)

PVscenarios_endo <- data_scenarios %>%
  select("Scenario","ppv20", "ppv20_low", "ppv20_hi",
         "npv20", "npv20_low", "npv20_hi",
         "ppv40", "ppv40_low", "ppv40_hi",
         "npv40", "npv40_low", "npv40_hi",
         "ppv60", "ppv60_low", "ppv60_hi",
         "npv60", "npv60_low", "npv60_hi") %>%
  pivot_longer(cols = c("ppv20", "ppv20_low", "ppv20_hi",
                        "npv20", "npv20_low", "npv20_hi",
                        "ppv40", "ppv40_low", "ppv40_hi",
                        "npv40", "npv40_low", "npv40_hi",
                        "ppv60", "ppv60_low", "ppv60_hi",
                        "npv60", "npv60_low", "npv60_hi")
  ) %>%
  mutate(element = ifelse(grepl("ppv", name), "PPV",
                          ifelse(grepl("npv", name), "NPV",NA)),
         prev = ifelse(grepl("20",name), "0.20", 
                       ifelse(grepl("40",name), "0.40",
                              ifelse(grepl("60",name), "0.60", NA))),
         names = ifelse(grepl("low",name),"lci",
                        ifelse(grepl("hi", name),"hci", "estimate"))) %>%
  pivot_wider(id_cols = c(Scenario,element,prev),values_from = value, names_from = names) %>%
  mutate(Scenario = factor(Scenario,
                           levels = c(
                             "Labo only","PVD only",
                             "PVD or esterase +","ENDO only",
                             "Luminometry only (100 RLU)",
                             "Luminometry only (250 RLU)", 
                             "Luminometry (100 RLU) and esterase +",
                             "Luminometry (250 RLU) and esterase +",
                             "Petrifilm only", "TP only"
                           ),
                           labels = c("LABO only",
                                      "PVD only",
                                      "PVD-ENDO in parallel",
                                      "ENDO only",
                                      "LUM100 only",
                                      "LUM250 only",
                                      "LUM100-ENDO in series",
                                      "LUM250-ENDO in series",
                                      "PETRI only", "TP only"))) %>%
  ggplot(aes(x = Scenario))  +
  geom_point(aes(y = estimate,color = element),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lci, ymax = hci, 
                    color = element),
                position = position_dodge(width = 0.5),
                width = 0.1) +
  ylab("Predictive values") +
  theme_bw(base_family = "Times",base_size = 10) +
  facet_wrap(~prev, nrow = 1)+
  xlab("\nScenarios")+
  ylim(c(0,1)) +
  theme(legend.position = "bottom",legend.title = element_blank(),
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

jpeg("PVscenarios_endo.jpeg", width = 19, 
     height = 14, 
     units = 'cm',res = 600)
PVscenarios_endo
dev.off()

table(data_scenarios$Scenario)
PVscenarios_endo_KT <- data_scenarios %>%
  filter(Scenario=="PVD only" | Scenario=="Labo only" | Scenario=="PVD or esterase +" | Scenario=="Luminometry only (250 RLU)") %>%
  select("Scenario","ppv20", "ppv20_low", "ppv20_hi",
         "npv20", "npv20_low", "npv20_hi",
         "ppv40", "ppv40_low", "ppv40_hi",
         "npv40", "npv40_low", "npv40_hi",
         "ppv60", "ppv60_low", "ppv60_hi",
         "npv60", "npv60_low", "npv60_hi") %>%
  pivot_longer(cols = c("ppv20", "ppv20_low", "ppv20_hi",
                        "npv20", "npv20_low", "npv20_hi",
                        "ppv40", "ppv40_low", "ppv40_hi",
                        "npv40", "npv40_low", "npv40_hi",
                        "ppv60", "ppv60_low", "ppv60_hi",
                        "npv60", "npv60_low", "npv60_hi")
  ) %>%
  mutate(element = ifelse(grepl("ppv", name), "VPP",
                          ifelse(grepl("npv", name), "VPN",NA)),
         prev = ifelse(grepl("20",name), "0.20", 
                       ifelse(grepl("40",name), "0.40",
                              ifelse(grepl("60",name), "0.60", NA))),
         names = ifelse(grepl("low",name),"lci",
                        ifelse(grepl("hi", name),"hci", "estimate"))) %>%
  pivot_wider(id_cols = c(Scenario,element,prev),values_from = value, names_from = names) %>%
  mutate(threshold = factor(Scenario,
                            levels = c(
                              "Labo only","PVD only",
                              "PVD or esterase +",
                              "Luminometry only (250 RLU)"
                            ),
                            labels = c("Lab",
                                       "EVP",
                                       "E&E",
                                       "Lum")),
         Scenario = factor(Scenario,
                           levels = c(
                             "Labo only","PVD only",
                             "PVD or esterase +",
                             "Luminometry only (250 RLU)"
                           ),
                           labels = c("Laboratoire",
                                      "EVP",
                                      "EVP & endo",
                                      "Luminomètre"))) %>%
  ggplot(aes(x = threshold))  +
  geom_point(aes(y = estimate,color = element,
                 shape = Scenario),
             position = position_dodge(width = 0.5),
             size = 3) +
  geom_errorbar(aes(ymin = lci, ymax = hci, 
                    color = element),
                position = position_dodge(width = 0.5),
                width = 0.1, 
                linewidth = 1.5) +
  ylab("Valeurs prédictives") +
  theme_bw(base_family = "Arial",base_size = 20) +
  facet_wrap(~prev, nrow = 1)+
  xlab("\nScénarios")+
  ylim(c(0,1)) +
  theme(legend.position = "right",legend.title = element_blank(),
        legend.direction = "vertical")+#,
  # axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1))


jpeg("endoFigPV_TC.jpeg", width = 11, 
     height = 4.36, 
     units = 'in',res = 600)
PVscenarios_endo_KT
dev.off()

data_scenarios %>%
  select("Scenario","ppv20", "ppv20_low", "ppv20_hi",
         "npv20", "npv20_low", "npv20_hi",
         "ppv40", "ppv40_low", "ppv40_hi",
         "npv40", "npv40_low", "npv40_hi",
         "ppv60", "ppv60_low", "ppv60_hi",
         "npv60", "npv60_low", "npv60_hi") %>%
  pivot_longer(cols = c("ppv20", "ppv20_low", "ppv20_hi",
                        "npv20", "npv20_low", "npv20_hi",
                        "ppv40", "ppv40_low", "ppv40_hi",
                        "npv40", "npv40_low", "npv40_hi",
                        "ppv60", "ppv60_low", "ppv60_hi",
                        "npv60", "npv60_low", "npv60_hi")
  ) %>%
  mutate(element = ifelse(grepl("ppv", name), "PPV",
                          ifelse(grepl("npv", name), "NPV",NA)),
         prev = ifelse(grepl("20",name), "0.20", 
                       ifelse(grepl("40",name), "0.40",
                              ifelse(grepl("60",name), "0.60", NA))),
         names = ifelse(grepl("low",name),"lci",
                        ifelse(grepl("hi", name),"hci", "estimate"))) %>%
  pivot_wider(id_cols = c(Scenario,element,prev),values_from = value, names_from = names)%>%
  filter(element=="PPV" & prev=="0.40") %>%
  arrange(-estimate)

#plot MCT####
MCTscenarios_endo <- data_scenarios %>%
  select("Scenario","mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
         "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
         "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
         
         "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
         "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
         "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
         
         "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
         "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
         "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi") %>%
  pivot_longer(cols = c("mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                        "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                        "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                        
                        "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                        "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                        "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                        
                        "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                        "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                        "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi")
  ) %>%
  mutate(ratio = factor(ifelse(grepl("3_1", name), "3:1",
                               ifelse(grepl("1_1", name), "1:1",
                                      ifelse(grepl("1_3", name), "1:3",NA))),
                        levels = c("1:3", "1:1", "3:1")),
         prev = ifelse(grepl("20",name), "0.20", 
                       ifelse(grepl("40",name), "0.40",
                              ifelse(grepl("60",name), "0.60", NA))),
         names = ifelse(grepl("low",name),"lci",
                        ifelse(grepl("hi", name),"hci", "estimate"))) %>%
  pivot_wider(id_cols = c(Scenario,ratio,prev),values_from = value, names_from = names) %>%
  mutate(Scenario = factor(Scenario,
                           levels = c(
                             "Labo only","PVD only",
                             "PVD or esterase +", "ENDO only",
                             "Luminometry only (100 RLU)",
                             "Luminometry only (250 RLU)", 
                             "Luminometry (100 RLU) and esterase +",
                             "Luminometry (250 RLU) and esterase +",
                             "Petrifilm only", "TP only"
                           ),
                           labels = c("LABO only",
                                      "PVD only",
                                      "PVD-ENDO in parallel",
                                      "ENDO only",
                                      "LUM100 only",
                                      "LUM250 only",
                                      "LUM100-ENDO in series",
                                      "LUM250-ENDO in series",
                                      "PETRI only", "TP only"))) %>%
  ggplot(aes(x = Scenario))  +
  geom_point(aes(y = estimate,color = ratio),
             position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lci, ymax = hci, 
                    color = ratio),
                position = position_dodge(width = 0.5),
                width = 0.1) +
  ylab("MCT") +
  theme_bw(base_family = "Times",base_size = 10) +
  facet_wrap(~prev, nrow = 1)+
  xlab("\nScenarios")+
  #ylim(c(0,1)) +
  theme(legend.position = "bottom",legend.title = element_text("Ratio"),
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

jpeg("MCTscenarios_endo.jpeg", width = 19, 
     height = 14, 
     units = 'cm',res = 600)
MCTscenarios_endo
dev.off()



MCTscenarios_endo_KT <- data_scenarios %>%
  filter(Scenario=="PVD only" | Scenario=="Labo only" | Scenario=="PVD or esterase +" | Scenario=="Luminometry only (250 RLU)") %>%
  select("Scenario","mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
         "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
         "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
         
         "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
         "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
         "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
         
         "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
         "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
         "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi") %>%
  pivot_longer(cols = c("mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                        "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                        "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                        
                        "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                        "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                        "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                        
                        "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                        "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                        "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi")
  ) %>%
  mutate(ratio = factor(ifelse(grepl("3_1", name), "3:1",
                               ifelse(grepl("1_1", name), "1:1",
                                      ifelse(grepl("1_3", name), "1:3",NA))),
                        levels = c("1:3", "1:1", "3:1")),
         prev = ifelse(grepl("20",name), "0.20", 
                       ifelse(grepl("40",name), "0.40",
                              ifelse(grepl("60",name), "0.60", NA))),
         names = ifelse(grepl("low",name),"lci",
                        ifelse(grepl("hi", name),"hci", "estimate"))) %>%
  pivot_wider(id_cols = c(Scenario,ratio,prev),values_from = value, names_from = names) %>%
  mutate(threshold = factor(Scenario,
                            levels = c(
                              "Labo only","PVD only",
                              "PVD or esterase +",
                              "Luminometry only (250 RLU)"
                            ),
                            labels = c("Lab",
                                       "EVP",
                                       "E&E",
                                       "Lum")),
         Scenario = factor(Scenario,
                           levels = c(
                             "Labo only","PVD only",
                             "PVD or esterase +",
                             "Luminometry only (250 RLU)"
                           ),
                           labels = c("Laboratoire",
                                      "EVP",
                                      "EVP & endo",
                                      "Luminomètre"))) %>%
  ggplot(aes(x = threshold))  +
  geom_point(aes(y = estimate,color = ratio,
                 shape = Scenario),
             position = position_dodge(width = 0.5), 
             size = 3) +
  geom_errorbar(aes(ymin = lci, ymax = hci, 
                    color = ratio),
                position = position_dodge(width = 0.5),
                width = 0.1, 
                linewidth =1.5) +
  ylab("Coût de mauvaise\nclassification") +
  scale_shape_discrete(name = "")+
  scale_color_discrete(name = "FN:FP")+
  theme_bw(base_family = "Arial",base_size = 20) +
  facet_wrap(~prev, nrow = 1)+
  xlab("\nScénarios")+
  #ylim(c(0,1)) +
  theme(legend.position = "right",
        legend.direction = "vertical") +#,
  # axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1))


jpeg("endoFigMCT_TC.jpeg", width = 11, 
     height = 4.36, 
     units = 'in',res = 600)
MCTscenarios_endo_KT
dev.off()



data_scenarios %>%
  select("Scenario","mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
         "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
         "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
         
         "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
         "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
         "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
         
         "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
         "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
         "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi") %>%
  pivot_longer(cols = c("mct20_3_1", "mct20_3_1_low", "mct20_3_1_hi",
                        "mct20_1_1", "mct20_1_1_low", "mct20_1_1_hi",
                        "mct20_1_3", "mct20_1_3_low", "mct20_1_3_hi",
                        
                        "mct40_3_1", "mct40_3_1_low", "mct40_3_1_hi",
                        "mct40_1_1", "mct40_1_1_low", "mct40_1_1_hi",
                        "mct40_1_3", "mct40_1_3_low", "mct40_1_3_hi",
                        
                        "mct60_3_1", "mct60_3_1_low", "mct60_3_1_hi",
                        "mct60_1_1", "mct60_1_1_low", "mct60_1_1_hi",
                        "mct60_1_3", "mct60_1_3_low", "mct60_1_3_hi")
  ) %>%
  mutate(ratio = factor(ifelse(grepl("3_1", name), "3:1",
                               ifelse(grepl("1_1", name), "1:1",
                                      ifelse(grepl("1_3", name), "1:3",NA))),
                        levels = c("1:3", "1:1", "3:1")),
         prev = ifelse(grepl("20",name), "0.20", 
                       ifelse(grepl("40",name), "0.40",
                              ifelse(grepl("60",name), "0.60", NA))),
         names = ifelse(grepl("low",name),"lci",
                        ifelse(grepl("hi", name),"hci", "estimate"))) %>%
  pivot_wider(id_cols = c(Scenario,ratio,prev),values_from = value, names_from = names) %>%
  filter(ratio=="1:3") %>%
  filter(prev=="0.60") %>%
  arrange(estimate) 

#

################################################################################
#Sensitivity analyses - independence####
################################################################################
par1=c("se","sp")

out_p20l100_7pop <- jags(model ="TEMPmodel_ntests_indep_7pop.bug", 
                         parameters.to.save=par1, 
                         data=dat_p20l100, n.chains=3,
                         n.iter=50000, n.burnin=10000, n.thin = 2,
                         #inits = inits,
                         DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l100_7pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l100####
bug_p20l100_i <- print(out_p20l100_7pop, digits.summary=2) 
bug_p20l100_i <- bug_p20l100_i$summary %>%
  data.frame(element = row.names(bug_p20l100_i$summary)) %>%
  mutate(cp_p = 20, 
         cp_l = 100) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)

##model - independence####

out_p20l250_7pop <- jags(model ="TEMPmodel_ntests_indep_7pop.bug", 
                         parameters.to.save=par1, 
                         data=dat_p20l250, n.chains=3,
                         n.iter=50000, n.burnin=10000, n.thin = 2,
                         #inits = inits,
                         DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l250_7pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l250####
bug_p20l250_i <- print(out_p20l250_7pop, digits.summary=2) 
bug_p20l250_i <- bug_p20l250_i$summary %>%
  data.frame(element = row.names(bug_p20l250_i$summary)) %>%
  mutate(cp_p = 20, 
         cp_l = 250) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)


################################################################################
#Sensitivity analyses - free/tiestall####
################################################################################
##diagnostic####
bug.mcmc <- as.mcmc(out_p20l100_2pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l100####
bug_p20l100_i2 <- print(out_p20l100_2pop, digits.summary=2) 
bug_p20l100_i2 <- bug_p20l100_i2$summary %>%
  data.frame(element = row.names(bug_p20l100_i2$summary)) %>%
  mutate(cp_p = 20, 
         cp_l = 100) %>%
  select(X50.,X2.5.,X97.5.,element,cp_p,cp_l)


################################################################################
#Sensitivity analyses - remove one herd at the time####
################################################################################
#no 7####
#n is the sample size,
n1 <- nrow(farm1)
n2 <- nrow(farm2)
n3 <- nrow(farm3)
n4 <- nrow(farm4)
n5 <- nrow(farm5)
n6 <- nrow(farm6)


#K is the number of tests,
K <- 6

y1_p20l100 <- farm1 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y2_p20l100 <- farm2 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y3_p20l100 <- farm3 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y4_p20l100 <- farm4 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y5_p20l100 <- farm5 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y6_p20l100 <- farm6 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)


m1_p20l100 <- as.matrix(y1_p20l100)
m2_p20l100 <- as.matrix(y2_p20l100)
m3_p20l100 <- as.matrix(y3_p20l100)
m4_p20l100 <- as.matrix(y4_p20l100)
m5_p20l100 <- as.matrix(y5_p20l100)
m6_p20l100 <- as.matrix(y6_p20l100)

x1_p20l100<- structure(m1_p20l100, .Dim=c(n1,K))
x2_p20l100<- structure(m2_p20l100, .Dim=c(n2,K))
x3_p20l100<- structure(m3_p20l100, .Dim=c(n3,K))
x4_p20l100<- structure(m4_p20l100, .Dim=c(n4,K))
x5_p20l100<- structure(m5_p20l100, .Dim=c(n5,K))
x6_p20l100<- structure(m6_p20l100, .Dim=c(n6,K))

z1<-rep(0,n1)
z2<-rep(0,n2)
z3<-rep(0,n3)
z4<-rep(0,n4)
z5<-rep(0,n5)
z6<-rep(0,n6)

dat_p20l100=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, 
                 x1=x1_p20l100,
                 x2=x2_p20l100,
                 x3=x3_p20l100,
                 x4=x4_p20l100,
                 x5=x5_p20l100,
                 x6=x6_p20l100,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6) #prepare the data list

par1=c("se","sp")

out_p20l100_6pop <- jags(model ="TEMPmodel_ntests_indep_6pop.bug", 
                         parameters.to.save=par1, 
                         data=dat_p20l100, n.chains=3,
                         n.iter=50000, n.burnin=10000, n.thin = 2,
                         #inits = inits,
                         DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l100_6pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l100####
bug_p20l100_i6no7 <- print(out_p20l100_6pop, digits.summary=2) 
bug_p20l100_i6no7 <- bug_p20l100_i6no7$summary %>%
  data.frame(element = row.names(bug_p20l100_i6no7$summary)) %>%
  select(X50.,X2.5.,X97.5.,element)

#no 6####
#n is the sample size,
n1 <- nrow(farm1)
n2 <- nrow(farm2)
n3 <- nrow(farm3)
n4 <- nrow(farm4)
n5 <- nrow(farm5)
n6 <- nrow(farm7)


#K is the number of tests,
K <- 6

y1_p20l100 <- farm1 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y2_p20l100 <- farm2 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y3_p20l100 <- farm3 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y4_p20l100 <- farm4 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y5_p20l100 <- farm5 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y6_p20l100 <- farm7 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)


m1_p20l100 <- as.matrix(y1_p20l100)
m2_p20l100 <- as.matrix(y2_p20l100)
m3_p20l100 <- as.matrix(y3_p20l100)
m4_p20l100 <- as.matrix(y4_p20l100)
m5_p20l100 <- as.matrix(y5_p20l100)
m6_p20l100 <- as.matrix(y6_p20l100)

x1_p20l100<- structure(m1_p20l100, .Dim=c(n1,K))
x2_p20l100<- structure(m2_p20l100, .Dim=c(n2,K))
x3_p20l100<- structure(m3_p20l100, .Dim=c(n3,K))
x4_p20l100<- structure(m4_p20l100, .Dim=c(n4,K))
x5_p20l100<- structure(m5_p20l100, .Dim=c(n5,K))
x6_p20l100<- structure(m6_p20l100, .Dim=c(n6,K))

z1<-rep(0,n1)
z2<-rep(0,n2)
z3<-rep(0,n3)
z4<-rep(0,n4)
z5<-rep(0,n5)
z6<-rep(0,n6)

dat_p20l100=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, 
                 x1=x1_p20l100,
                 x2=x2_p20l100,
                 x3=x3_p20l100,
                 x4=x4_p20l100,
                 x5=x5_p20l100,
                 x6=x6_p20l100,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6) #prepare the data list

par1=c("se","sp")

out_p20l100_6pop <- jags(model ="TEMPmodel_ntests_indep_6pop.bug", 
                         parameters.to.save=par1, 
                         data=dat_p20l100, n.chains=3,
                         n.iter=50000, n.burnin=10000, n.thin = 2,
                         #inits = inits,
                         DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l100_6pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l100####
bug_p20l100_i6no6 <- print(out_p20l100_6pop, digits.summary=2) 
bug_p20l100_i6no6 <- bug_p20l100_i6no6$summary %>%
  data.frame(element = row.names(bug_p20l100_i6no6$summary)) %>%
  select(X50.,X2.5.,X97.5.,element)

#no 5####
#n is the sample size,
n1 <- nrow(farm1)
n2 <- nrow(farm2)
n3 <- nrow(farm3)
n4 <- nrow(farm4)
n5 <- nrow(farm7)
n6 <- nrow(farm6)


#K is the number of tests,
K <- 6

y1_p20l100 <- farm1 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y2_p20l100 <- farm2 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y3_p20l100 <- farm3 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y4_p20l100 <- farm4 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y5_p20l100 <- farm7 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y6_p20l100 <- farm6 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)


m1_p20l100 <- as.matrix(y1_p20l100)
m2_p20l100 <- as.matrix(y2_p20l100)
m3_p20l100 <- as.matrix(y3_p20l100)
m4_p20l100 <- as.matrix(y4_p20l100)
m5_p20l100 <- as.matrix(y5_p20l100)
m6_p20l100 <- as.matrix(y6_p20l100)

x1_p20l100<- structure(m1_p20l100, .Dim=c(n1,K))
x2_p20l100<- structure(m2_p20l100, .Dim=c(n2,K))
x3_p20l100<- structure(m3_p20l100, .Dim=c(n3,K))
x4_p20l100<- structure(m4_p20l100, .Dim=c(n4,K))
x5_p20l100<- structure(m5_p20l100, .Dim=c(n5,K))
x6_p20l100<- structure(m6_p20l100, .Dim=c(n6,K))

z1<-rep(0,n1)
z2<-rep(0,n2)
z3<-rep(0,n3)
z4<-rep(0,n4)
z5<-rep(0,n5)
z6<-rep(0,n6)

dat_p20l100=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, 
                 x1=x1_p20l100,
                 x2=x2_p20l100,
                 x3=x3_p20l100,
                 x4=x4_p20l100,
                 x5=x5_p20l100,
                 x6=x6_p20l100,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6) #prepare the data list

par1=c("se","sp")

out_p20l100_6pop <- jags(model ="TEMPmodel_ntests_indep_6pop.bug", 
                         parameters.to.save=par1, 
                         data=dat_p20l100, n.chains=3,
                         n.iter=50000, n.burnin=10000, n.thin = 2,
                         #inits = inits,
                         DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l100_6pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l100####
bug_p20l100_i6no5 <- print(out_p20l100_6pop, digits.summary=2) 
bug_p20l100_i6no5 <- bug_p20l100_i6no5$summary %>%
  data.frame(element = row.names(bug_p20l100_i6no5$summary)) %>%
  select(X50.,X2.5.,X97.5.,element)

#no 4####
#n is the sample size,
n1 <- nrow(farm1)
n2 <- nrow(farm2)
n3 <- nrow(farm3)
n4 <- nrow(farm7)
n5 <- nrow(farm5)
n6 <- nrow(farm6)


#K is the number of tests,
K <- 6

y1_p20l100 <- farm1 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y2_p20l100 <- farm2 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y3_p20l100 <- farm3 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y4_p20l100 <- farm7 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y5_p20l100 <- farm5 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y6_p20l100 <- farm6 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)


m1_p20l100 <- as.matrix(y1_p20l100)
m2_p20l100 <- as.matrix(y2_p20l100)
m3_p20l100 <- as.matrix(y3_p20l100)
m4_p20l100 <- as.matrix(y4_p20l100)
m5_p20l100 <- as.matrix(y5_p20l100)
m6_p20l100 <- as.matrix(y6_p20l100)

x1_p20l100<- structure(m1_p20l100, .Dim=c(n1,K))
x2_p20l100<- structure(m2_p20l100, .Dim=c(n2,K))
x3_p20l100<- structure(m3_p20l100, .Dim=c(n3,K))
x4_p20l100<- structure(m4_p20l100, .Dim=c(n4,K))
x5_p20l100<- structure(m5_p20l100, .Dim=c(n5,K))
x6_p20l100<- structure(m6_p20l100, .Dim=c(n6,K))

z1<-rep(0,n1)
z2<-rep(0,n2)
z3<-rep(0,n3)
z4<-rep(0,n4)
z5<-rep(0,n5)
z6<-rep(0,n6)

dat_p20l100=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, 
                 x1=x1_p20l100,
                 x2=x2_p20l100,
                 x3=x3_p20l100,
                 x4=x4_p20l100,
                 x5=x5_p20l100,
                 x6=x6_p20l100,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6) #prepare the data list

par1=c("se","sp")

out_p20l100_6pop <- jags(model ="TEMPmodel_ntests_indep_6pop.bug", 
                         parameters.to.save=par1, 
                         data=dat_p20l100, n.chains=3,
                         n.iter=50000, n.burnin=10000, n.thin = 2,
                         #inits = inits,
                         DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l100_6pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l100####
bug_p20l100_i6no4 <- print(out_p20l100_6pop, digits.summary=2) 
bug_p20l100_i6no4 <- bug_p20l100_i6no4$summary %>%
  data.frame(element = row.names(bug_p20l100_i6no4$summary)) %>%
  select(X50.,X2.5.,X97.5.,element)

#no 3####
#n is the sample size,
n1 <- nrow(farm1)
n2 <- nrow(farm2)
n3 <- nrow(farm7)
n4 <- nrow(farm4)
n5 <- nrow(farm5)
n6 <- nrow(farm6)


#K is the number of tests,
K <- 6

y1_p20l100 <- farm1 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y2_p20l100 <- farm2 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y3_p20l100 <- farm7 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y4_p20l100 <- farm4 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y5_p20l100 <- farm5 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y6_p20l100 <- farm6 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)


m1_p20l100 <- as.matrix(y1_p20l100)
m2_p20l100 <- as.matrix(y2_p20l100)
m3_p20l100 <- as.matrix(y3_p20l100)
m4_p20l100 <- as.matrix(y4_p20l100)
m5_p20l100 <- as.matrix(y5_p20l100)
m6_p20l100 <- as.matrix(y6_p20l100)

x1_p20l100<- structure(m1_p20l100, .Dim=c(n1,K))
x2_p20l100<- structure(m2_p20l100, .Dim=c(n2,K))
x3_p20l100<- structure(m3_p20l100, .Dim=c(n3,K))
x4_p20l100<- structure(m4_p20l100, .Dim=c(n4,K))
x5_p20l100<- structure(m5_p20l100, .Dim=c(n5,K))
x6_p20l100<- structure(m6_p20l100, .Dim=c(n6,K))

z1<-rep(0,n1)
z2<-rep(0,n2)
z3<-rep(0,n3)
z4<-rep(0,n4)
z5<-rep(0,n5)
z6<-rep(0,n6)

dat_p20l100=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, 
                 x1=x1_p20l100,
                 x2=x2_p20l100,
                 x3=x3_p20l100,
                 x4=x4_p20l100,
                 x5=x5_p20l100,
                 x6=x6_p20l100,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6) #prepare the data list

par1=c("se","sp")

out_p20l100_6pop <- jags(model ="TEMPmodel_ntests_indep_6pop.bug", 
                         parameters.to.save=par1, 
                         data=dat_p20l100, n.chains=3,
                         n.iter=50000, n.burnin=10000, n.thin = 2,
                         #inits = inits,
                         DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l100_6pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l100####
bug_p20l100_i6no3 <- print(out_p20l100_6pop, digits.summary=2) 
bug_p20l100_i6no3 <- bug_p20l100_i6no3$summary %>%
  data.frame(element = row.names(bug_p20l100_i6no3$summary)) %>%
  select(X50.,X2.5.,X97.5.,element)

#no 2####
#n is the sample size,
n1 <- nrow(farm1)
n2 <- nrow(farm7)
n3 <- nrow(farm3)
n4 <- nrow(farm4)
n5 <- nrow(farm5)
n6 <- nrow(farm6)


#K is the number of tests,
K <- 6

y1_p20l100 <- farm1 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y2_p20l100 <- farm7 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y3_p20l100 <- farm3 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y4_p20l100 <- farm4 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y5_p20l100 <- farm5 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y6_p20l100 <- farm6 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)


m1_p20l100 <- as.matrix(y1_p20l100)
m2_p20l100 <- as.matrix(y2_p20l100)
m3_p20l100 <- as.matrix(y3_p20l100)
m4_p20l100 <- as.matrix(y4_p20l100)
m5_p20l100 <- as.matrix(y5_p20l100)
m6_p20l100 <- as.matrix(y6_p20l100)

x1_p20l100<- structure(m1_p20l100, .Dim=c(n1,K))
x2_p20l100<- structure(m2_p20l100, .Dim=c(n2,K))
x3_p20l100<- structure(m3_p20l100, .Dim=c(n3,K))
x4_p20l100<- structure(m4_p20l100, .Dim=c(n4,K))
x5_p20l100<- structure(m5_p20l100, .Dim=c(n5,K))
x6_p20l100<- structure(m6_p20l100, .Dim=c(n6,K))

z1<-rep(0,n1)
z2<-rep(0,n2)
z3<-rep(0,n3)
z4<-rep(0,n4)
z5<-rep(0,n5)
z6<-rep(0,n6)

dat_p20l100=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, 
                 x1=x1_p20l100,
                 x2=x2_p20l100,
                 x3=x3_p20l100,
                 x4=x4_p20l100,
                 x5=x5_p20l100,
                 x6=x6_p20l100,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6) #prepare the data list

par1=c("se","sp")

out_p20l100_6pop <- jags(model ="TEMPmodel_ntests_indep_6pop.bug", 
                         parameters.to.save=par1, 
                         data=dat_p20l100, n.chains=3,
                         n.iter=50000, n.burnin=10000, n.thin = 2,
                         #inits = inits,
                         DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l100_6pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l100####
bug_p20l100_i6no2 <- print(out_p20l100_6pop, digits.summary=2) 
bug_p20l100_i6no2 <- bug_p20l100_i6no2$summary %>%
  data.frame(element = row.names(bug_p20l100_i6no2$summary)) %>%
  select(X50.,X2.5.,X97.5.,element)

#no 1####
#n is the sample size,
n1 <- nrow(farm7)
n2 <- nrow(farm2)
n3 <- nrow(farm3)
n4 <- nrow(farm4)
n5 <- nrow(farm5)
n6 <- nrow(farm6)


#K is the number of tests,
K <- 6

y1_p20l100 <- farm7 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y2_p20l100 <- farm2 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y3_p20l100 <- farm3 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y4_p20l100 <- farm4 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y5_p20l100 <- farm5 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y6_p20l100 <- farm6 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)


m1_p20l100 <- as.matrix(y1_p20l100)
m2_p20l100 <- as.matrix(y2_p20l100)
m3_p20l100 <- as.matrix(y3_p20l100)
m4_p20l100 <- as.matrix(y4_p20l100)
m5_p20l100 <- as.matrix(y5_p20l100)
m6_p20l100 <- as.matrix(y6_p20l100)

x1_p20l100<- structure(m1_p20l100, .Dim=c(n1,K))
x2_p20l100<- structure(m2_p20l100, .Dim=c(n2,K))
x3_p20l100<- structure(m3_p20l100, .Dim=c(n3,K))
x4_p20l100<- structure(m4_p20l100, .Dim=c(n4,K))
x5_p20l100<- structure(m5_p20l100, .Dim=c(n5,K))
x6_p20l100<- structure(m6_p20l100, .Dim=c(n6,K))

z1<-rep(0,n1)
z2<-rep(0,n2)
z3<-rep(0,n3)
z4<-rep(0,n4)
z5<-rep(0,n5)
z6<-rep(0,n6)

dat_p20l100=list(K=K,
                 n1=n1, n2=n2, n3=n3, n4=n4, n5=n5, n6=n6, 
                 x1=x1_p20l100,
                 x2=x2_p20l100,
                 x3=x3_p20l100,
                 x4=x4_p20l100,
                 x5=x5_p20l100,
                 x6=x6_p20l100,
                 z1=z1, z2=z2, z3=z3, z4=z4, z5=z5, z6=z6) #prepare the data list

par1=c("se","sp")

out_p20l100_6pop <- jags(model ="TEMPmodel_ntests_indep_6pop.bug", 
                         parameters.to.save=par1, 
                         data=dat_p20l100, n.chains=3,
                         n.iter=50000, n.burnin=10000, n.thin = 2,
                         #inits = inits,
                         DIC = TRUE)

##diagnostic####
bug.mcmc <- as.mcmc(out_p20l100_6pop)          
mcmcplot(bug.mcmc, title="Diagnostic plots") 
effectiveSize(bug.mcmc)

##output - p20l100####
bug_p20l100_i6no1 <- print(out_p20l100_6pop, digits.summary=2) 
bug_p20l100_i6no1 <- bug_p20l100_i6no1$summary %>%
  data.frame(element = row.names(bug_p20l100_i6no1$summary)) %>%
  select(X50.,X2.5.,X97.5.,element)


################################################################################
# Independence // 7 populations ####
################################################################################

model_ntests_indep_7pop = "
model{
for(i in 1:n1){
for (k in 1:K){
s1_1[i,k]<- se[k]^x1[i,k]*((1-se[k])^(1-x1[i,k]))
s2_1[i,k]<- sp[k]^(1-x1[i,k])*((1-sp[k])^x1[i,k])
}
prob1[i]=pi1*(prod(s1_1[i,1:K])) + (1-pi1)*(prod(s2_1[i, 1:K]))
z1[i] ~ dpois( - log(prob1[i]))
}
for(i in 1:n2){
for (k in 1:K){
s1_2[i,k]<- se[k]^x2[i,k]*((1-se[k])^(1-x2[i,k]))
s2_2[i,k]<- sp[k]^(1-x2[i,k])*((1-sp[k])^x2[i,k])
}
prob2[i]=pi2*(prod(s1_2[i,1:K])) + (1-pi2)*(prod(s2_2[i, 1:K]))
z2[i] ~ dpois( - log(prob2[i]))
}
for(i in 1:n3){
for (k in 1:K){
s1_3[i,k]<- se[k]^x3[i,k]*((1-se[k])^(1-x3[i,k]))
s2_3[i,k]<- sp[k]^(1-x3[i,k])*((1-sp[k])^x3[i,k])
}
prob3[i]=pi3*(prod(s1_3[i,1:K])) + (1-pi3)*(prod(s2_3[i, 1:K]))
z3[i] ~ dpois( - log(prob3[i]))
}
for(i in 1:n4){
for (k in 1:K){
s1_4[i,k]<- se[k]^x4[i,k]*((1-se[k])^(1-x4[i,k]))
s2_4[i,k]<- sp[k]^(1-x4[i,k])*((1-sp[k])^x4[i,k])
}
prob4[i]=pi4*(prod(s1_4[i,1:K])) + (1-pi4)*(prod(s2_4[i, 1:K]))
z4[i] ~ dpois( - log(prob4[i]))
}
for(i in 1:n5){
for (k in 1:K){
s1_5[i,k]<- se[k]^x5[i,k]*((1-se[k])^(1-x5[i,k]))
s2_5[i,k]<- sp[k]^(1-x5[i,k])*((1-sp[k])^x5[i,k])
}
prob5[i]=pi5*(prod(s1_5[i,1:K])) + (1-pi5)*(prod(s2_5[i, 1:K]))
z5[i] ~ dpois( - log(prob5[i]))
}
for(i in 1:n6){
for (k in 1:K){
s1_6[i,k]<- se[k]^x6[i,k]*((1-se[k])^(1-x6[i,k]))
s2_6[i,k]<- sp[k]^(1-x6[i,k])*((1-sp[k])^x6[i,k])
}
prob6[i]=pi6*(prod(s1_6[i,1:K])) + (1-pi6)*(prod(s2_6[i, 1:K]))
z6[i] ~ dpois( - log(prob6[i]))
}
for(i in 1:n7){
for (k in 1:K){
s1_7[i,k]<- se[k]^x7[i,k]*((1-se[k])^(1-x7[i,k]))
s2_7[i,k]<- sp[k]^(1-x7[i,k])*((1-sp[k])^x7[i,k])
}
prob7[i]=pi7*(prod(s1_7[i,1:K])) + (1-pi7)*(prod(s2_7[i, 1:K]))
z7[i] ~ dpois( - log(prob7[i]))
}
for (k in 1:K){
se[k] ~ dbeta( omega1*(kappa1 -2)+1, (1-omega1)*(kappa1-2) +1)
sp[k] ~ dbeta( omega2*(kappa2 -2)+1, (1-omega2)*(kappa2-2) +1)
}
omega1 ~ dbeta(1,1)T(0.5,)
omega2 ~ dbeta(1,1)T(0.5,)
kappa1 = kappaMinusTwo1 +2
kappaMinusTwo1~ dgamma(0.01,0.01)
kappa2 = kappaMinusTwo2+2
kappaMinusTwo2 ~ dgamma(0.01,0.01)
pi1 ~ dbeta(1.4,1.4)
pi2 ~ dbeta(1.4,1.4)
pi3 ~ dbeta(1.4,1.4)
pi4 ~ dbeta(1.4,1.4)
pi5 ~ dbeta(1.4,1.4)
pi6 ~ dbeta(1.4,1.4)
pi7 ~ dbeta(1.4,1.4)
}"
writeLines( model_ntests_indep_7pop , con="TEMPmodel_ntests_indep_7pop.bug" )

################################################################################
##depB: dependence among bacteriological tests only - 7 populations####
################################################################################
model_ntests_dep_7pop = "
model{
for(i in 1:n1){
for (k in 1:K){
s1_1[i,k]<- se[k]^x1[i,k]*((1-se[k])^(1-x1[i,k]))
s2_1[i,k]<- sp[k]^(1-x1[i,k])*((1-sp[k])^x1[i,k])}
for (j in 1:3){
for (h in 1:3){
cop_1[i,j,h]<- c1[j,h]*(-1)^(x1[i,j] + x1[i,h])/(s1_1[i,j]*s1_1[i,h])
con_1[i,j,h]<- c2[j,h]*(-1)^(x1[i,j] + x1[i,h])/(s2_1[i,j]*s2_1[i,h])
}}
eta1[i] = (prod(s1_1[i,1:K]) *(1+ sum(cop_1[i,,])))
theta1[i] =(prod(s2_1[i, 1:K]) *(1+sum(con_1[i,,])))
prob1[i]=pi1*eta1[i] + (1-pi1)*theta1[i]
z1[i] ~ dpois( - log(prob1[i]))
}
for(i in 1:n2){
for (k in 1:K){
s1_2[i,k]<- se[k]^x2[i,k]*((1-se[k])^(1-x2[i,k]))
s2_2[i,k]<- sp[k]^(1-x2[i,k])*((1-sp[k])^x2[i,k])}
for (j in 1:3){
for (h in 1:3){
cop_2[i,j,h]<- c1[j,h]*(-1)^(x2[i,j] + x2[i,h])/(s1_2[i,j]*s1_2[i,h])
con_2[i,j,h]<- c2[j,h]*(-1)^(x2[i,j] + x2[i,h])/(s2_2[i,j]*s2_2[i,h])
}}
eta2[i] = (prod(s1_2[i,1:K]) *(1+ sum(cop_2[i,,])))
theta2[i] =(prod(s2_2[i, 1:K]) *(1+sum(con_2[i,,])))
prob2[i]=pi2*eta2[i] + (1-pi2)*theta2[i]
z2[i] ~ dpois( - log(prob2[i]))
}
for(i in 1:n3){
for (k in 1:K){
s1_3[i,k]<- se[k]^x3[i,k]*((1-se[k])^(1-x3[i,k]))
s2_3[i,k]<- sp[k]^(1-x3[i,k])*((1-sp[k])^x3[i,k])}
for (j in 1:3){
for (h in 1:3){
cop_3[i,j,h]<- c1[j,h]*(-1)^(x3[i,j] + x3[i,h])/(s1_3[i,j]*s1_3[i,h])
con_3[i,j,h]<- c2[j,h]*(-1)^(x3[i,j] + x3[i,h])/(s2_3[i,j]*s2_3[i,h])
}}
eta3[i] = (prod(s1_3[i,1:K]) *(1+ sum(cop_3[i,,])))
theta3[i] =(prod(s2_3[i, 1:K]) *(1+sum(con_3[i,,])))
prob3[i]=pi3*eta3[i] + (1-pi3)*theta3[i]
z3[i] ~ dpois( - log(prob3[i]))
}
for(i in 1:n4){
for (k in 1:K){
s1_4[i,k]<- se[k]^x4[i,k]*((1-se[k])^(1-x4[i,k]))
s2_4[i,k]<- sp[k]^(1-x4[i,k])*((1-sp[k])^x4[i,k])}
for (j in 1:3){
for (h in 1:3){
cop_4[i,j,h]<- c1[j,h]*(-1)^(x4[i,j] + x4[i,h])/(s1_4[i,j]*s1_4[i,h])
con_4[i,j,h]<- c2[j,h]*(-1)^(x4[i,j] + x4[i,h])/(s2_4[i,j]*s2_4[i,h])
}}
eta4[i] = (prod(s1_4[i,1:K]) *(1+ sum(cop_4[i,,])))
theta4[i] =(prod(s2_4[i, 1:K]) *(1+sum(con_4[i,,])))
prob4[i]=pi4*eta4[i] + (1-pi4)*theta4[i]
z4[i] ~ dpois( - log(prob4[i]))
}
for(i in 1:n5){
for (k in 1:K){
s1_5[i,k]<- se[k]^x5[i,k]*((1-se[k])^(1-x5[i,k]))
s2_5[i,k]<- sp[k]^(1-x5[i,k])*((1-sp[k])^x5[i,k])}
for (j in 1:3){
for (h in 1:3){
cop_5[i,j,h]<- c1[j,h]*(-1)^(x5[i,j] + x5[i,h])/(s1_5[i,j]*s1_5[i,h])
con_5[i,j,h]<- c2[j,h]*(-1)^(x5[i,j] + x5[i,h])/(s2_5[i,j]*s2_5[i,h])
}}
eta5[i] = (prod(s1_5[i,1:K]) *(1+ sum(cop_5[i,,])))
theta5[i] =(prod(s2_5[i, 1:K]) *(1+sum(con_5[i,,])))
prob5[i]=pi5*eta5[i] + (1-pi5)*theta5[i]
z5[i] ~ dpois( - log(prob5[i]))
}
for(i in 1:n6){
for (k in 1:K){
s1_6[i,k]<- se[k]^x6[i,k]*((1-se[k])^(1-x6[i,k]))
s2_6[i,k]<- sp[k]^(1-x6[i,k])*((1-sp[k])^x6[i,k])}
for (j in 1:3){
for (h in 1:3){
cop_6[i,j,h]<- c1[j,h]*(-1)^(x6[i,j] + x6[i,h])/(s1_6[i,j]*s1_6[i,h])
con_6[i,j,h]<- c2[j,h]*(-1)^(x6[i,j] + x6[i,h])/(s2_6[i,j]*s2_6[i,h])
}}
eta6[i] = (prod(s1_6[i,1:K]) *(1+ sum(cop_6[i,,])))
theta6[i] =(prod(s2_6[i, 1:K]) *(1+sum(con_6[i,,])))
prob6[i]=pi6*eta6[i] + (1-pi6)*theta6[i]
z6[i] ~ dpois( - log(prob6[i]))
}
for(i in 1:n7){
for (k in 1:K){
s1_7[i,k]<- se[k]^x7[i,k]*((1-se[k])^(1-x7[i,k]))
s2_7[i,k]<- sp[k]^(1-x7[i,k])*((1-sp[k])^x7[i,k])}
for (j in 1:3){
for (h in 1:3){
cop_7[i,j,h]<- c1[j,h]*(-1)^(x7[i,j] + x7[i,h])/(s1_7[i,j]*s1_7[i,h])
con_7[i,j,h]<- c2[j,h]*(-1)^(x7[i,j] + x7[i,h])/(s2_7[i,j]*s2_7[i,h])
}}
eta7[i] = (prod(s1_7[i,1:K]) *(1+ sum(cop_7[i,,])))
theta7[i] =(prod(s2_7[i, 1:K]) *(1+sum(con_7[i,,])))
prob7[i]=pi7*eta7[i] + (1-pi7)*theta7[i]
z7[i] ~ dpois( - log(prob7[i]))
}
for (k in 1:K){
se[k] ~ dbeta( omega1*(kappa1 -2)+1, (1-omega1)*(kappa1-2) +1)
sp[k] ~ dbeta( omega2*(kappa2 -2)+1, (1-omega2)*(kappa2-2) +1)
}
for (l in 1:(3-1)){
for (h in (l+1):3){
c1[l,h] ~ dunif((se[l]-1)*(1-se[h]), (min(se[l],se[h])-se[l]*se[h]))
c2[l,h] ~ dunif((sp[l]-1)*(1-sp[h]), (min(sp[l],sp[h])-sp[l]*sp[h]))
}}
for (h in 1:K){
for (l in h:K){
c1[l,h] <-0
c2[l,h] <-0
}}
omega1 ~ dbeta(1,1)T(0.5, )
omega2 ~ dbeta(1,1)T(0.5,)
kappa1 = kappaMinusTwo1 +2
kappaMinusTwo1~ dgamma(0.01,0.01)
kappa2 = kappaMinusTwo2+2
kappaMinusTwo2 ~ dgamma(0.01,0.01)
pi1 ~ dbeta(1.4,1.4)
pi2 ~ dbeta(1.4,1.4)
pi3 ~ dbeta(1.4,1.4)
pi4 ~ dbeta(1.4,1.4)
pi5 ~ dbeta(1.4,1.4)
pi6 ~ dbeta(1.4,1.4)
pi7 ~ dbeta(1.4,1.4)
}"
writeLines( model_ntests_dep_7pop , con="TEMPmodel_ntests_dep_7pop.bug" )

################################################################################
# Independence // 2 populations (free/tiestalls) ####
################################################################################

model_ntests_indep_2pop = "
model{
for(i in 1:n1){
for (k in 1:K){
s1_1[i,k]<- se[k]^x1[i,k]*((1-se[k])^(1-x1[i,k]))
s2_1[i,k]<- sp[k]^(1-x1[i,k])*((1-sp[k])^x1[i,k])
}
prob1[i]=pi1*(prod(s1_1[i,1:K])) + (1-pi1)*(prod(s2_1[i, 1:K]))
z1[i] ~ dpois( - log(prob1[i]))
}
for(i in 1:n2){
for (k in 1:K){
s1_2[i,k]<- se[k]^x2[i,k]*((1-se[k])^(1-x2[i,k]))
s2_2[i,k]<- sp[k]^(1-x2[i,k])*((1-sp[k])^x2[i,k])
}
prob2[i]=pi2*(prod(s1_2[i,1:K])) + (1-pi2)*(prod(s2_2[i, 1:K]))
z2[i] ~ dpois( - log(prob2[i]))
}
for (k in 1:K){
se[k] ~ dbeta( omega1*(kappa1 -2)+1, (1-omega1)*(kappa1-2) +1)
sp[k] ~ dbeta( omega2*(kappa2 -2)+1, (1-omega2)*(kappa2-2) +1)
}
omega1 ~ dbeta(1,1)T(0.5,)
omega2 ~ dbeta(1,1)T(0.5,)
kappa1 = kappaMinusTwo1 +2
kappaMinusTwo1~ dgamma(0.01,0.01)
kappa2 = kappaMinusTwo2+2
kappaMinusTwo2 ~ dgamma(0.01,0.01)
pi1 ~ dbeta(1.4,1.4)
pi2 ~ dbeta(1.4,1.4)
}"
writeLines( model_ntests_indep_2pop , con="TEMPmodel_ntests_indep_2pop.bug" )

################################################################################

#n is the sample size,
n1 <- nrow(farmnot5)
n2 <- nrow(farm5)
#K is the number of tests,
K <- 6

y1_p20l100 <- farmnot5 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)
y2_p20l100 <- farm5 %>%
  select(bacterio, tp, petrifilm20, lumino100, pvd, es)

m1_p20l100 <- as.matrix(y1_p20l100)
m2_p20l100 <- as.matrix(y2_p20l100)

x1_p20l100<- structure(m1_p20l100, .Dim=c(n1,K))
x2_p20l100<- structure(m2_p20l100, .Dim=c(n2,K))

z1<-rep(0,n1)
z2<-rep(0,n2)

dat_p20l100=list(K=K,
                 n1=n1, n2=n2, 
                 x1=x1_p20l100,
                 x2=x2_p20l100,
                 z1=z1, z2=z2) #prepare the data list

par1=c("se","sp")

out_p20l100_2pop <- jags(model ="TEMPmodel_ntests_indep_2pop.bug", 
                         parameters.to.save=par1, 
                         data=dat_p20l100, n.chains=3,
                         n.iter=50000, n.burnin=10000, n.thin = 2,
                         #inits = inits,
                         DIC = TRUE)



################################################################################
# Independence // 6 populations (remove one farm at the time) ####
################################################################################

model_ntests_indep_6pop = "
model{
for(i in 1:n1){
for (k in 1:K){
s1_1[i,k]<- se[k]^x1[i,k]*((1-se[k])^(1-x1[i,k]))
s2_1[i,k]<- sp[k]^(1-x1[i,k])*((1-sp[k])^x1[i,k])
}
prob1[i]=pi1*(prod(s1_1[i,1:K])) + (1-pi1)*(prod(s2_1[i, 1:K]))
z1[i] ~ dpois( - log(prob1[i]))
}
for(i in 1:n2){
for (k in 1:K){
s1_2[i,k]<- se[k]^x2[i,k]*((1-se[k])^(1-x2[i,k]))
s2_2[i,k]<- sp[k]^(1-x2[i,k])*((1-sp[k])^x2[i,k])
}
prob2[i]=pi2*(prod(s1_2[i,1:K])) + (1-pi2)*(prod(s2_2[i, 1:K]))
z2[i] ~ dpois( - log(prob2[i]))
}
for(i in 1:n3){
for (k in 1:K){
s1_3[i,k]<- se[k]^x3[i,k]*((1-se[k])^(1-x3[i,k]))
s2_3[i,k]<- sp[k]^(1-x3[i,k])*((1-sp[k])^x3[i,k])
}
prob3[i]=pi3*(prod(s1_3[i,1:K])) + (1-pi3)*(prod(s2_3[i, 1:K]))
z3[i] ~ dpois( - log(prob3[i]))
}
for(i in 1:n4){
for (k in 1:K){
s1_4[i,k]<- se[k]^x4[i,k]*((1-se[k])^(1-x4[i,k]))
s2_4[i,k]<- sp[k]^(1-x4[i,k])*((1-sp[k])^x4[i,k])
}
prob4[i]=pi4*(prod(s1_4[i,1:K])) + (1-pi4)*(prod(s2_4[i, 1:K]))
z4[i] ~ dpois( - log(prob4[i]))
}
for(i in 1:n5){
for (k in 1:K){
s1_5[i,k]<- se[k]^x5[i,k]*((1-se[k])^(1-x5[i,k]))
s2_5[i,k]<- sp[k]^(1-x5[i,k])*((1-sp[k])^x5[i,k])
}
prob5[i]=pi5*(prod(s1_5[i,1:K])) + (1-pi5)*(prod(s2_5[i, 1:K]))
z5[i] ~ dpois( - log(prob5[i]))
}
for(i in 1:n6){
for (k in 1:K){
s1_6[i,k]<- se[k]^x6[i,k]*((1-se[k])^(1-x6[i,k]))
s2_6[i,k]<- sp[k]^(1-x6[i,k])*((1-sp[k])^x6[i,k])
}
prob6[i]=pi6*(prod(s1_6[i,1:K])) + (1-pi6)*(prod(s2_6[i, 1:K]))
z6[i] ~ dpois( - log(prob6[i]))
}
for (k in 1:K){
se[k] ~ dbeta( omega1*(kappa1 -2)+1, (1-omega1)*(kappa1-2) +1)
sp[k] ~ dbeta( omega2*(kappa2 -2)+1, (1-omega2)*(kappa2-2) +1)
}
omega1 ~ dbeta(1,1)T(0.5,)
omega2 ~ dbeta(1,1)T(0.5,)
kappa1 = kappaMinusTwo1 +2
kappaMinusTwo1~ dgamma(0.01,0.01)
kappa2 = kappaMinusTwo2+2
kappaMinusTwo2 ~ dgamma(0.01,0.01)
pi1 ~ dbeta(1.4,1.4)
pi2 ~ dbeta(1.4,1.4)
pi3 ~ dbeta(1.4,1.4)
pi4 ~ dbeta(1.4,1.4)
pi5 ~ dbeta(1.4,1.4)
pi6 ~ dbeta(1.4,1.4)
}"
writeLines( model_ntests_indep_6pop , con="TEMPmodel_ntests_indep_6pop.bug" )

