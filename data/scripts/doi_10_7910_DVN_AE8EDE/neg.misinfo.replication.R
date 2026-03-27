# --------------- Negativity and Misinformation  ---------------

# Stuart Soroka and Christopher Wlezien, forthcoming in Political Communication.

# Due to copyright restrictions (from Lexis-Nexis) we are unable to provide 
# the raw media content used to generate the media signals. We accordingly
# do not distribute data with raw content, including the data required to 
# replicate human coding in Section A of the Supplementary Information.

# The data distributed with this file nevertheless include all the measures 
# derived fromthe raw content, and the script that follows reproduces all 
# the exhibits in the paper. 

# Note that time series analyses rely on STATA, run from R using the 
# rSTATA package. Running the syntax below thus depends on both R and STATA,
# and the specifications for STATA should be changed on lines 30 and 31 to match
# your system.

## --------------- Prep ---------------

library(foreign)
library(quanteda)
library(car)
library(dplyr)
library(prais)
library(modelsummary)
library(tseries)
library(multcomp)
library(RStata)
options("RStata.StataPath" = '/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp')
options("RStata.StataVersion" = 15)

load("misinfo.neg.data.Rdata") 

# The A data frame includes all variables necessary to replicate results. The
# variables in that data frame are as follows:

# year - year
# month - month
# time - a time counter
# ym - year and month as a character variable
# pres - the last name of the sitting president
# dem - 1 for democratic presidencies, 0 for republican presidencies
# unrate - the raw unemployment rate
# unrate.ch - changes in the raw unemployment rate
# unrate1 - the z-score standardized unemployment rate
# unrate1.ch - changes in unrate1
# net - the net media signal (all outlets combined)
# net.abc - the net media signal for ABC (standardized)
# net.cbs - the net media signal for CBS (standardized)
# net.nbc - the net media signal for NBC (standardized)
# net.cnn - the net media signal for CNN (standardized)
# net.fox - the net media signal for Fox News (standardized)
# net.msn - the net media signal for MSNBC (standardized)
# net.bls - the net media signal for the BLS press releases (standardized)

## --------------- TABLES ---------------

### --------------- Table 1 ---------------

stata_src <- "
 tsset time
 prais net_abc ld.unrate1 d.unrate1 fd.unrate1
 estimates store m1
 prais net_cbs ld.unrate1 d.unrate1 fd.unrate1
 estimates store m2
 prais net_nbc ld.unrate1 d.unrate1 fd.unrate1
 estimates store m3
 prais net_cnn ld.unrate1 d.unrate1 fd.unrate1
 estimates store m4
 prais net_fox ld.unrate1 d.unrate1 fd.unrate1
 estimates store m5
 prais net_msn ld.unrate1 d.unrate1 fd.unrate1
 estimates store m6
 estout m1 m2 m3 m4 m5 m6 using \"table1\", nolz notype cells(b( fmt(%9.3f) star)  se(par)) stats(N r2 rho, fmt(3)) starlevels(+ .10 * .05 ** .01 *** .001) replace
"
stata(stata_src,data.in=A,stata.echo=T)

### --------------- Table 2 ---------------

stata_src <- "
 tsset time
 gen unrate1_up = 0
 replace unrate1_up = d.unrate1 if d.unrate1>0
 gen unrate1_dn = 0
 replace unrate1_dn = d.unrate1 if d.unrate1<0
 prais net_abc l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn
 estimates store m1
 prais net_cbs l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn
 estimates store m2
 prais net_nbc l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn
 estimates store m3
 prais net_cnn l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn
 estimates store m4
 prais net_fox l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn
 estimates store m5
 prais net_msn l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn
 estimates store m6
 estout m1 m2 m3 m4 m5 m6 using \"table2\", nolz notype cells(b( fmt(%9.3f) star)  se(par)) stats(N r2 rho, fmt(3)) replace
"
stata(stata_src,data.in=A,stata.echo=T)

### --------------- Appendix Table 1 ---------------

stata_src <- "
 tsset time
 dfuller unrate1
 dfuller d.unrate1
 dfuller net_abc
 dfuller net_cbs
 dfuller net_nbc
 dfuller net_cnn
 dfuller net_fox
 dfuller net_msn
"
stata(stata_src,data.in=A,stata.echo=T)

### --------------- Appendix Table 2 ---------------

stata_src <- "
 tsset time
 gen dv = net_abc
 reg net_abc ld.unrate1 d.unrate1 fd.unrate1 l.dv
 estimates store m1
 replace dv = net_cbs
 reg net_cbs ld.unrate1 d.unrate1 fd.unrate1 l.dv
 estimates store m2
 replace dv = net_nbc
 reg net_nbc ld.unrate1 d.unrate1 fd.unrate1 l.dv
 estimates store m3
 replace dv = net_cnn
 reg net_cnn ld.unrate1 d.unrate1 fd.unrate1 l.dv
 estimates store m4
 replace dv = net_fox
 reg net_fox ld.unrate1 d.unrate1 fd.unrate1 l.dv
 estimates store m5
 replace dv = net_msn
 reg net_msn ld.unrate1 d.unrate1 fd.unrate1 l.dv
 estimates store m6
 estout m1 m2 m3 m4 m5 m6 using \"appendixtable2\", nolz notype cells(b( fmt(%9.3f) star)  se(par)) stats(N r2, fmt(3)) starlevels(+ .10 * .05 ** .01 *** .001) replace
"
stata(stata_src,data.in=A,stata.echo=T)

### --------------- Appendix Table 3 ---------------

stata_src <- "
 tsset time
 gen dv = net_abc
 reg net_abc l2d.unrate1 ld.unrate1 d.unrate1 fd.unrate1 l.dv
 estimates store m1
 replace dv = net_cbs
 reg net_cbs l2d.unrate1 ld.unrate1 d.unrate1 fd.unrate1 l.dv
 estimates store m2
 replace dv = net_nbc
 reg net_nbc l2d.unrate1 ld.unrate1 d.unrate1 fd.unrate1 l.dv
 estimates store m3
 replace dv = net_cnn
 reg net_cnn l2d.unrate1 ld.unrate1 d.unrate1 fd.unrate1 l.dv
 estimates store m4
 replace dv = net_fox
 reg net_fox l2d.unrate1 ld.unrate1 d.unrate1 fd.unrate1 l.dv
 estimates store m5
 replace dv = net_msn
 reg net_msn l2d.unrate1 ld.unrate1 d.unrate1 fd.unrate1 l.dv
 estimates store m6
 estout m1 m2 m3 m4 m5 m6 using \"appendixtable3\", nolz notype cells(b( fmt(%9.3f) star)  se(par)) stats(N r2, fmt(3)) starlevels(+ .10 * .05 ** .01 *** .001) replace
"
stata(stata_src,data.in=A,stata.echo=T)

### --------------- Appendix Table 4 ---------------

stata_src <- "
 tsset time
 prais net_abc ld.unrate1 d.unrate1 fd.unrate1 if dem==1
 estimates store m1
 prais net_cbs ld.unrate1 d.unrate1 fd.unrate1 if dem==1
 estimates store m2
 prais net_nbc ld.unrate1 d.unrate1 fd.unrate1 if dem==1
 estimates store m3
 prais net_cnn ld.unrate1 d.unrate1 fd.unrate1 if dem==1
 estimates store m4
 prais net_fox ld.unrate1 d.unrate1 fd.unrate1 if dem==1
 estimates store m5
 prais net_msn ld.unrate1 d.unrate1 fd.unrate1 if dem==1
 estimates store m6
 estout m1 m2 m3 m4 m5 m6 using \"appendixtable4\", nolz notype cells(b( fmt(%9.3f) star)  se(par)) stats(N r2 rho, fmt(3)) starlevels(+ .10 * .05 ** .01 *** .001) replace
"
stata(stata_src,data.in=A,stata.echo=T)

### --------------- Appendix Table 5 ---------------

stata_src <- "
 tsset time
 prais net_abc ld.unrate1 d.unrate1 fd.unrate1 if dem==0
 estimates store m1
 prais net_cbs ld.unrate1 d.unrate1 fd.unrate1 if dem==0
 estimates store m2
 prais net_nbc ld.unrate1 d.unrate1 fd.unrate1 if dem==0
 estimates store m3
 prais net_cnn ld.unrate1 d.unrate1 fd.unrate1 if dem==0
 estimates store m4
 prais net_fox ld.unrate1 d.unrate1 fd.unrate1 if dem==0
 estimates store m5
 prais net_msn ld.unrate1 d.unrate1 fd.unrate1 if dem==0
 estimates store m6
 estout m1 m2 m3 m4 m5 m6 using \"appendixtable5\", nolz notype cells(b( fmt(%9.3f) star)  se(par)) stats(N r2 rho, fmt(3)) starlevels(+ .10 * .05 ** .01 *** .001) replace
"
stata(stata_src,data.in=A,stata.echo=T)

### --------------- Appendix Table 6 ---------------

stata_src <- "
 tsset time
 gen unrate1_up = 0
 replace unrate1_up = d.unrate1 if d.unrate1>0
 gen unrate1_dn = 0
 replace unrate1_dn = d.unrate1 if d.unrate1<0
 prais net_abc l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn if dem==1
 estimates store m1
 prais net_cbs l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn if dem==1
 estimates store m2
 prais net_nbc l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn if dem==1
 estimates store m3
 prais net_cnn l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn if dem==1
 estimates store m4
 prais net_fox l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn if dem==1
 estimates store m5
 prais net_msn l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn if dem==1
 estimates store m6
 estout m1 m2 m3 m4 m5 m6 using \"appendixtable6\", nolz notype cells(b( fmt(%9.3f) star)  se(par)) stats(N r2 rho, fmt(3)) replace
"
stata(stata_src,data.in=A,stata.echo=T)

### --------------- Appendix Table 7 ---------------

stata_src <- "
 tsset time
 gen unrate1_up = 0
 replace unrate1_up = d.unrate1 if d.unrate1>0
 gen unrate1_dn = 0
 replace unrate1_dn = d.unrate1 if d.unrate1<0
 prais net_abc l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn if dem==0
 estimates store m1
 prais net_cbs l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn if dem==0
 estimates store m2
 prais net_nbc l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn if dem==0
 estimates store m3
 prais net_cnn l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn if dem==0
 estimates store m4
 prais net_fox l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn if dem==0
 estimates store m5
 prais net_msn l.unrate1_up unrate1_up f.unrate1_up l.unrate1_dn unrate1_dn f.unrate1_dn if dem==0
 estimates store m6
 estout m1 m2 m3 m4 m5 m6 using \"appendixtable7\", nolz notype cells(b( fmt(%9.3f) star)  se(par)) stats(N r2 rho, fmt(3)) replace
"
stata(stata_src,data.in=A,stata.echo=T)

### --------------- Appendix Table 8 ---------------

stata_src <- "
 tsset time
 prais net_bls l3d.unrate1 l2d.unrate1 ld.unrate1 d.unrate1 fd.unrate1 f2d.unrate1 
 estimates store m1
 estout m1 using \"appendixtable8\", nolz notype cells(b( fmt(%9.3f) star)  se(par)) stats(N r2 rho, fmt(3)) starlevels(+ .10 * .05 ** .01 *** .001) replace
"
stata(stata_src,data.in=A,stata.echo=T)


### --------------- Appendix Table 9 ---------------

stata_src <- "
 tsset time
 prais net_abc ld.unrate1 d.unrate1 fd.unrate1
 estimates store m1
 prais net_cbs ld.unrate1 d.unrate1 fd.unrate1
 estimates store m2
 prais net_nbc ld.unrate1 d.unrate1 fd.unrate1
 estimates store m3
 prais net_cnn ld.unrate1 d.unrate1 fd.unrate1
 estimates store m4
 prais net_fox ld.unrate1 d.unrate1 fd.unrate1
 estimates store m5
 prais net_msn ld.unrate1 d.unrate1 fd.unrate1
 estimates store m6
 estout m1 m2 m3 m4 m5 m6 using \"appendixtable9\", nolz notype cells(b( fmt(%9.3f) star)  se(par)) stats(N r2 rho, fmt(3)) starlevels(+ .10 * .05 ** .01 *** .001) replace
"
stata(stata_src,data.in=A[A$time<361,],stata.echo=T)



## --------------- FIGURES ---------------

### --------------- Figure 1 ---------------

A$net.sm <- (dplyr::lag(A$net) + dplyr::lead(A$net) + A$net) /3
A$net.cum <- cumsum(A$net)
A$net.cum.sm <- (dplyr::lag(A$net.cum) + dplyr::lead(A$net.cum) + A$net.cum) /3
A$unrate.sm <- (dplyr::lag(A$unrate) + dplyr::lead(A$unrate) + A$unrate) /3
A$unrate.ch.sm <- (dplyr::lag(A$unrate.ch) + dplyr::lead(A$unrate.ch) + A$unrate.ch) /3
cor.test(A$unrate,A$net.cum)
cor.test(A$unrate.ch,A$net)

{
  png("figure1.png", width=2800, height=2200, units="px", bg="white", res=300)
  
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), heights=c(1,1))
  par(mai = c(.3, 0.6806, 0.2, 0.2))
  
  plot(A$time,A$unrate.sm,type="l", axes=F, ann=F, lwd=2)
  axis(1, at=seq(121,362,by=12), labels=c(2000:2020), tck=-0.01)
  axis(1, at=seq(121,362,by=36), labels=c(2000, 2003, 2006,2009,2012,2015,2018))
  axis(2,las=1)
  mtext("Unemployment Rate", side=2, line=2.5)
  
  plot(A$time,A$unrate.ch.sm,type="l", axes=F, ann=F, lwd=2)
  axis(1, at=seq(121,362,by=12), labels=c(2000:2020), tck=-0.01)
  axis(1, at=seq(121,362,by=36), labels=c(2000, 2003, 2006,2009,2012,2015,2018))
  abline(h=0,lty=3,col="gray")
  axis(2,las=1)
  mtext("Chg in Unemployment Rate", side=2, line=2.5)
  
  plot(A$time,A$net.cum.sm,type="l", axes=F, ann=F, 
       ylim=c(-20,50), col="black", lwd=2)
  axis(3, at=seq(121,362,by=12), labels=c(2000:2020), tck=-0.01)
  axis(3, at=seq(121,362,by=36), labels=NA)
  axis(2,las=1)
  mtext("Cumulative Media Signal", side=2, line=2.5)
  
  plot(A$time,A$net.sm,type="l", axes=F, ann=F, 
       ylim=c(-1.8,5), col="black", lwd=2)
  abline(h=0,lty=3,col="gray")
  axis(3, at=seq(121,362,by=12), labels=c(2000:2020), tck=-0.01)
  axis(3, at=seq(121,362,by=36), labels=NA)
  axis(2,las=1)
  mtext("Media Signal", side=2, line=2.5)
  mtext("All lines shown as 3-month moving averages.", 
        side=1, line=0, adj=1, cex=0.8)
  
  invisible(dev.off())
}


### --------------- Figure 2 ---------------

#estimating accuracy

df <- read.csv("table1", sep="\t")
df <- df[c(2,4,6),]
for (i in 1:length(colnames(df))) {
  df[,i] <- gsub("\\*","",df[,i])
  df[,i] <- as.numeric(df[,i])
}
networks <- c("ABC","CBS","NBC","CNN","Fox","MSNBC")
v1 <- c(sum(df$m1),
        sum(df$m2),
        sum(df$m3),
        sum(df$m4),
        sum(df$m5),
        sum(df$m6) )

df <- read.csv("table1", sep="\t")

v2 <- c(df$m1[11],
        df$m2[11],
        df$m3[11],
        df$m4[11],
        df$m5[11],
        df$m6[11])
accuracy <- as.data.frame(cbind(networks,v1,v2))
accuracy$v1 <- as.numeric(accuracy$v1)
accuracy$v2 <- as.numeric(accuracy$v2)
rm(v1,v2)
accuracy

cor.test(accuracy$v2, accuracy$v1)

#figure 2

{
  png("figure2.png", width=1800, height=1800, units="px", bg="white", res=300)
  plot(accuracy$v2, accuracy$v1, pch=16, ann=F, axes=F, ylim=c(0,12), xlim=c(0,.5))
  axis(1)
  axis(2, las=1)
  mtext("Accuracy (Summed coefs)", side=2, line=3)
  mtext("Accuracy (R Squared)", side=1, line=2.5)
  text(accuracy$v2[accuracy$network=="ABC"], accuracy$v1[accuracy$network=="ABC"]+.1, labels="ABC", pos=2)
  text(accuracy$v2[accuracy$network=="CBS"], accuracy$v1[accuracy$network=="CBS"]-.05, labels="CBS", pos=4)
  text(accuracy$v2[accuracy$network=="NBC"], accuracy$v1[accuracy$network=="NBC"]-.05, labels="NBC", pos=2)
  text(accuracy$v2[accuracy$network=="Fox"], accuracy$v1[accuracy$network=="Fox"]+.13, labels="Fox", pos=4)
  text(accuracy$v2[accuracy$network=="MSNBC"], accuracy$v1[accuracy$network=="MSNBC"]-.22, labels="MSNBC", pos=4)
  text(accuracy$v2[accuracy$network=="CNN"], accuracy$v1[accuracy$network=="CNN"]+.01, labels="CNN", pos=4)
  invisible(dev.off())
}

### --------------- Figure 3 ---------------

#calculating negativity bias

networks <- c("ABC","CBS","NBC","CNN","Fox","MSNBC")
df <- read.csv("table2", sep="\t")
df <- df[c(2,4,6),]
for (i in 1:length(colnames(df))) {
  df[,i] <- gsub("\\*","",df[,i])
  df[,i] <- as.numeric(df[,i])
}
bias.up <- c(sum(df$m1),
             sum(df$m2),
             sum(df$m3),
             sum(df$m4),
             sum(df$m5),
             sum(df$m6) )

df <- read.csv("table2", sep="\t")
df <- df[c(8,10,12),]
for (i in 1:length(colnames(df))) {
  df[,i] <- gsub("\\*","",df[,i])
  df[,i] <- as.numeric(df[,i])
}
bias.dn <- c(sum(df$m1),
             sum(df$m2),
             sum(df$m3),
             sum(df$m4),
             sum(df$m5),
             sum(df$m6) )

nb <- as.data.frame(cbind(networks,bias.up,bias.dn))
nb$bias.up <- as.numeric(nb$bias.up)
nb$bias.dn <- as.numeric(nb$bias.dn)
colnames(nb) <- c("network","up","dn")
nb$bias2 <- (nb$up - nb$dn) / (abs(nb$up) + abs(nb$dn))
nb

cor.test(accuracy$v2,nb$bias2)

#figure 3

{
  png("figure3.png", width=1800, height=1800, units="px", bg="white", res=300)
  plot(accuracy$v2, nb$bias2 , pch=16, ann=F, axes=F, xlim=c(0,.5), ylim=c(.2,1))
  axis(1)
  axis(2, las=1)
  mtext("Negativity Bias", side=2, line=3)
  mtext("Accuracy (R Squared)", side=1, line=2.5)
  text(accuracy$v2, nb$bias2, labels=accuracy$network, pos=4)
  invisible(dev.off())
}

### --------------- Figure 4 ---------------

#accuracy across presidencies

networks <- c("ABC","CBS","NBC","CNN","Fox","MSNBC")

df <- read.csv("appendixtable4", sep="\t")
df <- df[c(2,4,6),]
for (i in 1:length(colnames(df))) {
  df[,i] <- gsub("\\*","",df[,i])
  df[,i] <- as.numeric(df[,i])
}
v1 <- c(sum(df$m1),
        sum(df$m2),
        sum(df$m3),
        sum(df$m4),
        sum(df$m5),
        sum(df$m6) )

df <- read.csv("appendixtable4", sep="\t")
v2 <- c(df$m1[11],
        df$m2[11],
        df$m3[11],
        df$m4[11],
        df$m5[11],
        df$m6[11])
accuracyD <- as.data.frame(cbind(networks,v1,v2))
accuracyD$v1 <- as.numeric(accuracyD$v1)
accuracyD$v2 <- as.numeric(accuracyD$v2)

df <- read.csv("appendixtable5", sep="\t")
df <- df[c(2,4,6),]
for (i in 1:length(colnames(df))) {
  df[,i] <- gsub("\\*","",df[,i])
  df[,i] <- as.numeric(df[,i])
}
v1 <- c(sum(df$m1),
        sum(df$m2),
        sum(df$m3),
        sum(df$m4),
        sum(df$m5),
        sum(df$m6) )

df <- read.csv("appendixtable5", sep="\t")
v2 <- c(df$m1[11],
        df$m2[11],
        df$m3[11],
        df$m4[11],
        df$m5[11],
        df$m6[11])
accuracyR <- as.data.frame(cbind(networks,v1,v2))
accuracyR$v1 <- as.numeric(accuracyR$v1)
accuracyR$v2 <- as.numeric(accuracyR$v2)

accuracyD
accuracyR

# negativity across presidencies

networks <- c("ABC","CBS","NBC","CNN","Fox","MSNBC")

df <- read.csv("appendixtable6", sep="\t")
df <- df[c(2,4,6),]
for (i in 1:length(colnames(df))) {
  df[,i] <- gsub("\\*","",df[,i])
  df[,i] <- as.numeric(df[,i])
}
bias.up <- c(sum(df$m1),
             sum(df$m2),
             sum(df$m3),
             sum(df$m4),
             sum(df$m5),
             sum(df$m6) )

df <- read.csv("appendixtable6", sep="\t")
df <- df[c(8,10,12),]
for (i in 1:length(colnames(df))) {
  df[,i] <- gsub("\\*","",df[,i])
  df[,i] <- as.numeric(df[,i])
}
bias.dn <- c(sum(df$m1),
             sum(df$m2),
             sum(df$m3),
             sum(df$m4),
             sum(df$m5),
             sum(df$m6) )

nbD <- as.data.frame(cbind(networks,bias.up,bias.dn))
nbD$bias.up <- as.numeric(nbD$bias.up)
nbD$bias.dn <- as.numeric(nbD$bias.dn)
colnames(nbD) <- c("network","up","dn")
nbD$bias1 <- nbD$up / nbD$dn
nbD$bias2 <- (nbD$up - nbD$dn) / (abs(nbD$up) + abs(nbD$dn))

df <- read.csv("appendixtable7", sep="\t")
df <- df[c(2,4,6),]
for (i in 1:length(colnames(df))) {
  df[,i] <- gsub("\\*","",df[,i])
  df[,i] <- as.numeric(df[,i])
}
bias.up <- c(sum(df$m1),
             sum(df$m2),
             sum(df$m3),
             sum(df$m4),
             sum(df$m5),
             sum(df$m6) )

df <- read.csv("appendixtable7", sep="\t")
df <- df[c(8,10,12),]
for (i in 1:length(colnames(df))) {
  df[,i] <- gsub("\\*","",df[,i])
  df[,i] <- as.numeric(df[,i])
}
bias.dn <- c(sum(df$m1),
             sum(df$m2),
             sum(df$m3),
             sum(df$m4),
             sum(df$m5),
             sum(df$m6) )

nbR <- as.data.frame(cbind(networks,bias.up,bias.dn))
nbR$bias.up <- as.numeric(nbR$bias.up)
nbR$bias.dn <- as.numeric(nbR$bias.dn)
colnames(nbR) <- c("network","up","dn")
nbR$bias1 <- nbR$up / nbR$dn
nbR$bias2 <- (nbR$up - nbR$dn) / (abs(nbR$up) + abs(nbR$dn))

nbD
nbR

cor.test(accuracyD$v2, accuracyR$v2)
cor.test(nbD$bias2, nbR$bias2)

{
  png("figure4.png", width=3600, height=1800, units="px", bg="white", res=300)
  par(mfrow=c(1,2))
  
  plot(accuracyD$v2, accuracyR$v2, pch=16, ann=F, axes=F, ylim=c(0,.6), xlim=c(0,.6))
  axis(1)
  axis(2, las=1)
  abline(a=0,b=1, lty=2, col="gray")
  mtext("Accuracy (R-squared, during Republican Presidents)", side=2, line=3)
  mtext("Accuracy (R-squared, during Democratic Presidents)", side=1, line=2.5)
  text(accuracyD$v2, accuracyR$v2, labels=accuracy$network, pos=4)
  
  plot(nbD$bias2, nbR$bias2, pch=16, ann=F, axes=F, ylim=c(-.1,1.2), xlim=c(-.1,1.2))
  axis(1, at=c(0,.2,.4,.6,.8,1))
  axis(2, las=1, at=c(0,.2,.4,.6,.8,1))
  abline(a=0,b=1, lty=2, col="gray")
  mtext("Negativity Bias (during Republican Presidents)", side=2, line=3)
  mtext("Negativity Bias (during Democratic Presidents)", side=1, line=2.5)
  text(nbD$bias2, nbR$bias2, labels=nbD$network, pos=4)
  
  invisible(dev.off())
}

### --------------- Appendix Figure 1 ---------------

{
  png("appendixfigure1.png", width=2800, height=1200, units="px", bg="white", res=300)
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), heights=c(2,1))
  brk <- c(-1, -0.8, -0.6, -0.4, -0.2, 0,  0.2,  0.4,  0.6,  0.8,  1)
  par(mai = c(.8, 0.8806, 0.1, 0.4486))
  hist(A$unrate.ch[A$dem==1], breaks=brk, ylim=c(0,80),
       main=" ", xlab="Monthly Changes in Unemployment Rate\n(Democratic Presidencies)", las=1, col="gray", border="gray")
  abline(v=mean(A$unrate.ch[A$dem==1]), lty=2)
  text(-.1, 75, paste0("Mean = ",round(mean(A$unrate.ch[A$dem==1]),3)), pos=4)
  par(mai = c(.8, 0.8806, 0.1, 0.4486))
  hist(A$unrate.ch[A$dem==0], breaks=brk, ylim=c(0,80),
       main=" ", xlab="Monthly Changes in Unemployment Rate\n(Republican Presidencies)", las=1, col="gray", border="gray")
  abline(v=mean(A$unrate.ch[A$dem==0]), lty=2)
  text(0, 75, paste0("Mean = ",round(mean(A$unrate.ch[A$dem==0]),3)), pos=4)
  invisible(dev.off())
}


### --------------- Appendix Figure 2 ---------------

accuracy$v3 <-  (nb$up + nb$dn) - (nb$up - nb$dn)

{
  png("appendixfigure2.png", width=1800, height=1800, units="px", bg="white", res=300)
  plot(accuracy$v1, accuracy$v3, pch=16, ann=F, axes=F, ylim=c(-8,12), xlim=c(0,10))
  axis(1)
  axis(2, las=1)
  mtext("Accuracy (Summed coefs)", side=1, line=3)
  mtext("Summed coefficients minus Asymmetry", side=2, line=2.5)
  text(accuracy$v1[accuracy$network=="ABC"], accuracy$v3[accuracy$network=="ABC"]+.1, labels="ABC", pos=2)
  text(accuracy$v1[accuracy$network=="CBS"], accuracy$v3[accuracy$network=="CBS"]-.05, labels="CBS", pos=2)
  text(accuracy$v1[accuracy$network=="NBC"], accuracy$v3[accuracy$network=="NBC"]-.05, labels="NBC", pos=2)
  text(accuracy$v1[accuracy$network=="Fox"], accuracy$v3[accuracy$network=="Fox"]+.13, labels="Fox", pos=2)
  text(accuracy$v1[accuracy$network=="MSNBC"], accuracy$v3[accuracy$network=="MSNBC"]-.22, labels="MSNBC", pos=2)
  text(accuracy$v1[accuracy$network=="CNN"], accuracy$v3[accuracy$network=="CNN"]+.01, labels="CNN", pos=2)
  invisible(dev.off())
}


### --------------- Appendix Figure 3 ---------------

cor.test(A$unrate,A$net.cum)
cor.test(A$unrate.ch,A$net)

A$net.sm <- (dplyr::lag(A$net.bls) + dplyr::lead(A$net.bls) + A$net.bls) /3
A$net.cum[A$year>1999] <- cumsum(A$net.bls[A$year>1999])
A$net.cum.sm <- (dplyr::lag(A$net.cum) + dplyr::lead(A$net.cum) + A$net.cum) /3

{
  png("appendixfigure3.png", width=2800, height=2200, units="px", bg="white", res=300)
  
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), heights=c(1,1))
  par(mai = c(.3, 0.6806, 0.2, 0.2))
  
  plot(A$time[A$year<2020],A$unrate.sm[A$year<2020],type="l", axes=F, ann=F, lwd=2)
  axis(1, at=seq(121,362,by=12), labels=c(2000:2020), tck=-0.01)
  axis(1, at=seq(121,362,by=36), labels=c(2000, 2003, 2006,2009,2012,2015,2018))
  axis(2,las=1)
  mtext("Unemployment Rate", side=2, line=2.5)
  
  plot(A$time[A$year<2020],A$unrate.ch.sm[A$year<2020],type="l", axes=F, ann=F, lwd=2)
  axis(1, at=seq(121,362,by=12), labels=c(2000:2020), tck=-0.01)
  axis(1, at=seq(121,362,by=36), labels=c(2000, 2003, 2006,2009,2012,2015,2018))
  abline(h=0,lty=3,col="gray")
  axis(2,las=1)
  mtext("Chg in Unemployment Rate", side=2, line=2.5)
  
  plot(A$time[A$year<2020],A$net.cum.sm[A$year<2020],type="l", axes=F, ann=F, 
       ylim=c(-10,50), col="black", lwd=2)
  axis(3, at=seq(121,362,by=12), labels=c(2000:2020), tck=-0.01)
  axis(3, at=seq(121,362,by=36), labels=NA)
  axis(2,las=1)
  mtext("Cumulative Press Release Signal", side=2, line=2.5)
  
  plot(A$time[A$year<2020],A$net.sm[A$year<2020],type="l", axes=F, ann=F, 
       ylim=c(-1.8,5), col="black", lwd=2)
  abline(h=0,lty=3,col="gray")
  axis(3, at=seq(121,362,by=12), labels=c(2000:2020), tck=-0.01)
  axis(3, at=seq(121,362,by=36), labels=NA)
  axis(2,las=1)
  mtext("Press Release Signal", side=2, line=2.5)
  mtext("All lines shown as 3-month moving averages.", 
        side=1, line=0, adj=1, cex=0.8)
  
  invisible(dev.off())
}



