rm(list = ls())
load("rollcall.RData")


save(list = ls(all = TRUE), file= "rollcall.RData")


install.packages("")

library(factoextra)
library(Morpho)
library(DSpat)
library(wnominate)
library(MASS)
library(reshape)
library(tidyr)
library(nFactors)
library(pscl)
library(geosphere)
library(graphics)
library(stats)
library(tcltk)
library(eeptools)
library(scales)
library(gtools)
library(plyr)
library(psych)
library(rvest)
library(anominate)
library(MCMCpack)
library(dplyr)
library(xlsx)
library(spdep)

# functions

minvotes <- function(x){ # get the number of non-NA votes
  as.vector(apply(x, 1, function(x) length(which(!is.na(x)))))
}

rescale <- function (x, nx1, nx2, minx, maxx){ # rescale the data so as to limit them [minx,maxx]
  nx = nx1 + (nx2 - nx1) * (x - minx)/(maxx - minx)
  return(nx)
}





####################################
####### Datasets Loading ###########
####################################

### total ###
# (dataset of the MPs' names and party affiliations derived from https://www.psp.cz/sqw/hp.sqw?k=1300)

total <- read.csv("data/total.csv", header = TRUE, sep = ",", fileEncoding = "Windows-1250")


### 2013 ###
# (datasets of roll calls in individual parliamentary terms derived from https://www.psp.cz/sqw/hp.sqw?k=1300)

hl2013h1 = read.csv("data/hl-2013ps/hl2013h1.csv",sep = "|",header = FALSE)
hl2013h2 = read.csv("data/hl-2013ps/hl2013h2.csv",sep = "|",header = FALSE)
hl2013 <- rbind(hl2013h1,hl2013h2)
rm(hl2013h1,hl2013h2)
hl2013$V4 <- NULL
names(hl2013) <- c("id_poslanec","id_hlasovani","vysledek")
hl2013$vysledek <- as.character(hl2013$vysledek)
table(hl2013$vysledek)
hl2013[hl2013 == "A"] <- 1 # 578 864
hl2013[hl2013 == "B"] <- 0 # 186 106
hl2013[hl2013 == "@"] <- NA # 263 942
hl2013[hl2013 == "K"] <- 0 # 155 077
hl2013[hl2013 == "W"] <- NA # 11
hl2013$vysledek <- as.integer(hl2013$vysledek)
hl2013 <- cast(hl2013, id_poslanec ~ id_hlasovani)
hl2013 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno")],hl2013,by="id_poslanec")


### 1993 ###

hl1993h1 = read.csv("data/hl-1993ps/hl1993h1.csv",sep = "|",header = FALSE)
hl1993h2 = read.csv("data/hl-1993ps/hl1993h2.csv",sep = "|",header = FALSE)
hl1993 <- rbind(hl1993h1,hl1993h2)
rm(hl1993h1,hl1993h2)
hl1993$V4 <- NULL
names(hl1993) <- c("id_poslanec","id_hlasovani","vysledek")
hl1993$vysledek <- as.character(hl1993$vysledek)
table(hl1993$vysledek)
hl1993[hl1993 == "A"] <- 1 # 371 703
hl1993[hl1993 == "B"] <- 0 # 181 260
hl1993[hl1993 == "C"] <- 0 # 44 653
hl1993[hl1993 == "F"] <- 0 # 118 903
hl1993[hl1993 == "@"] <- NA # 317 081
hl1993$vysledek <- as.integer(hl1993$vysledek)
hl1993 <- cast(hl1993, id_poslanec ~ id_hlasovani)
hl1993 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno")],hl1993,by="id_poslanec")
hl1993$kandidatka_zkratka[hl1993$kandidatka_zkratka=="ODS-KDS"] <- "ODS"


### 1996 ###

hl1996 = read.csv("data/hl-1996ps/hl1996h1.csv",sep = "|",header = FALSE)
hl1996$V4 <- NULL
names(hl1996) <- c("id_poslanec","id_hlasovani","vysledek")
hl1996$vysledek <- as.character(hl1996$vysledek)
table(hl1996$vysledek)
hl1996[hl1996 == "A"] <- 1 # 475 215
hl1996[hl1996 == "B"] <- 0 # 229 013
hl1996[hl1996 == "C"] <- 0 # 0
hl1996[hl1996 == "F"] <- 0 # 151 689
hl1996[hl1996 == "@"] <- NA # 143 683
hl1996$vysledek <- as.integer(hl1996$vysledek)
hl1996<-cast(hl1996, id_poslanec ~ id_hlasovani)
hl1996 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno")],hl1996,by="id_poslanec")


### 1998 ###

hl1998h1 = read.csv("data/hl-1998ps/hl1998h1.csv",sep = "|",header = FALSE)
hl1998h2 = read.csv("data/hl-1998ps/hl1998h2.csv",sep = "|",header = FALSE)
hl1998h3 = read.csv("data/hl-1998ps/hl1998h3.csv",sep = "|",header = FALSE)
hl1998 <- rbind(hl1998h1,hl1998h2,hl1998h3)
rm(hl1998h1,hl1998h2,hl1998h3)
hl1998$V4 <- NULL
names(hl1998) <- c("id_poslanec","id_hlasovani","vysledek")
hl1998$vysledek <- as.character(hl1998$vysledek)
table(hl1998$vysledek)
hl1998[hl1998 == "A"] <- 1 # 1 451 402
hl1998[hl1998 == "B"] <- 0 # 622 411
hl1998[hl1998 == "C"] <- 0 # 40 294
hl1998[hl1998 == "F"] <- 0 # 247 478
hl1998[hl1998 == "@"] <- NA # 453 613
hl1998[hl1998 == "N"] <- 0 # 2 - this is Nay - confirmed by the Chamber of Deputies Information Office
hl1998$vysledek <- as.integer(hl1998$vysledek)
hl1998<-cast(hl1998, id_poslanec ~ id_hlasovani)
hl1998 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno")],hl1998,by="id_poslanec")


### 2002 ###

hl2002h1 = read.csv("data/hl-2002ps/hl2002h1.csv",sep = "|",header = FALSE)
hl2002h2 = read.csv("data/hl-2002ps/hl2002h2.csv",sep = "|",header = FALSE)
hl2002h3 = read.csv("data/hl-2002ps/hl2002h3.csv",sep = "|",header = FALSE)
hl2002 <- rbind(hl2002h1,hl2002h2,hl2002h3)
rm(hl2002h1,hl2002h2,hl2002h3)
hl2002$V4 <- NULL
names(hl2002) <- c("id_poslanec","id_hlasovani","vysledek")
hl2002$vysledek <- as.character(hl2002$vysledek)
table(hl2002$vysledek)
hl2002[hl2002 == "A"] <- 1 # 1 404 880
hl2002[hl2002 == "B"] <- 0 # 479 468
hl2002[hl2002 == "C"] <- 0 # 137 060
hl2002[hl2002 == "F"] <- 0 # 309 031
hl2002[hl2002 == "@"] <- NA # 498 287
hl2002[hl2002 == "N"] <- 0 # 274 - this is Nay - confirmed by the Chamber of Deputies Information Office
hl2002$vysledek <- as.integer(hl2002$vysledek)
hl2002 <- cast(hl2002, id_poslanec ~ id_hlasovani)
hl2002 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno")],hl2002,by="id_poslanec")
# load the special dataset to differentiate between KDU-CSL and US-DEU (both parties shared the candidate list in 2002 as K)
koalice <- read.xlsx("/Users/lukashajek/Desktop/University/Ph.D./Data/ctyrkoalice.xlsx",header = T, sheetName = "4koalice")
koalice$ctyrkoalice <- as.character(koalice$ctyrkoalice)
hl2002$kandidatka_zkratka <- as.character(hl2002$kandidatka_zkratka)
koalice <- koalice[order(koalice$id_poslanec),]
hl2002 <- hl2002[order(hl2002$id_poslanec),]
hl2002$kandidatka_zkratka[hl2002$kandidatka_zkratka=="K"] <- 
koalice$ctyrkoalice[koalice$id_poslanec==hl2002$id_poslanec[hl2002$kandidatka_zkratka=="K"]]
rm(koalice)


### 2006 ###

hl2006h1 = read.csv("data/hl-2006ps/hl2006h1.csv",sep = "|",header = FALSE)
hl2006h2 = read.csv("data/hl-2006ps/hl2006h2.csv",sep = "|",header = FALSE)
hl2006 <- rbind(hl2006h1,hl2006h2)
rm(hl2006h1,hl2006h2)
hl2006$V4 <- NULL
names(hl2006) <- c("id_poslanec","id_hlasovani","vysledek")
hl2006$vysledek <- as.character(hl2006$vysledek)
table(hl2006$vysledek)
hl2006[hl2006 == "A"] <- 1 # 865 154
hl2006[hl2006 == "B"] <- 0 # 235 835
hl2006[hl2006 == "C"] <- 0 # 89 133
hl2006[hl2006 == "F"] <- 0 # 173 377
hl2006[hl2006 == "@"] <- NA # 330 271
hl2006[hl2006 == "N"] <- 0 # 1 - this is Nay - confirmed by the Chamber of Deputies Information Office
hl2006$vysledek <- as.integer(hl2006$vysledek)
hl2006 <- cast(hl2006, id_poslanec ~ id_hlasovani)
hl2006 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno")],hl2006,by="id_poslanec")


### 2010 ###

hl2010h1 = read.csv("data/hl-2010ps/hl2010h1.csv",sep = "|",header = FALSE)
hl2010h2 = read.csv("data/hl-2010ps/hl2010h2.csv",sep = "|",header = FALSE)
hl2010 <- rbind(hl2010h1,hl2010h2)
rm(hl2010h1,hl2010h2)
hl2010$V4 <- NULL
names(hl2010) <- c("id_poslanec","id_hlasovani","vysledek")
hl2010$vysledek <- as.character(hl2010$vysledek)
table(hl2010$vysledek)
hl2010[hl2010 == "A"] <- 1 # 592 144
hl2010[hl2010 == "B"] <- 0 # 191 503
hl2010[hl2010 == "C"] <- 0 # 28 947
hl2010[hl2010 == "F"] <- 0 # 108 479
hl2010[hl2010 == "@"] <- NA # 262 709
hl2010[hl2010 == "W"] <- NA # 78
hl2010$vysledek <- as.integer(hl2010$vysledek)
hl2010 <- hl2010[hl2010$id_hlasovani!=58297,] # exclude the roll call because of a wrong inclusion
hl2010 <- cast(hl2010, id_poslanec ~ id_hlasovani)
hl2010 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno")],hl2010,by="id_poslanec")





##########################
##### Roll Call 2013 #####
##########################

# create RC object
hl2013 <- hl2013[minvotes(hl2013[,4:ncol(hl2013)])>=100,] # drop MPs with fewer than 100 votes
legData <- matrix(hl2013[, 2], length(hl2013[, 2]), 1)
colnames(legData) <- "party"
rc2013 <- rollcall(data = hl2013[,4:ncol(hl2013)], yea=1, nay=0, missing=NA,
                   legis.names = hl2013$id_poslanec, legis.data = legData,
                   vote.names = colnames(hl2013[4:ncol(hl2013)]), vote.data = as.matrix(colnames(hl2013[4:ncol(hl2013)])))
rm(legData)
rc2013 <- dropUnanimous(rc2013,lop=9) # drop votes with minority of 9 or fewer MPs (less than 5 % MPs)

# run the roll call analysis
#ideal2013 <- ideal(rc2013, d=2, maxiter=10000, burnin=5000, thin=10)
#wnom2013 <- wnominate(rc2013,dims=2,trials=3,polarity=c("1422","1422")) # postprocess (leader ODS - Miroslava Němcová)

i2013 <- as.data.frame(ideal2013$xbar) # transform the output into the dataset
i2013 <- add_rownames(i2013, "id_poslanec")
i2013 <- merge(total[total$obdobi=="PSP7",c("id_poslanec","obdobi","kandidatka_zkratka","jmeno")],i2013,by="id_poslanec")
colnames(i2013)[colnames(i2013)=="kandidatka_zkratka"] <- "strana"

ci2013 <- as.data.frame(ideal2013$x) # calculate standard deviations
ci2013.d1 <- ci2013[,1:(ncol(ci2013)/2)]
ci2013.d2 <- ci2013[,(ncol(ci2013)/2+1):ncol(ci2013)]
colnames(ci2013.d1) <- c(i2013$id_poslanec)
colnames(ci2013.d2) <- c(i2013$id_poslanec)
ci2013.d1 <- as.data.frame(t(ci2013.d1))
ci2013.d2 <- as.data.frame(t(ci2013.d2))
i2013$D1.sd <- apply(ci2013.d1,1,function(x) sd(x))
i2013$D2.sd <- apply(ci2013.d2,1,function(x) sd(x))
ci2013.d1 <- data.matrix(ci2013.d1) # get confidence intervals
ci2013.d2 <- data.matrix(ci2013.d2)
i2013$D1.ci95.low <- apply(ci2013.d1,1,function(x) quantile(x,.025))
i2013$D1.ci95.high <- apply(ci2013.d1,1,function(x) quantile(x,.975))
i2013$D2.ci95.low <- apply(ci2013.d2,1,function(x) quantile(x,.025))
i2013$D2.ci95.high <- apply(ci2013.d2,1,function(x) quantile(x,.975))
rm(ci2013,ci2013.d1,ci2013.d2)

a <- prcomp(as.matrix(i2013[,5:6])) # rotate the coordinates so the largest variance is parallel to the first dimension
var2013 <- c(a$sdev[1]^2/sum(a$sdev^2), a$sdev[2]^2/sum(a$sdev^2))*100
i2013$D1 <- a$x[,1]
i2013$D2 <- a$x[,2]
rm(a)

if ((max(i2013$D1)-min(i2013$D1))>(max(i2013$D2)-min(i2013$D2))) { # rescale so as the dimension with the largest range is [-1;1]
  a <- 2/(max(i2013$D1)-min(i2013$D1))
  i2013$D1 <- rescale(i2013$D1, -1, 1, min(i2013$D1), max(i2013$D1))
  i2013$D2 <- i2013$D2*a
} else {
  a <- 2/(max(i2013$D2)-min(i2013$D2))
  i2013$D2 <- rescale(i2013$D2, -1, 1, min(i2013$D2), max(i2013$D2))
  i2013$D1 <- i2013$D1*a
}
rm(a)

# create a plot
pdf("figures/IDEAL_PSP7_2013-2017.pdf")
par(mfrow=c(1,1),family="Times")
plot(i2013$D1[i2013$strana=="ODS"],i2013$D2[i2013$strana=="ODS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var2013[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var2013[2], 2), nsmall = 2),"%)")),
     main="IDEAL Coordinates - Chamber of Deputies, 2013-2017",
     xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5),cex=1.3)
points(i2013$D1[i2013$strana=="CSSD"],i2013$D2[i2013$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5),cex=1.3)
points(i2013$D1[i2013$strana=="USVIT"],i2013$D2[i2013$strana=="USVIT"],pch=19,col=adjustcolor("limegreen", alpha = 0.5),cex=1.3)
points(i2013$D1[i2013$strana=="ANO2011"],i2013$D2[i2013$strana=="ANO2011"],pch=19,col=adjustcolor("dodgerblue", alpha = 0.5),cex=1.3)
points(i2013$D1[i2013$strana=="KSCM"],i2013$D2[i2013$strana=="KSCM"],pch=19,col=adjustcolor("red", alpha = 0.5),cex=1.3)
points(i2013$D1[i2013$strana=="TOP09"],i2013$D2[i2013$strana=="TOP09"],pch=19,col=adjustcolor("darkmagenta", alpha = 0.5),cex=1.3)
points(i2013$D1[i2013$strana=="KDU-CSL"],i2013$D2[i2013$strana=="KDU-CSL"],pch=19,col=adjustcolor("gold2", alpha = 0.5),cex=1.3)
legend("bottomright",ncol=2,c("ANO","CSSD","KDU-CSL","KSCM","ODS","TOP09","USVIT"),
       pch=19, cex=.8, col=adjustcolor(c("dodgerblue","darkorange1","gold2","red","darkblue","darkmagenta","limegreen"), alpha = 0.5),
       title="Legend",box.col="black",pt.cex=1.3)
dev.off()


##########################
##### Roll Call 2010 #####
##########################

# create RC object
hl2010 <- hl2010[minvotes(hl2010[,4:ncol(hl2010)])>=100,] # drop MPs with fewer than 100 votes
legData <- matrix(hl2010[, 2], length(hl2010[, 2]), 1)
colnames(legData) <- "party"
rc2010 <- rollcall(data = hl2010[,4:ncol(hl2010)], yea=1, nay=0, missing=NA,
                   legis.names=hl2010$id_poslanec, legis.data = legData,
                   vote.names = colnames(hl2010[4:ncol(hl2010)]), vote.data = as.matrix(colnames(hl2010[4:ncol(hl2010)])))
rm(legData)
rc2010 <- dropUnanimous(rc2010,lop=9) # drop votes with minority of 9 or fewer MPs (less than 5 % MPs)

# run the roll call analysis
#ideal2010 <- ideal(rc2010, d=2, maxiter=10000, burnin=5000, thin=10)
#wnom2010 <- wnominate(rc2010,dims=2,trials=3,polarity=c("1198","1198")) # postprocess (leader ODS - Petr Nečas)

i2010 <- as.data.frame(ideal2010$xbar) # transform the output into the dataset
i2010 <- add_rownames(i2010, "id_poslanec")
i2010 <- merge(total[total$obdobi=="PSP6",c("id_poslanec","obdobi","kandidatka_zkratka","jmeno")],i2010,by="id_poslanec")
colnames(i2010)[colnames(i2010)=="kandidatka_zkratka"] <- "strana"

ci2010 <- as.data.frame(ideal2010$x) # calculate standard deviations
ci2010.d1 <- ci2010[,1:(ncol(ci2010)/2)]
ci2010.d2 <- ci2010[,(ncol(ci2010)/2+1):ncol(ci2010)]
colnames(ci2010.d1) <- c(i2010$id_poslanec)
colnames(ci2010.d2) <- c(i2010$id_poslanec)
ci2010.d1 <- as.data.frame(t(ci2010.d1))
ci2010.d2 <- as.data.frame(t(ci2010.d2))
i2010$D1.sd <- apply(ci2010.d1,1,function(x) sd(x))
i2010$D2.sd <- apply(ci2010.d2,1,function(x) sd(x))
ci2010.d1 <- data.matrix(ci2010.d1) # get confidence intervals
ci2010.d2 <- data.matrix(ci2010.d2)
i2010$D1.ci95.low <- apply(ci2010.d1,1,function(x) quantile(x,.025))
i2010$D1.ci95.high <- apply(ci2010.d1,1,function(x) quantile(x,.975))
i2010$D2.ci95.low <- apply(ci2010.d2,1,function(x) quantile(x,.025))
i2010$D2.ci95.high <- apply(ci2010.d2,1,function(x) quantile(x,.975))
rm(ci2010,ci2010.d1,ci2010.d2)

a <- prcomp(as.matrix(i2010[,5:6])) # rotate the coordinates so the largest variance is parallel to the first dimension
var2010 <- c(a$sdev[1]^2/sum(a$sdev^2), a$sdev[2]^2/sum(a$sdev^2))*100
i2010$D1 <- a$x[,1]
i2010$D2 <- a$x[,2]
rm(a)

if ((max(i2010$D1)-min(i2010$D1))>(max(i2010$D2)-min(i2010$D2))) { # rescale so as the dimension with the largest range is [-1;1]
  a <- 2/(max(i2010$D1)-min(i2010$D1))
  i2010$D1 <- rescale(i2010$D1, -1, 1, min(i2010$D1), max(i2010$D1))
  i2010$D2 <- i2010$D2*a
} else {
  a <- 2/(max(i2010$D2)-min(i2010$D2))
  i2010$D2 <- rescale(i2010$D2, -1, 1, min(i2010$D2), max(i2010$D2))
  i2010$D1 <- i2010$D1*a
}
rm(a)

# create a plot
pdf("figures/IDEAL_PSP6_2010-2013.pdf")
par(mfrow=c(1,1),family="Times")
plot(i2010$D1[i2010$strana=="ODS"],i2010$D2[i2010$strana=="ODS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var2010[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var2010[2], 2), nsmall = 2),"%)")),
     main="IDEAL Coordinates - Chamber of Deputies, 2010-2013",
     xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5))
points(i2010$D1[i2010$strana=="CSSD"],i2010$D2[i2010$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5))
points(i2010$D1[i2010$strana=="KSCM"],i2010$D2[i2010$strana=="KSCM"],pch=19,col=adjustcolor("red", alpha = 0.5))
points(i2010$D1[i2010$strana=="TOP09"],i2010$D2[i2010$strana=="TOP09"],pch=19,col=adjustcolor("darkmagenta", alpha = 0.5))
points(i2010$D1[i2010$strana=="VV"],i2010$D2[i2010$strana=="VV"],pch=19,col=adjustcolor("deepskyblue", alpha = 0.5))
legend("bottomright",ncol=2,c("CSSD","KSCM","ODS","TOP09","VV"),
       pch=19, cex=.8, col=adjustcolor(c("darkorange1","red","darkblue","darkmagenta","deepskyblue"), alpha = 0.5),
       title="Legend",box.col="black",pt.cex=1)
dev.off()


##########################
##### Roll Call 2006 #####
##########################

# create RC object
hl2006 <- hl2006[minvotes(hl2006[,4:ncol(hl2006)])>=100,] # drop MPs with fewer than 100 votes
legData <- matrix(hl2006[, 2], length(hl2006[, 2]), 1)
colnames(legData) <- "party"
rc2006 <- rollcall(data = hl2006[,4:ncol(hl2006)], yea=1, nay=0, missing=NA,
                   legis.names=hl2006$id_poslanec, legis.data = legData,
                   vote.names = colnames(hl2006[4:ncol(hl2006)]), vote.data = as.matrix(colnames(hl2006[4:ncol(hl2006)])))
rm(legData)
rc2006 <- dropUnanimous(rc2006,lop=9) # drop votes with minority of 9 or fewer MPs (less than 5 % MPs)

# run the roll call analysis
#ideal2006 <- ideal(rc2006, d=2, maxiter=10000, burnin=5000, thin=10)
#wnom2006 <- wnominate(rc2006,dims=2,trials=3,polarity=c("1042","1042")) # postprocess (leader ODS - Mirek Topolánek)

i2006 <- as.data.frame(ideal2006$xbar) # transform the output into the dataset
i2006 <- add_rownames(i2006, "id_poslanec")
i2006 <- merge(total[total$obdobi=="PSP5",c("id_poslanec","obdobi","kandidatka_zkratka","jmeno")],i2006,by="id_poslanec")
colnames(i2006)[colnames(i2006)=="kandidatka_zkratka"] <- "strana"

ci2006 <- as.data.frame(ideal2006$x) # calculate standard deviations
ci2006.d1 <- ci2006[,1:(ncol(ci2006)/2)]
ci2006.d2 <- ci2006[,(ncol(ci2006)/2+1):ncol(ci2006)]
colnames(ci2006.d1) <- c(i2006$id_poslanec)
colnames(ci2006.d2) <- c(i2006$id_poslanec)
ci2006.d1 <- as.data.frame(t(ci2006.d1))
ci2006.d2 <- as.data.frame(t(ci2006.d2))
i2006$D1.sd <- apply(ci2006.d1,1,function(x) sd(x))
i2006$D2.sd <- apply(ci2006.d2,1,function(x) sd(x))
ci2006.d1 <- data.matrix(ci2006.d1) # get confidence intervals
ci2006.d2 <- data.matrix(ci2006.d2)
i2006$D1.ci95.low <- apply(ci2006.d1,1,function(x) quantile(x,.025))
i2006$D1.ci95.high <- apply(ci2006.d1,1,function(x) quantile(x,.975))
i2006$D2.ci95.low <- apply(ci2006.d2,1,function(x) quantile(x,.025))
i2006$D2.ci95.high <- apply(ci2006.d2,1,function(x) quantile(x,.975))
rm(ci2006,ci2006.d1,ci2006.d2)

a <- prcomp(as.matrix(i2006[,5:6])) # rotate the coordinates so the largest variance is parallel to the first dimension
var2006 <- c(a$sdev[1]^2/sum(a$sdev^2), a$sdev[2]^2/sum(a$sdev^2))*100
i2006$D1 <- a$x[,1]*-1
i2006$D2 <- a$x[,2]*-1
rm(a)

if ((max(i2006$D1)-min(i2006$D1))>(max(i2006$D2)-min(i2006$D2))) { # rescale so as the dimension with the largest range is [-1;1]
  a <- 2/(max(i2006$D1)-min(i2006$D1))
  i2006$D1 <- rescale(i2006$D1, -1, 1, min(i2006$D1), max(i2006$D1))
  i2006$D2 <- i2006$D2*a
} else {
  a <- 2/(max(i2006$D2)-min(i2006$D2))
  i2006$D2 <- rescale(i2006$D2, -1, 1, min(i2006$D2), max(i2006$D2))
  i2006$D1 <- i2006$D1*a
}
rm(a)

# create a plot
pdf("figures/IDEAL_PSP5_2006-2010.pdf")
par(mfrow=c(1,1),family="Times")
plot(i2006$D1[i2006$strana=="ODS"],i2006$D2[i2006$strana=="ODS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var2006[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var2006[2], 2), nsmall = 2),"%)")),
     main="IDEAL Coordinates - Chamber of Deputies, 2006-2010",
     xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5))
points(i2006$D1[i2006$strana=="CSSD"],i2006$D2[i2006$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5))
points(i2006$D1[i2006$strana=="KSCM"],i2006$D2[i2006$strana=="KSCM"],pch=19,col=adjustcolor("red", alpha = 0.5))
points(i2006$D1[i2006$strana=="KDU-CSL"],i2006$D2[i2006$strana=="KDU-CSL"],pch=19,col=adjustcolor("gold2", alpha = 0.5))
points(i2006$D1[i2006$strana=="SZ"],i2006$D2[i2006$strana=="SZ"],pch=19,col=adjustcolor("darkgreen", alpha = 0.5))
legend("bottomright",ncol=2,c("CSSD","KDU-CSL","KSCM","ODS","SZ"),
       pch=19, cex=.8, col=adjustcolor(c("darkorange1","gold2","red","darkblue","darkgreen"), alpha = 0.5),
       title="Legend",box.col="black",pt.cex=1)
dev.off()


##########################
##### Roll Call 2002 #####
##########################

# create RC object
hl2002 <- hl2002[minvotes(hl2002[,4:ncol(hl2002)])>=100,] # drop MPs with fewer than 100 votes
legData <- matrix(hl2002[, 2], length(hl2002[, 2]), 1)
colnames(legData) <- "party"
rc2002 <- rollcall(data = hl2002[,4:ncol(hl2002)], yea=1, nay=0, missing=NA,
                   legis.names=hl2002$id_poslanec, legis.data = legData,
                   vote.names = colnames(hl2002[4:ncol(hl2002)]), vote.data = as.matrix(colnames(hl2002[4:ncol(hl2002)])))
rm(legData)
rc2002 <- dropUnanimous(rc2002,lop=9) # drop votes with minority of 9 or fewer MPs (minority has to be at least 5 % MPs)

# run the roll call analysis
#ideal2002 <- ideal(rc2002, d=2, maxiter=10000, burnin=5000, thin=10)
#wnom2002 <- wnominate(rc2002,dims=2,trials=3,polarity=c("633","633")) # postprocess (leader ODS - Václav Klaus)

i2002 <- as.data.frame(ideal2002$xbar) # transform the output into the dataset
i2002 <- add_rownames(i2002, "id_poslanec")
i2002 <- merge(hl2002[,c("id_poslanec","kandidatka_zkratka")],i2002,by="id_poslanec")
i2002 <- merge(total[total$obdobi=="PSP4",c("id_poslanec","obdobi","jmeno")],i2002,by="id_poslanec")
colnames(i2002)[colnames(i2002)=="kandidatka_zkratka"] <- "strana"

ci2002 <- as.data.frame(ideal2002$x) # calculate standard deviations
ci2002.d1 <- ci2002[,1:(ncol(ci2002)/2)]
ci2002.d2 <- ci2002[,(ncol(ci2002)/2+1):ncol(ci2002)]
colnames(ci2002.d1) <- c(i2002$id_poslanec)
colnames(ci2002.d2) <- c(i2002$id_poslanec)
ci2002.d1 <- as.data.frame(t(ci2002.d1))
ci2002.d2 <- as.data.frame(t(ci2002.d2))
i2002$D1.sd <- apply(ci2002.d1,1,function(x) sd(x))
i2002$D2.sd <- apply(ci2002.d2,1,function(x) sd(x))
ci2002.d1 <- data.matrix(ci2002.d1) # get confidence intervals
ci2002.d2 <- data.matrix(ci2002.d2)
i2002$D1.ci95.low <- apply(ci2002.d1,1,function(x) quantile(x,.025))
i2002$D1.ci95.high <- apply(ci2002.d1,1,function(x) quantile(x,.975))
i2002$D2.ci95.low <- apply(ci2002.d2,1,function(x) quantile(x,.025))
i2002$D2.ci95.high <- apply(ci2002.d2,1,function(x) quantile(x,.975))
rm(ci2002,ci2002.d1,ci2002.d2)

a <- prcomp(as.matrix(i2002[,5:6])) # rotate the coordinates so the largest variance is parallel to the first dimension
var2002 <- c(a$sdev[1]^2/sum(a$sdev^2), a$sdev[2]^2/sum(a$sdev^2))*100
i2002$D1 <- a$x[,1]
i2002$D2 <- a$x[,2]*-1
rm(a)

if ((max(i2002$D1)-min(i2002$D1))>(max(i2002$D2)-min(i2002$D2))) { # rescale so as the dimension with the largest range is [-1;1]
  a <- 2/(max(i2002$D1)-min(i2002$D1))
  i2002$D1 <- rescale(i2002$D1, -1, 1, min(i2002$D1), max(i2002$D1))
  i2002$D2 <- i2002$D2*a
} else {
  a <- 2/(max(i2002$D2)-min(i2002$D2))
  i2002$D2 <- rescale(i2002$D2, -1, 1, min(i2002$D2), max(i2002$D2))
  i2002$D1 <- i2002$D1*a
}
rm(a)

i2002$D2 <- i2002$D2 - (min(i2002$D2)+1) # move the initial center of the figure

# create a plot
pdf("figures/IDEAL_PSP4_2002-2006.pdf")
par(mfrow=c(1,1),family="Times")
plot(i2002$D1[i2002$strana=="ODS"],i2002$D2[i2002$strana=="ODS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var2002[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var2002[2], 2), nsmall = 2),"%)")),
     main="IDEAL Coordinates - Chamber of Deputies, 2002-2006",
     xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5))
points(i2002$D1[i2002$strana=="CSSD"],i2002$D2[i2002$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5))
points(i2002$D1[i2002$strana=="KSCM"],i2002$D2[i2002$strana=="KSCM"],pch=19,col=adjustcolor("red", alpha = 0.5))
points(i2002$D1[i2002$strana=="KDU-CSL"],i2002$D2[i2002$strana=="KDU-CSL"],pch=19,col=adjustcolor("gold2", alpha = 0.5))
points(i2002$D1[i2002$strana=="US-DEU"],i2002$D2[i2002$strana=="US-DEU"],pch=19,col=adjustcolor("gold2", alpha = 0.5))
legend("bottomright",ncol=2,c("CSSD","K","KSCM","ODS"),
       pch=19, cex=.8, col=adjustcolor(c("darkorange1","gold2","red","darkblue"), alpha = 0.5),
       title="Legend",box.col="black",pt.cex=1)
dev.off()


##########################
##### Roll Call 1998 #####
##########################

# create RC object
hl1998 <- hl1998[minvotes(hl1998[,4:ncol(hl1998)])>=100,] # drop MPs with fewer than 100 votes
legData <- matrix(hl1998[, 2], length(hl1998[, 2]), 1)
colnames(legData) <- "party"
rc1998 <- rollcall(data = hl1998[,4:ncol(hl1998)], yea=1, nay=0, missing=NA,
                   legis.names=hl1998$id_poslanec, legis.data = legData,
                   vote.names = colnames(hl1998[4:ncol(hl1998)]), vote.data = as.matrix(colnames(hl1998[4:ncol(hl1998)])))
rm(legData)
rc1998 <- dropUnanimous(rc1998,lop=9) # drop votes with minority of 9 or fewer MPs (less than 5 % MPs)

# run the roll call analysis
#ideal1998 <- ideal(rc1998, d=2, maxiter=10000, burnin=5000, thin=10)
#wnom1998 <- wnominate(rc1998,dims=2,trials=3,polarity=c("493","493")) # postprocess (leader - ODS Václav Klaus)

i1998 <- as.data.frame(ideal1998$xbar) # transform the output into the dataset
i1998 <- add_rownames(i1998, "id_poslanec")
i1998 <- merge(total[total$obdobi=="PSP3",c("id_poslanec","obdobi","kandidatka_zkratka","jmeno")],i1998,by="id_poslanec")
colnames(i1998)[colnames(i1998)=="kandidatka_zkratka"] <- "strana"

ci1998 <- as.data.frame(ideal1998$x) # calculate standard deviations
ci1998.d1 <- ci1998[,1:(ncol(ci1998)/2)]
ci1998.d2 <- ci1998[,(ncol(ci1998)/2+1):ncol(ci1998)]
colnames(ci1998.d1) <- c(i1998$id_poslanec)
colnames(ci1998.d2) <- c(i1998$id_poslanec)
ci1998.d1 <- as.data.frame(t(ci1998.d1))
ci1998.d2 <- as.data.frame(t(ci1998.d2))
i1998$D1.sd <- apply(ci1998.d1,1,function(x) sd(x))
i1998$D2.sd <- apply(ci1998.d2,1,function(x) sd(x))
ci1998.d1 <- data.matrix(ci1998.d1) # get confidence intervals
ci1998.d2 <- data.matrix(ci1998.d2)
i1998$D1.ci95.low <- apply(ci1998.d1,1,function(x) quantile(x,.025))
i1998$D1.ci95.high <- apply(ci1998.d1,1,function(x) quantile(x,.975))
i1998$D2.ci95.low <- apply(ci1998.d2,1,function(x) quantile(x,.025))
i1998$D2.ci95.high <- apply(ci1998.d2,1,function(x) quantile(x,.975))
rm(ci1998,ci1998.d1,ci1998.d2)

a <- prcomp(as.matrix(i1998[,5:6])) # rotate the coordinates so the largest variance is parallel to the first dimension
var1998 <- c(a$sdev[1]^2/sum(a$sdev^2), a$sdev[2]^2/sum(a$sdev^2))*100
i1998$D1 <- a$x[,1]*-1
i1998$D2 <- a$x[,2]*-1
rm(a)

if ((max(i1998$D1)-min(i1998$D1))>(max(i1998$D2)-min(i1998$D2))) { # rescale so as the dimension with the largest range is [-1;1]
  a <- 2/(max(i1998$D1)-min(i1998$D1))
  i1998$D1 <- rescale(i1998$D1, -1, 1, min(i1998$D1), max(i1998$D1))
  i1998$D2 <- i1998$D2*a
} else {
  a <- 2/(max(i1998$D2)-min(i1998$D2))
  i1998$D2 <- rescale(i1998$D2, -1, 1, min(i1998$D2), max(i1998$D2))
  i1998$D1 <- i1998$D1*a
}
rm(a)

# create a plot
pdf("figures/IDEAL_PSP3_1998-2002.pdf")
par(mfrow=c(1,1),family="Times")
plot(i1998$D1[i1998$strana=="ODS"],i1998$D2[i1998$strana=="ODS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var1998[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var1998[2], 2), nsmall = 2),"%)")),
     main="IDEAL Coordinates - Chamber of Deputies, 1998-2002",
     xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5))
points(i1998$D1[i1998$strana=="CSSD"],i1998$D2[i1998$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5))
points(i1998$D1[i1998$strana=="KSCM"],i1998$D2[i1998$strana=="KSCM"],pch=19,col=adjustcolor("red", alpha = 0.5))
points(i1998$D1[i1998$strana=="KDU-CSL"],i1998$D2[i1998$strana=="KDU-CSL"],pch=19,col=adjustcolor("gold2", alpha = 0.5))
points(i1998$D1[i1998$strana=="US"],i1998$D2[i1998$strana=="US"],pch=19,col=adjustcolor("deepskyblue1", alpha = 0.5))
legend("bottomright",ncol=2,c("CSSD","KDU-CSL","KSCM","ODS","US"),
       pch=19, cex=.8, col=adjustcolor(c("darkorange1","gold2","red","darkblue","deepskyblue1"), alpha = 0.5),
       title="Legend",box.col="black",pt.cex=1)
dev.off()


##########################
##### Roll Call 1996 #####
##########################

# create RC object
hl1996 <- hl1996[minvotes(hl1996[,4:ncol(hl1996)])>=100,] # drop MPs with fewer than 100 votes
legData <- matrix(hl1996[, 2], length(hl1996[, 2]), 1)
colnames(legData) <- "party"
rc1996 <- rollcall(data = hl1996[,4:ncol(hl1996)], yea=1, nay=0, missing=NA,
                   legis.names=hl1996$id_poslanec, legis.data = legData,
                   vote.names = colnames(hl1996[4:ncol(hl1996)]), vote.data = as.matrix(colnames(hl1996[4:ncol(hl1996)])))
rm(legData)
rc1996 <- dropUnanimous(rc1996,lop=9) # drop votes with minority of 9 or fewer MPs (less than 5 % MPs)

# run the roll call analysis
#ideal1996 <- ideal(rc1996, d=2, maxiter=10000, burnin=5000, thin=10)
#wnom1996 <- wnominate(rc1996,dims=2,trials=3,polarity=c("285","285")) # postprocess (leader ODS - Václav Klaus)

i1996 <- as.data.frame(ideal1996$xbar) # transform the output into the dataset
i1996 <- add_rownames(i1996, "id_poslanec")
i1996 <- merge(total[total$obdobi=="PSP2",c("id_poslanec","obdobi","kandidatka_zkratka","jmeno")],i1996,by="id_poslanec")
colnames(i1996)[colnames(i1996)=="kandidatka_zkratka"] <- "strana"

ci1996 <- as.data.frame(ideal1996$x) # calculate standard deviations
ci1996.d1 <- ci1996[,1:(ncol(ci1996)/2)]
ci1996.d2 <- ci1996[,(ncol(ci1996)/2+1):ncol(ci1996)]
colnames(ci1996.d1) <- c(i1996$id_poslanec)
colnames(ci1996.d2) <- c(i1996$id_poslanec)
ci1996.d1 <- as.data.frame(t(ci1996.d1))
ci1996.d2 <- as.data.frame(t(ci1996.d2))
i1996$D1.sd <- apply(ci1996.d1,1,function(x) sd(x))
i1996$D2.sd <- apply(ci1996.d2,1,function(x) sd(x))
ci1996.d1 <- data.matrix(ci1996.d1) # get confidence intervals
ci1996.d2 <- data.matrix(ci1996.d2)
i1996$D1.ci95.low <- apply(ci1996.d1,1,function(x) quantile(x,.025))
i1996$D1.ci95.high <- apply(ci1996.d1,1,function(x) quantile(x,.975))
i1996$D2.ci95.low <- apply(ci1996.d2,1,function(x) quantile(x,.025))
i1996$D2.ci95.high <- apply(ci1996.d2,1,function(x) quantile(x,.975))
rm(ci1996,ci1996.d1,ci1996.d2)

a <- prcomp(as.matrix(i1996[,5:6])) # rotate the coordinates so the largest variance is parallel to the first dimension
var1996 <- c(a$sdev[1]^2/sum(a$sdev^2), a$sdev[2]^2/sum(a$sdev^2))*100
i1996$D1 <- a$x[,1]*-1
i1996$D2 <- a$x[,2]
rm(a)

if ((max(i1996$D1)-min(i1996$D1))>(max(i1996$D2)-min(i1996$D2))) { # rescale so as the dimension with the largest range is [-1;1]
  a <- 2/(max(i1996$D1)-min(i1996$D1))
  i1996$D1 <- rescale(i1996$D1, -1, 1, min(i1996$D1), max(i1996$D1))
  i1996$D2 <- i1996$D2*a
} else {
  a <- 2/(max(i1996$D2)-min(i1996$D2))
  i1996$D2 <- rescale(i1996$D2, -1, 1, min(i1996$D2), max(i1996$D2))
  i1996$D1 <- i1996$D1*a
}
rm(a)

# create a plot
pdf("figures/IDEAL_PSP2_1996-1998.pdf")
par(mfrow=c(1,1),family="Times")
plot(i1996$D1[i1996$strana=="ODS"],i1996$D2[i1996$strana=="ODS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var1996[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var1996[2], 2), nsmall = 2),"%)")),
     main="IDEAL Coordinates - Chamber of Deputies, 1996-1998",
     xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5))
points(i1996$D1[i1996$strana=="CSSD"],i1996$D2[i1996$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5))
points(i1996$D1[i1996$strana=="KSCM"],i1996$D2[i1996$strana=="KSCM"],pch=19,col=adjustcolor("red", alpha = 0.5))
points(i1996$D1[i1996$strana=="KDU-CSL"],i1996$D2[i1996$strana=="KDU-CSL"],pch=19,col=adjustcolor("gold2", alpha = 0.5))
points(i1996$D1[i1996$strana=="ODA"],i1996$D2[i1996$strana=="ODA"],pch=19,col=adjustcolor("deepskyblue2", alpha = 0.5))
points(i1996$D1[i1996$strana=="SPR-RSC"],i1996$D2[i1996$strana=="SPR-RSC"],pch=19,col=adjustcolor("black", alpha = 0.5))
legend("bottomright",ncol=2,c("CSSD","KDU-CSL","KSCM","ODA","ODS","SPR-RSC"),
       pch=19, cex=.8, col=adjustcolor(c("darkorange1","gold2","red","deepskyblue2","darkblue","black"), alpha = 0.5),
       title="Legend",box.col="black",pt.cex=1)
dev.off()


##########################
##### Roll Call 1993 #####
##########################

# create RC object
hl1993 <- hl1993[minvotes(hl1993[,4:ncol(hl1993)])>=100,] # drop MPs with fewer than 100 votes
legData <- matrix(hl1993[, 2], length(hl1993[, 2]), 1)
colnames(legData) <- "party"
rc1993 <- rollcall(data = hl1993[,4:ncol(hl1993)], yea=1, nay=0, missing=NA,
                   legis.names=hl1993$id_poslanec, legis.data = legData,
                   vote.names = colnames(hl1993[4:ncol(hl1993)]), vote.data = as.matrix(colnames(hl1993[4:ncol(hl1993)])))
rm(legData)
rc1993 <- dropUnanimous(rc1993,lop=9) # drop votes with minority of 9 or fewer MPs (less than 5 % MPs)

# run the roll call analysis
#ideal1993 <- ideal(rc1993, d=2, maxiter=10000, burnin=5000, thin=10)
#wnom1993 <- wnominate(rc1993,dims=2,trials=3,polarity=c("48","48")) # postprocess (ODS PPG leader - Jiří Honajzer)

i1993 <- as.data.frame(ideal1993$xbar) # transform the output into the dataset
i1993 <- add_rownames(i1993, "id_poslanec")
i1993 <- merge(total[total$obdobi=="PSP1",c("id_poslanec","obdobi","kandidatka_zkratka","jmeno")],i1993,by="id_poslanec")
colnames(i1993)[colnames(i1993)=="kandidatka_zkratka"] <- "strana"

ci1993 <- as.data.frame(ideal1993$x) # calculate standard deviations
ci1993.d1 <- ci1993[,1:(ncol(ci1993)/2)]
ci1993.d2 <- ci1993[,(ncol(ci1993)/2+1):ncol(ci1993)]
colnames(ci1993.d1) <- c(i1993$id_poslanec)
colnames(ci1993.d2) <- c(i1993$id_poslanec)
ci1993.d1 <- as.data.frame(t(ci1993.d1))
ci1993.d2 <- as.data.frame(t(ci1993.d2))
i1993$D1.sd <- apply(ci1993.d1,1,function(x) sd(x))
i1993$D2.sd <- apply(ci1993.d2,1,function(x) sd(x))
ci1993.d1 <- data.matrix(ci1993.d1) # get confidence intervals
ci1993.d2 <- data.matrix(ci1993.d2)
i1993$D1.ci95.low <- apply(ci1993.d1,1,function(x) quantile(x,.025))
i1993$D1.ci95.high <- apply(ci1993.d1,1,function(x) quantile(x,.975))
i1993$D2.ci95.low <- apply(ci1993.d2,1,function(x) quantile(x,.025))
i1993$D2.ci95.high <- apply(ci1993.d2,1,function(x) quantile(x,.975))
rm(ci1993,ci1993.d1,ci1993.d2)

a <- prcomp(as.matrix(i1993[,5:6])) # rotate the coordinates so the largest variance is parallel to the first dimension
var1993 <- c(a$sdev[1]^2/sum(a$sdev^2), a$sdev[2]^2/sum(a$sdev^2))*100
i1993$D1 <- a$x[,1]*-1
i1993$D2 <- a$x[,2]*-1
rm(a)

if ((max(i1993$D1)-min(i1993$D1))>(max(i1993$D2)-min(i1993$D2))) { # rescale so as the dimension with the largest range is [-1;1]
  a <- 2/(max(i1993$D1)-min(i1993$D1))
  i1993$D1 <- rescale(i1993$D1, -1, 1, min(i1993$D1), max(i1993$D1))
  i1993$D2 <- i1993$D2*a
} else {
  a <- 2/(max(i1993$D2)-min(i1993$D2))
  i1993$D2 <- rescale(i1993$D2, -1, 1, min(i1993$D2), max(i1993$D2))
  i1993$D1 <- i1993$D1*a
}
rm(a)

# create a plot
pdf("figures/IDEAL_PSP1_1993-1996.pdf")
par(mfrow=c(1,1),family="Times")
plot(i1993$D1[i1993$strana=="ODS-KDS"],i1993$D2[i1993$strana=="ODS-KDS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var1993[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var1993[2], 2), nsmall = 2),"%)")),     
     main="IDEAL Coordinates - Chamber of Deputies, 1993-1996", xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5))
points(i1993$D1[i1993$strana=="CSSD"],i1993$D2[i1993$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5))
points(i1993$D1[i1993$strana=="LB"],i1993$D2[i1993$strana=="LB"],pch=19,col=adjustcolor("red", alpha = 0.5))
points(i1993$D1[i1993$strana=="KDU-CSL"],i1993$D2[i1993$strana=="KDU-CSL"],pch=19,col=adjustcolor("gold2", alpha = 0.5))
points(i1993$D1[i1993$strana=="LSU"],i1993$D2[i1993$strana=="LSU"],pch=19,col=adjustcolor("green3", alpha = 0.5))
points(i1993$D1[i1993$strana=="HSD-SMS"],i1993$D2[i1993$strana=="HSD-SMS"],pch=19,col=adjustcolor("mediumorchid", alpha = 0.5))
points(i1993$D1[i1993$strana=="ODA"],i1993$D2[i1993$strana=="ODA"],pch=19,col=adjustcolor("deepskyblue2", alpha = 0.5))
points(i1993$D1[i1993$strana=="SPR-RSC"],i1993$D2[i1993$strana=="SPR-RSC"],pch=19,col=adjustcolor("black", alpha = 0.5))
legend("bottomright",ncol=2,c("CSSD","HSD-SMS","KDU-CSL","LB","LSU","ODA","ODS-KDS","SPR-RSC"),
       pch=19, cex=.8, col=adjustcolor(c("darkorange1","mediumorchid","gold2","red","green3","deepskyblue2","darkblue","black"), alpha = 0.5),
       title="Legend",box.col="black",pt.cex=1)
dev.off()





### save the output for other R scripts in this replication
save(wnom1993,wnom1996,wnom1998,wnom2002,wnom2006,wnom2010,wnom2013,
     rc1993,rc1996,rc1998,rc2002,rc2006,rc2010,rc2013, file= "wnom.RData")





##############################################
################ Figure ######################
##############################################

pdf("figures/IDEAL1.pdf", width = 10, height = 15)
par(mfrow=c(3,2),family="Times",ps=19, mar = c(4.5,4.5,4.5,1)) 
plot(i1993$D1[i1993$strana=="ODS-KDS"],i1993$D2[i1993$strana=="ODS-KDS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var1993[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var1993[2], 2), nsmall = 2),"%)")),
     main="a) 1993-1996", font.main = 1,
     xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5),cex=2)
points(i1993$D1[i1993$strana=="CSSD"],i1993$D2[i1993$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5),cex=2)
points(i1993$D1[i1993$strana=="LB"],i1993$D2[i1993$strana=="LB"],pch=19,col=adjustcolor("red", alpha = 0.5),cex=2)
points(i1993$D1[i1993$strana=="KDU-CSL"],i1993$D2[i1993$strana=="KDU-CSL"],pch=19,col=adjustcolor("gold2", alpha = 0.5),cex=2)
points(i1993$D1[i1993$strana=="LSU"],i1993$D2[i1993$strana=="LSU"],pch=19,col=adjustcolor("green3", alpha = 0.5),cex=2)
points(i1993$D1[i1993$strana=="HSD-SMS"],i1993$D2[i1993$strana=="HSD-SMS"],pch=19,col=adjustcolor("mediumorchid", alpha = 0.5),cex=2)
points(i1993$D1[i1993$strana=="ODA"],i1993$D2[i1993$strana=="ODA"],pch=19,col=adjustcolor("deepskyblue2", alpha = 0.5),cex=2)
points(i1993$D1[i1993$strana=="SPR-RSC"],i1993$D2[i1993$strana=="SPR-RSC"],pch=19,col=adjustcolor("black", alpha = 0.5),cex=2)
legend("bottomright",ncol=2,c("CSSD","HSD-SMS","KDU-CSL","LB","LSU","ODA","ODS-KDS","SPR-RSC"),
       pch=19, cex=1,  col=adjustcolor(c("darkorange1","mediumorchid","gold2","red","green3","deepskyblue2","darkblue","black"), alpha = 0.5),
       box.col="black",pt.cex=2,y.intersp= 1.5)
plot(i1996$D1[i1996$strana=="ODS"],i1996$D2[i1996$strana=="ODS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var1996[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var1996[2], 2), nsmall = 2),"%)")),
     main="b) 1996-1998", font.main = 1,
     xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5),cex=2)
points(i1996$D1[i1996$strana=="CSSD"],i1996$D2[i1996$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5),cex=2)
points(i1996$D1[i1996$strana=="KSCM"],i1996$D2[i1996$strana=="KSCM"],pch=19,col=adjustcolor("red", alpha = 0.5),cex=2)
points(i1996$D1[i1996$strana=="KDU-CSL"],i1996$D2[i1996$strana=="KDU-CSL"],pch=19,col=adjustcolor("gold2", alpha = 0.5),cex=2)
points(i1996$D1[i1996$strana=="ODA"],i1996$D2[i1996$strana=="ODA"],pch=19,col=adjustcolor("deepskyblue2", alpha = 0.5),cex=2)
points(i1996$D1[i1996$strana=="SPR-RSC"],i1996$D2[i1996$strana=="SPR-RSC"],pch=19,col=adjustcolor("black", alpha = 0.5),cex=2)
legend("bottomright",ncol=2,c("CSSD","KDU-CSL","KSCM","ODA","ODS","SPR-RSC"),
       pch=19, cex=1, col=adjustcolor(c("darkorange1","gold2","red","deepskyblue2","darkblue","black"), alpha = 0.5),
       box.col="black",pt.cex=2,y.intersp= 1.5)
plot(i1998$D1[i1998$strana=="ODS"],i1998$D2[i1998$strana=="ODS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var1998[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var1998[2], 2), nsmall = 2),"%)")),
     main="c) 1998-2002", font.main = 1,
     xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5),cex=2)
points(i1998$D1[i1998$strana=="CSSD"],i1998$D2[i1998$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5),cex=2)
points(i1998$D1[i1998$strana=="KSCM"],i1998$D2[i1998$strana=="KSCM"],pch=19,col=adjustcolor("red", alpha = 0.5),cex=2)
points(i1998$D1[i1998$strana=="KDU-CSL"],i1998$D2[i1998$strana=="KDU-CSL"],pch=19,col=adjustcolor("gold2", alpha = 0.5),cex=2)
points(i1998$D1[i1998$strana=="US"],i1998$D2[i1998$strana=="US"],pch=19,col=adjustcolor("deepskyblue1", alpha = 0.5),cex=2)
legend("bottomright",ncol=2,c("CSSD","KDU-CSL","KSCM","ODS","US"),
       pch=19, cex=1, col=adjustcolor(c("darkorange1","gold2","red","darkblue","deepskyblue1"), alpha = 0.5),
       box.col="black",pt.cex=2,y.intersp= 1.5)
plot(i2002$D1[i2002$strana=="ODS"],i2002$D2[i2002$strana=="ODS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var2002[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var2002[2], 2), nsmall = 2),"%)")),
     main="d) 2002-2006", font.main = 1,
     xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5),cex=2)
points(i2002$D1[i2002$strana=="CSSD"],i2002$D2[i2002$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5),cex=2)
points(i2002$D1[i2002$strana=="KSCM"],i2002$D2[i2002$strana=="KSCM"],pch=19,col=adjustcolor("red", alpha = 0.5),cex=2)
points(i2002$D1[i2002$strana=="KDU-CSL"],i2002$D2[i2002$strana=="KDU-CSL"],pch=19,col=adjustcolor("gold2", alpha = 0.5),cex=2)
points(i2002$D1[i2002$strana=="US-DEU"],i2002$D2[i2002$strana=="US-DEU"],pch=19,col=adjustcolor("gold2", alpha = 0.5),cex=2)
legend("bottomright",ncol=2,c("CSSD","K","KSCM","ODS"),
       pch=19, cex=1, col=adjustcolor(c("darkorange1","gold2","red","darkblue"), alpha = 0.5),
       box.col="black",pt.cex=2,y.intersp= 1.5)
plot(i2006$D1[i2006$strana=="ODS"],i2006$D2[i2006$strana=="ODS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var2006[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var2006[2], 2), nsmall = 2),"%)")),
     main="e) 2006-2010", font.main = 1,
     xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5),cex=2)
points(i2006$D1[i2006$strana=="CSSD"],i2006$D2[i2006$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5),cex=2)
points(i2006$D1[i2006$strana=="KSCM"],i2006$D2[i2006$strana=="KSCM"],pch=19,col=adjustcolor("red", alpha = 0.5),cex=2)
points(i2006$D1[i2006$strana=="KDU-CSL"],i2006$D2[i2006$strana=="KDU-CSL"],pch=19,col=adjustcolor("gold2", alpha = 0.5),cex=2)
points(i2006$D1[i2006$strana=="SZ"],i2006$D2[i2006$strana=="SZ"],pch=19,col=adjustcolor("darkgreen", alpha = 0.5),cex=2)
legend("bottomright",ncol=2,c("CSSD","KDU-CSL","KSCM","ODS","SZ"),
       pch=19, cex=1, col=adjustcolor(c("darkorange1","gold2","red","darkblue","darkgreen"), alpha = 0.5),
       box.col="black",pt.cex=2,y.intersp= 1.5)
plot(i2010$D1[i2010$strana=="ODS"],i2010$D2[i2010$strana=="ODS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var2010[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var2010[2], 2), nsmall = 2),"%)")),
     main="f) 2010-2013", font.main = 1,
     xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5),cex=2)
points(i2010$D1[i2010$strana=="CSSD"],i2010$D2[i2010$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5),cex=2)
points(i2010$D1[i2010$strana=="KSCM"],i2010$D2[i2010$strana=="KSCM"],pch=19,col=adjustcolor("red", alpha = 0.5),cex=2)
points(i2010$D1[i2010$strana=="TOP09"],i2010$D2[i2010$strana=="TOP09"],pch=19,col=adjustcolor("darkmagenta", alpha = 0.5),cex=2)
points(i2010$D1[i2010$strana=="VV"],i2010$D2[i2010$strana=="VV"],pch=19,col=adjustcolor("deepskyblue", alpha = 0.5),cex=2)
legend("bottomright",ncol=2,c("CSSD","KSCM","ODS","TOP09","VV"),
       pch=19, cex=1, col=adjustcolor(c("darkorange1","red","darkblue","darkmagenta","deepskyblue"), alpha = 0.5),
       box.col="black",pt.cex=2,y.intersp= 1.5)
dev.off()



pdf("figures/IDEAL2.pdf", width = 7, height = 7)
par(mfrow=c(1,1),family="Times",ps=18)
plot(i2013$D1[i2013$strana=="ODS"],i2013$D2[i2013$strana=="ODS"],
     xlab=(paste0("First Dimension (Expl. Var. = ",format(round(var2013[1], 2), nsmall = 2),"%)")),
     ylab=(paste0("Second Dimension (Expl. Var. = ",format(round(var2013[2], 2), nsmall = 2),"%)")),
     main="g) 2013-2017", font.main = 1,
     xlim=c(-1,1),ylim=c(-1,1),pch=19,col=adjustcolor("darkblue", alpha = 0.5),cex=2)
points(i2013$D1[i2013$strana=="CSSD"],i2013$D2[i2013$strana=="CSSD"],pch=19,col=adjustcolor("darkorange1", alpha = 0.5),cex=2)
points(i2013$D1[i2013$strana=="USVIT"],i2013$D2[i2013$strana=="USVIT"],pch=19,col=adjustcolor("limegreen", alpha = 0.5),cex=2)
points(i2013$D1[i2013$strana=="ANO2011"],i2013$D2[i2013$strana=="ANO2011"],pch=19,col=adjustcolor("dodgerblue", alpha = 0.5),cex=2)
points(i2013$D1[i2013$strana=="KSCM"],i2013$D2[i2013$strana=="KSCM"],pch=19,col=adjustcolor("red2", alpha = 0.5),cex=2)
points(i2013$D1[i2013$strana=="TOP09"],i2013$D2[i2013$strana=="TOP09"],pch=19,col=adjustcolor("darkmagenta", alpha = 0.5),cex=2)
points(i2013$D1[i2013$strana=="KDU-CSL"],i2013$D2[i2013$strana=="KDU-CSL"],pch=19,col=adjustcolor("gold2", alpha = 0.5),cex=2)
legend("bottomright",ncol=2,c("ANO","CSSD","KDU-CSL","KSCM","ODS","TOP09","USVIT"),
       pch=19, cex=1, col=adjustcolor(c("dodgerblue","darkorange1","gold2","red2","darkblue","darkmagenta","limegreen"), alpha = 0.5),
       box.col="black",pt.cex=2,y.intersp= 1.5)
dev.off()









