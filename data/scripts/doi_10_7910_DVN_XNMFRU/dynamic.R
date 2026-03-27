
rm(list = ls())
load("dynamic.RData")

save(list = ls(all = TRUE), file= "dynamic.RData")



install.packages("")

library(reshape)
library(pscl)
library(Rcpp)
library(xlsx)
library(BBmisc)
library(graphics)
library(dplyr)
library(wnominate)
library(Morpho)
library(MCMCpack)
library(coda)
library(TeachingDemos)
library(parallel)
library(coda)
library(runjags)


# functions

chain <- function(theta.start, mcmc, burnin, thin, seed){ # function utilizing dynamic IRT
  MCMCdynamicIRT1d(datamatrix = hl_p_s, item.time.map = time, theta.start = theta.start,
                   theta.constraints = list(KSCM="-",ODS="+"),
                   mcmc = mcmc, burnin = burnin, thin = thin,
                   verbose = 5, tau2.start=rep(0.1, nrow(hl_p_s)), store.item = F, store.ability = T,
                   e0=0, E0=1, a0=0, A0=1, b0=0, B0=1, c0=-1, d0=-1, seed = seed)
}


dynamic <- function(theta.start.list, mcmc, burnin, thin, seed.list) { # run several chains using different cores of a computer
  chain.list <- mclapply(1:length(theta.start.list),function(ii){
    theta.start <- theta.start.list[[ii]]
    seed <- seed.list[[ii]]
    this.chain <- chain(theta.start = theta.start.list[[ii]], seed = seed,
                        mcmc = mcmc, burnin = burnin, thin = thin)
    return(this.chain)}, 
    mc.cores=min(length(theta.start.list),detectCores()))
  return(chain.list)
}


postthin <- function(object, thin){ # postprocess thinning of the chains
  d1 <- as.mcmc(diag[[1]][seq(1, nrow(diag[[1]]), thin),])
  d2 <- as.mcmc(diag[[2]][seq(1, nrow(diag[[2]]), thin),])
  d3 <- as.mcmc(diag[[3]][seq(1, nrow(diag[[3]]), thin),])
  d <- mcmc.list(d1,d2,d3)
  return(d)
}


postburn <- function(object, burn){ # postprocess burnin of the chains
  d1 <- as.mcmc(diag[[1]][-c(1:burn),])
  d2 <- as.mcmc(diag[[2]][-c(1:burn),])
  d3 <- as.mcmc(diag[[3]][-c(1:burn),])
  d <- mcmc.list(d1,d2,d3)
  return(d)
}


postprocess <- function(object){ # combine the three chains together
  d <- as.mcmc.list(object)
  d1 <- d[[1]][,1:(ncol(d[[1]])-nlegs)]
  d2 <- d[[2]][,1:(ncol(d[[2]])-nlegs)]
  d3 <- d[[3]][,1:(ncol(d[[3]])-nlegs)]
  d <- mcmc.list(d1,d2,d3)
  return(d)
}


w.mean <- function(x,over,year){ # calculate the weighted mean
  party_share <- x$share[x$year==year]/100
  party_score <- over[x$year==year]
  a <- NULL
  for(i in 1:length(party_share)){
    a[i] <- party_share[i]*party_score[i]
  }
  output <- sum(a)
  return(output)
}


laakso <- function(x, year){ # calculate laakso-taagepera's effective number of parties
  count <- x$share[x$year==year]/100
  a <- NULL
  for(i in 1:length(count)){
    a[i] <- count[i]^2
  }
  output <- 1/sum(a)
  return(output)
}


rescale <- function (x, nx1, nx2, minx, maxx){ # rescale the data so as to limit them [minx,maxx]
  nx = nx1 + (nx2 - nx1) * (x - minx)/(maxx - minx)
  return(nx)
}


pol.index <- function(x, over, year){ # calculate the polarization index (Dalton 2008)
  party_share <- x$share[x$year==year]
  party_score <- over[x$year==year]
  system_average <- sum((party_score * party_share)/100)
  a <- NULL
  for(i in 1:length(party_score)){
    a[i] <- ((party_score[i] - system_average)/5)^2
  }
  b <- NULL
  for(i in 1:length(party_score)){
    b[i] <- party_share[i] * a[i]
  }
  output <- sum(b)
  return(sqrt(output))
}


angles <- function(wnom,rc,party1,party2,angle1,angle2) { # calculate bills' cutting lines based on wnominate, parties, and angles
  weight <- (wnom$weights[2])/(wnom$weights[1]) 
  DL1 <- wnom$rollcalls[,7]
  DL2 <- wnom$rollcalls[,8]
  ZM1 <- wnom$rollcalls[,9]
  ZM2 <- wnom$rollcalls[,10]
  YEA1 <- ZM1 - DL1
  YEA2W <- (ZM2 - DL2) * weight
  NAY1 <- ZM1 + DL1
  NAY2W <- (ZM2 + DL2) * weight
  A1 <- NAY1 - YEA1
  A2 <- NAY2W - YEA2W
  ALENGTH <- sqrt(A1*A1 + A2*A2)
  N1W <- A1 / ALENGTH
  N2W <- A2 / ALENGTH
  for (i in 1:nrow(wnom$rollcalls)){
    if (N1W[i] < 0 & !is.na(N2W[i])) N2W[i] <- -N2W[i]
    if (N1W[i] < 0 & !is.na(N1W[i])) N1W[i] <- -N1W[i]
  }
  ws <- N1W*ZM1 + N2W*ZM2*weight
  xws <- ws*N1W
  yws <- ws*N2W
  cut <- as.data.frame(xws+N2W)
  cut[,2] <- yws-N1W
  cut[,3] <- xws-N2W
  cut[,4] <- yws+N1W
  a <- matrix(NA,2,2)
  #a[,1] <- c(wnom$legislators$coord1D[wnom$legislators$party==party1], # use these four lines if party level is employed
  #           wnom$legislators$coord2D[wnom$legislators$party==party1]*weight)
  #a[,2] <- c(wnom$legislators$coord1D[wnom$legislators$party==party2],
  #           wnom$legislators$coord2D[wnom$legislators$party==party2]*weight)
  a[,1] <- c(median(wnom$legislators$coord1D[wnom$legislators$party==  # use these eight lines if individual level is employed
                                               party1][!is.na(wnom$legislators$coord1D[wnom$legislators$party==party1])]),
             median(wnom$legislators$coord2D[wnom$legislators$party==
                                               party1][!is.na(wnom$legislators$coord2D[wnom$legislators$party==party1])]*weight))
  a[,2] <- c(median(wnom$legislators$coord1D[wnom$legislators$party==
                                               party2][!is.na(wnom$legislators$coord1D[wnom$legislators$party==party2])]),
             median(wnom$legislators$coord2D[wnom$legislators$party==
                                               party2][!is.na(wnom$legislators$coord2D[wnom$legislators$party==party2])]*weight))
  for (i in 1:nrow(cut)){
    m1 <- (a[2,2]-a[2,1])/(a[1,2]-a[1,1])
    m2 <- (cut[i,4]-cut[i,2])/(cut[i,3]-cut[i,1])
    cut[i,5] <- atan(m1) - atan(m2)
    cut[i,5] <- cut[i,5]*180/pi
  }
  cut[,5] <- abs(cut[,5])
  for (i in 1:nrow(cut)){
    cut[i,6] <- ifelse(cut[i,5]>=angle1 & cut[i,5]<=angle2, yes = 1, no = 0)
  }
  cut[,6][is.na(cut[,6])] <- 0
  cut[,7] <- colnames(rc$votes)
  return(cut)
}


middle <- function(election, blank){ # calculate the results in the blind spots of electoral years
  parties <- d$party[d$year==(blank+1)]
  party_share <- d$share[d$year==(blank+1)]
  d1 <- d[d$year==blank,]
  d1 <- d1[d1$party %in% parties,]
  d1$share <- party_share
  pi <- NULL
  for(k in 1:(ncol(d1)-5)){
    pi[k] <- pol.index(x = d1, over = d1[,(k+5)], year = blank)
    rm(k)
  }
  m <- NULL
  for(k in 1:(ncol(d1)-5)){
    m[k] <- w.mean(x = d1, over = d1[,(k+5)], year = blank)
    rm(k)
  }
  pi1 <- quantile(pi, probs = c(0.025,0.50,0.975))
  m1 <- quantile(m, probs = c(0.025,0.50,0.975))
  parties <- d$party[d$year==(blank-1)]
  party_share <- d$share[d$year==(blank-1)]
  d1 <- d[d$year==blank,]
  d1 <- d1[d1$party %in% parties,]
  d1$share <- party_share
  pi <- NULL
  for(k in 1:(ncol(d1)-5)){
    pi[k] <- pol.index(x = d1, over = d1[,(k+5)], year = blank)
    rm(k)
  }
  m <- NULL
  for(k in 1:(ncol(d1)-5)){
    m[k] <- w.mean(x = d1, over = d1[,(k+5)], year = blank)
    rm(k)
  }
  pi0 <- quantile(pi, probs = c(0.025,0.50,0.975))
  m0 <- quantile(m, probs = c(0.025,0.50,0.975))
  ratio1 <- ratio[ratio$year==blank,]
  m.final <- (m0*nrow(ratio1[ratio1$datum<=as.Date(election),]) + m1*nrow(ratio1[ratio1$datum>=as.Date(election),]))/
    (nrow(ratio1[ratio1$datum<=as.Date(election),])+nrow(ratio1[ratio1$datum>=as.Date(election),]))
  pi.final <- (pi0*nrow(ratio1[ratio1$datum<=as.Date(election),]) + pi1*nrow(ratio1[ratio1$datum>=as.Date(election),]))/
    (nrow(ratio1[ratio1$datum<=as.Date(election),])+nrow(ratio1[ratio1$datum>=as.Date(election),]))
  output <- c(m.final,pi.final)
  return(output)
}


polar.na <- function(election, blank){ # calculate the polarization index in the blind spots of electoral years
  parties <- d$party[d$year==(blank+1)]
  party_share <- d$share[d$year==(blank+1)]
  d1 <- d[d$year==blank,]
  d1 <- d1[d1$party %in% parties,]
  d1$share <- party_share
  pi1 <- NULL
  for(k in 1:(ncol(d1)-5)){
    pi1[k] <- pol.index(x = d1, over = d1[,(k+5)], year = blank)
    rm(k)
  }
  parties <- d$party[d$year==(blank-1)]
  party_share <- d$share[d$year==(blank-1)]
  d1 <- d[d$year==blank,]
  d1 <- d1[d1$party %in% parties,]
  d1$share <- party_share
  pi2 <- NULL
  for(k in 1:(ncol(d1)-5)){
    pi2[k] <- pol.index(x = d1, over = d1[,(k+5)], year = blank)
    rm(k)
  }
  ratio1 <- ratio[ratio$year==blank,]
  pi.final <- (pi1*nrow(ratio1[ratio1$datum<=as.Date(election),]) + pi2*nrow(ratio1[ratio1$datum>=as.Date(election),]))/
    (nrow(ratio1[ratio1$datum<=as.Date(election),])+nrow(ratio1[ratio1$datum>=as.Date(election),]))
  return(pi.final)
}


mean.na <- function(election, blank){ # calculate the weighted mean in the blind spots of electoral years
  parties <- d$party[d$year==(blank+1)]
  party_share <- d$share[d$year==(blank+1)]
  d1 <- d[d$year==blank,]
  d1 <- d1[d1$party %in% parties,]
  d1$share <- party_share
  m1 <- NULL
  for(k in 1:(ncol(d1)-5)){
    m1[k] <- w.mean(x = d1, over = d1[,(k+5)], year = blank)
    rm(k)
  }
  parties <- d$party[d$year==(blank-1)]
  party_share <- d$share[d$year==(blank-1)]
  d1 <- d[d$year==blank,]
  d1 <- d1[d1$party %in% parties,]
  d1$share <- party_share
  m2 <- NULL
  for(k in 1:(ncol(d1)-5)){
    m2[k] <- w.mean(x = d1, over = d1[,(k+5)], year = blank)
    rm(k)
  }
  ratio1 <- ratio[ratio$year==blank,]
  m.final <- (m1*nrow(ratio1[ratio1$datum<=as.Date(election),]) + m2*nrow(ratio1[ratio1$datum>=as.Date(election),]))/
    (nrow(ratio1[ratio1$datum<=as.Date(election),])+nrow(ratio1[ratio1$datum>=as.Date(election),]))
  return(m.final)
}


compare <- function(dataset,final_dataset){ # calculate differences between main model and robustnes analyses
  dif <- dataset_final - dataset
  dif <- t(apply(dif,1,function(x) quantile(x, probs = c(0.005,0.50,0.995),na.rm=T)))
  dif <- !(dif[,1] <= 0 & dif[,3] >= 0)
  difference <- as.numeric(round(table(dif)[2]/(table(dif)[1] + table(dif)[2])*100,2))
  correlation <- round(cor(FINAL$mean, final_dataset$mean, use = "complete.obs", method = "pearson"),3)
  return(c(difference, correlation))
}




######################
###### START #########
######################

##### total #####
# (dataset of the MPs' names and party affiliations derived from https://www.psp.cz/sqw/hp.sqw?k=1300)

total <- read.csv("data/total.csv", header = TRUE, sep = ",",fileEncoding = "Windows-1250")


##### 2013 #####
# (datasets of roll calls in individual parliamentary terms derived from https://www.psp.cz/sqw/hp.sqw?k=1300)

hl2013h1 = read.csv("data/hl-2013ps/hl2013h1.csv",sep = "|",header = FALSE)
hl2013h2 = read.csv("data/hl-2013ps/hl2013h2.csv",sep = "|",header = FALSE)
hl2013 <- rbind(hl2013h1,hl2013h2)
rm(hl2013h1,hl2013h2)
hl2013$V4 <- NULL
names(hl2013) <- c("id_poslanec","id_hlasovani","vysledek")
hl2013$vysledek <- as.character(hl2013$vysledek)
table(hl2013$vysledek)
hl2013[hl2013 == "A"] <- 1
hl2013[hl2013 == "B"] <- 0
hl2013[hl2013 == "@"] <- NA
hl2013[hl2013 == "K"] <- 0
hl2013[hl2013 == "W"] <- NA
hl2013$vysledek <- as.integer(hl2013$vysledek)
hl2013 <- as.data.frame(cast(hl2013, id_poslanec ~ id_hlasovani))
hl2013 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno","id_osoba")],hl2013,by="id_poslanec")

# recode KDU-CSL as its third appearance in the chamber
levels(hl2013$kandidatka_zkratka) <- c(levels(hl2013$kandidatka_zkratka), "KDU-CSL3")
hl2013$kandidatka_zkratka[hl2013$kandidatka_zkratka=="KDU-CSL"] <- "KDU-CSL3"

# aggregate roll calls on a party level
hl2013_p <- matrix(0, ncol = (ncol(hl2013)-3), nrow = length(getUsedFactorLevels(hl2013[,"kandidatka_zkratka"])))
hl2013_p <- data.frame(hl2013_p)
hl2013_p[,1] <- as.data.frame(getUsedFactorLevels(hl2013[,"kandidatka_zkratka"]))
colnames(hl2013_p) <- c("strana", colnames(hl2013[5:ncol(hl2013)]))
for (i in 5:ncol(hl2013)) {
  for (k in 1:length(getUsedFactorLevels(hl2013[,"kandidatka_zkratka"]))) {
    as.data.frame(table(hl2013[,i][hl2013$kandidatka_zkratka==getUsedFactorLevels(hl2013[,"kandidatka_zkratka"])[k]])) -> a
    a$Var1 <- as.integer(as.character(a$Var1))
    if(nrow(a)==1) a <- rbind(a, c(-10, -10))
    a <- a[order(-a$Freq),]
    ifelse(a[1,2]>a[2,2], yes = a[1,1] -> hl2013_p[k,(i-3)] , no = NA -> hl2013_p[k,(i-3)]) # if there is no majority - NA
  rm(k)}
rm(i,a)}

# information on roll calls
hl2013s = read.csv("data/hl-2013ps/hl2013s.csv",
                   sep = "|",header = FALSE, fileEncoding = "windows-1250")
hl2013s$V18 <- NULL
names(hl2013s) <- c("id_hlasovani","id_organ","schuze","cislo","bod","datum","cas","pro","proti",
                    "zdrzel","nehlasoval","prihlaseno","kvorum","druh_hlasovani","vysledek","nazev_dlouhy","nazev_kratky")
hl2013s$datum <- as.Date(hl2013s$datum, "%d.%m.%Y")


##### 2010 #####

hl2010h1 = read.csv("data/hl-2010ps/hl2010h1.csv",sep = "|",header = FALSE)
hl2010h2 = read.csv("data/hl-2010ps/hl2010h2.csv",sep = "|",header = FALSE)
hl2010 <- rbind(hl2010h1,hl2010h2)
rm(hl2010h1,hl2010h2)
hl2010$V4 <- NULL
names(hl2010) <- c("id_poslanec","id_hlasovani","vysledek")
hl2010$vysledek <- as.character(hl2010$vysledek)
table(hl2010$vysledek)
hl2010[hl2010 == "A"] <- 1
hl2010[hl2010 == "B"] <- 0
hl2010[hl2010 == "C"] <- 0
hl2010[hl2010 == "F"] <- 0
hl2010[hl2010 == "@"] <- NA
hl2010[hl2010 == "W"] <- NA
hl2010$vysledek <- as.integer(hl2010$vysledek)
hl2010 <- hl2010[hl2010$id_hlasovani!=58297,] # exclude the roll call because of a wrong inclusion
hl2010 <- cast(hl2010, id_poslanec ~ id_hlasovani, value = "vysledek")
hl2010 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno","id_osoba")],hl2010,by="id_poslanec")

# aggregate roll calls on a party level
hl2010_p <- matrix(0, ncol = (ncol(hl2010)-3), nrow = length(getUsedFactorLevels(hl2010[,"kandidatka_zkratka"])))
hl2010_p <- data.frame(hl2010_p)
hl2010_p[,1] <- as.data.frame(getUsedFactorLevels(hl2010[,"kandidatka_zkratka"]))
colnames(hl2010_p) <- c("strana", colnames(hl2010[5:ncol(hl2010)]))
for (i in 5:ncol(hl2010)) {
  for (k in 1:length(getUsedFactorLevels(hl2010[,"kandidatka_zkratka"]))) {
    as.data.frame(table(hl2010[,i][hl2010$kandidatka_zkratka==getUsedFactorLevels(hl2010[,"kandidatka_zkratka"])[k]])) -> a
    a$Var1 <- as.integer(as.character(a$Var1))
    if(nrow(a)==1) a <- rbind(a, c(-10, -10))
    a <- a[order(-a$Freq),]
    ifelse(a[1,2]>a[2,2], yes = a[1,1] -> hl2010_p[k,(i-3)] , no = NA -> hl2010_p[k,(i-3)]) # if there is no majority - NA
    rm(k)}
  rm(i,a)}

# information on roll calls
hl2010s = read.csv("data/hl-2010ps/hl2010s.csv",
                   sep = "|",header = FALSE, fileEncoding = "windows-1250")
hl2010s$V18 <- NULL
names(hl2010s) <- c("id_hlasovani","id_organ","schuze","cislo","bod","datum","cas","pro","proti",
                    "zdrzel","nehlasoval","prihlaseno","kvorum","druh_hlasovani","vysledek","nazev_dlouhy","nazev_kratky")
hl2010s$datum <- as.Date(hl2010s$datum, "%d.%m.%Y")


##### 2006 #####

hl2006h1 = read.csv("data/hl-2006ps/hl2006h1.csv",sep = "|",header = FALSE)
hl2006h2 = read.csv("data/hl-2006ps/hl2006h2.csv",sep = "|",header = FALSE)
hl2006 <- rbind(hl2006h1,hl2006h2)
rm(hl2006h1,hl2006h2)
hl2006$V4 <- NULL
names(hl2006) <- c("id_poslanec","id_hlasovani","vysledek")
hl2006$vysledek <- as.character(hl2006$vysledek)
table(hl2006$vysledek)
hl2006[hl2006 == "A"] <- 1
hl2006[hl2006 == "B"] <- 0
hl2006[hl2006 == "C"] <- 0
hl2006[hl2006 == "F"] <- 0
hl2006[hl2006 == "@"] <- NA
hl2006[hl2006 == "N"] <- 0
hl2006$vysledek <- as.integer(hl2006$vysledek)
hl2006 <- cast(hl2006, id_poslanec ~ id_hlasovani, value = "vysledek")
hl2006 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno","id_osoba")],hl2006,by="id_poslanec")

# recode KDU-CSL as its second appearance in the chamber
levels(hl2006$kandidatka_zkratka) <- c(levels(hl2006$kandidatka_zkratka), "KDU-CSL2")
hl2006$kandidatka_zkratka[hl2006$kandidatka_zkratka=="KDU-CSL"] <- "KDU-CSL2"

# aggregate roll calls on a party level
hl2006_p <- matrix(0, ncol = (ncol(hl2006)-3), nrow = length(getUsedFactorLevels(hl2006[,"kandidatka_zkratka"])))
hl2006_p <- data.frame(hl2006_p)
hl2006_p[,1] <- as.data.frame(getUsedFactorLevels(hl2006[,"kandidatka_zkratka"]))
colnames(hl2006_p) <- c("strana", colnames(hl2006[5:ncol(hl2006)]))
for (i in 5:ncol(hl2006)) {
  for (k in 1:length(getUsedFactorLevels(hl2006[,"kandidatka_zkratka"]))) {
    as.data.frame(table(hl2006[,i][hl2006$kandidatka_zkratka==getUsedFactorLevels(hl2006[,"kandidatka_zkratka"])[k]])) -> a
    a$Var1 <- as.integer(as.character(a$Var1))
    if(nrow(a)==1) a <- rbind(a, c(-10, -10))
    a <- a[order(-a$Freq),]
    ifelse(a[1,2]>a[2,2], yes = a[1,1] -> hl2006_p[k,(i-3)] , no = NA -> hl2006_p[k,(i-3)]) # if there is no majority - NA
  rm(k)}
rm(i,a)}

# information on roll calls
hl2006s = read.csv("data/hl-2006ps/hl2006s.csv",
                   sep = "|",header = FALSE, fileEncoding = "windows-1250")
hl2006s$V18 <- NULL
names(hl2006s) <- c("id_hlasovani","id_organ","schuze","cislo","bod","datum","cas","pro","proti",
                    "zdrzel","nehlasoval","prihlaseno","kvorum","druh_hlasovani","vysledek","nazev_dlouhy","nazev_kratky")
hl2006s$datum <- as.Date(hl2006s$datum, "%d.%m.%Y")


##### 2002 #####

hl2002h1 = read.csv("data/hl-2002ps/hl2002h1.csv",sep = "|",header = FALSE)
hl2002h2 = read.csv("data/hl-2002ps/hl2002h2.csv",sep = "|",header = FALSE)
hl2002h3 = read.csv("data/hl-2002ps/hl2002h3.csv",sep = "|",header = FALSE)
hl2002 <- rbind(hl2002h1,hl2002h2,hl2002h3)
rm(hl2002h1,hl2002h2,hl2002h3)
hl2002$V4 <- NULL
names(hl2002) <- c("id_poslanec","id_hlasovani","vysledek")
hl2002$vysledek <- as.character(hl2002$vysledek)
table(hl2002$vysledek)
hl2002[hl2002 == "A"] <- 1
hl2002[hl2002 == "B"] <- 0
hl2002[hl2002 == "C"] <- 0
hl2002[hl2002 == "F"] <- 0
hl2002[hl2002 == "@"] <- NA
hl2002[hl2002 == "N"] <- 0
hl2002$vysledek <- as.integer(hl2002$vysledek)
hl2002 <- cast(hl2002, id_poslanec ~ id_hlasovani)
hl2002 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno","id_osoba")],hl2002,by="id_poslanec")

# potentially split K into both parties KDU-CSL and US-DEU
#koalice <- read.xlsx("data/ctyrkoalice.xlsx",header = T, sheetName = "4koalice")
#koalice$ctyrkoalice <- as.character(koalice$ctyrkoalice) # split K into KDU-CSL and US-DEU
#hl2002$kandidatka_zkratka <- as.character(hl2002$kandidatka_zkratka)
#koalice <- koalice[order(koalice$id_poslanec),]
#hl2002 <- hl2002[order(hl2002$id_poslanec),]
#hl2002$kandidatka_zkratka[hl2002$kandidatka_zkratka=="K"] <- 
#  koalice$ctyrkoalice[koalice$id_poslanec==hl2002$id_poslanec[hl2002$kandidatka_zkratka=="K"]]
#rm(koalice)
#hl2002$kandidatka_zkratka <- as.factor(hl2002$kandidatka_zkratka)

# aggregate roll calls on a party level
hl2002_p <- matrix(0, ncol = (ncol(hl2002)-3), nrow = length(getUsedFactorLevels(hl2002[,"kandidatka_zkratka"])))
hl2002_p <- data.frame(hl2002_p)
hl2002_p[,1] <- as.data.frame(getUsedFactorLevels(hl2002[,"kandidatka_zkratka"]))
colnames(hl2002_p) <- c("strana", colnames(hl2002[5:ncol(hl2002)]))
for (i in 5:ncol(hl2002)) {
  for (k in 1:length(getUsedFactorLevels(hl2002[,"kandidatka_zkratka"]))) {
    as.data.frame(table(hl2002[,i][hl2002$kandidatka_zkratka==getUsedFactorLevels(hl2002[,"kandidatka_zkratka"])[k]])) -> a
    a$Var1 <- as.integer(as.character(a$Var1))
    if(nrow(a)==1) a <- rbind(a, c(-10, -10))
    a <- a[order(-a$Freq),]
    ifelse(a[1,2]>a[2,2], yes = a[1,1] -> hl2002_p[k,(i-3)] , no = NA -> hl2002_p[k,(i-3)]) # if there is no majority - NA
  rm(k)}
rm(i,a)}

# information on roll calls
hl2002s = read.csv("data/hl-2002ps/hl2002s.csv",
                   sep = "|",header = FALSE, quote = "", fileEncoding = "windows-1250")
hl2002s$V18 <- NULL
names(hl2002s) <- c("id_hlasovani","id_organ","schuze","cislo","bod","datum","cas","pro","proti",
                    "zdrzel","nehlasoval","prihlaseno","kvorum","druh_hlasovani","vysledek","nazev_dlouhy","nazev_kratky")
hl2002s$datum <- as.Date(hl2002s$datum, "%d.%m.%Y")


##### 1998 #####

hl1998h1 = read.csv("data/hl-1998ps/hl1998h1.csv",sep = "|",header = FALSE)
hl1998h2 = read.csv("data/hl-1998ps/hl1998h2.csv",sep = "|",header = FALSE)
hl1998h3 = read.csv("data/hl-1998ps/hl1998h3.csv",sep = "|",header = FALSE)
hl1998 <- rbind(hl1998h1,hl1998h2,hl1998h3)
rm(hl1998h1,hl1998h2,hl1998h3)
hl1998$V4 <- NULL
names(hl1998) <- c("id_poslanec","id_hlasovani","vysledek")
hl1998$vysledek <- as.character(hl1998$vysledek)
table(hl1998$vysledek)
hl1998[hl1998 == "A"] <- 1
hl1998[hl1998 == "B"] <- 0
hl1998[hl1998 == "C"] <- 0
hl1998[hl1998 == "F"] <- 0
hl1998[hl1998 == "@"] <- NA
hl1998[hl1998 == "N"] <- 0
hl1998$vysledek <- as.integer(hl1998$vysledek)
hl1998<-cast(hl1998, id_poslanec ~ id_hlasovani)
hl1998 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno","id_osoba")],hl1998,by="id_poslanec")

# aggregate roll calls on a party level
hl1998_p <- matrix(0, ncol = (ncol(hl1998)-3), nrow = length(getUsedFactorLevels(hl1998[,"kandidatka_zkratka"])))
hl1998_p <- data.frame(hl1998_p)
hl1998_p[,1] <- as.data.frame(getUsedFactorLevels(hl1998[,"kandidatka_zkratka"]))
colnames(hl1998_p) <- c("strana", colnames(hl1998[5:ncol(hl1998)]))
for (i in 5:ncol(hl1998)) {
  for (k in 1:length(getUsedFactorLevels(hl1998[,"kandidatka_zkratka"]))) {
    as.data.frame(table(hl1998[,i][hl1998$kandidatka_zkratka==getUsedFactorLevels(hl1998[,"kandidatka_zkratka"])[k]])) -> a
    a$Var1 <- as.integer(as.character(a$Var1))
    if(nrow(a)==1) a <- rbind(a, c(-10, -10))
    a <- a[order(-a$Freq),]
    ifelse(a[1,2]>a[2,2], yes = a[1,1] -> hl1998_p[k,(i-3)] , no = NA -> hl1998_p[k,(i-3)]) # if there is no majority - NA
  rm(k)}
rm(i,a)}

# information on roll calls
hl1998s = read.csv("data/hl-1998ps/hl1998s.csv",
                   sep = "|",header = FALSE, quote = "", fileEncoding = "windows-1250")
hl1998s$V18 <- NULL
names(hl1998s) <- c("id_hlasovani","id_organ","schuze","cislo","bod","datum","cas","pro","proti",
                    "zdrzel","nehlasoval","prihlaseno","kvorum","druh_hlasovani","vysledek","nazev_dlouhy","nazev_kratky")
hl1998s$datum <- as.Date(hl1998s$datum, "%d.%m.%Y")


##### 1996 #####

hl1996 = read.csv("data/hl-1996ps/hl1996h1.csv",sep = "|",header = FALSE)
hl1996$V4 <- NULL
names(hl1996) <- c("id_poslanec","id_hlasovani","vysledek")
hl1996$vysledek <- as.character(hl1996$vysledek)
table(hl1996$vysledek)
hl1996[hl1996 == "A"] <- 1
hl1996[hl1996 == "B"] <- 0
hl1996[hl1996 == "F"] <- 0
hl1996[hl1996 == "@"] <- NA
hl1996$vysledek <- as.integer(hl1996$vysledek)
hl1996<-cast(hl1996, id_poslanec ~ id_hlasovani)
hl1996 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno","id_osoba")],hl1996,by="id_poslanec")

# aggregate roll calls on a party level
hl1996_p <- matrix(0, ncol = (ncol(hl1996)-3), nrow = length(getUsedFactorLevels(hl1996[,"kandidatka_zkratka"])))
hl1996_p <- data.frame(hl1996_p)
hl1996_p[,1] <- as.data.frame(getUsedFactorLevels(hl1996[,"kandidatka_zkratka"]))
colnames(hl1996_p) <- c("strana", colnames(hl1996[5:ncol(hl1996)]))
for (i in 5:ncol(hl1996)) {
  for (k in 1:length(getUsedFactorLevels(hl1996[,"kandidatka_zkratka"]))) {
    as.data.frame(table(hl1996[,i][hl1996$kandidatka_zkratka==getUsedFactorLevels(hl1996[,"kandidatka_zkratka"])[k]])) -> a
    a$Var1 <- as.integer(as.character(a$Var1))
    if(nrow(a)==1) a <- rbind(a, c(-10, -10))
    a <- a[order(-a$Freq),]
    ifelse(a[1,2]>a[2,2], yes = a[1,1] -> hl1996_p[k,(i-3)] , no = NA -> hl1996_p[k,(i-3)]) # if there is no majority - NA
  rm(k)}
rm(i,a)}

# information on roll calls
hl1996s = read.csv("data/hl-1996ps/hl1996s.csv",
                   sep = "|",header = FALSE, quote = "", fileEncoding = "windows-1250")
hl1996s$V18 <- NULL
names(hl1996s) <- c("id_hlasovani","id_organ","schuze","cislo","bod","datum","cas","pro","proti",
                    "zdrzel","nehlasoval","prihlaseno","kvorum","druh_hlasovani","vysledek","nazev_dlouhy","nazev_kratky")
hl1996s$datum <- as.Date(hl1996s$datum, "%d.%m.%Y")


##### 1993 #####

hl1993h1 = read.csv("data/hl-1993ps/hl1993h1.csv",sep = "|",header = FALSE)
hl1993h2 = read.csv("data/hl-1993ps/hl1993h2.csv",sep = "|",header = FALSE)
hl1993 <- rbind(hl1993h1,hl1993h2)
rm(hl1993h1,hl1993h2)
hl1993$V4 <- NULL
names(hl1993) <- c("id_poslanec","id_hlasovani","vysledek")
hl1993$vysledek <- as.character(hl1993$vysledek)
table(hl1993$vysledek)
hl1993[hl1993 == "A"] <- 1
hl1993[hl1993 == "B"] <- 0
hl1993[hl1993 == "C"] <- 0
hl1993[hl1993 == "F"] <- 0
hl1993[hl1993 == "@"] <- NA
hl1993$vysledek <- as.integer(hl1993$vysledek)
hl1993<-cast(hl1993, id_poslanec ~ id_hlasovani)
hl1993 <- merge(total[,c("id_poslanec","kandidatka_zkratka","jmeno","id_osoba")],hl1993,by="id_poslanec")

# recode ODS-KDS as ODS to ensure a continuity
hl1993$kandidatka_zkratka[hl1993$kandidatka_zkratka=="ODS-KDS"] <- "ODS"

# aggregate roll calls on a party level
hl1993_p <- matrix(0, ncol = (ncol(hl1993)-3), nrow = length(getUsedFactorLevels(hl1993[,"kandidatka_zkratka"])))
hl1993_p <- data.frame(hl1993_p)
hl1993_p[,1] <- as.data.frame(getUsedFactorLevels(hl1993[,"kandidatka_zkratka"]))
colnames(hl1993_p) <- c("strana", colnames(hl1993[5:ncol(hl1993)]))
for (i in 5:ncol(hl1993)) {
  for (k in 1:length(getUsedFactorLevels(hl1993[,"kandidatka_zkratka"]))) {
    as.data.frame(table(hl1993[,i][hl1993$kandidatka_zkratka==getUsedFactorLevels(hl1993[,"kandidatka_zkratka"])[k]])) -> a
    a$Var1 <- as.integer(as.character(a$Var1))
    if(nrow(a)==1) a <- rbind(a, c(-10, -10))
    a <- a[order(-a$Freq),]
    ifelse(a[1,2]>a[2,2], yes = a[1,1] -> hl1993_p[k,(i-3)] , no = NA -> hl1993_p[k,(i-3)]) # if there is no majority - NA
  rm(k)}
rm(i,a)}

# information on roll calls
hl1993s = read.csv("data/hl-1993ps/hl1993s.csv",
                   sep = "|",header = FALSE, quote = "", fileEncoding = "windows-1250")
hl1993s$V18 <- NULL
names(hl1993s) <- c("id_hlasovani","id_organ","schuze","cislo","bod","datum","cas","pro","proti",
                    "zdrzel","nehlasoval","prihlaseno","kvorum","druh_hlasovani","vysledek","nazev_dlouhy","nazev_kratky")
hl1993s$datum <- as.Date(hl1993s$datum, "%d.%m.%Y")





##############################################
############## WNOMINATE #####################
##############################################

# Get information on cutting lines based on individual level of MPs or an aggregated party level

##### Individual level #####

load("data/wnom.RData") # load the outputs of the static roll call analysis from rollcall.R


##### Party level #####

# create RC object 2013
party <- matrix(hl2013_p[,1], length(hl2013_p[,1]), 1)
colnames(party) <- "party"
rc2013_p <- rollcall(data = hl2013_p[,2:ncol(hl2013_p)], yea=1, nay=0, missing=NA,
                     legis.names=hl2013_p$strana, legis.data = party,
                     vote.names = colnames(hl2013_p[2:ncol(hl2013_p)]),vote.data = as.matrix(colnames(hl2013_p[2:ncol(hl2013_p)])))
rm(party)
rownames(rc2013_p$votes)
# run the roll call analysis
wnom2013_p <- wnominate(rc2013_p,dims=2,trials=3,polarity=c(4,4),lop = 0.05, minvotes = 100) # ODS used for polarity identification

# create RC object 2010
party <- matrix(hl2010_p[,1], length(hl2010_p[,1]), 1)
colnames(party) <- "party"
rc2010_p <- rollcall(data = hl2010_p[,2:ncol(hl2010_p)], yea=1, nay=0, missing=NA,
                     legis.names=hl2010_p$strana,legis.data = party,
                     vote.names = colnames(hl2010_p[2:ncol(hl2010_p)]),vote.data=as.matrix(colnames(hl2010_p[2:ncol(hl2010_p)])))
rm(party)
rownames(rc2010_p$votes)
# run the roll call analysis
wnom2010_p <- wnominate(rc2010_p,dims=2,trials=3,polarity=c(3,3),lop = 0.05, minvotes = 100) # ODS used for polarity identification

# create RC object 2006
party <- matrix(hl2006_p[,1], length(hl2006_p[,1]), 1)
colnames(party) <- "party"
rc2006_p <- rollcall(data = hl2006_p[,2:ncol(hl2006_p)], yea=1, nay=0, missing=NA,
                     legis.names=hl2006_p$strana,legis.data = party,
                     vote.names = colnames(hl2006_p[2:ncol(hl2006_p)]),vote.data =as.matrix(colnames(hl2006_p[2:ncol(hl2006_p)])))
rm(party)
rownames(rc2006_p$votes)
# run the roll call analysis
wnom2006_p <- wnominate(rc2006_p,dims=2,trials=3,polarity=c(3,3),lop = 0.05, minvotes = 100) # ODS used for polarity identification

# create RC object 2002
party <- matrix(hl2002_p[,1], length(hl2002_p[,1]), 1)
colnames(party) <- "party"
rc2002_p <- rollcall(data = hl2002_p[,2:ncol(hl2002_p)], yea=1, nay=0, missing=NA,
                     legis.names=hl2002_p$strana,legis.data = party,
                     vote.names = colnames(hl2002_p[2:ncol(hl2002_p)]),vote.data =as.matrix(colnames(hl2002_p[2:ncol(hl2002_p)])))
rm(party)
rownames(rc2002_p$votes)
# run the roll call analysis
wnom2002_p <- wnominate(rc2002_p,dims=2,trials=3,polarity=c(4,4),lop = 0.05, minvotes = 100) # ODS used for polarity identification

# create RC object 1998
party <- matrix(hl1998_p[,1], length(hl1998_p[,1]), 1)
colnames(party) <- "party"
rc1998_p <- rollcall(data = hl1998_p[,2:ncol(hl1998_p)], yea=1, nay=0, missing=NA,
                     legis.names=hl1998_p$strana,legis.data = party,
                     vote.names = colnames(hl1998_p[2:ncol(hl1998_p)]),vote.data =as.matrix(colnames(hl1998_p[2:ncol(hl1998_p)])))
rm(party)
rownames(rc1998_p$votes)
# run the roll call analysis
wnom1998_p <- wnominate(rc1998_p,dims=2,trials=3,polarity=c(4,4),lop = 0.05, minvotes = 100) # ODS used for polarity identification

# create RC object 1996
party <- matrix(hl1996_p[,1], length(hl1996_p[,1]), 1)
colnames(party) <- "party"
rc1996_p <- rollcall(data = hl1996_p[,2:ncol(hl1996_p)], yea=1, nay=0, missing=NA,
                     legis.names=hl1996_p$strana,legis.data = party,
                     vote.names = colnames(hl1996_p[2:ncol(hl1996_p)]),vote.data =as.matrix(colnames(hl1996_p[2:ncol(hl1996_p)])))
rm(party)
rownames(rc1996_p$votes)
# run the roll call analysis
wnom1996_p <- wnominate(rc1996_p,dims=2,trials=3,polarity=c(5,5),lop = 0.05, minvotes = 100) # ODS used for polarity identification

# create RC object 1993
party <- matrix(hl1993_p[,1], length(hl1993_p[,1]), 1)
colnames(party) <- "party"
rc1993_p <- rollcall(data = hl1993_p[,2:ncol(hl1993_p)], yea=1, nay=0, missing=NA,
                     legis.names=hl1993_p$strana,legis.data = party,
                     vote.names = colnames(hl1993_p[2:ncol(hl1993_p)]),vote.data =as.matrix(colnames(hl1993_p[2:ncol(hl1993_p)])))
rm(party)
rownames(rc1993_p$votes)
# run the roll call analysis
wnom1993_p <- wnominate(rc1993_p,dims=2,trials=3,polarity=c(7,7),lop = 0.05, minvotes = 100) # ODS used for polarity identification





###############################################
######## Dynamic IRT Model 1993-2017 ##########
###############################################

# adjust settings of the model (e.g. for robustness analyses)
# the main model utilizes the angle = 45, ODS as a right-wing pole, and WNOMINATE cutting lines from individual level

angle <- 45 # choose the angle that limits the space on both sides from the ideal cutting line

level <- list(wnom1993,rc1993,wnom1996,rc1996,wnom1998,rc1998,wnom2002,rc2002,wnom2006,rc2006,wnom2010,rc2010,wnom2013,rc2013) # individual level of static preliminary analyses
#level <- list(wnom1993_p,rc1993_p,wnom1996_p,rc1996_p,wnom1998_p,rc1998_p,wnom2002_p,rc2002_p,wnom2006_p,rc2006_p,wnom2010_p,rc2010_p,wnom2013_p,rc2013_p) # party level of static preliminary analyses

# identify the roll calls that depict the ideological dimension between two parties
cut2013 <- angles(wnom = level[13][[1]], rc = level[14][[1]], party1 = "KSCM", party2 = "ODS", angle1 = 90-angle, angle2 = 90+angle)
cut2010 <- angles(wnom = level[11][[1]], rc = level[12][[1]], party1 = "KSCM", party2 = "ODS", angle1 = 90-angle, angle2 = 90+angle)
cut2006 <- angles(wnom = level[9][[1]], rc = level[10][[1]], party1 = "KSCM", party2 = "ODS", angle1 = 90-angle, angle2 = 90+angle)
cut2002 <- angles(wnom = level[7][[1]], rc = level[8][[1]], party1 = "KSCM", party2 = "ODS", angle1 = 90-angle, angle2 = 90+angle)
cut1998 <- angles(wnom = level[5][[1]], rc = level[6][[1]], party1 = "KSCM", party2 = "ODS", angle1 = 90-angle, angle2 = 90+angle)
cut1996 <- angles(wnom = level[3][[1]], rc = level[4][[1]], party1 = "KSCM", party2 = "ODS", angle1 = 90-angle, angle2 = 90+angle)
cut1993 <- angles(wnom = level[1][[1]], rc = level[2][[1]], party1 = "LB", party2 = "ODS-KDS", angle1 = 90-angle, angle2 = 90+angle)
# for cut1993, use ODS if wnom1993_p and rc1993_p are used (party level), ODS-KDS if wnom1993 and rc1993 (individual level)

# check plots of the selection
plot(wnom2013$legislators$coord1D,wnom2013$legislators$coord2D*(wnom2013$weights[2])/(wnom2013$weights[1]),
     col = c("deepskyblue","darkorange","gold2","red","darkblue","violet","green")[wnom2013$legislators$party],
     xlim = c(-2,2), ylim = c(-2,2), pch = 19)
segments(median(wnom2013$legislators$coord1D[wnom2013$legislators$party=="KSCM"]),
    median(wnom2013$legislators$coord2D[wnom2013$legislators$party=="KSCM"])*(wnom2013$weights[2])/(wnom2013$weights[1]),
    median(wnom2013$legislators$coord1D[wnom2013$legislators$party=="ODS"]),
    median(wnom2013$legislators$coord2D[wnom2013$legislators$party=="ODS"])*(wnom2013$weights[2])/(wnom2013$weights[1]),
    col = "black", lwd = 2)
segments(cut2013[cut2013[,6]==1,1][seq(1, length(cut2013[cut2013[,6]==1,4]), 10)],
    cut2013[cut2013[,6]==1,2][seq(1, length(cut2013[cut2013[,6]==1,4]), 10)],
    cut2013[cut2013[,6]==1,3][seq(1, length(cut2013[cut2013[,6]==1,4]), 10)],
    cut2013[cut2013[,6]==1,4][seq(1, length(cut2013[cut2013[,6]==1,4]), 10)],col= "gray78")
text(0,1.9, label = paste0("originally ", nrow(cut2013)," bills"))
text(0,1.7, label = paste0("selected ", nrow(cut2013[cut2013$V6==1,])," bills"))

# subset datasets (select the roll calls depicting the ideological dimension)
hl2013_p_s <- cbind(hl2013_p[,1],hl2013_p[,2:ncol(hl2013_p)][colnames(hl2013_p[2:ncol(hl2013_p)])
                                                             %in% cut2013[cut2013[,6]==1,7]])
rownames(hl2013_p_s) <- hl2013_p_s[,1]
hl2013_p_s <- hl2013_p_s[,-1]
hl2010_p_s <- cbind(hl2010_p[,1],hl2010_p[,2:ncol(hl2010_p)][colnames(hl2010_p[2:ncol(hl2010_p)])
                                                             %in% cut2010[cut2010[,6]==1,7]])
rownames(hl2010_p_s) <- hl2010_p_s[,1]
hl2010_p_s <- hl2010_p_s[,-1]
hl2006_p_s <- cbind(hl2006_p[,1],hl2006_p[,2:ncol(hl2006_p)][colnames(hl2006_p[2:ncol(hl2006_p)])
                                                             %in% cut2006[cut2006[,6]==1,7]])
rownames(hl2006_p_s) <- hl2006_p_s[,1]
hl2006_p_s <- hl2006_p_s[,-1]
hl2002_p_s <- cbind(hl2002_p[,1],hl2002_p[,2:ncol(hl2002_p)][colnames(hl2002_p[2:ncol(hl2002_p)])
                                                             %in% cut2002[cut2002[,6]==1,7]])
rownames(hl2002_p_s) <- hl2002_p_s[,1]
hl2002_p_s <- hl2002_p_s[,-1]
hl1998_p_s <- cbind(hl1998_p[,1],hl1998_p[,2:ncol(hl1998_p)][colnames(hl1998_p[2:ncol(hl1998_p)])
                                                             %in% cut1998[cut1998[,6]==1,7]])
rownames(hl1998_p_s) <- hl1998_p_s[,1]
hl1998_p_s <- hl1998_p_s[,-1]
hl1996_p_s <- cbind(hl1996_p[,1],hl1996_p[,2:ncol(hl1996_p)][colnames(hl1996_p[2:ncol(hl1996_p)])
                                                             %in% cut1996[cut1996[,6]==1,7]])
rownames(hl1996_p_s) <- hl1996_p_s[,1]
hl1996_p_s <- hl1996_p_s[,-1]
hl1993_p_s <- cbind(hl1993_p[,1],hl1993_p[,2:ncol(hl1993_p)][colnames(hl1993_p[2:ncol(hl1993_p)])
                                                             %in% cut1993[cut1993[,6]==1,7]])
rownames(hl1993_p_s) <- hl1993_p_s[,1]
hl1993_p_s <- hl1993_p_s[,-1]

# merge the datasets together
hl_p_s <- merge(hl2010_p_s,hl2013_p_s, by = 0, all = TRUE)
rownames(hl_p_s) <- hl_p_s[,1]
hl_p_s <- hl_p_s[,-1]
hl_p_s <- merge(hl2006_p_s,hl_p_s, by = 0, all = TRUE)
rownames(hl_p_s) <- hl_p_s[,1]
hl_p_s <- hl_p_s[,-1]
hl_p_s <- merge(hl2002_p_s,hl_p_s, by = 0, all = TRUE)
rownames(hl_p_s) <- hl_p_s[,1]
hl_p_s <- hl_p_s[,-1]
hl_p_s <- merge(hl1998_p_s,hl_p_s, by = 0, all = TRUE)
rownames(hl_p_s) <- hl_p_s[,1]
hl_p_s <- hl_p_s[,-1]
hl_p_s <- merge(hl1996_p_s,hl_p_s, by = 0, all = TRUE)
rownames(hl_p_s) <- hl_p_s[,1]
hl_p_s <- hl_p_s[,-1]
hl_p_s <- merge(hl1993_p_s,hl_p_s, by = 0, all = TRUE)
rownames(hl_p_s) <- hl_p_s[,1]
hl_p_s <- hl_p_s[,-1]

# adjust the dataset and get the time identification of the roll calls
time <- as.data.frame(as.integer(colnames(hl_p_s)))
colnames(time) <- "id_hlasovani"
hls <- rbind(hl1993s,hl1996s,hl1998s,hl2002s,hl2006s,hl2010s,hl2013s)
time <- merge(time, hls[,c("id_hlasovani","datum")])
time$code <- ifelse(time$datum>=as.Date("1993-1-1")&as.Date("1993-12-31")>=time$datum, 1, # 25 years (1993-2017)
             ifelse(time$datum>=as.Date("1994-1-1")&as.Date("1994-12-31")>=time$datum, 2,
             ifelse(time$datum>=as.Date("1995-1-1")&as.Date("1995-12-31")>=time$datum, 3,
             ifelse(time$datum>=as.Date("1996-1-1")&as.Date("1996-12-31")>=time$datum, 4,
             ifelse(time$datum>=as.Date("1997-1-1")&as.Date("1997-12-31")>=time$datum, 5,
             ifelse(time$datum>=as.Date("1998-1-1")&as.Date("1998-12-31")>=time$datum, 6,
             ifelse(time$datum>=as.Date("1999-1-1")&as.Date("1999-12-31")>=time$datum, 7,
             ifelse(time$datum>=as.Date("2000-1-1")&as.Date("2000-12-31")>=time$datum, 8,
             ifelse(time$datum>=as.Date("2001-1-1")&as.Date("2001-12-31")>=time$datum, 9,
             ifelse(time$datum>=as.Date("2002-1-1")&as.Date("2002-12-31")>=time$datum, 10,
             ifelse(time$datum>=as.Date("2003-1-1")&as.Date("2003-12-31")>=time$datum, 11,
             ifelse(time$datum>=as.Date("2004-1-1")&as.Date("2004-12-31")>=time$datum, 12,
             ifelse(time$datum>=as.Date("2005-1-1")&as.Date("2005-12-31")>=time$datum, 13,
             ifelse(time$datum>=as.Date("2006-1-1")&as.Date("2006-12-31")>=time$datum, 14,
             ifelse(time$datum>=as.Date("2007-1-1")&as.Date("2007-12-31")>=time$datum, 15,
             ifelse(time$datum>=as.Date("2008-1-1")&as.Date("2008-12-31")>=time$datum, 16,
             ifelse(time$datum>=as.Date("2009-1-1")&as.Date("2009-12-31")>=time$datum, 17,
             ifelse(time$datum>=as.Date("2010-1-1")&as.Date("2010-12-31")>=time$datum, 18,
             ifelse(time$datum>=as.Date("2011-1-1")&as.Date("2011-12-31")>=time$datum, 19,
             ifelse(time$datum>=as.Date("2012-1-1")&as.Date("2012-12-31")>=time$datum, 20,
             ifelse(time$datum>=as.Date("2013-1-1")&as.Date("2013-12-31")>=time$datum, 21,
             ifelse(time$datum>=as.Date("2014-1-1")&as.Date("2014-12-31")>=time$datum, 22,
             ifelse(time$datum>=as.Date("2015-1-1")&as.Date("2015-12-31")>=time$datum, 23,
             ifelse(time$datum>=as.Date("2016-1-1")&as.Date("2016-12-31")>=time$datum, 24,
             ifelse(time$datum>=as.Date("2017-1-1")&as.Date("2017-12-31")>=time$datum, 25, 99)))))))))))))))))))))))))
ratio <- time # save roll calls time distribution in order to later identify ratio of roll calls in electoral years
ratio$year <- ratio$code+1992
hl_p_s <- as.data.frame(t(hl_p_s))
hl_p_s <- cbind(as.integer(rownames(hl_p_s)),hl_p_s)
colnames(hl_p_s)[1] <- "id_hlasovani"
hl_p_s <- merge(hl_p_s,time, by = "id_hlasovani")
hl_p_s <- hl_p_s[order(hl_p_s$datum),]
time <- hl_p_s$code
row.names(hl_p_s) <- hl_p_s$id_hlasovani
hl_p_s <- hl_p_s[,2:(ncol(hl_p_s)-2)]
hl_p_s <- as.data.frame(t(hl_p_s))

# set starting values
start.list <- list(rep(0, nrow(hl_p_s)),rep(0, nrow(hl_p_s)),rep(0, nrow(hl_p_s)))
# starting values are set to 0 except CSSD and ODS, which were in the chamber for the whole time (1993-2017)
start.list[[1]][2] <- -3 # CSSD # starting values for the first chain
start.list[[1]][12] <- 3 # ODS
start.list[[2]][2] <- -1 # CSSD # starting values for the second chain
start.list[[2]][12] <- 1 # ODS
start.list[[3]][2] <- -2 # CSSD # starting values for the third chain
start.list[[3]][12] <- 2 # ODS

# set seeds
seed.list <- list(1,2,3) # different seeds for the first, second, and the third chain

# reducing the dataset because of computational demands
hl_p_s <- hl_p_s[,seq(1, ncol(hl_p_s), 3)] # every third roll call is selected
time <- time[seq(1, length(time), 3)]
table(time) # check the roll calls' distribution

# get the number of legislators and the number of bills
nlegs <- nrow(hl_p_s)
nbills <- ncol(hl_p_s)
print(c(nlegs,nbills))

# get the number of utilized roll calls
table1 <- rbind(hl_p_s,time)
table <- matrix(0,nrow = (nrow(table1)-1), ncol = length(unique(time)))
table <- as.data.frame(table, row.names = row.names(table1)[1:(nrow(table1)-1)])
colnames(table) <- unique(time)
row.names(table1)[nrow(table1)] <- "time"
for(k in 1:(nrow(table1)-1)){
  help <- t(as.matrix(table(unlist(table1[nrow(table1),!is.na(table1[k,])]))))
  for(i in 1:length(help)){
    table[k,colnames(help)[i]] <- help[i]
    rm(i)
  }
  rm(k)
}
table <- rbind(table,c(table(time),0))
rm(table1)
table$total <- apply(table,1, sum)
colnames(table) <- c(1993:2017,"total")
rownames(table)[nrow(table)] <- "Maximum"
table <- t(table)
table[table==0] <- ""
write.xlsx(table,file = "data/bills_distribution.xlsx") # output of roll calls' distribution by parties and years





#############################################################################################
####################################### DYNAMIC ANALYSIS ####################################
#############################################################################################

# FINAL MODEL ###
#==================================================================================================================
# selection of 90 degrees interval, every 3rd roll call, 15 000 iterations, 0 burnin, thinning of 1
#a90_r3_i15000_b0_t1 <- dynamic(theta.start.list = start.list, seed.list = seed.list,
#                               mcmc = 15000, burnin = 0, thin = 1) # almost 100 hours of computation time
#save(list = ls(all = TRUE), file = "mcmc.RData")
#save(a90_r3_i15000_b0_t1, file = "a90_r3_i15000_b0_t1.RData")
#==================================================================================================================

### Robustnes Analyses ###
#=============================================================================================
# before each robustness analyses be sure that a right hl_p_s data are employed (based on model specification)
#a90_r6_i15000_b0_t1_top <- dynamic(theta.start.list = start.list, seed.list = seed.list, # TOP 09 as right (instead of ODS)
#                               mcmc = 15000, burnin = 0, thin = 1) # 10 hours
#a90_r6_i15000_b0_t1_p <- dynamic(theta.start.list = start.list, seed.list = seed.list, # cutting lines from party level (instead of individual one)
#                               mcmc = 15000, burnin = 0, thin = 1) # 5 hours
#a80_r6_i15000_b0_t1 <- dynamic(theta.start.list = start.list, seed.list = seed.list, # 80 degrees
#                               mcmc = 15000, burnin = 0, thin = 1) # 8 hours
#a70_r6_i15000_b0_t1 <- dynamic(theta.start.list = start.list, seed.list = seed.list, # 70 degrees
#                                mcmc = 15000, burnin = 0, thin = 1) # 6 hours
#a60_r6_i15000_b0_t1 <- dynamic(theta.start.list = start.list, seed.list = seed.list, # 60 degrees
#                                mcmc = 15000, burnin = 0, thin = 1) # 5 hours
#a50_r4_i15000_b0_t1 <- dynamic(theta.start.list = start.list, seed.list = seed.list, # 50 degrees
#                                mcmc = 15000, burnin = 0, thin = 1) # 7 hours
#a100_r8_i15000_b0_t1 <- dynamic(theta.start.list = start.list, seed.list = seed.list, # 100 degrees
#                                mcmc = 15000, burnin = 0, thin = 1) # 7 hours
#a110_r8_i15000_b0_t1 <- dynamic(theta.start.list = start.list, seed.list = seed.list, # 110 degrees
#                                mcmc = 15000, burnin = 0, thin = 1) # 7 hours
#a120_r8_i15000_b0_t1 <- dynamic(theta.start.list = start.list, seed.list = seed.list, # 120 degrees
#                                mcmc = 15000, burnin = 0, thin = 1) # 8 hours
#a130_r8_i15000_b0_t1 <- dynamic(theta.start.list = start.list, seed.list = seed.list, # 130 degrees
#                                mcmc = 15000, burnin = 0, thin = 1) # 9 hours
#=============================================================================================
save(list = ls(all = TRUE), file= "mcmc.RData")


####################################################
########## Transformation and Diagnostics ##########
####################################################

# change the object in order to get the results of different models
diag <- postprocess(object = a90_r3_i15000_b0_t1) # creating an mcmc.list without taus

plot(diag)

diag <- postburn(diag, burn = 5000) # postprocess burnin based on convergence of the chains

plot(diag)

diag <- postthin(diag, thin = 10) # postprocess thinning

# Gelman and Rubin's convergence diagnostic (done on mcmc.list)
gelman.diag(diag, confidence = 0.95, transform=FALSE, autoburnin=FALSE, multivariate = T)
gelman.plot(diag, bin.width=1000, max.bins=50, confidence=0.95, autoburnin = TRUE)

# Geweke's diagnostic (comparing first 10% of the chains with tha last 50% with z-score)
par(mfrow=c(3,1))
hist(geweke.diag(diag, frac1=0.1, frac2=0.5)[[1]]$z) 
hist(geweke.diag(diag, frac1=0.1, frac2=0.5)[[2]]$z)
hist(geweke.diag(diag, frac1=0.1, frac2=0.5)[[3]]$z)

diag <- combine.mcmc(diag) # combine together as single mcmc object

# check autocorrelation among iterations 
par(mfrow=c(1,1))
hist(autocorr.diag(diag, lags = c(50), relative=TRUE))
sort(autocorr.diag(diag, lags = c(50), relative=TRUE), decreasing = TRUE)





######################################################
######## Transform the results into a dataset ########
######################################################

data <- as.matrix(diag)
data <- as.data.frame(t(data))
data <- as.data.frame(t(apply(data,1,function(x) quantile(x, probs = c(0.005,0.025,0.05,0.25,0.50,0.75,0.95,0.975,0.995),na.rm=T))))
data$t <- 0
for (i in 1:nrow(data)){
  data$t[i] <- tail(strsplit(row.names(data),split=".t")[[i]],1)
  rm(i)
}
data <- data[!grepl("tau", data$t),]
data$t <- as.integer(data$t)
data$party <- read.table(text = row.names(data), sep = ".", as.is = TRUE)$V2
year <- matrix(data = c(1:max(data$t),1993:(1993+max(data$t)-1)), nrow =  max(data$t), ncol = 2)
data$year <- year[data$t,2]
save(data, file= "data/correlations/dynamic_model.RData") # save the results for a later analysis of the correlations with other methods of revealing spatial positions
rm(year)





pdf("figures/dynamic_final.pdf", width = 17.3, height = 10) # make a final plot
par(mfrow=c(1,1), mar=c(4.5,4.5,1,1), family="serif",ps=14)
plot(c(1993:2017),data[data$party=="CSSD",5],type = "l",col = "white",
     ylab = expression(paste("Ideal Point (",italic(theta['p,t']),")")),
     xlab = expression(paste("Year (",italic(t),")")), 
     ylim = c(-7,7), xlim = c(1992,2017), xaxt = "n", las = 2)
axis(1, at = seq(1993,2017, by = 1), las = 1)
axis(2, at = seq(-10,10, by = 1), las = 1, label = F)
abline(h=c(-10,-5,0,5,10), lty = 'solid', col = "gray90")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(data[data$party=="CSSD",2],
      rev(data[data$party=="CSSD",8])), col = adjustcolor("darkorange1",alpha = 0.3),  border = FALSE)
polygon(c(c(1993:2017),rev(c(1993:2017))),c(data[data$party=="ODS",2],
                rev(data[data$party=="ODS",8])), col = adjustcolor("darkblue",alpha = 0.3),  border = FALSE)
polygon(c(c(1996:2017),rev(c(1996:2017))),c(data[data$party=="KSCM",2],
                rev(data[data$party=="KSCM",8])), col = adjustcolor("red2",alpha = 0.3),  border = FALSE)
polygon(c(c(1993:1998),rev(c(1993:1998))),c(data[data$party=="SPR-RSC",2],
                rev(data[data$party=="SPR-RSC",8])), col = adjustcolor("black",alpha = 0.3),  border = FALSE)
polygon(c(c(1993:1996),rev(c(1993:1996))),c(data[data$party=="LB",2],
                rev(data[data$party=="LB",8])), col = adjustcolor("red",alpha = 0.3),  border = FALSE)
polygon(c(c(1993:1998),rev(c(1993:1998))),c(data[data$party=="ODA",2],
                rev(data[data$party=="ODA",8])), col = adjustcolor("deepskyblue2",alpha = 0.3),  border = FALSE)
polygon(c(c(1993:2002),rev(c(1993:2002))),c(data[data$party=="KDU-CSL",2],
                rev(data[data$party=="KDU-CSL",8])), col = adjustcolor("gold2",alpha = 0.3),  border = FALSE)
polygon(c(c(2002:2006),rev(c(2002:2006))),c(data[data$party=="K",2],
                rev(data[data$party=="K",8])), col = adjustcolor("gold2",alpha = 0.3),  border = FALSE)
polygon(c(c(2006:2010),rev(c(2006:2010))),c(data[data$party=="SZ",2],
                rev(data[data$party=="SZ",8])), col = adjustcolor("darkgreen",alpha = 0.3),  border = FALSE)
polygon(c(c(2006:2010),rev(c(2006:2010))),c(data[data$party=="KDU-CSL2",2],
                rev(data[data$party=="KDU-CSL2",8])), col = adjustcolor("gold2",alpha = 0.3),  border = FALSE)
polygon(c(c(2013:2017),rev(c(2013:2017))),c(data[data$party=="KDU-CSL3",2],
                rev(data[data$party=="KDU-CSL3",8])), col = adjustcolor("gold2",alpha = 0.3),  border = FALSE)
polygon(c(c(2010:2017),rev(c(2010:2017))),c(data[data$party=="TOP09",2],
                rev(data[data$party=="TOP09",8])), col = adjustcolor("darkmagenta",alpha = 0.3),  border = FALSE)
polygon(c(c(2013:2017),rev(c(2013:2017))),c(data[data$party=="ANO2011",2],
                rev(data[data$party=="ANO2011",8])), col = adjustcolor("dodgerblue",alpha = 0.3),  border = FALSE)
polygon(c(c(2013:2017),rev(c(2013:2017))),c(data[data$party=="USVIT",2],
                rev(data[data$party=="USVIT",8])), col = adjustcolor("limegreen",alpha = 0.3),  border = FALSE)
polygon(c(c(2010:2013),rev(c(2010:2013))),c(data[data$party=="VV",2],
                rev(data[data$party=="VV",8])), col = adjustcolor("deepskyblue",alpha = 0.3),  border = FALSE)
polygon(c(c(1998:2002),rev(c(1998:2002))),c(data[data$party=="US",2],
                rev(data[data$party=="US",8])), col = adjustcolor("deepskyblue1",alpha = 0.3),  border = FALSE)
polygon(c(c(1993:1996),rev(c(1993:1996))),c(data[data$party=="LSU",2],
                rev(data[data$party=="LSU",8])), col = adjustcolor("green3",alpha = 0.3),  border = FALSE)
polygon(c(c(1993:1996),rev(c(1993:1996))),c(data[data$party=="HSD-SMS",2],
                rev(data[data$party=="HSD-SMS",8])), col = adjustcolor("mediumorchid",alpha = 0.3),  border = FALSE)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
lines(c(1993:2017), data[data$party=="CSSD",5], type = "l", col = "darkorange1",lw=2)
lines(c(1993:2017), data[data$party=="ODS",5], type = "l", col = "darkblue",lw=2)
lines(c(1996:2017), data[data$party=="KSCM",5], type = "l", col = "red",lw=2)
lines(c(1993:1996), data[data$party=="LB",5], type = "l", col = "red2",lw=2)
lines(c(1993:1998), data[data$party=="SPR-RSC",5], type = "l", col = "black",lw=2)
lines(c(1993:1998), data[data$party=="ODA",5], type = "l", col = "deepskyblue2",lw=2)
lines(c(1993:2002), data[data$party=="KDU-CSL",5], type = "l", col = "gold2",lw=2)
lines(c(2002:2006), data[data$party=="K",5], type = "l", col = "gold2",lw=2)
lines(c(2006:2010), data[data$party=="KDU-CSL2",5], type = "l", col = "gold2",lw=2)
lines(c(2013:2017), data[data$party=="KDU-CSL3",5], type = "l", col = "gold2",lw=2)
lines(c(2010:2017), data[data$party=="TOP09",5], type = "l", col = "darkmagenta",lw=2)
lines(c(2013:2017), data[data$party=="ANO2011",5], type = "l", col = "dodgerblue",lw=2)
lines(c(2013:2017), data[data$party=="USVIT",5], type = "l", col = "limegreen",lw=2)
lines(c(2010:2013), data[data$party=="VV",5], type = "l", col = "deepskyblue",lw=2)
lines(c(2006:2010), data[data$party=="SZ",5], type = "l", col = "darkgreen",lw=2)
lines(c(1998:2002), data[data$party=="US",5], type = "l", col = "deepskyblue1",lw=2)
lines(c(1993:1996), data[data$party=="LSU",5], type = "l", col = "green3",lw=2)
lines(c(1993:1996), data[data$party=="HSD-SMS",5], type = "l", col = "mediumorchid",lw=2)
text(x=1993,y=-0.1 + data[data$party=="CSSD",5][1],label="CSSD",pos=2,cex=1,col = "darkorange1")
text(x=1993,y=data[data$party=="ODS",5][1],label="ODS-KDS",pos=2,cex=1,col = "darkblue")
text(x=1996,y=0.09 + data[data$party=="ODS",5][4],label="ODS",pos=2,cex=1,col = "darkblue")
text(x=1996,y=data[data$party=="KSCM",5][1],label="KSCM",pos=2,cex=1,col = "red2")
text(x=1993,y=data[data$party=="LB",5][1],label="LB",pos=2,cex=1,col = "red")
text(x=1993,y=data[data$party=="SPR-RSC",5][1],label="SPR-RSC",pos=2,cex=1,col = "black")
text(x=1993,y=0.13 + data[data$party=="ODA",5][1],label="ODA",pos=2,cex=1,col = "deepskyblue2")
text(x=1993,y=-0.13 + data[data$party=="KDU-CSL",5][1],label="KDU-CSL",pos=2,cex=1,col = "gold2")
text(x=2002,y=data[data$party=="K",5][1],label="K",pos=2,cex=1,col = "gold2")
text(x=2006,y=0.05 + data[data$party=="KDU-CSL2",5][1],label="KDU-CSL",pos=2,cex=1,col = "gold2")
text(x=2013,y=data[data$party=="KDU-CSL3",5][1],label="KDU-CSL",pos=2,cex=1,col = "gold2")
text(x=2010,y=data[data$party=="TOP09",5][1],label="TOP09",pos=2,cex=1,col = "darkmagenta")
text(x=2013,y=data[data$party=="ANO2011",5][1],label="ANO",pos=2,cex=1,col = "dodgerblue")
text(x=2013,y=-0.05 + data[data$party=="USVIT",5][1],label="USVIT",pos=2,cex=1,col = "limegreen")
text(x=2010,y=data[data$party=="VV",5][1],label="VV",pos=2,cex=1,col = "deepskyblue")
text(x=2006,y=-0.05 + data[data$party=="SZ",5][1],label="SZ",pos=2,cex=1,col = "darkgreen")
text(x=1998,y=data[data$party=="US",5][1],label="US",pos=2,cex=1,col = "deepskyblue1")
text(x=1993,y=data[data$party=="LSU",5][1],label="LSU",pos=2,cex=1,col = "green3")
text(x=1993,y=0.15 + data[data$party=="HSD-SMS",5][1],label="HSD-SMS",pos=2,cex=1,col = "mediumorchid")
dev.off()





#####################################
######## Polarization Index #########
#####################################

# load a dataset about mandates held by particular parties (derived from http://volby.cz)
mandates <- read.xlsx("data/mandates.xlsx",header = T, sheetName = "Sheet1")
mandates <- merge(mandates,data[,c("50%","party","year")],by = c("party","year"))
#mandates <- mandates[!is.na(mandates$M),]
colnames(mandates)[4] <- "mean"
mandates$share <- mandates$M/200*100
mandates$mean_rescale <- rescale(mandates$mean, 0, 10, min(mandates$mean), max(mandates$mean)) # rescale the data so as to be [0,10]

analysis <- as.data.frame(sort(unique(mandates$year))) 
colnames(analysis) <- "year"
for(i in 1:length(unique(mandates$year))){ # calculate polarization index
  analysis[i,2] <- pol.index(x = mandates, over = mandates$mean_rescale, year = sort(unique(mandates$year))[i])
  rm(i)
}
colnames(analysis)[2] <- "pi"
analysis$n <- NA
analysis$n <- ifelse(analysis$year==2014 | analysis$year==2015 | analysis$year==2016 | analysis$year==2017, yes = 7, no = analysis$n)
analysis$n <- ifelse(analysis$year==2011 | analysis$year==2012, yes = 5, no = analysis$n)
analysis$n <- ifelse(analysis$year==2007 | analysis$year==2008 | analysis$year==2009, yes = 5, no = analysis$n)
analysis$n <- ifelse(analysis$year==2003 | analysis$year==2004 | analysis$year==2005, yes = 4, no = analysis$n)
analysis$n <- ifelse(analysis$year==1999 | analysis$year==2000 | analysis$year==2001, yes = 5, no = analysis$n)
analysis$n <- ifelse(analysis$year==1997, yes = 6, no = analysis$n)
analysis$n <- ifelse(analysis$year==1993 | analysis$year==1994 | analysis$year==1995, yes = 8, no = analysis$n)
analysis$lt <- NA
for(i in 1:length(unique(mandates$year))){ # calculate laakso-taagepera's effective number of parties
  analysis[i,4] <- laakso(x = mandates, year = sort(unique(mandates$year))[i])
  rm(i)
}
analysis$w.mean <- NA
for(i in 1:length(unique(mandates$year))){ # calculate weighted mean
  analysis[i,5] <- w.mean(x = mandates, over = mandates$mean_rescale, year = sort(unique(mandates$year))[i])
  rm(i)
}



# calculation with confidence intervals
d <- as.matrix(diag)
d <- as.data.frame(t(d))
d <- d[!grepl("tau", row.names(d)),]
d <- rescale(d, 0, 10, min(d), max(d))
d$t <- 0
for (i in 1:nrow(d)){
  d$t[i] <- tail(strsplit(row.names(d),split=".t")[[i]],1)
  rm(i)
}
d$t <- as.integer(d$t)
d$party <- read.table(text = row.names(d), sep = ".", as.is = TRUE)$V2
year <- matrix(c(1:max(d$t),1993:(1993+max(d$t)-1)), nrow =  max(d$t), ncol = 2)
d$year <- year[d$t,2]
rm(year)
d <- merge(d,mandates[,c("party","year","M","share")], by = c("party","year"))
d <- d[,c(1,2,ncol(d),ncol(d)-1,ncol(d)-2,3:(ncol(d)-3))]

mean <- matrix(NA, nrow = length(unique(d$year)), ncol = (ncol(d)-5), dimnames = list(sort(unique(d$year)))) # calculate weighted mean
for(k in 1:(ncol(d)-5)){
  for(i in 1:length(unique(d$year))){
    mean[i,k] <- w.mean(x = d, over = d[,(k+5)], year = sort(unique(d$year))[i])
    rm(i)
  }
  rm(k)
}
# filling the blank spots of the electoral years
mean[rownames(mean)==2013,] <- mean.na(election = "2013-10-27", blank = 2013)
mean[rownames(mean)==2010,] <- mean.na(election = "2010-05-30", blank = 2010)
mean[rownames(mean)==2006,] <- mean.na(election = "2006-06-04", blank = 2006)
mean[rownames(mean)==2002,] <- mean.na(election = "2002-06-16", blank = 2002)
mean[rownames(mean)==1998,] <- mean.na(election = "1998-06-21", blank = 1998)
mean[rownames(mean)==1996,] <- mean.na(election = "1996-06-02", blank = 1996)
final <- as.data.frame(t(apply(mean,1,function(x) quantile(x, probs = c(0.025,0.50,0.975),na.rm=T))))
colnames(final) <- c("mean.cilow","mean","mean.cihigh")

polar <- matrix(NA, nrow = length(unique(d$year)), ncol = (ncol(d)-5), dimnames = list(sort(unique(d$year)))) # calculate polarization index
for(k in 1:(ncol(d)-5)){
  for(i in 1:length(unique(d$year))){
    polar[i,k] <- pol.index(x = d, over = d[,(k+5)], year = sort(unique(d$year))[i])
    rm(i)
  }
  rm(k)
}
# filling the blank spots of the electoral years
polar[rownames(polar)==2013,] <- polar.na(election = "2013-10-27", blank = 2013)
polar[rownames(polar)==2010,] <- polar.na(election = "2010-05-30", blank = 2010)
polar[rownames(polar)==2006,] <- polar.na(election = "2006-06-04", blank = 2006)
polar[rownames(polar)==2002,] <- polar.na(election = "2002-06-16", blank = 2002)
polar[rownames(polar)==1998,] <- polar.na(election = "1998-06-21", blank = 1998)
polar[rownames(polar)==1996,] <- polar.na(election = "1996-06-02", blank = 1996)
final1 <- as.data.frame(t(apply(polar,1,function(x) quantile(x, probs = c(0.025,0.50,0.975),na.rm=T))))
colnames(final1) <- c("pi.cilow","pi","pi.cihigh")
final <- cbind(final, final1, analysis$n, analysis$lt)
rm(final1)
colnames(final)[7:8] <- c("number","laakso")
polar_robust <- polar # save the results of the dynamic model for a calculation of the first differences below - DO NOT DO THIS for robustness analyses
dif <- polar - polar_robust # calculating first difference of the robustness analyses
dif <- t(apply(dif,1,function(x) quantile(x, probs = c(0.025,0.50,0.975),na.rm=T)))
dif <- !(dif[,1] <= 0 & dif[,3] >= 0)
final <- cbind(final,dif)
rm(mean,polar)


# save different "final"s for the result of the dynamic model and its robustness analyses
FINAL <- final # original final result of the dynamic model
#final_top <- final # robustness analysis with TOP09 as the right-wing party instead of ODS
#final_p <- final # robustness analysis with selection of roll calls based on aggregated party level
#final_80 <- final # robustness analysis with selection of roll calls based on 80 degrees interval
#final_70 <- final # robustness analysis with selection of roll calls based on 70 degrees interval
#final_60 <- final # robustness analysis with selection of roll calls based on 60 degrees interval
#final_50 <- final # robustness analysis with selection of roll calls based on 50 degrees interval
#final_100 <- final # robustness analysis with selection of roll calls based on 100 degrees interval
#final_110 <- final # robustness analysis with selection of roll calls based on 110 degrees interval
#final_120 <- final # robustness analysis with selection of roll calls based on 120 degrees interval
#final_130 <- final # robustness analysis with selection of roll calls based on 130 degrees interval

# load the dataset on volatility in the Czech Republic
# source: Linek, L. (2014). Čistá a celková volební volatilita v Česku v letech 1990-2013. Acta Politologica, 6(1). p. 27)
volatility <- read.xlsx("data/mandates.xlsx",header = T, sheetName = "volatility")

# calculate the correlation between the final results of the polarization and effective number of parties
cor(FINAL$pi, FINAL$laakso, use = "complete.obs", method = "pearson")
cor(FINAL$pi, FINAL$laakso, use = "complete.obs", method = "pearson")

cor(final_100$pi, final_100$number, use = "complete.obs", method = "pearson")

# calculate differences between the main model's results and the results of robustness analyses
d <- as.matrix(diag)
d <- as.data.frame(t(d))
#dataset_final <- d
#dataset_top <- d # robustness analysis with TOP09 as the right-wing party instead of ODS
#dataset_p <- d # robustness analysis with selection of roll calls based on aggregated party level
#dataset_80 <- d # robustness analysis with selection of roll calls based on 80 degrees interval
#dataset_70 <- d # robustness analysis with selection of roll calls based on 70 degrees interval
#dataset_60 <- d # robustness analysis with selection of roll calls based on 60 degrees interval
#dataset_50 <- d # robustness analysis with selection of roll calls based on 50 degrees interval
#dataset_100 <- d # robustness analysis with selection of roll calls based on 100 degrees interval
#dataset_110 <- d # robustness analysis with selection of roll calls based on 110 degrees interval
#dataset_120 <- d # robustness analysis with selection of roll calls based on 120 degrees interval
#dataset_130 <- d # robustness analysis with selection of roll calls based on 130 degrees interval


# create a dataset of comparisons
comparison <- as.data.frame(matrix(0,10,3))
comparison[,1] <- c("party","top","80","70","60","50","100","110","120","130")
comparison[1,2:3] <- compare(dataset = dataset_p, final_dataset = final_p)
comparison[2,2:3] <- compare(dataset = dataset_top, final_dataset = final_top)
comparison[3,2:3] <- compare(dataset = dataset_80, final_dataset = final_80)
comparison[4,2:3] <- compare(dataset = dataset_70, final_dataset = final_70)
comparison[5,2:3] <- compare(dataset = dataset_60, final_dataset = final_60)
comparison[6,2:3] <- compare(dataset = dataset_50, final_dataset = final_50)
comparison[7,2:3] <- compare(dataset = dataset_100, final_dataset = final_100)
comparison[8,2:3] <- compare(dataset = dataset_110, final_dataset = final_110)
comparison[9,2:3] <- compare(dataset = dataset_120, final_dataset = final_120)
comparison[10,2:3] <- compare(dataset = dataset_130, final_dataset = final_130)




##################################
###### FIGURES ###################
##################################


pdf("figures/null.pdf", width = 10, height = 4) # make a plot of the null hypothesis
par(mfrow=c(1,2), mar=c(5,3,3,0.5), family="serif",ps=13)
plot(c(1993:2013),type = "o",col = "white", ylab = "", xlab = "Year", 
     ylim = c(0,10), xlim = c(1993,2013), xaxt = "n", main = "a) Number of Parties", font.main = 1, las = 2)
axis(1, at = c(1993:2013), las = 1, labels = F)
axis(1, at = c(1993,1996,1998,2002,2006,2010,2013), las = 1, labels = T)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
lines(volatility$year, volatility$N, type = "l", col = "black",lw=2,pch=19)
lines(volatility$year, volatility$laakso, type = "l", col = "black",lw=2,pch=19, lty = "dotdash")
legend("topright", legend = c("Number of parties","Effective number of parties"),
       col=c("black", "black"),lty = c("solid","dotdash"), lw = 2, cex = 1, bg = "white") 

plot(c(1993:2013),type = "o",col = "white", ylab = "", xlab = "Year", 
     ylim = c(0,100), xlim = c(1993,2013), xaxt = "n", main = "b) Electoral Volatility", font.main = 1, las = 2)
axis(1, at = c(1993:2013), las = 1, labels = F)
axis(1, at = c(1993,1996,1998,2002,2006,2010,2013), las = 1, labels = T)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
abline(h=c(0,20,40,60,80,100), lty = 'solid', col = "gray90")
lines(volatility$year, volatility$Pedersen, type = "l", col = "black",lw=2,pch=19)
lines(volatility$year, volatility$typeA, type = "l", col = "black",lw=2,pch=19, lty = "dotdash")
legend("topright", legend = c("Pedersen Index","Type A Volatility"),
       col=c("black", "black"),lty = c("solid","dotdash"), lw = 2, cex = 1, bg = "white") 
dev.off()





pdf("figures/final.pdf", width = 10, height = 8)
par(mfrow=c(2,2),family="serif", cex = 1,ps=14) # make a plot of the final results compared to the null ypothesis
plot(c(1993:2017),type = "o",col = "white", ylab = "Index", xlab = "Year",
     ylim = c(0,10), xlim = c(1993,2017), xaxt = "n", font.main = 1, main = expression(paste("a) ",H[0]," - Number of Parties")))
axis(1, at = c(1993,1996,1998,2002,2006,2010,2013,2017), las = 1, labels = F)
axis(1, at = c(1993,1996,1998,2002,2006,2010,2013,2017), las = 1, labels = T)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
lines(volatility$year, volatility$N, type = "l", col = "black",lw=2,pch=19)
lines(volatility$year, volatility$laakso, type = "l", col = "black",lw=2,pch=19, lty = "dotdash")
legend("topright", legend = c("Number of Parties","Effective Number of Parties"),
       col=c("black", "black"),lty = c("solid","dotdash"), lw = 2, cex = 1, bg = "white") 

plot(c(1993:2017),type = "o",col = "white", ylab = "Index", xlab = "Year",
     ylim = c(0,100), xlim = c(1993,2017), xaxt = "n", font.main = 1, main = expression(paste("b) ",H[0]," - Electoral Volatility")))
axis(1, at = c(1993,1996,1998,2002,2006,2010,2013,2017), las = 1, labels = F)
axis(1, at = c(1993,1996,1998,2002,2006,2010,2013,2017), las = 1, labels = T)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
abline(h=c(0,20,40,60,80,100), lty = 'solid', col = "gray90")
lines(volatility$year, volatility$Pedersen, type = "l", col = "black",lw=2,pch=19)
lines(volatility$year, volatility$typeA, type = "l", col = "black",lw=2,pch=19, lty = "dotdash")
legend("topright", legend = c("Pedersen Index","Type A Volatility"),
       col=c("black", "black"),lty = c("solid","dotdash"), lw = 2, cex = 1, bg = "white") 

plot(c(1993:2017),FINAL$number,type = "o",col = "white", ylab = "Index", xlab = "Year", 
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", font.main = 1, main = "c) Ideological Centre of Gravity")
axis(1, at = c(1993:2017), las = 1, labels = F)
axis(1, at = c(1993,1996,1998,2002,2006,2010,2013,2017), las = 1, labels = T)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
polygon(x = c(c(seq(1993,1997.5,0.1)),rev(c(seq(1993,1997.5,0.1)))),
        y = c(rep(10,length(seq(1993,1997.5,0.1))),rep(0,length(seq(1993,1997.5,0.1)))),
        col = adjustcolor("darkblue", alpha = 0.1), border = FALSE)
polygon(x = c(c(1998:2006),rev(c(1998:2006))), y = c(rep(10,9),rep(0,9)),
        col = adjustcolor("darkorange", alpha = 0.1), border = FALSE)
polygon(x = c(c(seq(2006,2009,0.1)),rev(c(seq(2006,2009,0.1)))),
        y = c(rep(10,length(seq(2006,2009,0.1))),rep(0,length(seq(2006,2009,0.1)))),
        col = adjustcolor("darkblue", alpha = 0.1), border = FALSE)
polygon(x = c(c(seq(2010,2012.5,0.1)),rev(c(seq(2010,2012.5,0.1)))), 
        y = c(rep(10,length(seq(2010,2012.5,0.1))),rep(0,length(seq(2010,2012.5,0.1)))),
        col = adjustcolor("darkblue", alpha = 0.1), border = FALSE)
polygon(x = c(c(2013:2017),rev(c(2013:2017))), y = c(rep(10,5),rep(0,5)),
        col = adjustcolor("darkorange", alpha = 0.1), border = FALSE)
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$mean.cihigh, xout=seq_along(FINAL$mean.cihigh))$y,
          rev(approx(FINAL$mean.cilow, xout=seq_along(FINAL$mean.cilow))$y)), col = "greenyellow", border = FALSE)
lines(c(1993:2017), approx(FINAL$mean.cilow, xout=seq_along(FINAL$mean.cilow))$y, type = "l", col = "green4",lw=1,pch=19, lty = "dotted")
lines(c(1993:2017), approx(FINAL$mean.cihigh, xout=seq_along(FINAL$mean.cihigh))$y, type = "l", col = "green4",lw=1,pch=19, lty = "dotted")
lines(c(1993:2017), approx(FINAL$mean, xout=seq_along(FINAL$mean))$y, type = "l", col = "green4",lw=2,pch=19)
text(c(1993,1996,1998,2002,2004,2005,2006,2006.9,2010,2013),0, pos = 4, srt=90,cex=0.8, col = "gray50",
     labels = c("Klaus I","Klaus II","Zeman","Spidla","Gross","Paroubek","Topolanek I","Topolanek II","Necas","Sobotka"))
text(c(2013.7,2010.3,1998.7),10, pos = 2, srt=90,cex=0.8, col = "gray50",  labels = c("Rusnok","Fischer","Tosovsky"))

plot(c(1993:2017),FINAL$number,type = "o",col = "white", ylab = "Index", xlab = "Year", 
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", font.main = 1, main = "d) Polarization Index")
axis(1, at = c(1993:2016), las = 1, labels = F)
axis(1, at = c(1993,1996,1998,2002,2006,2010,2013,2017), las = 1, labels = T)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y,
          rev(approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y)), col = "greenyellow", border = FALSE)
lines(c(1993:2017), approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y, type = "l", col = "green4",lw=1,pch=19, lty = "dotted")
lines(c(1993:2017), approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y, type = "l", col = "green4",lw=1,pch=19, lty = "dotted")
lines(c(1993:2017), approx(FINAL$pi, xout=seq_along(FINAL$pi))$y, type = "l", col = "green4",lw=2,pch=19)
dev.off()





pdf("figures/final2.pdf", width = 10, height = 10) # make a plot for the publications
par(mfrow=c(2,1), mar=c(5,4,3,0.5),family="serif", cex = 1,ps=13)
plot(c(1993:2017),FINAL$number,type = "o",col = "white", ylab = "Ideological Centre of Gravity", xlab = "Year", 
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", font.main = 1, main = "a) Ideological Centre of Gravity", las = 2)
axis(1, at = c(1993:2017), las = 1, labels = F)
axis(1, at = c(1993,1996,1998,2002,2006,2010,2013,2017), las = 1, labels = T)
polygon(x = c(c(seq(1993,1997.5,0.1)),rev(c(seq(1993,1997.5,0.1)))),
        y = c(rep(10,length(seq(1993,1997.5,0.1))),rep(0,length(seq(1993,1997.5,0.1)))),
        col = adjustcolor("darkblue", alpha = 0.2), border = FALSE)
polygon(x = c(c(1998:2006),rev(c(1998:2006))), y = c(rep(10,9),rep(0,9)),
        col = adjustcolor("darkorange", alpha = 0.2), border = FALSE)
polygon(x = c(c(seq(2006,2009,0.1)),rev(c(seq(2006,2009,0.1)))),
        y = c(rep(10,length(seq(2006,2009,0.1))),rep(0,length(seq(2006,2009,0.1)))),
        col = adjustcolor("darkblue", alpha = 0.2), border = FALSE)
polygon(x = c(c(seq(2010,2012.5,0.1)),rev(c(seq(2010,2012.5,0.1)))), 
        y = c(rep(10,length(seq(2010,2012.5,0.1))),rep(0,length(seq(2010,2012.5,0.1)))),
        col = adjustcolor("darkblue", alpha = 0.2), border = FALSE)
polygon(x = c(c(2013:2017),rev(c(2013:2017))), y = c(rep(10,5),rep(0,5)),
        col = adjustcolor("darkorange", alpha = 0.2), border = FALSE)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$mean.cihigh, xout=seq_along(FINAL$mean.cihigh))$y,
        rev(approx(FINAL$mean.cilow, xout=seq_along(FINAL$mean.cilow))$y)), col = "olivedrab1", border = FALSE)
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "white")
lines(c(1993:2017), approx(FINAL$mean.cilow, xout=seq_along(FINAL$mean.cilow))$y, type = "l", col = "green4",lw=1,pch=19, lty = "dotted")
lines(c(1993:2017), approx(FINAL$mean.cihigh, xout=seq_along(FINAL$mean.cihigh))$y, type = "l", col = "green4",lw=1,pch=19, lty = "dotted")
lines(c(1993:2017), approx(FINAL$mean, xout=seq_along(FINAL$mean))$y, type = "l", col = "green4",lw=2,pch=19)
text(c(1993,1996,1998,2002,2004,2005,2006,2006.7,2010,2013),0, pos = 4, srt=90,cex=1, col = "black",
     labels = c("Klaus I","Klaus II","Zeman","Spidla","Gross","Paroubek","Topolanek I","Topolanek II","Necas","Sobotka"))
text(c(2013.1,2009.6,1998.1),10, pos = 2, srt=90,cex=1, col = "black",  labels = c("Rusnok","Fischer","Tosovsky"))

plot(c(1993:2017),FINAL$number,type = "o",col = "white", ylab = "Ideological Polarization Index", xlab = "Year", 
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", font.main = 1, main = "b) Ideological Polarization Index", las = 2)
axis(1, at = c(1993:2016), las = 1, labels = F)
axis(1, at = c(1993,1996,1998,2002,2006,2010,2013,2017), las = 1, labels = T)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y,
            rev(approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y)), col = "olivedrab1", border = FALSE)
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
lines(c(1993:2017), approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y, type = "l", col = "green4",lw=1,pch=19, lty = "dotted")
lines(c(1993:2017), approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y, type = "l", col = "green4",lw=1,pch=19, lty = "dotted")
lines(c(1993:2017), approx(FINAL$pi, xout=seq_along(FINAL$pi))$y, type = "l", col = "green4",lw=2,pch=19)
lines(c(as.integer(rownames(FINAL)[which.max(FINAL$pi)]),as.integer(rownames(FINAL)[which.max(FINAL$pi)])),
      c(max(FINAL$pi),max(FINAL$pi)+1), col = "gray30")
text(as.integer(rownames(FINAL)[which.max(FINAL$pi)]),max(FINAL$pi)+1,labels = format(round(max(FINAL$pi),2), nsmall = 2),
     pos = 3, col = "gray30")
lines(c(as.integer(rownames(FINAL)[which.min(FINAL$pi)]),as.integer(rownames(FINAL)[which.min(FINAL$pi)])),
      c(min(FINAL$pi),min(FINAL$pi)-1), col = "gray30")
text(as.integer(rownames(FINAL)[which.min(FINAL$pi)]),min(FINAL$pi)-1,labels = format(round(min(FINAL$pi),2), nsmall = 2),
     pos = 1, col = "gray30")
lines(c(2017,2017), c(FINAL$pi[row.names(FINAL)=="2017"],FINAL$pi[row.names(FINAL)=="2017"]+1), col = "gray30")
text(2017,FINAL$pi[row.names(FINAL)=="2017"]+1,labels = format(round(FINAL$pi[row.names(FINAL)=="2017"],2), nsmall = 2),
     pos = 3, col = "gray30")
lines(c(1993,1993), c(FINAL$pi[row.names(FINAL)=="1993"],FINAL$pi[row.names(FINAL)=="1993"]+1), col = "gray30")
text(1993,FINAL$pi[row.names(FINAL)=="1993"]+1,labels = format(round(FINAL$pi[row.names(FINAL)=="1993"],2), nsmall = 2),
     pos = 3, col = "gray30")
dev.off()





# the following figure is based on "data" delivered from the original results of "a90_r3_i15000_b0_t1"
pdf("figures/density.pdf", width = 10, height = 4) # plot density
par(mar=c(4.5,4,0.5,0.5),family="serif", cex = 1, ps=13)
plot(density(data[data$year==2017,5],weights=mandates$M[mandates$year==2017]/sum(mandates$M[mandates$year==2017])),
     lwd = 2, xlim = c(-7.3,7.3), xlab = expression(paste("Ideal Point (",italic(theta)['p,2017'],")")), main = "",
     xaxt = "n")
axis(1, at = seq(-6,6,2), las = 1, labels = T)
text(data[data$year==2017 & data$party=="KSCM",5],0.1,labels = "KSCM",
     pos = 3, col = "black")
text(data[data$year==2017 & data$party=="TOP09",5],0.08,labels = "TOP09",
     pos = 3, col = "black")
dev.off()



##################################
###### Robustness Analysis #######
##################################


pdf("figures/rob-analysis.pdf", width = 10, height = 11.2) # plot robustness analyses with different "final"s
par(mfrow=c(5,2),family="serif", oma = c(5,3.5,0.5,0.5), mar = c(0,0.3,0.3,0), cex = 1, ps=13) # graf
plot(c(1993:2017),FINAL$number,type = "o",col = "white",
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", las = 2)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y,
                                            rev(approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y)), col = adjustcolor("grey20",alpha = 0.2), border = FALSE)
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(final_top$pi.cihigh, xout=seq_along(final_top$pi.cihigh))$y,
                                            rev(approx(final_top$pi.cilow, xout=seq_along(final_top$pi.cilow))$y)), col = adjustcolor("grey20",alpha = 0.2), border = FALSE)
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
lines(c(1993:2017), approx(FINAL$pi, xout=seq_along(FINAL$pi))$y, type = "l", col = "black",lw=2,pch=19)
lines(c(1993:2017), approx(final_top$pi, xout=seq_along(final_top$pi))$y, type = "l", col = "black",lw=2,pch=19, lty = "dotted")
points(as.integer(rownames(final_top[final_top$dif,])),final_top$pi[final_top$dif], pch = 8, col = "black")
legend("top", cex = 1, bg = "white", 
       legend = paste0("a) TOP 09 as Right ","(r = ",sprintf("%.3f", round(cor(final_top$pi, final_top$number, use = "complete.obs", method = "pearson"),3)),")    "))

plot(c(1993:2017),FINAL$number,type = "o",col = "white",
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", yaxt = "n", las = 2)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y,
                                            rev(approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(final_p$pi.cihigh, xout=seq_along(final_p$pi.cihigh))$y,
                                            rev(approx(final_p$pi.cilow, xout=seq_along(final_p$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
lines(c(1993:2017), approx(FINAL$pi, xout=seq_along(FINAL$pi))$y, type = "l", col = "black",lw=2,pch=19)
lines(c(1993:2017), approx(final_p$pi, xout=seq_along(final_p$pi))$y, type = "l", col = "black",lw=2,pch=19, lty = "dotted")
points(as.integer(rownames(final_p[final_p$dif,])),final_p$pi[final_p$dif], pch = 8, col = "black")
legend("top", cex = 1, bg = "white", 
       legend = paste0("b) Aggregated Party Level ","(r = ",sprintf("%.3f", round(cor(final_p$pi, final_p$number, use = "complete.obs", method = "pearson"),3)),")    "))

plot(c(1993:2017),FINAL$number,type = "o",col = "white",
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", las = 2)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y,
                                            rev(approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(final_130$pi.cihigh, xout=seq_along(final_130$pi.cihigh))$y,
                                            rev(approx(final_130$pi.cilow, xout=seq_along(final_130$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
lines(c(1993:2017), approx(FINAL$pi, xout=seq_along(FINAL$pi))$y, type = "l", col = "black",lw=2,pch=19)
lines(c(1993:2017), approx(final_130$pi, xout=seq_along(final_130$pi))$y, type = "l", col = "black",lw=2,pch=19, lty = "dotted")
points(as.integer(rownames(final_130[final_130$dif,])),final_130$pi[final_130$dif], pch = 8, col = "black")
legend("top", cex = 1, bg = "white", 
       legend = paste0("c) 130 Degrees ","(r = ",sprintf("%.3f", round(cor(final_130$pi, final_130$number, use = "complete.obs", method = "pearson"),3)),")    "))

plot(c(1993:2017),FINAL$number,type = "o",col = "white",
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", yaxt = "n", las = 2)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y,
                                            rev(approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(final_120$pi.cihigh, xout=seq_along(final_120$pi.cihigh))$y,
                                            rev(approx(final_120$pi.cilow, xout=seq_along(final_120$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
lines(c(1993:2017), approx(FINAL$pi, xout=seq_along(FINAL$pi))$y, type = "l", col = "black",lw=2,pch=19)
lines(c(1993:2017), approx(final_120$pi, xout=seq_along(final_120$pi))$y, type = "l", col = "black",lw=2,pch=19, lty = "dotted")
points(as.integer(rownames(final_120[final_120$dif,])),final_120$pi[final_120$dif], pch = 8, col = "black")
legend("top", cex = 1, bg = "white", 
       legend = paste0("d) 120 Degrees ","(r = ",sprintf("%.3f", round(cor(final_120$pi, final_120$number, use = "complete.obs", method = "pearson"),3)),")    "))

plot(c(1993:2017),FINAL$number,type = "o",col = "white",
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", las = 2)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y,
                                            rev(approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(final_110$pi.cihigh, xout=seq_along(final_110$pi.cihigh))$y,
                                            rev(approx(final_110$pi.cilow, xout=seq_along(final_110$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
lines(c(1993:2017), approx(FINAL$pi, xout=seq_along(FINAL$pi))$y, type = "l", col = "black",lw=2,pch=19)
lines(c(1993:2017), approx(final_110$pi, xout=seq_along(final_110$pi))$y, type = "l", col = "black",lw=2,pch=19, lty = "dotted")
points(as.integer(rownames(final_110[final_110$dif,])),final_110$pi[final_110$dif], pch = 8, col = "black")
legend("top", cex = 1, bg = "white", 
       legend = paste0("e) 110 Degrees ","(r = ",sprintf("%.3f", round(cor(final_110$pi, final_110$number, use = "complete.obs", method = "pearson"),3)),")    "))

plot(c(1993:2017),FINAL$number,type = "o",col = "white",
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", yaxt = "n", las = 2)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y,
                                            rev(approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(final_100$pi.cihigh, xout=seq_along(final_100$pi.cihigh))$y,
                                            rev(approx(final_100$pi.cilow, xout=seq_along(final_100$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
lines(c(1993:2017), approx(FINAL$pi, xout=seq_along(FINAL$pi))$y, type = "l", col = "black",lw=2,pch=19)
lines(c(1993:2017), approx(final_100$pi, xout=seq_along(final_100$pi))$y, type = "l", col = "black",lw=2,pch=19, lty = "dotted")
points(as.integer(rownames(final_100[final_100$dif,])),final_100$pi[final_100$dif], pch = 8, col = "black")
legend("top", cex = 1, bg = "white", 
       legend = paste0("f) 100 Degrees ","(r = ",sprintf("%.3f", round(cor(final_100$pi, final_100$number, use = "complete.obs", method = "pearson"),3)),")    "))

plot(c(1993:2017),FINAL$number,type = "o",col = "white",
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", las = 2)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y,
                                            rev(approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(final_80$pi.cihigh, xout=seq_along(final_80$pi.cihigh))$y,
                                            rev(approx(final_80$pi.cilow, xout=seq_along(final_80$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
lines(c(1993:2017), approx(FINAL$pi, xout=seq_along(FINAL$pi))$y, type = "l", col = "black",lw=2,pch=19)
lines(c(1993:2017), approx(final_80$pi, xout=seq_along(final_80$pi))$y, type = "l", col = "black",lw=2,pch=19, lty = "dotted")
points(as.integer(rownames(final_80[final_80$dif,])),final_80$pi[final_80$dif], pch = 8, col = "black")
legend("top", cex = 1, bg = "white", 
       legend = paste0("g) 80 Degrees ","(r = ",sprintf("%.3f", round(cor(final_80$pi, final_80$number, use = "complete.obs", method = "pearson"),3)),")    "))

plot(c(1993:2017),FINAL$number,type = "o",col = "white",
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", yaxt = "n", las = 2)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y,
                                            rev(approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(final_70$pi.cihigh, xout=seq_along(final_70$pi.cihigh))$y,
                                            rev(approx(final_70$pi.cilow, xout=seq_along(final_70$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
lines(c(1993:2017), approx(FINAL$pi, xout=seq_along(FINAL$pi))$y, type = "l", col = "black",lw=2,pch=19)
lines(c(1993:2017), approx(final_70$pi, xout=seq_along(final_70$pi))$y, type = "l", col = "black",lw=2,pch=19, lty = "dotted")
points(as.integer(rownames(final_70[final_70$dif,])),final_70$pi[final_70$dif], pch = 8, col = "black")
legend("top", cex = 1, bg = "white", 
       legend = paste0("h) 70 Degrees ","(r = ",sprintf("%.3f", round(cor(final_70$pi, final_70$number, use = "complete.obs", method = "pearson"),3)),")    "))

plot(c(1993:2017),FINAL$number,type = "o",col = "white",
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", las = 2)
axis(1, at = c(1993:2017), las = 1, labels = F)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y,
                                            rev(approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(final_60$pi.cihigh, xout=seq_along(final_60$pi.cihigh))$y,
                                            rev(approx(final_60$pi.cilow, xout=seq_along(final_60$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
lines(c(1993:2017), approx(FINAL$pi, xout=seq_along(FINAL$pi))$y, type = "l", col = "black",lw=2,pch=19)
lines(c(1993:2017), approx(final_60$pi, xout=seq_along(final_60$pi))$y, type = "l", col = "black",lw=2,pch=19, lty = "dotted")
points(as.integer(rownames(final_60[final_60$dif,])),final_60$pi[final_60$dif], pch = 8, col = "black")
legend("top", cex = 1, bg = "white", 
       legend = paste0("i) 60 Degrees ","(r = ",sprintf("%.3f", round(cor(final_60$pi, final_60$number, use = "complete.obs", method = "pearson"),3)),")    "))
axis(1, at = c(1993,1996,1998,2002,2006,2010,2013,2017), las = 1, labels = T)

plot(c(1993:2017),FINAL$number,type = "o",col = "white",
     ylim = c(-0,10), xlim = c(1993,2017), xaxt = "n", yaxt = "n", las = 2)
axis(1, at = c(1993:2017), las = 1, labels = F)
abline(v=c(1996,1998,2002,2006,2010,2013), lty = 'dashed', col = "black")
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(FINAL$pi.cihigh, xout=seq_along(FINAL$pi.cihigh))$y,
                                            rev(approx(FINAL$pi.cilow, xout=seq_along(FINAL$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
polygon(c(c(1993:2017),rev(c(1993:2017))),c(approx(final_50$pi.cihigh, xout=seq_along(final_50$pi.cihigh))$y,
                                            rev(approx(final_50$pi.cilow, xout=seq_along(final_50$pi.cilow))$y)), col = adjustcolor("gray20",alpha = 0.2), border = FALSE)
abline(h=c(0,2,4,6,8,10), lty = 'solid', col = "gray90")
lines(c(1993:2017), approx(FINAL$pi, xout=seq_along(FINAL$pi))$y, type = "l", col = "black",lw=2,pch=19)
lines(c(1993:2017), approx(final_50$pi, xout=seq_along(final_50$pi))$y, type = "l", col = "black",lw=2,pch=19, lty = "dotted")
axis(1, at = c(1993,1996,1998,2002,2006,2010,2013,2017), las = 1, labels = T)
points(as.integer(rownames(final_50[final_50$dif,])),final_50$pi[final_50$dif], pch = 8, col = "black")
legend("top", cex = 1, bg = "white", 
       legend = paste0("j) 50 Degrees ","(r = ",sprintf("%.3f", round(cor(final_50$pi, final_50$number, use = "complete.obs", method = "pearson"),3)),")    "))
title(xlab = "Year", outer = TRUE, line = 3)
title(ylab = "Ideological Polarization Index", outer = TRUE, line = 2.5)
dev.off()









