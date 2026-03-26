### R Code for "Conflict Environments and Civil War" (Reid et al. 2020, JOGSS)
## Using CShapes Package from Weidman, Kuse, & Gleditsch (2010)
## This file includes base code for Conflict Environment Scores

#BASE CODE FOR CONFLICT ENVIRONMENT SCORES

library(foreign) 
library(readstata13)
library(cshapes) 

#setwd("/Users/rachelmyrick/Dropbox/Autsurv/civilwar/replication/")

setwd("/Users/rachelmyrick/Downloads/replication")
wars = read.dta13("Base Data for Civil Wars.dta")


# Spatial Lag of Civil Conflict (Armed Conflict Dataset) ------------------

distance = vector("list", 61)
civil = vector("list", 61)
c =vector("list", 61)
date.current = vector("list", 61)
cmap = vector("list", 61)
civilwar = vector("list", 61)
civilc = vector("list", 61)
civilm = vector("list", 61)
dmat = vector("list", 61)
degdmat = vector("list", 61)
civildeg = vector("list", 61)
year = vector("list", 61)
civilwar = vector("list", 61)
CEcivil = vector("list", 61)
ccodecivil = vector("list", 61)

load(file = "distmat1946.2006.rda")

for(k in 1:61){
  civil[[k]] = subset(wars, year == 1945+k, select = c(ccode, acdwar))
  date.current[[k]] = paste(1945+k, "6", "30", sep="-")
  cmap[[k]] = cshp(date=as.Date(date.current[[k]]), useGW=F)
  c[[k]] = match(cmap[[k]]$COWCODE, civil[[k]]$ccode)
  civilc[[k]] = civil[[k]][c[[k]],]
  row.names(civilc[[k]]) = cmap[[k]]$COWCODE
  civilm[[k]] = civilc[[k]]$acdwar
  civilm[[k]][is.na(civilm[[k]])] = 0
  civilm[[k]] = matrix(civilm[[k]])
  row.names(civilm[[k]]) = cmap[[k]]$COWCODE
  #dmat[[k]] = distmatrix(as.Date(date.current[[k]]), type ="mindist", useGW = F) #WAIT! 
  row.names(dmat[[k]]) = cmap[[k]]$COWCODE
  degdmat[[k]] = ifelse(dmat[[k]] > 950, 0, (1-(dmat[[k]]/950)^0.25))
  diag(degdmat[[k]]) = 0 
  civildeg[[k]] = degdmat[[k]] %*% civilm[[k]] 
  year[[k]] = rep.int(1945+k, length(civildeg[[k]][,1]))
  civilwar[[k]] = cbind(cmap[[k]]$COWCODE, year[[k]], civildeg[[k]])
  colnames(civilwar[[k]]) = c("ccode", "year", "slc_acd")
  CEcivil[[k]] = as.data.frame(civilwar[[k]])
  ccodecivil[[k]] = civilwar[[k]][,1]
  CEcivil[[k]] = civilwar[[k]][order(ccodecivil[[k]]) , ]
  row.names(CEcivil[[k]]) =NULL 
}

CEcivilMAT = do.call(rbind, CEcivil)
spatial.lag.civil = as.data.frame(CEcivilMAT)
write.dta(spatial.lag.civil, "slc_acd.dta")


# Spatial Lag of Civil Conflict Onset (Armed Conflict Dataset) ------------

distance = vector("list", 61)
civil = vector("list", 61)
c =vector("list", 61)
date.current = vector("list", 61)
cmap = vector("list", 61)
civilwar = vector("list", 61)
civilc = vector("list", 61)
civilm = vector("list", 61)
dmat = vector("list", 61)
degdmat = vector("list", 61)
civildeg = vector("list", 61)
year = vector("list", 61)
civilwar = vector("list", 61)
CEcivil = vector("list", 61)
ccodecivil = vector("list", 61)

load(file = "distmat1946.2006.rda")

for(k in 1:61){
  civil[[k]] = subset(wars, year == 1945+k, select = c(ccode, acdonset))
  date.current[[k]] = paste(1945+k, "6", "30", sep="-")
  cmap[[k]] = cshp(date=as.Date(date.current[[k]]), useGW=F)
  c[[k]] = match(cmap[[k]]$COWCODE, civil[[k]]$ccode)
  civilc[[k]] = civil[[k]][c[[k]],]
  row.names(civilc[[k]]) = cmap[[k]]$COWCODE
  civilm[[k]] = civilc[[k]]$acdonset
  civilm[[k]][is.na(civilm[[k]])] = 0
  civilm[[k]] = matrix(civilm[[k]])
  row.names(civilm[[k]]) = cmap[[k]]$COWCODE
  #dmat[[k]] = distmatrix(as.Date(date.current[[k]]), type ="mindist", useGW = F) #WAIT! 
  row.names(dmat[[k]]) = cmap[[k]]$COWCODE
  degdmat[[k]] = ifelse(dmat[[k]] > 950, 0, (1-(dmat[[k]]/950)^0.25))
  diag(degdmat[[k]]) = 0 
  civildeg[[k]] = degdmat[[k]] %*% civilm[[k]] 
  year[[k]] = rep.int(1945+k, length(civildeg[[k]][,1]))
  civilwar[[k]] = cbind(cmap[[k]]$COWCODE, year[[k]], civildeg[[k]])
  colnames(civilwar[[k]]) = c("ccode", "year", "slco_acd")
  CEcivil[[k]] = as.data.frame(civilwar[[k]])
  ccodecivil[[k]] = civilwar[[k]][,1]
  CEcivil[[k]] = civilwar[[k]][order(ccodecivil[[k]]) , ]
  row.names(CEcivil[[k]]) =NULL 
}

CEcivilMAT = do.call(rbind, CEcivil)
spatial.lag.civil = as.data.frame(CEcivilMAT)
write.dta(spatial.lag.civil, "slco_acd.dta")


# Spatial Lag of Civil Conflict (COW Dataset) -----------------------------

distance = vector("list", 61)
civil = vector("list", 61)
c =vector("list", 61)
date.current = vector("list", 61)
cmap = vector("list", 61)
civilwar = vector("list", 61)
civilc = vector("list", 61)
civilm = vector("list", 61)
dmat = vector("list", 61)
degdmat = vector("list", 61)
civildeg = vector("list", 61)
year = vector("list", 61)
civilwar = vector("list", 61)
CEcivil = vector("list", 61)
ccodecivil = vector("list", 61)

load(file = "distmat1946.2006.rda")

for(k in 1:61){
  civil[[k]] = subset(wars, year == 1945+k, select = c(ccode, cowwar))
  date.current[[k]] = paste(1945+k, "6", "30", sep="-")
  cmap[[k]] = cshp(date=as.Date(date.current[[k]]), useGW=F)
  c[[k]] = match(cmap[[k]]$COWCODE, civil[[k]]$ccode)
  civilc[[k]] = civil[[k]][c[[k]],]
  row.names(civilc[[k]]) = cmap[[k]]$COWCODE
  civilm[[k]] = civilc[[k]]$cowwar
  civilm[[k]][is.na(civilm[[k]])] = 0
  civilm[[k]] = matrix(civilm[[k]])
  row.names(civilm[[k]]) = cmap[[k]]$COWCODE
  #dmat[[k]] = distmatrix(as.Date(date.current[[k]]), type ="mindist", useGW = F) #WAIT! 
  row.names(dmat[[k]]) = cmap[[k]]$COWCODE
  degdmat[[k]] = ifelse(dmat[[k]] > 950, 0, (1-(dmat[[k]]/950)^0.25))
  diag(degdmat[[k]]) = 0 
  civildeg[[k]] = degdmat[[k]] %*% civilm[[k]] 
  year[[k]] = rep.int(1945+k, length(civildeg[[k]][,1]))
  civilwar[[k]] = cbind(cmap[[k]]$COWCODE, year[[k]], civildeg[[k]])
  colnames(civilwar[[k]]) = c("ccode", "year", "slc_civil")
  CEcivil[[k]] = as.data.frame(civilwar[[k]])
  ccodecivil[[k]] = civilwar[[k]][,1]
  CEcivil[[k]] = civilwar[[k]][order(ccodecivil[[k]]) , ]
  row.names(CEcivil[[k]]) =NULL 
}

CEcivilMAT = do.call(rbind, CEcivil)
spatial.lag.civil = as.data.frame(CEcivilMAT)
write.dta(spatial.lag.civil, "slc_civil.dta")

# Spatial Lag of Civil Conflict Onset (COW Dataset) -----------------------

distance = vector("list", 61)
civil = vector("list", 61)
c =vector("list", 61)
date.current = vector("list", 61)
cmap = vector("list", 61)
civilwar = vector("list", 61)
civilc = vector("list", 61)
civilm = vector("list", 61)
dmat = vector("list", 61)
degdmat = vector("list", 61)
civildeg = vector("list", 61)
year = vector("list", 61)
civilwar = vector("list", 61)
CEcivil = vector("list", 61)
ccodecivil = vector("list", 61)

load(file = "distmat1946.2006.rda")

for(k in 1:61){
  civil[[k]] = subset(wars, year == 1945+k, select = c(ccode, cowonset))
  date.current[[k]] = paste(1945+k, "6", "30", sep="-")
  cmap[[k]] = cshp(date=as.Date(date.current[[k]]), useGW=F)
  c[[k]] = match(cmap[[k]]$COWCODE, civil[[k]]$ccode)
  civilc[[k]] = civil[[k]][c[[k]],]
  row.names(civilc[[k]]) = cmap[[k]]$COWCODE
  civilm[[k]] = civilc[[k]]$cowonset
  civilm[[k]][is.na(civilm[[k]])] = 0
  civilm[[k]] = matrix(civilm[[k]])
  row.names(civilm[[k]]) = cmap[[k]]$COWCODE
  #dmat[[k]] = distmatrix(as.Date(date.current[[k]]), type ="mindist", useGW = F) #WAIT! 
  row.names(dmat[[k]]) = cmap[[k]]$COWCODE
  degdmat[[k]] = ifelse(dmat[[k]] > 950, 0, (1-(dmat[[k]]/950)^0.25))
  diag(degdmat[[k]]) = 0 
  civildeg[[k]] = degdmat[[k]] %*% civilm[[k]] 
  year[[k]] = rep.int(1945+k, length(civildeg[[k]][,1]))
  civilwar[[k]] = cbind(cmap[[k]]$COWCODE, year[[k]], civildeg[[k]])
  colnames(civilwar[[k]]) = c("ccode", "year", "slco_civil")
  CEcivil[[k]] = as.data.frame(civilwar[[k]])
  ccodecivil[[k]] = civilwar[[k]][,1]
  CEcivil[[k]] = civilwar[[k]][order(ccodecivil[[k]]) , ]
  row.names(CEcivil[[k]]) =NULL 
}

CEcivilMAT = do.call(rbind, CEcivil)
spatial.lag.civil = as.data.frame(CEcivilMAT)
write.dta(spatial.lag.civil, "slco_civil.dta")

# Spatial Lag of Civil Conflict (FL 2003) ---------------------------------

distance = vector("list", 61)
civil = vector("list", 61)
c =vector("list", 61)
date.current = vector("list", 61)
cmap = vector("list", 61)
civilwar = vector("list", 61)
civilc = vector("list", 61)
civilm = vector("list", 61)
dmat = vector("list", 61)
degdmat = vector("list", 61)
civildeg = vector("list", 61)
year = vector("list", 61)
civilwar = vector("list", 61)
CEcivil = vector("list", 61)
ccodecivil = vector("list", 61)

load(file = "distmat1946.2006.rda")

for(k in 1:61){
  civil[[k]] = subset(wars, year == 1945+k, select = c(ccode, flwar))
  date.current[[k]] = paste(1945+k, "6", "30", sep="-")
  cmap[[k]] = cshp(date=as.Date(date.current[[k]]), useGW=F)
  c[[k]] = match(cmap[[k]]$COWCODE, civil[[k]]$ccode)
  civilc[[k]] = civil[[k]][c[[k]],]
  row.names(civilc[[k]]) = cmap[[k]]$COWCODE
  civilm[[k]] = civilc[[k]]$flwar
  civilm[[k]][is.na(civilm[[k]])] = 0
  civilm[[k]] = matrix(civilm[[k]])
  row.names(civilm[[k]]) = cmap[[k]]$COWCODE
  #dmat[[k]] = distmatrix(as.Date(date.current[[k]]), type ="mindist", useGW = F) #WAIT! 
  row.names(dmat[[k]]) = cmap[[k]]$COWCODE
  degdmat[[k]] = ifelse(dmat[[k]] > 950, 0, (1-(dmat[[k]]/950)^0.25))
  diag(degdmat[[k]]) = 0 
  civildeg[[k]] = degdmat[[k]] %*% civilm[[k]] 
  year[[k]] = rep.int(1945+k, length(civildeg[[k]][,1]))
  civilwar[[k]] = cbind(cmap[[k]]$COWCODE, year[[k]], civildeg[[k]])
  colnames(civilwar[[k]]) = c("ccode", "year", "slc_fl")
  CEcivil[[k]] = as.data.frame(civilwar[[k]])
  ccodecivil[[k]] = civilwar[[k]][,1]
  CEcivil[[k]] = civilwar[[k]][order(ccodecivil[[k]]) , ]
  row.names(CEcivil[[k]]) =NULL 
}

CEcivilMAT = do.call(rbind, CEcivil)
spatial.lag.civil = as.data.frame(CEcivilMAT)
write.dta(spatial.lag.civil, "slc_fl.dta")


# Spatial Lag of Civil Conflict Onset (FL 2003) ---------------------------

distance = vector("list", 61)
civil = vector("list", 61)
c =vector("list", 61)
date.current = vector("list", 61)
cmap = vector("list", 61)
civilwar = vector("list", 61)
civilc = vector("list", 61)
civilm = vector("list", 61)
dmat = vector("list", 61)
degdmat = vector("list", 61)
civildeg = vector("list", 61)
year = vector("list", 61)
civilwar = vector("list", 61)
CEcivil = vector("list", 61)
ccodecivil = vector("list", 61)

load(file = "distmat1946.2006.rda")

for(k in 1:61){
  civil[[k]] = subset(wars, year == 1945+k, select = c(ccode, flonset))
  date.current[[k]] = paste(1945+k, "6", "30", sep="-")
  cmap[[k]] = cshp(date=as.Date(date.current[[k]]), useGW=F)
  c[[k]] = match(cmap[[k]]$COWCODE, civil[[k]]$ccode)
  civilc[[k]] = civil[[k]][c[[k]],]
  row.names(civilc[[k]]) = cmap[[k]]$COWCODE
  civilm[[k]] = civilc[[k]]$flonset
  civilm[[k]][is.na(civilm[[k]])] = 0
  civilm[[k]] = matrix(civilm[[k]])
  row.names(civilm[[k]]) = cmap[[k]]$COWCODE
  #dmat[[k]] = distmatrix(as.Date(date.current[[k]]), type ="mindist", useGW = F) #WAIT! 
  row.names(dmat[[k]]) = cmap[[k]]$COWCODE
  degdmat[[k]] = ifelse(dmat[[k]] > 950, 0, (1-(dmat[[k]]/950)^0.25))
  diag(degdmat[[k]]) = 0 
  civildeg[[k]] = degdmat[[k]] %*% civilm[[k]] 
  year[[k]] = rep.int(1945+k, length(civildeg[[k]][,1]))
  civilwar[[k]] = cbind(cmap[[k]]$COWCODE, year[[k]], civildeg[[k]])
  colnames(civilwar[[k]]) = c("ccode", "year", "slco_fl")
  CEcivil[[k]] = as.data.frame(civilwar[[k]])
  ccodecivil[[k]] = civilwar[[k]][,1]
  CEcivil[[k]] = civilwar[[k]][order(ccodecivil[[k]]) , ]
  row.names(CEcivil[[k]]) =NULL 
}

CEcivilMAT = do.call(rbind, CEcivil)
spatial.lag.civil = as.data.frame(CEcivilMAT)
write.dta(spatial.lag.civil, "slco_fl.dta")


# Spatial Lag of Civil Conflict  (Sambanis 2004) --------------------------

distance = vector("list", 61)
civil = vector("list", 61)
c =vector("list", 61)
date.current = vector("list", 61)
cmap = vector("list", 61)
civilwar = vector("list", 61)
civilc = vector("list", 61)
civilm = vector("list", 61)
dmat = vector("list", 61)
degdmat = vector("list", 61)
civildeg = vector("list", 61)
year = vector("list", 61)
civilwar = vector("list", 61)
CEcivil = vector("list", 61)
ccodecivil = vector("list", 61)

load(file = "distmat1946.2006.rda")

for(k in 1:61){
  civil[[k]] = subset(wars, year == 1945+k, select = c(ccode, samwar))
  date.current[[k]] = paste(1945+k, "6", "30", sep="-")
  cmap[[k]] = cshp(date=as.Date(date.current[[k]]), useGW=F)
  c[[k]] = match(cmap[[k]]$COWCODE, civil[[k]]$ccode)
  civilc[[k]] = civil[[k]][c[[k]],]
  row.names(civilc[[k]]) = cmap[[k]]$COWCODE
  civilm[[k]] = civilc[[k]]$samwar
  civilm[[k]][is.na(civilm[[k]])] = 0
  civilm[[k]] = matrix(civilm[[k]])
  row.names(civilm[[k]]) = cmap[[k]]$COWCODE
  #dmat[[k]] = distmatrix(as.Date(date.current[[k]]), type ="mindist", useGW = F) #WAIT! 
  row.names(dmat[[k]]) = cmap[[k]]$COWCODE
  degdmat[[k]] = ifelse(dmat[[k]] > 950, 0, (1-(dmat[[k]]/950)^0.25))
  diag(degdmat[[k]]) = 0 
  civildeg[[k]] = degdmat[[k]] %*% civilm[[k]] 
  year[[k]] = rep.int(1945+k, length(civildeg[[k]][,1]))
  civilwar[[k]] = cbind(cmap[[k]]$COWCODE, year[[k]], civildeg[[k]])
  colnames(civilwar[[k]]) = c("ccode", "year", "slc_s")
  CEcivil[[k]] = as.data.frame(civilwar[[k]])
  ccodecivil[[k]] = civilwar[[k]][,1]
  CEcivil[[k]] = civilwar[[k]][order(ccodecivil[[k]]) , ]
  row.names(CEcivil[[k]]) =NULL 
}

CEcivilMAT = do.call(rbind, CEcivil)
spatial.lag.civil = as.data.frame(CEcivilMAT)
write.dta(spatial.lag.civil, "slc_s.dta")


# Spatial Lag of Civil Conflict Onset (Sambanis 2004) ---------------------

distance = vector("list", 61)
civil = vector("list", 61)
c =vector("list", 61)
date.current = vector("list", 61)
cmap = vector("list", 61)
civilwar = vector("list", 61)
civilc = vector("list", 61)
civilm = vector("list", 61)
dmat = vector("list", 61)
degdmat = vector("list", 61)
civildeg = vector("list", 61)
year = vector("list", 61)
civilwar = vector("list", 61)
CEcivil = vector("list", 61)
ccodecivil = vector("list", 61)

load(file = "distmat1946.2006.rda")

for(k in 1:61){
  civil[[k]] = subset(wars, year == 1945+k, select = c(ccode, samonset))
  date.current[[k]] = paste(1945+k, "6", "30", sep="-")
  cmap[[k]] = cshp(date=as.Date(date.current[[k]]), useGW=F)
  c[[k]] = match(cmap[[k]]$COWCODE, civil[[k]]$ccode)
  civilc[[k]] = civil[[k]][c[[k]],]
  row.names(civilc[[k]]) = cmap[[k]]$COWCODE
  civilm[[k]] = civilc[[k]]$samonset
  civilm[[k]][is.na(civilm[[k]])] = 0
  civilm[[k]] = matrix(civilm[[k]])
  row.names(civilm[[k]]) = cmap[[k]]$COWCODE
  #dmat[[k]] = distmatrix(as.Date(date.current[[k]]), type ="mindist", useGW = F) #WAIT! 
  row.names(dmat[[k]]) = cmap[[k]]$COWCODE
  degdmat[[k]] = ifelse(dmat[[k]] > 950, 0, (1-(dmat[[k]]/950)^0.25))
  diag(degdmat[[k]]) = 0 
  civildeg[[k]] = degdmat[[k]] %*% civilm[[k]] 
  year[[k]] = rep.int(1945+k, length(civildeg[[k]][,1]))
  civilwar[[k]] = cbind(cmap[[k]]$COWCODE, year[[k]], civildeg[[k]])
  colnames(civilwar[[k]]) = c("ccode", "year", "slco_s")
  CEcivil[[k]] = as.data.frame(civilwar[[k]])
  ccodecivil[[k]] = civilwar[[k]][,1]
  CEcivil[[k]] = civilwar[[k]][order(ccodecivil[[k]]) , ]
  row.names(CEcivil[[k]]) =NULL 
}

CEcivilMAT = do.call(rbind, CEcivil)
spatial.lag.civil = as.data.frame(CEcivilMAT)
write.dta(spatial.lag.civil, "slco_s.dta")


# Create Base Data for Spatial Lags of Conflict ---------------------------

slc_civil = read.dta13("slc_civil.dta")
slco_civil = read.dta13("slco_civil.dta")
slc_acd = read.dta13("slc_acd.dta")
slco_acd = read.dta13("slco_acd.dta")
slc_fl = read.dta13("slc_fl.dta")
slco_fl = read.dta13("slco_fl.dta")  
slc_s = read.dta13("slc_s.dta")
slco_s = read.dta13("slco_s.dta")  

base_data = merge(slc_civil, slco_civil, by = c("ccode", "year"))
base_data = merge(base_data, slc_acd, by = c("ccode", "year"))
base_data = merge(base_data, slco_acd, by = c("ccode", "year"))
base_data = merge(base_data, slc_fl, by = c("ccode", "year"))
base_data = merge(base_data, slco_fl, by = c("ccode", "year"))
base_data = merge(base_data, slc_s, by = c("ccode", "year"))
base_data = merge(base_data, slco_s, by = c("ccode", "year"))

base_data = base_data[order(base_data$ccode, base_data$year),]
base_war_data = read.dta13("Base Data for Civil Wars.dta")

merged = merge(base_data, base_war_data, by = c("ccode", "year"), all.x = TRUE, all.y = FALSE)
merged = merged[order(merged$ccode, merged$year),]

write.dta(merged, "Base Data for Spatial Lags of Conflict.dta")
