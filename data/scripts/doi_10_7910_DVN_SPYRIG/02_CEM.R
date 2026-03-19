# Load and organize the wide and the long data file

library(data.table) 
library(fastmatch)
library(bit64) # packages for big data

# DELETED CODE HERE (still on server) and only use processed and shrunk data files
# remark 2021: this code part is lost since the server is gone. Basically the following steps were done on the server:
# - reading the csv file with WoS Data
# - saving a wide-shaped version of the data (wos_C.RData)
# - saving a long-shaped version of the data (wos_long_C.RData)

setwd("G:/Web of Science/WOS/2015/")
#for reentry
load("processed_files/wos_C.RData")

# reentry
load("processed_files/wos_long_C.RData")

# make lists of all nobels per wave (we need this to exclude those from control group; and to calculate some descritive things)
for(wave in list("w1","w2","w3")) {
  assign(paste0("all_nobels_",wave),NULL)
  for(nobelist in nobelists) {
    try(T9 <- as.integer64(as.numeric(substring(read.csv(paste0(wave,"/startfiles/",nobelist),head=F)$V1,3))))
    assign(paste0("all_nobels_",wave), c(get(paste0("all_nobels_",wave)),T9))
  }
}

# get average publication year per wave (only T)
w1 <- wos_C[match(all_nobels_w1,T9),]
w2 <- wos_C[match(all_nobels_w2,T9),]
w3 <- wos_C[match(all_nobels_w3,T9),]
mean(w1$PY) #1983.703 N=195
mean(w2$PY,na.rm = TRUE) #1980.897 N=3285
mean(w3$PY,na.rm = TRUE) #1979.009 N=33440


library(reshape2) #for dcast when reshaping from long to wide
library(haven) # to write data to stata format (for ebalancing)

nobelists <- list.files("w1/startfiles/")

for(wave in list("w1","w2","w3")) {
  print(paste("Processing",wave))
  if(wave=="w2") nobelists <- nobelists[nobelists!="Spence"] #remove Spence after w1 is done because Spence doesnt snowball to w2 and further
  for(nobelist in nobelists) {
    print(paste("Processing",nobelist))
    # grab Treatment T9 numbers and extract their base information
    T_T9 <- unique(as.integer64(as.numeric(substring(read.csv(paste0(wave,"/startfiles/",nobelist),head=F)$V1,3))))
    T <- wos_C[match(T_T9,T9),]
    # grab a Control group for that T 
    C <- wos_C[is.na(match(wos_C$T9,T_T9)) & !is.na(match(wos_C$hash,T$hash)),] #matches hash but is not a treated paper
    C <- C[!duplicated(C$T9),]
    
    # ... and remove "heros" from C (delete treated authors from C and remove prior wave authors from subsequent waves)
    if(wave=="w1") {
      print("removing w1 heros from C")
      C <- C[w1_heros==FALSE,]
    }
    if(wave=="w2") {
      print("removing w1 heros from T")
      T <- T[w1_heros==FALSE,]
      print("removing w1 and w2 heros from C")
      C <- C[w1_heros==FALSE & w2_heros==FALSE,]  
    }
    if(wave=="w3") {
      print("removing w1 and w2 heros from T")
      T <- T[w1_heros==FALSE & w2_heros==FALSE,]
      print("removing w1 and w2 heros from C")
      C <- C[w1_heros==FALSE & w2_heros==FALSE & w3_heros==FALSE,]
    }
    
    # for some papers we may not find a control. those Ts have to be removed:
    allT <- nrow(T)
    T <- T[!is.na(match(T$hash,C$hash)),]
    print(paste0(allT-nrow(T)," (",round((allT-nrow(T))/allT,2),"%) papers dont have a CE-Match by Hash and are deleted from T"))

    # count occurences of "hash" within T and C to derive weight (e.g. 5 Papers with same Hash and 20 Controls with that hash yields a weight of 5/20)
    T[,papercount:=.N,by="hash"]
    C[,matchcount:=.N,by="hash"]
    T$matchcount <- C$matchcount[match(T$hash,C$hash)]
    C$papercount <- T$papercount[match(C$hash,T$hash)]
    T[,T:=1]
    C[,T:=0]
    TC <- rbindlist(list(T,C),use.names = TRUE)
    TC[,weight:=papercount/matchcount]
    TC[T==1,weight:=ifelse(!is.na(weight),1,NA)]
    
    TC[,c("papercount","matchcount"):=NULL,with=FALSE]
    
    
    # get the long data
    T_long <- wos_long_C[!is.na(match(wos_long_C$ID,T$T9)),]
    C_long <- wos_long_C[!is.na(match(wos_long_C$ID,C$T9)),]
    T_long[,T:=1]
    C_long[,T:=0]
    TC_long <- rbindlist(list(T_long,C_long))
    TC_long[,coarse_Year:=cut(Year,breaks=c(1898,seq(1910,1980,10),1990:2014))] #data are between 1899 and 2013 (2014)
    # reshape
    TC_wide <- dcast.data.table(TC_long, ID~coarse_Year,value.var="n",fun.aggregate = sum)
    setnames(TC_wide,paste0("sum",gsub("[^[:alnum:][:blank:]+?&/\\]","",names(TC_wide)))) # proper col names
    setnames(TC_wide,1,"ID")
    # re-add all base information from wos_C
    TC_wide[,c("J1","PY","TI","AU","perzentil","coarse_PY","hash","T","weight","SC"):=TC[match(TC_wide$ID,TC$T9),list(J1,PY,TI,AU,perzentil,coarse_PY,hash,T,weight,SC)],with=F]
    # give proper type for stata to understand (no integer64)
    TC_wide$ID <- as.integer(TC_wide$ID)
    write_dta(TC_wide,paste0("stata_cem/",wave,"/",nobelist,".dta"))
  }
}

# From here we switch to Stata (ebalance_loop.do). Then we proceed with the balanced files
# lets make one huge file with the balanced data

nobelists <- list.files("w1/startfiles/")
nobelyears <- as.data.table(read.csv("source_files/UT Jahr Nobelist.csv",sep=";",header=FALSE))[,3:4,with=FALSE]
setnames(nobelyears,c("NY","Nobelist"))
setkey(nobelyears,"Nobelist")
nobelyears<-unique(nobelyears)

for(wave in list("w1","w2","w3")) {
  print(paste("Processing",wave))
  if(wave=="w2") nobelists <- nobelists[nobelists!="Spence"] #remove Spence after w1 is done because Spence doesnt snowball to w2 and further
  for(nobelist in nobelists) {
    print(paste("Processing",nobelist,"N: ",nrow(balanced)," cases"))
    balanced <- as.data.table(read_dta(paste0("ebalanced/",wave,"/",nobelist,".dta")))
    balanced$Nobelist <- nobelist
    balanced$NY <- nobelyears[Nobelist==nobelist,NY]
    balanced$Wave <- wave
    balanced_all <- rbindlist(list(balanced_all,balanced),use.names=TRUE,fill=TRUE)
  }
}

#balanced_all$ID <- as.integer64(balanced_all$ID)
rm(wos_C); gc()
balanced_all_long <- wos_long_C[!is.na(match(wos_long_C$ID,balanced_all$ID)),]
rm(wos_long_C); gc()
balanced_all_long[,c("J1","TI","AU","coarse_PY","hash","T","weight","webal","SC","Nobelist","NY","Wave"):=balanced_all[match(balanced_all_long$ID,balanced_all$ID),list(J1,TI,AU,coarse_PY,hash,T,weight,webal,SC,Nobelist,NY,Wave)],with=FALSE]
gc()
# make a summary (weighted means of each Year/Wave/NY for T and C)
balanced_all_long[,mean_n:=weighted.mean(n,webal,na.rm=TRUE),by=list(Wave,NY,T,Year)]
setkey(balanced_all_long,Wave,NY,T,Year)
balanced_all_summary <- unique(balanced_all_long)
balanced_all_summary[,c("J1","TI","AU","coarse_PY","hash","weight","webal","SC","Nobelist","ID"):=NULL,with=FALSE]
dim(balanced_all_summary)
rm(balanced_all_long); gc()   
balanced_all_summary$T <- factor(balanced_all_summary$T, levels=c(1,0),  labels=c("Treatment", "Control"))

