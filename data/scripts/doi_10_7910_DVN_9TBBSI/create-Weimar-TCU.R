#################################################################################
### Script to create time-constant Weimar units from 1924-1933 (a la King et al, 2008)
#################################################################################
library(foreign)
gesis <- read.dta("/Users/Kostya/Desktop/Dropbox/German Elections Project/German Project/Data/create-1890s-Weimar-units/data_weimar/raw_data/ZA8013_Wahldaten.dta")

### get rid of gemeinde-level GESIS data
nrow(gesis)
gesis$agglvl <- as.character(as.vector(gesis$agglvl))
gesis$name <- as.character(as.vector(gesis$name))

### drop gemeinde from GESIS data
gesis <- gesis[gesis$agglvl!="GEMEINDEN AB 2000 E." & gesis$agglvl!="RESTKREISE (GEM.< 20",]
nrow(gesis) #1246 kreise

### set missing values to NAs
gesis[gesis=="-9"] <- NA

## which units have 0 krnr_king and still have non-NA population data?
gesis[!is.na(gesis$n245pop) & gesis$krnr_king==0 ,c("krnr","name")]

### check where else there are pop values not taken into account
table.sub <- gesis[gesis$krnr_king==0 ,c("krnr","name", "n24dpop", "n285pop", "n309pop", "n327pop", "n32npop", "n333pop")]
table.sub[apply(table.sub,MARGIN=1,function(x) sum(is.na(x))!=6),]


# manually fix these codes
gesis[gesis$krnr==26,]$krnr_king <- 26
gesis[gesis$krnr==243,]$krnr_king <- 243
gesis[gesis$krnr==430,]$krnr_king <- 430
gesis[gesis$krnr==502,]$krnr_king <- 367 # ilfeld into hohenstein/nordhausen/worbis
gesis[gesis$krnr==718,]$krnr_king <- 718
gesis[gesis$krnr==723,]$krnr_king <- 723
gesis[gesis$krnr==878,]$krnr_king <- 878
gesis[gesis$krnr==907,]$krnr_king <- 907
gesis[gesis$krnr==1068,]$krnr_king <- 1068
gesis[gesis$krnr==1076,]$krnr_king <- 1076
gesis[gesis$krnr==1113,]$krnr_king <- 1113
gesis[gesis$krnr==1114,]$krnr_king <- 1114
gesis[gesis$krnr==1116,]$krnr_king <- 1116
gesis[gesis$krnr==1133,]$krnr_king <- 1133
gesis[gesis$krnr==1155,]$krnr_king <- 1155
gesis[gesis$krnr==1159,]$krnr_king <- 1159
gesis[gesis$krnr==1198,]$krnr_king <- 1212

# combine Kreis Der Twiste
gesis[gesis$krnr==644,]$krnr_king <- 644
gesis[gesis$krnr==678,]$krnr_king <- 644

# fix kreis des eisenberges (should be by itself)
gesis[gesis$krnr==645,]$krnr_king <- 645
gesis[gesis$krnr==679,]$krnr_king <- 645

# fix kreis der eder (should be by itself)
gesis[gesis$krnr==643,]$krnr_king <- 643
gesis[gesis$krnr==677,]$krnr_king <- 643

# I have checked population for all of these. They are all very stable over time.


########
# put Dillenburg and Wetzlar into krnr_king 655
gesis[gesis$krnr==656,]$krnr_king <- 655
gesis[gesis$krnr==673,]$krnr_king <- 655

# add Ruegen to self
gesis[gesis$krnr==148,]$krnr_king <- 176

# add Franzburg-Barth to self
gesis[gesis$krnr==138,]$krnr_king <- 172

# RHEINISCH-BERGISCHER KREIS is composed of MUELHEIM and WIPPERFUERTH
gesis[gesis$krnr==690,]$krnr_king <- 687
gesis[gesis$krnr==687,]$krnr_king <- 687
gesis[gesis$krnr==693,]$krnr_king <- 687
# also merged originally with SIEGKREIS in King et al data b/c of boundary adjustment b/w RHEINISCH-BERGISCHER KREIS, SIEGKREI< and OBERBERGISCHER in 1932

# let's separate out SIEGKREIS b/c pop doesn't change a ton in 1932
gesis[gesis$krnr==691,]$krnr_king <- 691

# OBERBERGISCHER KREIS is composed of GUMMERSBACH and WALDBROEL
gesis[gesis$krnr==688,]$krnr_king <- 684

# GRAFSCHAFT SCHAUMBURG to self
gesis[gesis$krnr==551,]$krnr_king <- 550

# Rybnik to GLEIWITZ S/TOST-GLEIWITZ
gesis[gesis$krnr==256,]$krnr_king <- 257

# RATHENOW S with Westhavelland
gesis[gesis$krnr==96,]$krnr_king <- 92

# Detmold and Lemgo into krnr_king==567
gesis[gesis$krnr==568,]$krnr_king <- 567
gesis[gesis$krnr==574,]$krnr_king <- 567

# UNNA (-16.10.30 HAMM L) into HAMM L
gesis[gesis$krnr==620,]$krnr_king <- 603

# WANNE-EICKEL  S into GELSENKIRCHEN
gesis[gesis$krnr==621,]$krnr_king <- 598

# NOWAWES -S into Teltow
gesis[gesis$krnr==74,]$krnr_king <- 94

# GOLDBERG into GOLDBERG-HAYNAU
gesis[gesis$krnr==212,]$krnr_king <- 226

# CUXHAVEN into 1187 (Hamburg + CUXHAVEN + RITZEBUETTEL)
gesis[gesis$krnr==1190,]$krnr_king <- 1187

# separate Meiningen from HERRSCHAFT SCHMALKALDEN
gesis[gesis$krnr==335,]$krnr_king <- 335

# move Zella-Mehlis into Meiningen
gesis[gesis$krnr==355,]$krnr_king <- 335

# merge Wesermuende S (475), Geestemunde (462), and Lehe (468), and Wesermuende LK (474)
gesis[gesis$krnr==475,]$krnr_king <- 475
gesis[gesis$krnr==462,]$krnr_king <- 475
gesis[gesis$krnr==468,]$krnr_king <- 475
gesis[gesis$krnr==474,]$krnr_king <- 475

# merge Bremervoerde 461 and zeven (476)
gesis[gesis$krnr==461,]$krnr_king <- 461
gesis[gesis$krnr==476,]$krnr_king <- 461

# separate out bremerhaven 439
gesis[gesis$krnr==439,]$krnr_king <- 439

## vegesack (440) to bremen
gesis[gesis$krnr==440,]$krnr_king <- 437


# merge the following to Berlin (code 48):
gesis[gesis$krnr_king==49,]$krnr_king <- 48
gesis[gesis$krnr_king==51,]$krnr_king <- 48
gesis[gesis$krnr_king==52,]$krnr_king <- 48
gesis[gesis$krnr_king==53,]$krnr_king <- 48
gesis[gesis$krnr_king==55,]$krnr_king <- 48
gesis[gesis$krnr_king==57,]$krnr_king <- 48
gesis[gesis$krnr_king==61,]$krnr_king <- 48
gesis[gesis$krnr_king==63,]$krnr_king <- 48
gesis[gesis$krnr_king==64,]$krnr_king <- 48
gesis[gesis$krnr_king==67,]$krnr_king <- 48
gesis[gesis$krnr_king==68,]$krnr_king <- 48
gesis[gesis$krnr_king==80,]$krnr_king <- 48
gesis[gesis$krnr_king==81,]$krnr_king <- 48
gesis[gesis$krnr_king==83,]$krnr_king <- 48

########
# create list
gesisKRNRList <- by(gesis,gesis$krnr_king,function(x) x[,"krnr"])

# omit krnr_king==0
gesisKRNRList <- gesisKRNRList[-1]

########################################
### checks on data
########################################

########################################
### population check
########################################

gesis.colyears <- which(colnames(gesis) %in% c("n245pop", "n24dpop", "n285pop", "n309pop", "n327pop", "n32npop", "n333pop"))


#####
pop.df <- as.data.frame(matrix(data=NA, ncol=(length(gesis.colyears)), nrow=length(gesisKRNRList))) 
colnames(pop.df) <- c("1924a", "1924b", "1928", "1930", "1932a", "1932b", "1933")
rownames(pop.df) <- names(gesisKRNRList)

for(l in 1:length(gesisKRNRList)){
		gesis.temp <- as.matrix(gesis[which(gesis$krnr %in% gesisKRNRList[[l]] ==TRUE),gesis.colyears])
		pop.df[l,] <- colSums(gesis.temp,na.rm=TRUE)
	}

### which have 0 population?
pop.df[apply(pop.df,MARGIN=1,function(x) any(x==0)),]

### calculate % change in population
pop.df.perc <- pop.df
for(c in 2:ncol(pop.df.perc)){
	pop.df.perc[,c] <- (pop.df[,c]-pop.df[,c-1])/pop.df[,c-1]
}

pop.df.perc[,1] <- 0
pop.df.perc <- pop.df.perc>.10
pop.df.perc[is.na(pop.df.perc)] <- FALSE

# print out list of king_krnr and krnr where there is a population jump of greater than 10%
gesisKRNRList[which(apply(pop.df.perc,MARGIN=1, function(x) any(x>0.10)))]

length(gesisKRNRList[which(apply(pop.df.perc,MARGIN=1, function(x) any(x>0.10)))])

pdf("/Users/Kostya/Desktop/Dropbox/German Elections Project/German Project/Data/create-1890s-Weimar-units/data_weimar/crosswalks/validation/kreise_population.pdf", height=8.5, width=11, paper="special")
par(mfrow=c(4,4),mgp=c(2.75,1,0)*0.55, mar=c(1.6,1.5,0.5,1)*1.6,omi=c(0.2,0.5,0.4,0), tcl=-0.25, bg="white", cex=0.85)
for (i in 1:nrow(pop.df)) {
  plot(c(1924,1924,1928,1930,1932,1932,1933),pop.df[i,],type="b",ylab=NA,xlab=NA,bty="l",las=1, main=rownames(pop.df)[i])
  for(y in 1:ncol(pop.df.perc)){
  	if(pop.df.perc[i,y]){
  		points(c(1924,1924,1928,1930,1932,1932,1933)[y],pop.df[i,y],col="red",pch=19,cex=2)
  	}
  }
#  grid(lty=1,col="grey")
}
  dev.off()

#########

### i also did a check using >20% as a margin, and there was only 1 such case:
#> gesis[gesis$krnr_king==68,"name"]
#[1] "ZEHLENDORF BA 10"
#(part of Berlin or Teltow)


###########################
### save crosswalk
###########################
### save matches of King et al codes to GESIS krnr numbers
save(gesisKRNRList, file="/Users/Kostya/Desktop/Dropbox/German Elections Project/German Project/Data/create-1890s-Weimar-units/data_weimar/crosswalks/King-to-GESIS.RData")