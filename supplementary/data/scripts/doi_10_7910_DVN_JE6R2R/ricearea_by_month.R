# R PACKAGES USED
library(manipulateR)
library(plyr)

# INPUT DATA
props <- read.csv("Data/RiceApy.csv", stringsAsFactors=FALSE)
iso <- read.csv("Data/country_groupings.csv", stringsAsFactors=FALSE)

# INTERMEDIATE DATA CONTAINERS
dat <- gaps <- warn2 <- warn <- vector()

for (i in 1:nrow(props)){
	
	for (j in 1:3){
		timecrop <- paste(c("ST", "PK", "END"), j, sep="")
		
		message(props$ISO[i],props$FID_2[i],": Retrieving planting dates for season ", j, "\r", appendLF=FALSE)
		flush.console()
		start <- props[i, paste("PLANT",timecrop, sep="_")]
		start[trimws(start)=="-"] <- 0
		start <- as.numeric(start)
		start[start==0] <- NA
		start <- start[!is.na(start)]
		start <- as.numeric(format(dateFromDoy(start, 2001), "%m"))
		
		if(length(start)<1){
			message(props$ISO[i],props$FID_2[i],": no season ", j, appendLF=TRUE)
		  flush.console()
			next
		} 
		for (k in 1:(length(start)-1)){
			if (start[k]>start[(k+1)]){
				start[(k+1):length(start)] <- start[(k+1):length(start)]+12
				break
			}
		}
		
		message(props$ISO[i],props$FID_2[i],": Retrieving harvest dates for season ", j, "\r", appendLF=FALSE)
		flush.console()
		end <- props[i, paste("HARV",timecrop, sep="_")]
		end[trimws(end)=="-"] <- 0
		end <- as.numeric(end)
		end[end==0] <- NA
		end <- end[!is.na(end)]
		end <- as.numeric(format(dateFromDoy(end,2001), "%m"))
		
		if(length(end)<1) {
			message(props$ISO[i],props$FID_2[i],": no harvest dates for season ", j, appendLF=TRUE)
		  flush.console()
			next
		} 
		
		for (k in 1:(length(end)-1)){
			if (end[k]>end[(k+1)]){
				end[(k+1):length(end)] <- end[(k+1):length(end)]+12
				break
			}
		}
		
		if(length(start)!=length(end)){
			message(props$ISO[i],props$FID_2[i],": Inconsistent date data for season ", j, appendLF=TRUE)
		  flush.console()
			warn2 <- rbind(warn2, c(i, j))
			next
		}
		
		PMO <- props[i,grep(paste("PMO_S",j, sep=""),colnames(props))[1:12]]
		PMO[grep("-",PMO)] <- 0
		PMO <- as.numeric(PMO)
		pgap <- sum(diff(which(PMO!=0))>1) 
		
		HMO <- props[i,grep(paste("HMO_S",j, sep=""),colnames(props))[1:12]]
		HMO[grep("-",HMO)] <- 0
		HMO <- as.numeric(HMO)
		hgap <- sum(diff(which(HMO!=0))>1)
		
		if (pgap|hgap|(sum(start>end)>0)) {
			if(sum(start>end)>0){
				end[end<start] <- end[end<start]+12
			}
			gaps <- rbind(gaps,c(i,j))
			PMO <- rep(PMO,2)
			PMO[1:(min(start)-1)] <- 0
			PMO[(max(start)+1):length(PMO)] <- 0
			
			HMO <- rep(HMO,2)
			HMO[1:(min(end)-1)] <- 0
			HMO[(max(end)+1):length(HMO)] <- 0
		}
		
		message(props$ISO[i],props$FID_2[i],": Computing monthly rice areas by month for season ", j, "\r", appendLF=FALSE)
		flush.console()
		AREA <- as.numeric(sub(",","",trimws(props[i,grep(paste("A_S",j, sep=""),colnames(props))])))

		# CHECK IF PROVIDED PERCENTAGES COINCIDE WITH SEASON DURATION
		if(min(start)!=min(which(PMO>0))|max(end)!=max(which(HMO>0))){
			message(paste(props[i,c(1,3,7)],collapse=","), ": Percentages provided doesn't match season start and end on season ", j, appendLF = TRUE)
		  flush.console()
			warn <- c(warn,i)
			next
		} 
		
		PMO <- cumsum(PMO)
		HMO <- cumsum(HMO)		
		
		MO_AREA <- (PMO-HMO)*AREA
		if(length(MO_AREA)>12){
			MO_AREA <- colSums(matrix(MO_AREA, ncol=12, byrow=TRUE))
		}
		dat <- rbind(dat, c(props$FID_2[i],j,MO_AREA))
		rm(PMO,HMO,AREA,MO_AREA,start,end,pgap, hgap, timecrop)
		gc(reset=TRUE)
		message(props$ISO[i],props$FID_2[i],": Done for season ", j, appendLF=TRUE)
		flush.console()
	}	
}

colnames(dat) <- c("FID_2", "season", format(ISOdate(2004,1:12,1),"%b"))
by.spatunit <- join(data.frame(dat),props[,c("FID_2", "ISO")], by="FID_2")
by.spatunit <- join(by.spatunit,iso[,1:3], by="ISO")

by.iso <- aggregate(by.spatunit[,format(ISOdate(2004,1:12,1),"%b")], by=list(by.spatunit$ISO, by.spatunit$CONTINENT, by.spatunit$COUNTRY), FUN=sum)
