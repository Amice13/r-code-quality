######################################################################
## Prepares a data set from the output of the estimations           ##
## ---------------------------------------------------------------- ##
## ADW: Presidents, Policy Compromise and Legislative Success. JOP. ##
## June 2016                                                        ##
## Implemented in R 3.2.3 "Wooden Christmas-Tree"                   ##
######################################################################

# --------------------------------------------------------------------
# gives a vector with names
dat.a.wf.out <- function(names.all){
	#### The data matrix
	# Create variables
	new.pres <- pres <- cyvec <- year <- coun  <- se.lb <- se.up <- new.coun <- rep(NA, length(names.all))
	# Country (takes name from the names vector) 
	coun[grepl("per", names.all)] <- "per"	# Peru is first to avoid the conflict grepl causes with 'SamPER' in the colombia data
	coun[grepl("arg", names.all)] <- "arg"	
	coun[grepl("bra", names.all)] <- "bra"	
	coun[grepl("chi", names.all)] <- "chi"
	coun[grepl("col", names.all)] <- "col"
	coun[grepl("cri", names.all)] <- "cri"
	coun[grepl("ecu", names.all)] <- "ecu"
	coun[grepl("gtm", names.all)] <- "gtm"
	coun[grepl("mex", names.all)] <- "mex"	
	coun[grepl("pry", names.all)] <- "pry"
	coun[grepl("slv", names.all)] <- "slv"	
	coun[grepl("ury", names.all)] <- "ury"
	coun[grepl("ven", names.all)] <- "ven"
	# Year (takes name from the names vector)
	for (i in seq(1979, 2015,1)){
		year[grepl(i, names.all)] <- i
	}
	# New country (whenever the year count is larger than 1, it must start again from the beginning)
	new.coun <- c(TRUE, rep(FALSE,length(names.all)-1))
	# President and Newpresident 
	for(i in 1:length(names.all)){
		# 1.1 combine country and year and subtract it from the name value
		cyvec <- paste(coun[i],year[i],"",sep="_")
		pres[i] <- sub(cyvec, '', names.all[i])  ## spaces only
		# 1.2 subtract the ending
		pres[i] <- sub(".txt", "", pres[i]) 
		# 
		# 2 check if there is a new president
		# fork to check for the first one in the row
		if (i==1) {new.pres[1] <- TRUE}
		else {new.pres[i] <- (pres[i]!=pres[i-1])}
	}
	# Bind it up in data frame		
	dat.pres.pos <- data.frame(coun, new.coun, year, pres,new.pres)	
	# output
	return(dat.pres.pos[seq(length(dat.pres.pos[,1]),1),])
}


# --------------------------------------------------------------------
# Implement for all countries
dat.pres.pos.arg80 <- dat.a.wf.out(names.arg)
dat.pres.pos.bra80 <- dat.a.wf.out(names.bra)
dat.pres.pos.chi80 <- dat.a.wf.out(names.chi)
dat.pres.pos.col80 <- dat.a.wf.out(names.col)
dat.pres.pos.cri80 <- dat.a.wf.out(names.cri)
dat.pres.pos.ecu80 <- dat.a.wf.out(names.ecu)
dat.pres.pos.gtm80 <- dat.a.wf.out(names.gtm)
dat.pres.pos.mex80 <- dat.a.wf.out(names.mex)
dat.pres.pos.per80 <- dat.a.wf.out(names.per)
dat.pres.pos.pry80 <- dat.a.wf.out(names.pry)
dat.pres.pos.slv80 <- dat.a.wf.out(names.slv)
dat.pres.pos.ury80 <- dat.a.wf.out(names.ury)
dat.pres.pos.ven80 <- dat.a.wf.out(names.ven)


