rm(list=ls())

library(readxl)			# Read Excel inputs
library(miceadds)		# For regression with clustered standard errors (lm.cluster)
library(lfe)			# For fixed effects regression (felm).

## Load and clean compliance data
load("Data/EUTL2021.Rdata")
ds <- ds[ds$Year>=ds$FirstYearOfEmission,]
ds <- ds[ds$Year<=ds$LastYearOfEmissions | ds$LastYearOfEmissions==0,]
art21 <- d

## Load and clean aggregate emissions data
d <- read.csv2("Data/(Chart)_Historical_emissions_data.csv", sep="\t", fileEncoding = "UCS-2LE", stringsAsFactors=FALSE) # Source EEA: https://www.eea.europa.eu/data-and-maps/dashboards/emissions-trading-viewer-1 (retreived August 1, 2021).
names(d) <- c("Main.Activity.Sector.Name","Country","Year","ETS.information","Emissions.Unit","Quantity")
d$Quantity <- as.numeric(gsub(" ", "", d$Quantity))

# Load EUR prices
dp <- as.data.frame(read_excel("Data/EmissionTradingPrices-23-01-2024..xlsx", sheet="icap-graph-data-23-01-2024", range = "A5:C6944", col_types = "text"))
dp$Date <- as.Date(as.numeric(dp$"Date as Text"), origin = "1899-12-30")
dp$price <- as.numeric(dp$"Domestic Currency (EUR)")

# Appendix: Figure 6
quartz(width = 8, height = 5, type = "pdf", "prices", file = "prices.pdf")
plot(dp$Date, dp$price, type="n",bty="n", xlim=c(min(dp$Date),max(dp$Date)), ylim=c(0,100), axes=F, xaxs="i", yaxs="i", ylab="", xlab="", xaxt='n', yaxt='n', main="")	#
mtext(side=3, "EUA price (€)", outer=T, line=-3, cex=1.1, at=0.07)
axis(1, at=as.Date(paste(seq(2005,2025, 5), "-01-01", sep="")), labels=c(seq(2005,2025, 5)), las=1)
axis(2, at=seq(0,100, 20), las=1)
lines(dp$Date, dp$price, col=rgb(51, 51, 179, alpha=200, max=256), lwd=1)
dev.off()


## Calculate annual allocated, verified, surrendered, and imported emissions allowances
caps <- data.frame(year=2005:2020, allocated=NA, verified=NA, surrendered=NA, import=NA, savings=NA, bank=0) 
for (i in 2005:2020) {
	caps$allocated[caps$year==i] <- d$Quantity[d$ETS.information=="1. Total allocated allowances (EUA or EUAA)" & d$Year==i]
	caps$verified[caps$year==i] <- d$Quantity[d$ETS.information=="2. Verified emissions" & d$Year==i]
	caps$surrendered[caps$year==i] <- d$Quantity[d$ETS.information=="4. Total surrendered units" & d$Year==i]
	if (i<2013) { caps$import[caps$year==i] <- d$Quantity[d$ETS.information=="4.2 Surrendered certified emission reductions (CERs)" & d$Year==i] + d$Quantity[d$ETS.information=="4.3 Surrendered emission reduction units (ERUs)" & d$Year==i] }	#
	
	# Trends and Projections in the EU ETS, https://www.eea.europa.eu/publications#c7=en&c11=5&c14=&c12=&b_start=0&c13=trends+and+projections+in+the+EU+ETS, URL date August 10, 2021.
	if (i==2013) { caps$import[caps$year==i] <- 175 * (10^6) } # Source: Trends and projections in the EU ETS in 2015
	if (i==2014) { caps$import[caps$year==i] <- 255 * (10^6) } # Source: Trends and projections in the EU ETS in 2015
	if (i==2015) { caps$import[caps$year==i] <- 28 * (10^6) } # Source: Trends and projections in the EU ETS in 2016
	if (i==2016) { caps$import[caps$year==i] <- 12.0 * (10^6) } # Source: Trends and projections in the EU ETS in 2017
	if (i==2017) { caps$import[caps$year==i] <- 11.7 * (10^6) } # Source: Trends and projections in the EU ETS in 2018
	if (i==2018) { caps$import[caps$year==i] <- (435.73-423.16) * (10^6) } # Source: Report on the functioning of the European carbon market 2017 and 2018. We compute the difference in cumulative CERs and ERUs converted to EUAs between June 2017 and June 2018.
	if (i==2019) { caps$import[caps$year==i] <- (453.49-435.73) * (10^6) } # Source: Report on the functioning of the European carbon market 2018 and 2019. We compute the difference in cumulative CERs and ERUs converted to EUAs between June 2018 and June 2019.
	if (i==2020) { caps$import[caps$year==i] <- (480.94-453.49) * (10^6) } # Source: Report on the functioning of the European carbon market 2019 and 2020. We compute the difference in cumulative CERs and ERUs converted to EUAs between June 2019 and June 2020.
	
	caps$savings[caps$year==i] <- caps$allocated[caps$year==i] + replace(caps$import[caps$year==i],is.na(caps$import[caps$year==i]),0) - caps$surrendered[caps$year==i]	# Allowances from year i available for future compliance
	if (!(i %in% c(2005,2008))) {
		caps$bank[caps$year==i] <- caps$bank[caps$year==i-1] + caps$savings[caps$year==i-1] + min(0,caps$savings[caps$year==i])	# All previously saved allowances, plus last periods savings, less any shortfall in the present period that will need to be met from accumulated savings.
		if (caps$savings[caps$year==i]<0 & caps$bank[caps$year==i]>=0) { caps$savings[caps$year==i] <- 0 }	# When the shortfall in the present period is met from accumulated savings, zero out the savings so the shortfall isn't deducted from next periods bank.
	}
}

## Figure 1
quartz(width = 9, height = 7, type = "pdf", "aggregate_plot", file = "aggregate_plot.pdf")
plot(caps$year,caps$verified/(10^6), type="n", xlim=c(2005,2020), ylim=c(0,4900), bty="l",axes=F,xaxs="i",yaxs="i",ylab="",xlab="",xaxt='n',yaxt='n')
polygon(c(caps$year,rev(caps$year)), c((caps$allocated + caps$import + caps$bank) / (10^6), rep(0, nrow(caps))), col="#D9D9FF", border = "#D9D9FF")
polygon(c(caps$year,rev(caps$year)), c((caps$allocated + caps$import)/(10^6),rep(0,nrow(caps))), col="#A1A1E5", border = "#A1A1E5")
polygon(c(caps$year,rev(caps$year)), c(caps$allocated/(10^6),rep(0,nrow(caps))), col="#3333B3", border = "#3333B3")

lines(caps$year,c((caps$allocated + caps$import + caps$bank) / (10^6)))
lines(caps$year,caps$verified/(10^6))
points(caps$year,caps$verified/(10^6),pch=16)

axis(1,at=c(2005:2020),labels=rep("",length(2005:2020)))
axis(1,at=seq(2005,2020,5))
abline(v=2007.5,lty=2)
abline(v=2012.5,lty=2)
axis(2,seq(0,5000,1000),las=2)
text(x=2006,y=4800, "Phase 1")
text(x=2008.5,y=4800, "Phase 2")
text(x=2013.5,y=4800, "Phase 3")

text(x=2012.5,y=1000, pos=4, "EU allowance allocation")
text(x=2012.5,y=2300, pos=4, "International offsets")
text(x=2012.5,y=3000, pos=4, "Banked EU allowances")
text(x=2008,y=3700, pos=4, "Total allowances available\n for compliance")

legend(x=2013, y=4700, "Verified emissions", bty = "n", lty=1, pch=16, cex=0.9)
mtext(side=3, expression('MtCO'[2]*'e'), outer=T, line=-4, cex=1.1, at=0.07)
dev.off()


## Verfied emissions as a fraction of cap.
sum(caps$allocated[caps$year %in% c(2005:2020)])/10^9	# Total allocated EUAs
sum(caps$import[caps$year %in% c(2005:2020)])/10^9		# Total imported allowances
sum(caps$verified[caps$year %in% c(2005:2020)])/10^9		# Total verified emissions
sum(caps$verified[caps$year %in% c(2005:2020)]) / (sum(caps$allocated[caps$year %in% c(2005:2020)])+sum(caps$import[caps$year %in% c(2005:2020)])) # Total emissions as a fraction of total available allowances.

sum(caps$verified[caps$year %in% c(2005:2007)]) / sum(caps$allocated[caps$year %in% c(2005:2007)]) # Total emissions as a fraction of total allocation in Phase 1.

# sum(caps$verified[caps$year %in% c(2008:2012)]) / sum(caps$allocated[caps$year %in% c(2008:2012)]) # Phase 2
# sum(caps$verified[caps$year %in% c(2008:2012)]) / (sum(caps$allocated[caps$year %in% c(2008:2012)])+sum(caps$import[caps$year %in% c(2008:2012)])) # Phase 2.
#
# sum(caps$verified[caps$year %in% c(2013:2020)]) / sum(caps$allocated[caps$year %in% c(2013:2020)]) # Phase 3
# sum(caps$verified[caps$year %in% c(2013:2020)]) / (sum(caps$allocated[caps$year %in% c(2013:2020)])+sum(caps$import[caps$year %in% c(2013:2020)])) # Phase 3. minus international offsets.


## Noncompliance rates
### Appendix: Table 3
table(ds$ComplianceCode, ds$Year)
ds$compliancetemp <- NA
ds$compliancetemp[ds$ComplianceCode=="A" | ds$ComplianceCode=="A*"] <- "A/A*"
ds$compliancetemp[ds$ComplianceCode=="B" | ds$ComplianceCode=="B*"] <- "B/B*"
ds$compliancetemp[ds$ComplianceCode=="C" | ds$ComplianceCode=="C*"] <- "C/C*"
ds$compliancetemp[ds$ComplianceCode=="D" | ds$ComplianceCode=="D*"] <- "D/D*"
ds$compliancetemp[ds$ComplianceCode=="E" | ds$ComplianceCode=="E*"] <- "E/E*"
ds$compliancetemp[ds$ComplianceCode=="-"] <- "-"
ds$compliancetemp[ds$ComplianceCode==""] <- ""
table(ds$compliancetemp, ds$Year)

### Instances of noncompliance
sum(ds$compliancetemp=="B/B*" | ds$compliancetemp=="D/D*", na.rm=TRUE)
sum(ds$compliancetemp=="C/C*", na.rm=TRUE)

### Noncompliance rates using official non-compliance codes
ds$ComplianceCode[ds$ComplianceCode==""] <- NA
length(unique(ds$EID[ds$Year %in% c(2005) & ds$ComplianceCode %in% c("B","B*","D","D*")])) / length(unique(ds$EID[ds$Year %in% c(2005) & !is.na(ds$ComplianceCode)]))	# Share of installations non-compliant in 2005
length(unique(ds$EID[ds$Year %in% c(2006) & ds$ComplianceCode %in% c("B","B*","D","D*")])) / length(unique(ds$EID[ds$Year %in% c(2006) & !is.na(ds$ComplianceCode)]))	# Share of installations non-compliant in 2006
length(unique(ds$EID[ds$Year %in% c(2007) & ds$ComplianceCode %in% c("B","B*","D","D*")])) / length(unique(ds$EID[ds$Year %in% c(2007) & !is.na(ds$ComplianceCode)]))	# Share of installations non-compliant in 2007

### Fill in missing values of verified emissions and surrendered allowances based on available cumulative figures
ds <- ds[order(ds$NationalAdministratorCodeCorrected, ds$InstallationOrAircraftOperatorID, ds$Year),]
tas_diff <- c(NA, ds$CumulativeSurrenderedUnits[-1] - ds$CumulativeSurrenderedUnits[-nrow(ds)])
tve_diff <- c(NA, ds$CumulativeVerifiedEmissions[-1] - ds$CumulativeVerifiedEmissions[-nrow(ds)])

for (i in 2:nrow(ds)) {
	if (ds$InstallationOrAircraftOperatorID[i]!=ds$InstallationOrAircraftOperatorID[i-1] | ds$Year[i] %in% c(2005,2008,2013)) {
		if (is.na(ds$UnitsSurrendered[i]) & !is.na(ds$CumulativeSurrenderedUnits[i])) { ds$UnitsSurrendered[i] <- ds$CumulativeSurrenderedUnits[i] }
		if (is.na(ds$VerifiedEmissions[i]) & !is.na(ds$CumulativeVerifiedEmissions[i])) { ds$VerifiedEmissions[i] <- ds$CumulativeVerifiedEmissions[i] } #
	}
	if (ds$InstallationOrAircraftOperatorID[i]==ds$InstallationOrAircraftOperatorID[i-1] & !(ds$Year[i] %in% c(2005,2008,2013))) {
		if (is.na(ds$UnitsSurrendered[i]) & !is.na(tas_diff[i])) { ds$UnitsSurrendered[i] <- tas_diff[i] }
		if (is.na(ds$VerifiedEmissions[i]) & !is.na(tve_diff[i])) { ds$VerifiedEmissions[i] <- tve_diff[i] }
	}
}

### Calculate cumulative verified emissions and surrendered allowances directly, to ensure internal consistency throughout
ds$ID <- paste(ds$NationalAdministratorCodeCorrected, ds$InstallationOrAircraftOperatorID, ds$PermitOrPlanID) # Continue to accumulate obligations as long as the same installatiton is run by the same operator
ds$cus <- ds$cve <- NA
for (i in unique(ds$ID)) {
	ds$cus[ds$ID==i] <- cumsum(ds$UnitsSurrendered[ds$ID==i])
	ds$cve[ds$ID==i] <- cumsum(ds$VerifiedEmissions[ds$ID==i])
}

ds$cus2006 <- ds$cve2006 <- NA	# Cumulative verified emissions and surrendered allowances, starting in 2006.
for (i in unique(ds$ID)) {
	ds$cus2006[ds$ID==i & ds$Year>2005] <- cumsum(ds$UnitsSurrendered[ds$ID==i & ds$Year>2005])
	ds$cve2006[ds$ID==i & ds$Year>2005] <- cumsum(ds$VerifiedEmissions[ds$ID==i & ds$Year>2005])
}


## Non-compliant installation-years
### Non-compliant, according to strict legal definition (including upper and lower bounds)
ds$noncompliant <- !(ds$VerifiedEmissions<=ds$UnitsSurrendered & ds$cve<=ds$cus)
sum(is.na(ds$noncompliant))	# Indeterminate cases
ds$noncompliant_upper <- ds$noncompliant_lower <- ds$noncompliant
ds$noncompliant_upper[is.na(ds$noncompliant)] <- TRUE
ds$noncompliant_lower[is.na(ds$noncompliant)] <- FALSE

### Non-compliant, with banking (including upper and lower bounds)
ds$noncompliant_banking <- !(ds$cve<=ds$cus)
ds$noncompliant_banking_upper <- ds$noncompliant_banking_lower <- ds$noncompliant_banking
ds$noncompliant_banking_upper[is.na(ds$noncompliant_banking)] <- TRUE
ds$noncompliant_banking_lower[is.na(ds$noncompliant_banking)] <- FALSE

### Non-compliant, with banking and allowing for the registry to receive allowances without recording them (FIX)
ds$UnitsSurrenderedLate <- ds$UnitsSurrendered
ds$cusLate <- ds$cus
ids <- unique(ds$ID[!is.na(ds$cus) & !is.na(ds$cve) & ds$cus<ds$cve & ds$ComplianceCode %in% c("A","A*","E","E*")])
for (i in unique(ids)) {
	for (t in unique(ds$Year[ds$ID==i])) {
		if (!is.na(ds$cusLate[ds$ID==i & ds$Year==t]) & !is.na(ds$cve[ds$ID==i & ds$Year==t]) & ds$cusLate[ds$ID==i & ds$Year==t]<ds$cve[ds$ID==i & ds$Year==t] & ds$ComplianceCode[ds$ID==i & ds$Year==t] %in% c("A","A*","E","E*")) {	#
			ds$UnitsSurrenderedLate[ds$ID==i & ds$Year==t] <- ds$UnitsSurrendered[ds$ID==i & ds$Year==t] + (ds$cve-ds$cus)[ds$ID==i & ds$Year==t]	# Impute units surrendered late to cover total outstanding balance
			ds$cusLate[ds$ID==i] <- cumsum(ds$UnitsSurrenderedLate[ds$ID==i])	# Recompute cumulative units surrendered
		}
	}
}
ds$noncompliant_banking_late <- !(ds$cve<=ds$cusLate)


### Summarize non-compliance rates
nc.events <- data.frame(year=2005:2020, noncompliance=NA, noncompliance_upper=NA, noncompliance_lower=NA, noncompliance_banking=NA, noncompliance_banking_upper=NA, noncompliance_banking_lower=NA, noncompliance_banking_late=NA, noncompliance_codes=NA, noncompliance_codes_withC=NA)
for (i in nc.events$year) {
	nc.events$noncompliance[nc.events$year==i] <- nrow(ds[ds$Year==i & ds$noncompliant==TRUE & !is.na(ds$noncompliant),])/nrow(ds[ds$Year==i & !is.na(ds$noncompliant),]) # 
	nc.events$noncompliance_upper[nc.events$year==i] <- nrow(ds[ds$Year==i & ds$noncompliant_upper==TRUE & !is.na(ds$noncompliant_upper),])/nrow(ds[ds$Year==i & !is.na(ds$noncompliant_upper),]) # 
	nc.events$noncompliance_lower[nc.events$year==i] <- nrow(ds[ds$Year==i & ds$noncompliant_lower==TRUE & !is.na(ds$noncompliant_lower),])/nrow(ds[ds$Year==i & !is.na(ds$noncompliant_lower),]) # 
	
	nc.events$noncompliance_banking[nc.events$year==i] <- nrow(ds[ds$Year==i & ds$noncompliant_banking==TRUE & !is.na(ds$noncompliant_banking),])/nrow(ds[ds$Year==i & !is.na(ds$noncompliant_banking),]) #
	nc.events$noncompliance_banking_upper[nc.events$year==i] <- nrow(ds[ds$Year==i & ds$noncompliant_banking_upper==TRUE & !is.na(ds$noncompliant_banking_upper),])/nrow(ds[ds$Year==i & !is.na(ds$noncompliant_banking_upper),]) #
	nc.events$noncompliance_banking_lower[nc.events$year==i] <- nrow(ds[ds$Year==i & ds$noncompliant_banking_lower==TRUE & !is.na(ds$noncompliant_banking_lower),])/nrow(ds[ds$Year==i & !is.na(ds$noncompliant_banking_lower),]) #
	
	nc.events$noncompliance_banking_late[nc.events$year==i] <- nrow(ds[ds$Year==i & ds$noncompliant_banking_late==TRUE & !is.na(ds$noncompliant_banking_late),])/nrow(ds[ds$Year==i & !is.na(ds$noncompliant_banking_late),]) #
	
	nc.events$noncompliance_codes[nc.events$year==i] <- sum(ds$ComplianceCode[ds$Year==i] %in% c("B","B*","D","D*"))/nrow(ds[ds$Year==i & !is.na(ds$ComplianceCode),]) # Registry's self-reported compliance rate
	nc.events$noncompliance_codes_withC[nc.events$year==i] <- sum(ds$ComplianceCode[ds$Year==i] %in% c("B","B*","C","C*","D","D*"))/nrow(ds[ds$Year==i & !is.na(ds$ComplianceCode),]) # Registry's self-reported compliance rate (with C as non-compliance)
}

### Non-compliance rates
mean(nc.events$noncompliance_codes[nc.events$year %in% c(2006:2020)])
mean(nc.events$noncompliance[nc.events$year %in% c(2006:2020)])


## Appendix: Figure 9
quartz(width = 6, height = 4, type = "pdf", "missing", file = "missing.pdf")
par (fig=c(0,1,0,1), # Figure region in the device display region (x1,x2,y1,y2)
	omi=c(0,0,0,0), # global margins in inches (bottom, left, top, right)
	mai=c(.5,.8,.5,.3)) # subplot margins in inches (bottom, left, top, right)

plot(nc.events$year,nc.events$noncompliance,type="n",bty="L", xlim=c(2005,2020), ylim=c(0,.25),axes=F,xaxs="i",yaxs="i",ylab="",xlab="",xaxt='n',yaxt='n', main="")
axis(1,at=c(2005:2020),labels=rep("",length(2005:2020)))
axis(1,at=seq(2005,2020,5))
axis(2,seq(0,.3,.05), label=c("0","5%","10%","15%","20%","25%","30%"),las=2)

polygon(c(2005:2020,2020:2005), c(nc.events$noncompliance_lower,rev(nc.events$noncompliance_upper)), col="#A1A1E5", border = "#A1A1E5")
lines(nc.events$year,nc.events$noncompliance, lty=1, col="#3333B3")
points(nc.events$year,nc.events$noncompliance,pch=16, col="#3333B3")

abline(v=2007.5,lty=2)
abline(v=2012.5,lty=2)
text(x=2006,y=.23, "Phase 1")
text(x=2008.5,y=.23, "Phase 2")
text(x=2013.5,y=.23, "Phase 3")

legend(x=2013, y=.22, "Non-compliance rate\n(missing at random)", lty=1,pch=16,bty="n",cex=.6, col="#3333B3")
text("Range consistent with observation", x=2017, y=.11, cex=.7)
arrows(x0=2014.9, y0=.1, x1 = 2014, y1 = .07, length = 0.1, angle = 30, code = 2, col = par("fg"), lty = par("lty"), lwd = par("lwd"))

dev.off()


## Figure 2
quartz(width = 6, height = 4, type = "pdf", "noncompliance", file = "noncompliance.pdf")
par (fig=c(0,1,0,1), # Figure region in the device display region (x1,x2,y1,y2)
	omi=c(0,0,0,.8), # global margins in inches (bottom, left, top, right)
	mai=c(.5,.8,.5,.3)) # subplot margins in inches (bottom, left, top, right)

plot(nc.events$year,nc.events$noncompliance,type="n",bty="L", xlim=c(2005,2020), ylim=c(0,.25),axes=F,xaxs="i",yaxs="i",ylab="",xlab="",xaxt='n',yaxt='n', main="")
axis(1,at=c(2005:2020),labels=rep("",length(2005:2020)))
axis(1,at=seq(2005,2020,5))
abline(v=2007.5,lty=2)
abline(v=2012.5,lty=2)
axis(2,seq(0,.25,.05), label=c("0","5%","10%","15%","20%","25%"),las=2)
text(x=2006.2,y=.23, "Phase 1")
text(x=2008.7,y=.23, "Phase 2")
text(x=2013.7,y=.23, "Phase 3")

lines(nc.events$year,nc.events$noncompliance,lty=5, col="#A1A1E5") # Legal definition of noncompliance
lines(nc.events$year,nc.events$noncompliance_banking, lty=6, col="#A1A1E5") # Allowing installations to bank with the registry
lines(nc.events$year,nc.events$noncompliance_banking_late, lty=2, col="#A1A1E5") # Allowing installations to bank with the registry, and allowing for the registry to receive allowances without recording them
lines(nc.events$year,nc.events$noncompliance_codes, lty=1, col="#3333B3") # Registry's self-reported compliance rate (excluding installations that had no verified emissions or allocations or surrendered permits, but nevertheless were assigned compliance codes).
points(nc.events$year,nc.events$noncompliance_codes,pch=16, col="#3333B3")
# plot(nc.events$noncompliance_banking_late, nc.events$noncompliance_codes)
legend(x=2013, y=.22, "Official non-\ncompliance rate",lty=1,pch=16,bty="n",cex=.7, col="#3333B3")
mtext("De jure", side=4, line=0,las=2, at=.038, col="#A1A1E5", cex=.7)
mtext("De jure w/ banking", side=4, line=0,las=2, at=.022, col="#A1A1E5", cex=.7)
mtext("De jure w/ banking", side=4, line=0,las=2, at=.012, col="#A1A1E5", cex=.7)
mtext("and late surrender", side=4, line=0,las=2, at=.005, col="#A1A1E5", cex=.7)

dev.off()

## Appendix: Figure 8
quartz(width = 6, height = 4, type = "pdf", "withC", file = "withC.pdf")
par (fig=c(0,1,0,1), # Figure region in the device display region (x1,x2,y1,y2)
	omi=c(0,0,0,0), # global margins in inches (bottom, left, top, right)
	mai=c(.5,.8,.5,.3)) # subplot margins in inches (bottom, left, top, right)

plot(nc.events$year,nc.events$nc.events$noncompliance_codes,type="n",bty="L", xlim=c(2005,2020), ylim=c(0,.25),axes=F,xaxs="i",yaxs="i",ylab="",xlab="",xaxt='n',yaxt='n', main="")
axis(1,at=c(2005:2020),labels=rep("",length(2005:2020)))
axis(1,at=seq(2005,2020,5))
abline(v=2007.5,lty=2)
abline(v=2012.5,lty=2)
axis(2,seq(0,.25,.05), label=c("0","5%","10%","15%","20%","25%"),las=2)
text(x=2006,y=.23, "Phase 1")
text(x=2008.5,y=.23, "Phase 2")
text(x=2013.5,y=.23, "Phase 3")

lines(nc.events$year,nc.events$noncompliance_codes,lty=5, col="#A1A1E5") # Registry's self-reported compliance rate
lines(nc.events$year,nc.events$noncompliance_codes_withC, lty=1, col="#3333B3") 
points(nc.events$year,nc.events$noncompliance_codes_withC,pch=16, col="#3333B3")

legend(x=2013, y=.22, c("Expanded non-\ncompliance rate"),lty=c(1),pch=c(16),bty="n",cex=.7, col=c("#3333B3"))
legend(x=2013, y=.18, c("Official non-\ncompliance rate"),lty=c(5),bty="n",cex=.7, col=c("#A1A1E5"))

dev.off()

## Magnitude of noncompliance shortfall
sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$Year %in% (2005:2020) & ds$ComplianceCode %in% c("B","B*","D","D*")],0, na.rm=T))/10^6 # official total
sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$Year %in% (2005:2007) & ds$ComplianceCode %in% c("B","B*","D","D*")],0, na.rm=T))/10^6 # official Phase 1
sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$Year %in% (2005) & ds$ComplianceCode %in% c("B","B*","D","D*")],0, na.rm=T))/10^6 		# official 2005

sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$Year %in% (2005:2020) & ds$noncompliant==TRUE],0, na.rm=T))/10^6 # strict total

######### Collapse by bvdidnumber #########
df <- ds[!is.na(ds$NationalAdministratorCodeCorrected) & !is.na(ds$bvdidnumber) & !is.na(ds$Year) & !is.na(ds$ComplianceCode) & !is.na(ds$NationalAdministratorCodeCorrected) & !is.na(ds$VerifiedEmissions) & !is.na(ds$UnitsSurrendered),]
df$FirmComplianceCode <- 0
df$FirmComplianceCode[df$ComplianceCode %in% c("B","B*","D","D*")] <- 1
FirmYear <- paste(df$NationalAdministratorCodeCorrected, df$bvdidnumber, df$Year)	# Firm-year id
df <- subset(df, select=c("VerifiedEmissions", "UnitsSurrendered", "FirmComplianceCode", "Year"))
df <- aggregate(df, by=list(FirmYear), FUN=sum)
df$FirmComplianceCode[df$FirmComplianceCode>0] <- 1
df$Year <- as.numeric(substr(df$Group.1, nchar(df$Group.1)-3, nchar(df$Group.1)))

sum(pmax((df$VerifiedEmissions - df$UnitsSurrendered)[df$Year %in% (2005:2020) & df$FirmComplianceCode==1],0, na.rm=T))/10^6 # official total


## Magnitude of implied fine
### Using official compliance codes
(sum(pmax((ds$cve - ds$cus)[ds$Year %in% (2005:2007) & ds$ComplianceCode %in% c("B","B*","D","D*")],0, na.rm=T))*40 + sum(pmax((ds$cve - ds$cus)[ds$Year %in% (2008:2020) & ds$ComplianceCode %in% c("B","B*","D","D*")],0, na.rm=T))*100)/10^9  # 
(sum(pmax((ds$cve2006 - ds$cus2006)[ds$Year %in% (2006:2007) & ds$ComplianceCode %in% c("B","B*","D","D*")],0, na.rm=T))*40 + sum(pmax((ds$cve2006 - ds$cus2006)[ds$Year %in% (2008:2020) & ds$ComplianceCode %in% c("B","B*","D","D*")],0, na.rm=T))*100)/10^9  # Excluding 2005

(sum(pmax((ds$cve - ds$cus)[ds$Year %in% (2005:2007) & ds$ComplianceCode %in% c("B","B*","D","D*")],(ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$Year %in% (2005:2007) & ds$ComplianceCode %in% c("B","B*","D","D*")],0, na.rm=T))*40 + sum(pmax((ds$cve - ds$cus)[ds$Year %in% (2008:2020) & ds$ComplianceCode %in% c("B","B*","D","D*")],(ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$Year %in% (2008:2020) & ds$ComplianceCode %in% c("B","B*","D","D*")], 0, na.rm=T))*100)/10^9  # 

### Using stricter legal definition
(sum(pmax((ds$cve - ds$cus)[ds$Year %in% (2005:2007) & ds$noncompliant==TRUE],(ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$Year %in% (2005:2007) & ds$noncompliant==TRUE],0, na.rm=T))*40 + sum(pmax((ds$cve - ds$cus)[ds$Year %in% (2008:2020) & ds$noncompliant==TRUE],(ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$Year %in% (2008:2020) & ds$noncompliant==TRUE], 0, na.rm=T))*100)/10^9  # 
(sum(pmax((ds$cve - ds$cus)[ds$Year %in% (2006:2007) & ds$noncompliant==TRUE],(ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$Year %in% (2006:2007) & ds$noncompliant==TRUE],0, na.rm=T))*40 + sum(pmax((ds$cve - ds$cus)[ds$Year %in% (2008:2020) & ds$noncompliant==TRUE],(ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$Year %in% (2008:2020) & ds$noncompliant==TRUE], 0, na.rm=T))*100)/10^9  # Excluding 2005


## Benford's law
### Plot conformity with Benford's law
pred.p <- log10(1+1/(1:9))	# Predicted probabilities of the first digit.
p <- as.numeric(substr(ds$VerifiedEmissions,1,1))
p <- p[!is.na(p)]
p <- p[p>0]
p <- table(p)
chi2 <- sum(((p - (pred.p*sum(p)))^2)/(pred.p*sum(p))) # Critical value 5% with 8 degrees of freedom: 15.51. 1%: 20.09.
mad <- mean(abs(p/sum(p) - (pred.p))) 

quartz(width = 4.5, height = 4, type = "pdf", "benford", file = "benford.pdf")
par (fig=c(0,1,0,1), # Figure region in the device display region (x1,x2,y1,y2)
	omi=c(.8,0,.2,.5), # global margins in inches (bottom, left, top, right)
	mai=c(0,.8,.2,.3)) # subplot margins in inches (bottom, left, top, right)
	# layout(matrix(c(1,2), 1,2, byrow = TRUE), widths=c(2,4), heights=c(3,3))

plot(pred.p,as.vector(p)/sum(p), pch=16, col="#A1A1E5", bty="l", xlim=c(0,.35),ylim=c(0,.35), xaxs="i", yaxs="i", ylab="", xlab="", yaxt="n", xaxt="n")
points(pred.p,as.vector(p)/sum(p), col="#3333B3")
text(x=pred.p, y=as.vector(p)/sum(p), labels=c(1:9), pos=2)
abline(a=0,b=1, lty=2)
axis(side=1, at=seq(0,.3,.1))
axis(side=2, at=seq(0,.3,.1), las=2)
mtext("Predicted density", side=1, line=3)
mtext("Observed density", side=3, line=1, at=0.01)
# text(x=0, y=.3, paste(expression(chi^2 * "="), round(chi2,1),sep=""), pos=4)
text(x=.02, y=.3, expression(chi^2 * " ="), pos=4)
text(x=.07, y=.3, round(chi2,1), pos=4)
text(x=.02, y=.25, expression("MAD" * " ="), pos=4)
text(x=.09, y=.25, round(mad,4), pos=4)
dev.off()


### Benford's law by long/short position
pred.p <- log10(1+1/(1:9))	# Predicted probabilities of the first digit.
p <- as.numeric(substr(ds$VerifiedEmissions[ds$VerifiedEmissions>ds$UnitsSurrendered & !is.na(ds$VerifiedEmissions) & !is.na(ds$UnitsSurrendered)],1,1)) # Short
p <- p[!is.na(p)]
p <- p[p>0]
p <- table(p)
chi2.short <- sum(((p - (pred.p*sum(p)))^2)/(pred.p*sum(p))) # Critical value 5% with 8 degrees of freedom: 15.51. 1%: 20.09.
mad.short <- mean(abs(p/sum(p) - (pred.p))) 

pred.p <- log10(1+1/(1:9))	# Predicted probabilities of the first digit.
p <- as.numeric(substr(ds$VerifiedEmissions[ds$VerifiedEmissions<ds$UnitsSurrendered & !is.na(ds$VerifiedEmissions) & !is.na(ds$UnitsSurrendered)],1,1)) # Long
p <- p[!is.na(p)]
p <- p[p>0]
p <- table(p)
chi2.long <- sum(((p - (pred.p*sum(p)))^2)/(pred.p*sum(p))) # Critical value 5% with 8 degrees of freedom: 15.51. 1%: 20.09.
mad.long <- mean(abs(p/sum(p) - (pred.p))) 


# Section 4 analysis
## Benford's law event study, by Tier
### Installations are put into three categories based on allocations (in Phase 1) or historical verified emissions (Phase 2 and after)
#### Category A: Annual average emissions are equal to or less than 50 000 tonnes of CO2(e);
#### Category B: Annual average emissions are more than 50 000 tonnes of CO2(e) and equal to or less than 500 000 tonnes of CO2(e);
#### Category C: Annual average emissions are more than 500 000 tonnes of CO2(e).
x <- aggregate(ds$AllowanceInAllocation[ds$Year<2008 & !is.na(ds$Year)], by=list(ds$ID[ds$Year<2008 & !is.na(ds$Year)]), mean)
ds$category1 <- NA
ds$category1[ds$ID %in% x$Group.1[x$x<=50000]] <- 1					# Category A
ds$category1[ds$ID %in% x$Group.1[x$x>50000]] <- 3					# Category B & C

x <- aggregate(ds$VerifiedEmissions[ds$Year<2008 & !is.na(ds$Year)], by=list(ds$ID[ds$Year<2008 & !is.na(ds$Year)]), mean)
ds$category2 <- NA
ds$category2[ds$ID %in% x$Group.1[x$x<=50000]] <- 1					# Category A
ds$category2[ds$ID %in% x$Group.1[x$x>50000]] <- 3					# Category B & C

pred.p <- log10(1+1/(1:9))	# Predicted probabilities of the first digit.
years <- unique(ds$Year)
years <- years[order(years)]
chi2 <- data.frame(year=years, chi2=NA, chi2A=NA, chi2B=NA, chi2C=NA, mad=NA, madA=NA, madB=NA, madC=NA)
for (i in years) {
	### All installations active in Phase 1 and 2
	p <- as.numeric(substr(ds$VerifiedEmissions[ds$Year==i & !is.na(ds$Year) & !is.na(ds$category1) & !is.na(ds$category2)],1,1))
	p <- p[!is.na(p)]
	p <- p[p>0]
	p <- table(p)
	chi2$chi2[chi2$year==i] <- sum(((p - (pred.p*sum(p)))^2)/(pred.p*sum(p)))
	chi2$mad[chi2$year==i] <- mean(abs(p/sum(p) - pred.p))

	### Category A installations active in Phase 1 and 2
	p <- as.numeric(substr(ds$VerifiedEmissions[ds$Year==i & !is.na(ds$Year) & ds$category1==1 & ds$category2==1],1,1))
	p <- p[!is.na(p)]
	p <- p[p>0]
	p <- table(p)
	chi2$chi2A[chi2$year==i] <- sum(((p - (pred.p*sum(p)))^2)/(pred.p*sum(p)))
	chi2$madA[chi2$year==i] <- mean(abs(p/sum(p) - pred.p))

	### Category B & C installations active in Phase 1 and 2
	p <- as.numeric(substr(ds$VerifiedEmissions[ds$Year==i & !is.na(ds$Year) & ds$category1==3 & ds$category2==3],1,1))
	p <- p[!is.na(p)]
	p <- p[p>0]
	p <- table(p)
	chi2$chi2C[chi2$year==i] <- sum(((p - (pred.p*sum(p)))^2)/(pred.p*sum(p)))
	chi2$madC[chi2$year==i] <- mean(abs(p/sum(p) - pred.p))
}

#### Plot conformity with Benford's law over time, by Category
quartz(width = 8, height = 5, type = "pdf", "benford_categories", file = "benford_categories.pdf")
plot(chi2$year,chi2$chi2,type="n",bty="L", xlim=c(2005,2020), ylim=c(0,1.8),axes=F,xaxs="i",yaxs="i",ylab="",xlab="",xaxt='n',yaxt='n', main="")
axis(1,at=c(2005:2020), labels=rep("",length(2005:2020)))
axis(1,at=seq(2005,2020,5))
abline(v=2007.5,lty=2)
axis(2,at=seq(0,2,.5),las=2)
text(x=2007,y=1.6, "€40",cex=1.1)
text(x=2008,y=1.6, "€100",cex=1.1)
arrows(2007.3, 1.6, 2008-.4, 1.6, length = 0.1, angle = 30)

lines(chi2$year,chi2$chi2A/chi2$chi2A[chi2$year==2007], col=rgb(51, 51, 179, alpha=200, max=256), lwd=1)
lines(chi2$year,chi2$chi2C/chi2$chi2C[chi2$year==2007], col=rgb(51, 51, 179, alpha=200, max=256), lwd=4)

mtext(expression(chi^2 * " (2007=1)"), side=3, adj=-.1)

legend("topright", legend=c("Large (>50 kt)","Small (<50 kt)"), lty=c(1,1), lwd=c(4,1), col=c(rgb(51, 51, 179, alpha=200, max=256),rgb(51, 51, 179, alpha=200, max=256)), bty="n")
dev.off()

#### Figure 3
# Panel A
### Plot conformity with Benford's law
pred.p <- log10(1+1/(1:9))	# Predicted probabilities of the first digit.
p <- as.numeric(substr(ds$VerifiedEmissions,1,1))
p <- p[!is.na(p)]
p <- p[p>0]
p <- table(p)
chi2 <- sum(((p - (pred.p*sum(p)))^2)/(pred.p*sum(p))) # Critical value 5% with 8 degrees of freedom: 15.51. 1%: 20.09.
mad <- mean(abs(p/sum(p) - (pred.p))) 

quartz(width = 10, height = 4, type = "pdf", "benford", file = "benford.pdf")
par (fig=c(0,1,0,1), # Figure region in the device display region (x1,x2,y1,y2)
	omi=c(.8,0,.2,.5), # global margins in inches (bottom, left, top, right)
	mai=c(0,.8,.2,.3)) # subplot margins in inches (bottom, left, top, right)
	layout(matrix(c(1,2), 1,2, byrow = TRUE), widths=c(4,6), heights=c(4,4))

plot(pred.p,as.vector(p)/sum(p), pch=16, col="#A1A1E5", bty="l", xlim=c(0,.35),ylim=c(0,.35), xaxs="i", yaxs="i", ylab="", xlab="", yaxt="n", xaxt="n")
points(pred.p,as.vector(p)/sum(p), col="#3333B3")
text(x=pred.p, y=as.vector(p)/sum(p), labels=c(1:9), pos=2)
abline(a=0,b=1, lty=2)
axis(side=1, at=seq(0,.3,.1))
axis(side=2, at=seq(0,.3,.1), las=2)
mtext("Predicted density", side=1, line=3)
mtext("Observed density", side=3, line=1, at=0.01)
text(x=.02, y=.3, expression(chi^2 * " ="), pos=4)
text(x=.07, y=.3, round(chi2,1), pos=4)
text(x=.02, y=.25, expression("MAD" * " ="), pos=4)
text(x=.09, y=.25, round(mad,4), pos=4)


# Panel B
x <- aggregate(ds$AllowanceInAllocation[ds$Year<2008 & !is.na(ds$Year)], by=list(ds$ID[ds$Year<2008 & !is.na(ds$Year)]), mean)
ds$category1 <- NA
ds$category1[ds$ID %in% x$Group.1[x$x<=50000]] <- 1					# Category A
ds$category1[ds$ID %in% x$Group.1[x$x>50000]] <- 3					# Category B & C

x <- aggregate(ds$VerifiedEmissions[ds$Year<2008 & !is.na(ds$Year)], by=list(ds$ID[ds$Year<2008 & !is.na(ds$Year)]), mean)
ds$category2 <- NA
ds$category2[ds$ID %in% x$Group.1[x$x<=50000]] <- 1					# Category A
ds$category2[ds$ID %in% x$Group.1[x$x>50000]] <- 3					# Category B & C

pred.p <- log10(1+1/(1:9))	# Predicted probabilities of the first digit.
years <- unique(ds$Year)
years <- years[order(years)]
chi2 <- data.frame(year=years, chi2=NA, chi2A=NA, chi2B=NA, chi2C=NA, mad=NA, madA=NA, madB=NA, madC=NA)
for (i in years) {
	### All installations active in Phase 1 and 2
	p <- as.numeric(substr(ds$VerifiedEmissions[ds$Year==i & !is.na(ds$Year) & !is.na(ds$category1) & !is.na(ds$category2)],1,1))
	p <- p[!is.na(p)]
	p <- p[p>0]
	p <- table(p)
	chi2$chi2[chi2$year==i] <- sum(((p - (pred.p*sum(p)))^2)/(pred.p*sum(p)))
	chi2$mad[chi2$year==i] <- mean(abs(p/sum(p) - pred.p))

	### Category A installations active in Phase 1 and 2
	p <- as.numeric(substr(ds$VerifiedEmissions[ds$Year==i & !is.na(ds$Year) & ds$category1==1 & ds$category2==1],1,1))
	p <- p[!is.na(p)]
	p <- p[p>0]
	p <- table(p)
	chi2$chi2A[chi2$year==i] <- sum(((p - (pred.p*sum(p)))^2)/(pred.p*sum(p)))
	chi2$madA[chi2$year==i] <- mean(abs(p/sum(p) - pred.p))

	### Category B & C installations active in Phase 1 and 2
	p <- as.numeric(substr(ds$VerifiedEmissions[ds$Year==i & !is.na(ds$Year) & ds$category1==3 & ds$category2==3],1,1))
	p <- p[!is.na(p)]
	p <- p[p>0]
	p <- table(p)
	chi2$chi2C[chi2$year==i] <- sum(((p - (pred.p*sum(p)))^2)/(pred.p*sum(p)))
	chi2$madC[chi2$year==i] <- mean(abs(p/sum(p) - pred.p))
}

#### Plot conformity with Benford's law over time, by Category
# quartz(width = 8, height = 5, type = "pdf", "benford_categories", file = "benford_categories.pdf")
plot(chi2$year,chi2$chi2,type="n",bty="L", xlim=c(2005,2020), ylim=c(0,1.8),axes=F,xaxs="i",yaxs="i",ylab="",xlab="",xaxt='n',yaxt='n', main="")
axis(1,at=c(2005:2020), labels=rep("",length(2005:2020)))
axis(1,at=seq(2005,2020,5))
abline(v=2007.5,lty=2)
axis(2,at=seq(0,2,.5),las=2)
text(x=2006.5,y=1.6, "€40",cex=1.1)
text(x=2008.5,y=1.6, "€100",cex=1.1)
arrows(2007.3, 1.6, 2008-.4, 1.6, length = 0.1, angle = 30)

lines(chi2$year,chi2$chi2A/chi2$chi2A[chi2$year==2007], col=rgb(51, 51, 179, alpha=200, max=256), lwd=1)
lines(chi2$year,chi2$chi2C/chi2$chi2C[chi2$year==2007], col=rgb(51, 51, 179, alpha=200, max=256), lwd=4)

mtext(expression(chi^2 * " (2007=1)"), side=3, adj=-.1)

legend("topright", legend=c("Large (>50 kt)","Small (<50 kt)"), lty=c(1,1), lwd=c(4,1), col=c(rgb(51, 51, 179, alpha=200, max=256),rgb(51, 51, 179, alpha=200, max=256)), bty="n")
dev.off()


#### Check the change in stringecy for each Category
# Large installations have the greater reduction in allocation
temp <- ds[ds$Year %in% c("2007","2008"),]
temp <- temp[temp$category1 %in% c(1,2,3) & temp$category2 %in% c(1,2,3),]
temp <- subset(temp, select=c("ID","Year","AllowanceInAllocation","category1","category2"))
temp <- reshape(temp, v.names = c("AllowanceInAllocation","category1","category2"), timevar = "Year", idvar = "ID", direction="wide") 
sum(temp$AllowanceInAllocation.2008[!is.na(temp$AllowanceInAllocation.2007) & !is.na(temp$AllowanceInAllocation.2008) & temp$category1.2007==1 & temp$category2.2007==1]) / sum(temp$AllowanceInAllocation.2007[!is.na(temp$AllowanceInAllocation.2007) & !is.na(temp$AllowanceInAllocation.2008) & temp$category1.2007==1 & temp$category2.2008==1])	# Change for category A
sum(temp$AllowanceInAllocation.2008[!is.na(temp$AllowanceInAllocation.2007) & !is.na(temp$AllowanceInAllocation.2008) & temp$category1.2007==3 & temp$category2.2008==3]) / sum(temp$AllowanceInAllocation.2007[!is.na(temp$AllowanceInAllocation.2007) & !is.na(temp$AllowanceInAllocation.2008) & temp$category1.2007==3 & temp$category2.2008==3])	# Change for category B & C


## Calculate noncompliance rates and enforcement rates for each country and year
nc <- expand.grid(year=2005:2020, country=unique(ds$NationalAdministratorCodeCorrected))
nc$country <- as.character(nc$country)
nc <- nc[order(nc$country, nc$year),]
nc$country[nc$country %in% c("Bulgaria","Romania") & nc$year %in% 2005:2006] <- NA	# Bulgaria and Romania entered the EU ETS in 2007
nc$country[nc$country %in% c("Norway","Liechtenstein") & nc$year %in% 2005:2007] <- NA	# These countries entered EU ETS in 2008
nc$country[nc$country %in% c("Croatia","Iceland") & nc$year %in% 2005:2012] <- NA	# Croatia entered EU ETS in 2013. Iceland entered earlier, but stationary installations only joined in 2013
nc <- nc[!is.na(nc$country),]


### Number and fraction of noncompliance events by country and year
nc$noncompliance_codes <- nc$noncompliance_codes_n <- NA 
for (i in 1:nrow(nc)) {
	nc$noncompliance_codes_n[i] <- length(unique(ds$ID[ds$Year==nc$year[i] & ds$NationalAdministratorCodeCorrected==nc$country[i] & ds$ComplianceCode %in% c("B","B*","D","D*")])) # Number of non-compliance events (numerator)
	nc$noncompliance_codes[i] <- length(unique(ds$ID[ds$Year==nc$year[i] & ds$NationalAdministratorCodeCorrected==nc$country[i]])) # Number of events (denominator)
}
nc$noncompliance_codes <- nc$noncompliance_codes_n / nc$noncompliance_codes	# Non-compliance rate


### Excess emissions enforcement actions
nc$enforcement_upper_n <- nc$enforcement_lower_n <- nc$missing <- NA	# Upper and lower bounds. The Article 21 reports are sometimes vague on the number of installation operators against which enforcement action was taken. We have computed upper and lower bounds, though in practice, this is likely to result in minimal differences.
for (i in 1:nrow(nc)) {
	if (nrow(art21[art21$country==nc$country[i] & art21$year==nc$year[i],])==0) { # If we do not have an Article 21 report from that country for that year, skip ahead.
		nc$missing[i] <- 1
		next
	}
	if (is.na(art21$emissionsfines.n.installations.upper[art21$country==nc$country[i] & art21$year==nc$year[i]])) { # If we do not have an Article 21 report from that country for that year, skip ahead.
		nc$missing[i] <- 1
		next
	}
	nc$enforcement_lower_n[i] <- art21$emissionsfines.n.installations.lower[art21$country==nc$country[i] & art21$year==nc$year[i]] #
	if (art21$emissionsfines.n.installations.upper[art21$country==nc$country[i] & art21$year==nc$year[i]]!="Maximum") {	#
		nc$enforcement_upper_n[i] <- as.numeric(art21$emissionsfines.n.installations.upper[art21$country==nc$country[i] & art21$year==nc$year[i]]) #
	}
	if (art21$emissionsfines.n.installations.upper[art21$country==nc$country[i] & art21$year==nc$year[i]]=="Maximum") { # Some Art 21 reports make sufficiently vague statements about the number of enforcement actions that it is possible to interpret them as saying that every noncompliant operator was punished. In these cases we note "Maximum" and impute a number of enforcement actions equal to the number of officially noncompliant installations.
		nc$enforcement_upper_n[i] <- nc$noncompliance_codes_n[i]
	}
}

### Calculate enforcement probabilities
nc$enforcement_lower <- nc$enforcement_upper <- NA
nc$enforcement_lower <- nc$enforcement_lower_n / nc$noncompliance_codes_n
nc$enforcement_upper <- nc$enforcement_upper_n / nc$noncompliance_codes_n
nc$enforcement_lower[nc$enforcement_lower_n==0 & nc$noncompliance_codes_n==0] <- 1 # When there are neither noncompliance events nor enforcement action, the probability is not defined. Impute the most generous value.
nc$enforcement_upper[nc$enforcement_upper_n==0 & nc$noncompliance_codes_n==0] <- 1 # When there are neither noncompliance events nor enforcement action, the probability is not defined. Impute the most generous value.
nc$enforcement_lower[nc$enforcement_lower>1 & !is.na(nc$enforcement_upper)] <- 1 # Whenever more enforcement actions are reported than we can find officially noncompliant installations, generously interpret this as an enforcement probability of 1.
nc$enforcement_upper[nc$enforcement_upper>1 & !is.na(nc$enforcement_upper)] <- 1 # Whenever more enforcement actions are reported than we can find officially noncompliant installations, generously interpret this as an enforcement probability of 1.

## Figure 4
quartz(width = 7, height = 6, type = "pdf", "enforcement", file = "enforcement.pdf")
plot(nc$enforcement_upper, nc$noncompliance_codes, xlim=c(0,1.1), type="n", bty="l", xlab="",ylab="",yaxt="n") # , xaxs="i", yaxs="i"

axis(side=2, las=2)
mtext(side=3, at=0, line=1, "Probability of\n non-compliance")
mtext(side=1, at=0.5, line=3, "Probability of enforcement")

x <- predict(lm(noncompliance_codes ~ enforcement_upper, data=nc[nc$enforcement_upper<1.1,]))
lines(x=c(0,1), y=c(max(x),min(x)), lwd=.7)

points(nc$enforcement_upper, nc$noncompliance_codes, pch=16, col=rgb(0.20,0.20,0.70,alpha=.3), cex=1.2)
points(nc$enforcement_upper, nc$noncompliance_codes, col="#3333B3", cex=1.2)

text(x=0.5, y=0.6, expression(rho[s] * " = "), cex=1.2)
text(x=0.59, y=0.61, round(cor(nc$enforcement_upper,nc$noncompliance_codes,method="spearman", use="pairwise.complete.obs"),2), cex=1.2)

dev.off()

# Calculate correlation excluding 2005
cor(nc$enforcement_upper[nc$year>2005],nc$noncompliance_codes[nc$year>2005],method="spearman", use="pairwise.complete.obs")

# Calculate correlation using only within-country variation
nc$noncompliance_codes_demeaned <- nc$enforcement_upper_demeaned <- NA
for (i in unique(nc$country)) {
	nc$noncompliance_codes_demeaned[nc$country==i] <- nc$noncompliance_codes[nc$country==i] - mean(nc$noncompliance_codes[nc$country==i], na.rm=T)
	nc$enforcement_upper_demeaned[nc$country==i] <- nc$enforcement_upper[nc$country==i] - mean(nc$enforcement_upper[nc$country==i], na.rm=T)
}
cor(nc$enforcement_upper_demeaned,nc$noncompliance_codes_demeaned, method="spearman", use="pairwise.complete.obs")



### Fines collected
sum(as.numeric(art21$emissionsfines.q.installations), na.rm=TRUE)/10^9


### Regressions
nc$fine <- 40
nc$fine[nc$year>2007] <- 100

summary(lm(noncompliance_codes ~ enforcement_upper + fine + I(enforcement_upper * fine), data=nc))
summary(lm(noncompliance_codes ~ I(enforcement_upper * fine), data=nc))


# Secondary analysis of enforcement
mean(art21$appointment, na.rm=TRUE) # Appointing the auditor
mean(art21$supervision, na.rm=TRUE) # Auditor supervision
mean(art21$namingshaming, na.rm=TRUE) # Naming and shaming
mean(art21$suspension, na.rm=TRUE) # Suspension
mean(art21$inspection, na.rm=TRUE) # Inspections
sum(art21$otherfines.q.installations, na.rm=TRUE) # 

nc$appointment <- nc$supervision <- nc$namingshaming <- nc$suspension <- nc$inspection <- nc$fines <- NA
for (i in 1:nrow(nc)) {
	if (nrow(art21[art21$country==nc$country[i] & art21$year==nc$year[i],])>0) {
		nc$appointment[i] <- art21$appointment[art21$country==nc$country[i] & art21$year==nc$year[i]]
		nc$supervision[i] <- art21$supervision[art21$country==nc$country[i] & art21$year==nc$year[i]]
		nc$namingshaming[i] <- art21$namingshaming[art21$country==nc$country[i] & art21$year==nc$year[i]]
		nc$suspension[i] <- art21$suspension[art21$country==nc$country[i] & art21$year==nc$year[i]]
		nc$inspection[i] <- art21$inspection[art21$country==nc$country[i] & art21$year==nc$year[i]]
		nc$fines[i] <- art21$otherfines.q.installations[art21$country==nc$country[i] & art21$year==nc$year[i]]
	}
}

cor(cbind(nc$appointment, nc$supervision, nc$inspection), use="complete.obs")

cor(cbind(nc$namingshaming, nc$suspension, nc$fines), use="complete.obs")


pl <- nc

# Appendix: Figure 11
quartz(width = 7, height = 4, type = "pdf", "monitoring1", file = "monitoring1.pdf")
par (fig=c(0,1,0,1), # Figure region in the device display region (x1,x2,y1,y2)
	omi=c(.8,0,.2,.5), # global margins in inches (bottom, left, top, right)
	mai=c(0,.8,.2,.3)) # subplot margins in inches (bottom, left, top, right)
	layout(matrix(c(1:3), 1,3, byrow = TRUE), widths=c(3,3,3), heights=c(3,3,3))

ymin=-.0
ymax=.05

boxplot(noncompliance_codes ~ appointment, data = pl, outline=FALSE, main="Appointment", ylim=c(ymin,ymax), xaxt="n", yaxt="n", ylab="")
set.seed(1)
stripchart(noncompliance_codes ~ appointment, data = pl, vertical = TRUE, method = "jitter", pch = 1, add = TRUE, col=rgb(0.20,0.20,0.70,alpha=.3))
set.seed(1)
stripchart(noncompliance_codes ~ appointment, data = pl, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col=rgb(0.20,0.20,0.70,alpha=.1))
axis(side=1,at=c(1,2), label=c("No","Yes"))
axis(side=2,las=2)

boxplot(noncompliance_codes ~ supervision, data = pl, outline=FALSE, main="Supervision", ylim=c(ymin,ymax), xaxt="n", yaxt="n", ylab="")
set.seed(1)
stripchart(noncompliance_codes ~ supervision, data = pl, vertical = TRUE, method = "jitter", pch = 1, add = TRUE, col=rgb(0.20,0.20,0.70,alpha=.3))
set.seed(1)
stripchart(noncompliance_codes ~ supervision, data = pl, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col=rgb(0.20,0.20,0.70,alpha=.1))
axis(side=1,at=c(1,2), label=c("No","Yes"))
axis(side=2,las=2)

boxplot(noncompliance_codes ~ inspection, data = pl, outline=FALSE, main="Inspection", ylim=c(ymin,ymax), xaxt="n", yaxt="n", ylab="")
set.seed(1)
stripchart(noncompliance_codes ~ inspection, data = pl, vertical = TRUE, method = "jitter", pch = 1, add = TRUE, col=rgb(0.20,0.20,0.70,alpha=.3))
set.seed(1)
stripchart(noncompliance_codes ~ inspection, data = pl, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col=rgb(0.20,0.20,0.70,alpha=.1))
axis(side=1,at=c(1,2), label=c("No","Yes"))
axis(side=2,las=2)

dev.off()


# Figure 5
pl <- ds
pl$clusters <- factor(paste(pl$NationalAdministratorCodeCorrected, pl$Year))
pl$diff <- (pl$VerifiedEmissions-pl$AllowanceInAllocation)

## Identify countries with variation in regulatory power
pl$inspection.var <- pl$supervision.var <- pl$appointment.var <- FALSE
for (i in unique(pl$NationalAdministratorCodeCorrected)) {
	if (sd(pl$appointment[pl$NationalAdministratorCodeCorrected==i], na.rm=TRUE)>0) { pl$appointment.var[pl$NationalAdministratorCodeCorrected==i] <- TRUE }
	if (sd(pl$supervision[pl$NationalAdministratorCodeCorrected==i], na.rm=TRUE)>0) { pl$supervision.var[pl$NationalAdministratorCodeCorrected==i] <- TRUE }
	if (sd(pl$inspection[pl$NationalAdministratorCodeCorrected==i], na.rm=TRUE)>0) { pl$inspection.var[pl$NationalAdministratorCodeCorrected==i] <- TRUE }
}

out <- c()
for (i in seq(-10^5,10^5,2*10^4)) {
	pl$bin <- (pl$diff<=i & pl$diff>(i-2*10^4))
	temp1 <- summary(lm.cluster(pl[pl$appointment.var==TRUE,], bin ~ appointment, pl$cluster[pl$appointment.var==TRUE]))
	temp2 <- summary(lm.cluster(pl[pl$supervision.var==TRUE,], bin ~ supervision, pl$cluster[pl$supervision.var==TRUE]))
	temp3 <- summary(lm.cluster(pl[pl$inspection.var==TRUE,], bin ~ inspection, pl$cluster[pl$inspection.var==TRUE]))
	out <- rbind(out, cbind(i, temp1[2], temp1[4], temp2[2], temp2[4], temp3[2], temp3[4]))
}
out <- data.frame(out)
colnames(out) <- c("bin","appointment","appointment.se","supervision","supervision.se","inspection","inspection.se")
out <- out[-1,]

quartz(width = 12, height = 4, type = "pdf", "monitoring2", file = "monitoring2.pdf")
par (fig=c(0,1,0,1), # Figure region in the device display region (x1,x2,y1,y2)
	omi=c(.8,0,.5,.5), # global margins in inches (bottom, left, top, right)
	mai=c(0,.8,.2,.3)) # subplot margins in inches (bottom, left, top, right)
	layout(matrix(c(1:3), 1,3, byrow = TRUE), widths=c(3,3,3), heights=c(3,3,3))

ymax=.3

mp <- barplot(out$appointment, col=rgb(0.20,0.20,0.70,alpha=.3), border=rgb(0.20,0.20,0.70,alpha=1),  ylim=c(-ymax,ymax), space=0, xaxs="i", yaxt="n", main="Appointment")
segments(mp, out$appointment - out$appointment.se*2, mp, out$appointment + out$appointment.se*2, lwd = 1.5)
arrows(mp, out$appointment - out$appointment.se*2, mp, out$appointment + out$appointment.se*2, lwd = 1.5, angle = 90, code = 3, length = 0.05)
abline(v=which(out$bin==0), lty=2)
axis(1,at=c(0,seq_along(out$bin),length(out$bin)+1), labels=c(min(out$bin/10^3)-20,out$bin/10^3,min(out$bin/10^3)+20))
axis(2,las=2,at=seq(-ymax,ymax,ymax/2))

mp <- barplot(out$supervision, col=rgb(0.20,0.20,0.70,alpha=.3), border=rgb(0.20,0.20,0.70,alpha=1),  ylim=c(-ymax,ymax), space=0, xaxs="i", yaxt="n", main="Supervision")
segments(mp, out$supervision - out$supervision.se*2, mp, out$supervision + out$supervision.se*2, lwd = 1.5)
arrows(mp, out$supervision - out$supervision.se*2, mp, out$supervision + out$supervision.se*2, lwd = 1.5, angle = 90, code = 3, length = 0.05)
abline(v=which(out$bin==0), lty=2)
axis(1,at=c(0,seq_along(out$bin),length(out$bin)+1), labels=c(min(out$bin/10^3)-20,out$bin/10^3,min(out$bin/10^3)+20))
axis(2,las=2,at=seq(-ymax,ymax,ymax/2))

mp <- barplot(out$inspection, col=rgb(0.20,0.20,0.70,alpha=.3), border=rgb(0.20,0.20,0.70,alpha=1),  ylim=c(-ymax,ymax), space=0, xaxs="i", yaxt="n", main="Inspection")
segments(mp, out$inspection - out$inspection.se*2, mp, out$inspection + out$inspection.se*2, lwd = 1.5)
arrows(mp, out$inspection - out$inspection.se*2, mp, out$inspection + out$inspection.se*2, lwd = 1.5, angle = 90, code = 3, length = 0.05)
abline(v=which(out$bin==0), lty=2)
axis(1,at=c(0,seq_along(out$bin),length(out$bin)+1), labels=c(min(out$bin/10^3)-20,out$bin/10^3,min(out$bin/10^3)+20))
axis(2,las=2,at=seq(-ymax,ymax,ymax/2))

mtext(side=1, "Verified emissions minus free allocation (in thousands of tonnes)", outer=T, line=3, cex=.8)
mtext(side=3, "Difference in\nprobability", outer=T, line=1, at=.05, cex=.8)

dev.off()



# Appendix: Figure 12
pl <- nc
quartz(width = 7, height = 4, type = "pdf", "enforcement1", file = "enforcement1.pdf")
par (fig=c(0,1,0,1), # Figure region in the device display region (x1,x2,y1,y2)
	omi=c(.8,0,.2,.5), # global margins in inches (bottom, left, top, right)
	mai=c(0,.8,.2,.3)) # subplot margins in inches (bottom, left, top, right)
	layout(matrix(c(1:3), 1,3, byrow = TRUE), widths=c(3,3,3), heights=c(3,3,3))

ymin=-.0
ymax=.05

boxplot(noncompliance_codes ~ namingshaming, data = pl, outline=FALSE, main="Naming & Shaming", ylim=c(ymin,ymax), xaxt="n", yaxt="n", ylab="")
set.seed(1)
stripchart(noncompliance_codes ~ namingshaming, data = pl, vertical = TRUE, method = "jitter", pch = 1, add = TRUE, col=rgb(0.20,0.20,0.70,alpha=.3))
set.seed(1)
stripchart(noncompliance_codes ~ namingshaming, data = pl, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col=rgb(0.20,0.20,0.70,alpha=.1))
axis(side=1,at=c(1,2), label=c("No","Yes"))
axis(side=2,las=2)

boxplot(noncompliance_codes ~ suspension, data = pl, outline=FALSE, main="Suspension", ylim=c(ymin,ymax), xaxt="n", yaxt="n", ylab="")
set.seed(1)
stripchart(noncompliance_codes ~ suspension, data = pl, vertical = TRUE, method = "jitter", pch = 1, add = TRUE, col=rgb(0.20,0.20,0.70,alpha=.3))
set.seed(1)
stripchart(noncompliance_codes ~ suspension, data = pl, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col=rgb(0.20,0.20,0.70,alpha=.1))
axis(side=1,at=c(1,2), label=c("No","Yes"))
axis(side=2,las=2)

plot(pl$fines, pl$noncompliance_codes, pch=1, col=rgb(0.20,0.20,0.70,alpha=.3),  main="Other fines", ylim=c(ymin,ymax), , xaxt="n", yaxt="n", ylab="", xlab="Million Euro", cex=.8)
points(pl$fines, pl$noncompliance_codes, pch=19, col=rgb(0.20,0.20,0.70,alpha=.1), cex=.8)
mtext("Million Euro", cex=.8, side=1, line=2)
abline(lm(pl$noncompliance_codes ~ pl$fines))
axis(side=1,at=seq(0, 20, 5)*10^6, label=seq(0, 20, 5))
axis(side=2,las=2)

dev.off()


# Appendix: Figure 13
## Treatment variable
ds$namingshaming <- NA
for (i in unique(ds$NationalAdministratorCodeCorrected)) {
	for (j in unique(ds$Year)) {
		ds$namingshaming[ds$NationalAdministratorCodeCorrected==i & ds$Year==j] <- nc$namingshaming[nc$country==i & nc$year==j]
	}
}

## Outcome variable
ds$noncompliant <- NA
ds$noncompliant[ds$ComplianceCode %in% c("B","B*","D","D*")] <- 1
ds$noncompliant[ds$ComplianceCode %in% c("A","A*","E","E*")] <- 0

## Cluster variable
ds$clusters <- factor(paste(ds$NationalAdministratorCodeCorrected, ds$Year))

## Version 1: Listed or Not
out <- summary(felm(noncompliant ~ namingshaming + listed + I(namingshaming*listed) | NationalAdministratorCodeCorrected | 0 | clusters, data = ds, cmethod = 'reghdfe'))$coef[c(3,6)] # Add country fixed effects

## Version 2: Domestic or not
out <- rbind(out, summary(felm(noncompliant ~ namingshaming + domestic + I(namingshaming*domestic) | NationalAdministratorCodeCorrected | 0 | clusters, data = ds, cmethod = 'reghdfe'))$coef[c(3,6)]) # Add country fixed effects

# Version 3: Listed or Not AND Domestic or not
out <- rbind(out, summary(felm(noncompliant ~ namingshaming + listed + I(namingshaming*listed*domestic) + domestic + I(namingshaming*domestic) + I(namingshaming*listed) | NationalAdministratorCodeCorrected | 0 | clusters, data = ds, cmethod = 'reghdfe'))$coef[c(3,6)]) # Add country fixed effects

colnames(out) <- c("coef","se")
rownames(out) <- c("public.fe","domestic.fe","both.fe")
out <- data.frame(out)
out <- out[nrow(out):1,]

quartz(width = 10, height = 3, type = "pdf", "enforcement2", file = "enforcement2.pdf")
mp <- barplot(out$coef,  space=.5, xaxs="i", yaxt="n", main="Naming and shaming", horiz=TRUE, xlim=c(-.15,.15), col = NA, border = NA, xlab="Difference in rates of non-compliance")
segments(out$coef - out$se*2, mp, out$coef + out$se*2, mp, lwd = 1.5)
arrows(out$coef - out$se*2, mp, out$coef + out$se*2, mp, lwd = 1.5, angle = 90, code = 3, length = 0.05)
points(out$coef, mp, cex=1.5, pch=15, col=rgb(0.20,0.20,0.70,alpha=1))
abline(v=0, lty=2)
text("Listed x N&S", x=-0.15, y=4, pos=4)
text("Domestic x N&S", x=-0.15, y=2.5, pos=4)
text("Listed x Domestic x N&S", x=-0.15, y=1, pos=4)
dev.off()


# Appendix: Figure 14
out <- c()
countries <- unique(ds$NationalAdministratorCodeCorrected)
countries <- countries[!(countries %in% c("BG","RO","NO","LI","HR","IS"))] # These countries entered EU ETS late
countries <- countries[!(countries %in% c("CY","MT"))] # No Phase 1 NAPs
for (i in countries) {
	dt <- ds[ds$NationalAdministratorCodeCorrected==i,]
	dt1 <- dt[dt$Year==2006 & !is.na(dt$PermitOrPlanDate),]
	dt2 <- dt[dt$Year==2008 & !is.na(dt$PermitOrPlanDate),]
	temp <- intersect(dt1$ID, dt2$ID)
	if (length(temp)==0) { next }
	dt1 <- dt1[dt1$ID %in% temp,]
	dt2 <- dt2[dt2$ID %in% temp,]
	dt1$allocation2 <- NA
	for (j in dt1$ID) {
		dt1$allocation2[dt1$ID==j] <- dt2$AllowanceInAllocation[dt2$ID==j]
	}
	dt1 <- dt1[dt1$AllowanceInAllocation!=0 & dt1$allocation2!=0,]
	fit <- lm(I(dt1$allocation2 - dt1$AllowanceInAllocation) ~ dt1$noncompliant)					# Levels
	coef <- coefficients(fit)[2]
	ci <- confint(fit, level=0.95)[2,]
	out <- rbind(out, cbind(i, coef, ci[1], ci[2]))
}
out <- data.frame(country=out[,1], discretion=as.numeric(out[,2]), ci.lower=as.numeric(out[,3]), ci.upper=as.numeric(out[,4]), noncompliance1=NA, noncompliance2=NA)
for (i in out$country) {
	out$noncompliance1[out$country==i] <- mean(nc$noncompliance_codes[nc$country==i & nc$year %in% c(2006:2007)], na.rm=TRUE)
	out$noncompliance2[out$country==i] <- mean(nc$noncompliance_codes[nc$country==i & nc$year %in% c(2008:2012)], na.rm=TRUE)
	out$noncompliancediff[out$country==i] <- out$noncompliance2[out$country==i] - out$noncompliance1[out$country==i]
}

quartz(width = 7, height = 6, type = "pdf", "favour", file = "favour.pdf")
plot(out$discretion, out$noncompliancediff, pch=1, col=rgb(0.20,0.20,0.70,alpha=.5), cex=1, bty="L", yaxt="n", xaxt="n", yaxs="i", ylab="", main="Change in non-compliance rate from Phase 1 to 2", ylim=c(-.06,.06), xlab="", xlim=c(-1.5*10^6, 1.5*10^6))
segments(out$ci.lower, out$noncompliancediff, out$ci.upper, out$noncompliancediff, lwd = 1.5, col="gray70")
arrows(out$ci.lower, out$noncompliancediff, out$ci.upper, out$noncompliancediff, lwd = 1.5, angle = 90, code = 3, length = 0.05, col="gray70")
points(out$discretion, out$noncompliancediff, pch=19, col=rgb(0.20,0.20,0.70,alpha=.3), cex=1)
points(out$discretion, out$noncompliancediff, pch=1, col=rgb(0.20,0.20,0.70,alpha=.5), cex=1)
points(out$discretion, out$noncompliancediff, pch=19, col=rgb(0.20,0.20,0.70,alpha=.3), cex=1)
axis(side=2,las=2)
axis(side=1, at=seq(-2,2,1)*10^6, labels=c("-2 MtCO2", "-1 MtCO2","0", "+1 MtCO2", "+2 MtCO2"))
abline(v=0, lty=2, col="gray80")
abline(h=0, lty=2, col="gray80")
abline(lm(out$noncompliancediff ~ out$discretion), lwd=3)
mtext(side=1, line=-2, outer=TRUE, cex=1.2, expression(paste("Regulatory favour (", beta[1], ")")))
dev.off()



# Appendix: Figure 10
quartz(width = 15, height = 9, type = "pdf", "disaggregated", file = "disaggregated.pdf")
par (fig=c(0,1,0,1), # Figure region in the device display region (x1,x2,y1,y2)
	omi=c(.8,0,.2,.5), # global margins in inches (bottom, left, top, right)
	mai=c(0,.8,.2,.3)) # subplot margins in inches (bottom, left, top, right)
	layout(matrix(c(1:2), 1,2, byrow = TRUE), widths=c(20,28), heights=c(20,20))

pl <- nc

x <- aggregate(pl$noncompliance_codes, by=list(pl$country), mean)
i <- rev(order(x$Group.1)) # Alphabetical
x <- x[i,]
dotchart(x$x,labels=x$Group.1, cex=1, col=rgb(0.20,0.20,0.70,alpha=1), xlim=c(0,.25),
  main="",
  xlab="Average annual non-compliance rate")

# Exclude first year of operation
pl <- pl[!(pl$year %in% 2005),]	#
pl <- pl[!(pl$country %in% c("BG","RO") & pl$year %in% 2005:2007),]	# Bulgaria and Romania entered the EU ETS in 2007
pl <- pl[!(pl$country %in% c("NO","LI") & pl$year %in% 2005:2008),]	# These countries entered EU ETS in 2008
pl <- pl[!(pl$country %in% c("HR","IS") & pl$year %in% 2005:2013),]	# Croatia entered EU ETS in 2013. Iceland entered earlier, but stationary installations only joined in 2013

x <- aggregate(pl$noncompliance_codes, by=list(pl$country), mean)
x <- x[i,]
points(x$x, 1:31, col=rgb(0.20,0.20,0.70,alpha=.3), pch=19)

abline(v=0, lty=3, col="grey")
abline(v=.01, lty=3, col="grey")
abline(v=.05, lty=3, col="grey")
abline(v=.1, lty=3, col="grey")
abline(v=.2, lty=3, col="grey")

# Plot non-compliance rate by activity / sector

## Calculate noncompliance rates and enforcement rates for each sector and year
### Define sectors
ds$nace2dig <- as.numeric(substr(ds$nace,1,2))
ds$sector <- NA
ds$sector[ds$nace2dig %in% c(1:3)] <- "(01-03) Agriculture, forestry, and fishing"
ds$sector[ds$nace2dig %in% c(5:9)] <- "(05-09) Mining and quarrying"
ds$sector[ds$nace2dig %in% c(10:12)] <- "(10-12) Food, beverages, and tobacco"
ds$sector[ds$nace2dig %in% c(13:15)] <- "(13-15) Textiles, clothes, and leather"
ds$sector[ds$nace2dig %in% c(16:18)] <- "(16-18) Wood, paper, and printing"
ds$sector[ds$nace2dig %in% c(19)] <- "(19) Coke and refined petroleum products"
ds$sector[ds$nace2dig %in% c(23:25)] <- "(23-25) Metals and non-metallic mineral products"
ds$sector[ds$nace2dig %in% c(26:28)] <- "(26-28) Electronics and machinery"
ds$sector[ds$nace2dig %in% c(29:30)] <- "(29-30) Motor vehicles and transport equipment"
ds$sector[ds$nace2dig %in% c(31:33)] <- "(31-33) Other manufacturing"
ds$sector[ds$nace2dig %in% c(35)] <- "(35) Electricity, gas, and steam"
ds$sector[ds$nace2dig %in% c(36:39)] <- "(36-39) Water supply and waste treatment"
ds$sector[ds$nace2dig %in% c(41:43)] <- "(41-43) Construction"
ds$sector[ds$nace2dig %in% c(45:47)] <- "(45-47) Wholesale and retail trade"
ds$sector[ds$nace2dig %in% c(49:53)] <- "(49-53) Transportation and storage"
ds$sector[ds$nace2dig %in% c(55:99)] <- "(55-99) Other Sectors"
ds$sector[is.na(ds$nace2dig)] <- "Sector not available"

nt <- expand.grid(year=2005:2020, sector=unique(ds$sector))
nt$sector <- as.character(nt$sector)
nt <- nt[order(nt$sector, nt$year),]
nt <- nt[!is.na(nt$sector),]

### Number and fraction of nontompliance events by sector and year
nt$noncompliance_codes_n <- NA  # Numerator
nt$noncompliance_codes <- NA # Denominator

# Exclude first year of operation
dt <- ds[!(ds$Year %in% 2005),]	#
dt <- dt[!(dt$NationalAdministratorCodeCorrected %in% c("BG","RO") & dt$Year %in% 2005:2007),]	# Bulgaria and Romania entered the EU ETS in 2007
dt <- dt[!(dt$NationalAdministratorCodeCorrected %in% c("NO","LI") & dt$Year %in% 2005:2008),]	# These countries entered EU ETS in 2008
dt <- dt[!(dt$NationalAdministratorCodeCorrected %in% c("HR","IS") & dt$Year %in% 2005:2013),]	# Croatia entered EU ETS in 2013. Iceland entered earlier, but stationary installations only joined in 2013

for (i in 1:nrow(nt)) {
	nt$noncompliance_codes_n[i] <- length(unique(ds$ID[ds$Year==nt$year[i] & ds$sector==nt$sector[i] & ds$ComplianceCode %in% c("B","B*","D","D*")])) # Registry's self-reported compliance rate
	nt$noncompliance_codes[i] <- length(unique(ds$ID[ds$Year==nt$year[i] & ds$sector==nt$sector[i]])) # Registry's self-reported compliance rate
	
	# Excluding first year
	nt$noncompliance_codes_n1[i] <- length(unique(dt$ID[dt$Year==nt$year[i] & dt$sector==nt$sector[i] & dt$ComplianceCode %in% c("B","B*","D","D*")])) # Registry's self-reported compliance rate
	nt$noncompliance_codes1[i] <- length(unique(dt$ID[dt$Year==nt$year[i] & dt$sector==nt$sector[i]])) # Registry's self-reported compliance rate	
}
nt$noncompliance_codes <- nt$noncompliance_codes_n / nt$noncompliance_codes
nt$noncompliance_codes1 <- nt$noncompliance_codes_n1 / nt$noncompliance_codes1

pl <- nt

x <- aggregate(pl$noncompliance_codes, by=list(pl$sector), mean)
i <- order(x$x)	# Rank order
i <- rev(order(x$Group.1)) # Alphabetical
x <- x[i,]
dotchart(x$x,labels=x$Group.1, cex=1, col=rgb(0.20,0.20,0.70,alpha=1), xlim=c(0,.25),
  main="",
  xlab="Average annual non-compliance rate")

# Exclude first year of operation
x <- aggregate(pl$noncompliance_codes1, by=list(pl$sector), mean, na.rm=TRUE)
x <- x[i,]
points(x$x, 1:nrow(x), col=rgb(0.20,0.20,0.70,alpha=.3), pch=19)

abline(v=0, lty=3, col="grey")
abline(v=.01, lty=3, col="grey")
abline(v=.05, lty=3, col="grey")
abline(v=.1, lty=3, col="grey")
abline(v=.2, lty=3, col="grey")

dev.off()


### Explanatory power
summary(lm(noncompliance_codes ~ enforcement_upper + fine + I(enforcement_upper * fine), data=nc[!is.na(nc$noncompliance_codes) & !is.na(nc$enforcement_upper) & !is.na(nc$fine) & !is.na(nc$inspection) & !is.na(nc$suspension) & !is.na(nc$namingshaming) & !is.na(nc$supervision) & !is.na(nc$appointment) & !is.na(nc$fines),]))
summary(lm(noncompliance_codes ~ enforcement_upper + fine + I(enforcement_upper * fine) + inspection + suspension + namingshaming + supervision + appointment + fines, data=nc[!is.na(nc$noncompliance_codes) & !is.na(nc$enforcement_upper) & !is.na(nc$fine) & !is.na(nc$inspection) & !is.na(nc$suspension) & !is.na(nc$namingshaming) & !is.na(nc$supervision) & !is.na(nc$appointment) & !is.na(nc$fines),]))

# Wald test
# Critical F (alpha=.05) =  2.17190001
# Critical F (alpha=.1) =  1.82180057 
rss1 <- lm(noncompliance_codes ~ enforcement_upper + fine + I(enforcement_upper * fine), data=nc[!is.na(nc$noncompliance_codes) & !is.na(nc$enforcement_upper) & !is.na(nc$fine) & !is.na(nc$inspection) & !is.na(nc$suspension) & !is.na(nc$namingshaming) & !is.na(nc$supervision) & !is.na(nc$appointment) & !is.na(nc$fines),])$residuals
rss2 <- lm(noncompliance_codes ~ enforcement_upper + fine + I(enforcement_upper * fine) + inspection + suspension + namingshaming + supervision + appointment + fines, data=nc[!is.na(nc$noncompliance_codes) & !is.na(nc$enforcement_upper) & !is.na(nc$fine) & !is.na(nc$inspection) & !is.na(nc$suspension) & !is.na(nc$namingshaming) & !is.na(nc$supervision) & !is.na(nc$appointment) & !is.na(nc$fines),])$residuals

((sum(rss1^2) - sum(rss2^2))/6)/(sum(rss2^2)/(length(rss2) - 10)) # = 1.519005 -> do not reject equivalence at 5% significance level.


summary(lm(noncompliance_codes ~ I(enforcement_upper * fine), data=nc[!is.na(nc$noncompliance_codes) & !is.na(nc$enforcement_upper) & !is.na(nc$fine) & !is.na(nc$inspection) & !is.na(nc$suspension) & !is.na(nc$namingshaming) & !is.na(nc$supervision) & !is.na(nc$appointment) & !is.na(nc$fines),]))
summary(lm(noncompliance_codes ~ I(enforcement_upper * fine) + inspection + suspension + namingshaming + supervision + appointment + fines, data=nc[!is.na(nc$noncompliance_codes) & !is.na(nc$enforcement_upper) & !is.na(nc$fine) & !is.na(nc$inspection) & !is.na(nc$suspension) & !is.na(nc$namingshaming) & !is.na(nc$supervision) & !is.na(nc$appointment) & !is.na(nc$fines),]))

summary(glm(noncompliance_codes ~ enforcement_upper + fine + I(enforcement_upper * fine), family = binomial(link = "logit"), data=nc[!is.na(nc$noncompliance_codes) & !is.na(nc$enforcement_upper) & !is.na(nc$fine) & !is.na(nc$inspection) & !is.na(nc$suspension) & !is.na(nc$namingshaming) & !is.na(nc$supervision) & !is.na(nc$appointment) & !is.na(nc$fines),]))
summary(glm(noncompliance_codes ~ I(enforcement_upper * fine) + inspection + suspension + namingshaming + supervision + appointment + fines, family = binomial(link = "logit"), data=nc[!is.na(nc$noncompliance_codes) & !is.na(nc$enforcement_upper) & !is.na(nc$fine) & !is.na(nc$inspection) & !is.na(nc$suspension) & !is.na(nc$namingshaming) & !is.na(nc$supervision) & !is.na(nc$appointment) & !is.na(nc$fines),]))

