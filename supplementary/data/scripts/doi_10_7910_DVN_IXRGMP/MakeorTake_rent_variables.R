#energy_MC

#agricultureGdpShare    Agriculture, value added (% of GDP)
#pikettyLand    Land value as a percentage of national income (Piketty) - Note: only available in limited years for Britain, Canada, France, Germany, and USA.
#Total_Oil_Income_PC    Real Value of Petroleum Produced Per Capita
#Coal_Income_PC    Real Value of Coal Produced Per Capita
#natural_gas_income_PC    Real Value of Natural Gas Produced Per Capita
#Total_Fuel_Income_PC    Real Value of Petroleum, Coal, and Natural Gas Produced Per Capita
#metals_income_PC    Real Value of Metal Minerals Produced Per Capita
#Total_Resources_Income_PC    Real Value of Petroleum, Coal, Natural Gas, and Metals Produced Per Capita
#Fiscal_Reliance    Percentage of Government Revenues from oil, gas, and minerals

load("MasterDataset.RData")

SCIENCE <- function(){
    
    exp.y <- list(expression(10^-6), expression(10^-5), expression(10^-4), expression(10^-3), expression(10^-2), expression(10^-1), expression(10^0), expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5), expression(10^6))
    
    POW <- c(-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6)
    
    for(i in 1:length(POW)){
        axis(side=2, at=POW[i], labels=exp.y[[i]], font=1, cex.axis=1.35, padj=0, las=2)
        #axis(side=1, at=POW[i], labels=exp.y[[i]], font=1, cex.axis=1.35, padj=0, las=1)
        #abline(h=POW[i], col=grey(.75), lty=2)
        #abline(v=POW[i], col=grey(.75), lty=2)
    }
}

# -------------------------------------------------- #
#pikettyLand    Land value as a percentage of national income (Piketty) - Note: only available in limited years for Britain, Canada, France, Germany, and USA.
# -------------------------------------------------- #
par(mar=c(5,5,2,2))
temp <- subset(master, !is.na(pikettyLand))
YEAR <- min(temp$year):max(temp$year)

cor.test(temp$pikettyLand, temp$energy_MC)
cor.test(log10(temp$pikettyLand+1), log10(temp$energy_MC+1))

plot(temp$pikettyLand ~ temp$year, type="n", ylim=c(0,300), yaxt="n", ylab="", xlab="")
lines(lowess(temp$pikettyLand ~ temp$year), lwd=2)
points(temp$pikettyLand ~ temp$year, pch=21, bg=grey(.9), cex=.5)
axis(side=2, at=c(0,50,100,150,200,250,300), las=2)
axis(1, at=c(YEAR), labels=rep("", length(YEAR)), las=1)
mtext(side=1, "Year", line=3.5, font=2,  cex=1.25)
mtext(side=2, "Land value as % of National Income (Piketty)", line=4.0, font=2,  cex=1.25)

plot(log10(temp$pikettyLand) ~ temp$year, type="n", ylim=c(0,3), yaxt="n", ylab="", xlab="")
lines(lowess(log10(temp$pikettyLand) ~ temp$year), lwd=2)
points(log10(temp$pikettyLand) ~ temp$year, pch=21, bg=grey(.9), cex=.5)
mtext(side=1, "Year", line=3.5, font=2,  cex=1.25)
mtext(side=2, "Land value as % of National Income (Piketty)", line=4.0, font=2,  cex=1.25)
SCIENCE()
axis(1, at=c(YEAR), labels=rep("", length(YEAR)), las=1)



# -------------------------------------------------- #
#agricultureGdpShare    Agriculture, value added (% of GDP)
# -------------------------------------------------- #
par(mar=c(5,5,2,2))
temp <- subset(master, !is.na(agricultureGdpShare))
YEAR <- min(temp$year):max(temp$year)

cor.test(temp$agricultureGdpShare, temp$energy_MC)
cor.test(log10(temp$agricultureGdpShare+1), log10(temp$energy_MC+1))

plot(temp$agricultureGdpShare ~ temp$year, type="n", ylim=c(0,100), yaxt="n", ylab="", xlab="")
lines(lowess(temp$agricultureGdpShare ~ temp$year), lwd=2)
points(temp$agricultureGdpShare ~ temp$year, pch=21, bg=grey(.9), cex=.5)
axis(side=2, at=c(0,10,20,30,40,50,60,70,80,90,100), las=2)
axis(1, at=c(YEAR), labels=rep("", length(YEAR)), las=1)
mtext(side=1, "Year", line=3.5, font=2,  cex=1.25)
mtext(side=2, "Agriculture, value added (% of GDP)", line=4.0, font=2,  cex=1.25)

plot(log10(temp$agricultureGdpShare) ~ temp$year, type="n", ylim=c(-2,2), yaxt="n", ylab="", xlab="")
lines(lowess(log10(temp$agricultureGdpShare) ~ temp$year), lwd=2)
points(log10(temp$agricultureGdpShare) ~ temp$year, pch=21, bg=grey(.9), cex=.5)
mtext(side=1, "Year", line=3.5, font=2,  cex=1.25)
mtext(side=2, "Agriculture, value added (% of GDP)", line=4.0, font=2,  cex=1.25)
SCIENCE()
axis(1, at=c(YEAR), labels=rep("", length(YEAR)), las=1)



# -------------------------------------------------- #
#Coal_Income_PC    Real Value of Coal Produced Per Capita
# -------------------------------------------------- #
par(mar=c(5,5,2,2))
temp <- subset(master, !is.na(Coal_Income_PC))
YEAR <- min(temp$year):max(temp$year)

cor.test(temp$Coal_Income_PC, temp$energy_MC)
cor.test(log10(temp$Coal_Income_PC+1), log10(temp$energy_MC+1))

plot(temp$Coal_Income_PC ~ temp$year, type="n", ylim=c(0,100), yaxt="n", ylab="", xlab="")
lines(lowess(temp$Coal_Income_PC ~ temp$year), lwd=2)
points(temp$Coal_Income_PC ~ temp$year, pch=21, bg=grey(.9), cex=.5)
axis(side=2, at=c(0,10,20,30,40,50,60,70,80,90,100), las=2)
axis(1, at=c(YEAR), labels=rep("", length(YEAR)), las=1)
mtext(side=1, "Year", line=3.5, font=2,  cex=1.25)
mtext(side=2, "Real Value of Coal Produced Per Capita", line=4.0, font=2,  cex=1.25)

plot(log10(temp$Coal_Income_PC) ~ temp$year, type="n", ylim=c(-2,2), yaxt="n", ylab="", xlab="")
lines(lowess(log10(temp$Coal_Income_PC) ~ temp$year), lwd=2)
points(log10(temp$Coal_Income_PC) ~ temp$year, pch=21, bg=grey(.9), cex=.5)
mtext(side=1, "Year", line=3.5, font=2,  cex=1.25)
mtext(side=2, "Real Value of Coal Produced Per Capita", line=4.0, font=2,  cex=1.25)
SCIENCE()
axis(1, at=c(YEAR), labels=rep("", length(YEAR)), las=1)



# -------------------------------------------------- #
#natural_gas_income_PC    Real Value of Natural Gas Produced Per Capita
# -------------------------------------------------- #
par(mar=c(5,5,2,2))
temp <- subset(master, !is.na(natural_gas_income_PC))
YEAR <- min(temp$year):max(temp$year)

cor.test(temp$natural_gas_income_PC, temp$energy_MC)
cor.test(log10(temp$natural_gas_income_PC+1), log10(temp$energy_MC+1))



# -------------------------------------------------- #
#Total_Fuel_Income_PC    Real Value of Petroleum, Coal, and Natural Gas Produced Per Capita
# -------------------------------------------------- #
par(mar=c(5,5,2,2))
temp <- subset(master, !is.na(Total_Fuel_Income_PC))
YEAR <- min(temp$year):max(temp$year)

cor.test(temp$Total_Fuel_Income_PC, temp$energy_MC)
cor.test(log10(temp$Total_Fuel_Income_PC+1), log10(temp$energy_MC+1))



# -------------------------------------------------- #
#metals_income_PC    Real Value of Metal Minerals Produced Per Capita
# -------------------------------------------------- #
par(mar=c(5,5,2,2))
temp <- subset(master, !is.na(metals_income_PC))
YEAR <- min(temp$year):max(temp$year)

cor.test(temp$metals_income_PC, temp$energy_MC)
cor.test(log10(temp$metals_income_PC+1), log10(temp$energy_MC+1))



# -------------------------------------------------- #
#Total_Resources_Income_PC    Real Value of Petroleum, Coal, Natural Gas, and Metals Produced Per Capita
# -------------------------------------------------- #
par(mar=c(5,5,2,2))
temp <- subset(master, !is.na(Total_Resources_Income_PC))
YEAR <- min(temp$year):max(temp$year)

cor.test(temp$Total_Resources_Income_PC, temp$energy_MC)
cor.test(log10(temp$Total_Resources_Income_PC+1), log10(temp$energy_MC+1))



# -------------------------------------------------- #
#Fiscal_Reliance    Percentage of Government Revenues from oil, gas, and minerals
# -------------------------------------------------- #
par(mar=c(5,5,2,2))
temp <- subset(master, !is.na(Fiscal_Reliance))
YEAR <- min(temp$year):max(temp$year)

cor.test(temp$Fiscal_Reliance, temp$energy_MC)
cor.test(log10(temp$Fiscal_Reliance+1), log10(temp$energy_MC+1))




# -------------------------------------------------- #
#Total_Oil_Income_PC    Real Value of Petroleum Produced Per Capita
# -------------------------------------------------- #
par(mar=c(5,5,2,2))
temp <- subset(master, !is.na(Total_Oil_Income_PC))
YEAR <- min(temp$year):max(temp$year)

cor.test(temp$Total_Oil_Income_PC, temp$energy_MC)
cor.test(log10(temp$Total_Oil_Income_PC+1), log10(temp$energy_MC+1))





