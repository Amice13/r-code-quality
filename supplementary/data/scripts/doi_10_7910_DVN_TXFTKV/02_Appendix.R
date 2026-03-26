# Sells 2019 "Building Parties from City Hall"
# Replication Code for Figures and Tables in the Online Appendix

rm(list = ls())

library(rdrobust)
library(rdd)


# Set the working directory to the folder where the datasets are saved
setwd("")


### Function for formatting rdrobust output into a matrix ###

RD.estimates <- function(y.name, x.name, data, subset=T, order=1){
  # y.name: name of dependent variable in dataset
  # x.name: name of running variable in dataset
  sub <- data[subset,]
  y <- sub[, y.name]
  x <- sub[, x.name]
  cluster <- sub[, "MunCode"]
  mod <- rdrobust(y, x, cluster=cluster, p=order)
  results <- matrix(c(mod$coef[3], mod$se[3], mod$pv[3], mod$ci[3, ], mod$bws[1,1], sum(mod$Nh)), ncol=7, byrow=T, dimnames=list(c(), c("Coefficient", "SE", "p-value", "CI Lower", "CI Upper", "h", "n")))
  return(results)
}


### Functions for Pre-Treatment Covariate Balance Tests ###

# Difference-in-Means
ttest.bandwidth <- function(var, h, subset.variable, subsets){
  results <- rep(NA, length(h))
  for(i in 1:length(subsets)){
    loser <- data[data$Margin > -h & data$Margin < 0 & data[,subset.variable] == subsets[i],]
    winner <- data[data$Margin < h & data$Margin > 0 & data[,subset.variable] == subsets[i],]
    test <- t.test(na.omit(loser[, var]), na.omit(winner[, var]))
    results[i] <- test$p.value
  }
  return(results)
}


# Regression Discontinuity using pre-treatment covariates as the DV
rd.test <- function(var, subset.variable, subsets){
  results <- rep(NA, length(subsets))
  for(i in 1:length(subsets)){
    test <- RD.estimates(var, "Margin", data, data[, subset.variable] == subsets[i])
    results[i] <- test[3]
  }
  return(results)
}


### Functions for testing robustness to alternative bandwidths ###

# Local Linear Specification
Sensitivity.Plot.LL <- function(data, DV, running.var, party, lower.width=0.02, upper.width=0.2, ylim=c(-1.5, 1.5)){
  running <- data[, running.var]
  dv <- data[, DV]
  width <- seq(lower.width, upper.width, by=.01)
  results <- numeric(length(width))
  lower <- numeric(length(width))
  upper <- numeric(length(width))
  for(i in 1:length(width)){
    mod <- RDestimate(dv ~ running, data=data, bw=width[i])
    results[i] <- mod$est[1]
    lower[i] <- mod$ci[1, 1]
    upper[i] <- mod$ci[1, 2]
  }
  plot(width, results, ylim=ylim, ylab="Treatment Effect", xlab="Bandwidth", main=party)
  segments(width, lower, width, upper)
  abline(h = 0, lty=3)
}

# Difference-in-Means Specification
Sensitivity.Plot.DM <- function(data, DV, running.var, party, lower.width=0.02, upper.width=0.2, ylim=c(-1.5, 1.5)){
  running <- data[, running.var]
  dv <- data[, DV]
  width <- seq(lower.width, upper.width, by=.01)
  results <- numeric(length(width))
  lower <- numeric(length(width))
  upper <- numeric(length(width))
  for(i in 1:length(width)){
    loser <- data[running > -width[i] & running < 0, DV]
    winner <- data[running < width[i] & running > 0, DV]
    test <- t.test(loser, winner)
    results[i] <- test$estimate[2] - test$estimate[1]
    lower[i] <--test$conf.int[1]
    upper[i] <- -test$conf.int[2]
  }
  plot(width, results, ylim=ylim, ylab="Treatment Effect", xlab="Bandwidth", main=party)
  segments(width, lower, width, upper)
  abline(h = 0, lty=3)
}



### Function for plotting RD effect at placebo thresholds between -0.1 and +0.1 margin of victory ###

Placebo.Plot <- function(data, DV, running.var, title, lower.width=-0.1, upper.width=0.1, ylim=c(-1.5, 1.5)){
  running <- data[, running.var]
  dv <- data[, DV]
  cutpoint <- seq(lower.width, upper.width, by=.02)
  results <- numeric(length(cutpoint))
  lower <- numeric(length(cutpoint))
  upper <- numeric(length(cutpoint))
  for(i in 1:length(cutpoint)){
    mod <- rdrobust(dv, running, c=cutpoint[i])
    results[i] <- mod$coef[3]
    lower[i] <- mod$ci[3, 1]
    upper[i] <- mod$ci[3, 2]
  }
  plot(cutpoint, results, ylim=ylim, ylab="Treatment Effect", xlab="Threshold", main=title)
  segments(cutpoint, lower, cutpoint, upper)
  abline(h = 0, lty=3)
}



### Figure A1: Covariate Balance by Party Type ###

data <- readRDS("MembersRD.rds")

# Calculate p-values based on a t-test
h <- .05
type <- c("Centralized-Programmatic",  "Centralized-Clientelistic",  "Decentralized" )
variables <- c("NewMembers01", "VereadorVoteshare00", "LegendaVotes00", "VereadoresElected00", "LulaVoteshare02", "DeputiesVoteshare02", "LegendaVotes02",  "CopartisanGovernor02", "CopartisanGovernor98", "IsIncumbentParty", "Electorate04", "ElectoratePercentChange", "PublicEmployment04", "DiscretionaryPositions04", "GDP.PerCapita", "BolsaSpending.PerCapita", "BolsaReceipients", "CapitalSpending.PerCapita", "NE", "DistanceFromCapital", "LaborForceParticipationRate", "HumanDevelopmentIndex", "EducationIndex", "HealthIndex", "LiteracyRate", "RuralPopulationShare")
p.values <- c()
for(var in variables){
  p.values <- rbind(p.values, data.frame(matrix(ttest.bandwidth(var, h, "Type", type), nrow=1, dimnames=list(var, NULL))))
}

# Calculate p-values based on a placebo RD on pre-treatment covariates
p.values.rd <- c()
for(var in variables){
  p.values.rd <- rbind(p.values.rd, data.frame(matrix(rd.test(var, "Type", type), nrow=1, dimnames=list(var, NULL))))
}

variable.labels <- c(expression("Affiliations Rate (01-05)"["mp"]), expression("Council Voteshare (00)"["mp"]), expression("Voteshare on Legenda (00)"["mp"]),  expression("# Councilors Elected (00)"["mp"]), expression("Lula Voteshare (02)"["m"]), expression("Deputies Voteshare (02)"["mp"]), expression("Voteshare on Legenda (02)"["mp"]),  expression("Copartisan Governor (03-07)"["mp"]), expression("Copartisan Governor (99-03)"["mp"]), expression("Copartisan Mayor (01-05)"["mp"]), expression("Electorate (04)"["m"]), expression("% Change Electorate (02-04)"["m"]), expression("Public Employees Per Capita"["m"]), expression("Discretionary Appts. Per Capita"["m"]), expression("GDP Per Capita (04)"["m"]), expression("Bolsa Familia Spending (04)"["m"]), expression("Bolsa Familia Receipients (04)"["m"]), expression("Capital Spending (04)"["m"]), expression("Northeast"["m"]), expression("Distance from Capital"["m"]), expression("Labor Force Participation Rate (00)"["m"]), expression("Human Development Index (00)"["m"]), expression("Education Index (00)"["m"]), expression("Health Index (00)"["m"]), expression("Literacy Rate (00)"["m"]), expression("Rural Population Share (00)"["m"]))

# Plot the p-values
pdf("BPFCH_BalancePlot.pdf", width=9, height=6)
par(mfrow=c(1,4), las=1)
length <- dim(p.values)[1]
plot(NA, ylim=c(1.3, length-.3), xlim=c(0, 1.01), bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
par(las=1, mar=c(4.1,20.1,1.1,2.1))
par(mar=c(3.1,0.5,2.1,1.1))
cex.axis <- 1.19
plot(NA, ylim=c(1.3, length-.5), xlim=c(0, 1.01), ylab="", xlab="", yaxt="n", xaxs="i", main=type[1])
title(xlab="p-value", line=2)
points(p.values[,1], length:1, pch=16)
points(p.values.rd[,1], length:1, pch=1)
axis(2, length:1, labels=variable.labels, cex.axis=cex.axis)
abline(v=.05, lty=1, lwd=3)
abline(h = 1:length + .5, lty=3)

plot(NA, ylim=c(1.3, length-.5), xlim=c(0, 1.01), ylab="", xlab="", yaxt="n", xaxs="i", main=type[2])
title(xlab="p-value", line=2)
points(p.values[,2], length:1, pch=16)
points(p.values.rd[,2], length:1, pch=1)
abline(v=.05, lty=1, lwd=3)
abline(h = 1:length + .5, lty=3)

plot(NA, ylim=c(1.3, length-.5), xlim=c(0, 1.01), ylab="", xlab="", yaxt="n", xaxs="i", main=type[3])
title(xlab="p-value", line=2)
points(p.values[,3], length:1, pch=16)
points(p.values.rd[,3], length:1, pch=1)
abline(v=.05, lty=1, lwd=3)
abline(h = 1:length + .5, lty=3)
par(mfrow=c(1,1))
dev.off()



### Figure A2: Covariate Balance for the PT and PSDB samples ###

data <- readRDS("MembersRD.rds")

# Calculate p-values based on a t-test
variables <- c("NewMembers01", "VereadorVoteshare00", "LegendaVotes00", "VereadoresElected00", "LulaVoteshare02", "DeputiesVoteshare02", "LegendaVotes02",  "CopartisanGovernor02", "CopartisanGovernor98", "IsIncumbentParty", "Electorate04", "ElectoratePercentChange", "PublicEmployment04", "DiscretionaryPositions04", "GDP.PerCapita", "BolsaSpending.PerCapita", "BolsaReceipients", "CapitalSpending.PerCapita", "NE", "DistanceFromCapital", "LaborForceParticipationRate", "HumanDevelopmentIndex", "EducationIndex", "HealthIndex", "LiteracyRate", "RuralPopulationShare")
variable.labels <- c(expression("Affiliations Rate (01-05)"["mp"]), expression("Council Voteshare (00)"["mp"]), expression("Voteshare on Legenda (00)"["mp"]),  expression("# Councilors Elected (00)"["mp"]), expression("Lula Voteshare (02)"["m"]), expression("Deputies Voteshare (02)"["mp"]), expression("Voteshare on Legenda (02)"["mp"]),  expression("Copartisan Governor (03-07)"["mp"]), expression("Copartisan Governor (99-03)"["mp"]), expression("Copartisan Mayor (01-05)"["mp"]), expression("Electorate (04)"["m"]), expression("% Change Electorate (02-04)"["m"]), expression("Public Employees Per Capita"["m"]), expression("Discretionary Appts. Per Capita"["m"]), expression("GDP Per Capita (04)"["m"]), expression("Bolsa Familia Spending (04)"["m"]), expression("Bolsa Familia Receipients (04)"["m"]), expression("Capital Spending (04)"["m"]), expression("Northeast"["m"]), expression("Distance from Capital"["m"]), expression("Labor Force Participation Rate (00)"["m"]), expression("Human Development Index (00)"["m"]), expression("Education Index (00)"["m"]), expression("Health Index (00)"["m"]), expression("Literacy Rate (00)"["m"]), expression("Rural Population Share (00)"["m"]))
p.values <- c()
h <- .08
type <- c("PT", "PSDB")
for(var in variables){
  p.values <- rbind(p.values, data.frame(matrix(ttest.bandwidth(var, h, "Party", type), nrow=1, dimnames=list(var, NULL))))
}

# Calculate p-values based on a placebo RD
p.values.rd <- c()
for(var in variables){
  p.values.rd <- rbind(p.values.rd, data.frame(matrix(rd.test(var, "Party", type), nrow=1, dimnames=list(var, NULL))))
}

# Plot the p-values
pdf("BPFCH_BalancePlotPTPSDB.pdf", width=9, height=6)
par(mfrow=c(1,3), las=1)
plot(NA, ylim=c(1.3, length-.3), xlim=c(0, 1.01), bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
par(las=1, mar=c(4.1,20.1,1.1,2.1))
par(mar=c(3.1,0.5,2.1,1.1))
cex.axis <- 1.19
length <- dim(p.values)[1]

plot(NA, ylim=c(1.3, length-.5), xlim=c(0, 1.01), ylab="", xlab="", yaxt="n", xaxs="i", main=type[1])
title(xlab="p-value", line=2)
points(p.values[,1], length:1, pch=16)
points(p.values.rd[,1], length:1, pch=1)
axis(2, length:1, labels=variable.labels, cex.axis=cex.axis)
abline(v=.05, lty=1, lwd=3)
abline(h = 1:length + .5, lty=3)

plot(NA, ylim=c(1.3, length-.5), xlim=c(0, 1.01), ylab="", xlab="", yaxt="n", xaxs="i", main=type[2])
title(xlab="p-value", line=2)
points(p.values[,2], length:1, pch=16)
points(p.values.rd[,2], length:1, pch=1)
abline(v=.05, lty=1, lwd=3)
abline(h = 1:length + .5, lty=3)
par(mfrow=c(1,1))
dev.off()



### Figure B1: McCrary Density Plots for the Pooled Samples ###
data <- readRDS("MembersRD.rds")

pdf("BPFCH_Sorting.pdf", width=10, height=4)
par(mfrow=c(1,3), mar=c(2.1,3.1,2.1,2.1), las=1)
DCdensity(data[data$Type == "Centralized-Programmatic",]$Margin)
title(main="Centralized-Programmatic")
DCdensity(data[data$Type == "Centralized-Clientelistic",]$Margin)
title(main="Centralized-Clientelistic")
DCdensity(data[data$Type == "Decentralized",]$Margin)
title(main="Decentralized")
par(mfrow=c(1,1))
dev.off()


### Figure B2: McCrary Density Plots for the PT and PSDB Samples ###
data <- readRDS("MembersRD.rds")

pdf("BPFCH_SortingPTPSDB.pdf", width=10, height=4)
par(mfrow=c(1,2), mar=c(2.1,3.1,2.1,2.1), las=1)
DCdensity(data[data$Party == "PT",]$Margin)
title(main="PT")
DCdensity(data[data$Party == "PSDB",]$Margin)
title(main="PSDB")
par(mfrow=c(1,1))
dev.off()



### Figure C1: Robustness to Alternative Bandwidths ###

data <- readRDS("MembersRD.rds")

# Make all alternative bandwidth plots
pdf("BPFCH_Robustness.pdf", width=10, height=12)
par(mfrow=c(6,2), mar=c(2.1,2.6,1.1,1.1), las=1)
plot(NA, bty="n", xaxt="n", yaxt="n", xlab="", ylab="", ylim=c(0, 1), xlim=c(0, 1))
text(.5, .1, "Local Linear RD", cex=2)
plot(NA, bty="n", xaxt="n", yaxt="n", xlab="", ylab="", ylim=c(0, 1), xlim=c(0, 1))
text(.5, .1, "Difference in Means", cex=2)
Sensitivity.Plot.LL(data[data$Type == "Centralized-Programmatic",], "NewMembers05", "Margin", "Main Results: Centralized-Programmatic", lower.width = .01)
Sensitivity.Plot.DM(data[data$Type == "Centralized-Programmatic",], "NewMembers05", "Margin", "Main Results: Centralized-Programmatic", lower.width = .01)
Sensitivity.Plot.LL(data[data$Type == "Centralized-Clientelistic",], "NewMembers05", "Margin", "Main Results: Centralized-Clientelistic", lower.width = .01)
Sensitivity.Plot.DM(data[data$Type == "Centralized-Clientelistic",], "NewMembers05", "Margin", "Main Results: Centralized-Clientelistic", lower.width = .01)
Sensitivity.Plot.LL(data[data$Type == "Decentralized",], "NewMembers05", "Margin", "Main Results: Decentralized", lower.width = .01)
Sensitivity.Plot.DM(data[data$Type == "Decentralized",], "NewMembers05", "Margin", "Main Results: Decentralized", lower.width = .01)
Sensitivity.Plot.LL(data[data$Type == "Centralized-Programmatic" & data$Office == 1,], "NewMembers05", "Margin", "Established Presence: Centralized-Programmatic", lower.width = .01)
Sensitivity.Plot.DM(data[data$Type == "Centralized-Programmatic" & data$Office == 1,], "NewMembers05", "Margin", "Established Presence: Centralized-Programmatic", lower.width = .01)
Sensitivity.Plot.LL(data[data$Type == "Centralized-Programmatic" & data$Office == 0,], "NewMembers05", "Margin", "New Presence: Centralized-Programmatic", lower.width = .01)
Sensitivity.Plot.DM(data[data$Type == "Centralized-Programmatic" & data$Office == 0,], "NewMembers05", "Margin", "New Presence: Centralized-Programmatic", lower.width = .01)
par(mfrow=c(1,1))
dev.off()




### Figure D1: Robustness to Higher-Order Polynomial Specifications ###

data <- readRDS("MembersRD.rds")

# Re-calculate RD estimates using polynomials of orders 1, 2, and 3
pdf("BPFCH_HigherOrder.pdf", width=10, height=6)
par(mfrow=c(1,2), las=1)
results <- rbind(RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Programmatic"),
                 RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Programmatic", order=2),
                 RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Programmatic", order=3),
                 RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Clientelistic"),
                 RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Clientelistic", order=2),
                 RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Clientelistic", order=3),
                 RD.estimates("NewMembers05", "Margin", data, data$Type == "Decentralized"),
                 RD.estimates("NewMembers05", "Margin", data, data$Type == "Decentralized", order=2),
                 RD.estimates("NewMembers05", "Margin", data, data$Type == "Decentralized", order=3))

point.type <- c(16, 17, 15)
point.col <- "black"
places <- c(1:3, 5:7, 9:11)
plot(NA, xlim=c(.5, 11.5), ylim=c(-1, 1), xlab="", ylab="Treatment Effect", xaxt="n", main="Pooled Samples", cex.main=1.2, cex.axis=1, cex.lab=1)   
segments(places, results[,4], places, results[,5], lwd=4)
points(places, results[,1], pch=point.type, cex=2, bg=point.col)
abline(h=0, lty=3)
mtext(1, at=places[c(2, 5, 8)], text=c("Centralized-\nProgrammatic", "Centralized-\nClientelistic", "Decentralized\n"),  cex=1.1, line=1.2)
legend("topright", c("Local-Linear", "Second-Order", "Third-Order"), pch=point.type, cex=1, bty="n", pt.cex = 1.5)

results <- rbind(RD.estimates("NewMembers05", "Margin", data, data$Party == "PT"),
                 RD.estimates("NewMembers05", "Margin", data, data$Party == "PT", order=2),
                 RD.estimates("NewMembers05", "Margin", data, data$Party == "PT", order=3),
                 RD.estimates("NewMembers05", "Margin", data, data$Party == "PSDB"),
                 RD.estimates("NewMembers05", "Margin", data, data$Party == "PSDB", order=2),
                 RD.estimates("NewMembers05", "Margin", data, data$Party == "PSDB", order=3),
                 RD.estimates("NewMembers05", "Margin", data, data$Party == "PMDB"),
                 RD.estimates("NewMembers05", "Margin", data, data$Party == "PMDB", order=2),
                 RD.estimates("NewMembers05", "Margin", data, data$Party == "PMDB", order=3),
                 RD.estimates("NewMembers05", "Margin", data, data$Party == "PFL"),
                 RD.estimates("NewMembers05", "Margin", data, data$Party == "PFL", order=2),
                 RD.estimates("NewMembers05", "Margin", data, data$Party == "PFL", order=3))

point.type <- c(16, 17, 15)
point.col <- "black"
places <- c(1:3, 5:7, 9:11, 13:15)
plot(NA, xlim=c(.5, 15.5), ylim=c(-1, 1), xlab="", ylab="Treatment Effect", xaxt="n", main="Party Samples", cex.main=1.2, cex.axis=1, cex.lab=1)   
segments(places, results[,4], places, results[,5], lwd=4)
points(places, results[,1], pch=point.type, cex=2, bg=point.col)
abline(h=0, lty=3)
mtext(1, at=places[c(2, 5, 8, 11)], text=c("PT", "PSDB", "PMDB", "PFL"),  cex=1.1, line=.5)
legend("topright", c("Local-Linear", "Second-Order", "Third-Order"), pch=point.type, cex=1, bty="n", pt.cex = 1.5)
par(mfrow=c(1,1))
dev.off()



### Figure E1: Treatment Effect at Placebo Thresholds ###

data <- readRDS("MembersRD.rds")

# Make all placebo threshold plots for Centralized-Programmatic, PT, and PSDB samples
pdf("BPFCH_Placebo.pdf", width=8, height=6)
par(mfrow=c(3,2), mar=c(2.1,2.6,1.1,1.1), las=1)
Placebo.Plot(data[data$Type == "Centralized-Programmatic",], "NewMembers05", "Margin", "Centralized-Programmatic, All Municipalities", ylim=c(-1, 1))
Placebo.Plot(data[data$Type == "Centralized-Programmatic" & data$Office == 1,], "NewMembers05", "Margin", "Centralized-Programmatic, Established Presence", ylim=c(-1, 1))
Placebo.Plot(data[data$Party == "PT",], "NewMembers05", "Margin", "PT, All Municipalities", ylim=c(-1, 1))
Placebo.Plot(data[data$Party == "PT" & data$Office == 1,], "NewMembers05", "Margin", "PT, Established Presence", ylim=c(-1, 1))
Placebo.Plot(data[data$Party == "PSDB",], "NewMembers05", "Margin", "PSDB, All Municipalities", ylim=c(-1, 1))
Placebo.Plot(data[data$Party == "PSDB"& data$Office == 1,], "NewMembers05", "Margin", "PSDB, Established Presence", ylim=c(-1, 1))
par(mfrow=c(1,1))
dev.off()



### Figure G1: Estimates of Party Centralization and Programmatism for Different Combinations of Survey Questions ###

Parties <- readRDS("PartyData.rds")

# The Idea: Cycle through each combination of a subset of the questions used to code party centralization and programmatism.
# Report the party type scores based on that subset. Any other weighting scheme based on these questions will produce scores that fall in the convex hull of the collection of score points for each party.
# Standardize each party's score by comparing it to the corresponding score for the PMDB.
pdf("BPFCH_PartyClassificationReliability.pdf", width=12, height=6)
par(mar=c(3.1,3.1,2.1,2.1))

# Use these permutations to extract different combinations of the questions
perm <- matrix(c(T, T, T, T, T, T,
                 T, T, F, T, T, T,
                 T, T, F, F, T, T,
                 T, T, T, F, T, T,
                 T, F, T, T, T, T,
                 T, F, F, T, T, T,
                 T, F, T, F, T, T,
                 T, T, T, T, T, F,
                 T, T, F, T, T, F,
                 T, T, F, F, T, F,
                 T, T, T, F, T, F,
                 T, F, T, T, T, F,
                 T, F, F, T, T, F,
                 T, F, T, F, T, F,
                 T, T, T, T, F, T,
                 T, T, F, T, F, T,
                 T, T, F, F, F, T,
                 T, T, T, F, F, T,
                 T, F, T, T, F, T,
                 T, F, F, T, F, T,
                 T, F, T, F, F, T,
                 F, T, T, T, T, T,
                 F, T, F, T, T, T,
                 F, T, F, F, T, T,
                 F, T, T, F, T, T,
                 F, F, T, T, T, T,
                 F, F, F, T, T, T,
                 F, F, T, F, T, T,
                 F, T, T, T, T, F,
                 F, T, F, T, T, F,
                 F, T, F, F, T, F,
                 F, T, T, F, T, F,
                 F, F, T, T, T, F,
                 F, F, F, T, T, F,
                 F, F, T, F, T, F,
                 F, T, T, T, F, T,
                 F, T, F, T, F, T,
                 F,  T, F, F, F, T,
                 F, T, T, F, F, T,
                 F, F, T, T, F, T,
                 F, F, F, T, F, T,
                 F, F, T, F, F, T), ncol=6, byrow = T)

par(mfrow=c(1,2))
plot(NA, type="n", xlim=c(2.2, 3.4), ylim=c(-.75, 0), ylab="", xlab="", xaxt="n", yaxt="n", main="Party Type Scores for Each Permutation")

# Cycle through each permutation and calculate the centralization and programmatism scores for each major party
PT.x <- rep(NA, nrow(perm))
PSDB.x <- rep(NA, nrow(perm))
PMDB.x <- rep(NA, nrow(perm))
PFL.x <- rep(NA, nrow(perm))
PP.x <- rep(NA, nrow(perm))
PL.x <- rep(NA, nrow(perm))
PTB.x <- rep(NA, nrow(perm))
PSB.x <- rep(NA, nrow(perm))
PT.y <- rep(NA, nrow(perm))
PSDB.y <- rep(NA, nrow(perm))
PMDB.y <- rep(NA, nrow(perm))
PFL.y <- rep(NA, nrow(perm))
PP.y <- rep(NA, nrow(perm))
PL.y <- rep(NA, nrow(perm))
PTB.y <- rep(NA, nrow(perm))
PSB.y <- rep(NA, nrow(perm))
for(i in 1:nrow(perm)){
  n.x <- sum(perm[i, 1:4])
  n.y <- sum(perm[i, 5:6])
  Clien <- (Parties$ConsumerGoods*perm[i, 1] + Parties$SocialServices*perm[i, 2] + Parties$Contracts*perm[i, 3] + Parties$Patronage*perm[i, 4])/n.x
  Cent <- (Parties$CandidateSelection*perm[i, 5] + Parties$Strategy*perm[i, 6])/n.y
  PT.x[i] <- Clien[10]
  PT.y[i] <- Cent[10]
  PSDB.x[i] <- Clien[9]
  PSDB.y[i] <- Cent[9]
  PMDB.x[i] <- Clien[5]
  PMDB.y[i] <- Cent[5]
  PFL.x[i] <- Clien[3]
  PFL.y[i] <- Cent[3]
  PP.x[i] <- Clien[6]
  PP.y[i] <- Cent[6]
  PTB.x[i] <- Clien[11]
  PTB.y[i] <- Cent[11]
  PL.x[i] <- Clien[4]
  PL.y[i] <- Cent[4]
  PSB.x[i] <- Clien[8]
  PSB.y[i] <- Cent[8]
}

# Standardize by taking the distance from the PMDB's score
PSDB.x <- PSDB.x - PMDB.x
PSDB.y <- PSDB.y - PMDB.y
PT.x <- PT.x - PMDB.x
PT.y <- PT.y - PMDB.y
PFL.x <- PFL.x - PMDB.x
PFL.y <- PFL.y - PMDB.y
PP.x <- PP.x - PMDB.x
PP.y <- PP.y - PMDB.y
PTB.x <- PTB.x - PMDB.x
PTB.y <- PTB.y - PMDB.y
PL.x <- PL.x - PMDB.x
PL.y <- PL.y - PMDB.y
PSB.x <- PSB.x - PMDB.x
PSB.y <- PSB.y - PMDB.y
x <- 3.342857
y <- -0.51666667
points(x + PSDB.x, y + PSDB.y, col="blue", pch=16)
points(x + PFL.x, y + PFL.y, col="darkgoldenrod2", pch=16)
points(x + PT.x, y + PT.y, col="red", pch=16)
points(x + PP.x, y + PP.y, col="deepskyblue", pch=16)
points(x + PTB.x, y + PTB.y, col="darkorange2", pch=16)
points(x + PL.x, y + PL.y, col="purple", pch=16)
points(x, y, col="forestgreen", cex=2, pch=16)

# Adjust the label placement slightly so that the party label is legible
text(x + mean(PT.x), y - .05 + mean(PT.y), "PT", col="red", lwd=6, font=2)
text(x + mean(PSDB.x), y + .05 + mean(PSDB.y), "PSDB", col="blue", lwd=6, font=2)
text(x + mean(PFL.x), y - .05 + mean(PFL.y), "PFL", col="darkgoldenrod2", lwd=6, font=2)
text(x + mean(PP.x), y - .04 + mean(PP.y), "PP", col="deepskyblue", lwd=6, font=2)
text(x + mean(PTB.x), y - .05 + mean(PTB.y), "PTB", col="darkorange2", lwd=6, font=2)
text(x + mean(PL.x), y - .03 + mean(PL.y), "PL", col="purple", lwd=6, font=2)
text(x, y - .04, "PMDB", col="forestgreen", lwd=6, font=2)
title(ylab="Decentralized -- Centralized", xlab="Programmatic -- Clientelistic", line=0.5)

# Plot the party scores used in Figure 1
subset <- c(10, 9, 5, 3, 6, 11, 4)
plot(Parties$Clientelism[subset], Parties$Centralization[subset], type="n", xlim=c(2.3, 3.5), ylim=c(-.9, .4), ylab="", xlab="", xaxt="n", yaxt="n", main="Party Type Scores Reported in Figure 1")
text(Parties$Clientelism[subset], Parties$Centralization[subset], Parties$Party[subset])
title(ylab="Decentralized -- Centralized", xlab="Programmatic -- Clientelistic", line=0.5)
par(mfrow=c(1,1))
dev.off()





### Figure H1: Pooled Sample RD Results with the Omission of Each Party ###

data <- readRDS("MembersRD.rds")
parties <- c("PT", "PSDB", "PPS", "PC do B", "PFL", "PL", "PDT", "PMDB", "PP", "PTB", "PSB")
types <- c(rep("Centralized-Programmatic", 4), rep("Centralized-Clientelistic", 3), rep("Decentralized", 4))

# Iterate through each party in the parties vector and re-calculate the RD estimate for that party's type when the party is excluded from the sample
results <- c()
for(i in 1:11){
  mod <- RD.estimates("NewMembers05", "Margin", data, data$Type == types[i] & data$Party != parties[i])
  results <- rbind(results, mod)
}

# Plot RD estimates for each excluded party
places <- 1:11
wall <- c(4.5, 7.5)
pdf("BPFCH_PooledSensitivity.pdf", width=10, height=6)
par(las=1)
plot(NA, ylim=c(-.6, .6), xlim=c(0.75, 11.25), xaxt="n", xlab="Omitted Party", ylab="Treatment Effect")
segments(places, results[,4], places, results[,5], lwd=3)
points(places, results[,1], cex=1.5, pch=19)
axis(1, places, parties)
abline(h=0, lty=3)
abline(v=wall)
mtext(3, at=c(2.5, 6, 9.5), text=c("Centralized-Programmatic", "Centralized-Clientelistic", "Dencentralized"),  cex=1.2, line=.5)
dev.off()



### Figure I1: Proportion of 2004 Candidates who Switched Parties by 2008

data <- readRDS("PartyData.rds")
Index <- data.frame(Party=c("PT", "PSDB", "PPS", "PCdoB", "PFL", "PL", "PDT", "PSB"), index=1:8)
data <- merge(data, Index, by="Party")

pdf("BPFCH_PartySwitch.pdf", width=8, height=6)
par(las=1)
barplot(data$Switch.Rate[order(data$index)], names.arg=data$Party[order(data$index)], ylab="Party Switching Rate", ylim=c(0, .8))
abline(v=c(4.9, 8.5))
dev.off()



### Figure I2: Partisans-Voters Ratio in 2006

data <- readRDS("PartyData.rds")
Index <- data.frame(Party=c("PT", "PSDB", "PFL", "PL", "PDT", "PSB"), index=1:6)
data <- merge(data, Index, by="Party")

pdf("BPFCH_PartisansVotes.pdf", width=8, height=6)
par(las=1)
barplot(data$Partisans.to.Voters[order(data$index)], names.arg=data$Party[order(data$index)], ylab="Partisans : Votes")
abline(v=c(2.5, 6.1))
dev.off()



### Figure I3: Party Cohesion in the 1995-1999 Congress

data <- readRDS("PartyData.rds")
Index <- data.frame(Party=c("PT", "PSDB", "PMDB", "PP"), index=1:4)
data <- merge(data, Index, by="Party")

pdf("BPFCH_PartyCohesion.pdf", width=8, height=6)
par(las=1)
barplot(data$Cohesion[order(data$index)], ylab="Party Cohesion Score", names.arg=data$Party[order(data$index)], ylim=c(0, 1))
segments(2.5, 0, 2.5, 1)
dev.off()



### Figure I4: Determinants of Legislative Voting in the Chamber of Deputies

data <- readRDS("PartyData.rds")
Index <- data.frame(Party=c("PT", "PSDB", "PPS", "PMDB", "PP", "PTB"), index=1:6)
data <- merge(data, Index, by="Party")
Party.Label <- c("PT", "PSDB", "PPS & PCdoB", "PMDB", "PP", "PTB & PSB")

pdf("BPFCH_PartyInterests.pdf", width=8, height=6)
par(las=1)
barplot(rbind(data$VoteWithParty[order(data$index)]*100, data$PartyInterest[order(data$index)]*100), ylab="Percent of Legislators", names.arg=Party.Label,  args.legend=list(bty="n"), beside=T, legend.text = c("Party Interests over Personal Views", "Party Interests over Local Interests"))
segments(9.5, 0, 9.5, 80)
dev.off()



### Table J1: The Effect of Municipal Control on New Affiliations for Modified Pooled Samples

data <- readRDS("MembersRD.rds")

# Run the RD model on each sub-sample, and save the coefficients, confidence interval, p-value, bandwidth, and sample size
# Exclude observations whose party is of the same type as its principal opponent
columns <- c(1, 4:5, 3, 6:7)
results <- round(rbind(RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Programmatic" & !data$OpponentSameType)[, columns],
                       RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Clientelistic"  & !data$OpponentSameType)[, columns],
                       RD.estimates("NewMembers05", "Margin", data, data$Type == "Decentralized"  & !data$OpponentSameType)[, columns]), 3)
rownames(results) <- c("Centralized-Programmatic", "Centralized-Clientelistic", "Decentralized")

results



### Figure K1: Pooled Sample RD Results, 2001 to 2013 ###

data <- readRDS("MembersRDextended.rds")

Pooled <- rbind(RD.estimates("Y", "Margin", data[data$Type %in% "Centralized-Programmatic",]),
                RD.estimates("Y", "Margin", data[data$Type %in% "Centralized-Clientelistic",]),
                RD.estimates("Y", "Margin", data[data$Type %in% "Decentralized",]))

pdf("BPFCH_LongWindow.pdf", width=8, height=5)
par(las=1)
places <- 1:3
plot(NA, xlim=c(.5, 3.5), ylim=c(-.25, .5), ylab="Treatment Effect", xlab="", xaxt="n", main="", cex.main=1.5, cex.axis=1.0, cex.lab=1.0)
abline(h=0, lty=3)
segments(places, Pooled[,4], places, Pooled[,5], lwd=5  )
point.col <- c("black")
point.type <- c(21)
points(places, Pooled[,1],  pch=point.type, cex=2, bg=point.col)
axis(1, at=1:3, line=0, labels=c("Centralized-Programmatic", "Centralized-Clientelistic", "Decentralized"), tick=F, cex.axis=1)
dev.off()



### Figure K2: Party Sample RD Results, 2001 to 2013 ###

data <- readRDS("MembersRDextended.rds")

Party <- rbind(RD.estimates("Y", "Margin", data[data$Party == "PT",]),
               RD.estimates("Y", "Margin", data[data$Party == "PSDB",]),
               RD.estimates("Y", "Margin", data[data$Party == "PPS",]),
               RD.estimates("Y", "Margin", data[data$Party == "PFL",]),
               RD.estimates("Y", "Margin", data[data$Party == "PL",]),
               RD.estimates("Y", "Margin", data[data$Party == "PDT",]),
               RD.estimates("Y", "Margin", data[data$Party == "PMDB",]),
               RD.estimates("Y", "Margin", data[data$Party == "PP",]),
               RD.estimates("Y", "Margin", data[data$Party == "PTB",]),
               RD.estimates("Y", "Margin", data[data$Party == "PSB",]))

pdf("BPFCH_LongWindowParty.pdf", width=10, height=5)
par(las=1)
places <- 1:10
plot(NA, xlim=c(.75, 10.25), ylim=c(-.5, .75), ylab="Treatment Effect", xlab="", xaxt="n", main="", cex.main=1.5, cex.axis=1.0, cex.lab=1.0)
abline(h=0, lty=3)
segments(places, Party[,4], places, Party[,5], lwd=5  )
point.col <- c("black")
point.type <- c(21)
points(places, Party[,1],  pch=point.type, cex=2, bg=point.col)
axis(1, at=1:10, line=0, labels=c("PT", "PSDB", "PPS", "PFL", "PL", "PDT", "PMDB", "PP", "PTB", "PSB"), tick=F, cex.axis=1)
abline(v=c(3.5, 6.5))
mtext(3, at=c(2, 5, 8.5), text=c("Centralized-Programmatic", "Centralized-Clientelistic", "Decentralized"),  cex=1.2, line=.5)
dev.off()



### Figure K3: Party Sample RD Results, By Mayoral Term ###

data <- readRDS("MembersRDextended.rds")

PT <- rbind(RD.estimates("Y", "Margin", data[data$Party == "PT" & data$ElectionYear == 2000,]),
            RD.estimates("Y", "Margin", data[data$Party == "PT" & data$ElectionYear == 2004,]),
            RD.estimates("Y", "Margin", data[data$Party == "PT" & data$ElectionYear == 2008,]))

PSDB <- rbind(RD.estimates("Y", "Margin", data[data$Party == "PSDB" & data$ElectionYear == 2000,]),
              RD.estimates("Y", "Margin", data[data$Party == "PSDB" & data$ElectionYear == 2004,]),
              RD.estimates("Y", "Margin", data[data$Party == "PSDB" & data$ElectionYear == 2008,]))

PFL <- rbind(RD.estimates("Y", "Margin", data[data$Party == "PFL" & data$ElectionYear == 2000,]),
             RD.estimates("Y", "Margin", data[data$Party == "PFL" & data$ElectionYear == 2004,]),
             RD.estimates("Y", "Margin", data[data$Party == "PFL" & data$ElectionYear == 2008,]))

PMDB <- rbind(RD.estimates("Y", "Margin", data[data$Party == "PMDB" & data$ElectionYear == 2000,]),
              RD.estimates("Y", "Margin", data[data$Party == "PMDB" & data$ElectionYear == 2004,]),
              RD.estimates("Y", "Margin", data[data$Party == "PMDB" & data$ElectionYear == 2008,]))

All <- rbind(PT, PSDB, PFL, PMDB)

pdf("BPFCH_GeneralizabilityParty.pdf", width=10, height=5)
par(las=1)
places <- 1:12
plot(NA, xlim=c(.75, 12.25), ylim=c(-.75, 1), ylab="Treatment Effect", xlab="", xaxt="n", main="", cex.main=1.5, cex.axis=1.0, cex.lab=1.0)
abline(h=0, lty=3)
segments(places, All[,4], places, All[,5], lwd=5  )
point.col <- c("black")
point.type <- c(21)
points(places, All[,1],  pch=point.type, cex=2, bg=point.col)
axis(1, at=1:12, line=0, labels=rep(c("2001-\n2005", "2005-\n2009", "2009-\n2013"), 4), tick=F, cex.axis=1)
abline(v=c(3.5, 6.5, 9.5))
mtext(3, at=c(2, 5, 8, 11), text=c("PT", "PSDB", "PFL/DEM", "PMDB"),  cex=1.2, line=.5)
dev.off()




### Figure L1: Other Heterogeneous Treatment Effects ###

data <- readRDS("MembersRD.rds")

pdf("BPFCH_HeterogeneousEffects.pdf", width=15, height=10)
par(mfrow=c(2,2), las=1)

# Population
small <- rbind(
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Programmatic") & data$Population < 10000,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Clientelistic") & data$Population < 10000,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Decentralized") & data$Population < 10000,]))

big <- rbind(
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Programmatic") & data$Population > 10000,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Clientelistic") & data$Population > 10000,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Decentralized") & data$Population > 10000,]))

Diff <- cbind(small[,1] - big[,1], small[,1] - big[,1] - 1.96*sqrt(small[,2]^2 + big[,2]^2), small[,1] - big[,1] + 1.96*sqrt(small[,2]^2 + big[,2]^2))
sep <- .12
places <- c(1-sep, 1, 1+sep, 2-sep, 2, 2+sep, 3-sep, 3, 3+sep)
plot(NA, xlim=c(.5, 3.5), ylim=c(-.5, 1), ylab="Treatment Effect", xlab="", xaxt="n", cex.main=1.5, cex.axis=1.0, cex.lab=1.0, main="By Population Size")
abline(h=0, lty=3)
segments(places, c(small[,4], big[,4], Diff[,2]), places, c(small[,5], big[,5], Diff[,3]), lwd=5  )
point.col <- c("black")
point.type <- c(21, 24, 22)
points(places, c(small[,1], big[,1], Diff[,1]),  pch=point.type, cex=2, bg=point.col)
axis(1, at=1:3, labels=c("Small\nMunicialities", "Large\nMunicipalities", "Difference\n"), tick=F, cex.axis=1)
legend("topright", c("Centralized-Programmatic", "Centralized-Clientelistic", "Decentralized"), pch=point.type, pt.bg=point.col, pt.cex=1.8, cex=1.2, bty="n")


# Income
low <- rbind(
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Programmatic") & data$GDP.PerCapita < 3.73745,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Clientelistic") & data$GDP.PerCapita < 3.73745,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Decentralized") & data$GDP.PerCapita < 3.73745,]))

high <- rbind(
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Programmatic") & data$GDP.PerCapita > 3.73745,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Clientelistic") & data$GDP.PerCapita > 3.737450,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Decentralized") & data$GDP.PerCapita > 3.73745,]))

Diff <- cbind(low[,1] - high[,1], low[,1] - high[,1] - 1.96*sqrt(low[,2]^2 + high[,2]^2), low[,1] - high[,1] + 1.96*sqrt(low[,2]^2 + high[,2]^2))
sep <- .12
places <- c(1-sep, 1, 1+sep, 2-sep, 2, 2+sep, 3-sep, 3, 3+sep)

plot(NA, xlim=c(.5, 3.5), ylim=c(-.75, 1), ylab="Treatment Effect", xlab="", xaxt="n", cex.main=1.5, cex.axis=1.0, cex.lab=1.0, main="By Income Level")
abline(h=0, lty=3)
segments(places, c(low[,4], high[,4], Diff[,2]), places, c(low[,5], high[,5], Diff[,3]), lwd=5  )
point.col <- c("black")
point.type <- c(21, 24, 22)
points(places, c(low[,1], high[,1], Diff[,1]),  pch=point.type, cex=2, bg=point.col)
axis(1, at=1:3, labels=c("Low-Income\nMunicipalities", "High-Income\nMunicipalities", "Difference\n"), tick=F, cex.axis=1)
legend("topright", c("Centralized-Programmatic", "Centralized-Clientelistic", "Decentralized"), pch=point.type, pt.bg=point.col, pt.cex=1.8, cex=1.2, bty="n")


# Region

SSE <- rbind(
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Programmatic") & (data$SE == 1 | data$S == 1),]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Clientelistic") & (data$SE == 1 | data$S == 1),]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Decentralized") & (data$SE == 1 | data$S == 1),]))

NNE <- rbind(
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Programmatic") & (data$NE == 1 | data$N == 1),]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Clientelistic") & (data$NE == 1 | data$N == 1),]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Decentralized") & (data$NE == 1 | data$N == 1),]))

Diff <- cbind(SSE[,1] - NNE[,1], SSE[,1] - NNE[,1] - 1.96*sqrt(SSE[,2]^2 + NNE[,2]^2), SSE[,1] - NNE[,1] + 1.96*sqrt(SSE[,2]^2 + NNE[,2]^2))
sep <- .12
places <- c(1-sep, 1, 1+sep, 2-sep, 2, 2+sep, 3-sep, 3, 3+sep)

plot(NA, xlim=c(.5, 3.5), ylim=c(-.75, 1), ylab="Treatment Effect", xlab="", xaxt="n",  cex.main=1.5, cex.axis=1.0, cex.lab=1.0, main="By Region")
abline(h=0, lty=3)
segments(places, c(SSE[,4], NNE[,4], Diff[,2]), places, c(SSE[,5], NNE[,5], Diff[,3]), lwd=5  )
point.col <- c("black")
point.type <- c(21, 24, 22)
points(places, c(SSE[,1], NNE[,1], Diff[,1]),  pch=point.type, cex=2, bg=point.col)
axis(1, at=1:3, labels=c("South and\nSoutheast", "North and \nNortheast", "Difference\n"), tick=F, cex.axis=1)
legend("topright", c("Centralized-Programmatic", "Centralized-Clientelistic", "Decentralized"), pch=point.type, pt.bg=point.col, pt.cex=1.8, cex=1.2, bty="n")

# Bolsa Familia
low <- rbind(
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Programmatic") & data$BolsaReceipients < 0.04884929,]),
  RD.estimates("NewMembers05", "Margin", data[data$Party %in% c("PT") & data$BolsaReceipients < 0.04884929,]),
  RD.estimates("NewMembers05", "Margin", data[data$Party %in% c("PSDB") & data$BolsaReceipients < 0.04884929,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Clientelistic") & data$BolsaReceipients < 0.04884929,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Decentralized") & data$BolsaReceipients < 0.04884929,]))

high <- rbind(
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Programmatic") & data$BolsaReceipients > 0.04884929,]),
  RD.estimates("NewMembers05", "Margin", data[data$Party %in% c("PT") & data$BolsaReceipients > 0.04884929,]),
  RD.estimates("NewMembers05", "Margin", data[data$Party %in% c("PSDB") & data$BolsaReceipients > 0.04884929,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Clientelistic") & data$BolsaReceipients > 0.04884929,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Decentralized") & data$BolsaReceipients > 0.04884929,]))

Diff <- cbind(low[,1] - high[,1], low[,1] - high[,1] - 1.96*sqrt(low[,2]^2 + high[,2]^2), low[,1] - high[,1] + 1.96*sqrt(low[,2]^2 + high[,2]^2))
sep <- .12
places <- c(1-2*sep, 1-1*sep, 1, 1+sep, 1+2*sep, 2-2*sep, 2-1*sep, 2, 2+sep, 2+2*sep, 3-2*sep, 3-1*sep, 3, 3+sep, 3+2*sep)

plot(NA, xlim=c(.5, 3.5), ylim=c(-1, 2), ylab="Treatment Effect", xlab="", xaxt="n", cex.main=1.5, cex.axis=1.0, cex.lab=1.0, main="By Scope of Bolsa Família")
abline(h=0, lty=3)
segments(places, c(low[,4], high[,4], Diff[,2]), places, c(low[,5], high[,5], Diff[,3]), lwd=5  )
point.col <- c("gray50", "black", "white", "gray50", "gray50")
point.type <- c(21, 21, 21, 24, 22)
points(places, c(low[,1], high[,1], Diff[,1]),  pch=point.type, cex=2, bg=point.col)
axis(1, at=1:3, labels=c("Low Share of\nBolsa Família Recipients", "High Share of\nBolsa Família Recipients", "Difference\n"), tick=F, cex.axis=1)
legend("topright", c("Centralized-Programmatic", "PT", "PSDB", "Centralized-Clientelistic", "Decentralized"), pch=point.type, pt.bg=point.col, pt.cex=1.8, cex=1.2, bty="n")
par(mfrow=c(1,1))
dev.off()



### Figure M1: RD Results for the PT, PSDB, and PPS Samples ###

data <- readRDS("MembersRD.rds")

CP <- rbind(RD.estimates("NewMembers05", "Margin", data[data$Party %in% c("PT"),]),
            RD.estimates("NewMembers05", "Margin", data[data$Party %in% c("PSDB"),]),
            RD.estimates("NewMembers05", "Margin", data[data$Party %in% c("PPS"),]))

pdf("BPFCH_WithPPS.pdf", width=8, height=5)
par(las=1)
places <- 1:3
plot(NA, xlim=c(.5, 3.5), ylim=c(-.5, 1), ylab="Treatment Effect", xlab="", xaxt="n", main="", cex.main=1.5, cex.axis=1.0, cex.lab=1.0)
abline(h=0, lty=3)
segments(places, CP[,4], places, CP[,5], lwd=5  )
point.col <- c("black")
point.type <- c(21)
points(places, CP[,1],  pch=point.type, cex=2, bg=point.col)
axis(1, at=1:3, line=1, labels=c("PT\n(Held the Presidency\nfrom 2003 to 2016)", "PSDB\n(Held the Presidency\nfrom 1995 to 2002)", "PPS\n(Never Held\nthe Presidency)"), tick=F, cex.axis=1)
dev.off()



### Figure M2: RD Results by Partisan Alignment with the Governor ###

data <- readRDS("MembersRD.rds")

unaligned <- rbind(
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Programmatic") & data$CopartisanGovernor02 == 0,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Clientelistic") & data$CopartisanGovernor02 == 0,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Decentralized") & data$CopartisanGovernor02 == 0,]))

aligned <- rbind(
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Programmatic") & data$CopartisanGovernor02 == 1,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Centralized-Clientelistic") & data$CopartisanGovernor02 == 1,]),
  RD.estimates("NewMembers05", "Margin", data[data$Type %in% c("Decentralized") & data$CopartisanGovernor02 == 1,]))

Diff <- cbind(unaligned[,1] - aligned[,1], unaligned[,1] - aligned[,1] - 1.96*sqrt(unaligned[,2]^2 + aligned[,2]^2), unaligned[,1] - aligned[,1] + 1.96*sqrt(unaligned[,2]^2 + aligned[,2]^2))
sep <- .12
places <- c(1-sep, 1, 1+sep, 2-sep, 2, 2+sep, 3-sep, 3, 3+sep)

pdf("BPFCH_Alignment.pdf", width=8, height=6)
par(las=1)
plot(NA, xlim=c(.5, 3.5), ylim=c(-.75, 1.25), ylab="Treatment Effect", xlab="", xaxt="n", main="", cex.main=1.5, cex.axis=1.0, cex.lab=1.0)
abline(h=0, lty=3)
segments(places, c(unaligned[,4], aligned[,4], Diff[,2]), places, c(unaligned[,5], aligned[,5], Diff[,3]), lwd=5  )
point.col <- c("black")
point.type <- c(21, 24, 22)
points(places, c(unaligned[,1], aligned[,1], Diff[,1]),  pch=point.type, cex=2, bg=point.col)
axis(1, at=1:3, labels=c("Mayor and Governor Are\nFrom Different Parties", "Mayor and Governor Are\nFrom The Same Party", "Difference\n"), tick=F, cex.axis=1)
legend("topright", c("Centralized-Programmatic", "Centralized-Clientelistic", "Decentralized"), pch=point.type, pt.bg=point.col, pt.cex=1.8, cex=1.2, bty="n")
dev.off()



### Figure N1: Daily PT and PSDB Affiliations, 2005-2009 ###

daily.affiliations <- readRDS("DailyAffiliations.rds")

pdf("BPFCH_AffiliationCycle.pdf", width=10, height=8)
par(mfrow=c(2,1), las=1)
text.size <- .75
par(mar=c(3,4.1,2,2.1))
plot(NA, xlim=c(0, 4.3), ylim=c(0, 12500), xaxs="i", yaxs="i", ylab="Affiliations", xlab="", main="PT")
title(xlab="Year of Term", line=1.7)
segments(daily.affiliations$t, 0, daily.affiliations$t, daily.affiliations$pt.affiliations)
arrow.angle <- 20
arrow.length <- .1
x <- 2.76
arrows(x, 9500, x, 7000, angle=arrow.angle, length=arrow.length)
text(x, 11000, "Municipal\nCandidate\nDeadline", cex=text.size)
x <- 3.88
arrows(x, 10500, x, 8000, angle=arrow.angle, length=arrow.length)
text(x, 11400, "Registration for\nInternal Elections", cex=text.size)
x <- 1.9
arrows(x, 10500, x, 3000, angle=arrow.angle, length=arrow.length)
text(x, 11400, "Registration for\nInternal Elections", cex=text.size)
x <- 0.76
arrows(x, 10000, x, 5000, angle=arrow.angle, length=arrow.length)
text(x, 11200, "National/State\nCandidate\nDeadline", cex=text.size)

plot(NA, xlim=c(0, 4.3), ylim=c(0, 17000), xaxs="i", yaxs="i", xlab="", ylab="Affiliations", main="PSDB")
title(xlab="Year of Term", line=1.7)
segments(daily.affiliations$t, 0, daily.affiliations$t, daily.affiliations$psdb.affiliations)
arrow.angle <- 20
arrow.length <- .1
x <- 2.76
arrows(x, 13500, x, 11500, angle=arrow.angle, length=arrow.length)
text(x, 15000, "Municipal\nCandidate\nDeadline", cex=text.size)
x <- 0.76
arrows(x, 14000, x, 5000, angle=arrow.angle, length=arrow.length)
text(x, 15000, "National/State\nCandidate\nDeadline", cex=text.size)
x <- 2.12
arrows(x, 14000, x, 12000, angle=arrow.angle, length=arrow.length)
text(x, 15400, "Registration for\nMunicipal Conventions", cex=text.size)
x <- 0.22
arrows(x, 13500, x, 5000, angle=arrow.angle, length=arrow.length)
text(x, 15000, "Registration\nfor Municipal\nConventions", cex=text.size)
x <- 1.22
arrows(x, 11000, x, 5000, angle=arrow.angle, length=arrow.length)
text(x, 12500, "Candidate-Selection\nConventions", cex=text.size)
x <- 3.22
arrows(x, 11000, x, 5000, angle=arrow.angle, length=arrow.length)
text(x, 12500, "Candidate-Selection\nConventions", cex=text.size)
x <- 3.9
dev.off()



### Figure P1: Affiliation Dates of 2008 PSDB Municipal Candidates ###

data <- readRDS("PSDBcandidates.rds")
y <- as.numeric(100 * table(data$YearMonth) / sum(table(data$YearMonth)))

pdf("BPFCH_CandidateAffiliations.pdf", width=8, height=5)
par(las=1)
barplot(y, space=0, xaxt="n", xlim=c(0, 96), ylab="% of Candidates", xlab="Year")
axis(1, c(0, 24, 48, 72, 96), 2005:2009)
dev.off()



### Figure Q1: Cumulative Affiliations of 2008 PSDB Municipal Candidates ###

daily.affiliations <- readRDS("DailyAffiliations.rds")

pdf("BPFCH_TreatmentProcrastination.pdf", width=6, height=5)
par(las=1)
plot(daily.affiliations$t, daily.affiliations$affiliations.control, xlim=c(2.7, 2.79), ylim=c(0, 1), type="l", lty=1, col="red", lwd=1.5, xaxt="n", xlab="Weeks to Deadline", ylab="Cumulative Density")
lines(daily.affiliations$t, daily.affiliations$affiliations.treatment, col="blue", lwd=1.5)
places <- 2 + c((276-21)/365, (276-14)/365, (276-7)/365, 276/365, (276+7)/365)
axis(1, places, 3:-1)
legend("topright", c("Treatment", "Control"), lwd=2, col=c("blue", "red"), cex=.9, bty="n")
dev.off()



