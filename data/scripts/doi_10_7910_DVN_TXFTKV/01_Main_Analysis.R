# Sells 2019 "Building Parties from City Hall"
# Replication Code for Figures and Tables in the Main Text

rm(list = ls())

library(rdrobust)
library(lfe)

# Note: the RD results presented in the main text of the article were based on
#       version 0.99.3 of the rdrobust package


# Set the working directory to the folder where the datasets are saved
setwd("")

### This function reformats the output of the rdrobust() function into a matrix ###
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





### Figure 1: The Brazilian Party System in the mid-2000s ###
# (Party Classifications)

Parties <- readRDS("PartyData.rds")
pdf("BPFCH_Figure01.pdf", width=5.5, height=5)
par(mar=c(3.1,3.1,2.1,2.1))
plot(Parties$Clientelism, Parties$Centralization, type="n", xlim=c(1.92, 3.5), ylab="", xlab="", xaxt="n", yaxt="n")
text(Parties$Clientelism, Parties$Centralization, Parties$Party)
title(ylab="Decentralized -- Centralized   ", xlab="Programmatic -- Clientelistic  ", line=0.2, cex.lab=1)
title(ylab="Centralization", xlab="Programmatism", line=1.6, cex.lab=1.2)
dev.off()



### Figure 2: The Effect of Municipal Control on New Affiliations (OLS Estimates) ###

# Run the difference-in-differences model for each party separately
data <- readRDS("MembersDD.rds")
parties <- c("PT", "PSDB", "PPS", "PC do B", "PFL", "PL", "PDT", "PMDB", "PP", "PTB", "PSB")
results <- c()
for(party in parties){
  sub <- data[data$Party == party,]                           # Subset by party
  mod <- felm(Y ~ Mayor | Mun + Election | 0 | Mun, data=sub) # Municipality and Term fixed effects, standard errors clustered on the Municipality
  estimate <- coef(mod)
  results <- rbind(results, c(estimate, confint(mod)))
}
rownames(results) <- parties
colnames(results) <- c("Estimate", "Lower", "Upper")

# Plot the point estimates and confidence intervals by party
pdf("BPFCH_Figure02.pdf", width=12, height=6)
wall <- c(4.5, 7.5)
len <- nrow(results)
par(las=1)
plot(1:len, results[,1], ylim=c(-.4, .4), pch=16, cex=2, xaxt="n", xlab="", ylab="Effect of Incumbency on Affiliations", cex.axis=1.2)
segments(1:len, results[,2], 1:len, results[,3], lwd=4)
axis(1, 1:len, parties, cex.axis=1.3)
abline(h=0, lty=3)
abline(v=wall)
mtext(3, at=c(2.5, 6, 9.5), text=c("Centralized-Programmatic", "Centralized-Clientelistic", "Decentralized"),  cex=1.5, line=.5)
dev.off()





### Table 1: The Effect of Municipal Control on New Affiliations (RD Estimates) ###

data <- readRDS("MembersRD.rds")

# Run the RD model on each sub-sample, and save the coefficients, confidence interval, p-value, bandwidth, and sample size
columns <- c(1, 4:5, 3, 6:7)
results <- round(rbind(RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Programmatic")[, columns],
            RD.estimates("NewMembers05", "Margin", data, data$Party == "PT")[, columns],
            RD.estimates("NewMembers05", "Margin", data, data$Party == "PSDB")[, columns],
            RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Clientelistic")[, columns],
            RD.estimates("NewMembers05", "Margin", data, data$Party == "PFL")[, columns],
            RD.estimates("NewMembers05", "Margin", data, data$Type == "Decentralized")[, columns],
            RD.estimates("NewMembers05", "Margin", data, data$Party == "PMDB")[, columns]), 3)
rownames(results) <- c("Centralized-Programmatic", "PT", "PSDB", "Centralized-Clientelistic", "PFL", "Decentralized", "PMDB")

results




### Figure 3: The Effect of Municipal Control on New Affiliations ###
# (RD Plot)

data <- readRDS("MembersRD.rds")

pdf("BPFCH_Figure03.pdf", width=10, height=8)
par(mfrow=c(2,2), mar=c(4.1,4.1,4.1,4.1), las=1)
rdplot(data$NewMembers05, data$Margin, y.lim=c(0, 1.2), subset=data$Party == "PT" & abs(data$Margin) < .1, title="PT", x.label="Margin of Victory", y.label="New Members as % of Electorate", p=3, col.lines="black")
rdplot(data$NewMembers05, data$Margin, y.lim=c(0, 1.2), subset=data$Party == "PSDB" & abs(data$Margin) < .1, title="PSDB", x.label="Margin of Victory", y.label="New Members as % of Electorate", p=3, col.lines="black")
rdplot(data$NewMembers05, data$Margin, y.lim=c(0, 1.2), subset=data$Party == "PFL" & abs(data$Margin) < .1, title="PFL", x.label="Margin of Victory", y.label="New Members as % of Electorate", p=3, col.lines="black")
rdplot(data$NewMembers05, data$Margin, y.lim=c(0, 1.2), subset=data$Party == "PMDB" & abs(data$Margin) < .1, title="PMDB", x.label="Margin of Victory", y.label="New Members as % of Electorate", p=3, col.lines="black")
par(mfrow=c(1,1))
dev.off()



### Figure 4: The Effect of Municipal Control, by Age of the Local Party (RD Estimates) ###

data <- readRDS("MembersRD.rds")
point.size <- 2.2
gap <- .15
places <- sort(c(1:3 - gap, 1:3, 1:3 + gap))
ylim <- c(-1, 2)
title.size <- 1
axis.size <- 1
label.gap <- 0
label.size <- 1
bar.width <- 5
point.type <- 21
point.col <- c("gray50", "black", "white")
Old <- rbind(RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Programmatic" & data$Office == 1),
                  RD.estimates("NewMembers05", "Margin", data, data$Party == "PT" & data$Office == 1),
                  RD.estimates("NewMembers05", "Margin", data, data$Party == "PSDB" & data$Office == 1))
New <- rbind(RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Programmatic" & data$Office == 0),
                     RD.estimates("NewMembers05", "Margin", data, data$Party == "PT" & data$Office == 0),
                     RD.estimates("NewMembers05", "Margin", data, data$Party == "PSDB" & data$Office == 0))
# Calculate differences and their standard errors:
Diff <- cbind(Old[,1] - New[,1], Old[,1] - New[,1] - 1.96*sqrt(Old[,2]^2 + New[,2]^2), Old[,1] - New[,1] + 1.96*sqrt(Old[,2]^2 + New[,2]^2))
Age <- rbind(Old[, c(1, 4, 5)], New[, c(1, 4, 5)], Diff)

pdf("BPFCH_Figure04.pdf", width=6.5, height=6)
par(las=1)
plot(NA, xlim=c(.5, 3.5), ylim=ylim, ylab="Effect of Incumbency on Affiliations", xlab="", xaxt="n", main="", cex.axis=axis.size)
abline(h=0, lty=3)
segments(places, Age[,2], places, Age[,3], lwd=5  )
points(places, Age[,1],  pch=point.type, cex=point.size, bg=point.col)
axis(1, at=1:3, labels=c("Established\nPresence", "New\nPresence", "Difference\n"), tick=F, cex.axis=1, line=label.gap)
legend("topright", c("Pooled", "PT", "PSDB"), pch=point.type, pt.bg = point.col, pt.cex = point.size, cex=1.2, bty="n")
dev.off()





### Figure 5: The Effect of Municipal Control on Affiliations, by Availability of Patronage (RD Estimates) ###

data <- readRDS("MembersRD.rds")
Low.pt <- RD.estimates("NewMembers05", "Margin", data[data$Party == "PT" & data$DiscretionaryPositions04 < .01,])
High.pt <- RD.estimates("NewMembers05", "Margin", data[data$Party == "PT" & data$DiscretionaryPositions04 > .01,])
Diff.pt <- cbind(High.pt[,1] - Low.pt[,1], High.pt[,1] - Low.pt[,1] - 1.96*sqrt(High.pt[,2]^2 + Low.pt[,2]^2), High.pt[,1] - Low.pt[,1] + 1.96*sqrt(High.pt[,2]^2 + Low.pt[,2]^2))
Low.psdb <- RD.estimates("NewMembers05", "Margin", data[data$Party == "PSDB" & data$DiscretionaryPositions04 < .01,])
High.psdb <- RD.estimates("NewMembers05", "Margin", data[data$Party == "PSDB" & data$DiscretionaryPositions04 > .01,])
Diff.psdb <- cbind(High.psdb[,1] - Low.psdb[,1], High.psdb[,1] - Low.psdb[,1] - 1.96*sqrt(High.psdb[,2]^2 + Low.psdb[,2]^2), High.psdb[,1] - Low.psdb[,1] + 1.96*sqrt(High.psdb[,2]^2 + Low.psdb[,2]^2))
Low <- RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Programmatic" & data$DiscretionaryPositions04 < .01)
High <- RD.estimates("NewMembers05", "Margin", data, data$Type == "Centralized-Programmatic" & data$DiscretionaryPositions04 > .01)
Diff <- cbind(High[,1] - Low[,1], High[,1] - Low[,1] - 1.96*sqrt(High[,2]^2 + Low[,2]^2), High[,1] - Low[,1] + 1.96*sqrt(High[,2]^2 + Low[,2]^2))

Patronage <- rbind(High[, c(1, 4, 5)], High.pt[, c(1, 4, 5)], High.psdb[, c(1, 4, 5)], Low[, c(1, 4, 5)], Low.pt[, c(1, 4, 5)], Low.psdb[, c(1, 4, 5)], Diff, Diff.pt, Diff.psdb)
sep <- .12
places <- c(1-sep, 1, 1+sep, 2-sep, 2, 2+sep, 3-sep, 3, 3+sep)

pdf("BPFCH_Figure05.pdf", width=6.5, height=6)
par(las=1)
plot(NA, xlim=c(.5, 3.5), ylim=c(-1, 2), ylab="Effect of Incumbency on Affiliations", xlab="", xaxt="n", main="", cex.main=1.5, cex.axis=1.0, cex.lab=1.0)
abline(h=0, lty=3)
segments(places, Patronage[,2], places, Patronage[,3], lwd=5  )
point.col <- c("gray50", "black", "white")
point.type <- c(21)
points(places, Patronage[,1],  pch=point.type, cex=2, bg=point.col)
axis(1, at=1:3, labels=c("Abundant Patronage\nResources", "Scarce Patronage\nResources", "Difference\n"), tick=F, cex.axis=1)
legend("topright", c("Pooled", "PT", "PSDB"), pch=point.type, pt.bg=point.col, pt.cex=2, cex=1.2, bty="n")
dev.off()



### Figure 6: Effect of Municipal Control on Duration of Membership (RD Estimates) ###
data <- readRDS("MembersRD.rds")

pdf("BPFCH_Figure06.pdf", width=10, height=5)
par(mfrow=c(1,2), mar=c(3.1,5.1,3.6,2.1), las=1)
layout(matrix(c(1,1,2), nrow = 1, ncol = 3, byrow = TRUE))
gap <- .15
places <- sort(c(1:3 - gap, 1:3, 1:3 + gap))
bar.width <- 5
point.type <- 21
point.col <- c("gray50", "black", "white")
Attrition <- rbind(RD.estimates("AttritionRate", "Margin", data[data$Party %in% c("PT", "PSDB"),]),
                   RD.estimates("AttritionRate", "Margin", data[data$Party == "PT",]),
                   RD.estimates("AttritionRate", "Margin", data[data$Party == "PSDB",]),
                   RD.estimates("SwitchShare", "Margin", data[data$Party %in% c("PT", "PSDB"),]),
                   RD.estimates("SwitchShare", "Margin", data[data$Party == "PT",]),
                   RD.estimates("SwitchShare", "Margin", data[data$Party == "PSDB",]),
                   RD.estimates("LeaveShare", "Margin", data[data$Party %in% c("PT", "PSDB"),]),
                   RD.estimates("LeaveShare", "Margin", data[data$Party == "PT",]),
                   RD.estimates("LeaveShare", "Margin", data[data$Party == "PSDB",]))
plot(NA, xlim=c(.5, 3.5), ylim=c(-.2, .2), ylab="Effect on Share of '05-'08 Affiliators", xlab="", xaxt="n", main="(A) Effect on Attrition By 2013", cex.main=1.6, cex.axis=1.2, cex.lab=1.8)
abline(h=0, lty=3)
segments(places, Attrition[,4], places, Attrition[,5], lwd=5  )
points(places, Attrition[,1],  pch=point.type, cex=2.5, bg=point.col)
axis(1, at=1:3, labels=c("All Attrition", "Party-Switching", "Disaffiliation"), tick=F, cex.axis=1.6)
legend("topright", c("Pooled", "PT", "PSDB"), pch=point.type, pt.bg = point.col, pt.cex = 2.2, cex=1.5, bty="n")

Long <- rbind(RD.estimates("Stayed", "Margin", data[data$Party %in% c("PT", "PSDB"),]),
              RD.estimates("Stayed", "Margin", data[data$Party == "PT",]),
              RD.estimates("Stayed", "Margin", data[data$Party == "PSDB",]))
gap <- .12
places <- c(1 - gap, 1, 1 + gap)
plot(NA, xlim=c(.5, 1.5), ylim=c(-.5, 1), ylab="Effect on New Members as % of Electorate", xlab="", xaxt="n", main="(B) Effect on Long-Term Members", cex.main=1.6, cex.axis=1.2, cex.lab=1.8)
abline(h=0, lty=3)
segments(places, Long[,4], places, Long[,5], lwd=5  )
points(places, Long[,1],  pch=point.type, cex=2.5, bg=point.col)
legend("topright", c("Pooled", "PT", "PSDB"), pch=point.type, pt.bg = point.col, pt.cex=2.2, cex=1.5, bty="n")
dev.off()






### Figure 7: PT Affiliations in September and October, 2007

data <- readRDS("DailyAffiliations.rds")
PT.affiliations <- data$pt.affiliations[data$t >= 2.664 & data$t <= 2.828]

pdf("BPFCH_Figure07.pdf", width=8, height=4)
par(mfrow=c(1,2), mar=c(3.1,4.3,3.6,1.6), las=1)

# Plot the observed number of affiliations by day
barplot(PT.affiliations, space=0, xaxt="n", ylim=c(0, 7200), xaxs="i", col="gray60", main="(A) Observed Affiliations", ylab="New Affiliations")
box()

abline(v=35, lwd=4)
text(35.25, 7000, "Deadline", adj=c(-.05, 1.2), cex=.9)
axis(1, c(0, 30, 61), c("9/1/07", "10/1/07", "11/1/07"))

# Randomly select some of the members to represent hypothetical PT prospective municipal candidates
politicians <- PT.affiliations[1:35]
set.seed(0)
offset <- rnorm(35, 160, 87.989)
offset[offset < 0] <- 0
other <- c(offset, PT.affiliations[36:length(PT.affiliations)])

# Plot the hypothetical member type distribution by day
barplot(politicians, space=0, xaxt="n", ylim=c(0, 7200), xaxs="i", col="white", xlim=c(0, 61), main="(B) Hypothetical Distribution by Type", ylab="New Affiliations")
barplot(other,  col="black", add=T, space=0, xaxt="n")
box()
abline(v=35, lwd=4)
text(35.25, 7000, "Deadline", adj=c(-.05, 1.2), cex=.9)
axis(1, c(0, 30, 61), c("9/1/07", "10/1/07", "11/1/07"))
text( 15, 5890, "Prospective\nCandidates", cex=.9)
arrows(18, 5400, 25, 4000, angle=15, length=.15)
text( 49, 1600, "Other Party\nJoiners", cex=.9)
arrows(43, 1500, 31, 300, angle=15, length=.15)
arrows(43.6, 1400, 40, 500, angle=15, length=.15)
par(mfrow=c(1,1))
dev.off()




### Figure 8: Effect of Municipal Control, by Member Type (RD Estimates) ###

data <- readRDS("MembersRD.rds")

pdf("BPFCH_Figure08.pdf", width=6, height=5)
par(mfrow=c(1,1), mar=c(3.1,5.1,3.6,2.1), las=1)
gap <- .1
places <- c(1-gap, 1+gap, 2)
bar.width <- 5
point.type <- c(19, 21)
point.col <- "white"
Type <- rbind(RD.estimates("ProspectiveCandidates", "Margin", data[data$Party == "PT",]),
              RD.estimates("ProspectiveCandidates", "Margin", data[data$Party == "PSDB",]),
              RD.estimates("InternalElections", "Margin", data[data$Party == "PT",]))
plot(NA, xlim=c(.5, 2.5), ylim=c(-.1, .3), ylab="Effect of Incumbency on Affiliations Near the Deadline", xlab="", xaxt="n", cex.axis=1, cex.lab=1)
abline(h=0, lty=3)
segments(places, Type[,4], places, Type[,5], lwd=5  )
points(places, Type[,1],  pch=point.type, cex=1.8, bg=point.col)
axis(1, at=1:2, labels=c("Prospective Candidates", "Voters in Internal Elections"), tick=F, cex.axis=1)
legend("topright", c("PT", "PSDB"), pch=point.type, cex=1.2, bty="n")
dev.off()
