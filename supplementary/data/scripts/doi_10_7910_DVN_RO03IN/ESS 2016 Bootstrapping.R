## Bootstrap Code to Calculate Confidence Intervals - ESS 2016 ##

#Load packages
library(beepr)
library(matrixStats)
library(gridExtra)
library(dplyr)
library(ggplot2)

#Number of Iterations
R <- 400 # should be 400, but not  when you're testing ...

#Remove NAs (Russia and Iceland)
#Region Variable
table(ESS2016sd$cntry)
ESS2016sd$region <- ESS2016sd$cntry
ESS2016sd$region[ESS2016sd$region=="RU"] <- NA
ESS2016sd$region[ESS2016sd$region=="IS"] <- NA
ESS2016sd$region[ESS2016sd$region=="AT"] <- "West"
ESS2016sd$region[ESS2016sd$region=="CY"] <- "West"
ESS2016sd$region[ESS2016sd$region=="BE"] <- "West"
ESS2016sd$region[ESS2016sd$region=="CH"] <- "West"
ESS2016sd$region[ESS2016sd$region=="CZ"] <- "East"
ESS2016sd$region[ESS2016sd$region=="DE"] <- "West"
ESS2016sd$region[ESS2016sd$region=="EE"] <- "East"
ESS2016sd$region[ESS2016sd$region=="ES"] <- "West"
ESS2016sd$region[ESS2016sd$region=="FI"] <- "West"
ESS2016sd$region[ESS2016sd$region=="FR"] <- "West"
ESS2016sd$region[ESS2016sd$region=="GB"] <- "West"
ESS2016sd$region[ESS2016sd$region=="HU"] <- "East"
ESS2016sd$region[ESS2016sd$region=="IE"] <- "West"
ESS2016sd$region[ESS2016sd$region=="IL"] <- "West"
ESS2016sd$region[ESS2016sd$region=="IT"] <- "West"
ESS2016sd$region[ESS2016sd$region=="LT"] <- "East"
ESS2016sd$region[ESS2016sd$region=="NL"] <- "West"
ESS2016sd$region[ESS2016sd$region=="NO"] <- "West"
ESS2016sd$region[ESS2016sd$region=="PL"] <- "East"
ESS2016sd$region[ESS2016sd$region=="PT"] <- "West"
ESS2016sd$region[ESS2016sd$region=="SE"] <- "West"
ESS2016sd$region[ESS2016sd$region=="SI"] <- "East"
table(ESS2016sd$region)

attach(ESS2016sd)
completeFun <- function(data, cntry) {
completeVec <- complete.cases(data[, cntry])
return(data[completeVec, ])
}

ESS2016sd <- completeFun(ESS2016sd, "cntry")
detach(ESS2016sd)

# containers to save output
nCountries <- length(unique(ESS2016sd$cntry)) #I continue to use the newly created dataframe now (ESS2016sd)
nCountries
economics2016 <- women2016 <- homs2016 <- immigration2016 <- matrix(nrow = nCountries, ncol = R)
colnames(economics2016) <- 1:R
colnames(homs2016) <- 1:R
colnames(women2016) <- 1:R
colnames(immigration2016) <- 1:R
reg2016 <- unique(subset(ESS2016sd, select = c(cntry, region)))   # because output of coef(m) seems to be alphabetical
rownames(economics2016) <- reg2016$cntry
rownames(women2016) <- reg2016$cntry
rownames(homs2016) <- reg2016$cntry
rownames(immigration2016) <- reg2016$cntry

#Bootstrapping (this takes a lot of time! For me it takes up to a day or two)
for (i in 1:R) {
    
    print(i)
    bsSample <- ESS2016sd[sample(1:nrow(ESS2016sd), nrow(ESS2016sd), replace = TRUE), ]

    m <- modelX2016 <- lmer(lrscale ~ EconomicInequality + AntiFeministAttitude + 
                              LessImmigration + IntOfHomosexuality + 
                              age + EducationLevel + hinctnta + 
                              female + rural + religiosity + union + ethnicMinority + ProEU + 
                              ReligDen + TraditionImportant + RuleFollowImportant +
                              SafetyImportant + OpenToExperience +
                              (1 + EconomicInequality + AntiFeministAttitude + 
                                 LessImmigration + 
                                 IntOfHomosexuality + ProEU | cntry), data=bsSample)
    b <- ranef(m)$cntry
    c <- fixef(m)
    economics2016[, i] <- b$EconomicInequality + c["EconomicInequality"]
    women2016[, i] <- b$AntiFeministAttitude + c["AntiFeministAttitude"]
    immigration2016[, i] <- b$LessImmigration + c["LessImmigration"]
    homs2016[, i] <- b$IntOfHomosexuality + c["IntOfHomosexuality"]
    
}

beep(sound = "shotgun")

#Data frame
economics2016b <- economics2016
economics2016 <- as.data.frame(economics2016)
economics2016$cntry <- rownames(economics2016)
economics2016 <- economics2016[c("CZ", "EE", "HU", "LT", "PL", "SI", "AT", "BE",
            "CH", "DE", "ES",  "FI", "FR", "GB", "IE",
            "IT", "NL", "NO", "PT", "SE"),]
economics2016$id <- c(1:nCountries)
economics2016$cntry <- factor(economics2016$cntry, levels = economics2016$cntry[order(economics2016$id)])
women2016b <- women2016
women2016 <- as.data.frame(women2016)
women2016$cntry <- rownames(women2016)
women2016 <- women2016[c("CZ", "EE", "HU", "LT", "PL", "SI", "AT", "BE",
                         "CH", "DE", "ES",  "FI", "FR", "GB", "IE", 
                         "IT", "NL", "NO", "PT", "SE"),]
women2016$id <- c(1:nCountries)
women2016$cntry <- factor(women2016$cntry, levels = women2016$cntry[order(women2016$id)])
immigration2016b <- immigration2016
immigration2016 <- as.data.frame(immigration2016)
immigration2016$cntry <- rownames(immigration2016)
immigration2016 <- immigration2016[c("CZ", "EE", "HU", "LT", "PL", "SI", "AT", "BE",
                         "CH", "DE", "ES",  "FI", "FR", "GB", "IE", 
                         "IT", "NL", "NO", "PT", "SE"),]
immigration2016$id <- c(1:nCountries)
immigration2016$cntry <- factor(immigration2016$cntry, levels = immigration2016$cntry[order(immigration2016$id)])
homs20166b <- homs2016
homs2016 <- as.data.frame(homs2016)
homs2016$cntry <- rownames(homs2016)
homs2016 <- homs2016[c("CZ", "EE", "HU", "LT", "PL", "SI", "AT", "BE",
                         "CH", "DE", "ES",  "FI", "FR", "GB", "IE", 
                         "IT", "NL", "NO", "PT", "SE"),]
homs2016$id <- c(1:nCountries)
homs2016$cntry <- factor(homs2016$cntry, levels = homs2016$cntry[order(homs2016$id)])

#Calculate the real coefficients
economicsCoef <- womenCoef <- homsCoef <- immigrationCoef <- matrix(nrow = nCountries, ncol = 1)

m <- modelX2016 <- lmer(lrscale ~ EconomicInequality + AntiFeministAttitude + 
                          LessImmigration + IntOfHomosexuality + 
                          age + EducationLevel + hinctnta + 
                          female + rural + religiosity + union + ethnicMinority + ProEU + 
                          ReligDen + TraditionImportant + RuleFollowImportant +
                          SafetyImportant + OpenToExperience +
                          (1 + EconomicInequality + AntiFeministAttitude + 
                             LessImmigration + 
                             IntOfHomosexuality + ProEU | cntry), data=ESS2016sd)
b <- ranef(m)$cntry
c <- fixef(m)
economicsCoef[] <- b$EconomicInequality + c["EconomicInequality"]
womenCoef[] <- b$AntiFeministAttitude + c["AntiFeministAttitude"]
immigrationCoef[] <- b$LessImmigration + c["LessImmigration"]
homsCoef[] <- b$IntOfHomosexuality + c["IntOfHomosexuality"]
beep()

#Data frame for coefficients
economicsCoef <- as.data.frame(economicsCoef)
economicsCoef$cntry <- reg2016$cntry
rownames(economicsCoef) <- reg2016$cntry
economicsCoef <- economicsCoef[c("CZ", "EE", "HU", "LT", "PL", "SI", "AT", "BE",
                         "CH", "DE", "ES",  "FI", "FR", "GB", "IE", 
                         "IT", "NL", "NO", "PT", "SE"),]
economicsCoef$id <- c(1:nCountries)
economicsCoef$cntry <- factor(economicsCoef$cntry, levels = economicsCoef$cntry[order(economicsCoef$id)])
womenCoef <- as.data.frame(womenCoef)
womenCoef$cntry <- reg2016$cntry
rownames(womenCoef) <- reg2016$cntry
womenCoef <- womenCoef[c("CZ", "EE", "HU", "LT", "PL", "SI", "AT", "BE",
                 "CH", "DE", "ES",  "FI", "FR", "GB", "IE", 
                 "IT", "NL", "NO", "PT", "SE"),]
womenCoef$id <- c(1:nCountries)
womenCoef$cntry <- factor(womenCoef$cntry, levels = womenCoef$cntry[order(womenCoef$id)])
immigrationCoef <- as.data.frame(immigrationCoef)
immigrationCoef$cntry <- reg2016$cntry
rownames(immigrationCoef) <- reg2016$cntry
immigrationCoef <- immigrationCoef[c("CZ", "EE", "HU", "LT", "PL", "SI", "AT", "BE",
                             "CH", "DE", "ES",  "FI", "FR", "GB", "IE",
                             "IT", "NL", "NO", "PT", "SE"),]
immigrationCoef$id <- c(1:nCountries)
immigrationCoef$cntry <- factor(immigrationCoef$cntry, levels = immigrationCoef$cntry[order(immigrationCoef$id)])
homsCoef <- as.data.frame(homsCoef)
homsCoef$cntry <- reg2016$cntry
rownames(homsCoef) <- reg2016$cntry
homsCoef <- homsCoef[c("CZ", "EE", "HU", "LT", "PL", "SI", "AT", "BE",
               "CH", "DE", "ES",  "FI", "FR", "GB", "IE", 
               "IT", "NL", "NO", "PT", "SE"),]
homsCoef$id <- c(1:nCountries)
homsCoef$cntry <- factor(homsCoef$cntry, levels = homsCoef$cntry[order(homsCoef$id)])

#Integrating the coefficients into the new dataframe
economics2016$RealCoef <- economicsCoef$V1
women2016$RealCoef <- womenCoef$V1
immigration2016$RealCoef <- immigrationCoef$V1
homs2016$RealCoef <- homsCoef$V1

#Integrating Region Value
OrderDF <- reg2016
rownames(OrderDF) <- OrderDF$cntry
OrderDF <- OrderDF[c("CZ", "EE", "HU", "LT", "PL", "SI", "AT", "BE",
                                 "CH", "DE", "ES",  "FI", "FR", "GB", "IE", 
                                 "IT", "NL", "NO", "PT", "SE"),]
OrderDF$region[OrderDF$region==0] <- "West"
OrderDF$region[OrderDF$region==1] <- "East"
economics2016$Region <- OrderDF$region
women2016$Region <- OrderDF$region
immigration2016$Region <- OrderDF$region
homs2016$Region <- OrderDF$region

#Calculate Confidence Intervals
economics2016$Low <- rowQuantiles(as.matrix(economics2016[,1:R]), probs = 0.025)
economics2016$High <- rowQuantiles(as.matrix(economics2016[,1:R]), probs = 0.975)
women2016$Low <- rowQuantiles(as.matrix(women2016[,1:R]), probs = 0.025)
women2016$High <- rowQuantiles(as.matrix(women2016[,1:R]), probs = 0.975)
immigration2016$Low <- rowQuantiles(as.matrix(immigration2016[,1:R]), probs = 0.025)
immigration2016$High <- rowQuantiles(as.matrix(immigration2016[,1:R]), probs = 0.975)
homs2016$Low <- rowQuantiles(as.matrix(homs2016[,1:R]), probs = 0.025)
homs2016$High <- rowQuantiles(as.matrix(homs2016[,1:R]), probs = 0.975)
economics2016$spread <- abs(economics2016$Low - economics2016$High)
women2016$spread <- abs(women2016$Low - women2016$High)
immigration2016$spread <- abs(immigration2016$Low - immigration2016$High)
homs2016$spread <- abs(homs2016$Low - homs2016$High)
economics2016$confLo <- economics2016$RealCoef - (economics2016$spread/2)
economics2016$confHi <- economics2016$RealCoef + (economics2016$spread/2)
women2016$confLo <- women2016$RealCoef - (women2016$spread/2)
women2016$confHi <- women2016$RealCoef + (women2016$spread/2)
immigration2016$confLo <- immigration2016$RealCoef - (immigration2016$spread/2)
immigration2016$confHi <- immigration2016$RealCoef + (immigration2016$spread/2)
homs2016$confLo <- homs2016$RealCoef - (homs2016$spread/2)
homs2016$confHi <- homs2016$RealCoef + (homs2016$spread/2)

#Reorder Coefficients Based on Size
economics2016$cntry <- factor(economics2016$cntry, 
                              levels = economics2016$cntry[order(economics2016$RealCoef)])
women2016$cntry <- factor(women2016$cntry, 
                              levels = women2016$cntry[order(women2016$RealCoef)])
immigration2016$cntry <- factor(immigration2016$cntry, 
                              levels = immigration2016$cntry[order(immigration2016$RealCoef)])
homs2016$cntry <- factor(homs2016$cntry, 
                              levels = homs2016$cntry[order(homs2016$RealCoef)])


# FIGURE 2
p12016 <- ggplot(economics2016, aes(x = RealCoef, y = cntry)) +
  theme_minimal() + xlim(-0.5, 1.1) +
  geom_hline(aes(yintercept = 0), linetype = 3, color = "gray30") +
  theme(legend.position = "none") + 
  geom_errorbarh(aes(y = cntry, xmin = confLo, xmax = confHi, height = 0)) +
  geom_point(aes(shape=Region)) +
  scale_shape_manual(values=c(15, 17)) +
  xlab("Economic Inequality") + 
  ylab("") + scale_y_discrete(limits = rev(levels(economics2016$cntry))) +
  geom_vline(xintercept = 0, color = "black", size=0.3)

p22016 <- ggplot(immigration2016, aes(x = RealCoef, y = cntry)) +
  theme_minimal() + xlim(-0.5, 1.1) +
  geom_hline(aes(yintercept = 0), linetype = 3, color = "gray30") +
  theme(legend.position = "none") + 
  geom_errorbarh(aes(y = cntry, xmin = confLo, xmax = confHi, height = 0)) +
  geom_point(aes(shape=Region)) +
  scale_shape_manual(values=c(15, 17)) +
  xlab("Less Immigration") + 
  ylab("") + scale_y_discrete(limits = rev(levels(immigration2016$cntry))) +
  geom_vline(xintercept = 0, color = "black", size=0.3)

p32016 <- ggplot(women2016, aes(x = RealCoef, y = cntry)) +
  theme_minimal() + xlim(-0.5, 1.1) +
  geom_hline(aes(yintercept = 0), linetype = 3, color = "gray30") +
  theme(legend.position = "none") + 
  geom_errorbarh(aes(y = cntry, xmin = confLo, xmax = confHi, height = 0)) +
  geom_point(aes(shape=Region)) +
  scale_shape_manual(values=c(15, 17)) +
  xlab("Anti-Feminist Attitude") + 
  ylab("") + scale_y_discrete(limits = rev(levels(women2016$cntry))) +
  geom_vline(xintercept = 0, color = "black", size=0.3)

p42016 <- ggplot(homs2016, aes(x = RealCoef, y = cntry)) +
  theme_minimal() + xlim(-0.5, 1.1) +
  geom_hline(aes(yintercept = 0), linetype = 3, color = "gray30") +
  theme(legend.position = c(0.88, 0.2), 
        legend.background = 
          element_rect(color = "black", 
                       fill = "white", size = 0.3, 
                       linetype = "solid")) + 
  geom_errorbarh(aes(y = cntry, xmin = confLo, xmax = confHi, height = 0)) +
  geom_point(aes(shape=Region)) +
  scale_shape_manual(values=c(15, 17)) +
  xlab("Intolerance of Homosexuality") + 
  ylab("") + scale_y_discrete(limits = rev(levels(homs2016$cntry))) +
  geom_vline(xintercept = 0, color = "black", size=0.3)

grid.arrange(p12016, p22016, p32016, p42016,
             widths = c(1, 1),
             layout_matrix = rbind(c(1, 2),
                                   c(3, 4))
)

#Save Generated Data
setwd("/")
write.csv(economics2016, "economics2016[DATE].csv")
write.csv(women2016, "women2016[DATE].csv")
write.csv(immigration2016, "immigration2016[DATE].csv")
write.csv(homs2016, "homs2016[DATE].csv")
