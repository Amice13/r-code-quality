## Bootstrap Code to Calculate Confidence Intervals - ESS 2008 ##

#Load packages
library(beepr)
library(matrixStats)
library(gridExtra)
library(foreign)
library(ggplot2)

#Number of Iterations
R <- 400 # should be 400, but not when you're testing ...

#Remove Country NAs (Russia, Cyprus, Israel, Turkey and Bulgaria)
ESS2008b <- ESS2008sd
ESS2008b$Region <- ESS2008b$cntry
ESS2008b$Region <- as.character(ESS2008b$Region)
ESS2008b$Region[ESS2008b$Region=="RU"] <- NA
ESS2008b$Region[ESS2008b$Region=="TR"] <- NA
ESS2008b$Region[ESS2008b$Region=="BG"] <- NA
ESS2008b$Region[ESS2008b$Region=="CY"] <- NA
ESS2008b$Region[ESS2008b$Region=="IL"] <- NA
ESS2008b$Region[ESS2008b$Region=="BE"] <- "West"
ESS2008b$Region[ESS2008b$Region=="CH"] <- "West"
ESS2008b$Region[ESS2008b$Region=="CZ"] <- "East"
ESS2008b$Region[ESS2008b$Region=="DE"] <- "West"
ESS2008b$Region[ESS2008b$Region=="DK"] <- "West"
ESS2008b$Region[ESS2008b$Region=="EE"] <- "East"
ESS2008b$Region[ESS2008b$Region=="ES"] <- "West"
ESS2008b$Region[ESS2008b$Region=="FI"] <- "West"
ESS2008b$Region[ESS2008b$Region=="FR"] <- "West"
ESS2008b$Region[ESS2008b$Region=="GB"] <- "West"
ESS2008b$Region[ESS2008b$Region=="GR"] <- "West"
ESS2008b$Region[ESS2008b$Region=="HR"] <- "East"
ESS2008b$Region[ESS2008b$Region=="HU"] <- "East"
ESS2008b$Region[ESS2008b$Region=="IE"] <- "West"
ESS2008b$Region[ESS2008b$Region=="LV"] <- "East"
ESS2008b$Region[ESS2008b$Region=="NL"] <- "West"
ESS2008b$Region[ESS2008b$Region=="NO"] <- "West"
ESS2008b$Region[ESS2008b$Region=="PL"] <- "East"
ESS2008b$Region[ESS2008b$Region=="PT"] <- "West"
ESS2008b$Region[ESS2008b$Region=="RO"] <- "East"
ESS2008b$Region[ESS2008b$Region=="SE"] <- "West"
ESS2008b$Region[ESS2008b$Region=="SI"] <- "East"
ESS2008b$Region[ESS2008b$Region=="SK"] <- "East"
ESS2008b$Region[ESS2008b$Region=="UA"] <- "East"
table(ESS2008b$Region)

# Containers to save output
nCountries <- length(unique(ESS2008b$cntry)) #I continue to use the newly created dataframe now (ESS2008b)
nCountries
table(ESS2008b$cntry)
economics2008 <- women2008 <- homs2008 <- immigration2008 <- matrix(nrow = nCountries, ncol = R) #I changed the matrix so that the dimensions are the opposite
colnames(economics2008) <- 1:R
colnames(homs2008) <- 1:R
colnames(women2008) <- 1:R
colnames(immigration2008) <- 1:R
reg2008 <- unique(subset(ESS2008b, select = c(cntry,Region)))    # output of coef(m) seems to be alphabetical
rownames(economics2008) <- reg2008$cntry
rownames(women2008) <- reg2008$cntry
rownames(homs2008) <- reg2008$cntry
rownames(immigration2008) <- reg2008$cntry

#Bootstrapping (this takes a lot of time! For me it takes up to a day or two)
for (i in 1:R) {
  
  print(i)
  bsSample <- ESS2008b[sample(1:nrow(ESS2008b), nrow(ESS2008b), replace = TRUE), ]
  
  m <- modelX2008 <- lmer(lrscale ~ EconomicInequality + AntiFeministAttitudeComb + 
                            LessImmigration + 
                            IntOfHomosexuality + age + EducationYears + hinctnta +
                            female + rural + religiosity + union + ethnicMinority + 
                            ReligDen + ProEU + TraditionImportant + RuleFollowImportant +
                            SafetyImportant + OpenToExperience +
                            (1 + EconomicInequality + AntiFeministAttitudeComb + 
                               LessImmigration + 
                               IntOfHomosexuality + ProEU | cntry), data=bsSample)
  
  b <- ranef(m)$cntry
  c <- fixef(m)
  economics2008[, i] <- b$EconomicInequality + c["EconomicInequality"]
  women2008[, i] <- b$AntiFeministAttitudeComb + c["AntiFeministAttitudeComb"]
  immigration2008[, i] <- b$LessImmigration + c["LessImmigration"]
  homs2008[, i] <- b$IntOfHomosexuality + c["IntOfHomosexuality"]
}

beep(sound = "coin")

#Data frame
economics2008b <- economics2008
economics2008 <- as.data.frame(economics2008)
economics2008$cntry <- rownames(economics2008)
economics2008 <- economics2008[c("CZ", "EE", "HR", "HU", "LV", 
                         "PL", "RO", "SI", "UA", "SK", 
                         "BE", "CH", "DE", "DK", "ES",
                         "FI", "FR", "GB", "GR", "IE",  
                         "NL", "NO", "PT", "SE"),]
economics2008$id <- c(1:nCountries)
economics2008$cntry <- factor(economics2008$cntry, levels = economics2008$cntry[order(economics2008$id)])
women2008b <- women2008
women2008 <- as.data.frame(women2008)
women2008$cntry <- rownames(women2008)
women2008 <- women2008[c("CZ", "EE", "HR", "HU", "LV", 
                         "PL", "RO", "SI", "UA", "SK", 
                         "BE", "CH", "DE", "DK", "ES",
                         "FI", "FR", "GB", "GR", "IE",  
                         "NL", "NO", "PT", "SE"),]
women2008$id <- c(1:nCountries)
women2008$cntry <- factor(women2008$cntry, levels = women2008$cntry[order(women2008$id)])
immigration2008b <- immigration2008
immigration2008 <- as.data.frame(immigration2008)
immigration2008$cntry <- rownames(immigration2008)
immigration2008 <- immigration2008[c("CZ", "EE", "HR", "HU", "LV", 
                                     "PL", "RO", "SI", "UA", "SK", 
                                     "BE", "CH", "DE", "DK", "ES",
                                     "FI", "FR", "GB", "GR", "IE",  
                                     "NL", "NO", "PT", "SE"),]
immigration2008$id <- c(1:nCountries)
immigration2008$cntry <- factor(immigration2008$cntry, levels = immigration2008$cntry[order(immigration2008$id)])
homs2008b <- homs2008
homs2008 <- as.data.frame(homs2008)
homs2008$cntry <- rownames(homs2008)
homs2008 <- homs2008[c("CZ", "EE", "HR", "HU", "LV", 
                       "PL", "RO", "SI", "UA", "SK", 
                       "BE", "CH", "DE", "DK", "ES",
                       "FI", "FR", "GB", "GR", "IE",  
                       "NL", "NO", "PT", "SE"),]
homs2008$id <- c(1:nCountries)
homs2008$cntry <- factor(homs2008$cntry, levels = homs2008$cntry[order(homs2008$id)])

#Calculate the mean
economics2008$Mean <- rowMeans(economics2008[,1:R])
women2008$Mean <- rowMeans(women2008[,1:R])
immigration2008$Mean <- rowMeans(immigration2008[,1:R])
homs2008$Mean <- rowMeans(homs2008[,1:R])

#Or calculate the real coefficients
economicsCoef2008 <- womenCoef2008 <- homsCoef2008 <- immigrationCoef2008 <- matrix(nrow = nCountries, ncol = 1)

m <- modelX2008 <- lmer(lrscale ~ EconomicInequality + AntiFeministAttitudeComb + 
                          LessImmigration + 
                          IntOfHomosexuality + age + EducationYears + hinctnta +
                          female + rural + religiosity + union + ethnicMinority + ReligDen + ProEU +
                          TraditionImportant + RuleFollowImportant +
                          SafetyImportant + OpenToExperience +
                          (1 + EconomicInequality + AntiFeministAttitudeComb + 
                          LessImmigration + 
                          IntOfHomosexuality + ProEU | cntry), data=ESS2008b)

beep()
b <- ranef(m)$cntry
c <- fixef(m)
economicsCoef2008 <- b$EconomicInequality + c["EconomicInequality"]
womenCoef2008 <- b$AntiFeministAttitudeComb + c["AntiFeministAttitudeComb"]
immigrationCoef2008 <- b$LessImmigration + c["LessImmigration"]
homsCoef2008 <- b$IntOfHomosexuality + c["IntOfHomosexuality"]

#Data frame for coefficients
economicsCoef2008 <- as.data.frame(economicsCoef2008)
economicsCoef2008$cntry <- reg2008$cntry
rownames(economicsCoef2008) <- reg2008$cntry
economicsCoef2008 <- economicsCoef2008[c("CZ", "EE", "HR", "HU", "LV", 
                                         "PL", "RO", "SI", "UA", "SK", 
                                         "BE", "CH", "DE", "DK", "ES",
                                         "FI", "FR", "GB", "GR", "IE",  
                                         "NL", "NO", "PT", "SE"),]
economicsCoef2008$id <- c(1:nCountries)
economicsCoef2008$cntry <- factor(economicsCoef2008$cntry, levels = economicsCoef2008$cntry[order(economicsCoef2008$id)])
womenCoef2008 <- as.data.frame(womenCoef2008)
womenCoef2008$cntry <- reg2008$cntry
rownames(womenCoef2008) <- reg2008$cntry
womenCoef2008 <- womenCoef2008[c("CZ", "EE", "HR", "HU", "LV", 
                                 "PL", "RO", "SI", "UA", "SK", 
                                 "BE", "CH", "DE", "DK", "ES",
                                 "FI", "FR", "GB", "GR", "IE",  
                                 "NL", "NO", "PT", "SE"),]
womenCoef2008$id <- c(1:nCountries)
womenCoef2008$cntry <- factor(womenCoef2008$cntry, levels = womenCoef2008$cntry[order(womenCoef2008$id)])
immigrationCoef2008 <- as.data.frame(immigrationCoef2008)
immigrationCoef2008$cntry <- reg2008$cntry
rownames(immigrationCoef2008) <- reg2008$cntry
immigrationCoef2008 <- immigrationCoef2008[c("CZ", "EE", "HR", "HU", "LV", 
                                             "PL", "RO", "SI", "UA", "SK", 
                                             "BE", "CH", "DE", "DK", "ES",
                                             "FI", "FR", "GB", "GR", "IE",  
                                             "NL", "NO", "PT", "SE"),]
immigrationCoef2008$id <- c(1:nCountries)
immigrationCoef2008$cntry <- factor(immigrationCoef2008$cntry, levels = immigrationCoef2008$cntry[order(immigrationCoef2008$id)])
homsCoef2008 <- as.data.frame(homsCoef2008)
homsCoef2008$cntry <- reg2008$cntry
rownames(homsCoef2008) <- reg2008$cntry
homsCoef2008 <- homsCoef2008[c("CZ", "EE", "HR", "HU", "LV", 
                               "PL", "RO", "SI", "UA", "SK", 
                               "BE", "CH", "DE", "DK", "ES",
                               "FI", "FR", "GB", "GR", "IE",  
                               "NL", "NO", "PT", "SE"),]
homsCoef2008$id <- c(1:nCountries)
homsCoef2008$cntry <- factor(homsCoef2008$cntry, levels = homsCoef2008$cntry[order(homsCoef2008$id)])

#Integrating the coefficients into the new dataframe
economics2008$RealCoef <- economicsCoef2008$economicsCoef2008
women2008$RealCoef <- womenCoef2008$womenCoef2008
immigration2008$RealCoef <- immigrationCoef2008$immigrationCoef2008
homs2008$RealCoef <- homsCoef2008$homsCoef2008

#Calculate Confidence Intervals
economics2008$Low <- rowQuantiles(as.matrix(economics2008[,1:R]), probs = 0.025)
economics2008$High <- rowQuantiles(as.matrix(economics2008[,1:R]), probs = 0.975)
women2008$Low <- rowQuantiles(as.matrix(women2008[,1:R]), probs = 0.025)
women2008$High <- rowQuantiles(as.matrix(women2008[,1:R]), probs = 0.975)
immigration2008$Low <- rowQuantiles(as.matrix(immigration2008[,1:R]), probs = 0.025)
immigration2008$High <- rowQuantiles(as.matrix(immigration2008[,1:R]), probs = 0.975)
homs2008$Low <- rowQuantiles(as.matrix(homs2008[,1:R]), probs = 0.025)
homs2008$High <- rowQuantiles(as.matrix(homs2008[,1:R]), probs = 0.975)
economics2008$spread <- abs(economics2008$Low - economics2008$High)
women2008$spread <- abs(women2008$Low - women2008$High)
immigration2008$spread <- abs(immigration2008$Low - immigration2008$High)
homs2008$spread <- abs(homs2008$Low - homs2008$High)
economics2008$confLo <- economics2008$RealCoef - (economics2008$spread/2)
economics2008$confHi <- economics2008$RealCoef + (economics2008$spread/2)
women2008$confLo <- women2008$RealCoef - (women2008$spread/2)
women2008$confHi <- women2008$RealCoef + (women2008$spread/2)
immigration2008$confLo <- immigration2008$RealCoef - (immigration2008$spread/2)
immigration2008$confHi <- immigration2008$RealCoef + (immigration2008$spread/2)
homs2008$confLo <- homs2008$RealCoef - (homs2008$spread/2)
homs2008$confHi <- homs2008$RealCoef + (homs2008$spread/2)

#Integrating Region Value
OrderDF2008 <- reg2008
rownames(OrderDF2008) <- OrderDF2008$cntry
OrderDF2008 <- OrderDF2008[c("CZ", "EE", "HR", "HU", "LV", 
                             "PL", "RO", "SI", "UA", "SK", 
                             "BE", "CH", "DE", "DK", "ES",
                             "FI", "FR", "GB", "GR", "IE",  
                             "NL", "NO", "PT", "SE"),]
economics2008$Region <- OrderDF2008$Region
women2008$Region <- OrderDF2008$Region
immigration2008$Region <- OrderDF2008$Region
homs2008$Region <- OrderDF2008$Region

#Reorder Coefficients Based on Size
economics2008$cntry <- factor(economics2008$cntry, 
                              levels = economics2008$cntry[order(economics2008$RealCoef)])
women2008$cntry <- factor(women2008$cntry, 
                          levels = women2008$cntry[order(women2008$RealCoef)])
immigration2008$cntry <- factor(immigration2008$cntry, 
                                levels = immigration2008$cntry[order(immigration2008$RealCoef)])
homs2008$cntry <- factor(homs2008$cntry, 
                         levels = homs2008$cntry[order(homs2008$RealCoef)])

# FIGURE 1
p12008 <- ggplot(economics2008, aes(x = RealCoef, y = cntry)) +
  theme_minimal() + xlim(-0.4, 1.1) +
  geom_hline(aes(yintercept = 0), linetype = 3, color = "gray30") +
  theme(legend.position = "none") + 
  geom_errorbarh(aes(y = cntry, xmin = confLo, xmax = confHi, height = 0)) +
  geom_point(aes(shape=Region)) +
  scale_shape_manual(values=c(15, 17)) +
  xlab("Economic Inequality") + 
  ylab("") + scale_y_discrete(limits = rev(levels(economics2008$cntry))) +
  geom_vline(xintercept = 0, color = "black", size=0.3)

p22008 <- ggplot(immigration2008, aes(x = RealCoef, y = cntry)) +
  theme_minimal() + xlim(-0.4, 1.1) +
  geom_hline(aes(yintercept = 0), linetype = 3, color = "gray30") +
  theme(legend.position = "none") + 
  geom_errorbarh(aes(y = cntry, xmin = confLo, xmax = confHi, height = 0)) +
  geom_point(aes(shape=Region)) +
  scale_shape_manual(values=c(15, 17)) +
  xlab("Less Immigration") + 
  ylab("") + scale_y_discrete(limits = rev(levels(immigration2008$cntry))) +
  geom_vline(xintercept = 0, color = "black", size=0.3)

p32008 <- ggplot(women2008, aes(x = RealCoef, y = cntry)) +
  theme_minimal() + xlim(-0.4, 1.1) +
  geom_hline(aes(yintercept = 0), linetype = 3, color = "gray30") +
  theme(legend.position = "none") + 
  geom_errorbarh(aes(y = cntry, xmin = confLo, xmax = confHi, height = 0)) +
  geom_point(aes(shape=Region)) +
  scale_shape_manual(values=c(15, 17)) +
  xlab("Anti-Feminist Attitude") + 
  ylab("") + scale_y_discrete(limits = rev(levels(women2008$cntry))) +
  geom_vline(xintercept = 0, color = "black", size=0.3)

p42008 <- ggplot(homs2008, aes(x = RealCoef, y = cntry)) +
  theme_minimal() + xlim(-0.4, 1.1) +
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
  ylab("") + scale_y_discrete(limits = rev(levels(homs2008$cntry))) +
  geom_vline(xintercept = 0, color = "black", size=0.3)

grid.arrange(p12008, p22008, p32008, p42008,
             widths = c(1, 1),
             layout_matrix = rbind(c(1, 2),
                                   c(3, 4))
)

#Save Generated Data
setwd("")
write.csv(economics2008, "economics2008[DATE].csv")
write.csv(women2008, "women2008[DATE].csv")
write.csv(immigration2008, "immigration2008[DATE].csv")
write.csv(homs2008, "homs2008[DATE].csv")
