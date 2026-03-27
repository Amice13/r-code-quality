

library(lme4)
library(mgcv)

gamMod <- gam(bci ~ s(age), data = bciFrame)

sett <- c()

for (i in 1:nrow(bciFrame)) {
  yr <- as.numeric(bciFrame$year[i])
  ind <- bciFrame$individual[i]
  sub <- subset(badgers_off, Tattoo == ind)
  
  sett <- c(sett, sub$Sett[1])

}

bciFrame$residual <- bciFrame$bci - predict(gamMod)
bciFrame$sett <- sett

bciMod <- lmer(residual ~ (1|year) + (1|season) + sex + (1|individual) + (1|social.group), data = bciFrame)
bciFrameAdults <- subset(bciFrame, age > 0)
bciFrameAdults$individual <- factor(bciFrameAdults$individual)
bciMod2 <- lmer(residual ~ (1|year) + season + sex + (1|individual) + (1|social.group), data = bciFrameAdults)
bciMod3 <- lmer(residual ~ (1|year) + season + sex + (1|individual), data = bciFrameAdults)
bciMod4 <- lmer(residual ~ (1|year) + season + sex, data = bciFrameAdults)
bciMod5 <- lmer(residual ~ (1|individual) + (1|social.group) + season + sex, data = bciFrameAdults)
bciMod6 <- lmer(residual ~ season + sex + (1 + 1|social.group/individual), data = bciFrameAdults)
bciMod7 <- lmer(residual ~ season + sex + (1 + 1|sett/individual), data = bciFrameAdults)
bciMod8 <- lmer(residual ~ season + sex + (1|individual) + (1|sett), data = bciFrameAdults)

#bciMod6 is probably our best one so far