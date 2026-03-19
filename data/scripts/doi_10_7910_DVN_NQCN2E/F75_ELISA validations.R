library(ggplot2)

F75 <- read.csv("F75_ELISA.csv", header = T, sep = ",")

F75$kwashf <- as.factor(F75$kwash)

#kwash vs maras
p <- ggplot(F75, aes(x=kwashf, y=(log(MMP2)), color = kwashf)) + geom_boxplot(width = 0.5)
p + geom_jitter(shape=16, position=position_jitter(0.2), size = 1) + ylab ("log MMP2 (ng/mL)") + theme_classic() + theme(legend.position = "none")

#change figure dimensions to 200 x 250

library(survival)

prot = clogit(kwash ~ I(MMP2/1e3) + hiv_results + agemons + sex + site + strata(albumine_d0), data=F75)
summary(prot)

#oedema grades

F75$LA = (F75$LUMICAN*1e-6)/(F75$albumine_d0*10)
F75$MMP2A = (F75$MMP2*1e-6)/(F75$albumine_d0*10)
F75$TIMP1A = (F75$TIMP1*1e-6)/(F75$albumine_d0*10)

library(plyr)

F75$oedema.num = factor(revalue(F75$oedema, c("None" = "0", "+" = "1", "++" = "2", "+++" = "3")), levels = c("0", "1", "2", "3"))

p <- ggplot(F75, aes(x=oedema.num, y=log(MMP2A), color = oedema.num)) + geom_boxplot(width = 0.5)
p + geom_jitter(shape=16, position=position_jitter(0.2), size = 1) + ylab ("log MMP2:Albumin (g/g)") + theme_classic() + theme(legend.position = "none")

#300 x 250

library(MASS)

oedema = polr(oedema.num ~ log(TIMP1A) + hiv_results + agemons + + sex + site, data = F75, Hess = TRUE)
oedema = polr(oedema.num ~ log(Hya/(1000*albumine_d0)) + hiv_results + agemons + + sex + site, data = SynT, Hess = TRUE)

summary(oedema)
(ctable <- coef(summary(oedema)))
pval <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = pval))
