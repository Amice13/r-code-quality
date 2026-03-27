library(dplyr)
library(ggplot2)

SynT <- read.csv("glycocalyx_data.csv", header = T, sep = ",")


kwash = SynT %>% filter(kwash == "Yes")
maras = SynT %>% filter(kwash =="No")

#kwash vs maras
p <- ggplot(SynT, aes(x=kwash, y=log(Hya), color = kwash)) + geom_boxplot()
p + geom_jitter(shape=16, position=position_jitter(0.2), size = 1) + ylab ("log Hyaluronan (ng/mL)") + theme_classic() + theme(legend.position = "none")

#change figure dimensions to 200 x 250

library(survival)
SynT$kwashn = as.numeric(as.factor(SynT$kwash)) - 1
prot = clogit(kwashn ~ log(Hya) + hiv_results + agemons + sex + site + strata(albumine_d0), data=SynT)
summary(prot)


#oedema grades

SynT$SA = (SynT$Syndecan*1e-6)/(SynT$albumine_d0*10)
SynT$HA = (SynT$Hya*1e-6)/(SynT$albumine_d0*10)

library(plyr)

SynT$oedema.num = factor(revalue(SynT$oedema, c("None" = "0", "+" = "1", "++" = "2", "+++" = "3")), levels = c("0", "1", "2", "3"))

p <- ggplot(SynT, aes(x=oedema.num, y=log(SA), color = oedema.num)) + geom_boxplot()
p + geom_jitter(shape=16, position=position_jitter(0.2), size = 1) + ylab ("log Syndecan:Albumin (g/g)") + theme_classic() + theme(legend.position = "none")

#300 x 250

library(MASS)

oedema = polr(oedema.num ~ log(HA) + hiv_results + agemons + + sex + site, data = SynT, Hess = TRUE)
oedema = polr(oedema.num ~ log(Hya/(1000*albumine_d0)) + hiv_results + agemons + + sex + site, data = SynT, Hess = TRUE)

summary(oedema)
(ctable <- coef(summary(oedema)))
pval <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = pval))


dat <- merge(F75, SynT[,c("subjid", "Hya", "Syndecan")], by = "subjid")

p <- ggplot(dat, aes(x=kwashf, y=(log(Syndecan)), color = kwashf)) + geom_boxplot()
p + geom_jitter(shape=16, position=position_jitter(0.2), size = 3) + ylab ("log Hyaluronan (ng/mL)") + theme_classic() + theme(legend.position = "none")

dat$kwashf = as.numeric(as.factor(dat$kwashf)) - 1
prot = clogit(kwashf ~ log(Syndecan) + hiv_results + agemons + sex + site + strata(albumine_d0), data=dat)
summary(prot)
