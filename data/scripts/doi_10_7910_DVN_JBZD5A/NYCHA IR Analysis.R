## Nested spatial regression models and summary statistics ##

Model.Data <- read.csv("~/NYCHA Spatial Regression Data.csv")

ONE <- lm(Model.Data$IR ~ Model.Data$NYCHA)
summary(ONE)

TWO <- lm(Model.Data$IR ~ Model.Data$NYCHA + Model.Data$Percent.NYCHA)
summary(TWO)

THREE <- lm(Model.Data$IR ~ Model.Data$NYCHA + Model.Data$Percent.NYCHA + Model.Data$Spatial.Lag)
summary(THREE)

FOUR <- lm(Model.Data$IR ~ Model.Data$NYCHA + Model.Data$Percent.NYCHA + Model.Data$Spatial.Lag + Model.Data$ln.Percent.18.35. + Model.Data$ln.Percent.Black.)
summary(FOUR)

FIVE <- lm(Model.Data$IR ~ Model.Data$NYCHA + Model.Data$Percent.NYCHA + Model.Data$Spatial.Lag + Model.Data$ln.Percent.18.35. + Model.Data$ln.Percent.Black. + Model.Data$ln.Percent.Hispanic.)
summary(FIVE)

SIX <- lm(Model.Data$IR ~ Model.Data$NYCHA + Model.Data$Percent.NYCHA + Model.Data$Spatial.Lag + Model.Data$ln.Percent.18.35. + Model.Data$ln.Percent.Black. + Model.Data$ln.Percent.Hispanic. + Model.Data$CD + Model.Data$CI)
summary(SIX)

SEVEN <- lm(Model.Data$IR ~ Model.Data$NYCHA + Model.Data$Percent.NYCHA + Model.Data$Spatial.Lag + Model.Data$ln.Percent.18.35. + Model.Data$ln.Percent.Black. + Model.Data$ln.Percent.Hispanic. + Model.Data$CD + Model.Data$CI + Model.Data$ln.Crime.Rate.)
summary(SEVEN)

EIGHT <- lm(Model.Data$IR ~ Model.Data$NYCHA + Model.Data$Percent.NYCHA + Model.Data$Spatial.Lag + Model.Data$ln.Percent.18.35. + Model.Data$ln.Percent.Black. + Model.Data$ln.Percent.Hispanic. + Model.Data$CD + Model.Data$CI + Model.Data$ln.SQF.Rate.)
summary(EIGHT)

NINE <- lm(Model.Data$IR ~ Model.Data$NYCHA + Model.Data$Percent.NYCHA + Model.Data$Spatial.Lag + Model.Data$ln.Percent.18.35. + Model.Data$ln.Percent.Black. + Model.Data$ln.Percent.Hispanic. + Model.Data$CD + Model.Data$CI + Model.Data$ln.Crime.Rate. + Model.Data$ln.SQF.Rate.)
summary(NINE)



## Figure 2 - Predicted rates of incarceration ##

## Full spatial regression model ##

eight_pred <- lm(IR ~ NYCHA + Percent.NYCHA + Spatial.Lag + Percent.18.35 + Percent.Black + Percent.Hispanic + CD + CI + Crime.Rate + SQF.Rate, data=Model.Data)


## Predicted rates of incarceration. Vary CD with Non-NYCHA medians. ##

CD_non <- read.csv(“CD Non-NYCHA Medians data”)

CD_non_preds <- predict(eight_pred, CD_non, type="response", se.fit=TRUE)
CD_non_probs <- CD_non_preds$fit

CD_non_lower <- CD_non_preds$fit - (1.96*CD_non_preds$se.fit)
CD_non_upper <- CD_non_preds$fit + (1.96*CD_non_preds$se.fit)


pdf()
plot(CD_non$CD, CD_non_probs, type="l", ylab="Predicted Incarceration Rate", xlab="Concentrated Disadvantage Index", ylim = c(50,850), bty="L")
lines(CD_non$CD, CD_non_lower, lty=2)
lines(CD_non$CD, CD_non_upper, lty=2)
dev.off()


## Predicted rates of incarceration. Vary CD with NYCHA medians. ##

CD_nycha <- read.csv(“CD NYCHA Medians data”)

CD_nycha_preds <- predict(eight_pred, CD_nycha, type="response", se.fit=TRUE)
CD_nycha_probs <- CD_nycha_preds$fit

CD_nycha_lower <- CD_nycha_preds$fit - (1.96*CD_nycha_preds$se.fit)
CD_nycha_upper <- CD_nycha_preds$fit + (1.96*CD_nycha_preds$se.fit)

pdf()
plot(CD_nycha$CD, CD_nycha_probs, type="l", ylab="Predicted Incarceration Rate", xlab="Concentrated Disadvantage Index", ylim = c(50,850), bty="L")
lines(CD_nycha$CD, CD_nycha_lower, lty=2)
lines(CD_nycha$CD, CD_nycha_upper, lty=2)
dev.off()


## Predicted rates of incarceration. Vary % Black with Non-NYCHA medians. ##

Black_non <- read.csv(“% Black Non-NYCHA Medians data”)

Black_non_preds <- predict(eight_pred, Black_non, type="response", se.fit=TRUE)
Black_non_probs <- Black_non_preds$fit

Black_non_lower <- Black_non_preds$fit - (1.96*Black_non_preds$se.fit)
Black_non_upper <- Black_non_preds$fit + (1.96*Black_non_preds$se.fit)

pdf()
plot(Black_non$Percent.Black, Black_non_probs, type="l", ylab="Predicted Incarceration Rate", xlab="% Black", ylim = c(50,850), bty="L")
lines(Black_non$Percent.Black, Black_non_lower, lty=2)
lines(Black_non$Percent.Black, Black_non_upper, lty=2)
dev.off()


## Predicted rates of incarceration. Vary % Black with NYCHA medians. ##

Black_nycha <- read.csv(“% Black NYCHA Medians data”)

Black_nycha_preds <- predict(eight_pred, Black_nycha, type="response", se.fit=TRUE)
Black_nycha_probs <- Black_nycha_preds$fit

Black_nycha_lower <- Black_nycha_preds$fit - (1.96*Black_nycha_preds$se.fit)
Black_nycha_upper <- Black_nycha_preds$fit + (1.96*Black_nycha_preds$se.fit)


pdf()
plot(Black_nycha$Percent.Black, Black_nycha_probs, type="l", ylab="Predicted Incarceration Rate", xlab="% Black", ylim = c(50,850), bty="L")
lines(Black_nycha$Percent.Black, Black_nycha_lower, lty=2)
lines(Black_nycha$Percent.Black, Black_nycha_upper, lty=2)
dev.off()


## Figure 3 - Incarceration rates at different geographic resolution ##

library(ggplot2) 
library(readxl)
library(gridExtra)

Figure_3_Data <- read.csv("~/Incarceration Rate Geographic Resolution.csv")

f3_tract <- Figure_3_Data[835:2929,]
f3_school <- Figure_3_Data[77:834,]
f3_zip <- Figure_3_Data[2930:3106,]
f3_precinct <- Figure_3_Data[1:76,]

colPal <- c("#004C99", "#990000")

p1 <- ggplot(data=f3_tract, aes(y=IR, group=NYCHA, fill=NYCHA)) + labs(title = "Census Tracts \n (n = 2095)") + geom_boxplot() + scale_fill_manual(values=colPal) + theme_minimal() + theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y=element_blank()) + theme(axis.text.x=element_blank()) + ylim(0, 2100)
p2 <- ggplot(data=f3_school, aes(y=IR, group=NYCHA, fill=NYCHA)) + labs(title = "Zoned Elementary Schools \n (n = 758)") + geom_boxplot() + scale_fill_manual(values=colPal) + theme_minimal() + theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y=element_blank()) + theme(axis.text.x=element_blank()) + ylim(0, 2100)
p3 <- ggplot(data=f3_zip, aes(y=IR, group=NYCHA, fill=NYCHA)) + labs(title = "ZIP Codes \n (n = 177)") + geom_boxplot() + scale_fill_manual(values=colPal) + theme_minimal() + theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y=element_blank()) + theme(axis.text.x=element_blank()) + ylim(0, 2100)
p4 <- ggplot(data=f3_precinct, aes(y=IR, group=NYCHA, fill=NYCHA)) + labs(title = "NYPD Precincts \n (n = 76)") + geom_boxplot() + scale_fill_manual(values=colPal) + theme_minimal() + theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y=element_blank()) + theme(axis.text.x=element_blank()) + ylim(0, 2100)

g <- arrangeGrob(p1, p2, p3, p4, nrow=2)
ggsave("Figure 3.pdf", g)
