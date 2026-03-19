
#Media Reflect! Policy, the Public, and the News
#Christopher Wlezien
#Stuart Soroka

#This file replicates analyses in the main body and text of the above paper, forthcoming in APSR.

library(dplyr)
library(modelsummary)
load("media.reflect.data.Rdata")

#Variables are as follows:
#fy Fiscal year
#domain Policy domain, numbered
#domain_text Policy domain, text
#sp2std Standardized changes in spending
#sig1n Media signal by fiscal year
#sig1an Media signal, first half of fiscal year
#sig1bn Media signal, second half of fiscal year
#pr Public opinion
#pri Public opinion, interpolated for missing year

#generate lags and leads
for (d in unique(B$domain)) {
  B$D.sp2std[B$domain==d] <- B$sp2std[B$domain==d] - lag(B$sp2std[B$domain==d])
  B$LD.sp2std[B$domain==d] <- lag(B$D.sp2std[B$domain==d])
  B$FD.sp2std[B$domain==d] <- lead(B$D.sp2std[B$domain==d])
  B$L.sig1n[B$domain==d] <- lag(B$sig1n[B$domain==d])
  B$L2.sig1n[B$domain==d] <- lag(B$sig1n[B$domain==d],2)
  B$L.sig1an[B$domain==d] <- lag(B$sig1an[B$domain==d])
  B$L.sig1bn[B$domain==d] <- lag(B$sig1bn[B$domain==d])
  B$L.pri[B$domain==d] <- lag(B$pri[B$domain==d])
  B$L2.pri[B$domain==d] <- lag(B$pri[B$domain==d],2)
  B$L.pr[B$domain==d] <- lag(B$pr[B$domain==d])
  B$L2.pr[B$domain==d] <- lag(B$pr[B$domain==d],2)
}


#Table 1.
B$public <- B$pri
B$public_lag <- B$L.pri
B$media_lag <- B$L.sig1n
B$policy_lag <- B$D.sp2std
model1 <- lm(public ~ public_lag + media_lag + policy_lag + domain_text, data=B)
B$policy <- B$D.sp2std
B$public_lag <- B$L2.pri
B$media_lag <- B$L2.sig1n
B$policy_lag <- B$LD.sp2std
model2 <- lm(policy ~ public_lag + media_lag + policy_lag + domain_text, data=B)
B$media <- B$sig1n
B$public_lag <- B$L.pri
B$media_lag <- B$L.sig1n
B$policy_lag <- B$D.sp2std
model3 <- lm(media ~ public_lag + media_lag + policy_lag + domain_text, data=B)
cm <- c("public_lag", "media_lag", "policy_lag")
modelsummary(list(model1, model2, model3),
             stars=T, 
             gof_omit="Log|IC|RMSE|F",
             coef_map = cm,
             output = "table1.rtf")

#Table 2.
B$public <- B$pri
B$public_lag <- B$L.pri
B$media_late <- B$L.sig1bn
B$policy_lag <- B$D.sp2std
model4 <- lm(public ~ public_lag + media_late + policy_lag + domain_text, data=B)
B$policy <- B$D.sp2std
B$public <- B$L.pri
B$media_early <- B$L.sig1an
B$policy_lag <- B$LD.sp2std
model5 <- lm(policy ~ public + media_early + policy_lag + domain_text, data=B)
B$media <- B$sig1an
B$public_lag <- B$L.pri
B$media_late <- B$L.sig1bn
B$policy_lag <- B$D.sp2std
model6 <- lm(media ~ public_lag + media_late + policy_lag + domain_text, data=B)
B$media <- B$sig1bn
B$public <- B$pri
B$media_early <- B$sig1an
B$policy <- B$FD.sp2std
model7 <- lm(media ~ public + media_early + policy + domain_text, data=B)
cm <- c("public", "public_lag", "media_early", "media_late",
        "policy", "policy_lag")
modelsummary(list(model4, model5, model6, model7),
             stars=T, 
             gof_omit="Log|IC|RMSE|F",
             coef_map = cm,
             output = "table2.rtf")

#Appendix

#Table A1.
#Note that ADF test were generated in STATA rather than R.The following text will reproduce 
#the table in R using the rStata package. Altenratively, Stata text on lines x-x can be 
#pasted directly in a STATA do-file.
library(RStata)
options("RStata.StataPath" = '/Applications/Stata/StataMP.app/Contents/MacOS/stata-mp')
options("RStata.StataVersion" = 15)
stata_src <- "
      tsset domain fy
      dfuller sp2std if domain==1 & fy>1979 & fy<2019, regr lags(1)
      dfuller d.sp2std if domain==1 & fy>1979 & fy<2019, regr
      dfuller sp2std if domain==2 & fy>1979 & fy<2019, regr trend
      dfuller d.sp2std if domain==2 & fy>1979 & fy<2019, regr
      dfuller sp2std if domain==3 & fy>1979 & fy<2019, regr trend
      dfuller d.sp2std if domain==3 & fy>1979 & fy<2019, regr
      dfuller sig1n if domain==1, regr
      dfuller sig1n if domain==2, regr
      dfuller sig1n if domain==3, regr
      dfuller pri if domain==1 & fy>1979 & fy<2019, regr lags(1)
      dfuller pri if domain==2 & fy>1979 & fy<2019, regr
      dfuller pri if domain==3 & fy>1979 & fy<2019, regr lags(1)
      "
stata(stata_src,data.in=B, data.out=F, stata.echo=T)
C <- B[!is.na(B$pr),c("fy","domain","pr")]
table(C$domain)
C$time <- 1:28
stata_src <- "
      tsset domain time
      dfuller pr if domain==1 & fy>1979 & fy<2019, regr
      dfuller pr if domain==2 & fy>1979 & fy<2019, regr
      dfuller pr if domain==3 & fy>1979 & fy<2019, regr 
      "
stata(stata_src,data.in=C, data.out=F, stata.echo=T)
rm(C)

#Table A2.
G <- B[, c("sig1n","FD.sp2std","pri")]
round(cor(G, use="complete.obs"),3)
G <- B[B$domain==1, c("sig1n","FD.sp2std","pri")]
round(cor(G, use="complete.obs"),3)
G <- B[B$domain==2, c("sig1n","FD.sp2std","pri")]
round(cor(G, use="complete.obs"),3)
G <- B[B$domain==3, c("sig1n","FD.sp2std","pri")]
round(cor(G, use="complete.obs"),3)

#Table A3.
B$public <- B$pri
B$public_lag <- B$L.pri
B$media_lag <- B$L.sig1n
B$policy_lag <- B$D.sp2std
model1.1 <- lm(public ~ public_lag + media_lag + policy_lag, data=B[B$domain==1,])
model1.2 <- lm(public ~ public_lag + media_lag + policy_lag, data=B[B$domain==2,])
model1.3 <- lm(public ~ public_lag + media_lag + policy_lag, data=B[B$domain==3,])
B$policy <- B$D.sp2std
B$public_lag <- B$L2.pri
B$media_lag <- B$L2.sig1n
B$policy_lag <- B$LD.sp2std
model2.1 <- lm(policy ~ public_lag + media_lag + policy_lag, data=B[B$domain==1,])
model2.2 <- lm(policy ~ public_lag + media_lag + policy_lag, data=B[B$domain==2,])
model2.3 <- lm(policy ~ public_lag + media_lag + policy_lag, data=B[B$domain==3,])
B$media <- B$sig1n
B$public_lag <- B$L.pri
B$media_lag <- B$L.sig1n
B$policy_lag <- B$D.sp2std
model3.1 <- lm(media ~ public_lag + media_lag + policy_lag, data=B[B$domain==1,])
model3.2 <- lm(media ~ public_lag + media_lag + policy_lag, data=B[B$domain==2,])
model3.3 <- lm(media ~ public_lag + media_lag + policy_lag, data=B[B$domain==3,])
cm <- c("public_lag", "media_lag", "policy_lag", "(Intercept)")
modelsummary(list(model1.1, model2.1, model3.1),
             stars=T, 
             gof_omit="Log|IC|RMSE|F",
             coef_map = cm,
             output = "tableA3.part1.rtf")
modelsummary(list(model1.2, model2.2, model3.2),
             stars=T, 
             gof_omit="Log|IC|RMSE|F",
             coef_map = cm,
             output = "tableA3.part2.rtf")
modelsummary(list(model1.3, model2.3, model3.3),
             stars=T, 
             gof_omit="Log|IC|RMSE|F",
             coef_map = cm,
             output = "tableA3.part3.rtf")

#Table A4
D <- B[!is.na(B$pr),]
D$time <- c(1:28)
for (d in unique(D$domain)) {
  D$D.sp2std[D$domain==d] <- D$sp2std[D$domain==d] - lag(D$sp2std[D$domain==d])
  D$LD.sp2std[D$domain==d] <- lag(D$D.sp2std[D$domain==d])
  D$FD.sp2std[D$domain==d] <- lead(D$D.sp2std[D$domain==d])
  D$L.sig1n[D$domain==d] <- lag(D$sig1n[D$domain==d])
  D$L2.sig1n[D$domain==d] <- lag(D$sig1n[D$domain==d],2)
  D$L.sig1an[D$domain==d] <- lag(D$sig1an[D$domain==d])
  D$L.sig1bn[D$domain==d] <- lag(D$sig1bn[D$domain==d])
  D$L.pri[D$domain==d] <- lag(D$pri[D$domain==d])
  D$L2.pri[D$domain==d] <- lag(D$pri[D$domain==d],2)
  D$L.pr[D$domain==d] <- lag(D$pr[D$domain==d])
  D$L2.pr[D$domain==d] <- lag(D$pr[D$domain==d],2)
}
D$public <- D$pr
D$public_lag <- D$L.pr
D$media_lag <- D$L.sig1n
D$policy_lag <- D$D.sp2std
modelA4.1 <- lm(public ~ public_lag + media_lag + policy_lag + domain_text, data=D)
D$policy <- D$D.sp2std
D$public_lag <- D$L2.pr
D$media_lag <- D$L2.sig1n
D$policy_lag <- D$LD.sp2std
modelA4.2 <- lm(policy ~ public_lag + media_lag + policy_lag + domain_text, data=D)
D$media <- D$sig1n
D$public_lag <- D$L.pr
D$media_lag <- D$L.sig1n
D$policy_lag <- D$D.sp2std
modelA4.3 <- lm(media ~ public_lag + media_lag + policy_lag + domain_text, data=D)
cm <- c("public_lag", "media_lag", "policy_lag")
modelsummary(list(modelA4.1, modelA4.2, modelA4.3),
             stars=T, 
             gof_omit="Log|IC|RMSE|F",
             coef_map = cm,
             output = "tableA4.rtf")

#Table A5
B$inmodel <- 0
B$inmodel[!is.na(B$sig1n) & !is.na(B$fy)] <- 1
m <- B[,c("fy","domain","inmodel")]
m <- m[m$inmodel==1,]
dt.model <- lm(sig1n ~ fy + domain_text, data=B)
m$pred <- predict(dt.model)
m$res <- resid(dt.model)
D <- merge(B,m,by.x=c("fy","domain"),by.y=c("fy","domain"),all.x=T)
D <- D[order(D$domain, D$fy),]
for (d in unique(D$domain)) {
  D$sig1n.dt[D$domain==d] <- D$res[B$domain==d]
  D$L.sig1n.dt[D$domain==d] <- lag(D$res[B$domain==d])
  D$L2.sig1n.dt[D$domain==d] <- lag(D$res[B$domain==d],2)
}
D$public <- D$pri
D$public_lag <- D$L.pri
D$media_lag <- D$L.sig1n.dt
D$policy_lag <- D$D.sp2std
modela5.1 <- lm(public ~ public_lag + media_lag + policy_lag + domain_text, data=D)
D$policy <- D$D.sp2std
D$public_lag <- D$L2.pri
D$media_lag <- D$L2.sig1n.dt
D$policy_lag <- D$LD.sp2std
modela5.2 <- lm(policy ~ public_lag + media_lag + policy_lag + domain_text, data=D)
D$media <- D$sig1n.dt
D$public_lag <- D$L.pri
D$media_lag <- D$L.sig1n.dt
D$policy_lag <- D$D.sp2std
modela5.3 <- lm(media ~ public_lag + media_lag + policy_lag + domain_text, data=D)
cm <- c("public_lag", "media_lag", "policy_lag", "(Intercept)")
modelsummary(list(model1, model2, model3),
             stars=T, 
             gof_omit="Log|IC|RMSE|F",
             coef_map = cm,
             output = "tableA5.rtf")

#Table A6.
B$public <- B$pri
B$public_lag <- B$L.pri
B$media_early <- B$sig1an
B$media_late <- B$L.sig1bn
B$policy_lag <- B$D.sp2std
modelA6.4 <- lm(public ~ public_lag + media_late + media_early + policy_lag + domain_text, data=B)
B$policy <- B$D.sp2std
B$public <- B$L.pri
B$media_early <- B$L.sig1an
B$media_late <- B$L.sig1bn
B$policy_lag <- B$LD.sp2std
modelA6.5 <- lm(policy ~ public + media_early + media_late + policy_lag + domain_text, data=B)
cm <- c("public", "public_lag", "media_early", "media_late",
        "policy", "policy_lag", "(Intercept)")
modelsummary(list(modelA6.4, modelA6.5),
             stars=T, 
             gof_omit="Log|IC|RMSE|F",
             coef_map = cm,
             output = "tableA6.rtf")

