##################################################
##################################################
# Replication Code for Abman and Lundberg (2019) #
#### "Does Free Trade Increase Deforestation" ####
##################################################
##################################################
library(gdata)
library(zoo)
library(stargazer)
library(data.table)
library(plm)

rm(list=ls())

############################################
############ Helper functions ##############
############################################

### inference on cumulative effects
cumulative_tests <- function(model,no_robust_se = F) {
  m <- 8
  k <- length(coef(model))	
  cum_eff <- cumsum(coef(model)[4:m])
  L <- matrix(rep(0,5*k),nrow=5,ncol=k)
  L[,4] <- 1
  L[-1,5] <- 1
  L[-(1:2),6] <- 1
  L[-(1:3),7] <- 1
  L[5, m] <- 1
  se_cum <- sqrt(diag(L%*%vcovHC(model, type="HC0",cluster="group")%*%t(L)))
  
  if (no_robust_se) {
    se_cum <- sqrt(diag(L%*%vcov(model)%*%t(L)))
  }
  
  names(se_cum) <- names(cum_eff)
  #p <- sapply(cum_eff/se_cum,function(x) round(1-pt(abs(x),df=1628),digits=3) )
  p <- sapply(abs(cum_eff/se_cum),function(x) round(2*pt(x,df=model$df,lower.tail=F),digits=3) )
  names(p) <- names(cum_eff)
  return(list(coef = cum_eff, se = se_cum, p_value = p))
}

### plots event study figure
plot_coef <- function(coef, se, plot_name, ylims, cum=F, lines=F, color = "black", angles=45, densities=20, alpha=.5) {
  if (cum) {
    coeff <- c(0,coef[-5])
    stderr <- c(0,se[-5])
    xx <- -1:3
  } else {
    coeff <- c(coef[2:3], 0, coef[4:7])
    stderr <- c(se[2:3], 0, se[4:7])
    xx <- -3:3
  }
  ci <- cbind(coeff-1.96*stderr, coeff + 1.96*stderr)
  
  if (lines) {
    lines(xx, coeff, lwd=2, col= color)
  } else {
  	if (cum) {
  		plot(xx, coeff,type='l',ylim=ylims, ylab="Cumulative",xlab="Time since RTA enactment", main= plot_name, lwd=2, col= color)
  	} else {plot(xx, coeff,type='l',ylim=ylims, ylab="Coefficient",xlab="Time since RTA enactment", main= plot_name, lwd=2, col= color)}
  }
  
  polygon(x = c(xx, rev(xx)), y = c(ci[,2],rev(ci[,1])) , border = NA, col=adjustcolor(color, alpha.f = alpha), angle=angles, density=densities)
  abline(h=0,lty=2,lwd=.5)
}

############################################
################ load  data ################
############################################

# choose trade_deforestation_DATA.csv from appropriate directory
data_file <- file.choose()
data_main <- data.table(read.csv(data_file, header=T))

# choose WTO_RTA_list_dates.xlsx from appropriate directory
data_file2 <- file.choose()
rta_timing <- data.table(read.xls(data_file2, header=T, stringsAsFactors=F) )

pdat <- pdata.frame(data_main[,],index=c("Country.Code", "Year"))

write_figs <- F

cat(rep("\n", 100))

############################################
################# DH Test ##################
############################################

model <- plm(rta~log(1+forest_loss),data=pdat, model="within",effect="twoways")

wald_stat <- function(reg,k) {
		b <- coef(reg)
		Sig <- vcov(reg)
		R <- matrix(0, nrow=k, ncol=2*k+1)
		R[,2:(k+1)] <- diag(1,k,k)	
		dims <- dim(reg$model)		
		ifelse(det(Sig)!=0 & sum(is.na(b))==0, return(b%*%t(R)%*%solve(R%*%(Sig)%*%t(R))%*%R%*%b), return(NA))
}

countries <- data_main[,unique(Country.Code)]
n <- length(countries)
TT <- length(data_main[,unique(Year)])
K <- 2
W_it <- rep(NA, n)
df_t <- rep(NA, n)
trend_t <- rep(NA,n)

for (i in 1:n) {
	country <- countries[i]
	W_it[i] <- data_main[ Country.Code == country ,suppressWarnings(wald_stat( lm(rta ~ shift(log(1 + forest_loss)) + shift(log(1 + forest_loss),2)+lag1+lag2 ) ,K)) ]	
}

N <- sum(!is.na(W_it))
W_NT <- mean(W_it,na.rm=T)
TT <- length(data_main[,unique(Year)])-K

cat(rep("\n", 5), "Deforestation: Dumitrescu-Hurlin Statistic \n", round(sqrt(N/(2*K)*(TT-2*K-5)/(TT-K-3))*((TT-2*K-3)/(TT-2*K-1)*W_NT -K ),3), rep("\n", 5))

############################################
######## TABLE 1 -- MAIN RESULTS ###########
############################################

reg_def <- plm(log(1+forest_loss) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")
se_def <- sqrt(diag(vcovHC(reg_def, type="HC0",cluster="group")))

reg_ag_gr <- plm(ag_area_gr ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")
se_ag_gr <- sqrt(diag(vcovHC(reg_ag_gr, type="HC0",cluster="group")))

cum_def <- cumulative_tests(reg_def)
cum_ag_gr <- cumulative_tests(reg_ag_gr)

reg_dummy <- plm(ag_area_gr ~ (rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")

cat(rep("\n",5), "Table 1 -- Main Results", rep("\n", 5))

stargazer(reg_def, reg_dummy, reg_ag_gr, reg_dummy, coef = list(reg_def$coef, c(rep(NA,4), cum_def$coef), reg_ag_gr$coef, c(rep(NA,4), cum_ag_gr$coef)), se=list(se_def,c(rep(NA,4), cum_def$se), se_ag_gr, c(rep(NA,4), cum_ag_gr$se)), title="Effects of Trade Shocks -- Full Sample", align=TRUE, dep.var.labels.include=F, column.labels=c("Deforestation", "Ag Area Growth"), covariate.labels=c("$RTA_{LR-}$","$RTA_{t-3}$", "$RTA_{t-2}$","$RTA_t$", "$RTA_{t+1}$", "$RTA_{t+2}$", "$RTA_{t+3}$", "$RTA_{LR+}$"),no.space=T, omit.stat=c("LL", "ser","f", "aic","bic", "adj.rsq"), digits=4)

R <- matrix(0,nrow=3, ncol=8)
diag(R)[1:3] <- 1

cat(rep("\n",5), "Wald Statistics on Leading Coefficients", rep("\n", 5))

cat("Wald statistic: deforestation leads jointly 0 \n", round(t(R%*%matrix(coef(reg_def),ncol=1))%*%solve(R%*%vcovHC(reg_def, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_def),ncol=1), 3))

cat("Wald statistic: ag area growth leads jointly 0 \n", round(t(R%*%matrix(coef(reg_ag_gr),ncol=1))%*%solve(R%*%vcovHC(reg_ag_gr, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_ag_gr),ncol=1), 3))

############################################
### TABLE 2 -- ALT DEFORESTATION MEAURES ###
############################################

reg_def <- plm(log(1+forest_loss) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")
se_def <- sqrt(diag(vcovHC(reg_def, type="HC0",cluster="group")))
cum_def <- cumulative_tests(reg_def)

reg_def_lev <- plm(I(forest_loss*.01) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")
se_def_lev <- sqrt(diag(vcovHC(reg_def_lev, type="HC0",cluster="group")))
cum_def_lev <- cumulative_tests(reg_def_lev)

reg_def_rate <- plm(defor_rate ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")
se_def_rate <- sqrt(diag(vcovHC(reg_def_rate, type="HC0",cluster="group")))
cum_def_rate <- cumulative_tests(reg_def_rate)

reg_dummy <- plm(log(1+forest_loss) ~ (rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")

cat(rep("\n",5), "Table 2 -- Alternative Deforestation Measures", rep("\n", 5))

stargazer(reg_def, reg_dummy, reg_def_lev, reg_dummy, reg_def_rate, reg_dummy, coef = list(reg_def$coef, c(rep(NA,4), cum_def$coef), reg_def_lev$coef, c(rep(NA,4), cum_def_lev$coef), reg_def_rate$coef, c(rep(NA,4), cum_def_rate$coef)), se=list(se_def,c(rep(NA,4), cum_def$se), se_def_lev,c(rep(NA,4), cum_def_lev$se), se_def_rate,c(rep(NA,4), cum_def_rate$se)), title="Effects of Trade Shocks -- Various Deforestation Measures", align=TRUE, dep.var.labels.include=F, column.labels=c("Log Deforestation", "Deforestation Level", "Deforestation Rate"), covariate.labels=c("$RTA_{LR-}$","$RTA_{t-3}$", "$RTA_{t-2}$","$RTA_t$", "$RTA_{t+1}$", "$RTA_{t+2}$", "$RTA_{t+3}$", "$RTA_{LR+}$"),no.space=T, omit.stat=c("LL", "ser","f", "aic","bic", "adj.rsq"),column.sep.width="-10pt", digits=4)

R <- matrix(0,nrow=3, ncol=8)
diag(R)[1:3] <- 1

cat(rep("\n",5), "Wald Statistics on Leading Coefficients", rep("\n", 5))

cat("Wald statistic: log deforestation leads jointly 0 \n", round(t(R%*%matrix(coef(reg_def),ncol=1))%*%solve(R%*%vcovHC(reg_def, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_def),ncol=1), 3))

cat("Wald statistic: deforestation level leads jointly 0 \n", round(t(R%*%matrix(coef(reg_def_lev),ncol=1))%*%solve(R%*%vcovHC(reg_def_lev, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_def_lev),ncol=1), 3))

cat("Wald statistic: deforestation rate leads jointly 0 \n", round(t(R%*%matrix(coef(reg_def_rate),ncol=1))%*%solve(R%*%vcovHC(reg_def_rate, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_def_rate),ncol=1), 3))

############################################
########### TABLE 3 -- CONTROLS ############
############################################

reg1 <- plm(log(1+forest_loss) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) +log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag), data=pdat, effect = "twoways", model="within")
k <- length(coef(reg1))
L <- matrix(rep(0,(k-7)*k),nrow=k-7,ncol=k)
L[1,4:7] <- 1
diag(L[,8:k]) <- 1
L[1,8] <- 0

coef1 <- L%*%coef(reg1)
se1 <- sqrt(diag(L%*%vcovHC(reg1, type="HC0",cluster="group")%*%t(L)))

# add population
reg2 <- plm(log(1+forest_loss) ~ (pre+lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+ log(pop), data=pdat, effect = "twoways", model="within")
k <- length(coef(reg2))
L <- matrix(rep(0,(k-7)*k),nrow=k-7,ncol=k)
L[1,4:7] <- 1
diag(L[,8:k]) <- 1
L[1,8] <- 0

coef2 <- L%*%coef(reg2)
se2 <- sqrt(diag(L%*%vcovHC(reg2, type="HC0",cluster="group")%*%t(L)))

# add gdp growth
reg3 <- plm(log(1+forest_loss) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+  log(pop) + gdp_gr, data=pdat, effect = "twoways", model="within")
k <- length(coef(reg3))
L <- matrix(rep(0,(k-7)*k),nrow=k-7,ncol=k)
L[1,4:7] <- 1
diag(L[,8:k]) <- 1
L[1,8] <- 0

coef3 <- L%*%coef(reg3)
se3 <- sqrt(diag(L%*%vcovHC(reg3, type="HC0",cluster="group")%*%t(L)))

# add openness
reg4 <- plm(log(1+forest_loss) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2)+ log(gdp_pc_lag) +log(pop) + gdp_gr + log(I(exports_gdp + imports_gdp)), data=pdat, effect = "twoways", model="within")
k <- length(coef(reg4))
L <- matrix(rep(0,(k-7)*k),nrow=k-7,ncol=k)
L[1,4:7] <- 1
diag(L[,8:k]) <- 1
L[1,8] <- 0

coef4 <- L%*%coef(reg4)
se4 <- sqrt(diag(L%*%vcovHC(reg4, type="HC0",cluster="group")%*%t(L)))

# various FE specifications
reg5 <- plm(log(1+forest_loss) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2)+ log(gdp_pc_lag) +log(pop) + gdp_gr+ log(I(exports_gdp + imports_gdp)) + imf_adv*Year, data=pdat, effect = "individual", model="within")
k <- length(coef(reg5))
L <- matrix(rep(0,7*k),nrow=7,ncol=k)
L[1,4:7] <- 1
diag(L[,8:k]) <- 1
L[1,8] <- 0

coef5 <- L%*%coef(reg5)
se5 <- sqrt(diag(L%*%vcovHC(reg5, type="HC0",cluster="group")%*%t(L)))

reg6 <- plm(log(1+forest_loss)~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post)+ log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+log(pop) + gdp_gr +  log(I(exports_gdp + imports_gdp)) + Tropical_any*Year, data=pdat, effect = "individual", model="within")
k <- length(coef(reg6))
L <- matrix(rep(0,7*k),nrow=7,ncol=k)
L[1,4:7] <- 1
diag(L[,8:k]) <- 1
L[1,8] <- 0

coef6 <- L%*%coef(reg6)
se6 <- sqrt(diag(L%*%vcovHC(reg6, type="HC0",cluster="group")%*%t(L)))

reg7 <- plm(log(1+forest_loss) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2)+ log(gdp_pc_lag) +log(pop) + gdp_gr + log(I(exports_gdp + imports_gdp)) + nx*Year, data=pdat, effect = "individual", model="within")
k <- length(coef(reg7))
L <- matrix(rep(0,7*k),nrow=7,ncol=k)
L[1,4:7] <- 1
diag(L[,8:k]) <- 1
L[1,8] <- 0

coef7 <- L%*%coef(reg7)
se7 <- sqrt(diag(L%*%vcovHC(reg7, type="HC0",cluster="group")%*%t(L)))

# governance
reg8 <- plm(log(1+forest_loss) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2)+ log(gdp_pc_lag) +log(pop) + gdp_gr +  log(I(exports_gdp + imports_gdp)) + cc+ va + rl + rq + pv, data=pdat, effect = "individual", model="within")
k <- length(coef(reg8))
L <- matrix(rep(0,(k-7)*k),nrow=k-7,ncol=k)
L[1,4:7] <- 1
diag(L[,8:k]) <- 1
L[1,8] <- 0

coef8 <- L%*%coef(reg8)
se8 <- sqrt(diag(L%*%vcovHC(reg8, type="HC0",cluster="group")%*%t(L)))

# cumulative effects table
reg_dummy <-  plm(log(1+forest_loss) ~  post + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+log(pop) + gdp_gr  + log(I(exports_gdp + imports_gdp)) + cc+ va + rl + rq + pv, data=pdat, effect = "twoways", model="within")

cat(rep("\n",5), "Table 3 -- Controls", rep("\n", 5))

stargazer(reg_dummy,reg_dummy,reg_dummy,reg_dummy,reg_dummy,reg_dummy,reg_dummy,reg_dummy, coef=list(coef1,coef2,coef3,coef4,coef5,coef6,coef7,coef8), se=list(se1,se2,se3,se4,se5,se6,se7,se8), title="Effects of Trade Shocks", align=TRUE, dep.var.labels.include=F, no.space=T, covariate.labels=c("RTA Cumulative", "Per Capita GDP", "(Per Capita GDP)$^2$", "Per Capita GDP (lag)", "Population", "GDP Growth", "Openness", "Corruption","Accountability"," Rule of Law", "Regulatory", "Stability"), omit.stat=c("LL", "ser","f", "aic","bic", "adj.rsq"))

cat(rep("\n",5), "Correct Sample Sizes and R2 by Model -- ignore remainder of table below", rep("\n", 5))

stargazer(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,keep=c("none"),align=T,no.space=T, omit.stat=c("LL", "ser","f", "aic","bic", "adj.rsq"))

############################################
######### TABLE 4 -- PRODUCTION ############
############################################

reg_for<- plm(log(1+forest_out_m3) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+log(pop) + gdp_gr  + log(I(exports_gdp + imports_gdp)) + cc+ va + rl + rq + pv,, data=pdat, effect = "twoways", model="within")
se_for <- sqrt(diag(vcovHC(reg_for, type="HC0",cluster="group")))

reg_ag_ton <- plm(log(1+ag_out_ton) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+log(pop) + gdp_gr  + log(I(exports_gdp + imports_gdp)) + cc+ va + rl + rq + pv,, data=pdat, effect = "twoways", model="within")
se_ag_ton <- sqrt(diag(vcovHC(reg_ag_ton, type="HC0",cluster="group")))

reg_ag_ha <- plm(log(1+ha_out) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+log(pop) + gdp_gr  + log(I(exports_gdp + imports_gdp)) + cc+ va + rl + rq + pv,, data=pdat, effect = "twoways", model="within")
se_ag_ha <- sqrt(diag(vcovHC(reg_ag_ha, type="HC0",cluster="group")))

cum_for <- cumulative_tests(reg_for)
cum_ag_ha <- cumulative_tests(reg_ag_ha)
cum_ag_ton <- cumulative_tests(reg_ag_ton)

reg_dummy <- plm(log(1+forest_out_m3) ~ ( rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")

cat(rep("\n",5), "Table 4 -- Production", rep("\n", 5))

stargazer(reg_for, reg_dummy, reg_ag_ha, reg_dummy, reg_ag_ton, reg_dummy, coef = list(reg_for$coef, c(rep(NA,4), cum_for$coef), reg_ag_ha$coef, c(rep(NA,4), cum_ag_ha$coef), reg_ag_ton$coef, c(rep(NA,4), cum_ag_ton$coef)), se=list(se_for,c(rep(NA,4), cum_for$se), se_ag_ha, c(rep(NA,4), cum_ag_ha$se),se_ag_ton, c(rep(NA,4), cum_ag_ton$se)), align=TRUE, dep.var.labels.include=F, covariate.labels=c("$RTA_{LR-}$","$RTA_{t-3}$", "$RTA_{t-2}$","$RTA_t$", "$RTA_{t+1}$", "$RTA_{t+2}$", "$RTA_{t+3}$", "$RTA_{LR+}$"), keep = c("pre", "lead3", "lead2", "rta", "lag1", "lag2", "lag3", "post"), no.space=T, omit.stat=c("LL", "ser","f", "aic","bic", "adj.rsq"))

R <- matrix(0,nrow=3, ncol=length(coef(reg_for)))
diag(R)[1:3] <- 1

cat(rep("\n",5), "Wald Statistics on Leading Coefficients", rep("\n", 5))

cat("Wald statistic: forest output leads jointly 0 \n", round(t(R%*%matrix(coef(reg_for),ncol=1))%*%solve(R%*%vcovHC(reg_for, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_for),ncol=1), 3))

cat("Wald statistic: ag harvest area leads jointly 0 \n", round(t(R%*%matrix(coef(reg_ag_ha),ncol=1))%*%solve(R%*%vcovHC(reg_ag_ha, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_ag_ha),ncol=1), 3))

cat("Wald statistic: ag harvest weight leads jointly 0 \n", round(t(R%*%matrix(coef(reg_ag_ton),ncol=1))%*%solve(R%*%vcovHC(reg_ag_ton, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_ag_ton),ncol=1), 3))

############################################
######### TABLE 5 -- SUBSAMPLE #############
############################################

reg_def <- plm(log(1+forest_loss) ~ dev_tropic*dev_non*(pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+log(pop) + gdp_gr  + log(I(exports_gdp + imports_gdp)) + cc+ va + rl + rq + pv+ dev_tropic*dev_non*Year, data=pdat, effect = "twoways", model="within")
se_def <- sqrt(diag(vcovHC(reg_def, type="HC0",cluster="group")))
k <- length(coef(reg_def))
L <- matrix(rep(0,24*k),nrow=24,ncol=k)
L[1:8,1:8] <- diag(1,nrow=8)
L[9:16, 1:8] <- diag(1,nrow=8)
L[17:24,1:8] <- diag(1,nrow=8)
L[9:16,20:27] <- diag(1,nrow=8)
L[17:24,28:35] <- diag(1,nrow=8)

ses <- sqrt(diag(L%*%vcovHC(reg_def, type="HC0",cluster="group")%*%t(L)))

coef_adv <- (L%*%coef(reg_def))[1:8]
coef_dev_trop <- (L%*%coef(reg_def))[9:16]
coef_dev_non <- (L%*%coef(reg_def))[17:24]

se_adv <- ses[1:8]
se_dev_trop  <- ses[9:16]
se_dev_non <- ses[17:24]

reg_ag <- plm(ag_area_gr ~ dev_tropic*dev_non*(pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+log(pop) + gdp_gr  + log(I(exports_gdp + imports_gdp)) + cc+ va + rl + rq + pv+ dev_tropic*dev_non*Year, data=pdat, effect = "twoways", model="within")
se_ag <- sqrt(diag(vcovHC(reg_ag, type="HC0",cluster="group")))
k <- length(coef(reg_ag))
L <- matrix(rep(0,24*k),nrow=24,ncol=k)
L[1:8,1:8] <- diag(1,nrow=8)
L[9:16, 1:8] <- diag(1,nrow=8)
L[17:24,1:8] <- diag(1,nrow=8)
L[9:16,20:27] <- diag(1,nrow=8)
L[17:24,28:35] <- diag(1,nrow=8)

ses2 <- sqrt(diag(L%*%vcovHC(reg_ag, type="HC0",cluster="group")%*%t(L)))

coef_adv2 <- (L%*%coef(reg_ag))[1:8]
coef_dev_trop2 <- (L%*%coef(reg_ag))[9:16]
coef_dev_non2 <- (L%*%coef(reg_ag))[17:24]

se_adv2 <- ses2[1:8]
se_dev_trop2  <- ses2[9:16]
se_dev_non2 <- ses2[17:24]

cat(rep("\n",5), "Table 5 -- Subsample Analysis (point estimates)", rep("\n", 5))

options(scipen = 999)
write.table( format( cbind( c("RTA_{LR-}", "", "RTA_{t-3}", "", "RTA_{t-2}", "", "RTA_{t}", "",  "RTA_{t+1}", "",  "RTA_{t+2}", "",  "RTA_{t+3}",  "", "RTA_{LR+}", ""), rep("&",16) , as.vector(rbind(round(coef_adv,3), unlist(lapply(se_adv, function(x) paste("(",round(x,3), ")" )))  )), rep("&", 16), as.vector(rbind(round(coef_adv2,4),unlist(lapply(se_adv2, function(x) paste("(",round(x,4), ")" )))  )), rep("&", 16), as.vector(rbind(round(coef_dev_trop,3),unlist(lapply(se_dev_trop, function(x) paste("(",round(x,3), ")" )))  )), rep("&", 16), as.vector(rbind(round(coef_dev_trop2,4),unlist(lapply(se_dev_trop2, function(x) paste("(",round(x,4), ")" )))  )), rep("&", 16), as.vector(rbind(round(coef_dev_non,3),unlist(lapply(se_dev_non, function(x) paste("(",round(x,3), ")" )))  )), rep("&", 16), as.vector(rbind(round(coef_dev_non2,4),unlist(lapply(se_dev_non2, function(x) paste("(",round(x,4), ")" ) ))  )), rep("\\\\",16) ),format=F )  , row.names=F,col.names=F, quote=F,sep=" ")

cat(rep("\n",5), "Table 5 -- Inference (coef, t-stats)", rep("\n", 5))

cat("Developed deforestation coefficients \n", round(coef_adv,3), "\n", "Developed deforestation t-stats \n", coef_adv/se_adv, rep("\n",5))
cat("Developed ag gr coefficients \n", round(coef_adv2,4), "\n", "Developed ag gr t-stats \n", coef_adv2/se_adv2, rep("\n",5))
cat("Tropical developing deforestation coefficients \n", round(coef_dev_trop,3), "\n", "Tropical developing deforestation t-stats \n", coef_dev_trop/se_dev_trop, rep("\n",5))
cat("Tropical developing ag area gr coefficients \n", round(coef_dev_trop2,4), "\n", "Tropical developing ag area gr t-stats \n", coef_dev_trop2/se_dev_trop2, rep("\n",5))
cat("Nontropical developing deforestation coefficients \n", round(coef_dev_non,3), "\n", "Nontropical developing deforestation t-stats \n", coef_dev_non/se_dev_non, rep("\n",5))
cat("Nontropical developing ag area gr coefficients \n", round(coef_dev_non2,4), "\n", "Nontropical developing ag area gr t-stats \n", coef_dev_non2/se_dev_non2, rep("\n",5))

cat(rep("\n",5), "Wald Statistics on Leading Coefficients", rep("\n", 5))

R <- matrix(0,nrow=3, ncol=k)
diag(R)[1:3] <- 1

cat("Wald statistic: Developed deforestation leads jointly 0 \n", round(t(R%*%matrix(coef(reg_def),ncol=1))%*%solve(R%*%vcovHC(reg_def, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_def),ncol=1), 3), "\n")
cat("Wald statistic: Developed ag area gr leads jointly 0 \n", round(t(R%*%matrix(coef(reg_ag),ncol=1))%*%solve(R%*%vcovHC(reg_ag, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_ag),ncol=1), 3), "\n")

R <- matrix(0,nrow=3, ncol=k)
diag(R)[1:3] <- 1
diag(R[,20:22]) <- 1

cat("Wald statistic: Tropical developing deforestation leads jointly 0 \n", round(t(R%*%matrix(coef(reg_def),ncol=1))%*%solve(R%*%vcovHC(reg_def, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_def),ncol=1), 3), "\n")
cat("Wald statistic: Tropical developing ag area gr leads jointly 0 \n", round(t(R%*%matrix(coef(reg_ag),ncol=1))%*%solve(R%*%vcovHC(reg_ag, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_ag),ncol=1), 3), "\n")

R <- matrix(0,nrow=3, ncol=k)
diag(R)[1:3] <- 1
diag(R[,28:30]) <- 1

cat("Wald statistic: Nontropical developing deforestation leads jointly 0 \n", round(t(R%*%matrix(coef(reg_def),ncol=1))%*%solve(R%*%vcovHC(reg_def, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_def),ncol=1), 3), "\n")
cat("Wald statistic: Nontropical developing ag area gr leads jointly 0 \n", round(t(R%*%matrix(coef(reg_ag),ncol=1))%*%solve(R%*%vcovHC(reg_ag, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_ag),ncol=1), 3), "\n")

############################################
# TABLE 6 -- SUBSAMPLE CUMULATIVE EFFECTS ##
############################################

k <- length(coef(reg_def))
L <- matrix(rep(0,15*k),nrow=15,ncol=k)
L[1:5,4] <- 1
L[2:5,5] <- 1
L[3:5,6] <- 1
L[4:5,7] <- 1
L[5,8] <- 1
L[6:10, 4:8] <- L[1:5,4:8]
L[11:15,4:8] <- L[1:5,4:8]

L[6:10, 23:27] <- L[1:5,4:8]
L[11:15,31:35] <- L[1:5,4:8]


ses <- sqrt(diag(L%*%vcovHC(reg_def, type="HC0",cluster="group")%*%t(L)))

coef_adv <- (L%*%coef(reg_def))[1:5]
coef_dev_trop <- (L%*%coef(reg_def))[6:10]
coef_dev_non <- (L%*%coef(reg_def))[11:15]

se_adv <- ses[1:5]
se_dev_trop  <- ses[6:10]
se_dev_non <- ses[11:15]

ses2 <- sqrt(diag(L%*%vcovHC(reg_ag, type="HC0",cluster="group")%*%t(L)))

coef_adv2 <- (L%*%coef(reg_ag))[1:5]
coef_dev_trop2 <- (L%*%coef(reg_ag))[6:10]
coef_dev_non2 <- (L%*%coef(reg_ag))[11:15]

se_adv2 <- ses2[1:5]
se_dev_trop2  <- ses2[6:10]
se_dev_non2 <- ses2[11:15]

cat(rep("\n",5), "Table 6 -- Subsample Cumulative Effects (point estimates)", rep("\n", 5))

write.table( format( cbind( c("RTA_{t}", "",  "RTA_{t+1}", "",  "RTA_{t+2}", "",  "RTA_{t+3}",  "", "RTA_{LR+}", ""), rep("&",5) , as.vector(rbind(round(coef_adv,3), unlist(lapply(se_adv, function(x) paste("(",round(x,3), ")" )))  )), rep("&", 5), as.vector(rbind(round(coef_adv2,4),unlist(lapply(se_adv2, function(x) paste("(",round(x,4), ")" )))  )), rep("&", 5), as.vector(rbind(round(coef_dev_trop,3),unlist(lapply(se_dev_trop, function(x) paste("(",round(x,3), ")" )))  )), rep("&",5), as.vector(rbind(round(coef_dev_trop2,4),unlist(lapply(se_dev_trop2, function(x) paste("(",round(x,4), ")" )))  )), rep("&", 5), as.vector(rbind(round(coef_dev_non,3),unlist(lapply(se_dev_non, function(x) paste("(",round(x,3), ")" )))  )), rep("&",5), as.vector(rbind(round(coef_dev_non2,4),unlist(lapply(se_dev_non2, function(x) paste("(",round(x,4), ")" ) ))  )), rep("\\\\",5) ),format=F )  , row.names=F,col.names=F, quote=F,sep=" ")

cat(rep("\n",5), "Table 6 -- Inference (coef, t-stats)", rep("\n", 5))

cat("Developed deforestation coefficients \n", round(coef_adv,3), "\n", "Developed deforestation t-stats \n", coef_adv/se_adv, rep("\n",5))
cat("Developed ag gr coefficients \n", round(coef_adv2,4), "\n", "Developed ag gr t-stats \n", coef_adv2/se_adv2, rep("\n",5))
cat("Tropical developing deforestation coefficients \n", round(coef_dev_trop,3), "\n", "Tropical developing deforestation t-stats \n", coef_dev_trop/se_dev_trop, rep("\n",5))
cat("Tropical developing ag area gr coefficients \n", round(coef_dev_trop2,4), "\n", "Tropical developing ag area gr t-stats \n", coef_dev_trop2/se_dev_trop2, rep("\n",5))
cat("Nontropical developing deforestation coefficients \n", round(coef_dev_non,3), "\n", "Nontropical developing deforestation t-stats \n", coef_dev_non/se_dev_non, rep("\n",5))
cat("Nontropical developing ag area gr coefficients \n", round(coef_dev_non2,4), "\n", "Nontropical developing ag area gr t-stats \n", coef_dev_non2/se_dev_non2, rep("\n",5))

############################################
## TABLE 7 -- SUBSAMPLE BY FOOD EXPORTER ###
############################################

reg_yield <- plm(log(1+yield_weighted) ~ Tropical_any*food_exporter*(pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+log(pop) + gdp_gr  + log(I(exports_gdp + imports_gdp)) + cc+ va + rl + rq + pv + Tropical_any*food_exporter*Year, data=pdat, effect = "twoways", model="within")
se_yield <- sqrt(diag(vcovHC(reg_yield, type="HC0",cluster="group")))
k <- length(coef(reg_yield))
L <- matrix(rep(0,32*k),nrow=32,ncol=k)
L[1:8,1:8] <- diag(1,nrow=8)
L[9:16, 1:8] <- diag(1,nrow=8)
L[17:24,1:8] <- diag(1,nrow=8)
L[25:32,1:8] <- diag(1,nrow=8)
L[9:16,20:27] <- diag(1,nrow=8)
L[17:24,28:35] <- diag(1,nrow=8)
L[25:32, 58:65] <- diag(1,nrow=8)

ses <- sqrt(diag(L%*%vcovHC(reg_yield, type="HC0",cluster="group")%*%t(L)))

coef_non_imp <- (L%*%coef(reg_yield))[1:8]
coef_trop_imp <- (L%*%coef(reg_yield))[9:16]
coef_non_exp <- (L%*%coef(reg_yield))[17:24]
coef_trop_exp <- (L%*%coef(reg_yield))[25:32]

se_non_imp <- ses[1:8]
se_trop_imp  <- ses[9:16]
se_non_exp  <- ses[17:24]
se_trop_exp  <- ses[25:32]

reg_def <- plm(log(1+forest_loss) ~ Tropical_any*food_exporter*(pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+log(pop) + gdp_gr  + log(I(exports_gdp + imports_gdp)) + cc+ va + rl + rq + pv + Tropical_any*food_exporter*Year, data=pdat, effect = "twoways", model="within")
se_def <- sqrt(diag(vcovHC(reg_def, type="HC0",cluster="group")))
k <- length(coef(reg_def))
L <- matrix(rep(0,32*k),nrow=32,ncol=k)
L[1:8,1:8] <- diag(1,nrow=8)
L[9:16, 1:8] <- diag(1,nrow=8)
L[17:24,1:8] <- diag(1,nrow=8)
L[25:32,1:8] <- diag(1,nrow=8)
L[9:16,20:27] <- diag(1,nrow=8)
L[17:24,28:35] <- diag(1,nrow=8)
L[25:32, 58:65] <- diag(1,nrow=8)

ses2 <- sqrt(diag(L%*%vcovHC(reg_def, type="HC0",cluster="group")%*%t(L)))

coef_non_imp2 <- (L%*%coef(reg_def))[1:8]
coef_trop_imp2 <- (L%*%coef(reg_def))[9:16]
coef_non_exp2 <- (L%*%coef(reg_def))[17:24]
coef_trop_exp2 <- (L%*%coef(reg_def))[25:32]

se_non_imp2 <- ses2[1:8]
se_trop_imp2  <- ses2[9:16]
se_non_exp2  <- ses2[17:24]
se_trop_exp2  <- ses2[25:32]

reg_ag <- plm(ag_area_gr ~ Tropical_any*food_exporter*(pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+log(pop) + gdp_gr  + log(I(exports_gdp + imports_gdp)) + cc+ va + rl + rq + pv + Tropical_any*food_exporter*Year, data=pdat, effect = "twoways", model="within")
se_ag <- sqrt(diag(vcovHC(reg_ag, type="HC0",cluster="group")))
k <- length(coef(reg_ag))
L <- matrix(rep(0,32*k),nrow=32,ncol=k)
L[1:8,1:8] <- diag(1,nrow=8)
L[9:16, 1:8] <- diag(1,nrow=8)
L[17:24,1:8] <- diag(1,nrow=8)
L[25:32,1:8] <- diag(1,nrow=8)
L[9:16,20:27] <- diag(1,nrow=8)
L[17:24,28:35] <- diag(1,nrow=8)
L[25:32, 58:65] <- diag(1,nrow=8)

ses3 <- sqrt(diag(L%*%vcovHC(reg_ag, type="HC0",cluster="group")%*%t(L)))

coef_non_imp3 <- (L%*%coef(reg_ag))[1:8]
coef_trop_imp3 <- (L%*%coef(reg_ag))[9:16]
coef_non_exp3 <- (L%*%coef(reg_ag))[17:24]
coef_trop_exp3 <- (L%*%coef(reg_ag))[25:32]

se_non_imp3 <- ses3[1:8]
se_trop_imp3  <- ses3[9:16]
se_non_exp3  <- ses3[17:24]
se_trop_exp3  <- ses3[25:32]

reg_trac <- plm(log(1+ag_mach_im) ~ Tropical_any*food_exporter*(pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+log(pop) + gdp_gr  + log(I(exports_gdp + imports_gdp)) + cc+ va + rl + rq + pv + Tropical_any*food_exporter*Year, data=pdat, effect = "twoways", model="within")
se_trac <- sqrt(diag(vcovHC(reg_trac, type="HC0",cluster="group")))
k <- length(coef(reg_trac))
L <- matrix(rep(0,32*k),nrow=32,ncol=k)
L[1:8,1:8] <- diag(1,nrow=8)
L[9:16, 1:8] <- diag(1,nrow=8)
L[17:24,1:8] <- diag(1,nrow=8)
L[25:32,1:8] <- diag(1,nrow=8)
L[9:16,20:27] <- diag(1,nrow=8)
L[17:24,28:35] <- diag(1,nrow=8)
L[25:32, 58:65] <- diag(1,nrow=8)

ses4 <- sqrt(diag(L%*%vcovHC(reg_trac, type="HC0",cluster="group")%*%t(L)))

coef_non_imp4 <- (L%*%coef(reg_trac))[1:8]
coef_trop_imp4 <- (L%*%coef(reg_trac))[9:16]
coef_non_exp4 <- (L%*%coef(reg_trac))[17:24]
coef_trop_exp4 <- (L%*%coef(reg_trac))[25:32]

se_non_imp4 <- ses4[1:8]
se_trop_imp4  <- ses4[9:16]
se_non_exp4  <- ses4[17:24]
se_trop_exp4  <- ses4[25:32]

reg_foodx <- plm(log(1+food_exports) ~ Tropical_any*food_exporter*(pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+log(pop) + gdp_gr  + log(I(exports_gdp + imports_gdp)) + cc+ va + rl + rq + pv + Tropical_any*food_exporter*Year, data=pdat, effect = "twoways", model="within")
se_foodx <- sqrt(diag(vcovHC(reg_foodx, type="HC0",cluster="group")))
k <- length(coef(reg_foodx))
L <- matrix(rep(0,32*k),nrow=32,ncol=k)
L[1:8,1:8] <- diag(1,nrow=8)
L[9:16, 1:8] <- diag(1,nrow=8)
L[17:24,1:8] <- diag(1,nrow=8)
L[25:32,1:8] <- diag(1,nrow=8)
L[9:16,20:27] <- diag(1,nrow=8)
L[17:24,28:35] <- diag(1,nrow=8)
L[25:32, 58:65] <- diag(1,nrow=8)

ses5 <- sqrt(diag(L%*%vcovHC(reg_foodx, type="HC0",cluster="group")%*%t(L)))

coef_non_imp5 <- (L%*%coef(reg_foodx))[1:8]
coef_trop_imp5 <- (L%*%coef(reg_foodx))[9:16]
coef_non_exp5 <- (L%*%coef(reg_foodx))[17:24]
coef_trop_exp5 <- (L%*%coef(reg_foodx))[25:32]

se_non_imp5 <- ses5[1:8]
se_trop_imp5  <- ses5[9:16]
se_non_exp5 <- ses5[17:24]
se_trop_exp5  <- ses5[25:32]

reg_foodm <- plm(log(1+food_imports) ~ Tropical_any*food_exporter*(pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post) + log(gdp_pc) + I(log(gdp_pc)^2) + log(gdp_pc_lag)+log(pop) + gdp_gr  + log(I(exports_gdp + imports_gdp)) + cc+ va + rl + rq + pv + Tropical_any*food_exporter*Year, data=pdat, effect = "twoways", model="within")
se_foodm <- sqrt(diag(vcovHC(reg_foodm, type="HC0",cluster="group")))
k <- length(coef(reg_foodm))
L <- matrix(rep(0,32*k),nrow=32,ncol=k)
L[1:8,1:8] <- diag(1,nrow=8)
L[9:16, 1:8] <- diag(1,nrow=8)
L[17:24,1:8] <- diag(1,nrow=8)
L[25:32,1:8] <- diag(1,nrow=8)
L[9:16,20:27] <- diag(1,nrow=8)
L[17:24,28:35] <- diag(1,nrow=8)
L[25:32, 58:65] <- diag(1,nrow=8)

ses6 <- sqrt(diag(L%*%vcovHC(reg_foodm, type="HC0",cluster="group")%*%t(L)))

coef_non_imp6 <- (L%*%coef(reg_foodm))[1:8]
coef_trop_imp6 <- (L%*%coef(reg_foodm))[9:16]
coef_non_exp6 <- (L%*%coef(reg_foodm))[17:24]
coef_trop_exp6 <- (L%*%coef(reg_foodm))[25:32]

se_non_imp6 <- ses6[1:8]
se_trop_imp6  <- ses6[9:16]
se_non_exp6  <- ses6[17:24]
se_trop_exp6  <- ses6[25:32]

options(scipen = 999)
#food importers
def <- as.vector( rbind( round(coef_trop_imp2,3), unlist(lapply(se_trop_imp2, function(x) paste("(",round(x,3), ")" )))  )    )   
ag <- as.vector( rbind( round(coef_trop_imp3,4), unlist(lapply(se_trop_imp3, function(x) paste("(",round(x,4), ")" )))  )    ) 
yield <- as.vector( rbind( round(coef_trop_imp,3), unlist(lapply(se_trop_imp, function(x) paste("(",round(x,3), ")" )))  )    )
tractor <- as.vector( rbind( round(coef_trop_imp4,3), unlist(lapply(se_trop_imp4, function(x) paste("(",round(x,3), ")" )))  )    )
exp <- as.vector( rbind( round(coef_trop_imp5,3), unlist(lapply(se_trop_imp5, function(x) paste("(",round(x,3), ")" )))  )    )
imp <- as.vector( rbind( round(coef_trop_imp6,3), unlist(lapply(se_trop_imp6, function(x) paste("(",round(x,3), ")" )))  )    )

cat(rep("\n",5), "Table 7 -- Food Importers (point estimates)", rep("\n", 5))

write.table(cbind( c("RTA_{LR-}", "", "RTA_{t-3}", "", "RTA_{t-2}", "", "RTA_{t}", "",  "RTA_{t+1}", "",  "RTA_{t+2}", "",  "RTA_{t+3}",  "", "RTA_{LR+}", ""), rep("&", 8), def, rep("&", 8), ag, rep("&", 8), yield, rep("&", 8), tractor, rep("&", 8), exp, rep("&", 8), imp,  rep("\\\\",8)   ),  row.names=F,col.names=F, quote=F)

cat(rep("\n",5), "Table 7 -- Food Importers inference (coef, t-stats)", rep("\n", 5))

cat("Food importer deforestation coefficients \n", round(coef_trop_imp2,3), "\n", "Food importer deforestation t-stats \n", round(coef_trop_imp2/se_trop_imp2,3), rep("\n",5))
cat("Food importer ag area gr coefficients \n", round(coef_trop_imp3,4), "\n", "Food importer ag area gr t-stats \n", round(coef_trop_imp3/se_trop_imp3,3), rep("\n",5))
cat("Food importer yields coefficients \n", round(coef_trop_imp,3), "\n", "Food importer yields t-stats \n", round(coef_trop_imp/se_trop_imp,3), rep("\n",5))
cat("Food importer tractor imports coefficients \n", round(coef_trop_imp4,3), "\n", "Food importer tractor imports t-stats \n", round(coef_trop_imp4/se_trop_imp4,3), rep("\n",5))
cat("Food importer food exports coefficients \n", round(coef_trop_imp5,3), "\n", "Food importer food exports t-stats \n", round(coef_trop_imp5/se_trop_imp5,3), rep("\n",5))
cat("Food importer food imports coefficients \n", round(coef_trop_imp6,3), "\n", "Food importer food imports t-stats \n", round(coef_trop_imp6/se_trop_imp6,3), rep("\n",5))

def <- as.vector( rbind( round(coef_trop_exp2,3), unlist(lapply(se_trop_exp2, function(x) paste("(",round(x,3), ")" )))  )    )   
ag <- as.vector( rbind( round(coef_trop_exp3,4), unlist(lapply(se_trop_exp3, function(x) paste("(",round(x,4), ")" )))  )    ) 
yield <- as.vector( rbind( round(coef_trop_exp,3), unlist(lapply(se_trop_exp, function(x) paste("(",round(x,3), ")" )))  )    )
tractor <- as.vector( rbind( round(coef_trop_exp4,3), unlist(lapply(se_trop_exp4, function(x) paste("(",round(x,3), ")" )))  )    )
exp <- as.vector( rbind( round(coef_trop_exp5,3), unlist(lapply(se_trop_exp5, function(x) paste("(",round(x,3), ")" )))  )    )
imp <- as.vector( rbind( round(coef_trop_exp6,3), unlist(lapply(se_trop_exp6, function(x) paste("(",round(x,3), ")" )))  )    )

cat(rep("\n",5), "Table 7 -- Food Exporters (point estimates)", rep("\n", 5))

write.table(cbind( c("RTA_{LR-}", "", "RTA_{t-3}", "", "RTA_{t-2}", "", "RTA_{t}", "",  "RTA_{t+1}", "",  "RTA_{t+2}", "",  "RTA_{t+3}",  "", "RTA_{LR+}", ""), rep("&", 8), def, rep("&", 8), ag, rep("&", 8), yield, rep("&", 8), tractor, rep("&", 8), exp, rep("&", 8), imp,  rep("\\\\",8) ) , row.names=F, col.names=F, quote=F)

cat(rep("\n",5), "Table 7 -- Food Exporters inference (coef, t-stats)", rep("\n", 5))

cat("Food importer deforestation coefficients \n", round(coef_trop_exp2,3), "\n", "Food importer deforestation t-stats \n", round(coef_trop_exp2/se_trop_exp2,3), rep("\n",5))
cat("Food importer ag area gr coefficients \n", round(coef_trop_exp3,4), "\n", "Food importer ag area gr t-stats \n", round(coef_trop_exp3/se_trop_exp3,3), rep("\n",5))
cat("Food importer yields coefficients \n", round(coef_trop_exp,3), "\n", "Food importer yields t-stats \n", round(coef_trop_exp/se_trop_exp,3), rep("\n",5))
cat("Food importer tractor imports coefficients \n", round(coef_trop_exp4,3), "\n", "Food importer tractor imports t-stats \n", round(coef_trop_exp4/se_trop_exp4,3), rep("\n",5))
cat("Food importer food exports coefficients \n", round(coef_trop_exp5,3), "\n", "Food importer food exports t-stats \n", round(coef_trop_exp5/se_trop_exp5,3), rep("\n",5))
cat("Food importer food imports coefficients \n", round(coef_trop_exp6,3), "\n", "Food importer food imports t-stats \n", round(coef_trop_exp6/se_trop_exp6,3), rep("\n",5))

############################################
### FIGURE 1 -- RTA Enactment and Timing ###
############################################

par(mfrow=c(1,1))
rta_timing[,Year:=year(as.Date(Date.of.entry.into.force))]
rta_timing[,delay_months:=12*(as.yearmon(as.Date(Date.of.entry.into.force)) -as.yearmon(as.Date(Signature.Date)))]
plot(rta_timing[,.N, keyby=Year][Year>2000 & Year < 2015,], type='l', ylim=c(0,21), ylab="Number of RTAs")
par(new=T)
plot(rta_timing[,mean(delay_months), keyby=Year][Year>2000 & Year < 2015,], type='l', ylim=c(0,40),lty=2,xlab=NA, ylab=NA, axes=F)
par(new=T)
plot(rta_timing[,median(delay_months), keyby=Year][Year>2000 & Year < 2015,], type='l', ylim=c(0,40), lty=3,xlab=NA, ylab=NA, axes=F)
axis(side=4)


############################################
###### FIGURE 3 -- EVENT STUDY PLOTS #######
############################################

reg_def <- plm(log(1+forest_loss) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")
se_def <- sqrt(diag(vcovHC(reg_def, type="HC0",cluster="group")))
cum_def <- cumulative_tests(reg_def)

reg_ag_gr <- plm(ag_area_gr ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")
se_ag_gr <- sqrt(diag(vcovHC(reg_ag_gr, type="HC0",cluster="group")))
cum_ag_gr <- cumulative_tests(reg_ag_gr)

par(mfrow=c(2,1))
plot_coef(reg_def$coef, se_def, "Deforestation", c(-.15,.15),cum=F, angles=45, densities=30, alpha=.6)
plot_coef(cum_def$coef, cum_def$se, "Cumulative Deforestation", c(-.10,.45),cum=T, angles=-45, densities=20, alpha=.6)

if (write_figs) {
  dev.copy(pdf, "~/FIGURE_deforestation.pdf")
  dev.off()
}

par(mfrow=c(2,1))
plot_coef(reg_ag_gr$coef, se_ag_gr, "Agricultural Area Growth", c(-.012,.012),cum=F, angles=45, densities=30, alpha=.6)
plot_coef(cum_ag_gr$coef, cum_ag_gr$se, "Cumulative Agricultural Area Growth", c(-.007,.022),cum=T, angles=-45, densities=20, alpha=.6)

if (write_figs) {
  dev.copy(pdf, "~/FIGURE_ag_area_growth.pdf")
  dev.off()
}

############################################
####### FIGURE 4 -- IMPULSE RESPONSE #######
############################################

par(mar = c(5,5,2,5),mfrow=c(1,1))
plot(-1:3,c(0, reg_def$coef[4:7]), type='l', lwd=2, xlab="Time since RTA enactment", ylab="Deforestation Coefficient", ylim=c(0,0.09))
par(new=T)
plot(-1:3,c(0, reg_ag_gr$coef[4:7]), lwd=2, lty=9, axes=F, xlab=NA, ylab=NA, type='l',ylim=c(0,.0052))
axis(side=4)
mtext(side=4, line=3, "Agricultural Area Growth Coefficient")
legend("topright", col=c("black","black"), lwd=c(2,2), lty=c(1,9), legend=c("Deforestation","Agriculture" ), bty='n')

if (write_figs) {
	dev.copy(pdf, "~/FIGURE_impulse.pdf")
	dev.off()
}