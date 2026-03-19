##################################################
##################################################
# Replication Code for Abman and Lundberg (2019) #
#### "Does Free Trade Increase Deforestation" ####
####### ONLINE APPENDIX TABLES AND FIGURES #######
##################################################
##################################################
library(stargazer)
library(data.table)
library(plm)
library(gdata)
library(zoo)

rm(list=ls())

##########################################
########### Helper function ##############
##########################################
cumulative_tests <- function(model,no_robust_se = F) {
  
  # number of dummy time variables 2*LR + 2*event window
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
  p <- sapply(abs(cum_eff/se_cum),function(x) round(2*pt(x,df=model$df,lower.tail=F),digits=3) )
  names(p) <- names(cum_eff)
  return(list(coef = cum_eff, se = se_cum, p_value = p))
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
######## TABLE 3: Mining output ############
############################################

reg_gold <- plm(log(1+gold_output) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")
se_gold <- sqrt(diag(vcovHC(reg_gold, type="HC0",cluster="group")))
cum_gold <- cumulative_tests(reg_gold)

reg_copper <- plm(log(1+copper_output) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")
se_copper <- sqrt(diag(vcovHC(reg_copper, type="HC0",cluster="group")))
cum_copper <- cumulative_tests(reg_copper)

reg_dummy <- plm(log(1+gold_output) ~ ( rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")

cat(rep("\n",5), "Appendix Table 3 -- Mining Output", rep("\n", 5))

stargazer(reg_gold, reg_dummy, reg_copper, reg_dummy, coef = list(reg_gold$coef, c(rep(NA,4), cum_gold$coef), reg_copper$coef, c(rep(NA,4), cum_copper$coef)), se=list(se_gold,c(rep(NA,4), cum_gold$se), se_copper, c(rep(NA,4), cum_copper$se)), title="Effects of Trade Shocks -- Mining Output", align=TRUE, dep.var.labels.include=F, column.labels=c("Gold Production", "Copper Production"), covariate.labels=c("$RTA_{LR-}$","$RTA_{t-3}$", "$RTA_{t-2}$","$RTA_t$", "$RTA_{t+1}$", "$RTA_{t+2}$", "$RTA_{t+3}$", "$RTA_{LR+}$"),no.space=T, omit.stat=c("LL", "ser","f", "aic","bic", "adj.rsq"))

R <- matrix(0,nrow=3, ncol=8)
diag(R)[1:3] <- 1

cat(rep("\n",5), "Wald Statistics on Leading Coefficients", rep("\n", 5))

cat(rep("\n",2), "Wald statistic: gold leads jointly 0 \n", round(t(R%*%matrix(coef(reg_gold),ncol=1))%*%solve(R%*%vcovHC(reg_gold, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_gold),ncol=1), 3),"\n" )

cat(rep("\n",2), "Wald statistic: copper leads jointly 0 \n", round(t(R%*%matrix(coef(reg_copper),ncol=1))%*%solve(R%*%vcovHC(reg_copper, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_copper),ncol=1), 3))

############################################
########## TABLE 4: GDP Growth #############
############################################

reg_gdp <- plm(log(1+gdp_pc) ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")
se_gdp <- sqrt(diag(vcovHC(reg_gdp, type="HC0",cluster="group")))
cum_gdp <- cumulative_tests(reg_gdp)

reg_gdp_gr <- plm(gdp_pc_gr ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")
se_gdp_gr <- sqrt(diag(vcovHC(reg_gdp_gr, type="HC0",cluster="group")))
cum_gdp_gr <- cumulative_tests(reg_gdp_gr)

reg_dummy <- plm(log(1+gdp_pc) ~ (rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")

cat(rep("\n",5), "Appendix Table 4 -- GDP", rep("\n", 5))

stargazer(reg_gdp, reg_dummy, reg_gdp_gr, reg_dummy, coef = list(reg_gdp$coef, c(rep(NA,4), cum_gdp$coef),reg_gdp_gr$coef,  c(rep(NA,4), cum_gdp_gr$coef)), se=list(se_gdp,c(rep(NA,4), cum_gdp$se), se_gdp_gr,c(rep(NA,4), cum_gdp_gr$se)), title="Effects of Trade Shocks -- GDP and Trade", align=TRUE, dep.var.labels.include=F, column.labels=c(" ", "", ""), covariate.labels=c("$RTA_{LR-}$","$RTA_{t-3}$", "$RTA_{t-2}$","$RTA_t$", "$RTA_{t+1}$", "$RTA_{t+2}$", "$RTA_{t+3}$", "$RTA_{LR+}$"), no.space=T, omit.stat=c("LL", "ser","f", "aic","bic", "adj.rsq"))

R <- matrix(0,nrow=3, ncol=8)
diag(R)[1:3] <- 1

cat(rep("\n",5), "Wald Statistics on Leading Coefficients", rep("\n", 5))

cat(rep("\n",2), "Wald statistic: GDP per capita leads jointly 0 \n", round(t(R%*%matrix(coef(reg_gdp),ncol=1))%*%solve(R%*%vcovHC(reg_gdp, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_gdp),ncol=1), 3), "\n" )

cat(rep("\n",2), "Wald statistic: GDP per capita growth leads jointly 0 \n", round(t(R%*%matrix(coef(reg_gdp_gr),ncol=1))%*%solve(R%*%vcovHC(reg_gdp_gr, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_gdp_gr),ncol=1), 3))

############################################
############ TABLE 5: Ag Trade #############
############################################

reg_food_x <- plm(food_exports_gr ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")
se_food_x <- sqrt(diag(vcovHC(reg_food_x, type="HC0",cluster="group")))
cum_food_x <- cumulative_tests(reg_food_x)

reg_food_m <- plm(food_imports_gr ~ (pre + lead3 + lead2 + rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")
se_food_m <- sqrt(diag(vcovHC(reg_food_m, type="HC0",cluster="group")))
cum_food_m <- cumulative_tests(reg_food_m)

reg_dummy <- plm(food_exports_gr ~ ( rta + lag1+ lag2  + lag3 + post), data=pdat, effect = "twoways", model="within")

cat(rep("\n",5), "Appendix Table 5 -- Ag Trade Growth", rep("\n", 5))

stargazer(reg_food_x, reg_dummy, reg_food_m, reg_dummy, coef = list(reg_food_x$coef, c(rep(NA,4), cum_food_x$coef), reg_food_m$coef, c(rep(NA,4), cum_food_m$coef)), se=list(se_food_x,c(rep(NA,4), cum_food_x$se), se_food_m,c(rep(NA,4), cum_food_m$se)), title="Effects of Trade Shocks -- Ag Trade Growth", align=TRUE, dep.var.labels.include=F, column.labels=c(" ", "", ""), covariate.labels=c("$RTA_{LR-}$","$RTA_{t-3}$", "$RTA_{t-2}$","$RTA_t$", "$RTA_{t+1}$", "$RTA_{t+2}$", "$RTA_{t+3}$", "$RTA_{LR+}$"), no.space=T, omit.stat=c("LL", "ser","f", "aic","bic", "adj.rsq"))

R <- matrix(0,nrow=3, ncol=8)
diag(R)[1:3] <- 1

cat(rep("\n",5), "Wald Statistics on Leading Coefficients", rep("\n", 5))

cat(rep("\n",2), "Wald statistic: Ag export growth leads jointly 0 \n", round(t(R%*%matrix(coef(reg_food_x),ncol=1))%*%solve(R%*%vcovHC(reg_food_x, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_food_x),ncol=1), 3), "\n" )

cat(rep("\n",2), "Wald statistic: Ag import growth leads jointly 0 \n", round(t(R%*%matrix(coef(reg_food_m),ncol=1))%*%solve(R%*%vcovHC(reg_food_m, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_food_m),ncol=1), 3))

############################################
######### TABLE 6: Number of RTAs ##########
############################################

reg_def_n <- plm(log(1+forest_loss) ~ (pre_n + lead3_n + lead2_n + rta_n + lag1_n+ lag2_n  + lag3_n + post_n), data=pdat, effect = "twoways", model="within")
se_def_n <- sqrt(diag(vcovHC(reg_def_n, type="HC0",cluster="group")))

reg_ag_gr_n <- plm(ag_area_gr ~  (pre_n + lead3_n + lead2_n + rta_n + lag1_n+ lag2_n  + lag3_n + post_n), data=pdat, effect = "twoways", model="within")
se_ag_gr_n <- sqrt(diag(vcovHC(reg_ag_gr_n, type="HC0",cluster="group")))

cum_def_n <- cumulative_tests(reg_def_n)
cum_ag_gr_n <- cumulative_tests(reg_ag_gr_n)

reg_dummy <- plm(log(1+forest_loss) ~ ( rta_n + lag1_n+ lag2_n  + lag3_n + post_n), data=pdat, effect = "twoways", model="within")

cat(rep("\n",5), "Appendix Table 6 -- Number of RTAs", rep("\n", 5))

stargazer(reg_def_n, reg_dummy, reg_ag_gr_n, reg_dummy, coef = list(reg_def_n$coef, c(rep(NA,4), cum_def_n$coef), reg_ag_gr_n$coef, c(rep(NA,4), cum_ag_gr_n$coef)), se=list(se_def_n,c(rep(NA,4), cum_def_n$se), se_ag_gr_n, c(rep(NA,4), cum_ag_gr_n$se)), title="Effects of Trade Shocks -- Number of RTAs", align=TRUE, dep.var.labels.include=F, column.labels=c("Deforestation", "Ag Area Growth"), covariate.labels=c("$RTA_{LR-}$","$RTA_{t-3}$", "$RTA_{t-2}$","$RTA_t$", "$RTA_{t+1}$", "$RTA_{t+2}$", "$RTA_{t+3}$", "$RTA_{LR+}$"),no.space=T, omit.stat=c("LL", "ser","f", "aic","bic", "adj.rsq"), digits = 4)

R <- matrix(0,nrow=3, ncol=8)
diag(R)[1:3] <- 1

cat(rep("\n",5), "Wald Statistics on Leading Coefficients", rep("\n", 5))

cat(rep("\n",2), "Wald statistic: deforestation leads jointly 0 \n", round(t(R%*%matrix(coef(reg_def_n),ncol=1))%*%solve(R%*%vcovHC(reg_def_n, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_def_n),ncol=1), 3), "\n" )

cat(rep("\n",2), "Wald statistic: Ag area growth leads jointly 0 \n", round(t(R%*%matrix(coef(reg_ag_gr_n),ncol=1))%*%solve(R%*%vcovHC(reg_ag_gr_n, type="HC0", cluster="group")%*%t(R))%*%R%*%matrix(coef(reg_ag_gr_n),ncol=1), 3))

############################################
########### FIGURE 1: RTA Timing ###########
############################################

par(mfrow=c(2,1) )

rta_timing[,delay:=12*(as.yearmon(as.Date(Date.of.entry.into.force)) - as.yearmon(as.Date(Signature.Date)))]

rta_timing[delay >= 0 ,hist(delay, probability= T, breaks=50, col="lightgrey", xlab="Time Between Signing RTA and Entry in Force (months)",main="All RTAs")]
rta_timing[year(as.Date(Date.of.entry.into.force))>2000 & year(as.Date(Date.of.entry.into.force))<2013,hist(delay, probability= T, breaks=50, col="lightgrey", xlab="Time Between Signing RTA and Entry in Force (months)",main="In Sample RTAs")]

cat(rep("\n",2), "Average delay between signing and entry into force: \n", rta_timing[delay>=0,round(mean(delay),1)], "\n" )

cat(rep("\n",2), "Standard deviation of delay between signing and entry into force: \n", rta_timing[delay>=0,round(sd(delay),1)], "\n" )

if (write_figs) {
  dev.copy(pdf, "~/FIGURE_RTA_delay.pdf")
  dev.off()
}

############################################
########### FIGURE 2: RTA Share ############
############################################
n_adv <- data_main[imf_adv==1,.N,by=Year][,unique(N)]
n_adv_trop <- data_main[imf_adv==1 & Tropical_any==1,.N,by=Year][,unique(N)]
n_adv_non <- data_main[imf_adv==1 & Tropical_any==0,.N,by=Year][,unique(N)]

n_dev <- data_main[imf_adv==0,.N,by=Year, ][,unique(N)]
n_dev_trop <- data_main[imf_adv==0 & Tropical_any==1,.N,by=Year][,unique(N)]
n_dev_non <- data_main[imf_adv==0 & Tropical_any==0,.N,by=Year][,unique(N)]

par(mfrow=c(2,1))
plot(data_main[,sum(rta==1)/189,keyby=Year],type='l', ylim=c(0,1),ylab="", main="Developed Countries")
lines(data_main[,sum(rta==1 & imf_adv==1 & Tropical_any==1)/n_adv_trop,keyby=Year], lty=2)
lines(data_main[,sum(rta==1 & imf_adv==1 & Tropical_any==0)/n_adv_non,keyby=Year], lty=3)

plot(data_main[,sum(rta==1)/189,keyby=Year],type='l', ylim=c(0,1),ylab="", main="Developing Countries")
lines(data_main[,sum(rta==1 & imf_adv==0 & Tropical_any==1)/n_dev_trop,keyby=Year], lty=2)
lines(data_main[,sum(rta==1 & imf_adv==0 & Tropical_any==0)/n_dev_non,keyby=Year], lty=3)

############################################
######## FIGURE 3: NET FOREST LOSS #########
############################################

gain <- data_main[,list(loss=sum(forest_loss), gain=mean(forest_gain), baseline=mean(extent)),by=Country]

par(mfrow=c(1,1) )
gain[,plot(log(1+.01*loss),log(1+.01*loss)-log(1+.01*gain),xlim=c(-1,13),ylim=c(-3,9),pch=16, col=rgb(0,0,0,alpha=.75),ylab="Net Log Forest Loss", xlab="Log Forest Loss")]
abline(h=0,lty=2,lwd=.5)

if (write_figs) {
  dev.copy(pdf, "~/FIGURE_net_loss.pdf")
  dev.off()
}