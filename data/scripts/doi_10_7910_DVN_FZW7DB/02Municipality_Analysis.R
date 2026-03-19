#################
# load packages #
#################

library(stargazer)
library(ggplot2)
library(estimatr)
library(lme4)
library(rdrobust)
library(rdd)

sessionInfo()
#R version 4.2.0 (2022-04-22 ucrt)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 22000)

#Matrix products: default

#locale:
#[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8
#[4] LC_NUMERIC=C                           LC_TIME=English_United States.utf8    

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#[1] rdd_0.57        Formula_1.2-4   AER_1.2-10      survival_3.3-1  car_3.1-0       carData_3.0-5   lmtest_0.9-40  
#[8] zoo_1.8-10      sandwich_3.0-2  rdrobust_2.0.2  lme4_1.1-29     Matrix_1.4-1    estimatr_0.30.6 ggplot2_3.3.6  
#[15] stargazer_5.2.3

#loaded via a namespace (and not attached):
#[1] zip_2.2.0        Rcpp_1.0.8.3     nloptr_2.0.3     pillar_1.7.0     compiler_4.2.0   tools_4.2.0     
#[7] boot_1.3-28      nlme_3.1-157     lifecycle_1.0.1  tibble_3.1.7     gtable_0.3.0     lattice_0.20-45 
#[13] pkgconfig_2.0.3  rlang_1.0.2      openxlsx_4.2.5   cli_3.3.0        withr_2.5.0      dplyr_1.0.9     
#[19] generics_0.1.2   vctrs_0.4.1      grid_4.2.0       tidyselect_1.1.2 glue_1.6.2       R6_2.5.1        
#[25] fansi_1.0.3      minqa_1.2.4      purrr_0.3.4      magrittr_2.0.3   MASS_7.3-56      scales_1.2.0    
#[31] ellipsis_0.3.2   splines_4.2.0    abind_1.4-5      colorspace_2.0-3 utf8_1.2.2       stringi_1.7.6   
#[37] munsell_0.5.0    crayon_1.5.1  




#############
# load data #
#############

load("TransferData_October2022_JJPS.RData")


# subset mix districts
mixdistricts <- pre.red[which(pre.red$district_type == "mix"
                              & !is.na(pre.red$snow_distance2)),]


# check summary of distance to the snow border
summary(mixdistricts$snow_distance2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -98580  -19920   -7355   -8829    2916   54663




############
# Figure 2 #
############

# get optimal bandwidth (not used, just to see)
bw <- rdbwselect(y=mixdistricts$F1logngaid_pc, x=mixdistricts$snow_distance2, c=0,
                 cluster=mixdistricts$district_year)
bw$bws
#      h (left) h (right) b (left) b (right)
#mserd 6375.231  6375.231 12520.45  12520.45


# function to estimate and plot GRD results with varying bandwidths
plotRD <- function(outcome, main, ylim_adjust=0, conf_level=0.95,
                   ylab="Effect at Discontinuity"){
  library(rdrobust);library(rdd)
  
  # dataframe to store the results
  result <- NULL
  
  # bandwidth range
  bw <- seq(4000, 15000, by=1000)
  
  # loop through bandwidths
  for(i in bw){
	# get weights
    mixdistricts$tkd.temp <- kernelwts(mixdistricts$snow_distance2, 0, i, 
                                       kernel="triangular")
    
	# use weight > 0
    mix.temp <- mixdistricts[mixdistricts$tkd.temp > 0,]
    colnames(mix.temp)[which(colnames(mix.temp)==outcome)] <- "outcome"
    
	# estimate GRD
    llt <- lm_robust(outcome ~ snow_treat + poly(Xl,1) + poly(Xr,1),
                     fixed_effects=district_year,
                     data=mix.temp, weights=mix.temp$tkd.temp,
                     clusters=muncode_num, se_type="stata", 
                     alpha=1 - conf_level)
					 
	# store the estimate and CI
    coefs <- data.frame(Estimate=coef(llt)[1],
                        CI.Lower=confint(llt, level=conf_level)[1,1],
                        CI.Upper=confint(llt, level=conf_level)[1,2])
    result <- rbind(result, coefs)
  }
  
  # plot results
  plot(bw, result[,1], main=main, type="n",
       xlab="Bandwidth", ylab=ylab,
       ylim=c(min(c(0,result[,2]))-ylim_adjust, max(result[,3])+ylim_adjust))
  abline(h=0, col="gray", lwd=2)
  lines(bw, result[,1], lwd=3)
  lines(bw, result[,2], lty=2, lwd=1.5)
  lines(bw, result[,3], lty=2, lwd=1.5)
  
  return(data.frame(cbind(bw, result)))
}


# outcome = LDP vote share, 95% CI
ldp_vs1 <- plotRD("ldp_voteshare1", main="Outcome = LDP Total Vote Share / Total Votes", 
                  0.02, 0.95,
                  ylab="Local Average Treatment Effect of Snow Subsidy")


# outcome = LDP vote share, 90% CI
ldp_vs1_2 <- plotRD("ldp_voteshare1", main="Outcome = LDP Total Vote Share / Total Votes", 
                    0.02, 0.90,
                    ylab="Local Average Treatment Effect of Snow Subsidy")


# plot results
ggplot(data = ldp_vs1_2, aes(x = bw, y = Estimate)) +
  geom_hline(yintercept=0, lty=3) +
  geom_ribbon(data = ldp_vs1_2, aes(ymin = CI.Lower, ymax = CI.Upper), alpha=0.2) +
  geom_ribbon(data = ldp_vs1, aes(ymin = CI.Lower, ymax = CI.Upper), alpha=0.1) +
  geom_line(lwd=1.1) +
  xlab("Bandwidth") + 
  ylab("LATE of Snow Subsidy on LDP Vote Share") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))
#ggsave("lpdvoteshare_grd.pdf", width=10, height=6)




############
# Figure 3 #
############

# outcome = transfers per capita, 90% CI
sci90 <- plotRD("F1logngaid_pc", main="", 0.1, 0.9,
                ylab="Local Average Treatment Effect of Snow Subsidy")


# outcome = transfers per capita, 95% CI
sci95 <- plotRD("F1logngaid_pc", main="", 0.1, 0.95,
                ylab="Local Average Treatment Effect of Snow Subsidy")


# average effect size
mean(sci90$Estimate) # 0.2282231


# plot results
ggplot(data = sci90, aes(x = bw, y = Estimate)) +
  geom_hline(yintercept=0, lty=3) +
  geom_ribbon(data = sci90, aes(ymin = CI.Lower, ymax = CI.Upper), alpha=0.2) +
  geom_ribbon(data = sci95, aes(ymin = CI.Lower, ymax = CI.Upper), alpha=0.1) +
  geom_line(lwd=1.1) +
  xlab("Bandwidth") + 
  ylab("LATE of Snow Subsidy on Transfers") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))
#ggsave("locallinear.pdf", width=10, height=6)




###########
# Table 2 #
###########

d1c <- lmer(F1logngaid_pc ~ snow_treat
            + mun_ceif + needy_pc + primary_pc
            + lnpop + logincome_pc + lndensity 
            + DISceif + DISneedy + DISprimary + logDISpop +
            + logDISincomepc + logDISdensity + DISmal + HI + logNum + LDPseats
            + factor(year)
            + (1|district_year),
            data=pre.red, subset = pre.red$district_type != "mix")
summary(d1c)




#######################
# Appendix Figure C.1 #
#######################

# GRD for other variables
covariates <- rbind(plotRD("lnpop", main="Population (log)", 0.2),
                    plotRD("lndensity", main="Population Density (log)", 0.2),
                    plotRD("logincome_pc", main="Income per capita (log)", 0.02),
                    plotRD("primary_pc", main="Primary Industry Proportion", 0.1),
                    plotRD("needy_pc", main="Dependency Proportion", 0.01),
                    plotRD("mun_ceif", main="Fiscal Power", 0.02),
                    plotRD("lnarea_size", main="Area Size (log)", 0.2),
                    plotRD("lnaltitude", main="Altitude (log)", 0.2))


# add variable names
covariates$labels <- rep(c("Population (log)", "Population Density (log)",
                           "Income Per Capita (log)", "Proportion in Agriculture",
                           "Proportion Dependent", "Fiscal Power",
                           "Area Size (log)", "Altitude (log)"),
                         each=12)


# change variable names to factor
covariates$labels <- factor(covariates$labels,
                            levels=c("Population (log)", "Population Density (log)",
                                     "Income Per Capita (log)", "Proportion in Agriculture",
                                     "Proportion Dependent", "Fiscal Power",
                                     "Area Size (log)", "Altitude (log)"))


# plot all
ggplot(data = covariates, aes(x = bw, y = Estimate)) +
  geom_hline(yintercept=0, lty=3) +
  geom_ribbon(data = covariates, aes(ymin = CI.Lower, ymax = CI.Upper), alpha=0.2) +
  geom_line(lwd=1.1) +
  facet_wrap(. ~ labels, ncol = 3, scales = "free") +
  xlab("Bandwidth") + 
  ylab("Local Average Treatment Effect of Snow Subsidy on Covariates") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))
#ggsave("balance.pdf", width=12, height=7)




#######################
# Appendix Figure D.1 #
#######################

bw <- rdbwselect(y=mixdistricts$F1logngaid_pc, x=mixdistricts$snow_distance2, c=0,
                 cluster=mixdistricts$district_year,
                 masspoints="off")


# plot results
#pdf("sorting.pdf", width=8, height=5.5)
par(mar=c(5.1,4.1,2.1,2.1))
mtest <- DCdensity(mixdistricts$snow_distance2, 
                   cutpoint=0, bw=bw$bws[1],
                   ext.out=TRUE)
mtext("Distance to Treatment Border", 1, 3)
#dev.off()


# significance
mtest$p # 0.4581655




#######################
# Appendix Figure E.1 #
#######################

# outcome = winning LDP vote share, 95% CI
wldp_vs1 <- plotRD("sumLDP_VshareVP", main="Winning LDP Vote Share", 
                   0.02, 0.95,
                   ylab="Local Average Treatment Effect of Snow Subsidy")


# outcome = winning LDP vote share, 90% CI
wldp_vs1_2 <- plotRD("sumLDP_VshareVP", main="Winning LDP Vote Share", 
                     0.02, 0.90,
                     ylab="Local Average Treatment Effect of Snow Subsidy")


# plot results
ggplot(data = wldp_vs1_2, aes(x = bw, y = Estimate)) +
  geom_hline(yintercept=0, lty=3) +
  geom_ribbon(data = wldp_vs1_2, aes(ymin = CI.Lower, ymax = CI.Upper), alpha=0.2) +
  geom_ribbon(data = wldp_vs1, aes(ymin = CI.Lower, ymax = CI.Upper), alpha=0.1) +
  geom_line(lwd=1.1) +
  xlab("Bandwidth") + 
  ylab("LATE of Snow Subsidy on Winning LDP Vote Share") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))
#ggsave("winning_lpdvoteshare_grd.pdf", width=10, height=6)




######################
# Appendix Table F.1 #
######################

# outcome = LDP vote share
m1c <- lm_robust(ldp_voteshare1 ~ snow_treat 
                 + mun_ceif + needy_pc + primary_pc 
                 + lnpop + logincome_pc + lndensity
                 + lnarea_size + lnaltitude,
                 data=mixdistricts,
                 fixed_effects=district_year,
                 clusters=muncode_num, se_type="stata")
summary(m1c)


# outcome = winning LDP vote share
m2c <- lm_robust(sumLDP_VshareVP ~ snow_treat 
                 + mun_ceif + needy_pc + primary_pc 
                 + lnpop + logincome_pc + lndensity
                 + lnarea_size + lnaltitude,
                 data=mixdistricts,
                 fixed_effects=district_year,
                 clusters=muncode_num, se_type="stata")

summary(m2c)


# outcome = transfers per capita
m3c <- lm_robust(F1logngaid_pc ~ snow_treat 
                 + mun_ceif + needy_pc + primary_pc 
                 + lnpop + logincome_pc + lndensity
                 + lnarea_size + lnaltitude,
                 data=mixdistricts,
                 fixed_effects=district_year,
                 clusters=muncode_num, se_type="stata")

summary(m3c)
