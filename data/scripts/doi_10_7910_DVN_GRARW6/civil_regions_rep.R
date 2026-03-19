###### Estimating trust in the civil service
###### Running Stan Models Using cmdstanr
###### v5

# libraries
library(arm)
library(dplyr)
library(tidyr)
library(loo)
library(ggplot2)
library(bayesplot)
library(cmdstanr)
library(rio)
library(tidyverse)
library(ggplot2)

# options
options("cmdstanr_verbose" = TRUE)
options(mc.cores = parallel::detectCores())

# read trust data
trustcivil = read.csv("trends_civil.csv")

# Include only countries which were liberal or electoral democracies in a majority of years included, according to the VDem Regions of the world measure
trustcivil.dem <- subset(trustcivil, vdem_row_maj == 1)

# I originally used the item variable with wording variation as the item variable for this analysis
# Use a broader item variable instead, which only uses the survey source (as Chris Claassen does), generally results in more dynamic (less flat) trends
trustcivil.dem <- dplyr::select(trustcivil.dem, -Item)
trustcivil.dem <- dplyr::rename(trustcivil.dem, Item = Project)

# Recode the value of Item to make the EB and AsiaBarometer data source identifiers more specific
# Initially, the grep code for item-country length identifiers below thought that many other observations were also from these sources
trustcivil.dem$Item[trustcivil.dem$Item=="EB"] <- "SSEB" # stands for "Standard and Special Eurobarometer
trustcivil.dem$Item[trustcivil.dem$Item=="AsB"] <- "AsiaB" # stands for "Standard and Special Eurobarometer

# Remove observations before 1980 because they are very few and only from the US
# The US is also dominant before 1990 but 16 other countries in the WENA region have observations there (very few per country)
trustcivil.dem <- trustcivil.dem [!trustcivil.dem$Year < 1980,]
# Also remove observations outside WENA before 1990, because there are only 7 observations from 7 countries in total there
trustcivil.dem <- trustcivil.dem [!(trustcivil.dem$Year < 1990 & trustcivil.dem$regpol6!="W-Europe & N-America"),]

# Remove data for the SSA and MENA regions as there are very few observations for trust in government there
trustcivil.dem <- subset(trustcivil.dem, regpol6 != "Sub-Saharan Africa")
trustcivil.dem <- subset(trustcivil.dem, regpol6 != "M-East & N-Africa")

# remove NAs
trustcivil.dem = trustcivil.dem[!trustcivil.dem$Response==0,]

# order
trustcivil.dem = arrange(trustcivil.dem, Country, Year)

# set first year
year0civil = 1979 # = year before first year of available survey data
trustcivil.dem = trustcivil.dem[trustcivil.dem$Year > year0civil,]

# create item by country indicators
trustcivil.dem = unite(trustcivil.dem, ItemCnt, c(Item, Country), sep = "_", remove = FALSE)

# create item by region indicators
trustcivil.dem = unite(trustcivil.dem, ItemReg, c(Item, regpol6), sep = "_", remove = FALSE)

## Prepare data for stan

# factorise
trustcivil.dem$Country = as.factor(as.character(trustcivil.dem$Country))
trustcivil.dem$Item = as.factor(as.character(trustcivil.dem$Item))
trustcivil.dem$ItemCnt = as.factor(as.character(trustcivil.dem$ItemCnt))
trustcivil.dem$Year = trustcivil.dem$Year-year0civil

# extract data
n.items = length(unique(trustcivil.dem$Item))
n.cntrys = length(unique(trustcivil.dem$Country))
n.yrs = 2019-year0civil # estimates up to 2019
n.resp = dim(trustcivil.dem)[1]
n.itm.cnt = length(unique(trustcivil.dem$ItemCnt))
n.reg = length(unique(trustcivil.dem$regpol6))
cntrys = as.numeric(factor(trustcivil.dem$Country))
cnt.names = levels(trustcivil.dem$Country)
items = as.numeric(factor(trustcivil.dem$Item))
yrs = trustcivil.dem$Year
itm.cnts = as.numeric(factor(trustcivil.dem$ItemCnt))
mean.resp.log = logit(mean(trustcivil.dem$Response))
regions = as.numeric(factor(trustcivil.dem$regpol6))
reg.names = levels(factor(trustcivil.dem$regpol6))

# create item-country length indicator for items
item.ind.kp = rep(0, length(levels(trustcivil.dem$ItemCnt)))
for(i in 1:length(levels(trustcivil.dem$Item))) {
  item.ind.kp[grepl(levels(trustcivil.dem$Item)[i], levels(trustcivil.dem$ItemCnt))] = i
}
item.ind.len = sapply(lapply(levels(trustcivil.dem$Item), function(x) grep(x, levels(trustcivil.dem$ItemCnt))), length)

## Fit stan model

# specify data for stan
dat.1 = list(N=n.resp, K=n.items, T=n.yrs, J=n.cntrys, L=n.reg, P=n.itm.cnt, jj=cntrys, ll=regions, tt=yrs, 
             pp=itm.cnts, kk=items, it_len=item.ind.len, 
             x=trustcivil.dem$RespN, samp=trustcivil.dem$Sample, mn_resp_log=mean.resp.log)
sapply(dat.1, summary)

# parameters to save 
pars.1 = c("Sigma","Omega","sigma_delta","sigma_theta","phi","mu_lambda","lambda","gamm","delta",
           "theta","x_pred","log_lik")

# iterations for MCMC simulation
n.iter = 1000
n.warm = 500
n.samp = n.iter - n.warm
n.chn = 4

# compile model
stan.mod = cmdstan_model('stan_mod6_v6_reg_rep.stan')

# Stan fit
fit.mod= stan.mod$sample(
  data = dat.1,
  chains = n.chn,
  init = 0.1,
  parallel_chains = n.chn,
  iter_warmup = n.warm,
  iter_sampling = n.samp,
  refresh = round(n.iter/20, 0),
  adapt_delta = 0.99, 
  max_treedepth = 13,
  save_warmup = FALSE
)

## Check convergence

# Examine model fit
res = fit.mod
res$cmdstan_diagnose()
res.tab = res$print(pars.1, max_rows=80, digits=3)
sum = res$summary(pars.1)
print(sum[order(sum$rhat, decreasing=TRUE), ], n=50)
res_neff_ratio = neff_ratio(res)
res_neff_ratio[order(res_neff_ratio, decreasing=FALSE)][1:50]

# traceplot
tp.pars = c("Sigma[1,1]","Sigma[2,2]","Omega[1,2]","sigma_theta","sigma_delta","mu_lambda",
            "phi","delta[23]")
tp = bayesplot::mcmc_trace(res$draws(tp.pars), size=0.3, np=nuts_params(res))
tp

## Extract and save mood estimates

theta.m.out = apply(res$draws("theta"), 3, as.vector)
(theta.m.mean = mean(as.vector(theta.m.out)))
(theta.m.sd = sd(as.vector(theta.m.out)))
theta.m.std = (theta.m.out - theta.m.mean) / theta.m.sd # standardize
theta.m.t = apply(theta.m.std, 1, function(x) t(x) )
theta.pe = apply(theta.m.t, 1, mean)
theta.u95 = apply(theta.m.t, 1, quantile, probs=c(0.975))
theta.l95 = apply(theta.m.t, 1, quantile, probs=c(0.025))
theta.sd = apply(theta.m.t, 1, sd)
civil.theta.m.df = data.frame(Region=rep(reg.names, each=n.yrs), 
                        Year=rep(1980:2019, times=n.reg), civil=theta.pe, 
                        civil_u95=theta.u95, civil_l95=theta.l95, civil_sd=theta.sd)

# remove estimates before first survey year and create a trimmed dataset
trustcivil.dem$Region=trustcivil.dem$regpol6
first.yr = data.frame(Region = levels(as.factor(trustcivil.dem$Region)),
                      First_yr = as.vector(
                        by(trustcivil.dem, trustcivil.dem$Region, function(x) min(as.numeric(x$Year)) + year0civil)))
civil.theta.trim = merge(civil.theta.m.df, first.yr, by="Region", all.x=TRUE)
cnts = civil.theta.trim[civil.theta.trim$Year==2008, "Region"]
frst.yr = civil.theta.trim[civil.theta.trim$Year==2008, "First_yr"]
civil.theta.trim$civil_trim = civil.theta.trim$civil
civil.theta.trim$civil_trim = ifelse(civil.theta.trim$Year < civil.theta.trim$First_yr, NA, civil.theta.trim$civil_trim)
civil.theta.trim = civil.theta.trim[order(civil.theta.trim$Region, civil.theta.trim$Year), ]
civil.theta.trim = civil.theta.trim[!is.na(civil.theta.trim$civil_trim),]
civil.theta.trim$civil_trim = NULL

# save year point estimates
write.csv(civil.theta.trim, "civil_reg_est.csv", row.names=FALSE)

# plot trends
civil.theta.trim = read.csv("civil_reg_est.csv")

col1 = rgb(1, 0.55, 0, 1)
col2 = rgb(0, 0.45, 0.7, 1)

col_swd = rgb(0, 158, 115, 255, maxColorValue = 255)
col_swd_ci = rgb(0, 158, 115, 100, maxColorValue = 255)
col_civil = rgb(213, 94, 0, 255, maxColorValue = 255)
col_civil_ci = rgb(213, 94, 0, 100, maxColorValue = 255)
col_gov = rgb(0, 0, 0, 255, maxColorValue = 255)
col_gov_ci = rgb(0, 0, 0, 100, maxColorValue = 255)
col_polpar = rgb(230, 159, 0, 255, maxColorValue = 255)
col_polpar_ci = rgb(230, 159, 0, 100, maxColorValue = 255)

col_ap = rgb(0, 255, 0, 255, maxColorValue = 255)
col_eeca = rgb(255, 0, 0, 255, maxColorValue = 255)
col_lac = rgb(255, 165, 0, 255, maxColorValue = 255)
col_mena = rgb(0, 96, 0, 255, maxColorValue = 255)
col_ssa = rgb(156, 136, 71, 255, maxColorValue = 255)
col_wena = rgb(65, 105, 225, 255, maxColorValue = 255)
col_glob = rgb(0, 0, 0, 255, maxColorValue = 255)

col_regions <- c(col_wena, col_eeca, col_lac, col_mena, col_ssa, col_ap)

civil.theta.trim$Region_f <- as.factor(civil.theta.trim$Region)
civil.theta.trim$Region_f <- factor(civil.theta.trim$Region_f,levels = 
                                    c("W-Europe & N-America", "E-Europe & C-Asia", "L-America & Caribbean",
                                      "M-East & N-Africa", "Sub-Saharan Africa", "Asia & Pacific"))

cnts = levels(civil.theta.m.df$Region)
n.cnts = length(cnts)
decadelabels <- c("'80", "'90", "'00", "'10", "'20")

civil_reg_plot_dem <- civil.theta.trim %>%
  ggplot(aes(x = Year, y = civil, color = Region_f)) + # this and next 2 are the bones
  geom_line() + # this is where you change the point estimate line colour
  scale_color_manual(values = col_regions) + 
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = civil_l95, ymax = civil_u95), alpha = 0.1, linetype = "dotted", size = 0.5) +
  facet_wrap(~Region_f) + # this makes a separate plot for each country
  theme(legend.position = "none") + # removes legend
  scale_x_continuous("", breaks = c(1980, 1990, 2000, 2010, 2020), 
                     labels = c("'80","'90", "'00", "'10", "'20")) + # this adds the labels to the breaks
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
civil_reg_plot_dem

ggsave("civil_reg_dem.png", civil_reg_plot_dem)

###########