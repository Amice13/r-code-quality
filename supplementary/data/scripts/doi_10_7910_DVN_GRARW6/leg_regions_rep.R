###### Estimating Trust in the legal system
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
trustleg = read.csv("trends_leg.csv")

# Include only countries which were liberal or electoral democracies in a majority of years included, according to the VDem Regions of the world measure
trustleg.dem <- subset(trustleg, vdem_row_maj == 1)

# I originally used the item variable with wording variation as the item variable for this analysis
# Use a broader item variable instead, which only uses the survey source (as Chris Claassen does), generally results in more dynamic (less flat) trends
trustleg.dem <- dplyr::select(trustleg.dem, -Item)
trustleg.dem <- dplyr::rename(trustleg.dem, Item = Project)

# Recode the value of Item to make the EB and AsiaBarometer data source identifiers more specific
# Initially, the grep code for item-country length identifiers below thought that many other observations were also from these sources
trustleg.dem$Item[trustleg.dem$Item=="EB"] <- "SSEB" # stands for "Standard and Special Eurobarometer
trustleg.dem$Item[trustleg.dem$Item=="AsB"] <- "AsiaB" # stands for "Standard and Special Eurobarometer

# Remove observations before 1980 because they are very few and only from the US
# The US is also dominant before 1990 but 16 other countries in the WENA region have observations there (very few per country)
trustleg.dem <- trustleg.dem [!trustleg.dem$Year < 1980,]
# Also remove observations outside WENA before 1990, because there are only 7 observations from 7 countries in total there
trustleg.dem <- trustleg.dem [!(trustleg.dem$Year < 1990 & trustleg.dem$regpol6!="W-Europe & N-America"),]

# remove NAs
trustleg.dem = trustleg.dem[!trustleg.dem$Response==0,]

# order
trustleg.dem = arrange(trustleg.dem, Country, Year)

# set first year
year0leg = 1979 # = year before first year of available survey data
trustleg.dem = trustleg.dem[trustleg.dem$Year > year0leg,]

# create item by country indicators
trustleg.dem = unite(trustleg.dem, ItemCnt, c(Item, Country), sep = "_", remove = FALSE)

# create item by region indicators
trustleg.dem = unite(trustleg.dem, ItemReg, c(Item, regpol6), sep = "_", remove = FALSE)

## Prepare data for stan

# factorise
trustleg.dem$Country = as.factor(as.character(trustleg.dem$Country))
trustleg.dem$Item = as.factor(as.character(trustleg.dem$Item))
trustleg.dem$ItemCnt = as.factor(as.character(trustleg.dem$ItemCnt))
trustleg.dem$Year = trustleg.dem$Year-year0leg

# extract data
n.items = length(unique(trustleg.dem$Item))
n.cntrys = length(unique(trustleg.dem$Country))
n.yrs = 2019-year0leg # estimates up to 2019
n.resp = dim(trustleg.dem)[1]
n.itm.cnt = length(unique(trustleg.dem$ItemCnt))
n.reg = length(unique(trustleg.dem$regpol6))
cntrys = as.numeric(factor(trustleg.dem$Country))
cnt.names = levels(trustleg.dem$Country)
items = as.numeric(factor(trustleg.dem$Item))
yrs = trustleg.dem$Year
itm.cnts = as.numeric(factor(trustleg.dem$ItemCnt))
mean.resp.log = logit(mean(trustleg.dem$Response))
regions = as.numeric(factor(trustleg.dem$regpol6))
reg.names = levels(factor(trustleg.dem$regpol6))

# create item-country length indicator for items
item.ind.kp = rep(0, length(levels(trustleg.dem$ItemCnt)))
for(i in 1:length(levels(trustleg.dem$Item))) {
  item.ind.kp[grepl(levels(trustleg.dem$Item)[i], levels(trustleg.dem$ItemCnt))] = i
}
item.ind.len = sapply(lapply(levels(trustleg.dem$Item), function(x) grep(x, levels(trustleg.dem$ItemCnt))), length)
## Fit stan model

# specify data for stan
dat.1 = list(N=n.resp, K=n.items, T=n.yrs, J=n.cntrys, L=n.reg, P=n.itm.cnt, jj=cntrys, ll=regions, tt=yrs, 
             pp=itm.cnts, kk=items, it_len=item.ind.len, 
             x=trustleg.dem$RespN, samp=trustleg.dem$Sample, mn_resp_log=mean.resp.log)
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
leg.theta.m.df = data.frame(Region=rep(reg.names, each=n.yrs), 
                        Year=rep(1980:2019, times=n.reg), leg=theta.pe, 
                        leg_u95=theta.u95, leg_l95=theta.l95, leg_sd=theta.sd)

# remove estimates before first survey year and create a trimmed dataset
trustleg.dem$Region=trustleg.dem$regpol6
first.yr = data.frame(Region = levels(as.factor(trustleg.dem$Region)),
                      First_yr = as.vector(
                        by(trustleg.dem, trustleg.dem$Region, function(x) min(as.numeric(x$Year)) + year0leg)))
leg.theta.trim = merge(leg.theta.m.df, first.yr, by="Region", all.x=TRUE)
cnts = leg.theta.trim[leg.theta.trim$Year==2008, "Region"]
frst.yr = leg.theta.trim[leg.theta.trim$Year==2008, "First_yr"]
leg.theta.trim$leg_trim = leg.theta.trim$leg
leg.theta.trim$leg_trim = ifelse(leg.theta.trim$Year < leg.theta.trim$First_yr, NA, leg.theta.trim$leg_trim)
leg.theta.trim = leg.theta.trim[order(leg.theta.trim$Region, leg.theta.trim$Year), ]
leg.theta.trim = leg.theta.trim[!is.na(leg.theta.trim$leg_trim),]
leg.theta.trim$leg_trim = NULL

# save year point estimates
write.csv(leg.theta.trim, "leg_reg_est.csv", row.names=FALSE)

# plot trends
leg.theta.trim = read.csv("leg_reg_est.csv")

col1 = rgb(1, 0.55, 0, 1)
col2 = rgb(0, 0.45, 0.7, 1)

col_swd = rgb(0, 158, 115, 255, maxColorValue = 255)
col_swd_ci = rgb(0, 158, 115, 100, maxColorValue = 255)
col_leg = rgb(213, 94, 0, 255, maxColorValue = 255)
col_leg_ci = rgb(213, 94, 0, 100, maxColorValue = 255)
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

leg.theta.trim$Region_f <- as.factor(leg.theta.trim$Region)
leg.theta.trim$Region_f <- factor(leg.theta.trim$Region_f,levels = 
                                    c("W-Europe & N-America", "E-Europe & C-Asia", "L-America & Caribbean",
                                      "M-East & N-Africa", "Sub-Saharan Africa", "Asia & Pacific"))

cnts = levels(leg.theta.m.df$Region)
n.cnts = length(cnts)
decadelabels <- c("'80", "'90", "'00", "'10", "'20")

leg_reg_plot_dem <- leg.theta.trim %>%
  ggplot(aes(x = Year, y = leg, color = Region_f)) + # this and next 2 are the bones
  geom_line() + # this is where you change the point estimate line colour
  scale_color_manual(values = col_regions) + 
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = leg_l95, ymax = leg_u95), alpha = 0.1, linetype = "dotted", size = 0.5) +
  facet_wrap(~Region_f) + # this makes a separate plot for each country
  theme(legend.position = "none") + # removes legend
  scale_x_continuous("", breaks = c(1980, 1990, 2000, 2010, 2020), 
                     labels = c("'80","'90", "'00", "'10", "'20")) + # this adds the labels to the breaks
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
leg_reg_plot_dem

ggsave("leg_reg_dem.png", leg_reg_plot_dem)

###########