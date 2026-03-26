###### Estimating Trust in Parliament, by regions
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
trustparl = read.csv("trends_parl.csv")

# Include only countries which were liberal or electoral democracies in a majority of years included, according to the VDem Regions of the world measure
trustparl.dem <- subset(trustparl, vdem_row_maj == 1)

# I originally used the item variable with wording variation as the item variable for this analysis
# Use a broader item variable instead, which only uses the survey source (as Chris Claassen does), generally results in more dynamic (less flat) trends
trustparl.dem <- dplyr::select(trustparl.dem, -Item)
trustparl.dem <- dplyr::rename(trustparl.dem, Item = Project)

# Recode the value of Item to make the EB and AsiaBarometer data source identifiers more specific
# Initially, the grep code for item-country length identifiers below thought that many other observations were also from these sources
trustparl.dem$Item[trustparl.dem$Item=="EB"] <- "SSEB" # stands for "Standard and Special Eurobarometer
trustparl.dem$Item[trustparl.dem$Item=="AsB"] <- "AsiaB" # stands for "Standard and Special Eurobarometer

# Remove observations before 1980 because they are very few and only from the US
# The US is also dominant before 1990 but 16 other countries in the WENA region have observations there (very few per country)
trustparl.dem <- trustparl.dem [!trustparl.dem$Year < 1980,]
# Also remove observations outside WENA before 1990, because there are only 7 observations from 7 countries in total there
trustparl.dem <- trustparl.dem [!(trustparl.dem$Year < 1990 & trustparl.dem$regpol6!="W-Europe & N-America"),]

# remove NAs
trustparl.dem = trustparl.dem[!trustparl.dem$Response==0,]

# order
trustparl.dem = arrange(trustparl.dem, Country, Year)

# set first year
year0parl = 1979 # = year before first year of available survey data
trustparl.dem = trustparl.dem[trustparl.dem$Year > year0parl,]

# create item by country indicators
trustparl.dem = unite(trustparl.dem, ItemCnt, c(Item, Country), sep = "_", remove = FALSE)

# create item by region indicators
trustparl.dem = unite(trustparl.dem, ItemReg, c(Item, regpol6), sep = "_", remove = FALSE)

## Prepare data for stan

# factorise
trustparl.dem$Country = as.factor(as.character(trustparl.dem$Country))
trustparl.dem$Item = as.factor(as.character(trustparl.dem$Item))
trustparl.dem$ItemCnt = as.factor(as.character(trustparl.dem$ItemCnt))
trustparl.dem$Year = trustparl.dem$Year-year0parl

# extract data
n.items = length(unique(trustparl.dem$Item))
n.cntrys = length(unique(trustparl.dem$Country))
n.yrs = 2019-year0parl # estimates up to 2019
n.resp = dim(trustparl.dem)[1]
n.itm.cnt = length(unique(trustparl.dem$ItemCnt))
n.reg = length(unique(trustparl.dem$regpol6))
cntrys = as.numeric(factor(trustparl.dem$Country))
cnt.names = levels(trustparl.dem$Country)
items = as.numeric(factor(trustparl.dem$Item))
yrs = trustparl.dem$Year
itm.cnts = as.numeric(factor(trustparl.dem$ItemCnt))
mean.resp.log = logit(mean(trustparl.dem$Response))
regions = as.numeric(factor(trustparl.dem$regpol6))
reg.names = levels(factor(trustparl.dem$regpol6))

# create item-country length indicator for items
item.ind.kp = rep(0, length(levels(trustparl.dem$ItemCnt)))
for(i in 1:length(levels(trustparl.dem$Item))) {
  item.ind.kp[grepl(levels(trustparl.dem$Item)[i], levels(trustparl.dem$ItemCnt))] = i
}
item.ind.len = sapply(lapply(levels(trustparl.dem$Item), function(x) grep(x, levels(trustparl.dem$ItemCnt))), length)
## Fit stan model

# specify data for stan
dat.1 = list(N=n.resp, K=n.items, T=n.yrs, J=n.cntrys, L=n.reg, P=n.itm.cnt, jj=cntrys, ll=regions, tt=yrs, 
             pp=itm.cnts, kk=items, it_len=item.ind.len, 
             x=trustparl.dem$RespN, samp=trustparl.dem$Sample, mn_resp_log=mean.resp.log)
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
parl.theta.m.df = data.frame(Region=rep(reg.names, each=n.yrs), 
                        Year=rep(1980:2019, times=n.reg), parl=theta.pe, 
                        parl_u95=theta.u95, parl_l95=theta.l95, parl_sd=theta.sd)

# remove estimates before first survey year and create a trimmed dataset
trustparl.dem$Region=trustparl.dem$regpol6
first.yr = data.frame(Region = levels(as.factor(trustparl.dem$Region)),
                      First_yr = as.vector(
                        by(trustparl.dem, trustparl.dem$Region, function(x) min(as.numeric(x$Year)) + year0parl)))
parl.theta.trim = merge(parl.theta.m.df, first.yr, by="Region", all.x=TRUE)
cnts = parl.theta.trim[parl.theta.trim$Year==2008, "Region"]
frst.yr = parl.theta.trim[parl.theta.trim$Year==2008, "First_yr"]
parl.theta.trim$parl_trim = parl.theta.trim$parl
parl.theta.trim$parl_trim = ifelse(parl.theta.trim$Year < parl.theta.trim$First_yr, NA, parl.theta.trim$parl_trim)
parl.theta.trim = parl.theta.trim[order(parl.theta.trim$Region, parl.theta.trim$Year), ]
parl.theta.trim = parl.theta.trim[!is.na(parl.theta.trim$parl_trim),]
parl.theta.trim$parl_trim = NULL

# save year point estimates
write.csv(parl.theta.trim, "parl_reg_est.csv", row.names=FALSE)

# plot trends
parl.theta.trim = read.csv("parl_reg_est.csv")

col1 = rgb(1, 0.55, 0, 1)
col2 = rgb(0, 0.45, 0.7, 1)

col_swd = rgb(0, 158, 115, 255, maxColorValue = 255)
col_swd_ci = rgb(0, 158, 115, 100, maxColorValue = 255)
col_parl = rgb(213, 94, 0, 255, maxColorValue = 255)
col_parl_ci = rgb(213, 94, 0, 100, maxColorValue = 255)
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

parl.theta.trim$Region_f <- as.factor(parl.theta.trim$Region)
parl.theta.trim$Region_f <- factor(parl.theta.trim$Region_f,levels = 
                                    c("W-Europe & N-America", "E-Europe & C-Asia", "L-America & Caribbean",
                                      "M-East & N-Africa", "Sub-Saharan Africa", "Asia & Pacific"))

cnts = levels(parl.theta.m.df$Region)
n.cnts = length(cnts)
decadelabels <- c("'80", "'90", "'00", "'10", "'20")

parl_reg_plot_dem <- parl.theta.trim %>%
  ggplot(aes(x = Year, y = parl, color = Region_f)) + # this and next 2 are the bones
  geom_line() + # this is where you change the point estimate line colour
  scale_color_manual(values = col_regions) + 
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = parl_l95, ymax = parl_u95), alpha = 0.1, linetype = "dotted", size = 0.5) +
  facet_wrap(~Region_f) + # this makes a separate plot for each country
  theme(legend.position = "none") + # removes legend
  scale_x_continuous("", breaks = c(1980, 1990, 2000, 2010, 2020), 
                     labels = c("'80","'90", "'00", "'10", "'20")) + # this adds the labels to the breaks
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
parl_reg_plot_dem

ggsave("parl_reg_dem.png", parl_reg_plot_dem)

###########
