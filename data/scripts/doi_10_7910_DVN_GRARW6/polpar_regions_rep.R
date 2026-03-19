###### Estimating Trust in Political Parties, 1990-2022
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
trustpolpar = read.csv("trends_polpar.csv")

# Include only countries which were liberal or electoral democracies in a majority of years included, according to the VDem Regions of the world measure
trustpolpar.dem <- subset(trustpolpar, vdem_row_maj == 1)

# I originally used the item variable with wording variation as the item variable for this analysis
# Use a broader item variable instead, which only uses the survey source (as Chris Claassen does), generally results in more dynamic (less flat) trends
trustpolpar.dem <- dplyr::select(trustpolpar.dem, -Item)
trustpolpar.dem <- dplyr::rename(trustpolpar.dem, Item = Project)

# Recode the value of Item to make the EB and AsiaBarometer data source identifiers more specific
# Initially, the grep code for item-country length identifiers below thought that many other observations were also from these sources
trustpolpar.dem$Item[trustpolpar.dem$Item=="EB"] <- "SSEB" # stands for "Standard and Special Eurobarometer"
trustpolpar.dem$Item[trustpolpar.dem$Item=="AsB"] <- "AsiaB"

#Remove data for the SSA region as there are very few observations for trust in political parties there
trustpolpar.dem <- subset(trustpolpar.dem, regpol6 != "Sub-Saharan Africa")

# remove NAs
trustpolpar.dem = trustpolpar.dem[!trustpolpar.dem$Response==0,]

# order
trustpolpar.dem = arrange(trustpolpar.dem, Country, Year)

# set first year
year0polpar = 1989 # = year before first year of available survey data
trustpolpar.dem = trustpolpar.dem[trustpolpar.dem$Year > year0polpar,]

# create item by country indicators
trustpolpar.dem = unite(trustpolpar.dem, ItemCnt, c(Item, Country), sep = "_", remove = FALSE, na.rm = TRUE)

# create item by region indicators
trustpolpar.dem = unite(trustpolpar.dem, ItemReg, c(Item, regpol6), sep = "_", remove = FALSE, na.rm = TRUE)

## Prepare data for stan

# factorise
trustpolpar.dem$Country = as.factor(as.character(trustpolpar.dem$Country))
trustpolpar.dem$Item = as.factor(as.character(trustpolpar.dem$Item))
trustpolpar.dem$ItemCnt = as.factor(as.character(trustpolpar.dem$ItemCnt))
trustpolpar.dem$ItemReg = as.factor(as.character(trustpolpar.dem$ItemReg))
trustpolpar.dem$Year = trustpolpar.dem$Year-year0polpar

# extract data
n.items = length(unique(trustpolpar.dem$Item))
n.cntrys = length(unique(trustpolpar.dem$Country))
n.reg = length(unique(trustpolpar.dem$regpol6))
n.yrs = 2020-year0polpar # estimates up to 2020
n.resp = dim(trustpolpar.dem)[1]
n.itm.cnt = length(unique(trustpolpar.dem$ItemCnt))
cntrys = as.numeric(factor(trustpolpar.dem$Country))
regions = as.numeric(factor(trustpolpar.dem$regpol6))
cnt.names = levels(trustpolpar.dem$Country)
reg.names = levels(factor(trustpolpar.dem$regpol6))
items = as.numeric(factor(trustpolpar.dem$Item))
yrs = trustpolpar.dem$Year
itm.cnts = as.numeric(factor(trustpolpar.dem$ItemCnt))
mean.resp.log = logit(mean(trustpolpar.dem$Response))

# create item-country length indicator for items
item.ind.kp = rep(0, length(levels(trustpolpar.dem$ItemCnt)))
for(i in 1:length(levels(trustpolpar.dem$Item))) {
  item.ind.kp[grepl(levels(trustpolpar.dem$Item)[i], levels(trustpolpar.dem$ItemCnt))] = i
}
item.ind.len = sapply(lapply(levels(trustpolpar.dem$Item), function(x) grep(x, levels(trustpolpar.dem$ItemCnt))), length)

## Fit stan model

# specify data for stan
dat.1 = list(N=n.resp, K=n.items, T=n.yrs, J=n.cntrys, L=n.reg, P=n.itm.cnt, jj=cntrys, ll=regions, tt=yrs, 
             pp=itm.cnts, kk=items, it_len=item.ind.len, 
             x=trustpolpar.dem$RespN, samp=trustpolpar.dem$Sample, mn_resp_log=mean.resp.log)
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
polpar.theta.m.df = data.frame(Region=rep(reg.names, each=n.yrs), 
                        Year=rep(1990:2020, times=n.reg), polpar=theta.pe, 
                        polpar_u95=theta.u95, polpar_l95=theta.l95, polpar_sd=theta.sd)

# remove estimates before first survey year and create a trimmed dataset
trustpolpar.dem$Region=trustpolpar.dem$regpol6
first.yr = data.frame(Region = levels(as.factor(trustpolpar.dem$Region)),
                      First_yr = as.vector(
                        by(trustpolpar.dem, trustpolpar.dem$Region, function(x) min(as.numeric(x$Year)) + year0polpar)))
polpar.theta.trim = merge(polpar.theta.m.df, first.yr, by="Region", all.x=TRUE)
cnts = polpar.theta.trim[polpar.theta.trim$Year==2008, "Region"]
frst.yr = polpar.theta.trim[polpar.theta.trim$Year==2008, "First_yr"]
polpar.theta.trim$polpar_trim = polpar.theta.trim$polpar
polpar.theta.trim$polpar_trim = ifelse(polpar.theta.trim$Year < polpar.theta.trim$First_yr, NA, polpar.theta.trim$polpar_trim)
polpar.theta.trim = polpar.theta.trim[order(polpar.theta.trim$Region, polpar.theta.trim$Year), ]
polpar.theta.trim = polpar.theta.trim[!is.na(polpar.theta.trim$polpar_trim),]
polpar.theta.trim$polpar_trim = NULL

# save year point estimates
write.csv(polpar.theta.trim, "polpar_reg_est.csv", row.names=FALSE)

# plot trends
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

# Region colours based on the continent colours in the Risk board game (MENA takes the colour of Australia)
col_ap2 = rgb(180, 196, 76, 255, maxColorValue = 255)
col_eeca2 = rgb(204, 0, 0, 255, maxColorValue = 255)
col_lac2 = rgb(220, 162, 92, 255, maxColorValue = 255)
col_mena2 = rgb(150, 140, 164, 255, maxColorValue = 255)
col_ssa2 = rgb(180, 148, 90, 255, maxColorValue = 255)
col_wena2 = rgb(44, 68, 188, 255, maxColorValue = 255)

col_regions <- c(col_wena, col_eeca, col_lac, col_mena, col_ssa, col_ap)
col_regions2 <- c(col_wena2, col_eeca2, col_lac2, col_mena2, col_ssa2, col_ap2)

polpar.theta.trim$Region_f <- as.factor(polpar.theta.trim$Region)
polpar.theta.trim$Region_f <- factor(polpar.theta.trim$Region_f,levels = 
                                    c("W-Europe & N-America", "E-Europe & C-Asia", "L-America & Caribbean",
                                      "M-East & N-Africa", "Sub-Saharan Africa", "Asia & Pacific"))

cnts = levels(polpar.theta.m.df$Region)
n.cnts = length(cnts)
decadelabels <- c("1980", "'90", "'00", "'10", "2020")

polpar_reg_plot_dem <- polpar.theta.trim %>%
  ggplot(aes(x = Year, y = polpar, color = Region_f)) + # this and next 2 are the bones
  geom_line() + # this is where you change the point estimate line colour
  scale_color_manual(values = col_regions) + 
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = polpar_l95, ymax = polpar_u95), alpha = 0.1, linetype = "dotted", size = 0.5) +
  facet_wrap(~Region_f) + # this makes a separate plot for each region
  theme(legend.position = "none") + # removes legend
  scale_x_continuous("", breaks = c(1980, 1990, 2000, 2010, 2020), 
                     labels = c("1980","'90", "'00", "'10", "'20")) + # this adds the labels to the breaks
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
polpar_reg_plot_dem

ggsave("polpar_reg_dem.png", polpar_reg_plot_dem)

###########