###### Estimating Trust in government, 1973-2022
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
library(ggeffects)

# options
options("cmdstanr_verbose" = TRUE)
options(mc.cores = parallel::detectCores())

# read trust data
trustgov = read.csv("trends_gov.csv")

## Edit data

# remove NAs
trustgov = trustgov[!trustgov$Response==0, ]

# order
trustgov = arrange(trustgov, Country, Year)

# Include only countries which were liberal or electoral democracies in a majority of years included, according to the VDem Regions of the world measure
trustgov.dem <- subset(trustgov, vdem_row_maj == 1)

# I originally used the item variable with wording variation as the item variable for this analysis
# Use a broader item variable instead, which only uses the survey source (as Chris Claassen does), generally results in more dynamic (less flat) trends
trustgov.dem <- dplyr::select(trustgov.dem, -Item)
trustgov.dem <- dplyr::rename(trustgov.dem, Item = Project)

# Recode the value of Item to make the EB and AsiaBarometer data source identifiers more specific
# Initially, the grep code for item-country length identifiers below thought that many other observations were also from these sources
trustgov.dem$Item[trustgov.dem$Item=="EB"] <- "SSEB" # stands for "Standard and Special Eurobarometer"
trustgov.dem$Item[trustgov.dem$Item=="AsB"] <- "AsiaB"

# Remove data for the SSA region as there are very few observations for trust in government there
trustgov.dem <- subset(trustgov.dem, regpol6 != "Sub-Saharan Africa")

# Remove observations before 1990 because they are almost only from the US)
trustgov.dem <- trustgov.dem [!trustgov.dem$Year < 1990,]

# Set first year
year0gov = 1989 # = year before first year of available survey data
trustgov.dem = trustgov.dem[trustgov.dem$Year > year0gov,]

# Create item by country indicators
trustgov.dem = unite(trustgov.dem, ItemCnt, c(Item, Country), sep = "_", remove = FALSE)

# Create item by region indicators
trustgov.dem = unite(trustgov.dem, ItemReg, c(Item, regpol6), sep = "_", remove = FALSE)

## Prepare data for Stan
# Factorise
trustgov.dem$Country = as.factor(as.character(trustgov.dem$Country))
trustgov.dem$Item = as.factor(as.character(trustgov.dem$Item))
trustgov.dem$ItemCnt = as.factor(as.character(trustgov.dem$ItemCnt))
trustgov.dem$Year = trustgov.dem$Year-year0gov

# Extract data
n.items = length(unique(trustgov.dem$Item))
n.cntrys = length(unique(trustgov.dem$Country))
n.yrs = 2019-year0gov # estimates up to 2019
n.resp = dim(trustgov.dem)[1]
n.reg = length(unique(trustgov.dem$regpol6))
n.itm.cnt = length(unique(trustgov.dem$ItemCnt))
cntrys = as.numeric(factor(trustgov.dem$Country))
cnt.names = levels(trustgov.dem$Country)
items = as.numeric(factor(trustgov.dem$Item))
yrs = trustgov.dem$Year
itm.cnts = as.numeric(factor(trustgov.dem$ItemCnt))
mean.resp.log = logit(mean(trustgov.dem$Response))
regions = as.numeric(factor(trustgov.dem$regpol6))
reg.names = levels(factor(trustgov.dem$regpol6))

# Create item-country length indicator for items
item.ind.kp = rep(0, length(levels(trustgov.dem$ItemCnt)))
for(i in 1:length(levels(trustgov.dem$Item))) {
  item.ind.kp[grepl(levels(trustgov.dem$Item)[i], levels(trustgov.dem$ItemCnt))] = i
}
item.ind.len = sapply(lapply(levels(trustgov.dem$Item), function(x) grep(x, levels(trustgov.dem$ItemCnt))), length)

## Fit stan model

# Specify data for stan
dat.1 = list(N=n.resp, K=n.items, T=n.yrs, J=n.cntrys, L=n.reg, P=n.itm.cnt, jj=cntrys, ll=regions, tt=yrs, 
             pp=itm.cnts, kk=items, it_len=item.ind.len, 
             x=trustgov.dem$RespN, samp=trustgov.dem$Sample, mn_resp_log=mean.resp.log)
sapply(dat.1, summary)

# Parameters to save 
pars.1 = c("Sigma","Omega","sigma_delta","sigma_theta","phi","mu_lambda","lambda","gamm","delta",
           "theta","x_pred","log_lik")

# Iterations for MCMC simulation
n.iter = 1000
n.warm = 500
n.samp = n.iter - n.warm
n.chn = 4

# Compile model
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
gov.theta.m.df = data.frame(Region=rep(reg.names, each=n.yrs), 
                               Year=rep(1990:2019, times=n.reg), gov=theta.pe, 
                               gov_u95=theta.u95, gov_l95=theta.l95, gov_sd=theta.sd)

# remove estimates before first survey year and create a trimmed dataset
trustgov.dem$Region=trustgov.dem$regpol6
first.yr = data.frame(Region = levels(as.factor(trustgov.dem$Region)),
                      First_yr = as.vector(
                        by(trustgov.dem, trustgov.dem$Region, function(x) min(as.numeric(x$Year)) + year0gov)))
gov.theta.trim = merge(gov.theta.m.df, first.yr, by="Region", all.x=TRUE)
cnts = gov.theta.trim[gov.theta.trim$Year==2008, "Region"]
frst.yr = gov.theta.trim[gov.theta.trim$Year==2008, "First_yr"]
gov.theta.trim$gov_trim = gov.theta.trim$gov
gov.theta.trim$gov_trim = ifelse(gov.theta.trim$Year < gov.theta.trim$First_yr, NA, gov.theta.trim$gov_trim)
gov.theta.trim = gov.theta.trim[order(gov.theta.trim$Region, gov.theta.trim$Year), ]
gov.theta.trim = gov.theta.trim[!is.na(gov.theta.trim$gov_trim),]
gov.theta.trim$gov_trim = NULL

# save year point estimates
write.csv(gov.theta.trim, "gov_reg_est.csv", row.names=FALSE)

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

col_regions <- c(col_wena, col_eeca, col_lac, col_mena, col_ssa, col_ap)

gov.theta.trim$Region_f <- as.factor(gov.theta.trim$Region)
gov.theta.trim$Region_f <- factor(gov.theta.trim$Region_f,levels = 
                               c("W-Europe & N-America", "E-Europe & C-Asia", "L-America & Caribbean",
                                 "M-East & N-Africa", "Sub-Saharan Africa", "Asia & Pacific"))
cnts = levels(gov.theta.trim$Region)
n.cnts = length(cnts)
decadelabels <- c("1980", "'90", "'00", "'10", "2020")

gov_reg_plot_dem <- gov.theta.trim %>%
  ggplot(aes(x = Year, y = gov, color = Region_f)) + # this and next 2 are the bones
  geom_line() + # this is where you change the point estimate line colour
  scale_color_manual(values = col_regions) + 
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = gov_l95, ymax = gov_u95), alpha = 0.1, linetype = "dotted", size = 0.5) +
  facet_wrap(~Region_f) + # this makes a separate plot for each country
  theme(legend.position = "none") + # removes legend
  scale_x_continuous("",  breaks = c(1970, 1980, 1990, 2000, 2010, 2020), 
                     labels = c("1970", "'80", "'90", "'00", "'10", "'20")) + # this adds the labels to the breaks
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
gov_reg_plot_dem

ggsave("gov_reg_dem.png", gov_reg_plot_dem)

###########