###### Estimating Trust in alliament, 1973-2022
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
trustall = read.csv("trends_trust.csv")

## Edit data
# remove NAs
trustall = trustall[!trustall$Response==0, ]

# order
trustall = arrange(trustall, Country, Year)

# Include only countries which were liberal or electoral democracies in a majority of years included, according to the VDem Regions of the world measure
trustall <- subset(trustall, vdem_row_maj == 1)

# I originally used the item variable with wording variation as the item variable for this analysis
# Use a broader item variable instead, which only uses the survey source (as Chris Claassen does), generally results in more dynamic (less flat) trends
trustall <- dplyr::select(trustall, -Item)
trustall <- dplyr::rename(trustall, Item = Project)

# Recode the value of Item to make the EB and AsiaBarometer data source identifiers more specific
# Initially, the grep code for item-country length identifiers below thought that many other observations were also from these sources
trustall$Item[trustall$Item=="EB"] <- "SSEB" # stands for "Standard and Special Eurobarometer
trustall$Item[trustall$Item=="AsB"] <- "AsiaB" # stands for "Standard and Special Eurobarometer

#Remove observations before 1990, because they are few and unrepresentative - only in the WENA region and mostly in the US
trustall <- trustall [!trustall$Year < 1990,]

# set first year
year0all = 1989 # = year before first year of available survey data
trustall = trustall[trustall$Year > year0all,]

# create item by country indicators
trustall = unite(trustall, ItemCnt, c(Item, Country), sep = "_", remove = FALSE)

# create item by region indicators
trustall = unite(trustall, ItemReg, c(Item, regpol6), sep = "_", remove = FALSE)

# identify countries with few years of data
cnt.obs.years = rowSums(table(trustall$Country, trustall$Year) > 0)
sort(cnt.obs.years)

# run the next line to drop countries with less than 2 years of data
trustall = trustall[trustall$Country %in% levels(factor(trustall$Country))[cnt.obs.years > 1], ]
length(unique(trustall$Country))

## Prepare data for stan

# factorise
trustall$Country = as.factor(as.character(trustall$Country))
trustall$Item = as.factor(as.character(trustall$Item))
trustall$ItemCnt = as.factor(as.character(trustall$ItemCnt))
trustall$Year = trustall$Year-year0all

# extract data
n.items = length(unique(trustall$Item))
n.cntrys = length(unique(trustall$Country))
n.yrs = 2019-year0all # estimates up to 2019
n.resp = dim(trustall)[1]
n.itm.cnt = length(unique(trustall$ItemCnt))
cntrys = as.numeric(factor(trustall$Country))
cnt.names = levels(trustall$Country)
items = as.numeric(factor(trustall$Item))
yrs = trustall$Year
itm.cnts = as.numeric(factor(trustall$ItemCnt))
mean.resp.log = logit(mean(trustall$Response))

n.reg = length(unique(trustall$regpol6))
regions = as.numeric(factor(trustall$regpol6))
reg.names = levels(factor(trustall$regpol6))

n.type = length(unique(trustall$type))
type = as.numeric(factor(trustall$type))
type.names = levels(factor(trustall$type))

# create item-country length indicator for items
item.ind.kp = rep(0, length(levels(trustall$ItemCnt)))
for(i in 1:length(levels(trustall$Item))) {
  item.ind.kp[grepl(levels(trustall$Item)[i], levels(trustall$ItemCnt))] = i
}
item.ind.len = sapply(lapply(levels(trustall$Item), function(x) grep(x, levels(trustall$ItemCnt))), length)

## Fit stan model

# specify data for stan
dat.1 = list(N=n.resp, K=n.items, T=n.yrs, J=n.cntrys, L=n.reg, M=n.type, P=n.itm.cnt, jj=cntrys, ll=regions, mm=type, tt=yrs, 
             pp=itm.cnts, kk=items, it_len=item.ind.len, 
             x=trustall$RespN, samp=trustall$Sample, mn_resp_log=mean.resp.log)
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
stan.mod = cmdstan_model('stan_mod6_v6_all2.stan')

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
all.theta.m.df = data.frame(type=rep(type.names, each=n.yrs), 
                               Year=rep(1990:2019, times=n.type), all=theta.pe, 
                               all_u95=theta.u95, all_l95=theta.l95, all_sd=theta.sd)

# remove estimates before first survey year and create a trimmed dataset
first.yr = data.frame(type = levels(as.factor(trustall$type)),
                      First_yr = as.vector(
                        by(trustall, trustall$type, function(x) min(as.numeric(x$Year)) + year0all)))
all.theta.trim = merge(all.theta.m.df, first.yr, by="type", all.x=TRUE)
cnts = all.theta.trim[all.theta.trim$Year==2008, "type"]
frst.yr = all.theta.trim[all.theta.trim$Year==2008, "First_yr"]
all.theta.trim$all_trim = all.theta.trim$all
all.theta.trim$all_trim = ifelse(all.theta.trim$Year < all.theta.trim$First_yr, NA, all.theta.trim$all_trim)
all.theta.trim = all.theta.trim[order(all.theta.trim$type, all.theta.trim$Year), ]
all.theta.trim = all.theta.trim[!is.na(all.theta.trim$all_trim),]
all.theta.trim$all_trim = NULL

# save year point estimates
write.csv(all.theta.trim, "all_reg_est.csv", row.names=FALSE)

# plot trends
col1 = rgb(1, 0.55, 0, 1)
col2 = rgb(0, 0.45, 0.7, 1)

col_swd = rgb(0, 158, 115, 255, maxColorValue = 255)
col_swd_ci = rgb(0, 158, 115, 100, maxColorValue = 255)
col_parl = rgb(213, 94, 0, 255, maxColorValue = 255)
col_parl_ci = rgb(213, 94, 0, 100, maxColorValue = 255)
col_gov = rgb(220, 135, 0, 255, maxColorValue = 255)
col_gov_ci = rgb(220, 135, 0, 100, maxColorValue = 255)
col_polpar = rgb(230, 159, 0, 255, maxColorValue = 255)
col_polpar_ci = rgb(230, 159, 0, 100, maxColorValue = 255)

col_civil = rgb(0, 158, 115, 255, maxColorValue = 255)
col_civil_ci = rgb(0, 158, 115, 100, maxColorValue = 255)
col_leg = rgb(10, 128, 158, 255, maxColorValue = 255)
col_leg_ci = rgb(10, 128, 158, 100, maxColorValue = 255)
col_police = rgb(19,93,216, 255, maxColorValue = 255)
col_police_ci = rgb(19,93,216, 100, maxColorValue = 255)

col_types <- c(col_parl, col_gov, col_polpar, col_civil, col_leg, col_police)
col_types_ci <- c(col_parl_ci, col_gov_ci, col_polpar_ci, col_civil_ci, col_leg_ci, col_police_ci)

# Recode type variable for labels
all.theta.trim$type <- dplyr::recode(all.theta.trim$type, "gov" = "Government", "parl" = "Parliament",
                                     "polpar" = "Political parties", "civil" = "Civil service",
                                     "leg" = "Legal system", "police" = "Police")

all.theta.trim$type_f <- as.factor(all.theta.trim$type)
all.theta.trim$type_f <- factor(all.theta.trim$type_f,levels = 
                                    c("Parliament", "Government", "Political parties",
                                      "Civil service", "Legal system", "Police"))

trends_all_plot <- all.theta.trim %>%
  ggplot(aes(x = Year, y = all, color = type_f)) + # this and next 2 are the bones
  geom_line() + # this is where you change the point estimate line colour
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = all_l95, ymax = all_u95), alpha = 0.1, linetype = "dotted") + # fill = "colourname" will change the grey to whatever
  facet_wrap(~type_f) + # this makes a separate plot for each type
  theme(legend.position = "none") + # removes legend
  scale_x_continuous("", breaks = c(1980, 1990, 2000, 2010, 2020), 
                     labels = c("1980","'90", "'00", "'10", "'20")) + # this adds the labels to the breaks
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  scale_color_manual(values = col_types)
trends_all_plot

ggsave("trends_trust_all.png", trends_all_plot, dpi = 600)

trends_all_plot_bw <- all.theta.trim %>%
  ggplot(aes(x = Year, y = all)) + # this and next 2 are the bones
  geom_line() + # this is where you change the point estimate line colour
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = all_l95, ymax = all_u95), alpha = 0.1, linetype = "dotted") + # fill = "colourname" will change the grey to whatever
  facet_wrap(~type_f) + # this makes a separate plot for each type
  theme(legend.position = "none") + # removes legend
  scale_x_continuous("", breaks = c(1980, 1990, 2000, 2010, 2020), 
                     labels = c("1980","'90", "'00", "'10", "'20")) + # this adds the labels to the breaks
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
trends_all_plot_bw

ggsave("trends_trust_all_bw.png", trends_all_plot_bw)
