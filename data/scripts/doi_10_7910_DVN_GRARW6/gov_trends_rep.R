###### Estimating Trust in Government, 1958-2022
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

# I originally used the item variable with wording variation as the item variable for this analysis
# Use a broader item variable instead, which only uses the survey source (as Chris Claassen does), generally results in more dynamic (less flat) trends
trustgov <- dplyr::select(trustgov, -Item)
trustgov <- dplyr::rename(trustgov, Item = Project)

# Recode the value of Item to make the EB and AsiaBarometer data source identifiers more specific
# Initially, the grep code for item-country length identifiers below thought that many other observations were also from these sources
trustgov$Item[trustgov$Item=="EB"] <- "SSEB" # stands for "Standard and Special Eurobarometer
trustgov$Item[trustgov$Item=="AsB"] <- "AsiaB" # stands for "Standard and Special Eurobarometer

# set first year
year0gov = 1957 # = year before first year of available survey data
trustgov = trustgov[trustgov$Year > year0gov,]

# create item by country indicators
trustgov = unite(trustgov, ItemCnt, c(Item, Country), sep = "_", remove = FALSE)

# create item by region indicators
trustgov = unite(trustgov, ItemReg, c(Item, regpol6), sep = "_", remove = FALSE)

# identify countries with few years of data
cnt.obs.years = rowSums(table(trustgov$Country, trustgov$Year) > 0)
sort(cnt.obs.years)

# run the next line to drop countries with less than 2 years of data
trustgov = trustgov[trustgov$Country %in% levels(factor(trustgov$Country))[cnt.obs.years > 1], ]
length(unique(trustgov$Country))

## Prepare data for stan

# factorise
trustgov$Country = as.factor(as.character(trustgov$Country))
trustgov$Item = as.factor(as.character(trustgov$Item))
trustgov$ItemCnt = as.factor(as.character(trustgov$ItemCnt))
trustgov$Year = trustgov$Year-year0gov

# extract data
n.items = length(unique(trustgov$Item))
n.cntrys = length(unique(trustgov$Country))
n.yrs = 2020-year0gov # estimates up to 2020
n.resp = dim(trustgov)[1]
n.itm.cnt = length(unique(trustgov$ItemCnt))
cntrys = as.numeric(factor(trustgov$Country))
cnt.names = levels(trustgov$Country)
items = as.numeric(factor(trustgov$Item))
yrs = trustgov$Year
itm.cnts = as.numeric(factor(trustgov$ItemCnt))
mean.resp.log = logit(mean(trustgov$Response))

# create item-country length indicator for items
item.ind.kp = rep(0, length(levels(trustgov$ItemCnt)))
for(i in 1:length(levels(trustgov$Item))) {
  item.ind.kp[grepl(levels(trustgov$Item)[i], levels(trustgov$ItemCnt))] = i
}
item.ind.len = sapply(lapply(levels(trustgov$Item), function(x) grep(x, levels(trustgov$ItemCnt))), length)

## Fit stan model

# specify data for stan
dat.1 = list(N=n.resp, K=n.items, T=n.yrs, J=n.cntrys, P=n.itm.cnt, jj=cntrys, tt=yrs, 
             pp=itm.cnts, kk=items, it_len=item.ind.len, 
             x=trustgov$RespN, samp=trustgov$Sample, mn_resp_log=mean.resp.log)
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
stan.mod = cmdstan_model('stan_mod6_v6_rep.stan')

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
            "phi","delta[23]","theta[31,35]")
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
theta.m.df = data.frame(Country=rep(cnt.names, each=n.yrs), 
                        Year=rep(1958:2020, times=n.cntrys), gov=theta.pe, 
                        gov_u95=theta.u95, gov_l95=theta.l95, gov_sd=theta.sd)

# remove estimates before first survey year and create a trimmed dataset
first.yr = data.frame(Country = levels(trustgov$Country),
                      First_yr = as.vector(
                        by(trustgov, trustgov$Country, function(x) min(as.numeric(x$Year)) + year0gov)))
theta.trim = merge(theta.m.df, first.yr, by="Country", all.x=TRUE)
cnts = theta.trim[theta.trim$Year==2008, "Country"]
frst.yr = theta.trim[theta.trim$Year==2008, "First_yr"]
theta.trim$gov_trim = theta.trim$gov
theta.trim$gov_trim = ifelse(theta.trim$Year < theta.trim$First_yr, NA, theta.trim$gov_trim)
theta.trim = theta.trim[order(theta.trim$Country, theta.trim$Year), ]
theta.trim = theta.trim[!is.na(theta.trim$gov_trim),]
theta.trim$gov_trim = NULL

# save country-year point estimates
write.csv(theta.trim, "gov_mood_est.csv", row.names=FALSE)
theta.trim = read.csv("gov_mood_est.csv")

# Restore year variable
trustgov$Year = trustgov$Year+year0gov

# merge estimates data with other variables
theta.trim = read.csv("gov_mood_est.csv")
trustgov.est <- merge(trustgov,theta.trim,by=c("Country","Year"))

write.csv(trustgov.est, "gov_mood2.csv", row.names=FALSE)

# plot trends
trustgov.est = read.csv("gov_mood2.csv")

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

cnts = levels(trustgov.est$Country)
n.cnts = length(cnts)
decadelabels <- c("'60", "'70", "'80", "'90", "'00", "'10", "'20")
decadebreaks <- c(1960, 1970, 1980, 1990, 2000, 2010, 2020)

trustgov.est$Country <- as.character(trustgov.est$Country)
trustgov.est$Country[trustgov.est$Country=="Dominican Republic"] <- "Dominican Rep."
trustgov.est$Country[trustgov.est$Country=="Trinidad and Tobago"] <- "Trinidad & Tobago"
trustgov.est$Country[trustgov.est$Country=="Sao Tome and Principe"] <- "Sao Tome & P."
trustgov.est$Country[trustgov.est$Country=="Bosnia and Herzegovina"] <- "Bosnia & Herz."
trustgov.est$Country[trustgov.est$Country=="North Macedonia"] <- "N. Macedonia"

wena_plot_gov <- trustgov.est %>% filter(regpol6 == "W-Europe & N-America") %>% #
  ggplot(aes(x = Year, y = gov)) +
  geom_line(color = col_gov) +
  theme_minimal() +
  geom_ribbon(aes(ymin = gov_l95, ymax = gov_u95), alpha = 0.1, linetype = "dashed",
              color = col_gov_ci, fill = col_gov_ci) +
  facet_wrap(~Country) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) +
  labs(title = "", y ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
wena_plot_gov
ggsave("gov_wena.png", wena_plot_gov)

eeca_plot_gov <- trustgov.est %>% filter(regpol6 == "E-Europe & C-Asia") %>%
  ggplot(aes(x = Year, y = gov)) +
  geom_line(color = col_gov) +
  theme_minimal() +
  geom_ribbon(aes(ymin = gov_l95, ymax = gov_u95), alpha = 0.1, linetype = "dashed",
              color = col_gov_ci, fill = col_gov_ci) +
  facet_wrap(~Country) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) +
  labs(title = "", y ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
eeca_plot_gov
ggsave("gov_eeca.png", eeca_plot_gov)

lac_plot_gov <- trustgov.est %>% filter(regpol6 == "L-America & Caribbean") %>%
  ggplot(aes(x = Year, y = gov)) +
  geom_line(color = col_gov) +
  theme_minimal() +
  geom_ribbon(aes(ymin = gov_l95, ymax = gov_u95), alpha = 0.1, linetype = "dashed",
              color = col_gov_ci, fill = col_gov_ci) +
  facet_wrap(~Country) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) +
  labs(title = "", y ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
lac_plot_gov
ggsave("gov_lac.png", lac_plot_gov)

mena_plot_gov <- trustgov.est %>% filter(regpol6 == "M-East & N-Africa") %>%
  ggplot(aes(x = Year, y = gov)) +
  geom_line(color = col_gov) +
  theme_minimal() +
  geom_ribbon(aes(ymin = gov_l95, ymax = gov_u95), alpha = 0.1, linetype = "dashed",
              color = col_gov_ci, fill = col_gov_ci) +
  facet_wrap(~Country) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) +
  labs(title = "", y ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
mena_plot_gov
ggsave("gov_mena.png", mena_plot_gov)

ssa_plot_gov <- trustgov.est %>% filter(regpol6 == "Sub-Saharan Africa") %>%
  ggplot(aes(x = Year, y = gov)) +
  geom_line(color = col_gov) +
  theme_minimal() +
  geom_ribbon(aes(ymin = gov_l95, ymax = gov_u95), alpha = 0.1, linetype = "dashed",
              color = col_gov_ci, fill = col_gov_ci) +
  facet_wrap(~Country) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) +
  labs(title = "", y ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
ssa_plot_gov
ggsave("gov_ssa.png", ssa_plot_gov)

aspac_plot_gov <- trustgov.est %>% filter(regpol6 == "Asia & Pacific") %>% # the filter() filters the region
  ggplot(aes(x = Year, y = gov)) + # this and next 2 are the bones
  geom_line(color = col_gov) + # this is where you change the point estimate line colour
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = gov_l95, ymax = gov_u95), alpha = 0.1, linetype = "dashed", 
              color = col_gov_ci, fill = col_gov_ci) + # fill = "colourname" will change the grey to whatever
  facet_wrap(~Country) + # this makes a separate plot for each country
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "", y ="") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
aspac_plot_gov
ggsave("gov_aspac.png", aspac_plot_gov)

############