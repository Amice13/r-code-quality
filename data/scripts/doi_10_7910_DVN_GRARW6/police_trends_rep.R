###### Estimating Trust in the police
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
library(posterior)
library(rio)
library(tidyverse)

# options
options("cmdstanr_verbose" = TRUE)
options(mc.cores = parallel::detectCores())

# read trust data
trustpolice = read.csv("trends_police.csv")

## Edit data
# remove NAs
trustpolice = trustpolice[!trustpolice$Response==0, ]

# order
trustpolice = arrange(trustpolice, Country, Year)

# set first year
year0police = 1980 # = year before first year of available survey data
trustpolice = trustpolice[trustpolice$Year > year0police,]

# I originally used the item variable with wording variation as the item variable for this analysis
# This resulted in many item-country combinations only including one observation and thus there were very high rhat values
# Try with a broader item variable instead, which only uses the survey source (as Chris Claassen does)
trustpolice <- dplyr::select(trustpolice, -Item)
trustpolice <- dplyr::rename(trustpolice, Item = Project)

# Recode the value of Item to make the EB and AsiaBarometer data source identifiers more specific
# Initially, the grep code for item-country length identifiers below thought that many other observations were also from these sources
trustpolice$Item[trustpolice$Item=="EB"] <- "SSEB" # stands for "Standard and Special Eurobarometer
trustpolice$Item[trustpolice$Item=="AsB"] <- "AsiaB" # stands for "Standard and Special Eurobarometer

# create item by country indicators
trustpolice = unite(trustpolice, ItemCnt, c(Item, Country), sep = "_", remove = FALSE)

# create item by region indicators
trustpolice = unite(trustpolice, ItemReg, c(Item, regpol6), sep = "_", remove = FALSE)

# identify countries with few years of data
cnt.obs.years = rowSums(table(trustpolice$Country, trustpolice$Year) > 0)
sort(cnt.obs.years)

# run the next line to drop countries with less than 2 years of data
trustpolice = trustpolice[trustpolice$Country %in% levels(factor(trustpolice$Country))[cnt.obs.years > 1], ]
length(unique(trustpolice$Country))

## Prepare data for stan

# factorise
trustpolice$Country = as.factor(as.character(trustpolice$Country))
trustpolice$Item = as.factor(as.character(trustpolice$Item))
trustpolice$ItemCnt = as.factor(as.character(trustpolice$ItemCnt))
trustpolice$Year = trustpolice$Year-year0police

# extract data
n.items = length(unique(trustpolice$Item))
n.cntrys = length(unique(trustpolice$Country))
n.yrs = 2020-year0police # estimates up to 2020
n.resp = dim(trustpolice)[1]
n.itm.cnt = length(unique(trustpolice$ItemCnt))
cntrys = as.numeric(factor(trustpolice$Country))
cnt.names = levels(trustpolice$Country)
items = as.numeric(factor(trustpolice$Item))
yrs = trustpolice$Year
itm.cnts = as.numeric(factor(trustpolice$ItemCnt))
mean.resp.log = logit(mean(trustpolice$Response))

# create item-country length indicator for items
item.ind.kp = rep(0, length(levels(trustpolice$ItemCnt)))
for(i in 1:length(levels(trustpolice$Item))) {
  item.ind.kp[grepl(levels(trustpolice$Item)[i], levels(trustpolice$ItemCnt))] = i
}
item.ind.len = sapply(lapply(levels(trustpolice$Item), function(x) grep(x, levels(trustpolice$ItemCnt))), length)

## Fit stan model

# specify data for stan
dat.1 = list(N=n.resp, K=n.items, T=n.yrs, J=n.cntrys, P=n.itm.cnt, jj=cntrys, tt=yrs, 
             pp=itm.cnts, kk=items, it_len=item.ind.len, 
             x=trustpolice$RespN, samp=trustpolice$Sample, mn_resp_log=mean.resp.log)
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
  adapt_delta = 0.8, 
  max_treedepth = 15,
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
            "phi","delta[23]","theta[31,121]")
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
                        Year=rep(1981:2020, times=n.cntrys), police=theta.pe, 
                        police_u95=theta.u95, police_l95=theta.l95, police_sd=theta.sd)

# remove estimates before first survey year and create a trimmed dataset
first.yr = data.frame(Country = levels(trustpolice$Country),
                      First_yr = as.vector(
                        by(trustpolice, trustpolice$Country, function(x) min(as.numeric(x$Year)) + year0police)))
theta.trim = merge(theta.m.df, first.yr, by="Country", all.x=TRUE)
cnts = theta.trim[theta.trim$Year==2008, "Country"]
frst.yr = theta.trim[theta.trim$Year==2008, "First_yr"]
theta.trim$police_trim = theta.trim$police
theta.trim$police_trim = ifelse(theta.trim$Year < theta.trim$First_yr, NA, theta.trim$police_trim)
theta.trim = theta.trim[order(theta.trim$Country, theta.trim$Year), ]
theta.trim = theta.trim[!is.na(theta.trim$police_trim),]
theta.trim$police_trim = NULL

# save country-year point estimates
write.csv(theta.trim, "police_mood_est.csv", row.names=FALSE)

# Restore year variable
trustpolice$Year = trustpolice$Year+year0police

# merge estimates data with other variables
theta.trim = read.csv("police_mood_est.csv")
trustpolice.est <- merge(trustpolice,theta.trim,by=c("Country","Year"))

write.csv(trustpolice.est, "police_mood2.csv", row.names=FALSE)

# plot trends
trustpolice.est = read.csv("police_mood2.csv")

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

decadelabels <- c("'70", "'80", "'90", "'00", "'10", "'20")
decadebreaks <- c(1970, 1980, 1990, 2000, 2010, 2020)
trustpolice.est$Country <- as.character(trustpolice.est$Country)

trustpolice.est$Country[trustpolice.est$Country=="Dominican Republic"] <- "Dominican Rep."
trustpolice.est$Country[trustpolice.est$Country=="Trinidad and Tobago"] <- "Trinidad & Tobago"
trustpolice.est$Country[trustpolice.est$Country=="Bosnia and Herzegovina"] <- "Bosnia & Herz."
trustpolice.est$Country[trustpolice.est$Country=="North Macedonia"] <- "N. Macedonia"
trustpolice.est$Country[trustpolice.est$Country=="Sao Tome and Principe"] <- "Sao Tome & P."

wena_plot_police <- trustpolice.est %>% filter(regpol6 == "W-Europe & N-America") %>% #
  ggplot(aes(x = Year, y = police)) +
  geom_line(color = col_police) +
  theme_minimal() +
  geom_ribbon(aes(ymin = police_l95, ymax = police_u95), alpha = 0.1, linetype = "dashed",
              color = col_police_ci, fill = col_police_ci) +
  facet_wrap(~Country) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) +
  labs(title = "", y ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
wena_plot_police
ggsave("police_wena.png", wena_plot_police)

eeca_plot_police <- trustpolice.est %>% filter(regpol6 == "E-Europe & C-Asia") %>%
  ggplot(aes(x = Year, y = police)) +
  geom_line(color = col_police) +
  theme_minimal() +
  geom_ribbon(aes(ymin = police_l95, ymax = police_u95), alpha = 0.1, linetype = "dashed",
              color = col_police_ci, fill = col_police_ci) +
  facet_wrap(~Country) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) +
  labs(title = "", y ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
eeca_plot_police
ggsave("police_eeca.png", eeca_plot_police)

lac_plot_police <- trustpolice.est %>% filter(regpol6 == "L-America & Caribbean") %>%
  ggplot(aes(x = Year, y = police)) +
  geom_line(color = col_police) +
  theme_minimal() +
  geom_ribbon(aes(ymin = police_l95, ymax = police_u95), alpha = 0.1, linetype = "dashed",
              color = col_police_ci, fill = col_police_ci) +
  facet_wrap(~Country) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) +
  labs(title = "", y ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
lac_plot_police
ggsave("police_lac.png", lac_plot_police)

mena_plot_police <- trustpolice.est %>% filter(regpol6 == "M-East & N-Africa") %>%
  ggplot(aes(x = Year, y = police)) +
  geom_line(color = col_police) +
  theme_minimal() +
  geom_ribbon(aes(ymin = police_l95, ymax = police_u95), alpha = 0.1, linetype = "dashed",
              color = col_police_ci, fill = col_police_ci) +
  facet_wrap(~Country) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) +
  labs(title = "", y = "") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
mena_plot_police
ggsave("police_mena.png", mena_plot_police)

ssa_plot_police <- trustpolice.est %>% filter(regpol6 == "Sub-Saharan Africa") %>%
  ggplot(aes(x = Year, y = police)) +
  geom_line(color = col_police) +
  theme_minimal() +
  geom_ribbon(aes(ymin = police_l95, ymax = police_u95), alpha = 0.1, linetype = "dashed",
              color = col_police_ci, fill = col_police_ci) +
  facet_wrap(~Country) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) +
  labs(title = "", y ="") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
ssa_plot_police
ggsave("police_ssa.png", ssa_plot_police)

aspac_plot_police <- trustpolice.est %>% filter(regpol6 == "Asia & Pacific") %>% # the filter() filters the region
  ggplot(aes(x = Year, y = police)) + # this and next 2 are the bones
  geom_line(color = col_police) + # this is where you change the point estimate line colour
  theme_minimal() + # geom_ribbon() adds the CIs
  geom_ribbon(aes(ymin = police_l95, ymax = police_u95), alpha = 0.1, linetype = "dashed", 
              color = col_police_ci, fill = col_police_ci) + # fill = "colourname" will change the grey to whatever
  facet_wrap(~Country) + # this makes a separate plot for each country
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7)) +
  scale_x_continuous("", breaks = decadebreaks,
                     labels = decadelabels) +
  scale_y_continuous("", breaks = c(-2, 0, 2, 4)) + # y axis to show these
  labs(title = "", y ="") + # adds a title
  theme(plot.background = element_rect(fill = 'white', colour = 'white'))
aspac_plot_police
ggsave("police_aspac.png", aspac_plot_police)

###########