library(dplyr)
library(ggplot2)
library(gridExtra)
library(rstan)
library(tidyr)
library(data.table)
### Finalized base model and results for revision

# load data files
train_data_04_revision <- readRDS("train_data_04_revision.rds")
train_data_514_revision <- readRDS("train_data_514_revision.rds")
map_data_revision <- readRDS("map_data_revision.rds")
load("who_ped_est_2019.rds")
regions <- readRDS("regions.rds")

## USE RSTAN TO RUN OPTIMIZATION

options(mc.cores = parallel::detectCores())

## PRIOR SETTING

# function for calculating gamma parameters, from Nick
gammapar <- function(tgt) {
  tgt <- as.numeric(tgt)
  mn <- tgt[1]; cir <- (tgt[3]-tgt[2])
  xopt <- function(b,mn,cir) {
    cir2 <- qgamma(c(1,39)/40,mn*b,b); 
    cir2 <- cir2[2]-cir2[1]
    (cir2-cir)^2 }
  zz <- optimize(xopt,c(0.1,100000),mn=mn,cir=cir)$minimum
  c(zz*mn,zz) }

# function for calculating beta parameters, from Nick
betapar <- function(tgt) {
  tgt <- as.numeric(tgt)
  mn <- tgt[1]; cir <- (tgt[3]-tgt[2])
  xopt <- function(xx,mn=mn,cir=cir) {
    cir2 <- qbeta(c(1,39)/40,xx*(mn/(1-mn)),xx); 
    cir2 <- cir2[2]-cir2[1]
    sum((cir2-cir)^2) }
  zz <- optimize(xopt,c(0.2,100000),mn=mn,cir=cir)$minimum
  bp <-  c(zz*(mn/(1-mn)),zz)
  if(sum(abs(bp-1))<0.2) { c(1,1) } else { bp }  }

# extract sums of contacts for each country and age group
contact_means <- unique(map_data_revision[, c("country", "age_group", "contact_sum")])

# calculate means for infection probability parameter in both age groups
infect_means_2021 <- 12/mean(aggregate(contact_means$contact_sum, by=list(contact_means$age_group), mean)[, 2])

# calculate lower bounds for infection probability parameter in both age groups
infect_lowers_2021 <- 6/mean(aggregate(contact_means$contact_sum, by=list(contact_means$age_group), mean)[, 2])

# calculate upper bounds for infection probability parameter in both age groups
infect_uppers_2021 <- 15/mean(aggregate(contact_means$contact_sum, by=list(contact_means$age_group), mean)[, 2])

# set priors
infect_priors <- betapar(c(infect_means_2021[1], infect_lowers_2021[1], infect_uppers_2021[1]))
active_priors_04 <- betapar(c(0.19, 0.084, 0.374)) # this is updated
active_priors_514 <- betapar(c(0.089, 0.049, 0.161)) # this is updated

dur_priors <- gammapar(c(3, 2.5, 3.5))
fract_priors <- betapar(c(0.25, 0.17, 0.33))
hiv_rr_priors <- gammapar(c(7.9, 4.5, 13.7))
pem_rr_priors <- gammapar(c(4, 2, 6))
art_rr_priors <- gammapar(c(0.3, 0.21, 0.39))

rr_ped_detect_priors <- betapar(c(0.9, 0.8, 0.95))  #~~  new parameter

## OPTIMIZATION

# turn on autowrite
rstan_options(auto_write = TRUE)

# declare inputs
datList_art <- list(
  N_obs = nrow(train_data_04_revision),
  cat1_start = 1,
  cat1_end = 98,
  cat2_start = 99,
  cat2_end = 168,
  cat3_start = 169,
  cat3_end = 189,
  pop_04 = train_data_04_revision$pop,
  pop_514 = train_data_514_revision$pop,
  p_tx_it = pmin(0.99, train_data_04_revision$p_tx_it), 
  hiv_04 = train_data_04_revision$hiv,
  hiv_514 = train_data_514_revision$hiv,
  pem_04 = train_data_04_revision$pem,
  pem_514 = train_data_514_revision$pem,
  bcg_04 = train_data_04_revision$bcg,
  bcg_514 = train_data_514_revision$bcg,
  art = train_data_04_revision$art_coverage,
  contacts_sum_04 = train_data_04_revision$contact_sum,
  contacts_sum_514 = train_data_514_revision$contact_sum, 
  contacts_inc_04 = train_data_04_revision$contacts_inc,
  contacts_inc_514 = train_data_514_revision$contacts_inc,
  who_cases_04 = train_data_04_revision$newrel_04_total,
  who_cases_514 = train_data_514_revision$newrel_514_total,
  mn = c(0.31, 0.68, 0.77),
  infect_a = infect_priors[[1]],
  infect_b = infect_priors[[2]],
  active_04_a = active_priors_04[[1]],
  active_04_b = active_priors_04[[2]],
  active_514_a = active_priors_514[[1]],
  active_514_b = active_priors_514[[2]],
  dur_untx_a = dur_priors[[1]],
  dur_untx_b = dur_priors[[2]],
  fract_early_a = fract_priors[[1]],
  fract_early_b = fract_priors[[2]],
  inv_sqrt_phi_sd = 1,
  hiv_rr_a = hiv_rr_priors[[1]],
  hiv_rr_b = hiv_rr_priors[[2]],
  pem_rr_a = pem_rr_priors[[1]],
  pem_rr_b = pem_rr_priors[[2]],
  art_rr_a = art_rr_priors[[1]],
  art_rr_b = art_rr_priors[[2]],
  rr_ped_detect_a = rr_ped_detect_priors[[1]],  #~~ 
  rr_ped_detect_b = rr_ped_detect_priors[[2]],  #~~
  shape_rate = 23)

# fit model
source("stan_underreporting_2021_revision_base.stan")
fit_stan_underreporting_art <- rstan::stan(
  model_code = stan_code, 
  data    = datList_art,
  seed    = 123,
  chains  = 4,
  iter    = 11000,
  warmup  = 10000)

## ESTIMATION USING MONTE CARLO SIMULATION

# extract samples from fitted model 
samps_2021_art <- extract(fit_stan_underreporting_art)

#### 1 Create the array of unadjusted results

# define countries and years of interest
map_countries_art <- unique(map_data_revision$country)
years <- 2013:2019

# define array dimensions
n_country <- length(unique(map_countries_art))  
n_year    <- length(years)    
n_age     <- 2    
n_sim     <- 4000 

# declare array
all_res_art <- array(NA,dim=c(n_country, n_year, n_age, n_sim), 
                          dimnames = list(map_countries_art, years, c("0-4", "5-14"), 1:4000))
all_res_art_adj <- all_res_art

#### 2 Fill in array
# iterate through countries
for(i in 1:n_country){
  
  # iterate through years
  for(j in 1:n_year){
    
    # select data for given country and year  
    map_data_sub_04 <- subset(map_data_revision, country==map_countries_art[i] & year==years[j] & age_group=="0-4")
    map_data_sub_514 <- subset(map_data_revision, country==map_countries_art[i] & year==years[j] & age_group=="5-14")
    map_data_sub_04$p_tx_it <- min(c(0.99,map_data_sub_04$p_tx_it))
    map_data_sub_514$p_tx_it <- min(c(0.99,map_data_sub_514$p_tx_it))
  
    # choose appropriate rate ratio
    if (map_data_sub_04$lat_cat[1]=="above40"){
      
      mn = 0.31
      
    } else if (map_data_sub_04$lat_cat[1]=="20to40"){
      
      mn = 0.68
      
    } else {
      
      mn = 0.77
      
    }
      
      duration = samps_2021_art[["dur_untx"]] * (1 - map_data_sub_04$p_tx_it * (1 - samps_2021_art[["fract_early"]]));
      
      # calculate 0-4 cases
      all_res_art[i,j,1,] <- (map_data_sub_04$pop * samps_2021_art[["active_04"]] * 
                                 (1 - map_data_sub_04$hiv + map_data_sub_04$hiv*(1-map_data_sub_04$art) * samps_2021_art[["hiv_rr"]] +
                                 map_data_sub_04$hiv*map_data_sub_04$art * samps_2021_art[["hiv_rr"]] * samps_2021_art[["art_rr"]]) *
                                      (1 - map_data_sub_04$pem + samps_2021_art[["pem_rr"]] * map_data_sub_04$pem) *
                                      (1 - map_data_sub_04$bcg + samps_2021_art[["z"]] * mn * map_data_sub_04$bcg) *
                                      (map_data_sub_04$contacts_inc/map_data_sub_04$contact_sum) *
                                      ((1 - exp(-samps_2021_art[["infect"]] * duration * map_data_sub_04$contact_sum * samps_2021_art[["sat"]])) / samps_2021_art[["sat"]]))
      
      # calculate 5-14 cases
      all_res_art[i,j,2,] <- (map_data_sub_514$pop * samps_2021_art[["active_514"]] *
                                 (1 - map_data_sub_514$hiv + map_data_sub_514$hiv*(1-map_data_sub_514$art) * samps_2021_art[["hiv_rr"]] + 
                                    map_data_sub_514$hiv*map_data_sub_514$art * samps_2021_art[["hiv_rr"]] * samps_2021_art[["art_rr"]]) *
                                      (1 - map_data_sub_514$pem + samps_2021_art[["pem_rr"]] * map_data_sub_514$pem) *
                                      (1 - map_data_sub_514$bcg + samps_2021_art[["z"]] * mn * map_data_sub_514$bcg) *
                                      (map_data_sub_514$contacts_inc/map_data_sub_514$contact_sum) *
                                      ((1 - exp(-samps_2021_art[["infect"]] * duration * map_data_sub_514$contact_sum * samps_2021_art[["sat"]])) / samps_2021_art[["sat"]]))
    
    # adjust estimates
    all_res_art_adj[i,j,1, ] <- all_res_art[i,j,1, ] * max(1, map_data_sub_04$newrel_04_total   / mean(all_res_art[i,j,1, ]) )
    all_res_art_adj[i,j,2, ] <- all_res_art[i,j,2, ] * max(1, map_data_sub_514$newrel_514_total / mean(all_res_art[i,j,2, ]) )
    
  }
  
  print(map_data_sub_04$country)
  
}

#### 4 Perform whatever analyses you want. For example....

# replace NA values with 0 (Grenada 2014)
all_res_art_adj[which(is.na(all_res_art_adj))] <- 0

#a  below calculates mean and interval by country, year and age group (dim())
new_res_art1 <- apply(all_res_art_adj,1:3,function(x) c(mean(x),quantile(x,c(1,39)/40)))
dim(new_res_art1) # first dimension is the 3 calculated statistics

# extract regions for analysis countries
map_data_revision <- subset(regions, country %in% map_countries_art)

# extract names of countries in each region
AFR_countries <- unique(map_data_revision$country[which(map_data_revision$g_whoregion=="AFR")])
AMR_countries <- unique(map_data_revision$country[which(map_data_revision$g_whoregion=="AMR")])
EMR_countries <- unique(map_data_revision$country[which(map_data_revision$g_whoregion=="EMR")])
EUR_countries <- unique(map_data_revision$country[which(map_data_revision$g_whoregion=="EUR")])
SEA_countries <- unique(map_data_revision$country[which(map_data_revision$g_whoregion=="SEA")])
WPR_countries <- unique(map_data_revision$country[which(map_data_revision$g_whoregion=="WPR")])

# calculate mean and interval by country, year, and age group for each region
AFR_all_res_art_adj <- all_res_art_adj[AFR_countries,,,]
AMR_all_res_art_adj <- all_res_art_adj[AMR_countries,,,]
EMR_all_res_art_adj <- all_res_art_adj[EMR_countries,,,]
EUR_all_res_art_adj <- all_res_art_adj[EUR_countries,,,]
SEA_all_res_art_adj <- all_res_art_adj[SEA_countries,,,]
WPR_all_res_art_adj <- all_res_art_adj[WPR_countries,,,]

# sum countries in each region
AFR_temp_art_age <- apply(AFR_all_res_art_adj,c(2,3,4),sum)
AMR_temp_art_age <- apply(AMR_all_res_art_adj,c(2,3,4),sum)
EMR_temp_art_age <- apply(EMR_all_res_art_adj,c(2,3,4),sum)
EUR_temp_art_age <- apply(EUR_all_res_art_adj,c(2,3,4),sum)
SEA_temp_art_age <- apply(SEA_all_res_art_adj,c(2,3,4),sum)
WPR_temp_art_age <- apply(WPR_all_res_art_adj,c(2,3,4),sum)

# calculate mean and interval by age group and year for each region - TABLE 2
AFR_new_res_art_age <- apply(AFR_temp_art_age, 1:2, function(x) c(mean(x),quantile(x,c(1,39)/40)))
AMR_new_res_art_age <- apply(AMR_temp_art_age, 1:2, function(x) c(mean(x),quantile(x,c(1,39)/40)))
EMR_new_res_art_age <- apply(EMR_temp_art_age, 1:2, function(x) c(mean(x),quantile(x,c(1,39)/40)))
EUR_new_res_art_age <- apply(EUR_temp_art_age, 1:2, function(x) c(mean(x),quantile(x,c(1,39)/40)))
SEA_new_res_art_age <- apply(SEA_temp_art_age, 1:2, function(x) c(mean(x),quantile(x,c(1,39)/40)))
WPR_new_res_art_age <- apply(WPR_temp_art_age, 1:2, function(x) c(mean(x),quantile(x,c(1,39)/40)))

#c  below sums globally then does the same thing (mean and interval by year) - TABLE 2
temp_art_2 <- apply(all_res_art_adj,c(2,4),sum)
new_res_art3 <- apply(temp_art_2,1,function(x) c(mean(x),quantile(x,c(1,39)/40)))
dim(new_res_art3) # first dimension is the 3 calculated statistics

# calculate mean and interval by year for each region - TABLE 2
AFR_temp_art_2 <- apply(all_res_art_adj[AFR_countries,,,],c(2,4),sum)
AFR_new_res_art3 <- apply(AFR_temp_art_2, 1, function(x) c(mean(x),quantile(x,c(1,39)/40)))
AMR_temp_art_2 <- apply(all_res_art_adj[AMR_countries,,,],c(2,4),sum)
AMR_new_res_art3 <- apply(AMR_temp_art_2, 1, function(x) c(mean(x),quantile(x,c(1,39)/40)))
EMR_temp_art_2 <- apply(all_res_art_adj[EMR_countries,,,],c(2,4),sum)
EMR_new_res_art3 <- apply(EMR_temp_art_2, 1, function(x) c(mean(x),quantile(x,c(1,39)/40)))
EUR_temp_art_2 <- apply(all_res_art_adj[EUR_countries,,,],c(2,4),sum)
EUR_new_res_art3 <- apply(EUR_temp_art_2, 1, function(x) c(mean(x),quantile(x,c(1,39)/40)))
SEA_temp_art_2 <- apply(all_res_art_adj[SEA_countries,,,],c(2,4),sum)
SEA_new_res_art3 <- apply(SEA_temp_art_2, 1, function(x) c(mean(x),quantile(x,c(1,39)/40)))
WPR_temp_art_2 <- apply(all_res_art_adj[WPR_countries,,,],c(2,4),sum)
WPR_new_res_art3 <- apply(WPR_temp_art_2, 1, function(x) c(mean(x),quantile(x,c(1,39)/40)))

# sum globally by age group - TABLE 2
temp_art_3 <- apply(all_res_art_adj,c(2,3,4),sum)
new_res_art_4 <- apply(temp_art_3, 1:2, function(x) c(mean(x),quantile(x,c(1,39)/40)))

#b  below sums age groups then does the same thing (mean and interval by country and year)
temp_art <- apply(all_res_art_adj,c(1,2,4),sum)
new_res_art2 <- apply(temp_art,1:2,function(x) c(mean(x),quantile(x,c(1,39)/40)))
dim(new_res_art2) # first dimension is the 3 calculated statistics

# transpose 2019 data
new_res_art2_t <- as.data.frame(t(new_res_art2[,,"2019"]))
rownames(new_res_art2_t) <- colnames(new_res_art2[,,"2019"])
colnames(new_res_art2_t) <- rownames(new_res_art2[,,"2019"])

# add country column 
new_res_art2_t$country <- rownames(new_res_art2_t)

# extract 2019 data
sub2019_revision <- subset(map_data_revision, year==2019)

# sum 2019 pediatric cases by country 
sub2019_oneage_revision <- sub2019_revision  %>% 
  group_by(country) %>% 
  summarise(total_notif = sum(who_cases))

# insert total pediatric cases 
new_res_art2_t <- merge(new_res_art2_t, sub2019_oneage_revision, by="country", all.x=TRUE)

# calculate reported cases as percentage of estimated incidence
new_res_art2_t$rpe_mean <- new_res_art2_t$total_notif/new_res_art2_t$Var.2
new_res_art2_t$rpe_lower <- new_res_art2_t$total_notif/new_res_art2_t$`97.5%`
new_res_art2_t$rpe_upper <- new_res_art2_t$total_notif/new_res_art2_t$`2.5%`

# calculate mean and interval by country and year for each region
AFR_temp_art <- temp_art[AFR_countries, ,]
AFR_new_res_art2 <- apply(AFR_temp_art,1:2,function(x) c(mean(x),quantile(x,c(1,39)/40)))
AMR_temp_art <- temp_art[AMR_countries, ,]
AMR_new_res_art2 <- apply(AMR_temp_art,1:2,function(x) c(mean(x),quantile(x,c(1,39)/40)))
EMR_temp_art <- temp_art[EMR_countries, ,]
EMR_new_res_art2 <- apply(EMR_temp_art,1:2,function(x) c(mean(x),quantile(x,c(1,39)/40)))
EUR_temp_art <- temp_art[EUR_countries, ,]
EUR_new_res_art2 <- apply(EUR_temp_art,1:2,function(x) c(mean(x),quantile(x,c(1,39)/40)))
SEA_temp_art <- temp_art[SEA_countries, ,]
SEA_new_res_art2 <- apply(SEA_temp_art,1:2,function(x) c(mean(x),quantile(x,c(1,39)/40)))
WPR_temp_art <- temp_art[WPR_countries, ,]
WPR_new_res_art2 <- apply(WPR_temp_art,1:2,function(x) c(mean(x),quantile(x,c(1,39)/40)))

# extract estimates by country and region
AFR_2019_est <- as.data.frame(AFR_new_res_art2[1,,"2019"])
AFR_2019_est$country <- rownames(AFR_2019_est)
AFR_2019_est$region <- "AFR"
AMR_2019_est <- as.data.frame(AMR_new_res_art2[1,,"2019"])
AMR_2019_est$country <- rownames(AMR_2019_est)
AMR_2019_est$region <- "AMR"
EMR_2019_est <- as.data.frame(EMR_new_res_art2[1,,"2019"])
EMR_2019_est$country <- rownames(EMR_2019_est)
EMR_2019_est$region <- "EMR"
EUR_2019_est <- as.data.frame(EUR_new_res_art2[1,,"2019"])
EUR_2019_est$country <- rownames(EUR_2019_est)
EUR_2019_est$region <- "EUR"
SEA_2019_est <- as.data.frame(SEA_new_res_art2[1,,"2019"])
SEA_2019_est$country <- rownames(SEA_2019_est)
SEA_2019_est$region <- "SEA"
WPR_2019_est <- as.data.frame(WPR_new_res_art2[1,,"2019"])
WPR_2019_est$country <- rownames(WPR_2019_est)
WPR_2019_est$region <- "WPR"

# rename columns
colnames(AFR_2019_est)[1] <- "inc_mean"
colnames(AMR_2019_est)[1] <- "inc_mean"
colnames(EMR_2019_est)[1] <- "inc_mean"
colnames(EUR_2019_est)[1] <- "inc_mean"
colnames(SEA_2019_est)[1] <- "inc_mean"
colnames(WPR_2019_est)[1] <- "inc_mean"

# combine dataframes
regions_2019_est_revision <- rbindlist(list(AFR_2019_est, AMR_2019_est, EMR_2019_est, EUR_2019_est, SEA_2019_est,
                                   WPR_2019_est))

# insert total pediatric cases
regions_2019_est_revision <- merge(regions_2019_est_revision, sub2019_oneage_revision, by="country", all.x=TRUE)
regions_2019_est_revision <- data.frame(regions_2019_est_revision)

pdfnam <- "Figure_3_main_text.pdf"
pdf(file=pdfnam,width=7.5, height=5) 
options("scipen"=100, "digits"=4)
# plot country-specific incidence estimates versus WHO case notifications by WHO region - FIGURE 3
ggplot(subset(regions_2019_est_revision, total_notif > 0), aes(x=total_notif, y=inc_mean, shape=region, color=region)) +
  geom_point() + #ggtitle("Model Estimates vs. WHO Notifications by WHO Region") + 
  geom_abline(aes(intercept=0, slope=1, linetype="Line of equality"), color="grey5") +
  geom_abline(aes(intercept=c(log10(0.1)), slope=1, linetype="10-times difference in values"), color="grey5") + 
  geom_abline(aes(intercept=c(log10(10)), slope=1, linetype="10-times difference in values"), color="grey5") + 
  geom_abline(aes(intercept=c(log10(0.5)), slope=1, linetype="2-times difference in values"), color="grey5") + 
  geom_abline(aes(intercept=c(log10(2)), slope=1, linetype="2-times difference in values"), color="grey5") + 
  scale_x_continuous(limits=c(0.8, 250000), trans='log10') + 
  scale_y_continuous(limits=c(0.8, 250000), trans='log10') + 
  xlab("TB case notifications, log-scale") + ylab("Modeled number of TB cases, log-scale") + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank()) +
  scale_colour_discrete(labels=c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", 
                                 "Western Pacific")) +
  scale_shape_discrete(labels=c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", 
                                "Western Pacific"), ) +
  scale_linetype_manual(breaks=c("Line of equality", "2-times difference in values", "10-times difference in values"),
                        values=c("solid", "dashed", "dotted")) +
  guides(linetype = guide_legend(order = 2), shape=guide_legend(order = 1), colour=guide_legend(order=1))

dev.off();system(paste("open", pdfnam))

# calculate total WHO notifications by region by year
region_notif <- map_data_revision %>%
  group_by(g_whoregion, year) %>%
  summarise(region_notif = sum(who_cases))

# reshape region estimates by year
AFR_years <- transpose(as.data.frame(AFR_new_res_art3))
AFR_years$year <- years
AFR_years$region <- "AFR"
AMR_years <- transpose(as.data.frame(AMR_new_res_art3))
AMR_years$year <- years
AMR_years$region <- "AMR"
EMR_years <- transpose(as.data.frame(EMR_new_res_art3))
EMR_years$year <- years
EMR_years$region <- "EMR"
EUR_years <- transpose(as.data.frame(EUR_new_res_art3))
EUR_years$year <- years
EUR_years$region <- "EUR"
SEA_years <- transpose(as.data.frame(SEA_new_res_art3))
SEA_years$year <- years
SEA_years$region <- "SEA"
WPR_years <- transpose(as.data.frame(WPR_new_res_art3))
WPR_years$year <- years
WPR_years$region <- "WPR"

# combine dataframes
regions_years <- rbindlist(list(AFR_years, AMR_years, EMR_years, EUR_years, SEA_years, WPR_years))

# rename columns
colnames(regions_years)[1:3] <- c("inc_mean", "inc_lower", "inc_upper")

# reorder columns
regions_years <- regions_years[, c(5, 4, 1:3)]

# insert WHO notifications
regions_years <- merge(regions_years, region_notif, by.x=c("region", "year"), by.y=c("g_whoregion", "year"))

# declare vector of region names
region_names <- c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", "Western Pacific")

# create empty list of plots for storage
plot_list = list()

# initialize storage index
index = 1

# produce regional incidence estimates for parameter sets produced by Stan
for (curr_region in c("AFR", "AMR", "EMR", "EUR", "SEA", "WPR")){
  
  print(curr_region)
  
  print(index)
  
  plot_list[[index]] <- local({
    
    index <- index
    
    p <- ggplot(data=subset(regions_years, region==curr_region), aes(x=year, y=inc_mean, linetype="solid")) +
      geom_line() + 
      geom_ribbon(aes(ymin=inc_lower, ymax=inc_upper), alpha=0.1) + 
      ggtitle(region_names[[index]]) + 
      xlab("Year") + ylab("Number of TB cases") + scale_y_continuous(expand=c(0, 0)) +
      expand_limits(y=0) +
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.direction = "horizontal",
            legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.border = element_blank()) +
      scale_linetype_discrete(labels="Model incidence estimate") 
    
    p1 <- p + geom_line(aes(year, region_notif, linetype="WHO case notifications"), 
                        data=subset(regions_years, region==curr_region)) + 
      scale_linetype_discrete(labels=c("Model incidence estimate", "WHO case notifications"))
    
  })
  
  index = index + 1
  
}

library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(plot_list[[1]])

blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()

plot_list[[1]] <- plot_list[[1]] + theme(legend.position="none")
plot_list[[2]] <- plot_list[[2]] + theme(legend.position="none")
plot_list[[3]] <- plot_list[[3]] + theme(legend.position="none")
plot_list[[4]] <- plot_list[[4]] + theme(legend.position="none")
plot_list[[5]] <- plot_list[[5]] + theme(legend.position="none")
plot_list[[6]] <- plot_list[[6]] + theme(legend.position="none")

g <- grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]],
                  plot_list[[6]],
                  ncol=2, nrow = 3, 
                  widths = c(2.5, 2.5), heights = c(1.2, 1.2, 1.2))

# insert WHO incidence estimates
regions_2019_est_revision <- merge(regions_2019_est_revision, who_ped_est_2019[, c(1,6)], by="country", all.x=TRUE)

# plot country-specific incidence estimates versus WHO incidence estimates by WHO region - FIGURE 4a
foura <- ggplot(subset(regions_2019_est_revision, ped_total > 0), aes(x=ped_total, y=inc_mean, shape=region, color=region)) +
  geom_point() + ggtitle('(A) Comparison with WHO estimates') +
  geom_abline(intercept=0, slope=1) +
  scale_x_continuous(limits=c(0.8, 350000), trans='log10') + 
  scale_y_continuous(limits=c(0.8, 350000), trans='log10') + 
  xlab("WHO estimate for number of TB cases, log-scale") + ylab("Modeled number of TB cases, log-scale") + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank()) +
  scale_colour_discrete(labels=c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", 
                                 "Western Pacific")) +
  scale_shape_discrete(labels=c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", 
                                "Western Pacific"), ) 

# load IHME incidence estimates
ihme_inc <- read.csv("ihme_inc.csv")

# extract IHME incidence estimates for 2019
ihme_inc_2019 <- subset(ihme_inc, year==2019)

# sum all cases by country
ihme_inc_2019_allcases <- ihme_inc_2019 %>% 
  group_by(country) %>%
  summarise(ihme_val_total = sum(ihme_val))

# insert IHME incidence estimates
regions_2019_est_revision <- merge(regions_2019_est_revision, ihme_inc_2019_allcases, by="country", all.x=TRUE)

pdfnam <- "Figure_4_main_text.pdf"
pdf(file=pdfnam,width=7, height=9.5) 
options("scipen"=100, "digits"=4)
# plot country-specific incidence estimates versus IHME incidence estimates by WHO region - FIGURE 4b
fourb <- ggplot(subset(regions_2019_est_revision, inc_mean > 1), aes(x=ihme_val_total, y=inc_mean, shape=region, color=region)) +
  geom_point() + ggtitle('(B) Comparison with IHME estimates') +
  geom_abline(intercept=0, slope=1) +
  scale_x_continuous(limits=c(0.8, 350000), trans='log10', breaks=c(10, 1000, 100000)) + 
  scale_y_continuous(limits=c(0.8, 350000), trans='log10', breaks=c(10, 1000, 100000)) + 
  xlab("IHME estimate for number of TB cases, log-scale") + ylab("Modeled number of TB cases, log-scale") + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank()) +
  scale_colour_discrete(labels=c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", 
                                 "Western Pacific")) +
  scale_shape_discrete(labels=c("Africa", "Americas", "Eastern Mediterranean", "Europe", "South-East Asia", 
                                "Western Pacific"), ) 

# combine plots
grid.arrange(foura, fourb, ncol=1, nrow=2, heights=c(2, 2))

dev.off();system(paste("open", pdfnam))


# declare vector of high-burden countries
high_burden_countries <- c("Angola", "Bangladesh", "Brazil", "Cambodia", "Central African Republic", "China", 
                           "Congo", "Democratic People's Republic of Korea", "Democratic Republic of the Congo", 
                           "Ethiopia", "India", "Indonesia", "Kenya", "Lesotho", "Liberia", "Mozambique", "Myanmar", 
                           "Namibia", "Nigeria", "Pakistan", "Papua New Guinea","Philippines", "Russian Federation",
                           "Sierra Leone", "South Africa", "Thailand", "United Republic of Tanzania", "Viet Nam",
                           "Zambia", "Zimbabwe")

# extract estimates for high-burden countries
hb_est_2019_art <-  subset(regions_2019_est_revision, country %in% high_burden_countries)

# reshape high-incidence country estimate data for plotting
hb_est_2019_art_melt <- x[, c(1,2,5,6)] %>% gather(inc_est, inc_mean, -country)

# edit country names for plotting
hb_est_2019_art_melt[which(hb_est_2019_art_melt$country=="Viet Nam"), "country"] <- "Vietnam"
hb_est_2019_art_melt[which(hb_est_2019_art_melt$country=="United Republic of Tanzania"), "country"] <- "Tanzania"
hb_est_2019_art_melt[which(hb_est_2019_art_melt$country=="Democratic Republic of the Congo"), "country"] <- "DR Congo"
hb_est_2019_art_melt[which(hb_est_2019_art_melt$country=="Democratic People's Republic of Korea"), "country"] <- "DPR Korea"
hb_est_2019_art_melt[which(hb_est_2019_art_melt$country=="Central African Republic"), "country"] <- "CAR"

# extract high-burden country estimates and intervals - TABLE 3
hb_est_2019_art_interval <- subset(new_res_art2_t, country %in% high_burden_countries)

# reorder rows 
hb_est_2019_art_melt$country <- factor(hb_est_2019_art_melt$country, levels=c("India", "Nigeria", "Indonesia", "DR Congo",
                                                                              "Pakistan", "Philippines", "South Africa",
                                                                              "Ethiopia", "Angola", "China", "Tanzania",
                                                                              "Bangladesh", "Myanmar", "Mozambique",
                                                                              "Kenya", "Zambia", "Vietnam", "Papua New Guinea",
                                                                              "DPR Korea", "Brazil", "CAR", "Zimbabwe",
                                                                              "Cambodia", "Sierra Leone", "Thailand", "Congo",
                                                                              "Liberia", "Russian Federation", "Namibia",
                                                                              "Lesotho"))
hb_est_2019_art_melt$inc_est <- factor(hb_est_2019_art_melt$inc_est , levels=c("inc_mean", "ped_total", "ihme_val_total"))

# plot country-specific incidence estimates and WHO incidence estimate for high-burden countries - FIGURE S2
ggplot(subset(hb_est_2019_art_melt, inc_est != "total_notif"), aes(x=country, y=inc_mean, col=inc_est, shape=inc_est)) +
  scale_y_continuous(trans='log10') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Country") + ylab("Estimated number of TB cases, log-scale") + 
  geom_vline(xintercept=unique(hb_est_2019_art_melt$country), 
             color="#DCDCDC", linetype="solid") + geom_point() +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank()) +
  scale_colour_discrete(labels=c("Model incidence estimate", "WHO incidence estimate", "IHME incidence estimate")) +
  scale_shape_discrete(labels=c("Model incidence estimate", "WHO incidence estimate", "IHME incidence estimate"))

# produce alternate version of FIGURE S2 using jitter
ggplot(subset(hb_est_2019_art_melt, inc_est != "total_notif"), aes(x=country, y=inc_mean, col=inc_est)) +
  scale_y_continuous(trans='log10') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Country") + ylab("Estimated number of TB cases, log-scale") + 
  geom_vline(xintercept=unique(hb_est_2019_art_melt$country), 
             color="#DCDCDC", linetype="solid") + geom_point(position=position_jitter(w=0.15)) +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(), legend.position="top") +
  scale_colour_discrete(labels=c("Model incidence estimate", "WHO incidence estimate", "IHME incidence estimate"))

?position_jitter
# extract estimates for training countries
train_est_2019_revision <- subset(regions_2019_est_revision, country %in% train_data_04_revision$country)

# extract 2019 data
sub2019_revision <- subset(map_data_revision, year==2019)

# insert CDR
train_est_2019_revision <- unique(merge(train_est_2019_revision, sub2019_revision[, c(1, 6)], by="country", all.x=TRUE))

# calculate WHO notification estimates
train_est_2019_revision$notif_est <- train_est_2019_revision$inc_mean * train_est_2019_revision$p_tx_it * 
  mean(samps_2021_art[["rr_ped_detect"]])

# plot country-specific incidence estimates versus WHO case notifications for training countries - FIGURE S1
ggplot(train_est_2019_revision, aes(x=total_notif, y=notif_est)) +
  geom_point() + 
  geom_abline(intercept=0, slope=1) + scale_x_continuous(limits=c(1, 10000), trans='log10') + 
  scale_y_continuous(limits=c(1, 10000), trans='log10') + 
  xlab("WHO case notifications, log-scale") + ylab("Modeled number of notifications, log-scale") + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank()) 
