library(ggplot2)
library(readr)
library(rstan)
mTrsp <- function(cl,a)  { apply(col2rgb(cl), 2, function(x){ rgb(x[1],x[2],x[3],a,maxColorValue=255)}) }

# set wd
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

### Using RStan to run optimization

# load files
train_data_04_revision <- readRDS("train_data_04_revision.rds")
train_data_514_revision <- readRDS("train_data_514_revision.rds")
map_data_revision <- readRDS("map_data_revision.rds")

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

# define countries and years of interest
map_countries_revision <- unique(map_data_revision$country)
years <- 2013:2019

# define array dimensions
n_country <- length(unique(map_countries_revision))  
n_year    <- length(years)    
n_age     <- 2    
n_sim     <- 4000 

# extract posterior means of parameters from base model 
mean(samps_2021_art[["infect"]])
mean(samps_2021_art[["active_04"]])
mean(samps_2021_art[["active_514"]])
mean(samps_2021_art[["dur_untx"]])
mean(samps_2021_art[["fract_early"]])
mean(samps_2021_art[["sat"]])
mean(samps_2021_art[["z"]])
mean(samps_2021_art[["hiv_rr"]])
mean(samps_2021_art[["pem_rr"]])
mean(samps_2021_art[["art_rr"]])
mean(samps_2021_art[["rr_ped_detect"]])




# create empty list of plots for storage
plot_list = list()

## SENSITIVITY ANALYSIS WITH RESPECT TO INFECTION PROBABILITY PARAMETER

# set priors for other parameters
active_priors_04 <- betapar(c(0.19, 0.084, 0.374)) # this is updated
active_priors_514 <- betapar(c(0.089, 0.049, 0.161)) # this is updated
dur_priors <- gammapar(c(3, 2.5, 3.5))
fract_priors <- betapar(c(0.25, 0.17, 0.33))
hiv_rr_priors <- gammapar(c(7.9, 4.5, 13.7))
pem_rr_priors <- gammapar(c(4, 2, 6))

# declare vector of values to fix for infection probability parameter
infect_vals <- seq(from = infect_lowers_2021, to = infect_uppers_2021, length.out = 10)

# initialize vector for storing results of sensitivity analysis
infect_sens <- rep(0, length(infect_vals))

# initialize storage index
index <- 1

# run optimization and produce incidence estimates for parameter sets produced by Stan for each value of parameter
for (val in infect_vals){
  
  print(val)
  
  # declare inputs
  datList_sens <- list(
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
    infect = val,
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
  source("stan_infect_sens_revision.stan")
  fit_stan_infect_sens <- rstan::stan(
    model_code = stan_code, 
    data    = datList_sens,
    seed    = 123,
    chains  = 4,
    iter    = 11000,
    warmup  = 10000)
  
  # extract samples from fitted model 
  samps_sens <- extract(fit_stan_infect_sens)
  
  # declare array
  all_res_sens <- array(NA,dim=c(n_country, n_year, n_age, n_sim), 
                   dimnames = list(map_countries_revision, years, c("0-4", "5-14"), 1:4000))
  all_res_sens_adj <- all_res_sens
  
  #### 2 Fill in array
  # iterate through countries
  for(i in 1:n_country){
    
    # iterate through years
    for(j in 1:n_year){
      
      # select data for given country and year  
      map_data_sub_04 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="0-4")
      map_data_sub_514 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="5-14")
      map_data_sub_04$p_tx_it <- min(c(0.99,map_data_sub_04$p_tx_it))  #~~
      map_data_sub_514$p_tx_it <- min(c(0.99,map_data_sub_514$p_tx_it))  #~~
      
      # choose appropriate rate ratio
      if (map_data_sub_04$lat_cat[1]=="above40"){
        
        mn = 0.31
        
      } else if (map_data_sub_04$lat_cat[1]=="20to40"){
        
        mn = 0.68
        
      } else {
        
        mn = 0.77
        
      }
      
      # iterate through samples
      for (k in 1:n_sim){
        
        duration = samps_sens[["dur_untx"]][k] * (1 - map_data_sub_04$p_tx_it * (1 - samps_sens[["fract_early"]][k])); #~~
        
        # calculate 0-4 cases
        all_res_sens[i,j,1,k] <- (map_data_sub_04$pop * samps_sens[["active_04"]][k] * 
                                   # (1 - map_data_sub_04$hiv + samps_sens[[8]][k] * map_data_sub_04$hiv) *   #~~
                                   (1 - map_data_sub_04$hiv + map_data_sub_04$hiv*(1-map_data_sub_04$art) * samps_sens[["hiv_rr"]][k] + #~~
                                      map_data_sub_04$hiv*map_data_sub_04$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *     #~~
                                   (1 - map_data_sub_04$pem + samps_sens[["pem_rr"]][k] * map_data_sub_04$pem) * #~~
                                   (1 - map_data_sub_04$bcg + samps_sens[["z"]][k] * mn * map_data_sub_04$bcg) *  #~~
                                   (map_data_sub_04$contacts_inc/map_data_sub_04$contact_sum) *
                                   ((1 - exp(-val * duration * map_data_sub_04$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))   #~~
        
        # calculate 5-14 cases
        all_res_sens[i,j,2,k] <- (map_data_sub_514$pop * samps_sens[["active_514"]][k] * #~~
                                   #     (1 - map_data_sub_514$hiv + samps_sens[[8]][k] * map_data_sub_514$hiv) *  #~~
                                   (1 - map_data_sub_514$hiv + map_data_sub_514$hiv*(1-map_data_sub_514$art) * samps_sens[["hiv_rr"]][k] +  #~~
                                      map_data_sub_514$hiv*map_data_sub_514$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *   #~~
                                   (1 - map_data_sub_514$pem + samps_sens[["pem_rr"]][k] * map_data_sub_514$pem) *  #~~
                                   (1 - map_data_sub_514$bcg + samps_sens[["z"]][k] * mn * map_data_sub_514$bcg) *  #~~
                                   (map_data_sub_514$contacts_inc/map_data_sub_514$contact_sum) *
                                   ((1 - exp(-val * duration * map_data_sub_514$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))  #~~
        
      }
      
      # adjust estimates
      all_res_sens_adj[i,j,1, ] <- all_res_sens[i,j,1, ] * max(1, map_data_sub_04$newrel_04_total   / mean(all_res_sens[i,j,1, ]) )   #~~
      all_res_sens_adj[i,j,2, ] <- all_res_sens[i,j,2, ] * max(1, map_data_sub_514$newrel_514_total / mean(all_res_sens[i,j,2, ]) )
      
    }
    
  }
  
  # replace NA values with 0 (Grenada 2014)
  all_res_sens_adj[which(is.na(all_res_sens_adj))] <- 0
  
  # calculate global estimates by year
  temp_2_sens <- apply(all_res_sens_adj,c(2,4),sum)
  new_res3_sens <- apply(temp_2_sens,1,function(x) c(mean(x),quantile(x,c(1,39)/40)))

  infect_sens[index] <- new_res3_sens[1, 7]
  
  print(infect_sens[index])
  
  index <- index + 1
  
}

# merge results with values to create dataframe to facilitate plotting
infect_sens_plot_revision <- data.frame(val=infect_vals, est=infect_sens)

# plot results of sensitivity analysis 
plot_list[[1]] <- ggplot(infect_sens_plot_revision, aes(x=val, y=est)) + 
  geom_line(size=1, show.legend=FALSE) + ylim(500000, 1500000) + ggtitle("(A)") +
  geom_vline(aes(xintercept=0.005001018, linetype="Fitted value from main analysis", 
                 colour="Fitted value from main analysis"), size=1.5) + 
  xlab(expression(paste("Value of ", italic(b)))) + ylab("Modeled number of TB cases") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank()) +
  scale_colour_manual(values="blue") + scale_linetype_manual(values="dotted") +
  labs(colour  = "", linetype = "") 

## SENSITIVITY ANALYSIS WITH RESPECT TO 0-4 ACTIVATION PROBABILITY PARAMETER

# extract sums of contacts for each country and age group
contact_means <- unique(map_data_revision[, c("country", "age_group", "contact_sum")])

# calculate means for infection probability parameter in both age groups
infect_means_revision <- 12/mean(aggregate(contact_means$contact_sum, by=list(contact_means$age_group), mean)[, 2])

# calculate lower bounds for infection probability parameter in both age groups
infect_lowers_revision <- 6/mean(aggregate(contact_means$contact_sum, by=list(contact_means$age_group), mean)[, 2])

# calculate upper bounds for infection probability parameter in both age groups
infect_uppers_revision <- 15/mean(aggregate(contact_means$contact_sum, by=list(contact_means$age_group), mean)[, 2])

# set prior for infection probability parameter
infect_priors <- betapar(c(infect_means_revision[1], infect_lowers_revision[1], infect_uppers_revision[1]))

# declare vector of values to fix for 0-4 activation probability parameter
active_04_vals <- seq(from = 0.0845, to = 0.374, length.out = 10)

# initialize vector for storing results of sensitivity analysis
active_04_sens <- rep(0, length(active_04_vals))

# initialize storage index
index <- 1

# run optimization and produce incidence estimates for parameter sets produced by Stan for each value of parameter
for (val in active_04_vals){
  
  print(val)
  
  # declare inputs
  datList_sens <- list(
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
    active_04 = val,
    infect_a = infect_priors[[1]],
    infect_b = infect_priors[[2]],
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
  source("stan_active04_sens_revision.stan")
  fit_stan_active_04_sens <- rstan::stan(
    model_code = stan_code, 
    data    = datList_sens,
    seed    = 123,
    chains  = 4,
    iter    = 11000,
    warmup  = 10000)
  
  # extract samples from fitted model 
  samps_sens <- rstan::extract(fit_stan_active_04_sens)
  
  # declare array
  all_res_sens <- array(NA,dim=c(n_country, n_year, n_age, n_sim), 
                        dimnames = list(map_countries_revision, years, c("0-4", "5-14"), 1:4000))
  all_res_sens_adj <- all_res_sens
  
  #### 2 Fill in array
  # iterate through countries
  for(i in 1:n_country){
    
    # iterate through years
    for(j in 1:n_year){
      
      # select data for given country and year  
      map_data_sub_04 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="0-4")
      map_data_sub_514 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="5-14")
      map_data_sub_04$p_tx_it <- min(c(0.99,map_data_sub_04$p_tx_it))  #~~
      map_data_sub_514$p_tx_it <- min(c(0.99,map_data_sub_514$p_tx_it))  #~~
      
      # choose appropriate rate ratio
      if (map_data_sub_04$lat_cat[1]=="above40"){
        
        mn = 0.31
        
      } else if (map_data_sub_04$lat_cat[1]=="20to40"){
        
        mn = 0.68
        
      } else {
        
        mn = 0.77
        
      }
      
      # iterate through samples
      for (k in 1:n_sim){
        
        duration = samps_sens[["dur_untx"]][k] * (1 - map_data_sub_04$p_tx_it * (1 - samps_sens[["fract_early"]][k])); #~~
        
        # calculate 0-4 cases
        all_res_sens[i,j,1,k] <- (map_data_sub_04$pop * val * 
                                    # (1 - map_data_sub_04$hiv + samps_sens[[8]][k] * map_data_sub_04$hiv) *   #~~
                                    (1 - map_data_sub_04$hiv + map_data_sub_04$hiv*(1-map_data_sub_04$art) * samps_sens[["hiv_rr"]][k] + #~~
                                       map_data_sub_04$hiv*map_data_sub_04$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *     #~~
                                    (1 - map_data_sub_04$pem + samps_sens[["pem_rr"]][k] * map_data_sub_04$pem) * #~~
                                    (1 - map_data_sub_04$bcg + samps_sens[["z"]][k] * mn * map_data_sub_04$bcg) *  #~~
                                    (map_data_sub_04$contacts_inc/map_data_sub_04$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_04$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))   #~~
        
        # calculate 5-14 cases
        all_res_sens[i,j,2,k] <- (map_data_sub_514$pop * samps_sens[["active_514"]][k] * #~~
                                    #     (1 - map_data_sub_514$hiv + samps_sens[[8]][k] * map_data_sub_514$hiv) *  #~~
                                    (1 - map_data_sub_514$hiv + map_data_sub_514$hiv*(1-map_data_sub_514$art) * samps_sens[["hiv_rr"]][k] +  #~~
                                       map_data_sub_514$hiv*map_data_sub_514$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *   #~~
                                    (1 - map_data_sub_514$pem + samps_sens[["pem_rr"]][k] * map_data_sub_514$pem) *  #~~
                                    (1 - map_data_sub_514$bcg + samps_sens[["z"]][k] * mn * map_data_sub_514$bcg) *  #~~
                                    (map_data_sub_514$contacts_inc/map_data_sub_514$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_514$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))  #~~
        
      }
      
      # adjust estimates
      all_res_sens_adj[i,j,1, ] <- all_res_sens[i,j,1, ] * max(1, map_data_sub_04$newrel_04_total   / mean(all_res_sens[i,j,1, ]) )   #~~
      all_res_sens_adj[i,j,2, ] <- all_res_sens[i,j,2, ] * max(1, map_data_sub_514$newrel_514_total / mean(all_res_sens[i,j,2, ]) )
      
    }
    
  }
  
  # replace NA values with 0 (Grenada 2014)
  all_res_sens_adj[which(is.na(all_res_sens_adj))] <- 0
  
  # calculate global estimates by year
  temp_2_sens <- apply(all_res_sens_adj,c(2,4),sum)
  new_res3_sens <- apply(temp_2_sens,1,function(x) c(mean(x),quantile(x,c(1,39)/40)))
  
  active_04_sens[index] <- new_res3_sens[1, 7]
  
  print(active_04_sens[index])
  
  index <- index + 1
  
}  

# merge results with values to create dataframe to facilitate plotting
active_04_sens_plot_revision <- data.frame(val=active_04_vals, est=active_04_sens)

# plot results of sensitivity analysis 
plot_list[[2]] <- ggplot(active_04_sens_plot_revision, aes(x=val, y=est)) + 
  geom_line(size=1) + ylim(500000, 1500000) + ggtitle("(B)") + 
  geom_vline(xintercept=0.171648, linetype="dotted", color="blue", size=1.5) + 
  xlab(expression(paste("Value of ", italic(a[0-4])))) + ylab("Modeled number of TB cases") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank()) 

## SENSITIVITY ANALYSIS WITH RESPECT TO 5-14 ACTIVATION PROBABILITY PARAMETER

# declare vector of values to fix for 5-14 activation probability parameter
active_514_vals <- seq(from = 0.049, to = 0.161, length.out = 10)

# initialize vector for storing results of sensitivity analysis
active_514_sens <- rep(0, length(active_514_vals))

# initialize storage index
index <- 1

# run optimization and produce incidence estimates for parameter sets produced by Stan for each value of parameter
for (val in active_514_vals){
  
  print(val)
  
  # declare inputs
  datList_sens <- list(
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
    active_514 = val,
    infect_a = infect_priors[[1]],
    infect_b = infect_priors[[2]],
    active_04_a = active_priors_04[[1]],
    active_04_b = active_priors_04[[2]],
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
  source("stan_active514_sens_revision.stan")
  fit_stan_active_514_sens <- rstan::stan(
    model_code = stan_code, 
    data    = datList_sens,
    seed    = 123,
    chains  = 4,
    iter    = 11000,
    warmup  = 10000)
  
  # extract samples from fitted model 
  samps_sens <- extract(fit_stan_active_514_sens)
  
  # declare array
  all_res_sens <- array(NA,dim=c(n_country, n_year, n_age, n_sim), 
                        dimnames = list(map_countries_revision, years, c("0-4", "5-14"), 1:4000))
  all_res_sens_adj <- all_res_sens
  
  #### 2 Fill in array
  # iterate through countries
  for(i in 1:n_country){
    
    # iterate through years
    for(j in 1:n_year){
      
      # select data for given country and year  
      map_data_sub_04 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="0-4")
      map_data_sub_514 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="5-14")
      map_data_sub_04$p_tx_it <- min(c(0.99,map_data_sub_04$p_tx_it))  #~~
      map_data_sub_514$p_tx_it <- min(c(0.99,map_data_sub_514$p_tx_it))  #~~
      
      # choose appropriate rate ratio
      if (map_data_sub_04$lat_cat[1]=="above40"){
        
        mn = 0.31
        
      } else if (map_data_sub_04$lat_cat[1]=="20to40"){
        
        mn = 0.68
        
      } else {
        
        mn = 0.77
        
      }
      
      # iterate through samples
      for (k in 1:n_sim){
        
        duration = samps_sens[["dur_untx"]][k] * (1 - map_data_sub_04$p_tx_it * (1 - samps_sens[["fract_early"]][k])); #~~
        
        # calculate 0-4 cases
        all_res_sens[i,j,1,k] <- (map_data_sub_04$pop * samps_sens[["active_04"]][k] * 
                                    # (1 - map_data_sub_04$hiv + samps_sens[[8]][k] * map_data_sub_04$hiv) *   #~~
                                    (1 - map_data_sub_04$hiv + map_data_sub_04$hiv*(1-map_data_sub_04$art) * samps_sens[["hiv_rr"]][k] + #~~
                                       map_data_sub_04$hiv*map_data_sub_04$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *     #~~
                                    (1 - map_data_sub_04$pem + samps_sens[["pem_rr"]][k] * map_data_sub_04$pem) * #~~
                                    (1 - map_data_sub_04$bcg + samps_sens[["z"]][k] * mn * map_data_sub_04$bcg) *  #~~
                                    (map_data_sub_04$contacts_inc/map_data_sub_04$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_04$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))   #~~
        
        # calculate 5-14 cases
        all_res_sens[i,j,2,k] <- (map_data_sub_514$pop * val * #~~
                                    #     (1 - map_data_sub_514$hiv + samps_sens[[8]][k] * map_data_sub_514$hiv) *  #~~
                                    (1 - map_data_sub_514$hiv + map_data_sub_514$hiv*(1-map_data_sub_514$art) * samps_sens[["hiv_rr"]][k] +  #~~
                                       map_data_sub_514$hiv*map_data_sub_514$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *   #~~
                                    (1 - map_data_sub_514$pem + samps_sens[["pem_rr"]][k] * map_data_sub_514$pem) *  #~~
                                    (1 - map_data_sub_514$bcg + samps_sens[["z"]][k] * mn * map_data_sub_514$bcg) *  #~~
                                    (map_data_sub_514$contacts_inc/map_data_sub_514$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_514$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))  #~~
        
      }
      
      # adjust estimates
      all_res_sens_adj[i,j,1, ] <- all_res_sens[i,j,1, ] * max(1, map_data_sub_04$newrel_04_total   / mean(all_res_sens[i,j,1, ]) )   #~~
      all_res_sens_adj[i,j,2, ] <- all_res_sens[i,j,2, ] * max(1, map_data_sub_514$newrel_514_total / mean(all_res_sens[i,j,2, ]) )
      
    }
    
  }
  
  # replace NA values with 0 (Grenada 2014)
  all_res_sens_adj[which(is.na(all_res_sens_adj))] <- 0
  
  # calculate global estimates by year
  temp_2_sens <- apply(all_res_sens_adj,c(2,4),sum)
  new_res3_sens <- apply(temp_2_sens,1,function(x) c(mean(x),quantile(x,c(1,39)/40)))
  
  active_514_sens[index] <- new_res3_sens[1, 7]
  
  index = index + 1
  
}

 # merge results with values to create dataframe to facilitate plotting
active_514_sens_plot <- data.frame(val=active_514_vals, est=active_514_sens)

# plot results of sensitivity analysis 
plot_list[[3]] <- ggplot(active_514_sens_plot, aes(x=val, y=est)) + 
  geom_line(size=1) + ylim(500000, 1500000) + ggtitle("(C)") + 
  geom_vline(xintercept=0.1256858, linetype="dotted", color="blue", size=1.5) + 
  xlab(expression(paste("Value of ", italic(a[5-14])))) + ylab("Modeled number of TB cases") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank())

## SENSITIVITY ANALYSIS WITH RESPECT TO DURATION PARAMETER

# declare vector of values to fix for 5-14 activation probability parameter
dur_vals <- seq(from = 2.5, to = 3.5, length.out = 10)

# initialize vector for storing results of sensitivity analysis
dur_sens <- rep(0, length(dur_vals))

# initialize storage index
index <- 1

# run optimization and produce incidence estimates for parameter sets produced by Stan for each value of parameter
for (val in dur_vals){
  
  print(val)
  
  # declare inputs
  datList_sens <- list(
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
    dur_untx = val,
    infect_a = infect_priors[[1]],
    infect_b = infect_priors[[2]],
    active_04_a = active_priors_04[[1]],
    active_04_b = active_priors_04[[2]],
    active_514_a = active_priors_514[[1]],
    active_514_b = active_priors_514[[2]],
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
  source("stan_dur_sens_revision.stan")
  fit_stan_dur_sens <- rstan::stan(
    model_code = stan_code, 
    data    = datList_sens,
    seed    = 123,
    chains  = 4,
    iter    = 11000,
    warmup  = 10000)
  
  # extract samples from fitted model 
  samps_sens <- extract(fit_stan_dur_sens)
  
  # declare array
  all_res_sens <- array(NA,dim=c(n_country, n_year, n_age, n_sim), 
                        dimnames = list(map_countries_revision, years, c("0-4", "5-14"), 1:4000))
  all_res_sens_adj <- all_res_sens
  
  #### 2 Fill in array
  # iterate through countries
  for(i in 1:n_country){
    
    # iterate through years
    for(j in 1:n_year){
      
      # select data for given country and year  
      map_data_sub_04 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="0-4")
      map_data_sub_514 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="5-14")
      map_data_sub_04$p_tx_it <- min(c(0.99,map_data_sub_04$p_tx_it))  #~~
      map_data_sub_514$p_tx_it <- min(c(0.99,map_data_sub_514$p_tx_it))  #~~
      
      # choose appropriate rate ratio
      if (map_data_sub_04$lat_cat[1]=="above40"){
        
        mn = 0.31
        
      } else if (map_data_sub_04$lat_cat[1]=="20to40"){
        
        mn = 0.68
        
      } else {
        
        mn = 0.77
        
      }
      
      # iterate through samples
      for (k in 1:n_sim){
        
        duration = val * (1 - map_data_sub_04$p_tx_it * (1 - samps_sens[["fract_early"]][k])); #~~
        
        # calculate 0-4 cases
        all_res_sens[i,j,1,k] <- (map_data_sub_04$pop * samps_sens[["active_04"]][k] * 
                                    # (1 - map_data_sub_04$hiv + samps_sens[[8]][k] * map_data_sub_04$hiv) *   #~~
                                    (1 - map_data_sub_04$hiv + map_data_sub_04$hiv*(1-map_data_sub_04$art) * samps_sens[["hiv_rr"]][k] + #~~
                                       map_data_sub_04$hiv*map_data_sub_04$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *     #~~
                                    (1 - map_data_sub_04$pem + samps_sens[["pem_rr"]][k] * map_data_sub_04$pem) * #~~
                                    (1 - map_data_sub_04$bcg + samps_sens[["z"]][k] * mn * map_data_sub_04$bcg) *  #~~
                                    (map_data_sub_04$contacts_inc/map_data_sub_04$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_04$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))   #~~
        
        # calculate 5-14 cases
        all_res_sens[i,j,2,k] <- (map_data_sub_514$pop * samps_sens[["active_514"]][k] * #~~
                                    #     (1 - map_data_sub_514$hiv + samps_sens[[8]][k] * map_data_sub_514$hiv) *  #~~
                                    (1 - map_data_sub_514$hiv + map_data_sub_514$hiv*(1-map_data_sub_514$art) * samps_sens[["hiv_rr"]][k] +  #~~
                                       map_data_sub_514$hiv*map_data_sub_514$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *   #~~
                                    (1 - map_data_sub_514$pem + samps_sens[["pem_rr"]][k] * map_data_sub_514$pem) *  #~~
                                    (1 - map_data_sub_514$bcg + samps_sens[["z"]][k] * mn * map_data_sub_514$bcg) *  #~~
                                    (map_data_sub_514$contacts_inc/map_data_sub_514$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_514$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))  #~~
        
      }
      
      # adjust estimates
      all_res_sens_adj[i,j,1, ] <- all_res_sens[i,j,1, ] * max(1, map_data_sub_04$newrel_04_total   / mean(all_res_sens[i,j,1, ]) )   #~~
      all_res_sens_adj[i,j,2, ] <- all_res_sens[i,j,2, ] * max(1, map_data_sub_514$newrel_514_total / mean(all_res_sens[i,j,2, ]) )
      
    }
    
  }
  
  # replace NA values with 0 (Grenada 2014)
  all_res_sens_adj[which(is.na(all_res_sens_adj))] <- 0
  
  # calculate global estimates by year
  temp_2_sens <- apply(all_res_sens_adj,c(2,4),sum)
  new_res3_sens <- apply(temp_2_sens,1,function(x) c(mean(x),quantile(x,c(1,39)/40)))
  
  dur_sens[index] <- new_res3_sens[1, 7]
  
  index = index + 1
  
}

# merge results with values to create dataframe to facilitate plotting
dur_sens_plot <- data.frame(val=dur_vals, est=dur_sens)

# plot results of sensitivity analysis 
plot_list[[4]] <- ggplot(dur_sens_plot, aes(x=val, y=est)) + 
  geom_line(size=1) + ylim(500000, 1500000) + ggtitle("(D)") + 
  geom_vline(xintercept=3.045478, linetype="dotted", color="blue", size=1.5) + 
  xlab(expression(paste("Value of ", italic(d[untx])))) + ylab("Modeled number of TB cases") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank())

## SENSITIVTY ANALYSIS WITH RESPECT TO FRACTION OF TIME IN EARLY DISEASE PARAMETER

# declare vector of values to fix for fraction of time in early disease parameter
fract_vals <- seq(from = 0.17, to = 0.33, length.out = 10)

# initialize vector for storing results of sensitivity analysis
fract_sens <- rep(0, length(fract_vals))

# initialize storage index
index <- 1

# run optimization and produce incidence estimates for parameter sets produced by Stan for each value of parameter
for (val in fract_vals){
  
  print(val)
  
  # declare inputs
  datList_sens <- list(
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
    fract_early = val,
    infect_a = infect_priors[[1]],
    infect_b = infect_priors[[2]],
    active_04_a = active_priors_04[[1]],
    active_04_b = active_priors_04[[2]],
    active_514_a = active_priors_514[[1]],
    active_514_b = active_priors_514[[2]],
    dur_untx_a = dur_priors[[1]],
    dur_untx_b = dur_priors[[2]],
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
  source("stan_fract_sens_revision.stan")
  fit_stan_fract_sens <- rstan::stan(
    model_code = stan_code, 
    data    = datList_sens,
    seed    = 123,
    chains  = 4,
    iter    = 11000,
    warmup  = 10000)
  
  # extract samples from fitted model 
  samps_sens <- extract(fit_stan_fract_sens)
  
  # declare array
  all_res_sens <- array(NA,dim=c(n_country, n_year, n_age, n_sim), 
                        dimnames = list(map_countries_revision, years, c("0-4", "5-14"), 1:4000))
  all_res_sens_adj <- all_res_sens
  
  #### 2 Fill in array
  # iterate through countries
  for(i in 1:n_country){
    
    # iterate through years
    for(j in 1:n_year){
      
      # select data for given country and year  
      map_data_sub_04 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="0-4")
      map_data_sub_514 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="5-14")
      map_data_sub_04$p_tx_it <- min(c(0.99,map_data_sub_04$p_tx_it))  #~~
      map_data_sub_514$p_tx_it <- min(c(0.99,map_data_sub_514$p_tx_it))  #~~
      
      # choose appropriate rate ratio
      if (map_data_sub_04$lat_cat[1]=="above40"){
        
        mn = 0.31
        
      } else if (map_data_sub_04$lat_cat[1]=="20to40"){
        
        mn = 0.68
        
      } else {
        
        mn = 0.77
        
      }
      
      # iterate through samples
      for (k in 1:n_sim){
        
        duration = samps_sens[["dur_untx"]][k] * (1 - map_data_sub_04$p_tx_it * (1 - val)); #~~
        
        # calculate 0-4 cases
        all_res_sens[i,j,1,k] <- (map_data_sub_04$pop * samps_sens[["active_04"]][k] * 
                                    # (1 - map_data_sub_04$hiv + samps_sens[[8]][k] * map_data_sub_04$hiv) *   #~~
                                    (1 - map_data_sub_04$hiv + map_data_sub_04$hiv*(1-map_data_sub_04$art) * samps_sens[["hiv_rr"]][k] + #~~
                                       map_data_sub_04$hiv*map_data_sub_04$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *     #~~
                                    (1 - map_data_sub_04$pem + samps_sens[["pem_rr"]][k] * map_data_sub_04$pem) * #~~
                                    (1 - map_data_sub_04$bcg + samps_sens[["z"]][k] * mn * map_data_sub_04$bcg) *  #~~
                                    (map_data_sub_04$contacts_inc/map_data_sub_04$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_04$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))   #~~
        
        # calculate 5-14 cases
        all_res_sens[i,j,2,k] <- (map_data_sub_514$pop * samps_sens[["active_514"]][k] * #~~
                                    #     (1 - map_data_sub_514$hiv + samps_sens[[8]][k] * map_data_sub_514$hiv) *  #~~
                                    (1 - map_data_sub_514$hiv + map_data_sub_514$hiv*(1-map_data_sub_514$art) * samps_sens[["hiv_rr"]][k] +  #~~
                                       map_data_sub_514$hiv*map_data_sub_514$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *   #~~
                                    (1 - map_data_sub_514$pem + samps_sens[["pem_rr"]][k] * map_data_sub_514$pem) *  #~~
                                    (1 - map_data_sub_514$bcg + samps_sens[["z"]][k] * mn * map_data_sub_514$bcg) *  #~~
                                    (map_data_sub_514$contacts_inc/map_data_sub_514$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_514$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))  #~~
        
      }
      
      # adjust estimates
      all_res_sens_adj[i,j,1, ] <- all_res_sens[i,j,1, ] * max(1, map_data_sub_04$newrel_04_total   / mean(all_res_sens[i,j,1, ]) )   #~~
      all_res_sens_adj[i,j,2, ] <- all_res_sens[i,j,2, ] * max(1, map_data_sub_514$newrel_514_total / mean(all_res_sens[i,j,2, ]) )
      
    }
    
  }
  
  # replace NA values with 0 (Grenada 2014)
  all_res_sens_adj[which(is.na(all_res_sens_adj))] <- 0
  
  # calculate global estimates by year
  temp_2_sens <- apply(all_res_sens_adj,c(2,4),sum)
  new_res3_sens <- apply(temp_2_sens,1,function(x) c(mean(x),quantile(x,c(1,39)/40)))
  
  fract_sens[index] <- new_res3_sens[1, 7]
  
  index = index + 1
  
}

# merge results with values to create dataframe to facilitate plotting
fract_sens_plot <- data.frame(val=fract_vals, est=fract_sens)

# plot results of sensitivity analysis 
plot_list[[5]] <- ggplot(fract_sens_plot, aes(x=val, y=est)) + 
  geom_line(size=1) + ylim(500000, 1500000) + ggtitle("(E)") + 
  geom_vline(xintercept=0.2494644, linetype="dotted", color="blue", size=1.5) + 
  xlab(expression(paste("Value of ", italic(f)))) + ylab("Modeled number of TB cases") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank())

## SENSITIVITY ANALYSIS WITH RESPECT TO CONTACT SATURATION PARAMETER

# declare vector of values to fix for contact saturation parameter
sat_vals <- seq(from = 0.09, to = 0.91, length.out = 10)

# initialize vector for storing results of sensitivity analysis
sat_sens <- rep(0, length(fract_vals))

# initialize storage index
index <- 1

# run optimization and produce incidence estimates for parameter sets produced by Stan for each value of parameter
for (val in sat_vals){
  
  print(val)
  
  # declare inputs
  datList_sens <- list(
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
    sat = val,
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
  source("stan_sat_sens_revision.stan")
  fit_stan_sat_sens <- rstan::stan(
    model_code = stan_code, 
    data    = datList_sens,
    seed    = 123,
    chains  = 4,
    iter    = 11000,
    warmup  = 10000)
  
  # extract samples from fitted model 
  samps_sens <- extract(fit_stan_sat_sens)
  
  # declare array
  all_res_sens <- array(NA,dim=c(n_country, n_year, n_age, n_sim), 
                        dimnames = list(map_countries_revision, years, c("0-4", "5-14"), 1:4000))
  all_res_sens_adj <- all_res_sens
  
  #### 2 Fill in array
  # iterate through countries
  for(i in 1:n_country){
    
    # iterate through years
    for(j in 1:n_year){
      
      # select data for given country and year  
      map_data_sub_04 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="0-4")
      map_data_sub_514 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="5-14")
      map_data_sub_04$p_tx_it <- min(c(0.99,map_data_sub_04$p_tx_it))  #~~
      map_data_sub_514$p_tx_it <- min(c(0.99,map_data_sub_514$p_tx_it))  #~~
      
      # choose appropriate rate ratio
      if (map_data_sub_04$lat_cat[1]=="above40"){
        
        mn = 0.31
        
      } else if (map_data_sub_04$lat_cat[1]=="20to40"){
        
        mn = 0.68
        
      } else {
        
        mn = 0.77
        
      }
      
      # iterate through samples
      for (k in 1:n_sim){
        
        duration = samps_sens[["dur_untx"]][k] * (1 - map_data_sub_04$p_tx_it * (1 - samps_sens[["fract_early"]][k])); #~~
        
        # calculate 0-4 cases
        all_res_sens[i,j,1,k] <- (map_data_sub_04$pop * samps_sens[["active_04"]][k] * 
                                    # (1 - map_data_sub_04$hiv + samps_sens[[8]][k] * map_data_sub_04$hiv) *   #~~
                                    (1 - map_data_sub_04$hiv + map_data_sub_04$hiv*(1-map_data_sub_04$art) * samps_sens[["hiv_rr"]][k] + #~~
                                       map_data_sub_04$hiv*map_data_sub_04$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *     #~~
                                    (1 - map_data_sub_04$pem + samps_sens[["pem_rr"]][k] * map_data_sub_04$pem) * #~~
                                    (1 - map_data_sub_04$bcg + samps_sens[["z"]][k] * mn * map_data_sub_04$bcg) *  #~~
                                    (map_data_sub_04$contacts_inc/map_data_sub_04$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_04$contact_sum * val)) / val))   #~~
        
        # calculate 5-14 cases
        all_res_sens[i,j,2,k] <- (map_data_sub_514$pop * samps_sens[["active_514"]][k] * #~~
                                    #     (1 - map_data_sub_514$hiv + samps_sens[[8]][k] * map_data_sub_514$hiv) *  #~~
                                    (1 - map_data_sub_514$hiv + map_data_sub_514$hiv*(1-map_data_sub_514$art) * samps_sens[["hiv_rr"]][k] +  #~~
                                       map_data_sub_514$hiv*map_data_sub_514$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *   #~~
                                    (1 - map_data_sub_514$pem + samps_sens[["pem_rr"]][k] * map_data_sub_514$pem) *  #~~
                                    (1 - map_data_sub_514$bcg + samps_sens[["z"]][k] * mn * map_data_sub_514$bcg) *  #~~
                                    (map_data_sub_514$contacts_inc/map_data_sub_514$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_514$contact_sum * val)) / val))  #~~
        
      }
      
      # adjust estimates
      all_res_sens_adj[i,j,1, ] <- all_res_sens[i,j,1, ] * max(1, map_data_sub_04$newrel_04_total   / mean(all_res_sens[i,j,1, ]) )   #~~
      all_res_sens_adj[i,j,2, ] <- all_res_sens[i,j,2, ] * max(1, map_data_sub_514$newrel_514_total / mean(all_res_sens[i,j,2, ]) )
      
    }
    
  }
  
  # replace NA values with 0 (Grenada 2014)
  all_res_sens_adj[which(is.na(all_res_sens_adj))] <- 0
  
  # calculate global estimates by year
  temp_2_sens <- apply(all_res_sens_adj,c(2,4),sum)
  new_res3_sens <- apply(temp_2_sens,1,function(x) c(mean(x),quantile(x,c(1,39)/40)))
  
  sat_sens[index] <- new_res3_sens[1, 7]
  
  index = index + 1
  
}

# merge results with values to create dataframe to facilitate plotting
sat_sens_plot <- data.frame(val=sat_vals, est=sat_sens)

# plot results of sensitivity analysis 
plot_list[[6]] <- ggplot(sat_sens_plot, aes(x=val, y=est)) + 
  geom_line(size=1) + ylim(500000, 1500000) + ggtitle("(F)") + 
  geom_vline(xintercept=0.4026461, linetype="dotted", color="blue", size=1.5) + 
  xlab(expression(paste("Value of ", italic(q)))) + ylab("Modeled number of TB cases") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank()) 

## SENSITIVITY ANALYSIS WITH RESPECT TO HIV RISK RATIO PARAMETER

# declare vector of values to fix for contact saturation parameter
hiv_rr_vals <- seq(from = 4.5, to = 13.7, length.out = 10)

# initialize vector for storing results of sensitivity analysis
hiv_rr_sens <- rep(0, length(fract_vals))

# initialize storage index
index <- 1

# run optimization and produce incidence estimates for parameter sets produced by Stan for each value of parameter
for (val in hiv_rr_vals){
  
  print(val)
  
  # declare inputs
  datList_sens <- list(
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
    hiv_rr = val,
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
    pem_rr_a = pem_rr_priors[[1]],
    pem_rr_b = pem_rr_priors[[2]],
    art_rr_a = art_rr_priors[[1]],
    art_rr_b = art_rr_priors[[2]],
    rr_ped_detect_a = rr_ped_detect_priors[[1]],  #~~ 
    rr_ped_detect_b = rr_ped_detect_priors[[2]],  #~~
    shape_rate = 23)
  
  # fit model
  source("stan_hiv_rr_sens_revision.stan")
  fit_stan_hiv_rr_sens <- rstan::stan(
    model_code = stan_code, 
    data    = datList_sens,
    seed    = 123,
    chains  = 4,
    iter    = 11000,
    warmup  = 10000)
  
  # extract samples from fitted model 
  samps_sens <- extract(fit_stan_hiv_rr_sens)
  
  # declare array
  all_res_sens <- array(NA,dim=c(n_country, n_year, n_age, n_sim), 
                        dimnames = list(map_countries_revision, years, c("0-4", "5-14"), 1:4000))
  all_res_sens_adj <- all_res_sens
  
  #### 2 Fill in array
  # iterate through countries
  for(i in 1:n_country){
    
    # iterate through years
    for(j in 1:n_year){
      
      # select data for given country and year  
      map_data_sub_04 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="0-4")
      map_data_sub_514 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="5-14")
      map_data_sub_04$p_tx_it <- min(c(0.99,map_data_sub_04$p_tx_it))  #~~
      map_data_sub_514$p_tx_it <- min(c(0.99,map_data_sub_514$p_tx_it))  #~~
      
      # choose appropriate rate ratio
      if (map_data_sub_04$lat_cat[1]=="above40"){
        
        mn = 0.31
        
      } else if (map_data_sub_04$lat_cat[1]=="20to40"){
        
        mn = 0.68
        
      } else {
        
        mn = 0.77
        
      }
      
      # iterate through samples
      for (k in 1:n_sim){
        
        duration = samps_sens[["dur_untx"]][k] * (1 - map_data_sub_04$p_tx_it * (1 - samps_sens[["fract_early"]][k])); #~~
        
        # calculate 0-4 cases
        all_res_sens[i,j,1,k] <- (map_data_sub_04$pop * samps_sens[["active_04"]][k] * 
                                    # (1 - map_data_sub_04$hiv + samps_sens[[8]][k] * map_data_sub_04$hiv) *   #~~
                                    (1 - map_data_sub_04$hiv + map_data_sub_04$hiv*(1-map_data_sub_04$art) * val + #~~
                                       map_data_sub_04$hiv*map_data_sub_04$art * val * samps_sens[["art_rr"]][k]) *     #~~
                                    (1 - map_data_sub_04$pem + samps_sens[["pem_rr"]][k] * map_data_sub_04$pem) * #~~
                                    (1 - map_data_sub_04$bcg + samps_sens[["z"]][k] * mn * map_data_sub_04$bcg) *  #~~
                                    (map_data_sub_04$contacts_inc/map_data_sub_04$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_04$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))   #~~
        
        # calculate 5-14 cases
        all_res_sens[i,j,2,k] <- (map_data_sub_514$pop * samps_sens[["active_514"]][k] * #~~
                                    #     (1 - map_data_sub_514$hiv + samps_sens[[8]][k] * map_data_sub_514$hiv) *  #~~
                                    (1 - map_data_sub_514$hiv + map_data_sub_514$hiv*(1-map_data_sub_514$art) * val +  #~~
                                       map_data_sub_514$hiv*map_data_sub_514$art * val * samps_sens[["art_rr"]][k]) *   #~~
                                    (1 - map_data_sub_514$pem + samps_sens[["pem_rr"]][k] * map_data_sub_514$pem) *  #~~
                                    (1 - map_data_sub_514$bcg + samps_sens[["z"]][k] * mn * map_data_sub_514$bcg) *  #~~
                                    (map_data_sub_514$contacts_inc/map_data_sub_514$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_514$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))  #~~
        
      }
      
      # adjust estimates
      all_res_sens_adj[i,j,1, ] <- all_res_sens[i,j,1, ] * max(1, map_data_sub_04$newrel_04_total   / mean(all_res_sens[i,j,1, ]) )   #~~
      all_res_sens_adj[i,j,2, ] <- all_res_sens[i,j,2, ] * max(1, map_data_sub_514$newrel_514_total / mean(all_res_sens[i,j,2, ]) )
      
    }
    
  }
  
  # replace NA values with 0 (Grenada 2014)
  all_res_sens_adj[which(is.na(all_res_sens_adj))] <- 0
  
  # calculate global estimates by year
  temp_2_sens <- apply(all_res_sens_adj,c(2,4),sum)
  new_res3_sens <- apply(temp_2_sens,1,function(x) c(mean(x),quantile(x,c(1,39)/40)))
  
  hiv_rr_sens[index] <- new_res3_sens[1, 7]
  
  print(hiv_rr_sens[index])
  
  index = index + 1
  
}

# merge results with values to create dataframe to facilitate plotting
hiv_rr_sens_plot <- data.frame(val=hiv_rr_vals, est=hiv_rr_sens)

# plot results of sensitivity analysis 
plot_list[[7]] <- ggplot(hiv_rr_sens_plot, aes(x=val, y=est)) + 
  geom_line(size=1) + ylim(500000, 1500000) + ggtitle("(G)") + 
  geom_vline(xintercept=7.919435, linetype="dotted", color="blue", size=1.5) + 
  xlab(expression(paste("Value of ", italic(m^h)))) + ylab("Modeled number of TB cases") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank()) 

## SENSITIVITY ANALYSIS WITH RESPECT TO PEM RISK RATIO PARAMETER

# declare vector of values to fix for contact saturation parameter
pem_rr_vals <- seq(from = 2, to = 6, length.out = 10)

# initialize vector for storing results of sensitivity analysis
pem_rr_sens <- rep(0, length(fract_vals))

# initialize storage index
index <- 1

# run optimization and produce incidence estimates for parameter sets produced by Stan for each value of parameter
for (val in pem_rr_vals){
  
  print(val)
  
  # declare inputs
  datList_sens <- list(
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
    pem_rr = val,
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
    art_rr_a = art_rr_priors[[1]],
    art_rr_b = art_rr_priors[[2]],
    rr_ped_detect_a = rr_ped_detect_priors[[1]],  #~~ 
    rr_ped_detect_b = rr_ped_detect_priors[[2]],  #~~
    shape_rate = 23)
  
  # fit model
  source("stan_pem_rr_sens_revision.stan")
  fit_stan_pem_rr_sens <- rstan::stan(
    model_code = stan_code, 
    data    = datList_sens,
    seed    = 123,
    chains  = 4,
    iter    = 11000,
    warmup  = 10000)
  
  # extract samples from fitted model 
  samps_sens <- extract(fit_stan_pem_rr_sens)
  
  # declare array
  all_res_sens <- array(NA,dim=c(n_country, n_year, n_age, n_sim), 
                        dimnames = list(map_countries_revision, years, c("0-4", "5-14"), 1:4000))
  all_res_sens_adj <- all_res_sens
  
  #### 2 Fill in array
  # iterate through countries
  for(i in 1:n_country){
    
    # iterate through years
    for(j in 1:n_year){
      
      # select data for given country and year  
      map_data_sub_04 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="0-4")
      map_data_sub_514 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="5-14")
      map_data_sub_04$p_tx_it <- min(c(0.99,map_data_sub_04$p_tx_it))  #~~
      map_data_sub_514$p_tx_it <- min(c(0.99,map_data_sub_514$p_tx_it))  #~~
      
      # choose appropriate rate ratio
      if (map_data_sub_04$lat_cat[1]=="above40"){
        
        mn = 0.31
        
      } else if (map_data_sub_04$lat_cat[1]=="20to40"){
        
        mn = 0.68
        
      } else {
        
        mn = 0.77
        
      }
      
      # iterate through samples
      for (k in 1:n_sim){
        
        duration = samps_sens[["dur_untx"]][k] * (1 - map_data_sub_04$p_tx_it * (1 - samps_sens[["fract_early"]][k])); #~~
        
        # calculate 0-4 cases
        all_res_sens[i,j,1,k] <- (map_data_sub_04$pop * samps_sens[["active_04"]][k] * 
                                    # (1 - map_data_sub_04$hiv + samps_sens[[8]][k] * map_data_sub_04$hiv) *   #~~
                                    (1 - map_data_sub_04$hiv + map_data_sub_04$hiv*(1-map_data_sub_04$art) * samps_sens[["hiv_rr"]][k] + #~~
                                       map_data_sub_04$hiv*map_data_sub_04$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *     #~~
                                    (1 - map_data_sub_04$pem + val * map_data_sub_04$pem) * #~~
                                    (1 - map_data_sub_04$bcg + samps_sens[["z"]][k] * mn * map_data_sub_04$bcg) *  #~~
                                    (map_data_sub_04$contacts_inc/map_data_sub_04$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_04$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))   #~~
        
        # calculate 5-14 cases
        all_res_sens[i,j,2,k] <- (map_data_sub_514$pop * samps_sens[["active_514"]][k] * #~~
                                    #     (1 - map_data_sub_514$hiv + samps_sens[[8]][k] * map_data_sub_514$hiv) *  #~~
                                    (1 - map_data_sub_514$hiv + map_data_sub_514$hiv*(1-map_data_sub_514$art) * samps_sens[["hiv_rr"]][k] +  #~~
                                       map_data_sub_514$hiv*map_data_sub_514$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *   #~~
                                    (1 - map_data_sub_514$pem + val * map_data_sub_514$pem) *  #~~
                                    (1 - map_data_sub_514$bcg + samps_sens[["z"]][k] * mn * map_data_sub_514$bcg) *  #~~
                                    (map_data_sub_514$contacts_inc/map_data_sub_514$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_514$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))  #~~
        
      }
      
      # adjust estimates
      all_res_sens_adj[i,j,1, ] <- all_res_sens[i,j,1, ] * max(1, map_data_sub_04$newrel_04_total   / mean(all_res_sens[i,j,1, ]) )   #~~
      all_res_sens_adj[i,j,2, ] <- all_res_sens[i,j,2, ] * max(1, map_data_sub_514$newrel_514_total / mean(all_res_sens[i,j,2, ]) )
      
    }
    
  }
  
  # replace NA values with 0 (Grenada 2014)
  all_res_sens_adj[which(is.na(all_res_sens_adj))] <- 0
  
  # calculate global estimates by year
  temp_2_sens <- apply(all_res_sens_adj,c(2,4),sum)
  new_res3_sens <- apply(temp_2_sens,1,function(x) c(mean(x),quantile(x,c(1,39)/40)))
  
  pem_rr_sens[index] <- new_res3_sens[1, 7]
  
  print(pem_rr_sens[index])
  
  index = index + 1
  
}

# merge results with values to create dataframe to facilitate plotting
pem_rr_sens_plot <- data.frame(val=pem_rr_vals, est=pem_rr_sens)

# plot results of sensitivity analysis 
plot_list[[8]] <- ggplot(pem_rr_sens_plot, aes(x=val, y=est)) + 
  geom_line(size=1) + ylim(500000, 1500000) + ggtitle("(H)") + 
  geom_vline(xintercept=3.962905, linetype="dotted", color="blue", size=1.5) + 
  xlab(expression(paste("Value of ", italic(m^u)))) + ylab("Modeled number of TB cases") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank())

## SENSITIVITY ANALYSIS WITH RESPECT TO BCG RISK RATIO PARAMETER

# declare vector of values to fix for contact saturation parameter
bcg_rr_vals <- seq(from = qgamma(0.025, 23, 23), to = qgamma(0.975, 23, 23), length.out = 10)

# initialize vector for storing results of sensitivity analysis
bcg_rr_sens <- rep(0, length(bcg_rr_vals))

# initialize storage index
index <- 1

# run optimization and produce incidence estimates for parameter sets produced by Stan for each value of parameter
for (val in bcg_rr_vals){
  
  print(val)
  
  # declare inputs
  datList_sens <- list(
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
    z = val,
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
    rr_ped_detect_b = rr_ped_detect_priors[[2]])
  
  # fit model
  source("stan_bcg_rr_sens_revision.stan")
  fit_stan_bcg_rr_sens <- rstan::stan(
    model_code = stan_code, 
    data    = datList_sens,
    seed    = 123,
    chains  = 4,
    iter    = 11000,
    warmup  = 10000)
  
  # extract samples from fitted model 
  samps_sens <- extract(fit_stan_bcg_rr_sens)
  
  # declare array
  all_res_sens <- array(NA,dim=c(n_country, n_year, n_age, n_sim), 
                        dimnames = list(map_countries_revision, years, c("0-4", "5-14"), 1:4000))
  all_res_sens_adj <- all_res_sens
  
  #### 2 Fill in array
  # iterate through countries
  for(i in 1:n_country){
    
    # iterate through years
    for(j in 1:n_year){
      
      # select data for given country and year  
      map_data_sub_04 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="0-4")
      map_data_sub_514 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="5-14")
      map_data_sub_04$p_tx_it <- min(c(0.99,map_data_sub_04$p_tx_it))  #~~
      map_data_sub_514$p_tx_it <- min(c(0.99,map_data_sub_514$p_tx_it))  #~~
      
      # choose appropriate rate ratio
      if (map_data_sub_04$lat_cat[1]=="above40"){
        
        mn = 0.31
        
      } else if (map_data_sub_04$lat_cat[1]=="20to40"){
        
        mn = 0.68
        
      } else {
        
        mn = 0.77
        
      }
      
      # iterate through samples
      for (k in 1:n_sim){
        
        duration = samps_sens[["dur_untx"]][k] * (1 - map_data_sub_04$p_tx_it * (1 - samps_sens[["fract_early"]][k])); #~~
        
        # calculate 0-4 cases
        all_res_sens[i,j,1,k] <- (map_data_sub_04$pop * samps_sens[["active_04"]][k] * 
                                    # (1 - map_data_sub_04$hiv + samps_sens[[8]][k] * map_data_sub_04$hiv) *   #~~
                                    (1 - map_data_sub_04$hiv + map_data_sub_04$hiv*(1-map_data_sub_04$art) * samps_sens[["hiv_rr"]][k] + #~~
                                       map_data_sub_04$hiv*map_data_sub_04$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *     #~~
                                    (1 - map_data_sub_04$pem + samps_sens[["pem_rr"]][k] * map_data_sub_04$pem) * #~~
                                    (1 - map_data_sub_04$bcg + val * mn * map_data_sub_04$bcg) *  #~~
                                    (map_data_sub_04$contacts_inc/map_data_sub_04$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_04$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))   #~~
        
        # calculate 5-14 cases
        all_res_sens[i,j,2,k] <- (map_data_sub_514$pop * samps_sens[["active_514"]][k] * #~~
                                    #     (1 - map_data_sub_514$hiv + samps_sens[[8]][k] * map_data_sub_514$hiv) *  #~~
                                    (1 - map_data_sub_514$hiv + map_data_sub_514$hiv*(1-map_data_sub_514$art) * samps_sens[["hiv_rr"]][k] +  #~~
                                       map_data_sub_514$hiv*map_data_sub_514$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *   #~~
                                    (1 - map_data_sub_514$pem + samps_sens[["pem_rr"]][k] * map_data_sub_514$pem) *  #~~
                                    (1 - map_data_sub_514$bcg + val * mn * map_data_sub_514$bcg) *  #~~
                                    (map_data_sub_514$contacts_inc/map_data_sub_514$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_514$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))  #~~
        
      }
      
      # adjust estimates
      all_res_sens_adj[i,j,1, ] <- all_res_sens[i,j,1, ] * max(1, map_data_sub_04$newrel_04_total   / mean(all_res_sens[i,j,1, ]) )   #~~
      all_res_sens_adj[i,j,2, ] <- all_res_sens[i,j,2, ] * max(1, map_data_sub_514$newrel_514_total / mean(all_res_sens[i,j,2, ]) )
      
    }
    
  }
  
  # replace NA values with 0 (Grenada 2014)
  all_res_sens_adj[which(is.na(all_res_sens_adj))] <- 0
  
  # calculate global estimates by year
  temp_2_sens <- apply(all_res_sens_adj,c(2,4),sum)
  new_res3_sens <- apply(temp_2_sens,1,function(x) c(mean(x),quantile(x,c(1,39)/40)))
  
  bcg_rr_sens[index] <- new_res3_sens[1, 7]
  
  index = index + 1
  
}

# merge results with values to create dataframe to facilitate plotting
bcg_rr_sens_plot <- data.frame(val=bcg_rr_vals, est=bcg_rr_sens)

# plot results of sensitivity analysis 
plot_list[[9]] <- ggplot(bcg_rr_sens_plot, aes(x=val, y=est)) + 
  geom_line(size=1) + ylim(500000, 1500000) + ggtitle("(I)") + 
  geom_vline(xintercept=0.6700508, linetype="dotted", color="blue", size=1.5) + 
  xlab(expression(paste("Value of ", italic(m^v), " relative to mean val."))) + 
  ylab("Modeled number of TB cases") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank())

## SENSITIVITY ANALYSIS WITH RESPECT TO ART RISK RATIO PARAMETER

# declare vector of values to fix for contact saturation parameter
art_rr_vals <- seq(from = 0.21, to = 0.39, length.out = 10)

# initialize vector for storing results of sensitivity analysis
art_rr_sens <- rep(0, length(art_rr_vals))

# initialize storage index
index <- 1

# run optimization and produce incidence estimates for parameter sets produced by Stan for each value of parameter
for (val in art_rr_vals){
  
  print(val)
  
  # declare inputs
  datList_sens <- list(
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
    art_rr = val,
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
    rr_ped_detect_a = rr_ped_detect_priors[[1]],  #~~ 
    rr_ped_detect_b = rr_ped_detect_priors[[2]],  #~~
    shape_rate = 23)
  
  # fit model
  source("stan_art_rr_sens_revision.stan")
  fit_stan_art_rr_sens <- rstan::stan(
    model_code = stan_code, 
    data    = datList_sens,
    seed    = 123,
    chains  = 4,
    iter    = 11000,
    warmup  = 10000)
  
  # extract samples from fitted model 
  samps_sens <- extract(fit_stan_art_rr_sens)
  
  # declare array
  all_res_sens <- array(NA,dim=c(n_country, n_year, n_age, n_sim), 
                        dimnames = list(map_countries_revision, years, c("0-4", "5-14"), 1:4000))
  all_res_sens_adj <- all_res_sens
  
  #### 2 Fill in array
  # iterate through countries
  for(i in 1:n_country){
    
    # iterate through years
    for(j in 1:n_year){
      
      # select data for given country and year  
      map_data_sub_04 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="0-4")
      map_data_sub_514 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="5-14")
      map_data_sub_04$p_tx_it <- min(c(0.99,map_data_sub_04$p_tx_it))  #~~
      map_data_sub_514$p_tx_it <- min(c(0.99,map_data_sub_514$p_tx_it))  #~~
      
      # choose appropriate rate ratio
      if (map_data_sub_04$lat_cat[1]=="above40"){
        
        mn = 0.31
        
      } else if (map_data_sub_04$lat_cat[1]=="20to40"){
        
        mn = 0.68
        
      } else {
        
        mn = 0.77
        
      }
      
      # iterate through samples
      for (k in 1:n_sim){
        
        duration = samps_sens[["dur_untx"]][k] * (1 - map_data_sub_04$p_tx_it * (1 - samps_sens[["fract_early"]][k])); #~~
        
        # calculate 0-4 cases
        all_res_sens[i,j,1,k] <- (map_data_sub_04$pop * samps_sens[["active_04"]][k] * 
                                    # (1 - map_data_sub_04$hiv + samps_sens[[8]][k] * map_data_sub_04$hiv) *   #~~
                                    (1 - map_data_sub_04$hiv + map_data_sub_04$hiv*(1-map_data_sub_04$art) * samps_sens[["hiv_rr"]][k] + #~~
                                       map_data_sub_04$hiv*map_data_sub_04$art * samps_sens[["hiv_rr"]][k] * val) *     #~~
                                    (1 - map_data_sub_04$pem + samps_sens[["pem_rr"]][k] * map_data_sub_04$pem) * #~~
                                    (1 - map_data_sub_04$bcg + samps_sens[["z"]][k] * mn * map_data_sub_04$bcg) *  #~~
                                    (map_data_sub_04$contacts_inc/map_data_sub_04$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_04$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))   #~~
        
        # calculate 5-14 cases
        all_res_sens[i,j,2,k] <- (map_data_sub_514$pop * samps_sens[["active_514"]][k] * #~~
                                    #     (1 - map_data_sub_514$hiv + samps_sens[[8]][k] * map_data_sub_514$hiv) *  #~~
                                    (1 - map_data_sub_514$hiv + map_data_sub_514$hiv*(1-map_data_sub_514$art) * samps_sens[["hiv_rr"]][k] +  #~~
                                       map_data_sub_514$hiv*map_data_sub_514$art * samps_sens[["hiv_rr"]][k] * val) *   #~~
                                    (1 - map_data_sub_514$pem + samps_sens[["pem_rr"]][k] * map_data_sub_514$pem) *  #~~
                                    (1 - map_data_sub_514$bcg + samps_sens[["z"]][k] * mn * map_data_sub_514$bcg) *  #~~
                                    (map_data_sub_514$contacts_inc/map_data_sub_514$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_514$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))  #~~
        
      }
      
      # adjust estimates
      all_res_sens_adj[i,j,1, ] <- all_res_sens[i,j,1, ] * max(1, map_data_sub_04$newrel_04_total   / mean(all_res_sens[i,j,1, ]) )   #~~
      all_res_sens_adj[i,j,2, ] <- all_res_sens[i,j,2, ] * max(1, map_data_sub_514$newrel_514_total / mean(all_res_sens[i,j,2, ]) )
      
    }
    
  }
  
  # replace NA values with 0 (Grenada 2014)
  all_res_sens_adj[which(is.na(all_res_sens_adj))] <- 0
  
  # calculate global estimates by year
  temp_2_sens <- apply(all_res_sens_adj,c(2,4),sum)
  new_res3_sens <- apply(temp_2_sens,1,function(x) c(mean(x),quantile(x,c(1,39)/40)))
  
  art_rr_sens[index] <- new_res3_sens[1, 7]
  
  print(art_rr_sens[index])
  
  index = index + 1
  
}

# merge results with values to create dataframe to facilitate plotting
art_rr_sens_plot <- data.frame(val=art_rr_vals, est=art_rr_sens)

# plot results of sensitivity analysis 
plot_list[[10]] <- ggplot(art_rr_sens_plot, aes(x=val, y=est)) + 
  geom_line(size=1) + ylim(500000, 1500000) + ggtitle("(J)") + 
  geom_vline(xintercept=0.3005776, linetype="dotted", color="blue", size=1.5) + 
  xlab(expression(paste("Value of ", italic(m^t)))) + ylab("Modeled number of TB cases") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank())

## SENSITIVITY ANALYSIS WITH RESPECT TO RELATIVE CASE DETECTION PROBABILITY PARAMETER

# declare vector of values to fix for contact saturation parameter
rr_ped_detect_vals <- seq(from = 0.8, to = 0.95, length.out = 10)

# initialize vector for storing results of sensitivity analysis
rr_ped_detect_sens <- rep(0, length(rr_ped_detect_vals))

# initialize storage index
index <- 1

# run optimization and produce incidence estimates for parameter sets produced by Stan for each value of parameter
for (val in rr_ped_detect_vals){
  
  print(val)
  
  # declare inputs
  datList_sens <- list(
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
    rr_ped_detect = val,
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
    art_rr_b = art_rr_priors[[2]],  #~~
    shape_rate = 23)
  
  # fit model
  source("stan_rr_ped_detect_sens_revision.stan")
  fit_stan_rr_ped_detect_sens <- rstan::stan(
    model_code = stan_code, 
    data    = datList_sens,
    seed    = 123,
    chains  = 4,
    iter    = 11000,
    warmup  = 10000)
  
  # extract samples from fitted model 
  samps_sens <- extract(fit_stan_rr_ped_detect_sens)
  
  # declare array
  all_res_sens <- array(NA,dim=c(n_country, n_year, n_age, n_sim), 
                        dimnames = list(map_countries_revision, years, c("0-4", "5-14"), 1:4000))
  all_res_sens_adj <- all_res_sens
  
  #### 2 Fill in array
  # iterate through countries
  for(i in 1:n_country){
    
    # iterate through years
    for(j in 1:n_year){
      
      # select data for given country and year  
      map_data_sub_04 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="0-4")
      map_data_sub_514 <- subset(map_data_revision, country==map_countries_revision[i] & year==years[j] & age_group=="5-14")
      map_data_sub_04$p_tx_it <- min(c(0.99,map_data_sub_04$p_tx_it))  #~~
      map_data_sub_514$p_tx_it <- min(c(0.99,map_data_sub_514$p_tx_it))  #~~
      
      # choose appropriate rate ratio
      if (map_data_sub_04$lat_cat[1]=="above40"){
        
        mn = 0.31
        
      } else if (map_data_sub_04$lat_cat[1]=="20to40"){
        
        mn = 0.68
        
      } else {
        
        mn = 0.77
        
      }
      
      # iterate through samples
      for (k in 1:n_sim){
        
        duration = samps_sens[["dur_untx"]][k] * (1 - map_data_sub_04$p_tx_it * (1 - samps_sens[["fract_early"]][k])); #~~
        
        # calculate 0-4 cases
        all_res_sens[i,j,1,k] <- (map_data_sub_04$pop * samps_sens[["active_04"]][k] * 
                                    # (1 - map_data_sub_04$hiv + samps_sens[[8]][k] * map_data_sub_04$hiv) *   #~~
                                    (1 - map_data_sub_04$hiv + map_data_sub_04$hiv*(1-map_data_sub_04$art) * samps_sens[["hiv_rr"]][k] + #~~
                                       map_data_sub_04$hiv*map_data_sub_04$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *     #~~
                                    (1 - map_data_sub_04$pem + samps_sens[["pem_rr"]][k] * map_data_sub_04$pem) * #~~
                                    (1 - map_data_sub_04$bcg + samps_sens[["z"]][k] * mn * map_data_sub_04$bcg) *  #~~
                                    (map_data_sub_04$contacts_inc/map_data_sub_04$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_04$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))   #~~
        
        # calculate 5-14 cases
        all_res_sens[i,j,2,k] <- (map_data_sub_514$pop * samps_sens[["active_514"]][k] * #~~
                                    #     (1 - map_data_sub_514$hiv + samps_sens[[8]][k] * map_data_sub_514$hiv) *  #~~
                                    (1 - map_data_sub_514$hiv + map_data_sub_514$hiv*(1-map_data_sub_514$art) * samps_sens[["hiv_rr"]][k] +  #~~
                                       map_data_sub_514$hiv*map_data_sub_514$art * samps_sens[["hiv_rr"]][k] * samps_sens[["art_rr"]][k]) *   #~~
                                    (1 - map_data_sub_514$pem + samps_sens[["pem_rr"]][k] * map_data_sub_514$pem) *  #~~
                                    (1 - map_data_sub_514$bcg + samps_sens[["z"]][k] * mn * map_data_sub_514$bcg) *  #~~
                                    (map_data_sub_514$contacts_inc/map_data_sub_514$contact_sum) *
                                    ((1 - exp(-samps_sens[["infect"]][k] * duration * map_data_sub_514$contact_sum * samps_sens[["sat"]][k])) / samps_sens[["sat"]][k]))  #~~
        
      }
      
      # adjust estimates
      all_res_sens_adj[i,j,1, ] <- all_res_sens[i,j,1, ] * max(1, map_data_sub_04$newrel_04_total   / mean(all_res_sens[i,j,1, ]) )   #~~
      all_res_sens_adj[i,j,2, ] <- all_res_sens[i,j,2, ] * max(1, map_data_sub_514$newrel_514_total / mean(all_res_sens[i,j,2, ]) )
      
    }
    
  }
  
  # replace NA values with 0 (Grenada 2014)
  all_res_sens_adj[which(is.na(all_res_sens_adj))] <- 0
  
  # calculate global estimates by year
  temp_2_sens <- apply(all_res_sens_adj,c(2,4),sum)
  new_res3_sens <- apply(temp_2_sens,1,function(x) c(mean(x),quantile(x,c(1,39)/40)))
  
  rr_ped_detect_sens[index] <- new_res3_sens[1, 7]
  
  print(rr_ped_detect_sens[index])
  
  index = index + 1
  
}

# merge results with values to create dataframe to facilitate plotting
rr_ped_detect_sens_plot <- data.frame(val=rr_ped_detect_vals, est=rr_ped_detect_sens)

# plot results of sensitivity analysis 
plot_list[[11]] <- ggplot(rr_ped_detect_sens_plot, aes(x=val, y=est)) + 
  geom_line(size=1) + ylim(500000, 1500000) + ggtitle("(K)") + 
  geom_vline(xintercept=0.9060522, linetype="dotted", color="blue", size=1.5) + 
  xlab(expression(paste("Value of ", italic(g)))) + ylab("Modeled number of TB cases") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank())

## COMBINING PLOTS

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

# FIGURE S3
grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]],
                  plot_list[[6]], plot_list[[7]], plot_list[[8]], plot_list[[9]], plot_list[[10]],
                  plot_list[[11]], legend,
                  ncol=3, nrow = 4, 
                  widths = c(1.75, 1.75, 1.75), heights = c(0.75, 0.75, 0.75, 0.75))

#grid.arrange(g, legend, ncol=1, nrow=2, heights=c(4, 0.2))
