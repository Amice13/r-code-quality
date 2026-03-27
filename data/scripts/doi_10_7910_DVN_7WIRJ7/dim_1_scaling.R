#   UNSC Debates over the Syrian Civil War scaling
#       Dim 1: Human Rights Violations (a.k.a. CvS)
#   Juraj Medzihorsky
#   2017-02-28

rm(list=ls())


#   libraries
    library(quanteda)
    library(rstan)
    library(parallel)


#   misc  
    rstan_options(auto_write=TRUE)
    options(mc.cores=parallel::detectCores())  
    options(stringsAsFactors=F)


#   load the data with the speeches
load('d20160511.RData')


#   process the data

#   #   Syria 7433 duplicate
#   d$country[d$meeting==7433]
aux_syria_7433_1 <- d$speech[(d$country=='Syria')&(d$meeting==7433)] 
aux_syria_7433_2 <- d$speech[(d$country=='Syrian Arab Republic')&(d$meeting==7433)] 
aux_syria_7433_both <- paste(aux_syria_7433_1, aux_syria_7433_2, collapse=' ')
d$speech[(d$country=='Syrian Arab Republic')&(d$meeting==7433)] <- aux_syria_7433_both
out_syria_7433_1 <- which((d$country=='Syria')&(d$meeting==7433))
d <- d[-out_syria_7433_1, ]    


#   Shorten selected country names
d$country[grep('^Syr', d$country)] <- 'Syria'
d$country[grep('^Russ', d$country)] <- 'Russia'
d$country[grep('^United K', d$country)] <- 'UK'
d$country[grep('^United S', d$country)] <- 'USA'
d$country[grep('^United A', d$country)] <- 'UAE'
d$country[grep('^Islamic Rep', d$country)] <- 'Iran'
d$country[grep('^Bolivarian', d$country)] <- 'Venezuela'
d$country[grep('^Plurin', d$country)] <- 'Bolivia'
d$country[grep('^Bosnia', d$country)] <- 'BiH'
d$country[grep('^Republic of K', d$country)] <- 'South Korea'


#   load and process the dictionary
dictionary_text <- readLines('dictionary_20160527.txt')
dict <- sapply(dictionary_text, function(x) strsplit(x, ': ')[[1]][-1], USE.NAMES=F)
dict <- strsplit(dict, ', ')[-5]
names(dict) <- c('proc', 'act', 'conf', 'crim')


#   create the DFM 
mat_1 <- dfm(d$speech, dictionary=dict, stem=TRUE)

#   work copy
pm <- as.data.frame(as.matrix(mat_1))


#   a new dataframe, for now all countries
a <- d[, 1:3]
a$wp1 <- pm$proc
a$wp2 <- pm$conf
a$wn1 <- pm$act
a$wn2 <- pm$crim
a$n1 <- pm$proc + pm$act
a$n2 <- pm$conf + pm$crim



#   long form, only dim CvS here
l <- a[a$n1>0, ]

#   find countries that spoke at > 1 meeting
countries_to_take <- names(table(l$country)[table(l$country)>1])

l <- l[l$country%in%countries_to_take, ]


#   recode the meetings into integers by overall chronological order
meetings <- unique(l$meeting)
meetings <- meetings[order(as.numeric(meetings))]
l$meet_fact <- match(l$meeting, meetings)


#   recode the meetings within countries by local chronological order

#   first split the data by country
s <- split(l, f=as.factor(l$country))

#   a function to recode within countries
aux.relative <-
    function(x)
    {
        x <- x[order(x$meet_fact), ]
        x$relative <- 1:nrow(x)
        x$relat_max <- nrow(x)
        x$dist <- c(999^2, sapply(2:nrow(x), function(i) x$meet_fact[i]-x$meet_fact[i-1])) 
        x$sqrt_dist <- sqrt(x$dist)
        return(x)
    }

#   apply the function and merge the data
l <- do.call(rbind, lapply(s, aux.relative))

#    countries
countries <- unique(l$country)
countries <- countries[order(countries)]

l$coun_fact <- match(l$country, countries)

#   data list for stan
ldx <- list(y=l$wp2,
            nw=l$n2,
            meet_fact=l$meet_fact,
            meet_relative=l$relative,
            country=l$coun_fact, 
            n_ms=nrow(l),
            n_c=length(countries),
            n_m=length(meetings))
country_frame <- matrix(nrow=ldx$n_c, ncol=2)
for (tc in 1:ldx$n_c) {
    country_frame[tc, ] <- range(which(ldx$country==tc)) 
}
ldx$country_frame <- country_frame
ldx$omega <- l$sqrt_dist


model_code_1 <-
    'data {
        int<lower=1> n_ms;
        int<lower=1> n_c;
        int<lower=1> n_m;
        int<lower=0> y[n_ms];
        int<lower=1> nw[n_ms];
        int<lower=1> meet_fact[n_ms];
        int<lower=1> meet_relative[n_ms];
        int<lower=1> country[n_ms];
        int country_frame[n_c, 2];
        real<lower=1> omega[n_ms];
    }
    parameters {
        vector[n_m] gamma;
        real<lower=0> sigma_gamma;
        vector[n_ms] theta;
        vector<lower=0>[n_c] sigma_theta;
        real<lower=0> tau;
    }
    model {
        y ~ binomial_logit(nw, gamma[meet_fact] + theta);
        for (i in 1:n_ms) {
            if (meet_relative[i]==1) {
                theta[i] ~ normal(0, 1);
            } else {
                theta[i] ~ cauchy(theta[i-1], omega[i]*sigma_theta[country[i]]);
            }
        }
        sigma_theta ~ normal(0, tau);
        tau ~ normal(0, 1);
        gamma ~ normal(0, sigma_gamma);
        sigma_gamma ~ normal(0, 1);
    }
    generated quantities {
        real country_mean[n_c];
        real country_sd[n_c];
        for (tc in 1:n_c) {
            country_mean[tc] = mean(theta[country_frame[tc, 1]:country_frame[tc, 2]]);
            country_sd[tc] = sd(theta[country_frame[tc, 1]:country_frame[tc, 2]]);
        }
    }'
 
#   this is a test run, to see if it compiles
system.time(run_test <- stan(model_code=model_code_1, data=ldx, seed=19260723,
                             iter=1e1*2, thin=1, chains=2, cores=2))



#   this is the full run, it was executed on a 16-core machine
system.time(run_1 <- stan(model_code=model_code_1, data=ldx, seed=19260723,
                          iter=1e6*4, thin=2e3*4, chains=8, cores=8))


#   summarize the posteriors and inspect Rhat
st <- summary(run_1)
summary(st$summary[, 'Rhat'])

#   check the full summary
st


#   rename objects and save them
dim_cvs <- run_1
data_cvs <- ldx
save(list=c('countries', 'data_cvs', 'dim_cvs'), file='sim_cvs.RData')

#   NOTE:
#   In the paper the dimension is flipped by multiplying it with -1.
#   Subsequent scripts that process the stan output do this.


#   SCRIPT END