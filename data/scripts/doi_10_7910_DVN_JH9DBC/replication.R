# Milena Ang
# August 2025
# Replication code for "Guilty by Association"
# https://doi.org/10.1177/1354068825136682

# Setup: Set working directory to source file location

# Libraries needed
library(haven)
library(tidyr)
library(dplyr)
library(lme4)
library(fixest)
library(sjPlot)
library(cobalt)

# Full dataset
dataall <- read_dta("data.dta")

# Keeping 2015 and 2018 for main analysis
data <- dataall |>
  filter(year_2012 == 0)

################################################################################
############################ REPLICATION FOR PAPER ############################# 
################################################################################


## Table 2. Votes issued for legislative candidates of governor’s party
## (multilevel models)

model1 <- lmer(voto_pro_gober ~ 
                    juicio +
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    (1|unique_id) +
                    (1|partido_gober) ,
                  data = data)

model2 <- lmer(voto_pro_gober ~ 
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    (1|unique_id) +
                    (1|partido_gober),
                  data = data)

model3 <- lmer(voto_pro_gober ~ 
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100 +
                    mentions_prop +
                    mentions_prop*year_2018 +
                    (1|unique_id)+
                    (1|partido_gober),
                  data = data)

model4 <- lmer(voto_pro_gober ~
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    prev_exp_fleg +
                    (1|unique_id) +
                    (1|partido_gober),
                  data = data)

################################################################################
## Figure 1: Predicted vote in 2012, 2015, and 2018.

pred_mod <- lmer(voto_pro_gober ~ 
                   juicio +
                   year_2015 +
                   year_2018 +
                   juicio*year_2015 +
                   juicio*year_2018 +
                   log_pobtot +
                   log_pib +
                   (1|unique_id) +
                   (1|partido_gober),
                 data = dataall)

unique_id <- as.factor(dataall$unique_id[33]) # Taking one district

year_2015 <- c(0,1,0)
year_2018 <- c(0,0,1)

# Dataframes for bootstrapping
data_0 <- as.data.frame(cbind(unique_id,
                              rep(0, length(year_2018)),
                              year_2015,
                              year_2018,
                              rep(mean(data$log_pobtot), length(year_2018)), 
                              rep(mean(data$log_pib), length(year_2018))))

data_1 <-as.data.frame(cbind(unique_id,
                             rep(1, length(year_2018)),
                             year_2015,
                             year_2018, 
                             rep(mean(data$log_pobtot), length(year_2018)),
                             rep(mean(data$log_pib), length(year_2018))))

names(data_0) <- names(data_1) <- c("unique_id",
                                    "juicio",
                                    "year_2015",
                                    "year_2018",
                                    "log_pobtot",
                                    "log_pib")
# Bootstrapping:
# Code adapted from http://tinyurl.com/y7oevwky
b_0 <- bootMer(pred_mod, nsim=1000, 
               FUN=function(x)predict(x, newdata=data_0,
                                      re.form=NA))
b_1 <- bootMer(pred_mod, nsim=1000, 
               FUN=function(x)predict(x, newdata=data_1,
                                      re.form=NA))

data_0$pred <- NA
data_0$lower <- NA
data_1$lower <- NA
data_1$pred <- NA
data_0$upper <- NA
data_1$upper <- NA


for (i in 1:length(year_2018)) {
  data_0$pred[i] <- mean(b_0$t[,i])
  data_0$lower[i] <- quantile(b_0$t[,i], 0.025)
  data_0$upper[i] <- quantile(b_0$t[,i], 0.975)
  data_1$pred[i] <- mean(b_1$t[,i])
  data_1$lower[i] <- quantile(b_1$t[,i], 0.025)
  data_1$upper[i] <- quantile(b_1$t[,i], 0.975)
}

data_0$xpos <- c(0:2)
data_1$xpos <- c(0:2)

plot(0,0, pch = "",
     xlim = c(-0.05,2.05),
     ylim = c(0,0.4),
     axes = F,
     xlab = "",
     ylab = "Predicted vote for party of governor",
     cex.lab = 1)

# X-axis:
axis(1, at = c(0,1,2),
     labels = c("2012", "2015", "2018"),
     cex.axis= 1,
     tck=-0.01,
     las = 1)
#Y-axis:
axis(2, at = seq(0,0.4,0.05),
     cex.axis= 0.75,
     tck=-0.01,
     las = 1)


# When indicted equal to zero
lines(data_0$xpos,
      data_0$pred,
      cex = 1.2,
      col = "gray")

# When  indicted equal to one
lines(data_1$xpos,
      data_1$pred,
      pch = 16,
      cex = 1.2,
      col = "black")

# Predicted line
slope <- (data_0$pred[3]-data_0$pred[2])/(data_0$x[3]-data_0$x[2])
yval <- data_1$pred[2] + (slope)
lines(c(1,2),
      c(data_1$pred[2],yval),
      lty = 2,
      col = "black")

rm(b_0, b_1, data_0, data_1, pred_mod, i, slope, unique_id, year_2015, year_2018, yval)
################################################################################
########################### REPLICATION FOR APPENDIX ########################### 
################################################################################

## Table A: Descriptive Statistics
summary(data$voto_pro_gober)
summary(data$juicio)
summary(data$year_2018)
summary(data$log_pobtot)
summary(data$log_pib)
summary(data$econ_growth)
summary(data$hom_per100)
summary(data$mentions_prop)
summary(data$prev_exp_fleg)

################################################################################
## Figure A: Balance of Covariates
dataloveplot  <- dataall |>
  filter(year_2015 == 1) # Pretreatment

wdata <- WeightIt::weightit(
  juicio ~
    log_pobtot +
    log_pib +
    econ_growth +
    hom_per100 +
    mentions_prop +
    prev_exp_fleg,
  data = dataloveplot, estimand = "ATE", method = "ps")

set.cobalt.options(binary = "std")

love.plot(wdata, 
          drop.distance = TRUE, 
          var.order = "unadjusted",
          abs = TRUE,
          line = TRUE, 
          thresholds = c(m = .1),
          var.names = c(year_2018 = "Year 2018",
                        log_pobtot = "Population (logged)",
                        log_pib = "State GDP (logged)",
                        econ_growth = "Economic growth",
                        hom_per100 = "Homicides (per 100,000)",
                        mentions_prop = "Scandal",
                        prev_exp_fleg = "Candidate's experience"),
          sample.names = c("Unweighted", "PS Weighted"),
          colors = c("gray", "black"))

rm(dataloveplot, wdata)
################################################################################
## Table B: Votes issued for Legislative Candidates of Governor Party
## (fixed effects and clustered standard errors)

## Note we do not include indicted as covariate because of collinearity (district)

model1B <- feols(voto_pro_gober ~ 
                      year_2018 + 
                      juicio*year_2018 +
                      log_pobtot +
                      log_pib |
                      unique_id +
                      partido_gober,
            data = data)

model2B <- feols(voto_pro_gober ~
                      year_2018 + 
                      juicio*year_2018 +
                      log_pobtot +
                      log_pib +
                      econ_growth +
                      hom_per100 |
                      unique_id +
                      partido_gober,
            data = data)

model3B <- feols(voto_pro_gober ~
                      year_2018 + 
                      juicio*year_2018 +
                      log_pobtot +
                      log_pib +
                      econ_growth +
                      hom_per100 +
                      mentions_prop +
                      mentions_prop*year_2018 |
                      unique_id +
                      partido_gober,
            data = data)

model4B <- feols(voto_pro_gober ~
                      year_2018 + 
                      juicio*year_2018 +
                      log_pobtot +
                      log_pib +
                      econ_growth +
                      hom_per100 +
                      prev_exp_fleg |
                      unique_id +
                      partido_gober,
            data = data)

################################################################################
## Table C: Votes issued for legislative candidates of governor's party
## (parallel trends assumption)

# Only socioeconomic controls
model1C <- lmer(voto_pro_gober ~ 
                    juicio +
                    year_2015 +
                    year_2018 +
                    juicio*year_2015 +
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    (1|unique_id) +
                    (1|partido_gober),
                  data = dataall)

model2C <- lmer(voto_pro_gober ~ 
                    juicio+
                    year_2015 +
                    year_2018 + 
                    juicio*year_2015 +
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100 +
                    (1|unique_id) +
                    (1|partido_gober) ,
                  data = dataall)

model3C <- lmer(voto_pro_gober ~
                    juicio+
                    year_2015 +
                    year_2018 + 
                    juicio*year_2015 +
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    mentions_prop+
                    mentions_prop*year_2018 +
                    (1|unique_id) +
                    (1|partido_gober),
                  data = dataall)

################################################################################
## Table D: Votes issued for Legislative Candidates of Governor Party
## (lagged dependent variable)
datalagged <- dataall |>
  arrange(unique_id, elec_year) |>
  group_by(unique_id) |>
  mutate(lag_voto_pro_gober = dplyr::lag(voto_pro_gober, n = 1, default = NA)) |>
  filter(year_2012 != 1)

model1D <- lmer(lag_voto_pro_gober ~ 
                    juicio +
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    (1|unique_id) +
                    (1|partido_gober) ,
                  data = datalagged)

model2D <- lmer(lag_voto_pro_gober ~ 
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    (1|unique_id) +
                    (1|partido_gober),
                  data = datalagged)

model3D <- lmer(lag_voto_pro_gober ~
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    mentions_prop +
                    mentions_prop *year_2018 +
                    (1|unique_id) +
                    (1|partido_gober),
                  data = datalagged)

model4D <- lmer(lag_voto_pro_gober ~
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    prev_exp_fleg +
                    (1|unique_id) +
                    (1|partido_gober),
                  data = datalagged)

rm(datalagged)
################################################################################
## Figure B: Placebo treatment effects
set.seed(100)

# Making copy of data for placebo treatment
datar <- data |>
  # We delete all the actually treated states
  filter(id_estado != 8 &
           id_estado != 23 &
           id_estado != 26 &
           id_estado != 30) |>
  #Creating empty variable for randomly assigned indictment
  mutate(juicior = NA)

# Empty objects for filling
juicioyear <- data.frame(estimate = double(),
                         conf.low = double(),
                         conf.high = double())

## For each iteration
for (i in 1:100){
  # Randomly sample some states for treatment
  rstates <- sample(unique(datar$id_estado), 4, replace=F)
  # Set variable juicior equal to 1 for sampled states
  datar$juicior <- ifelse(datar$id_estado%in%rstates,1,0)
  # Estimate the model
  mod <- lmer(voto_pro_gober ~ 
                juicior +
                year_2018 + 
                juicior*year_2018 +
                log_pobtot +
                log_pib +
                econ_growth +
                hom_per100+
                (1|unique_id)+
                (1|partido_gober),
              data = datar)
  # Extract estimates for interaction and put them in juicioyeardataset
  juicioyear <- rbind(juicioyear, get_model_data(mod, type="est",ci.lvl = 0.95)[7, c(2,5,6)])
  remove(rstates, mod)
}

# Actual model, same specification, different dataset
mod <- lmer(voto_pro_gober ~ 
              juicio +
              year_2018 + 
              juicio*year_2018 +
              log_pobtot +
              log_pib +
              econ_growth +
              hom_per100+
              (1|unique_id)+
              (1|partido_gober),
            data = data)
# Extracting information of actual model
poly <- get_model_data(mod, type="est",ci.lvl = 0.5)[7,c(2,5,6)]

# With this information, we create a dummy variable

juicioyear <- juicioyear |>
  mutate(falsepos= ifelse(conf.low > 0 &
                            conf.high > 0, 1, 0),
         falseneg= ifelse(juicioyear$conf.low < 0 &
                            juicioyear$conf.high < 0, 1, 0),
         atleast = ifelse(juicioyear$conf.low < 0 &
                            juicioyear$conf.high < 0 &
                            juicioyear$estimate<poly$estimate,
                          1, 0))
## Figure
plot(0,0,
     pch = "",
     xlim = c(1,101),
     ylim = c(-0.15,0.15),
     xlab="",
     xaxt = "n",
     ylab="Estimates and 95% c.i.")

points(c(1:100), juicioyear[,1],
       pch=16, cex = 0.6)
segments(1:100, juicioyear[,2],
         1:100, juicioyear[,3])

segments(1,0, 100, 0,
         lty=2)


rm(poly, i, datar, juicioyear, mod)
################################################################################
## Table F: Testing for party and governor-specific effects

onlyPRI <- lmer(voto_pro_gober ~ 
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    (1|unique_id),
                  data = data[which(data$partido_gober == "PRI"),])


onlyPAN <- lmer(voto_pro_gober ~ 
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    (1|unique_id),
                  data = data[which(data$partido_gober == "PAN"),])

# With Nuevo leon as treated
datanl <- data |>
  mutate(juicio = ifelse(estado == "NUEVO LEON", 1, juicio))

withNL <- lmer(voto_pro_gober ~ 
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    (1|unique_id) +
                    (1|partido_gober),
                  data = datanl)

datanltm <- data |>
  mutate(juicio = ifelse(estado == "NUEVO LEON", 1, juicio)) |>
  mutate(juicio = ifelse(estado == "TAMAULIPAS", 1, juicio))

nlandtamps <- lmer(voto_pro_gober ~ 
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    (1|unique_id) +
                    (1|partido_gober),
                  data = datanltm)

datawover <- data |>
  mutate(juicio = ifelse(estado == "NUEVO LEON", 0, juicio)) |>
  mutate(juicio = ifelse(estado == "TAMAULIPAS", 0, juicio)) |>
  filter(estado != "VERACRUZ")

wover <- lmer(voto_pro_gober ~ 
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    (1|unique_id) +
                    (1|partido_gober),
                  data = datawover)

rm(datanl, datanltm, datawover)

################################################################################
## Table G: Votes issued for legislative candidates of governor party
## (only states that had an election)

datasubset <- data |>
  filter(year_2012 != 1) |>
  filter(st_elections_15_18 == 1)

model1G <- lmer(voto_pro_gober ~ 
                    juicio +
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    (1|unique_id) +
                    (1|partido_gober) ,
                  data = datasubset)

# Socioeconomic and performance
model2G <- lmer(voto_pro_gober ~ 
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    (1|unique_id) +
                    (1|partido_gober),
                  data = datasubset)

# Political
model3G <- lmer(voto_pro_gober ~ 
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    mentions_prop +
                    mentions_prop*year_2018 +
                    (1|unique_id)+
                    (1|partido_gober),
                  data = datasubset)

# Candidate experience
model4G <- lmer(voto_pro_gober ~
                    juicio+
                    year_2018 + 
                    juicio*year_2018 +
                    log_pobtot +
                    log_pib +
                    econ_growth +
                    hom_per100+
                    prev_exp_fleg +
                    (1|unique_id) +
                    (1|partido_gober),
                  data = datasubset)

rm(datasubset)

################################################################################
