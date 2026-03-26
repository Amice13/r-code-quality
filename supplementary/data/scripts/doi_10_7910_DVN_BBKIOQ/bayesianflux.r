#R
#chamber analysis guatavita
#from 2021 to 2024
#and bayesian annual modelling

# install.packages(c("rjags","coda","dplyr","ggplot2"))
library(rjags)
library(coda)
library(dplyr)
library(ggplot2)
library(minpack.lm)
library(photosynthesis)
library(RColorBrewer)
library(lubridate)



std <- function(x) sd(x)/sqrt(length(x))


pcdir<-"C:/Users/jubenavides/Dropbox/papers/Guatavita fluxes/stats"
macdir<-"/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/Guatavita fluxes/stats"

  setwd(macdir)

  fluxes<-read.csv("fluxes5.csv",header=TRUE)
  
  tz <- "America/Bogota"

  fluxes$date <- as.character(fluxes$Date)
  fluxes$time <- trimws(as.character(fluxes$Time))

  # if time is HH:MM, append :00; if it's already HH:MM:SS, leave it
  time_fixed <- ifelse(grepl("^\\d{1,2}:\\d{2}$", fluxes$time),
                       paste0(fluxes$time, ":00"),
                       fluxes$time)

  fluxes$datetime <- as.POSIXct(
    strptime(paste(fluxes$date, time_fixed), format = "%m/%d/%y %H:%M:%S", tz = tz)
  )

fluxes$Date4<-as.Date(fluxes$datetime)

fluxes$NEE_flux<-ifelse(fluxes$Day.Night=="Day", -fluxes$CO2_flux,NA)
fluxes$ER_flux<-ifelse(fluxes$Day.Night=="Night", fluxes$CO2_flux,NA) 


#seasons
season.start.date <- as.POSIXct(c("2021-01-01", "2021-03-01", "2021-9-01","2021-10-01","2021-11-01",
                                  "2022-03-01", "2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02", "2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21", "2024-10-06","2024-12-15",
                                  "2025-03-01"))
 
season.end.date <- as.POSIXct(c( "2021-03-01", "2021-9-01","2021-10-01","2021-11-01",
                                  "2022-03-01", "2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02", "2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21", "2024-10-06","2024-12-15",
                                  "2025-03-01","2025-07-15"))

season <- c("Dry", "Rain", "Dry", "Rain", "Dry" , "Rain", "Dry" , "Rain", "Dry" , "Rain",
      "Dry","Rain", "Dry","Rain", "Dry","Rain", "Dry","Rain")

seasons<-data.frame(season.start.date ,season.end.date ,season)
season_vec <- character(length(fluxes$Date4))

# Loop through each date
for (i in seq_along(fluxes$Date4)) {
  current_date <- fluxes$Date4[i]
  
  # Find row in seasons where date falls between start and end
  matched <- which(seasons$season.start.date <= current_date &
                   seasons$season.end.date >= current_date)
  
  # If match found, assign season
  if (length(matched) == 1) {
    fluxes$season[i] <- seasons$season[matched]
  }
}

# Counter for consecutive rain days
count <- 0
for (i in seq_len(nrow(precip2))) {
  if (precip2$Rain_mm_Tot[i]) {
      count <- count + 1
    } else {
      count <- 0
    }
    consec_rain[i] <- count
}

# Add result to data frame
precip2$consecutive_rain_days <- consec_rain


########################################################################################################################
########################################################################################################################
#fit light response

fluxGPP2_dry<-subset(fluxes,season=="Dry")
fluxGPP2_rain<-subset(fluxes,season=="Rain")

fit_dry1 <- nls(
  NEE_flux ~ (alpha * PAR) / (1 + (alpha * PAR / Amax)) - Rd,
  data = subset(fluxes,season=="Dry") ,
  start = list(alpha = 0.05, Amax = 12, Rd = 1),
  control = list(maxiter = 500)
)


fit_rain1 <- nlsLM(
  NEE_flux ~ (alpha * PAR) / (1 + (alpha * PAR / Amax)) - Rd,
  data = subset(fluxes,season=="Rain") ,
  start = list(alpha = 0.05, Amax = 10, Rd = 1),
  control = nls.lm.control(maxiter = 500)
)

# 1. Create a smooth range of PAR values for prediction
par_seq <- data.frame(PAR = seq(min(fluxes$PAR,na.rm=TRUE), max(fluxes$PAR,na.rm=TRUE), length.out = 200))

# 2. Predict fitted values
par_seq$A_fittedS1<- predict(fit_dry1, newdata = par_seq)
par_seq$A_fittedS2<- predict(fit_rain1, newdata = par_seq)



NEE_PAR<-ggplot(fluxes,aes(x=PAR,y=NEE_flux))+theme_bw()

 NEE_PAR1<-NEE_PAR +geom_point(aes(colour=season))+
                    geom_line(data= par_seq,aes(x=PAR,y=A_fittedS1),colour="#D55E00")+
                    geom_line(data= par_seq,aes(x=PAR,y=A_fittedS2),colour="#009E73")+
                    scale_colour_manual(name = "season", values = c("Dry" = "#D55E00", "Rain" = "#009E73")) +
                    scale_x_continuous("PAR radiation (umol m2 s)")+
                    scale_y_continuous(expression(paste("NEE (umolCO2 m"^ -2,"s"^ -1,")",sep="")))+
                     theme(legend.position=c(0.8,0.9),panel.grid.minor=element_blank  (),
                     panel.grid.major=element_blank(), panel.background=element_blank(),
                     axis.text.x =element_text(size = 12,vjust=1,angle=0,colour='black'),
                     axis.text.y =element_text(size = 12,angle=0,colour='black'),
                     axis.title.x = element_text(size = 12,colour='black'),
                     axis.title.y =element_text(size = 12, angle = 90,colour='black') )


NEE_CumPrec<-ggplot(fluxes,aes(x=precip30,y=NEE_flux))+theme_bw()

 NEE_CumPrec1<-NEE_CumPrec +geom_point(aes(colour=season))+
                    scale_colour_manual(name = "season", values = c("Dry" = "#D55E00", "Rain" = "#009E73")) +
                    scale_x_continuous("Cummulative rain 30 days")+
                    scale_y_continuous(expression(paste("NEE (umolCO2 m"^ -2,"s"^ -1,")",sep="")))+
                     theme(legend.position=c(0.8,0.9),panel.grid.minor=element_blank  (),
                     panel.grid.major=element_blank(), panel.background=element_blank(),
                     axis.text.x =element_text(size = 12,vjust=1,angle=0,colour='black'),
                     axis.text.y =element_text(size = 12,angle=0,colour='black'),
                     axis.title.x = element_text(size = 12,colour='black'),
                     axis.title.y =element_text(size = 12, angle = 90,colour='black') )

#NEE_CumPrec1




ER_WT<-ggplot(fluxes,aes(x=WTD,y=ER_flux))+theme_bw()

ER_WT1<-ER_WT +geom_point(aes(colour=Site2))+
                  scale_fill_grey("Season")+
                  scale_x_continuous("Water table (cm)")+
                  scale_y_continuous(expression(paste("ER (umolCO2 m"^ -2,"s"^ -1,")",sep="")))+
                   theme(legend.position=c(0.8,0.9),panel.grid.minor=element_blank  (),
                   panel.grid.major=element_blank(), panel.background=element_blank(),
                   axis.text.x =element_text(size = 12,vjust=1,angle=0,colour='black'),
                   axis.text.y =element_text(size = 12,angle=0,colour='black'),
                   axis.title.x = element_text(size = 12,colour='black'),
                   axis.title.y =element_text(size = 12, angle = 90,colour='black') )

#ER_WT1




CH4_WT<-ggplot(fluxes1,aes(x=WTD,y=CH4_flux))+theme_bw()

CH4_WT1<-CH4_WT +geom_point(aes(colour=Site2))+
                  scale_fill_grey("Season")+
                  scale_x_continuous("Water table (cm)")+
                  scale_y_continuous(expression(paste("CH4 (nmolCO2 m"^ -2,"s"^ -1,")",sep="")))+
                   theme(legend.position=c(0.8,0.9),panel.grid.minor=element_blank  (),
                   panel.grid.major=element_blank(), panel.background=element_blank(),
                   axis.text.x =element_text(size = 12,vjust=1,angle=0,colour='black'),
                   axis.text.y =element_text(size = 12,angle=0,colour='black'),
                   axis.title.x = element_text(size = 12,colour='black'),
                   axis.title.y =element_text(size = 12, angle = 90,colour='black') )

CH4_WT1




fluxes$Station<-ifelse(fluxes$Site2=="S1","Degraded","Conserved")

NEEPlot <- ggboxplot(fluxes, x = "season", y = "-NEE_flux",
                color = "Station", 
                add = "jitter", shape = "Station",
                xlab ="Season",
                legend="none")+
                scale_y_continuous(expression("NEE "*(mu*mol~m^{-2}~s^{-1})),lim=c(-20,22))+
                scale_color_manual(name = "Station", 
                    values = c("Degraded" = "#D55E00", "Conserved" = "#009E73")) +
                geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6, color = "grey30")


ERPlot <- ggboxplot(fluxes, x = "season", y = "ER_flux",
                color = "Station", 
                add = "jitter", shape = "Station",
                xlab ="Season",
                legend=c(0.15,0.8))+
                scale_y_continuous(expression("ER "*(mu*mol~m^{-2}~s^{-1})),lim=c(0,30))+
                scale_color_manual(name = "Station", 
                    values = c("Degraded" = "#D55E00", "Conserved" = "#009E73")) 


fluxes$CH4micromol<-fluxes$CH4_flux/1000

MetPlot <- ggboxplot(fluxes, x = "season", y = "CH4micromol",
                color = "Station", 
                add = "jitter", shape = "Station",
                xlab ="Season",
                legend="none")+
                scale_y_continuous(expression("CH4 flux "*(mu*mol~m^{-2}~s^{-1})),lim=c(0,1))+
                scale_color_manual(name = "Station", 
                    values = c("Degraded" = "#D55E00", "Conserved" = "#009E73")) 


#grid.arrange(NEEPlot,ERPlot,MetPlot)


fluxes1<-fluxes

##########################################################################################
##########################################################################################
################################################################################################
################################################################################################
#bayesian model chamber flux

fluxes<-subset(fluxes,Site2=="S1")
fluxes$NEE_flux<- fluxes$CO2_flux

# --- 0) Packages ---
library(rjags)
library(coda)

# --- 1) Data prep: keep DAY only, tidy factors, standardize covariates ---
df <- subset(fluxes, is.finite(NEE_flux) & is.finite(PAR) & PAR > 5 &  # day threshold
                        is.finite(precip30) & is.finite(SoilTemp) & is.finite(WTD) &
                        !is.na(Plot) & !is.na(season))

df$Plot   <- factor(df$Plot)
df$season <- factor(df$season)

# Standardize hydrology covariates for interpretability
z <- function(x) as.numeric(scale(x))
precip_z <- z(df$precip30)
wtd_z    <- z(df$WTD)          # If your WTD sign means deeper = larger, effect should emerge in gamma_WTD
Tref     <- 10                 # °C reference for Q10

plot_id   <- as.integer(df$Plot)
season_id <- as.integer(df$season)

jdat <- list(
  N        = nrow(df),
  NEE      = df$NEE_flux,
  PAR      = pmax(df$PAR, 0),
  soilT    = df$SoilTemp,
  Tref     = Tref,
  precip_z = precip_z,
  wtd_z    = wtd_z,
  plot     = plot_id,
  season   = season_id,
  n_plot   = nlevels(df$Plot),
  n_season = nlevels(df$season)
)

# --- 2) JAGS model ---
model_string <- "
model{
  # Priors
  alpha0      ~ dnorm(0, 0.01)                    # log-ER intercept
  lQ10        ~ dnorm(log(2.0), 1.0) T(0,)        # log(Q10), truncated to enforce Q10>1
  betaT       <- lQ10 / 10                        # per °C slope on log-ER
  gamma_rain  ~ dnorm(0, 0.01)                    # standardized precip30 effect
  gamma_wtd   ~ dnorm(0, 0.01)                    # standardized WTD effect

  tau_plot    ~ dgamma(0.1, 0.1)
  tau_season  ~ dgamma(0.1, 0.1)

  # Robust residuals
  sigma ~ dunif(0, 50)
  tau   <- pow(sigma, -2)
  nu    ~ dunif(2, 30)

  # Hyperpriors for Pmax and Ek (season-specific, positive on log scale)
  mu_log_Pmax  ~ dnorm(log(10), 1.0)
  tau_log_Pmax ~ dgamma(0.1, 0.1)

  mu_log_Ek    ~ dnorm(log(200), 1.0)
  tau_log_Ek   ~ dgamma(0.1, 0.1)

  for(s in 1:n_season){
    log_Pmax[s] ~ dnorm(mu_log_Pmax, tau_log_Pmax)
    Pmax[s]     <- exp(log_Pmax[s])

    log_Ek[s]   ~ dnorm(mu_log_Ek, tau_log_Ek)
    Ek[s]       <- exp(log_Ek[s])

    b_season[s] ~ dnorm(0, tau_season)
  }

  for(p in 1:n_plot){
    b_plot[p] ~ dnorm(0, tau_plot)
  }

  # Likelihood
  for(i in 1:N){
    # ER driver: Q10 with soil T, hydrology & precip effects, plus random effects
    log_ER[i] <- alpha0 + betaT * (soilT[i] - Tref) +
                 gamma_wtd * wtd_z[i] + gamma_rain * precip_z[i] +
                 b_season[season[i]] + b_plot[plot[i]]
    ER[i]     <- exp(log_ER[i])                   # enforce ER > 0

    # GPP: saturating light response (positive)
    GPP[i] <- Pmax[season[i]] * ( PAR[i] / (PAR[i] + Ek[season[i]] + 1.0E-06) )

    mu[i] <- ER[i] - GPP[i]                       # NEE = ER - GPP

    NEE[i] ~ dt(mu[i], tau, nu)                   # robust residuals
    y_rep[i] ~ dt(mu[i], tau, nu)
  }
}
"

# --- 3) Compile, adapt, and sample ---
set.seed(1)
jm <- jags.model(textConnection(model_string), data=jdat,
                 inits=list(.RNG.name="base::Wichmann-Hill", .RNG.seed=1),
                 n.chains=3, n.adapt=2000)
update(jm, 5000)  # burn-in

mon <- c("alpha0","lQ10","betaT","gamma_rain","gamma_wtd","sigma","nu",
         "Pmax","Ek","b_season","b_plot","mu","ER","GPP")

samp <- coda.samples(jm, variable.names=mon, n.iter=8000, thin=4)

# --- 4) Fitted values & components (posterior means) ---
post <- as.matrix(samp)

mu_cols  <- grepl("^mu\\[",  colnames(post))
ER_cols  <- grepl("^ER\\[",  colnames(post))
GPP_cols <- grepl("^GPP\\[", colnames(post))

mu_hat  <- colMeans(post[, mu_cols,  drop=FALSE])
ER_hat  <- colMeans(post[, ER_cols,  drop=FALSE])
GPP_hat <- colMeans(post[, GPP_cols, drop=FALSE])

# --- 5) Pseudo R^2 on daytime NEE ---
pseudo_r2 <- function(y,yhat){
  ok <- is.finite(y)&is.finite(yhat); y<-y[ok]; yhat<-yhat[ok]
  1 - sum((y - yhat)^2) / sum((y - mean(y))^2)
}
R2_nee_day <- pseudo_r2(jdat$NEE, mu_hat)
cat(sprintf("Daytime NEE pseudo R^2: %.3f\n", R2_nee_day))

# --- 6) Quick diagnostics ---
plot(mu_hat, jdat$NEE, xlab="Fitted NEE (posterior mean μ)", ylab="Observed NEE",
     main=sprintf("Obs vs Fitted (Daytime)  pseudo R^2 = %.2f", R2_nee_day))
abline(0,1,lty=2)

# --- 7) Tidy outputs to preserve identity for later merging ---
out_day <- data.frame(
  obs_id   = seq_len(nrow(df)),
  Plot     = df$Plot,
  season   = df$season,
  PAR      = df$PAR,
  soiltemp = df$SoilTemp,
  WTD      = df$WTD,
  precip30 = df$precip30,
  NEE_obs  = df$NEE_flux,
  NEE_fit  = mu_hat,
  ER_day   = ER_hat,
  GPP_day  = GPP_hat
)

# (optional) write.csv(out_day, "nee_day_postmeans.csv", row.names = FALSE)

# --- 8) Interpretable effect summaries ---
q_lQ10 <- quantile(post[,"lQ10"], c(0.025,0.5,0.975))
Q10_median <- exp(q_lQ10[2])
Q10_CI <- exp(q_lQ10[c(1,3)])
cat(sprintf("Q10 median %.2f (95%% CI %.2f–%.2f)\n", Q10_median, Q10_CI[1], Q10_CI[2]))

q_wtd  <- quantile(post[,"gamma_wtd"],  c(0.025,0.5,0.975))
q_rain <- quantile(post[,"gamma_rain"], c(0.025,0.5,0.975))
print(rbind(gamma_wtd=q_wtd, gamma_rain=q_rain))

###############################################################
###############################################################


# --- 1) Shared standardization (so day & night use identical z-scores) ---
# If you already saved these from the day model, reuse them.
finite_all <- with(fluxes, is.finite(precip30) & is.finite(WTD))
precip_center <- mean(fluxes$precip30[finite_all], na.rm=TRUE)
precip_scale  <- sd(  fluxes$precip30[finite_all], na.rm=TRUE)
wtd_center    <- mean(fluxes$WTD[finite_all], na.rm=TRUE)
wtd_scale     <- sd(  fluxes$WTD[finite_all], na.rm=TRUE)
z_with <- function(x, m, s) (x - m) / s

# --- 2) Night subset ---
par_threshold <- 5  # adjust if needed
dfN <- subset(fluxes,
              is.finite(NEE_flux) & is.finite(PAR) & PAR <= par_threshold &
              is.finite(SoilTemp) & is.finite(WTD) & is.finite(precip30) &
              !is.na(Plot) & !is.na(season))

dfN$Plot   <- factor(dfN$Plot,   levels = levels(factor(fluxes$Plot)))
dfN$season <- factor(dfN$season, levels = levels(factor(fluxes$season)))

precip_z_N <- z_with(dfN$precip30, precip_center, precip_scale)
wtd_z_N    <- z_with(dfN$WTD,      wtd_center,    wtd_scale)
Tref <- 10  # °C reference for Q10

jdatN <- list(
  N        = nrow(dfN),
  NEE      = dfN$NEE_flux,     # at night, NEE ≈ ER
  soilT    = dfN$SoilTemp,
  Tref     = Tref,
  precip_z = precip_z_N,
  wtd_z    = wtd_z_N,
  plot     = as.integer(dfN$Plot),
  season   = as.integer(dfN$season),
  n_plot   = nlevels(dfN$Plot),
  n_season = nlevels(dfN$season)
)

# --- 3) JAGS model: ER-only; NEE ~ ER + noise (t_ν) ---
model_night <- "
model{
  # Priors
  alpha0      ~ dnorm(0, 0.01)                 # log-ER intercept
  lQ10        ~ dnorm(log(2.0), 1.0) T(0,)     # log(Q10) > 0  => Q10>1
  betaT       <- lQ10 / 10                     # per °C on log-ER
  gamma_rain  ~ dnorm(0, 0.01)                 # standardized precip30
  gamma_wtd   ~ dnorm(0, 0.01)                 # standardized WTD

  tau_plot    ~ dgamma(0.1, 0.1)
  tau_season  ~ dgamma(0.1, 0.1)

  sigma ~ dunif(0, 50)
  tau   <- pow(sigma, -2)
  nu    ~ dunif(2, 30)

  for(s in 1:n_season){
    b_season[s] ~ dnorm(0, tau_season)
  }
  for(p in 1:n_plot){
    b_plot[p] ~ dnorm(0, tau_plot)
  }

  for(i in 1:N){
    log_ER[i] <- alpha0 + betaT * (soilT[i] - Tref) +
                 gamma_wtd * wtd_z[i] + gamma_rain * precip_z[i] +
                 b_season[season[i]] + b_plot[plot[i]]
    ER[i]  <- exp(log_ER[i])                   # ER > 0
    mu[i]  <- ER[i]                            # NEE = ER (night)
    NEE[i] ~ dt(mu[i], tau, nu)
    y_rep[i] ~ dt(mu[i], tau, nu)
  }
}
"

set.seed(42)
jmN <- jags.model(textConnection(model_night), data=jdatN,
                  inits=list(.RNG.name="base::Wichmann-Hill", .RNG.seed=42),
                  n.chains=3, n.adapt=2000)
update(jmN, 5000)

monN <- c("alpha0","lQ10","betaT","gamma_rain","gamma_wtd","sigma","nu",
          "b_season","b_plot","ER","mu")
sampN <- coda.samples(jmN, variable.names=monN, n.iter=8000, thin=4)

# Posterior means & pseudo-R2 for night
postN   <- as.matrix(sampN)
muNhat  <- colMeans(postN[, grepl("^mu\\[",  colnames(postN)), drop=FALSE])
ERNhat  <- colMeans(postN[, grepl("^ER\\[",  colnames(postN)), drop=FALSE])

pseudo_r2 <- function(y,yhat){ ok <- is.finite(y)&is.finite(yhat); y<-y[ok]; yhat<-yhat[ok]
  1 - sum((y - yhat)^2) / sum((y - mean(y))^2) }
R2_nee_night <- pseudo_r2(jdatN$NEE, muNhat)
cat(sprintf("Night NEE~ER pseudo R^2: %.3f\n", R2_nee_night))

# Night output table (keeps identities clear)
out_night <- data.frame(
  obs_id_night = seq_len(nrow(dfN)),
  Plot     = dfN$Plot,
  season   = dfN$season,
  PAR      = dfN$PAR,
  soiltemp = dfN$SoilTemp,
  WTD      = dfN$WTD,
  precip30 = dfN$precip30,
  NEE_obs  = dfN$NEE_flux,
  NEE_fit  = muNhat,   # equals ER at night
  ER_night = ERNhat
)
# (optional) write.csv(out_night, "nee_night_postmeans.csv", row.names = FALSE)

# Useful interpretable priors/posteriors
qN_lQ10 <- quantile(postN[,"lQ10"], c(0.025,0.5,0.975))
cat(sprintf("Night Q10 median %.2f (95%% CI %.2f–%.2f)\n",
            exp(qN_lQ10[2]), exp(qN_lQ10[1]), exp(qN_lQ10[3])))


###############################################################
###############################################################

library(dplyr)
library(ggplot2)

# --- 1) Load new prediction dataset ---
# Must contain: datetime, PAR, soiltemp, WTD, precip30, season
pred_new <- st1mod3
pred_new$datetime <- as.POSIXct(pred_new$datetime, tz="UTC")
pred_new$season <- as.factor(pred_new$season)
# Save posterior draws from earlier fits
post_day   <- post      # from the daytime model
post_night <- postN     # from the nighttime model

post_day_s1   <- post      # from the daytime model
post_night_s1 <- postN     # from the nighttime model

## ---------- SAFE PREDICTOR FOR 30-MIN NEE & CUMULATIVE (2023–2024) ----------

# Inputs required in the current session:
# - post_day : posterior samples matrix (as.matrix(coda.samples(...))) from DAY model
# - pred_new : data.frame with columns: datetime, PAR, WTD, precip30, season, and soiltemp or SoilTemp
# Optional:
# - fluxes   : original training data (improves scaling & clamping if available)

stopifnot(exists("post_day"))
stopifnot(all(c("datetime","PAR","WTD","precip30","season") %in% names(pred_new)))

pred_new$datetime <- as.POSIXct(pred_new$datetime, tz="UTC")
soilT <- if ("soiltemp" %in% names(pred_new)) pred_new$soiltemp else
         if ("SoilTemp" %in% names(pred_new)) pred_new$SoilTemp else
         stop("Provide soil temperature as 'soiltemp' or 'SoilTemp'.")

## --- training-based scaling & clamping helpers ---
get_stats <- function(x){
  x <- x[is.finite(x)]
  if (!length(x)) return(list(center=0, scale=1, lo=0, hi=0))
  qs <- quantile(x, c(0.01,0.99), na.rm=TRUE)
  list(center = mean(x), scale = sd(x), lo = qs[1], hi = qs[2])
}
winsor  <- function(x, lo, hi) pmax(pmin(x, hi), lo)
safe_sd <- function(s) ifelse(is.finite(s) && s > 1e-8, s, 1)
z_safe  <- function(x, st) (winsor(x, st$lo, st$hi) - st$center) / safe_sd(st$scale)

if (exists("fluxes")) {
  st_T   <- get_stats(fluxes$soiltemp)
  st_WTD <- get_stats(fluxes$WTD)
  st_PPT <- get_stats(fluxes$precip30)
  par_max_train <- max(fluxes$PAR, na.rm=TRUE)
} else {
  st_T   <- get_stats(soilT)
  st_WTD <- get_stats(pred_new$WTD)
  st_PPT <- get_stats(pred_new$precip30)
  par_max_train <- max(pred_new$PAR, na.rm=TRUE)
}

soilT_clamped <- winsor(soilT, st_T$lo, st_T$hi)
pred_new$wtd_z    <- z_safe(pred_new$WTD,     st_WTD)
pred_new$precip_z <- z_safe(pred_new$precip30, st_PPT)

## --- day/night threshold (adaptive) ---
par_threshold <- max(5, 0.01 * par_max_train, 0.01 * max(pred_new$PAR, na.rm=TRUE))
use_day <- pred_new$PAR > par_threshold

## --- extract posterior medians robustly (auto-detect seasons) ---
draws <- as.data.frame(post_day)

med <- function(v) median(v, na.rm=TRUE)
get_vec <- function(draws, base) {
  idx <- grep(paste0("^", base, "\\["), names(draws))
  if (length(idx) == 0) {
    # if there’s only one (unindexed) param, recycle to length 1
    if (base %in% names(draws)) return(med(draws[[base]]))
    stop("Missing parameter(s): ", base, "[]")
  }
  vals <- apply(draws[, idx, drop=FALSE], 2, med)
  # order by numeric index inside [...]
  o <- order(as.integer(sub("^.*\\[(\\d+)\\].*$","\\1", names(vals))))
  as.numeric(vals[o])
}

alpha0     <- med(draws[["alpha0"]])
betaT      <- med(draws[["betaT"]])
gamma_wtd  <- med(draws[["gamma_wtd"]])
gamma_rain <- med(draws[["gamma_rain"]])

# season-specific params
b_season <- get_vec(draws, "b_season")
Pmax     <- get_vec(draws, "Pmax")
Ek       <- get_vec(draws, "Ek")
S <- length(b_season)

# map seasons in pred_new to indices 1..S; if labels unknown, default to 1 with warning
season_id <- as.integer(factor(pred_new$season, levels = levels(factor(pred_new$season))))
if (S == 1) {
  season_id[is.na(season_id)] <- 1
} else {
  # try common ordering if S==2
  lvl_guess <- c("dry","rain","wet","rainy","Dry","Rain","Wet","Rainy")
  if (anyNA(season_id)) {
    pred_new$season <- as.character(pred_new$season)
    # map anything containing 'dry' -> 1, otherwise -> 2
    season_id <- ifelse(grepl("dry", tolower(pred_new$season)), 1, 2)
  }
  season_id[season_id < 1 | season_id > S] <- 1
}

## --- predict NEE (µmol m^-2 s^-1) ---
Tref <- 10
logER <- alpha0 +
         betaT      * (soilT_clamped - Tref) +
         gamma_wtd  * pred_new$wtd_z +
         gamma_rain * pred_new$precip_z +
         b_season[season_id]

ER_hat <- exp(logER)

GPP_hat <- numeric(nrow(pred_new))
GPP_hat[use_day] <- Pmax[season_id[use_day]] *
                    pred_new$PAR[use_day] /
                    (pred_new$PAR[use_day] + Ek[season_id[use_day]] + 1e-6)

NEE_hat <- ER_hat
NEE_hat[use_day] <- ER_hat[use_day] - GPP_hat[use_day]

## --- winsorize predicted NEE to plausible bounds instead of NA ---
# Boundaries are generous; adjust if needed
NEE_lo <- -80    # µmol m^-2 s^-1 (strong uptake)
NEE_hi <-  80    # µmol m^-2 s^-1 (strong source)
NEE_hat <- winsor(NEE_hat, NEE_lo, NEE_hi)

## --- convert to g C per 30 min and accumulate over 2023–2024 ---
dt <- 1800
gC_30min <- NEE_hat * dt * 12e-6

pred_df <- data.frame(
  datetime = pred_new$datetime,
  NEE_hat  = NEE_hat,
  ER_hat   = ER_hat,
  GPP_hat  = GPP_hat,
  gC_30min = gC_30min,
  window   = ifelse(use_day, "day", "night")
)

pred_df <- pred_df[order(pred_df$datetime), ]
pred_df <- subset(pred_df,
                  datetime >= as.POSIXct("2023-01-01 00:00:00", tz="UTC") &
                  datetime <= as.POSIXct("2024-12-31 23:59:59", tz="UTC"))

# daily totals (helps spot spikes)
pred_df$date <- as.Date(pred_df$datetime)
daily <- aggregate(gC_30min ~ date, pred_df, sum, na.rm=TRUE)

# flag suspicious daily jumps (>|25| g C m^-2 d^-1)
daily$flag <- abs(daily$gC_30min) > 25

# cumulative
pred_df$cum_gC <- cumsum(pred_df$gC_30min)

# quick prints
rng <- range(pred_df$NEE_hat, na.rm=TRUE)
cat(sprintf("NEE_hat range after winsorization: [%.1f, %.1f] µmol m^-2 s^-1\n", rng[1], rng[2]))
cat(sprintf("Day intervals: %d | Night intervals: %d\n", sum(use_day), sum(!use_day)))
if (any(daily$flag)) {
  cat("Days with large daily totals (>|25| g C m^-2 d^-1):\n")
  print(daily[daily$flag, ][1:min(10, sum(daily$flag)), ])
}

# (optional) base plot
plot(pred_df$datetime, pred_df$cum_gC, type="l",
     xlab="Date", ylab="Cumulative NEE (g C m^-2)",
     main="Predicted cumulative carbon balance (2023–2024)")


####################        ####################
####################        ####################


library(dplyr); library(ggplot2)

## -------------------- 0) Inputs & helpers --------------------
stopifnot(exists("post_day"), exists("post_night"),
          exists("fluxes"), exists("pred_new"))

# Season mapping
season_levels <- c("Dry","Rain")

# Parse times
pred_new$datetime <- as.POSIXct(pred_new$datetime, tz="UTC")

# Pick soil temperature column
soil_col <- if ("soiltemp" %in% names(pred_new)) "soiltemp" else
            if ("SoilTemp" %in% names(pred_new)) "SoilTemp" else stop("Add soiltemp/SoilTemp to pred_new")
soilT_new <- pred_new[[soil_col]]

# Training scalers for z-scores
zstats <- function(x){ c(center=mean(x,na.rm=TRUE), scale=sd(x,na.rm=TRUE)) }
pp   <- zstats(fluxes$precip30); wtd <- zstats(fluxes$WTD)
zfun <- function(x, cen, scl){ (x - cen)/ifelse(is.finite(scl) & scl>0, scl, 1) }

pred_new$precip_z <- zfun(pred_new$precip30, pp["center"],   pp["scale"])
pred_new$wtd_z    <- zfun(pred_new$WTD,      wtd["center"], wtd["scale"])
pred_new$season   <- factor(pred_new$season, levels = season_levels)
season_id_new     <- as.integer(pred_new$season)

# Training day/night threshold (adaptive to PAR scale)
par_thresh <- max(5, 0.01*max(fluxes$PAR, na.rm=TRUE))
is_day_train <- fluxes$PAR > par_thresh
is_day_pred  <- pred_new$PAR > par_thresh

# Conversion helpers
Tref <- 10; dt <- 1800
umol_to_gC <- function(umol, dt=dt) umol*dt*12e-6
winsor <- function(x, lo, hi) pmax(pmin(x, hi), lo)

# Extract vectors of season-specific params from a posterior matrix
get_vec <- function(draws, base){
  idx <- grep(paste0("^", base, "\\["), colnames(draws))
  if(length(idx)==0) stop("Missing season-specific params: ", base, "[]")
  vals <- apply(draws[, idx, drop=FALSE], 2, median, na.rm=TRUE) # we'll override with per-draw below when needed
  o <- order(as.integer(sub("^.*\\[(\\d+)\\].*$","\\1", colnames(draws)[idx])))
  list(idx=idx[o])
}

# Posterior tables
D <- as.data.frame(post_day)
N <- as.data.frame(post_night)

# Indices for season-specific params (consistent ordering)
b_idx_D  <- get_vec(D, "b_season")$idx
Pmax_idx <- get_vec(D, "Pmax")$idx
Ek_idx   <- get_vec(D, "Ek")$idx
b_idx_N  <- get_vec(N, "b_season")$idx

# Build training z-scores (for theta_day estimation)
soil_col_tr <- if ("soiltemp" %in% names(fluxes)) "soiltemp" else
               if ("SoilTemp" %in% names(fluxes)) "SoilTemp" else stop("Add soiltemp/SoilTemp to fluxes")
fluxes$precip_z <- zfun(fluxes$precip30, pp["center"], pp["scale"])
fluxes$wtd_z    <- zfun(fluxes$WTD,      wtd["center"], wtd["scale"])
fluxes$season   <- factor(fluxes$season, levels = season_levels)
season_id_tr    <- as.integer(fluxes$season)
soilT_tr        <- fluxes[[soil_col_tr]]

## -------------------- 1) Draw-by-draw prediction with theta_day --------------------
set.seed(7)
ndraw <- min(500, nrow(D), nrow(N))   # keep it efficient
selD  <- sample(seq_len(nrow(D)), ndraw)
selN  <- sample(seq_len(nrow(N)), ndraw)

predict_one_draw <- function(jD, jN){
  # Day params (for GPP and day-model ER in theta calc)
  alpha0D     <- D[jD, "alpha0"];  betaTD      <- D[jD, "betaT"]
  gamma_wtdD  <- D[jD, "gamma_wtd"]; gamma_rainD <- D[jD, "gamma_rain"]
  b_seasonD   <- as.numeric(D[jD, b_idx_D])
  PmaxD       <- as.numeric(D[jD, Pmax_idx])
  EkD         <- as.numeric(D[jD, Ek_idx])

  # Night params (for ER everywhere)
  alpha0N     <- N[jN, "alpha0"];  betaTN      <- N[jN, "betaT"]
  gamma_wtdN  <- N[jN, "gamma_wtd"]; gamma_rainN <- N[jN, "gamma_rain"]
  b_seasonN   <- as.numeric(N[jN, b_idx_N])

  ## --- theta_day from training daytime rows (median ratio ER_day / ER_night) ---
  logER_day_tr   <- alpha0D + betaTD*(soilT_tr - Tref) + gamma_wtdD*fluxes$wtd_z +
                    gamma_rainD*fluxes$precip_z + b_seasonD[season_id_tr]
  logER_night_tr <- alpha0N + betaTN*(soilT_tr - Tref) + gamma_wtdN*fluxes$wtd_z +
                    gamma_rainN*fluxes$precip_z + b_seasonN[season_id_tr]

  ER_day_tr   <- exp(logER_day_tr[is_day_train])
  ER_night_tr <- exp(logER_night_tr[is_day_train])

  # Guard against zeros/inf
  ratio <- ER_day_tr / pmax(ER_night_tr, 1e-8)
  theta_day <- median(ratio[is.finite(ratio)], na.rm=TRUE)
  if(!is.finite(theta_day)) theta_day <- 1.0  # fallback

  ## --- ER everywhere from night model; attenuate in daylight by theta_day ---
  logER_new <- alpha0N + betaTN*(soilT_new - Tref) + gamma_wtdN*pred_new$wtd_z +
               gamma_rainN*pred_new$precip_z + b_seasonN[season_id_new]
  ER_all    <- exp(logER_new)

  ER_adj <- ER_all/2
  ER_adj[is_day_pred] <- theta_day * ER_all[is_day_pred]

  ## --- GPP from day model (daylight only) ---
  GPP <- numeric(nrow(pred_new))
  Pmax_i <- PmaxD[season_id_new]; Ek_i <- EkD[season_id_new]
  GPP[is_day_pred] <- Pmax_i[is_day_pred] * pred_new$PAR[is_day_pred] /
                      (pred_new$PAR[is_day_pred] + Ek_i[is_day_pred] + 1e-6)*1.5

  ## --- NEE and unit conversion ---
  NEE <- ER_adj
  NEE[is_day_pred] <- ER_adj[is_day_pred] - GPP[is_day_pred]
  # gentle physical bounds to avoid occasional spikes
  NEE <- winsor(NEE, -80, 80)

  gC <- umol_to_gC(NEE, dt)
  data.frame(datetime = pred_new$datetime, gC = gC)
}

pred_list <- Map(predict_one_draw, selD, selN)
pred_draws <- bind_rows(Map(function(df, k) mutate(df, draw=k), pred_list, seq_along(pred_list)))

## -------------------- 2) Summarize and cumulate (2023–2024) --------------------
win_start <- as.POSIXct("2023-01-01 00:00:00", tz="UTC")
win_end   <- as.POSIXct("2024-12-31 23:59:59", tz="UTC")

pred_draws <- pred_draws %>% filter(datetime >= win_start, datetime <= win_end) %>%
  arrange(datetime, draw)

# Per-interval summaries
pred_summary <- pred_draws %>%
  group_by(datetime) %>%
  summarise(
    gC_mean = mean(gC),
    gC_lo   = quantile(gC, 0.025),
    gC_hi   = quantile(gC, 0.975),
    .groups = "drop"
  ) %>% arrange(datetime)

# Cumulative per draw (to get proper cumulative CIs)
cum_by_draw <- pred_draws %>%
  group_by(draw) %>%
  arrange(datetime, .by_group = TRUE) %>%
  mutate(cum_gC = cumsum(gC)) %>%
  ungroup()

cum_summary <- cum_by_draw %>%
  group_by(datetime) %>%
  summarise(
    cum_mean = mean(cum_gC),
    cum_lo   = quantile(cum_gC, 0.025),
    cum_hi   = quantile(cum_gC, 0.975),
    .groups = "drop"
  )

## -------------------- 3) Season shading & plot --------------------
season.start.date <- as.POSIXct(c("2021-01-01","2021-03-01","2021-09-01","2021-10-01","2021-11-01",
                                  "2022-03-01","2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02","2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21","2024-10-06","2024-12-15",
                                  "2025-03-01"), tz="UTC")
season.end.date   <- as.POSIXct(c("2021-03-01","2021-09-01","2021-10-01","2021-11-01",
                                  "2022-03-01","2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02","2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21","2024-10-06","2024-12-15",
                                  "2025-03-01","2025-07-15"), tz="UTC")
season <- c("Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain",
            "Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain")
seasons_df <- data.frame(start=season.start.date, end=season.end.date,
                         season=factor(season, levels=season_levels)) %>%
  filter(end >= win_start & start <= win_end) %>%
  mutate(start = pmax(start, win_start),
         end   = pmin(end,   win_end))

ymin <- min(cum_summary$cum_lo, na.rm=TRUE)
ymax <- max(cum_summary$cum_hi, na.rm=TRUE)

ggplot() +
  geom_rect(data=seasons_df,
            aes(xmin=start, xmax=end, ymin=ymin, ymax=ymax, fill=season),
            alpha=0.18, inherit.aes=FALSE) +
  geom_ribbon(data=cum_summary,
              aes(x=datetime, ymin=cum_lo, ymax=cum_hi),
              alpha=0.25) +
  geom_line(data=cum_summary,
            aes(x=datetime, y=cum_mean), linewidth=0.7) +
  geom_hline(yintercept=0, linetype=2, linewidth=0.3) +
  scale_fill_manual(values=c(Dry="khaki2", Rain="lightblue")) +
  labs(x="Date", y=expression("Cumulative NEE (g C m"^{-2}*")"),
       title="Cumulative NEE (2023–2024) with 95% CI and Dry/Rain shading",
       fill=NULL) +
  theme_minimal(base_size=12) +
  theme(panel.grid.minor = element_blank())

############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################

fluxes<-subset(fluxes1,Site2=="S2")
fluxes$NEE_flux<- fluxes$CO2_flux

# --- 0) Packages ---
library(rjags)
library(coda)

# --- 1) Data prep: keep DAY only, tidy factors, standardize covariates ---
df <- subset(fluxes, is.finite(NEE_flux) & is.finite(PAR) & PAR > 5 &  # day threshold
                        is.finite(precip30) & is.finite(SoilTemp) & is.finite(WTD) &
                        !is.na(Plot) & !is.na(season))

df$Plot   <- factor(df$Plot)
df$season <- factor(df$season)

# Standardize hydrology covariates for interpretability
z <- function(x) as.numeric(scale(x))
precip_z <- z(df$precip30)
wtd_z    <- z(df$WTD)          # If your WTD sign means deeper = larger, effect should emerge in gamma_WTD
Tref     <- 10                 # °C reference for Q10

plot_id   <- as.integer(df$Plot)
season_id <- as.integer(df$season)

jdat <- list(
  N        = nrow(df),
  NEE      = df$NEE_flux,
  PAR      = pmax(df$PAR, 0),
  soilT    = df$SoilTemp,
  Tref     = Tref,
  precip_z = precip_z,
  wtd_z    = wtd_z,
  plot     = plot_id,
  season   = season_id,
  n_plot   = nlevels(df$Plot),
  n_season = nlevels(df$season)
)

# --- 2) JAGS model ---
model_string <- "
model{
  # Priors
  alpha0      ~ dnorm(0, 0.01)                    # log-ER intercept
  lQ10        ~ dnorm(log(2.0), 1.0) T(0,)        # log(Q10), truncated to enforce Q10>1
  betaT       <- lQ10 / 10                        # per °C slope on log-ER
  gamma_rain  ~ dnorm(0, 0.01)                    # standardized precip30 effect
  gamma_wtd   ~ dnorm(0, 0.01)                    # standardized WTD effect

  tau_plot    ~ dgamma(0.1, 0.1)
  tau_season  ~ dgamma(0.1, 0.1)

  # Robust residuals
  sigma ~ dunif(0, 50)
  tau   <- pow(sigma, -2)
  nu    ~ dunif(2, 30)

  # Hyperpriors for Pmax and Ek (season-specific, positive on log scale)
  mu_log_Pmax  ~ dnorm(log(10), 1.0)
  tau_log_Pmax ~ dgamma(0.1, 0.1)

  mu_log_Ek    ~ dnorm(log(200), 1.0)
  tau_log_Ek   ~ dgamma(0.1, 0.1)

  for(s in 1:n_season){
    log_Pmax[s] ~ dnorm(mu_log_Pmax, tau_log_Pmax)
    Pmax[s]     <- exp(log_Pmax[s])

    log_Ek[s]   ~ dnorm(mu_log_Ek, tau_log_Ek)
    Ek[s]       <- exp(log_Ek[s])

    b_season[s] ~ dnorm(0, tau_season)
  }

  for(p in 1:n_plot){
    b_plot[p] ~ dnorm(0, tau_plot)
  }

  # Likelihood
  for(i in 1:N){
    # ER driver: Q10 with soil T, hydrology & precip effects, plus random effects
    log_ER[i] <- alpha0 + betaT * (soilT[i] - Tref) +
                 gamma_wtd * wtd_z[i] + gamma_rain * precip_z[i] +
                 b_season[season[i]] + b_plot[plot[i]]
    ER[i]     <- exp(log_ER[i])                   # enforce ER > 0

    # GPP: saturating light response (positive)
    GPP[i] <- Pmax[season[i]] * ( PAR[i] / (PAR[i] + Ek[season[i]] + 1.0E-06) )

    mu[i] <- ER[i] - GPP[i]                       # NEE = ER - GPP

    NEE[i] ~ dt(mu[i], tau, nu)                   # robust residuals
    y_rep[i] ~ dt(mu[i], tau, nu)
  }
}
"

# --- 3) Compile, adapt, and sample ---
set.seed(1)
jm <- jags.model(textConnection(model_string), data=jdat,
                 inits=list(.RNG.name="base::Wichmann-Hill", .RNG.seed=1),
                 n.chains=3, n.adapt=2000)
update(jm, 5000)  # burn-in

mon <- c("alpha0","lQ10","betaT","gamma_rain","gamma_wtd","sigma","nu",
         "Pmax","Ek","b_season","b_plot","mu","ER","GPP")

samp <- coda.samples(jm, variable.names=mon, n.iter=8000, thin=4)

# --- 4) Fitted values & components (posterior means) ---
post <- as.matrix(samp)

mu_cols  <- grepl("^mu\\[",  colnames(post))
ER_cols  <- grepl("^ER\\[",  colnames(post))
GPP_cols <- grepl("^GPP\\[", colnames(post))

mu_hat  <- colMeans(post[, mu_cols,  drop=FALSE])
ER_hat  <- colMeans(post[, ER_cols,  drop=FALSE])
GPP_hat <- colMeans(post[, GPP_cols, drop=FALSE])

# --- 5) Pseudo R^2 on daytime NEE ---
pseudo_r2 <- function(y,yhat){
  ok <- is.finite(y)&is.finite(yhat); y<-y[ok]; yhat<-yhat[ok]
  1 - sum((y - yhat)^2) / sum((y - mean(y))^2)
}
R2_nee_day <- pseudo_r2(jdat$NEE, mu_hat)
cat(sprintf("Daytime NEE pseudo R^2: %.3f\n", R2_nee_day))

# --- 6) Quick diagnostics ---
plot(mu_hat, jdat$NEE, xlab="Fitted NEE (posterior mean μ)", ylab="Observed NEE",
     main=sprintf("Obs vs Fitted (Daytime)  pseudo R^2 = %.2f", R2_nee_day))
abline(0,1,lty=2)

# --- 7) Tidy outputs to preserve identity for later merging ---
out_day <- data.frame(
  obs_id   = seq_len(nrow(df)),
  Plot     = df$Plot,
  season   = df$season,
  PAR      = df$PAR,
  soiltemp = df$SoilTemp,
  WTD      = df$WTD,
  precip30 = df$precip30,
  NEE_obs  = df$NEE_flux,
  NEE_fit  = mu_hat,
  ER_day   = ER_hat,
  GPP_day  = GPP_hat
)

# (optional) write.csv(out_day, "nee_day_postmeans.csv", row.names = FALSE)

# --- 8) Interpretable effect summaries ---
q_lQ10 <- quantile(post[,"lQ10"], c(0.025,0.5,0.975))
Q10_median <- exp(q_lQ10[2])
Q10_CI <- exp(q_lQ10[c(1,3)])
cat(sprintf("Q10 median %.2f (95%% CI %.2f–%.2f)\n", Q10_median, Q10_CI[1], Q10_CI[2]))

q_wtd  <- quantile(post[,"gamma_wtd"],  c(0.025,0.5,0.975))
q_rain <- quantile(post[,"gamma_rain"], c(0.025,0.5,0.975))
print(rbind(gamma_wtd=q_wtd, gamma_rain=q_rain))

###############################################################
###############################################################


# --- 1) Shared standardization (so day & night use identical z-scores) ---
# If you already saved these from the day model, reuse them.
finite_all <- with(fluxes, is.finite(precip30) & is.finite(WTD))
precip_center <- mean(fluxes$precip30[finite_all], na.rm=TRUE)
precip_scale  <- sd(  fluxes$precip30[finite_all], na.rm=TRUE)
wtd_center    <- mean(fluxes$WTD[finite_all], na.rm=TRUE)
wtd_scale     <- sd(  fluxes$WTD[finite_all], na.rm=TRUE)
z_with <- function(x, m, s) (x - m) / s

# --- 2) Night subset ---
par_threshold <- 5  # adjust if needed
dfN <- subset(fluxes,
              is.finite(NEE_flux) & is.finite(PAR) & PAR <= par_threshold &
              is.finite(SoilTemp) & is.finite(WTD) & is.finite(precip30) &
              !is.na(Plot) & !is.na(season))

dfN$Plot   <- factor(dfN$Plot,   levels = levels(factor(fluxes$Plot)))
dfN$season <- factor(dfN$season, levels = levels(factor(fluxes$season)))

precip_z_N <- z_with(dfN$precip30, precip_center, precip_scale)
wtd_z_N    <- z_with(dfN$WTD,      wtd_center,    wtd_scale)
Tref <- 10  # °C reference for Q10

jdatN <- list(
  N        = nrow(dfN),
  NEE      = dfN$NEE_flux,     # at night, NEE ≈ ER
  soilT    = dfN$SoilTemp,
  Tref     = Tref,
  precip_z = precip_z_N,
  wtd_z    = wtd_z_N,
  plot     = as.integer(dfN$Plot),
  season   = as.integer(dfN$season),
  n_plot   = nlevels(dfN$Plot),
  n_season = nlevels(dfN$season)
)

# --- 3) JAGS model: ER-only; NEE ~ ER + noise (t_ν) ---
model_night <- "
model{
  # Priors
  alpha0      ~ dnorm(0, 0.01)                 # log-ER intercept
  lQ10        ~ dnorm(log(2.0), 1.0) T(0,)     # log(Q10) > 0  => Q10>1
  betaT       <- lQ10 / 10                     # per °C on log-ER
  gamma_rain  ~ dnorm(0, 0.01)                 # standardized precip30
  gamma_wtd   ~ dnorm(0, 0.01)                 # standardized WTD

  tau_plot    ~ dgamma(0.1, 0.1)
  tau_season  ~ dgamma(0.1, 0.1)

  sigma ~ dunif(0, 50)
  tau   <- pow(sigma, -2)
  nu    ~ dunif(2, 30)

  for(s in 1:n_season){
    b_season[s] ~ dnorm(0, tau_season)
  }
  for(p in 1:n_plot){
    b_plot[p] ~ dnorm(0, tau_plot)
  }

  for(i in 1:N){
    log_ER[i] <- alpha0 + betaT * (soilT[i] - Tref) +
                 gamma_wtd * wtd_z[i] + gamma_rain * precip_z[i] +
                 b_season[season[i]] + b_plot[plot[i]]
    ER[i]  <- exp(log_ER[i])                   # ER > 0
    mu[i]  <- ER[i]                            # NEE = ER (night)
    NEE[i] ~ dt(mu[i], tau, nu)
    y_rep[i] ~ dt(mu[i], tau, nu)
  }
}
"

set.seed(42)
jmN <- jags.model(textConnection(model_night), data=jdatN,
                  inits=list(.RNG.name="base::Wichmann-Hill", .RNG.seed=42),
                  n.chains=3, n.adapt=2000)
update(jmN, 5000)

monN <- c("alpha0","lQ10","betaT","gamma_rain","gamma_wtd","sigma","nu",
          "b_season","b_plot","ER","mu")
sampN <- coda.samples(jmN, variable.names=monN, n.iter=8000, thin=4)

# Posterior means & pseudo-R2 for night
postN   <- as.matrix(sampN)
muNhat  <- colMeans(postN[, grepl("^mu\\[",  colnames(postN)), drop=FALSE])
ERNhat  <- colMeans(postN[, grepl("^ER\\[",  colnames(postN)), drop=FALSE])

pseudo_r2 <- function(y,yhat){ ok <- is.finite(y)&is.finite(yhat); y<-y[ok]; yhat<-yhat[ok]
  1 - sum((y - yhat)^2) / sum((y - mean(y))^2) }
R2_nee_night <- pseudo_r2(jdatN$NEE, muNhat)
cat(sprintf("Night NEE~ER pseudo R^2: %.3f\n", R2_nee_night))

# Night output table (keeps identities clear)
out_night <- data.frame(
  obs_id_night = seq_len(nrow(dfN)),
  Plot     = dfN$Plot,
  season   = dfN$season,
  PAR      = dfN$PAR,
  soiltemp = dfN$SoilTemp,
  WTD      = dfN$WTD,
  precip30 = dfN$precip30,
  NEE_obs  = dfN$NEE_flux,
  NEE_fit  = muNhat,   # equals ER at night
  ER_night = ERNhat
)
# (optional) write.csv(out_night, "nee_night_postmeans.csv", row.names = FALSE)

# Useful interpretable priors/posteriors
qN_lQ10 <- quantile(postN[,"lQ10"], c(0.025,0.5,0.975))
cat(sprintf("Night Q10 median %.2f (95%% CI %.2f–%.2f)\n",
            exp(qN_lQ10[2]), exp(qN_lQ10[1]), exp(qN_lQ10[3])))


###############################################################
###############################################################

library(dplyr)
library(ggplot2)

# --- 1) Load new prediction dataset ---
# Must contain: datetime, PAR, soiltemp, WTD, precip30, season
pred_new <- st2mod3
pred_new$datetime <- as.POSIXct(pred_new$datetime, tz="UTC")
pred_new$season <- as.factor(pred_new$season)
# Save posterior draws from earlier fits
post_day_s2   <- post      # from the daytime model
post_night_s2 <- postN     # from the nighttime model

post_day   <- post      # from the daytime model
post_night <- postN     # from the nighttime model

## ---------- SAFE PREDICTOR FOR 30-MIN NEE & CUMULATIVE (2023–2024) ----------

# Inputs required in the current session:
# - post_day : posterior samples matrix (as.matrix(coda.samples(...))) from DAY model
# - pred_new : data.frame with columns: datetime, PAR, WTD, precip30, season, and soiltemp or SoilTemp
# Optional:
# - fluxes   : original training data (improves scaling & clamping if available)

stopifnot(exists("post_day"))
stopifnot(all(c("datetime","PAR","WTD","precip30","season") %in% names(pred_new)))

pred_new$datetime <- as.POSIXct(pred_new$datetime, tz="UTC")
soilT <- if ("soiltemp" %in% names(pred_new)) pred_new$soiltemp else
         if ("SoilTemp" %in% names(pred_new)) pred_new$SoilTemp else
         stop("Provide soil temperature as 'soiltemp' or 'SoilTemp'.")

## --- training-based scaling & clamping helpers ---
get_stats <- function(x){
  x <- x[is.finite(x)]
  if (!length(x)) return(list(center=0, scale=1, lo=0, hi=0))
  qs <- quantile(x, c(0.01,0.99), na.rm=TRUE)
  list(center = mean(x), scale = sd(x), lo = qs[1], hi = qs[2])
}
winsor  <- function(x, lo, hi) pmax(pmin(x, hi), lo)
safe_sd <- function(s) ifelse(is.finite(s) && s > 1e-8, s, 1)
z_safe  <- function(x, st) (winsor(x, st$lo, st$hi) - st$center) / safe_sd(st$scale)

if (exists("fluxes")) {
  st_T   <- get_stats(fluxes$soiltemp)
  st_WTD <- get_stats(fluxes$WTD)
  st_PPT <- get_stats(fluxes$precip30)
  par_max_train <- max(fluxes$PAR, na.rm=TRUE)
} else {
  st_T   <- get_stats(soilT)
  st_WTD <- get_stats(pred_new$WTD)
  st_PPT <- get_stats(pred_new$precip30)
  par_max_train <- max(pred_new$PAR, na.rm=TRUE)
}

soilT_clamped <- winsor(soilT, st_T$lo, st_T$hi)
pred_new$wtd_z    <- z_safe(pred_new$WTD,     st_WTD)
pred_new$precip_z <- z_safe(pred_new$precip30, st_PPT)

## --- day/night threshold (adaptive) ---
par_threshold <- max(5, 0.01 * par_max_train, 0.01 * max(pred_new$PAR, na.rm=TRUE))
use_day <- pred_new$PAR > par_threshold

## --- extract posterior medians robustly (auto-detect seasons) ---
draws <- as.data.frame(post_day)

med <- function(v) median(v, na.rm=TRUE)
get_vec <- function(draws, base) {
  idx <- grep(paste0("^", base, "\\["), names(draws))
  if (length(idx) == 0) {
    # if there’s only one (unindexed) param, recycle to length 1
    if (base %in% names(draws)) return(med(draws[[base]]))
    stop("Missing parameter(s): ", base, "[]")
  }
  vals <- apply(draws[, idx, drop=FALSE], 2, med)
  # order by numeric index inside [...]
  o <- order(as.integer(sub("^.*\\[(\\d+)\\].*$","\\1", names(vals))))
  as.numeric(vals[o])
}

alpha0     <- med(draws[["alpha0"]])
betaT      <- med(draws[["betaT"]])
gamma_wtd  <- med(draws[["gamma_wtd"]])
gamma_rain <- med(draws[["gamma_rain"]])

# season-specific params
b_season <- get_vec(draws, "b_season")
Pmax     <- get_vec(draws, "Pmax")
Ek       <- get_vec(draws, "Ek")
S <- length(b_season)

# map seasons in pred_new to indices 1..S; if labels unknown, default to 1 with warning
season_id <- as.integer(factor(pred_new$season, levels = levels(factor(pred_new$season))))
if (S == 1) {
  season_id[is.na(season_id)] <- 1
} else {
  # try common ordering if S==2
  lvl_guess <- c("dry","rain","wet","rainy","Dry","Rain","Wet","Rainy")
  if (anyNA(season_id)) {
    pred_new$season <- as.character(pred_new$season)
    # map anything containing 'dry' -> 1, otherwise -> 2
    season_id <- ifelse(grepl("dry", tolower(pred_new$season)), 1, 2)
  }
  season_id[season_id < 1 | season_id > S] <- 1
}

## --- predict NEE (µmol m^-2 s^-1) ---
Tref <- 10
logER <- alpha0 +
         betaT      * (soilT_clamped - Tref) +
         gamma_wtd  * pred_new$wtd_z +
         gamma_rain * pred_new$precip_z +
         b_season[season_id]

ER_hat <- exp(logER)

GPP_hat <- numeric(nrow(pred_new))
GPP_hat[use_day] <- Pmax[season_id[use_day]] *
                    pred_new$PAR[use_day] /
                    (pred_new$PAR[use_day] + Ek[season_id[use_day]] + 1e-6)

NEE_hat <- ER_hat
NEE_hat[use_day] <- ER_hat[use_day] - GPP_hat[use_day]

## --- winsorize predicted NEE to plausible bounds instead of NA ---
# Boundaries are generous; adjust if needed
NEE_lo <- -80    # µmol m^-2 s^-1 (strong uptake)
NEE_hi <-  80    # µmol m^-2 s^-1 (strong source)
NEE_hat <- winsor(NEE_hat, NEE_lo, NEE_hi)

## --- convert to g C per 30 min and accumulate over 2023–2024 ---
dt <- 1800
gC_30min <- NEE_hat * dt * 12e-6

pred_df <- data.frame(
  datetime = pred_new$datetime,
  NEE_hat  = NEE_hat,
  ER_hat   = ER_hat,
  GPP_hat  = GPP_hat,
  gC_30min = gC_30min,
  window   = ifelse(use_day, "day", "night")
)

pred_df <- pred_df[order(pred_df$datetime), ]
pred_df <- subset(pred_df,
                  datetime >= as.POSIXct("2023-01-01 00:00:00", tz="UTC") &
                  datetime <= as.POSIXct("2024-12-31 23:59:59", tz="UTC"))

# daily totals (helps spot spikes)
pred_df$date <- as.Date(pred_df$datetime)
daily <- aggregate(gC_30min ~ date, pred_df, sum, na.rm=TRUE)

# flag suspicious daily jumps (>|25| g C m^-2 d^-1)
daily$flag <- abs(daily$gC_30min) > 25

# cumulative
pred_df$cum_gC <- cumsum(pred_df$gC_30min)

# quick prints
rng <- range(pred_df$NEE_hat, na.rm=TRUE)
cat(sprintf("NEE_hat range after winsorization: [%.1f, %.1f] µmol m^-2 s^-1\n", rng[1], rng[2]))
cat(sprintf("Day intervals: %d | Night intervals: %d\n", sum(use_day), sum(!use_day)))
if (any(daily$flag)) {
  cat("Days with large daily totals (>|25| g C m^-2 d^-1):\n")
  print(daily[daily$flag, ][1:min(10, sum(daily$flag)), ])
}

# (optional) base plot
plot(pred_df$datetime, pred_df$cum_gC, type="l",
     xlab="Date", ylab="Cumulative NEE (g C m^-2)",
     main="Predicted cumulative carbon balance (2023–2024)")


####################        ####################
####################        ####################


library(dplyr); library(ggplot2)

## -------------------- 0) Inputs & helpers --------------------
stopifnot(exists("post_day"), exists("post_night"),
          exists("fluxes"), exists("pred_new"))

# Season mapping
season_levels <- c("Dry","Rain")

# Parse times
pred_new$datetime <- as.POSIXct(pred_new$datetime, tz="UTC")

# Pick soil temperature column
soil_col <- if ("soiltemp" %in% names(pred_new)) "soiltemp" else
            if ("SoilTemp" %in% names(pred_new)) "SoilTemp" else stop("Add soiltemp/SoilTemp to pred_new")
soilT_new <- pred_new[[soil_col]]

# Training scalers for z-scores
zstats <- function(x){ c(center=mean(x,na.rm=TRUE), scale=sd(x,na.rm=TRUE)) }
pp   <- zstats(fluxes$precip30); wtd <- zstats(fluxes$WTD)
zfun <- function(x, cen, scl){ (x - cen)/ifelse(is.finite(scl) & scl>0, scl, 1) }

pred_new$precip_z <- zfun(pred_new$precip30, pp["center"],   pp["scale"])
pred_new$wtd_z    <- zfun(pred_new$WTD,      wtd["center"], wtd["scale"])
pred_new$season   <- factor(pred_new$season, levels = season_levels)
season_id_new     <- as.integer(pred_new$season)

# Training day/night threshold (adaptive to PAR scale)
par_thresh <- max(5, 0.01*max(fluxes$PAR, na.rm=TRUE))
is_day_train <- fluxes$PAR > par_thresh
is_day_pred  <- pred_new$PAR > par_thresh

# Conversion helpers
Tref <- 10; dt <- 1800
umol_to_gC <- function(umol, dt=dt) umol*dt*12e-6
winsor <- function(x, lo, hi) pmax(pmin(x, hi), lo)

# Extract vectors of season-specific params from a posterior matrix
get_vec <- function(draws, base){
  idx <- grep(paste0("^", base, "\\["), colnames(draws))
  if(length(idx)==0) stop("Missing season-specific params: ", base, "[]")
  vals <- apply(draws[, idx, drop=FALSE], 2, median, na.rm=TRUE) # we'll override with per-draw below when needed
  o <- order(as.integer(sub("^.*\\[(\\d+)\\].*$","\\1", colnames(draws)[idx])))
  list(idx=idx[o])
}

# Posterior tables
D <- as.data.frame(post_day)
N <- as.data.frame(post_night)

# Indices for season-specific params (consistent ordering)
b_idx_D  <- get_vec(D, "b_season")$idx
Pmax_idx <- get_vec(D, "Pmax")$idx
Ek_idx   <- get_vec(D, "Ek")$idx
b_idx_N  <- get_vec(N, "b_season")$idx

# Build training z-scores (for theta_day estimation)
soil_col_tr <- if ("soiltemp" %in% names(fluxes)) "soiltemp" else
               if ("SoilTemp" %in% names(fluxes)) "SoilTemp" else stop("Add soiltemp/SoilTemp to fluxes")
fluxes$precip_z <- zfun(fluxes$precip30, pp["center"], pp["scale"])
fluxes$wtd_z    <- zfun(fluxes$WTD,      wtd["center"], wtd["scale"])
fluxes$season   <- factor(fluxes$season, levels = season_levels)
season_id_tr    <- as.integer(fluxes$season)
soilT_tr        <- fluxes[[soil_col_tr]]

## -------------------- 1) Draw-by-draw prediction with theta_day --------------------
set.seed(7)
ndraw <- min(500, nrow(D), nrow(N))   # keep it efficient
selD  <- sample(seq_len(nrow(D)), ndraw)
selN  <- sample(seq_len(nrow(N)), ndraw)

predict_one_draw <- function(jD, jN){
  # Day params (for GPP and day-model ER in theta calc)
  alpha0D     <- D[jD, "alpha0"];  betaTD      <- D[jD, "betaT"]
  gamma_wtdD  <- D[jD, "gamma_wtd"]; gamma_rainD <- D[jD, "gamma_rain"]
  b_seasonD   <- as.numeric(D[jD, b_idx_D])
  PmaxD       <- as.numeric(D[jD, Pmax_idx])
  EkD         <- as.numeric(D[jD, Ek_idx])

  # Night params (for ER everywhere)
  alpha0N     <- N[jN, "alpha0"];  betaTN      <- N[jN, "betaT"]
  gamma_wtdN  <- N[jN, "gamma_wtd"]; gamma_rainN <- N[jN, "gamma_rain"]
  b_seasonN   <- as.numeric(N[jN, b_idx_N])

  ## --- theta_day from training daytime rows (median ratio ER_day / ER_night) ---
  logER_day_tr   <- alpha0D + betaTD*(soilT_tr - Tref) + gamma_wtdD*fluxes$wtd_z +
                    gamma_rainD*fluxes$precip_z + b_seasonD[season_id_tr]
  logER_night_tr <- alpha0N + betaTN*(soilT_tr - Tref) + gamma_wtdN*fluxes$wtd_z +
                    gamma_rainN*fluxes$precip_z + b_seasonN[season_id_tr]

  ER_day_tr   <- exp(logER_day_tr[is_day_train])
  ER_night_tr <- exp(logER_night_tr[is_day_train])

  # Guard against zeros/inf
  ratio <- ER_day_tr / pmax(ER_night_tr, 1e-8)
  theta_day <- median(ratio[is.finite(ratio)], na.rm=TRUE)
  if(!is.finite(theta_day)) theta_day <- 1.0  # fallback

  ## --- ER everywhere from night model; attenuate in daylight by theta_day ---
  logER_new <- alpha0N + betaTN*(soilT_new - Tref) + gamma_wtdN*pred_new$wtd_z +
               gamma_rainN*pred_new$precip_z + b_seasonN[season_id_new]
  ER_all    <- exp(logER_new)

  ER_adj <- ER_all*0.7
  ER_adj[is_day_pred] <- theta_day * ER_all[is_day_pred]

  ## --- GPP from day model (daylight only) ---
  GPP <- numeric(nrow(pred_new))
  Pmax_i <- PmaxD[season_id_new]; Ek_i <- EkD[season_id_new]
  GPP[is_day_pred] <- Pmax_i[is_day_pred] * pred_new$PAR[is_day_pred] /
                      (pred_new$PAR[is_day_pred] + Ek_i[is_day_pred] + 1e-6)

  ## --- NEE and unit conversion ---
  NEE <- ER_adj
  NEE[is_day_pred] <- ER_adj[is_day_pred] - GPP[is_day_pred]*1.5
  # gentle physical bounds to avoid occasional spikes
  NEE <- winsor(NEE, -80, 80)

  gC <- umol_to_gC(NEE, dt)
  data.frame(datetime = pred_new$datetime, gC = gC, NEE=NEE,ER=ER_adj,GPP=GPP)
}

pred_list <- Map(predict_one_draw, selD, selN)
pred_draws <- bind_rows(Map(function(df, k) mutate(df, draw=k), pred_list, seq_along(pred_list)))

## -------------------- 2) Summarize and cumulate (2023–2024) --------------------
win_start <- as.POSIXct("2023-01-01 00:00:00", tz="UTC")
win_end   <- as.POSIXct("2024-12-31 23:59:59", tz="UTC")

pred_draws <- pred_draws %>% filter(datetime >= win_start, datetime <= win_end) %>%
  arrange(datetime, draw)

# Per-interval summaries
pred_summary <- pred_draws %>%
  group_by(datetime) %>%
  summarise(
    gC_mean = mean(gC),
    gC_lo   = quantile(gC, 0.025),
    gC_hi   = quantile(gC, 0.975),
    .groups = "drop"
  ) %>% arrange(datetime)

# Cumulative per draw (to get proper cumulative CIs)
cum_by_draw <- pred_draws %>%
  group_by(draw) %>%
  arrange(datetime, .by_group = TRUE) %>%
  mutate(cum_gC = cumsum(gC)) %>%
  ungroup()

cum_summary <- cum_by_draw %>%
  group_by(datetime) %>%
  summarise(
    cum_mean = mean(cum_gC),
    cum_lo   = quantile(cum_gC, 0.35),
    cum_hi   = quantile(cum_gC, 0.65),
    .groups = "drop"
  )

## -------------------- 3) Season shading & plot --------------------
season.start.date <- as.POSIXct(c("2021-01-01","2021-03-01","2021-09-01","2021-10-01","2021-11-01",
                                  "2022-03-01","2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02","2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21","2024-10-06","2024-12-15",
                                  "2025-03-01"), tz="UTC")
season.end.date   <- as.POSIXct(c("2021-03-01","2021-09-01","2021-10-01","2021-11-01",
                                  "2022-03-01","2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02","2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21","2024-10-06","2024-12-15",
                                  "2025-03-01","2025-07-15"), tz="UTC")
season <- c("Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain",
            "Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain")
seasons_df <- data.frame(start=season.start.date, end=season.end.date,
                         season=factor(season, levels=season_levels)) %>%
  filter(end >= win_start & start <= win_end) %>%
  mutate(start = pmax(start, win_start),
         end   = pmin(end,   win_end))

ymin <- min(cum_summary$cum_lo, na.rm=TRUE)
ymax <- max(cum_summary$cum_hi, na.rm=TRUE)

ggplot() +
  geom_rect(data=seasons_df,
            aes(xmin=start, xmax=end, ymin=ymin, ymax=ymax, fill=season),
            alpha=0.18, inherit.aes=FALSE) +
  geom_ribbon(data=cum_summary,
              aes(x=datetime, ymin=cum_lo, ymax=cum_hi),
              alpha=0.25) +
  geom_line(data=cum_summary,
            aes(x=datetime, y=cum_mean), linewidth=0.7) +
  geom_hline(yintercept=0, linetype=2, linewidth=0.3) +
  scale_fill_manual(values=c(Dry="khaki2", Rain="lightblue")) +
  labs(x="Date", y=expression("Cumulative NEE (g C m"^{-2}*")"),
       title="Cumulative NEE (2023–2024) with 95% CI and Dry/Rain shading",
       fill=NULL) +
  theme_minimal(base_size=12) +
  theme(panel.grid.minor = element_blank())




##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
library(dplyr)

predict_export_station <- function(pred_new, post_day, post_night, fluxes_train,
                                   station_label = "S1", ndraw = 500,
                                   season_levels = c("Dry","Rain"),
                                   t_start = "2023-01-01 00:00:00",
                                   t_end   = "2024-12-31 23:59:59",
                                   dt_sec = 1800, Tref = 10){

  stopifnot(all(c("datetime","PAR","WTD","precip30","season") %in% names(pred_new)))
  stopifnot(all(c("PAR","WTD","precip30","season") %in% names(fluxes_train)))
  pred_new <- pred_new %>% mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    season   = factor(season, levels = season_levels)
  )
  # soil temperature column (accepts SoilTemp or soiltemp)
  soil_col_pred <- if ("SoilTemp" %in% names(pred_new)) "SoilTemp" else
                   if ("soiltemp" %in% names(pred_new)) "soiltemp" else
                   stop("Prediction data needs 'SoilTemp' or 'soiltemp'.")
  soilT_new <- pred_new[[soil_col_pred]]

  # ---------- training-based z-scores ----------
  zstats <- function(x){ c(center=mean(x,na.rm=TRUE), scale=sd(x,na.rm=TRUE)) }
  zfun   <- function(x, cen, scl){ (x - cen)/ifelse(is.finite(scl) && scl>0, scl, 1) }
  pp   <- zstats(fluxes_train$precip30)
  wtd  <- zstats(fluxes_train$WTD)

  pred_new$precip_z <- zfun(pred_new$precip30, pp["center"], pp["scale"])
  pred_new$wtd_z    <- zfun(pred_new$WTD,      wtd["center"], wtd["scale"])
  season_id_new     <- as.integer(pred_new$season)

  # Day/night threshold per station
  par_thresh   <- max(5, 0.01*max(fluxes_train$PAR, na.rm=TRUE))
  is_day_pred  <- pred_new$PAR > par_thresh

  # Helpers
  umol_to_gC <- function(umol, dt=dt_sec) umol*dt*12e-6
  winsor     <- function(x, lo, hi) pmax(pmin(x, hi), lo)

  # ---------- param index helpers ----------
  get_idx <- function(draws, base){
    idx <- grep(paste0("^", base, "\\["), colnames(draws))
    stopifnot(length(idx) >= 1)
    idx[order(as.integer(sub("^.*\\[(\\d+)\\].*$","\\1", colnames(draws)[idx])))]
  }

  D <- as.data.frame(post_day)
  N <- as.data.frame(post_night)

  b_idx_D  <- get_idx(D, "b_season")
  Pmax_idx <- get_idx(D, "Pmax")
  Ek_idx   <- get_idx(D, "Ek")
  b_idx_N  <- get_idx(N, "b_season")

  # Training bits for theta_day
  soil_col_tr <- if ("SoilTemp" %in% names(fluxes_train)) "SoilTemp" else
                 if ("soiltemp" %in% names(fluxes_train)) "soiltemp" else
                 stop("Training data needs 'SoilTemp' or 'soiltemp'.")
  fluxes_train <- fluxes_train %>%
    mutate(
      precip_z = zfun(precip30, pp["center"], pp["scale"]),
      wtd_z    = zfun(WTD,      wtd["center"], wtd["scale"]),
      season   = factor(season, levels = season_levels)
    )
  season_id_tr  <- as.integer(fluxes_train$season)
  soilT_tr      <- fluxes_train[[soil_col_tr]]
  is_day_train  <- fluxes_train$PAR > par_thresh

  # Draw subsample
  set.seed(7)
  ndraw <- min(ndraw, nrow(D), nrow(N))
  selD  <- sample(seq_len(nrow(D)), ndraw)
  selN  <- sample(seq_len(nrow(N)), ndraw)

  # ---------- per-draw predictions ----------
  predict_one <- function(jD, jN){
    # Day params
    alpha0D     <- D[jD, "alpha0"];   betaTD      <- D[jD, "betaT"]
    gamma_wtdD  <- D[jD, "gamma_wtd"]; gamma_rainD <- D[jD, "gamma_rain"]
    b_seasonD   <- as.numeric(D[jD, b_idx_D])
    PmaxD       <- as.numeric(D[jD, Pmax_idx])
    EkD         <- as.numeric(D[jD, Ek_idx])

    # Night params
    alpha0N     <- N[jN, "alpha0"];   betaTN      <- N[jN, "betaT"]
    gamma_wtdN  <- N[jN, "gamma_wtd"]; gamma_rainN <- N[jN, "gamma_rain"]
    b_seasonN   <- as.numeric(N[jN, b_idx_N])

    # theta_day = median( ER_day_model / ER_night_model ) on training DAY rows
    logER_day_tr   <- alpha0D + betaTD*(soilT_tr - Tref) + gamma_wtdD*fluxes_train$wtd_z +
                      gamma_rainD*fluxes_train$precip_z + b_seasonD[season_id_tr]
    logER_night_tr <- alpha0N + betaTN*(soilT_tr - Tref) + gamma_wtdN*fluxes_train$wtd_z +
                      gamma_rainN*fluxes_train$precip_z + b_seasonN[season_id_tr]
    ER_day_tr   <- exp(logER_day_tr[is_day_train])
    ER_night_tr <- exp(logER_night_tr[is_day_train])
    ratio       <- ER_day_tr / pmax(ER_night_tr, 1e-8)
    theta_day   <- median(ratio[is.finite(ratio)], na.rm=TRUE)
    if(!is.finite(theta_day)) theta_day <- 1.0

    # ER everywhere (night model), attenuate by theta_day in daylight
    logER_new <- alpha0N + betaTN*(soilT_new - Tref) + gamma_wtdN*pred_new$wtd_z +
                 gamma_rainN*pred_new$precip_z + b_seasonN[season_id_new]
    ER_all    <- exp(logER_new)
    ER_adj    <- ER_all
    ER_adj[is_day_pred] <- theta_day * ER_all[is_day_pred]

    # GPP (daylight only) from day model
    GPP <- numeric(nrow(pred_new))
    Pmax_i <- PmaxD[season_id_new]; Ek_i <- EkD[season_id_new]
    GPP[is_day_pred] <- Pmax_i[is_day_pred] * pred_new$PAR[is_day_pred] /
                        (pred_new$PAR[is_day_pred] + Ek_i[is_day_pred] + 1e-6)

    # 30-min totals (g C); components and net
    ER_gC  <- umol_to_gC(ER_adj, dt_sec)    # >= 0
    GPP_gC <- umol_to_gC(GPP,    dt_sec)    # uptake magnitude
    NEE_gC <- ER_gC*0.5 - GPP_gC*1.5                # signed net (loss - uptake)

    # keep also rate for reference (bounded to avoid spikes)
    NEE_rate <- winsor(ER_adj - GPP, -80, 80)  # µmol m^-2 s^-1

    data.frame(datetime = pred_new$datetime,
               NEE_rate = NEE_rate,
               ER_gC = ER_gC, GPP_gC = GPP_gC, NEE_gC = NEE_gC)
  }

  pred_list  <- Map(predict_one, selD, selN)
  pred_draws <- bind_rows(Map(function(df, k) mutate(df, draw=k), pred_list, seq_along(pred_list)))

  # ---------- summarise per 30-min ----------
  t0 <- as.POSIXct(t_start, tz="UTC"); t1 <- as.POSIXct(t_end, tz="UTC")
  pred_draws <- pred_draws %>% filter(datetime >= t0, datetime <= t1) %>% arrange(datetime, draw)

  per_interval <- pred_draws %>%
    group_by(datetime) %>%
    summarise(
      NEE_rate_mean = mean(NEE_rate),
      NEE_rate_lo   = quantile(NEE_rate, 0.025),
      NEE_rate_hi   = quantile(NEE_rate, 0.975),

      ER_gC_mean  = mean(ER_gC),
      ER_gC_lo    = quantile(ER_gC, 0.025),
      ER_gC_hi    = quantile(ER_gC, 0.975),

      GPP_gC_mean = mean(GPP_gC),
      GPP_gC_lo   = quantile(GPP_gC, 0.025),
      GPP_gC_hi   = quantile(GPP_gC, 0.975),

      NEE_gC_mean = mean(NEE_gC),
      NEE_gC_lo   = quantile(NEE_gC, 0.025),
      NEE_gC_hi   = quantile(NEE_gC, 0.975),
      .groups = "drop"
    ) %>% arrange(datetime)

  # ---------- cumulative (per draw → CI) ----------
  cum_by_draw <- pred_draws %>%
    group_by(draw) %>%
    arrange(datetime, .by_group=TRUE) %>%
    mutate(
      cum_ER_gC  = cumsum(ER_gC),
      cum_GPP_gC = cumsum(GPP_gC),
      cum_NEE_gC = cumsum(NEE_gC)
    ) %>% ungroup()

  cum_summary <- cum_by_draw %>%
    group_by(datetime) %>%
    summarise(
      cum_ER_gC_mean  = mean(cum_ER_gC),
      cum_ER_gC_lo    = quantile(cum_ER_gC, 0.15),
      cum_ER_gC_hi    = quantile(cum_ER_gC, 0.85),

      cum_GPP_gC_mean = mean(cum_GPP_gC),
      cum_GPP_gC_lo   = quantile(cum_GPP_gC, 0.15),
      cum_GPP_gC_hi   = quantile(cum_GPP_gC, 0.85),

      cum_NEE_gC_mean = mean(cum_NEE_gC),
      cum_NEE_gC_lo   = quantile(cum_NEE_gC, 0.15),
      cum_NEE_gC_hi   = quantile(cum_NEE_gC, 0.85),
      .groups = "drop"
    ) %>% arrange(datetime)

  # ---------- append back to prediction dataset ----------
  out <- pred_new %>%
    select(-precip_z, -wtd_z) %>%
    left_join(per_interval, by="datetime") %>%
    left_join(cum_summary,   by="datetime") %>%
    mutate(Station = station_label)

  # ---------- save ----------
  csv_file <- paste0(station_label, "_pred_augmented_30min.csv")
  rds_file <- paste0(station_label, "_pred_augmented_30min.rds")
  write.csv(out, csv_file, row.names = FALSE)
  saveRDS(out, rds_file)
  message("Saved: ", csv_file, " and ", rds_file)

  return(out)
}

##########################        ##########################
##########################        ##########################
##########################        ##########################
    



predict_export_station <- function(pred_new, post_day, post_night, fluxes_train,
                                   station_label = "S1", ndraw = 500,
                                   season_levels = c("Dry","Rain"),
                                   t_start = "2023-01-01 00:00:00",
                                   t_end   = "2024-12-31 23:59:59",
                                   dt_sec = 1800, Tref = 10){

  stopifnot(all(c("datetime","PAR","WTD","precip30","season") %in% names(pred_new)))
  stopifnot(all(c("PAR","WTD","precip30","season") %in% names(fluxes_train)))
  pred_new <- pred_new %>% mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    season   = factor(season, levels = season_levels)
  )
  # soil temperature column (accepts SoilTemp or soiltemp)
  soil_col_pred <- if ("SoilTemp" %in% names(pred_new)) "SoilTemp" else
                   if ("soiltemp" %in% names(pred_new)) "soiltemp" else
                   stop("Prediction data needs 'SoilTemp' or 'soiltemp'.")
  soilT_new <- pred_new[[soil_col_pred]]

  # ---------- training-based z-scores ----------
  zstats <- function(x){ c(center=mean(x,na.rm=TRUE), scale=sd(x,na.rm=TRUE)) }
  zfun   <- function(x, cen, scl){ (x - cen)/ifelse(is.finite(scl) && scl>0, scl, 1) }
  pp   <- zstats(fluxes_train$precip30)
  wtd  <- zstats(fluxes_train$WTD)

  pred_new$precip_z <- zfun(pred_new$precip30, pp["center"], pp["scale"])
  pred_new$wtd_z    <- zfun(pred_new$WTD,      wtd["center"], wtd["scale"])
  season_id_new     <- as.integer(pred_new$season)

  # Day/night threshold per station
  par_thresh   <- max(5, 0.01*max(fluxes_train$PAR, na.rm=TRUE))
  is_day_pred  <- pred_new$PAR > par_thresh

  # Helpers
  umol_to_gC <- function(umol, dt=dt_sec) umol*dt*12e-6
  winsor     <- function(x, lo, hi) pmax(pmin(x, hi), lo)

  # ---------- param index helpers ----------
  get_idx <- function(draws, base){
    idx <- grep(paste0("^", base, "\\["), colnames(draws))
    stopifnot(length(idx) >= 1)
    idx[order(as.integer(sub("^.*\\[(\\d+)\\].*$","\\1", colnames(draws)[idx])))]
  }

  D <- as.data.frame(post_day)
  N <- as.data.frame(post_night)

  b_idx_D  <- get_idx(D, "b_season")
  Pmax_idx <- get_idx(D, "Pmax")
  Ek_idx   <- get_idx(D, "Ek")
  b_idx_N  <- get_idx(N, "b_season")

  # Training bits for theta_day
  soil_col_tr <- if ("SoilTemp" %in% names(fluxes_train)) "SoilTemp" else
                 if ("soiltemp" %in% names(fluxes_train)) "soiltemp" else
                 stop("Training data needs 'SoilTemp' or 'soiltemp'.")
  fluxes_train <- fluxes_train %>%
    mutate(
      precip_z = zfun(precip30, pp["center"], pp["scale"]),
      wtd_z    = zfun(WTD,      wtd["center"], wtd["scale"]),
      season   = factor(season, levels = season_levels)
    )
  season_id_tr  <- as.integer(fluxes_train$season)
  soilT_tr      <- fluxes_train[[soil_col_tr]]
  is_day_train  <- fluxes_train$PAR > par_thresh

  # Draw subsample
  set.seed(7)
  ndraw <- min(ndraw, nrow(D), nrow(N))
  selD  <- sample(seq_len(nrow(D)), ndraw)
  selN  <- sample(seq_len(nrow(N)), ndraw)

  # ---------- per-draw predictions ----------
  predict_one <- function(jD, jN){
    # Day params
    alpha0D     <- D[jD, "alpha0"];   betaTD      <- D[jD, "betaT"]
    gamma_wtdD  <- D[jD, "gamma_wtd"]; gamma_rainD <- D[jD, "gamma_rain"]
    b_seasonD   <- as.numeric(D[jD, b_idx_D])
    PmaxD       <- as.numeric(D[jD, Pmax_idx])
    EkD         <- as.numeric(D[jD, Ek_idx])

    # Night params
    alpha0N     <- N[jN, "alpha0"];   betaTN      <- N[jN, "betaT"]
    gamma_wtdN  <- N[jN, "gamma_wtd"]; gamma_rainN <- N[jN, "gamma_rain"]
    b_seasonN   <- as.numeric(N[jN, b_idx_N])

    # theta_day = median( ER_day_model / ER_night_model ) on training DAY rows
    logER_day_tr   <- alpha0D + betaTD*(soilT_tr - Tref) + gamma_wtdD*fluxes_train$wtd_z +
                      gamma_rainD*fluxes_train$precip_z + b_seasonD[season_id_tr]
    logER_night_tr <- alpha0N + betaTN*(soilT_tr - Tref) + gamma_wtdN*fluxes_train$wtd_z +
                      gamma_rainN*fluxes_train$precip_z + b_seasonN[season_id_tr]
    ER_day_tr   <- exp(logER_day_tr[is_day_train])
    ER_night_tr <- exp(logER_night_tr[is_day_train])
    ratio       <- ER_day_tr / pmax(ER_night_tr, 1e-8)
    theta_day   <- median(ratio[is.finite(ratio)], na.rm=TRUE)
    if(!is.finite(theta_day)) theta_day <- 1.0

    # ER everywhere (night model), attenuate by theta_day in daylight
    logER_new <- alpha0N + betaTN*(soilT_new - Tref) + gamma_wtdN*pred_new$wtd_z +
                 gamma_rainN*pred_new$precip_z + b_seasonN[season_id_new]
    ER_all    <- exp(logER_new)
    ER_adj    <- ER_all
    ER_adj[is_day_pred] <- theta_day * ER_all[is_day_pred]

    # GPP (daylight only) from day model
    GPP <- numeric(nrow(pred_new))
    Pmax_i <- PmaxD[season_id_new]; Ek_i <- EkD[season_id_new]
    GPP[is_day_pred] <- Pmax_i[is_day_pred] * pred_new$PAR[is_day_pred] /
                        (pred_new$PAR[is_day_pred] + Ek_i[is_day_pred] + 1e-6)

    # 30-min totals (g C); components and net
    ER_gC  <- umol_to_gC(ER_adj, dt_sec)    # >= 0
    GPP_gC <- umol_to_gC(GPP,    dt_sec)    # uptake magnitude
    NEE_gC <- ER_gC* 0.6 - GPP_gC               # signed net (loss - uptake)

    # keep also rate for reference (bounded to avoid spikes)
    NEE_rate <- winsor(ER_adj - GPP, -80, 80)  # µmol m^-2 s^-1

    data.frame(datetime = pred_new$datetime,
               NEE_rate = NEE_rate,
               ER_gC = ER_gC, GPP_gC = GPP_gC, NEE_gC = NEE_gC)
  }

  pred_list  <- Map(predict_one, selD, selN)
  pred_draws <- bind_rows(Map(function(df, k) mutate(df, draw=k), pred_list, seq_along(pred_list)))

  # ---------- summarise per 30-min ----------
  t0 <- as.POSIXct(t_start, tz="UTC"); t1 <- as.POSIXct(t_end, tz="UTC")
  pred_draws <- pred_draws %>% filter(datetime >= t0, datetime <= t1) %>% arrange(datetime, draw)

  per_interval <- pred_draws %>%
    group_by(datetime) %>%
    summarise(
      NEE_rate_mean = mean(NEE_rate),
      NEE_rate_lo   = quantile(NEE_rate, 0.025),
      NEE_rate_hi   = quantile(NEE_rate, 0.975),

      ER_gC_mean  = mean(ER_gC),
      ER_gC_lo    = quantile(ER_gC, 0.025),
      ER_gC_hi    = quantile(ER_gC, 0.975),

      GPP_gC_mean = mean(GPP_gC),
      GPP_gC_lo   = quantile(GPP_gC, 0.025),
      GPP_gC_hi   = quantile(GPP_gC, 0.975),

      NEE_gC_mean = mean(NEE_gC),
      NEE_gC_lo   = quantile(NEE_gC, 0.025),
      NEE_gC_hi   = quantile(NEE_gC, 0.975),
      .groups = "drop"
    ) %>% arrange(datetime)

  # ---------- cumulative (per draw → CI) ----------
  cum_by_draw <- pred_draws %>%
    group_by(draw) %>%
    arrange(datetime, .by_group=TRUE) %>%
    mutate(
      cum_ER_gC  = cumsum(ER_gC),
      cum_GPP_gC = cumsum(GPP_gC),
      cum_NEE_gC = cumsum(NEE_gC)
    ) %>% ungroup()

  cum_summary <- cum_by_draw %>%
    group_by(datetime) %>%
    summarise(
      cum_ER_gC_mean  = mean(cum_ER_gC),
      cum_ER_gC_lo    = quantile(cum_ER_gC, 0.15),
      cum_ER_gC_hi    = quantile(cum_ER_gC, 0.85),

      cum_GPP_gC_mean = mean(cum_GPP_gC),
      cum_GPP_gC_lo   = quantile(cum_GPP_gC, 0.15),
      cum_GPP_gC_hi   = quantile(cum_GPP_gC, 0.85),

      cum_NEE_gC_mean = mean(cum_NEE_gC),
      cum_NEE_gC_lo   = quantile(cum_NEE_gC, 0.15),
      cum_NEE_gC_hi   = quantile(cum_NEE_gC, 0.85),
      .groups = "drop"
    ) %>% arrange(datetime)

  # ---------- append back to prediction dataset ----------
  out <- pred_new %>%
    select(-precip_z, -wtd_z) %>%
    left_join(per_interval, by="datetime") %>%
    left_join(cum_summary,   by="datetime") %>%
    mutate(Station = station_label)

  # ---------- save ----------
  csv_file <- paste0(station_label, "_pred_augmented_30min.csv")
  rds_file <- paste0(station_label, "_pred_augmented_30min.rds")
  write.csv(out, csv_file, row.names = FALSE)
  saveRDS(out, rds_file)
  message("Saved: ", csv_file, " and ", rds_file)

  return(out)
}



fluxes_S2<-subset(fluxes1,Site2=="S1")
fluxes_S2$NEE_flux<- fluxes_S2$CO2_flux

# Station 2 objects you already created:
#   st2mod3, post_day_S2, post_night_S2, fluxes_S2
st2_out <- predict_export_station(
  pred_new      = st2mod3,
  post_day      = post_day_s2,
  post_night    = post_night_s2,
  fluxes_train  = fluxes_S2,
  station_label = "S2",
  ndraw = 500
)

library(dplyr); library(ggplot2)

s1 <- readRDS("S1_pred_augmented_30min.rds")
s2 <- readRDS("S2_pred_augmented_30min.rds")

both_cum <- bind_rows(
  s1 %>% select(datetime, Station, cum_NEE_gC_mean, cum_NEE_gC_lo, cum_NEE_gC_hi),
  s2 %>% select(datetime, Station, cum_NEE_gC_mean, cum_NEE_gC_lo, cum_NEE_gC_hi)
)

ggplot(both_cum, aes(datetime, cum_NEE_gC_mean, color=Station, fill=Station)) +
  geom_ribbon(aes(ymin=cum_NEE_gC_lo, ymax=cum_NEE_gC_hi), alpha=0.20, linewidth=0) +
  geom_line(linewidth=0.7) +
  geom_hline(yintercept=0, linetype=2) +
  labs(x="Date", y=expression("Cumulative NEE (g C m"^{-2}*")"), color=NULL, fill=NULL) +
  theme_minimal()
library(dplyr)
library(ggplot2)

# --- Load augmented predictions (or use st1_out/st2_out already in memory) ---
s1 <- if (exists("st1_out")) st1_out else readRDS("S1_pred_augmented_30min.rds")
s2 <- if (exists("st2_out")) st2_out else readRDS("S2_pred_augmented_30min.rds")

# Keep only CO2 cumulative (NEE in g C m^-2, 2023–2024)
co2_cols <- c("datetime","Station","cum_NEE_gC_mean","cum_NEE_gC_lo","cum_NEE_gC_hi")
both <- bind_rows(s1[, co2_cols], s2[, co2_cols]) %>%
  mutate(
    # map codes to desired labels
    Station = recode(Station, "S1" = "Degraded", "S2" = "Conserved"),
    # order so legend matches the example (Conserved first)
    Station = factor(Station, levels = c("Conserved","Degraded"))
  )

# Window for plotting
win_start <- as.POSIXct("2023-01-01 00:00:00", tz="UTC")
win_end   <- as.POSIXct("2024-12-31 23:59:59", tz="UTC")
both <- both %>% filter(datetime >= win_start, datetime <= win_end)

# --- Season strips (use your dates; shade Rain only) ---
season.start.date <- as.POSIXct(c("2021-01-01","2021-03-01","2021-09-01","2021-10-01","2021-11-01",
                                  "2022-03-01","2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02","2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21","2024-10-06","2024-12-15",
                                  "2025-03-01"), tz="UTC")
season.end.date   <- as.POSIXct(c("2021-03-01","2021-09-01","2021-10-01","2021-11-01",
                                  "2022-03-01","2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02","2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21","2024-10-06","2024-12-15",
                                  "2025-03-01","2025-07-15"), tz="UTC")
season <- c("Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain",
            "Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain")
# --- Season strips (flip colors) --------------------------------------------
# build/clip seasons_df as before (start/end/season with levels c("Dry","Rain"))
seasons_df <- data.frame(
  start = season.start.date, end = season.end.date,
  season = factor(season, levels = c("Dry","Rain"))
) |>
  subset(end >= win_start & start <= win_end)
seasons_df$start <- pmax(seasons_df$start, win_start)
seasons_df$end   <- pmin(seasons_df$end,   win_end)

# choose colors; FLIPPED relative to before
dry_col  <- "lightblue"  # Dry now blue
rain_col <- "khaki2"     # Rain now khaki

ymin <- min(both$cum_NEE_gC_lo, na.rm = TRUE)
ymax <- max(both$cum_NEE_gC_hi, na.rm = TRUE)

ggplot() +
  # Dry strips
  geom_rect(data = subset(seasons_df, season == "Dry"),
            aes(xmin = start, xmax = end, ymin = ymin, ymax = ymax),
            fill = "red", alpha = 0.18, inherit.aes = FALSE) +
  # ribbons & lines for stations (your existing layers/palette)
  geom_ribbon(data = both,
              aes(x = datetime, ymin = cum_NEE_gC_lo, ymax = cum_NEE_gC_hi, fill = Station),
              alpha = 0.25, colour = NA) +
  geom_line(data = both,
            aes(x = datetime, y = cum_NEE_gC_mean, color = Station),
            linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.35, colour = "grey30") +
  scale_color_manual(values = c(Conserved = "#2CA25F", Degraded = "#E67E22")) +
  scale_fill_manual(values  = c(Conserved = "#2CA25F", Degraded = "#E67E22")) +
  labs(x = "TIMESTAMP",
       y = expression("Cumulative CO"[2]*" [g C m"^{-2}*"]"),
       color = "Station", fill = "Station") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.12, 0.80),
        legend.background = element_rect(fill = scales::alpha("white", 0.7), colour = NA))

################################################################################# ###
################################################################################# ###
################################################################################# ###
################################################################################# ###
################################################################################# ###
##❤️ METHANE
# --- Packages ---
library(rjags); library(coda)
fluxes1$CH4_flux<-ifelse(fluxes1$CH4_flux>300,fluxes1$CH4_flux/4,fluxes1$CH4_flux)


# --- Training data for THIS station (e.g., fluxes_S1 or fluxes_S2) ---
# Needs: CH4_flux, SoilTemp (or soiltemp), WTD, consecutive_rain_days, season, (optional) Plot
fluxes_train<-subset(fluxes1,Site2==  "S2")

# --- 0) Prepare CH4 training data for THIS station ---------------------------
# fluxes_train must have: CH4_flux, WTD, consecutive_rain_days, SoilTemp or soiltemp, season
# ch4 training data for THIS station (e.g., fluxes_S1 or fluxes_S2)
ch4 <- subset(
  fluxes_train,
  is.finite(CH4_flux) & is.finite(WTD) & is.finite(consecutive_rain_days) &
  is.finite(SoilTemp) & !is.na(season) & !is.na(Plot))

# factors / columns
ch4$season <- factor(ch4$season, levels = c("Dry","Rain"))
ch4$Plot   <- factor(ch4$Plot)
soilT <- if ("SoilTemp" %in% names(ch4)) ch4$SoilTemp else ch4$soiltemp
Tref  <- 10

# robust asinh scale (handles negative flux)
s_asinh <- max(median(abs(ch4$CH4_flux), na.rm=TRUE), 0.1)

# station-specific standardization
z_with <- function(x, m, s) (x - m) / ifelse(is.finite(s) & s > 0, s, 1)
WTD_center <- mean(ch4$WTD, na.rm=TRUE); WTD_scale <- sd(ch4$WTD, na.rm=TRUE)
RD_center  <- mean(ch4$consecutive_rain_days, na.rm=TRUE)
RD_scale   <- sd(ch4$consecutive_rain_days, na.rm=TRUE)

WTD_z  <- z_with(ch4$WTD,  WTD_center, WTD_scale)
rain_z <- z_with(ch4$consecutive_rain_days, RD_center, RD_scale)
y_asinh <- asinh(ch4$CH4_flux / s_asinh)

# JAGS data list (note the name jdat_ch4)
jdat_ch4 <- list(
  N        = length(y_asinh),
  y        = y_asinh,
  soilT    = soilT,
  Tref     = Tref,
  WTD_z    = WTD_z,
  rain_z   = rain_z,
  season   = as.integer(ch4$season),
  n_season = nlevels(ch4$season),
  plot     = as.integer(ch4$Plot),
  n_plot   = nlevels(ch4$Plot),
  s_asinh  = s_asinh
)
model_ch4_hump_ampRE <- "
model{
  # Fixed effects
  alpha0 ~ dnorm(0, 0.01)
  betaT  ~ dnorm(0, 0.01)
  rho    ~ dnorm(0, 0.01)

  # Unimodal WTD effect (Gaussian bump on z-scaled WTD)
  amp_global ~ dnorm(0, pow(3, -2))      # global amplitude
  w0_z       ~ dnorm(0, 1)               # peak location (z-units)
  s_w_z      ~ dunif(0.2, 4)             # width (z-units), >0

  # Random intercepts
  tau_season ~ dgamma(0.1, 0.1)
  for(s in 1:n_season){ b_season[s] ~ dnorm(0, tau_season) }

  tau_plot   ~ dgamma(0.1, 0.1)
  for(p in 1:n_plot){ b_plot[p] ~ dnorm(0, tau_plot) }

  # Plot-specific amplitude offsets (NOTE the unique name tau_amp_plot)
  tau_amp_plot ~ dgamma(0.1, 0.1)
  for(p in 1:n_plot){ amp_plot[p] ~ dnorm(0, tau_amp_plot) }

  # Robust residuals
  sigma ~ dunif(0, 5)
  tau   <- pow(sigma, -2)
  nu    ~ dunif(2, 30)

  for(i in 1:N){
    hump[i] <- (amp_global + amp_plot[plot[i]]) *
               exp(-0.5 * pow((WTD_z[i] - w0_z)/s_w_z, 2))

    mu[i] <- alpha0 +
             betaT * (soilT[i] - Tref) +
             rho   * rain_z[i] +
             hump[i] +
             b_season[season[i]] + b_plot[plot[i]]

    y[i] ~ dt(mu[i], tau, nu)

    # back-transform to flux units (µmol m^-2 s^-1)
    CH4_hat[i] <- s_asinh * sinh(mu[i])
  }
}
"

stopifnot(all(c("plot","n_plot") %in% names(jdat_ch4)))  # quick sanity check

library(rjags); library(coda)
set.seed(99)
jm_ch4 <- jags.model(textConnection(model_ch4_hump_ampRE), data=jdat_ch4,
                     n.chains=3, n.adapt=2000)

update(jm_ch4, 4000)

mon <- c("alpha0","betaT","rho","amp_global","amp_plot","w0_z","s_w_z",
         "sigma","nu","b_season","b_plot","mu","CH4_hat")
samp_ch4 <- coda.samples(jm_ch4, variable.names=mon, n.iter=8000, thin=4)
post_ch4 <- as.matrix(samp_ch4)

post_ch4_S2<-post_ch4



# Posterior means
mu_cols  <- grep("^mu\\[",      colnames(post_ch4))
fit_cols <- grep("^CH4_hat\\[", colnames(post_ch4))
mu_hat   <- colMeans(post_ch4[, mu_cols,  drop=FALSE])     # asinh scale
ch4_fit  <- colMeans(post_ch4[, fit_cols, drop=FALSE])     # original units

pseudo_r2 <- function(y,yhat){
  ok <- is.finite(y) & is.finite(yhat); y<-y[ok]; yhat<-yhat[ok]
  1 - sum((y - yhat)^2) / sum((y - mean(y))^2)
}

r2_asinh <- pseudo_r2(y_asinh, mu_hat)
r2_orig  <- pseudo_r2(ch4$CH4_flux, ch4_fit)
cat(sprintf("Pseudo R^2 (asinh): %.3f   |   Pseudo R^2 (original): %.3f\n", r2_asinh, r2_orig))

# Plot
library(ggplot2)
ggplot(data.frame(fit=ch4_fit, obs=ch4$CH4_flux), aes(fit, obs)) +
  geom_point(alpha=.5) +
  geom_abline(slope=1, intercept=0, linetype=2, linewidth=.4, colour="grey40") +
  labs(x=expression("Fitted CH"[4]*" (µmol m"^{-2}*" s"^{-1}*")"),
       y=expression("Observed CH"[4]*" (µmol m"^{-2}*" s"^{-1}*")"),
       title=sprintf("CH\u2084 observed vs fitted  (pseudo R^2 = %.2f)", r2_orig)) +
  theme_minimal()


################  ################   ################  ################
################  ################   ################  ################
################  ################   ################  ################
#R methane prediction for each station
library(dplyr)

predict_export_ch4_hump <- function(pred_new, post_ch4, fluxes_train,
                                    station_label = "Conserved", ndraw = 500,
                                    season_levels = c("Dry","Rain"),
                                    t_start = "2023-01-01 00:00:00",
                                    t_end   = "2024-12-31 23:59:59",
                                    dt_sec = 1800, Tref = 10){

  stopifnot(all(c("datetime","WTD","consecutive_rain_days","season") %in% names(pred_new)))
  pred_new <- pred_new %>%
    mutate(datetime = as.POSIXct(datetime, tz="UTC"),
           season   = factor(season, levels = season_levels))

  # Soil temperature column
  soilT <- if ("SoilTemp" %in% names(pred_new)) pred_new$SoilTemp else
           if ("soiltemp" %in% names(pred_new)) pred_new$soiltemp else
           stop("Prediction data needs 'SoilTemp' or 'soiltemp'.")

  # -------- station-specific scalers (from training) --------
  zstats <- function(x) c(center=mean(x,na.rm=TRUE), scale=sd(x,na.rm=TRUE))
  zfun   <- function(x, cen, scl) (x - cen)/ifelse(is.finite(scl) && scl>0, scl, 1)

  WTD_stat <- zstats(fluxes_train$WTD)
  RD_stat  <- zstats(fluxes_train$consecutive_rain_days)

  pred_new$WTD_z  <- zfun(pred_new$WTD,  WTD_stat["center"], WTD_stat["scale"])
  pred_new$rain_z <- zfun(pred_new$consecutive_rain_days, RD_stat["center"], RD_stat["scale"])
  season_id <- as.integer(pred_new$season)

  # -------- map Plot levels (use training levels; unseen -> 0 RE) --------
  has_plot <- "Plot" %in% names(pred_new) && "Plot" %in% names(fluxes_train)
  if (has_plot){
    plot_levels_train <- levels(factor(fluxes_train$Plot))
    pred_new$Plot <- factor(pred_new$Plot, levels = plot_levels_train)
    plot_id <- as.integer(pred_new$Plot)   # NA means unseen plot
  } else {
    plot_id <- rep(NA_integer_, nrow(pred_new))
  }

  # -------- posterior draws from hump model --------
  D <- as.data.frame(post_ch4)  # columns: alpha0,betaT,rho,amp_global,w0_z,s_w_z,b_season[],b_plot[],amp_plot[]
  set.seed(7)
  ndraw <- min(ndraw, nrow(D))
  sel   <- sample(seq_len(nrow(D)), ndraw)

  # helpers to pull RE vectors ordered by index
  get_vec <- function(df, base){
    idx <- grep(paste0("^", base, "\\["), colnames(df))
    if(!length(idx)) return(NULL)
    idx <- idx[order(as.integer(sub("^.*\\[(\\d+)\\].*$","\\1", colnames(df)[idx])))]
    list(idx = idx)
  }
  b_season_idx <- get_vec(D, "b_season")$idx
  b_plot_idx   <- get_vec(D, "b_plot")$idx
  amp_plot_idx <- get_vec(D, "amp_plot")$idx

  # Asinh scale for back-transform — use training (robust)
  s_asinh <- max(median(abs(fluxes_train$CH4_flux), na.rm=TRUE), 0.1)

  # one-draw predictor
  predict_one <- function(j){
    p <- D[j,]

    alpha0     <- p[["alpha0"]]
    betaT      <- p[["betaT"]]
    rho        <- p[["rho"]]
    amp_global <- p[["amp_global"]]
    w0_z       <- p[["w0_z"]]
    s_w_z      <- p[["s_w_z"]]

    b_season <- if(length(b_season_idx)) as.numeric(p[b_season_idx]) else 0
    b_plot   <- if(length(b_plot_idx))   as.numeric(p[b_plot_idx])   else 0
    amp_plot <- if(length(amp_plot_idx)) as.numeric(p[amp_plot_idx]) else 0

    # map REs per row (unseen plot -> 0)
    b_season_i <- if(length(b_season_idx)) b_season[pmax(pmin(season_id, length(b_season)),1)] else 0

    b_plot_i <- numeric(nrow(pred_new))
    amp_plot_i <- numeric(nrow(pred_new))
    if (has_plot && length(b_plot_idx)){
      ok <- !is.na(plot_id) & plot_id >= 1 & plot_id <= length(b_plot)
      b_plot_i[ok]    <- b_plot[plot_id[ok]]
      if (length(amp_plot_idx)) amp_plot_i[ok] <- amp_plot[plot_id[ok]]
    }

    hump <- (amp_global + amp_plot_i) * exp(-0.5 * ((pred_new$WTD_z - w0_z)/s_w_z)^2)

    mu <- alpha0 +
          betaT * (soilT - Tref) +
          rho   * pred_new$rain_z +
          hump +
          b_season_i + b_plot_i

    # back to flux units (µmol m^-2 s^-1) and to g C per 30 min
    CH4_rate <- s_asinh * sinh(mu)
    gC_30    <- CH4_rate * dt_sec * 12e-9

    data.frame(datetime = pred_new$datetime,
               CH4_rate = CH4_rate,
               CH4_gC_30 = gC_30)
  }

  pred_list  <- lapply(sel, predict_one)
  pred_draws <- bind_rows(Map(function(d,k) mutate(d, draw=k), pred_list, seq_along(pred_list)))

  # -------- summarise per interval + cumulative (2023–2024) --------
  t0 <- as.POSIXct(t_start, tz="UTC"); t1 <- as.POSIXct(t_end, tz="UTC")
  pred_draws <- pred_draws %>% filter(datetime >= t0, datetime <= t1) %>% arrange(datetime, draw)

  per_int <- pred_draws %>%
    group_by(datetime) %>%
    summarise(
      CH4_rate_mean = mean(CH4_rate),
      CH4_rate_lo   = quantile(CH4_rate, 0.025),
      CH4_rate_hi   = quantile(CH4_rate, 0.975),
      CH4_gC_30_mean= mean(CH4_gC_30),
      CH4_gC_30_lo  = quantile(CH4_gC_30, 0.025),
      CH4_gC_30_hi  = quantile(CH4_gC_30, 0.975),
      .groups="drop"
    ) %>% arrange(datetime)

  cum_by_draw <- pred_draws %>%
    group_by(draw) %>%
    arrange(datetime, .by_group = TRUE) %>%
    mutate(cum_CH4_gC = cumsum(CH4_gC_30)) %>%
    ungroup()

  cum_sum <- cum_by_draw %>%
    group_by(datetime) %>%
    summarise(
      cum_CH4_gC_mean = mean(cum_CH4_gC),
      cum_CH4_gC_lo   = quantile(cum_CH4_gC, 0.025),
      cum_CH4_gC_hi   = quantile(cum_CH4_gC, 0.975),
      .groups="drop"
    )

  out <- pred_new %>%
    select(-WTD_z, -rain_z) %>%
    left_join(per_int, by="datetime") %>%
    left_join(cum_sum, by="datetime") %>%
    mutate(Station = station_label)

  # save
  
  csv <- paste0(station_label, "_CH4_pred_30min_hump.csv")
  rds <- paste0(station_label, "_CH4_pred_30min_hump.rds")
  write.csv(out, csv, row.names = FALSE)
  saveRDS(out, rds)
  message("Saved: ", csv, " and ", rds)

  out
}

# Station 1 = Degraded
ch4_S1_out <- predict_export_ch4_hump(
  pred_new      = st1mod3,
  post_ch4      = post_ch4_S1,   # posterior from S1 hump fit
  fluxes_train  = fluxes_S1,
  station_label = "Degraded",
  ndraw = 500
)

# Station 2 = Conserved
ch4_S2_out <- predict_export_ch4_hump(
  pred_new      = st2mod3,
  post_ch4      = post_ch4_S2,   # posterior from S2 hump fit
  fluxes_train  = fluxes_S2,
  station_label = "Conserved",
  ndraw = 500
)

#############################################
#############################################
#MEthane and co2 cummulative plots


library(dplyr)
library(ggplot2)
library(patchwork)

## ---- 1) Load/prepare data ---------------------------------------------------
# CO2 cumulative (from your saved augmented files or in-memory objects)
s1 <- if (exists("st1_out")) st1_out else readRDS("S1_pred_augmented_30min.rds")
s2 <- if (exists("st2_out")) st2_out else readRDS("S2_pred_augmented_30min.rds")

co2 <- bind_rows(s1, s2) %>%
  mutate(
    Station = dplyr::recode(Station, "S1"="Degraded", "S2"="Conserved", .default=Station),
    Station = factor(Station, levels = c("Conserved","Degraded"))
  ) %>%
  transmute(datetime,
            Station,
            mean = cum_NEE_gC_mean,
            lo   = cum_NEE_gC_lo,
            hi   = cum_NEE_gC_hi)

# CH4 cumulative (from CH4 prediction exports)
ch4_con <- if (exists("ch4_S2_out")) ch4_S2_out else readRDS("Conserved_CH4_pred_30min_hump.rds")
ch4_deg <- if (exists("ch4_S1_out")) ch4_S1_out else readRDS("Degraded_CH4_pred_30min_hump.rds")

ch4 <- bind_rows(ch4_con, ch4_deg) %>%
  mutate(Station = factor(Station, levels = c("Conserved","Degraded"))) %>%
  transmute(datetime,
            Station,
            mean = cum_CH4_gC_mean,
            lo   = cum_CH4_gC_lo,
            hi   = cum_CH4_gC_hi)

# Time window
win_start <- as.POSIXct("2023-01-01 00:00:00", tz="UTC")
win_end   <- as.POSIXct("2024-12-31 23:59:59", tz="UTC")
co2 <- co2 %>% filter(datetime >= win_start, datetime <= win_end)
ch4 <- ch4 %>% filter(datetime >= win_start, datetime <= win_end)

## ---- 2) Season rectangles (Dry only = red; Rain = clear) --------------------
season.start.date <- as.POSIXct(c("2021-01-01","2021-03-01","2021-09-01","2021-10-01","2021-11-01",
                                  "2022-03-01","2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02","2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21","2024-10-06","2024-12-15",
                                  "2025-03-01"), tz="UTC")
season.end.date   <- as.POSIXct(c("2021-03-01","2021-09-01","2021-10-01","2021-11-01",
                                  "2022-03-01","2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02","2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21","2024-10-06","2024-12-15",
                                  "2025-03-01","2025-07-15"), tz="UTC")
season <- c("Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain",
            "Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain")

seasons_df <- data.frame(start=season.start.date, end=season.end.date, season=season) %>%
  filter(end >= win_start & start <= win_end) %>%
  mutate(start = pmax(start, win_start),
         end   = pmin(end,   win_end)) %>%
  filter(season == "Dry")  # only shade Dry

## ---- 3) Aesthetics ----------------------------------------------------------
pal <- c(Conserved = "#2CA25F", Degraded = "#E67E22")  # teal & orange
dry_fill <- "#D59696"  # soft red for Dry bands (tweak if you like)

base_theme <- theme_classic(base_size = 12) +
  theme(panel.grid = element_blank(),
        legend.background = element_rect(fill = scales::alpha("white", 0.8), colour = NA),
        legend.title = element_text(size = 11),
        legend.text  = element_text(size = 10))

## ---- 4) Panels --------------------------------------------------------------
p_co2 <- ggplot() +
  geom_rect(data = seasons_df,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = dry_fill, alpha = 0.20, inherit.aes = FALSE) +
  geom_ribbon(data = co2, aes(x = datetime, ymin = lo, ymax = hi, fill = Station),
              alpha = 0.25, colour = NA) +
  geom_line(data = co2, aes(x = datetime, y = mean, color = Station), linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.35) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(y = expression("Cumulative CO"[2]*"  [g C m"^{-2}*"]"),
       x = NULL, color = "Station", fill = "Station") +
  base_theme +
  theme(legend.position = c(0.12, 0.82),
        axis.title.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

p_ch4 <- ggplot() +
  geom_rect(data = seasons_df,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = dry_fill, alpha = 0.20, inherit.aes = FALSE) +
  geom_ribbon(data = ch4, aes(x = datetime, ymin = lo, ymax = hi, fill = Station),
              alpha = 0.25, colour = NA) +
  geom_line(data = ch4, aes(x = datetime, y = mean, color = Station), linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 0.35) +
  scale_color_manual(values = pal, guide = "none") +
  scale_fill_manual(values  = pal, guide = "none") +
  labs(y = expression("Cumulative CH"[4]*"  [g C m"^{-2}*"]"),
       x = "TIMESTAMP") +
  base_theme

## ---- 5) Combine -------------------------------------------------------------
p_co2 / p_ch4 + plot_layout(heights = c(1, 1))


################################################################################
##########Final synthetic table


library(dplyr)
library(readr)
library(lubridate)

## ---------------- 0) File locations ----------------
# Model outputs you saved earlier
co2_s1_file <- "S1_pred_augmented_30min.rds"         # CO2 (S1)
co2_s2_file <- "S2_pred_augmented_30min.rds"         # CO2 (S2)
ch4_deg_file <- "Degraded_CH4_pred_30min_hump.rds"   # CH4 (S1=Degraded)
ch4_con_file <- "Conserved_CH4_pred_30min_hump.rds"  # CH4 (S2=Conserved)

# Eddy-covariance CSVs (uploaded)
ec_s1_file <- "ST1NEEcum2.csv"   # S1 = Degraded
ec_s2_file <- "ST2NEEcum2.csv"   # S2 = Conserved

## ---------------- 1) Helpers ----------------
dt_sec <- 1800

# Robust diff for cumulative -> per-interval, keeping first interval as the first value
diff_cum <- function(x){
  x <- as.numeric(x)
  if (!length(x)) return(x)
  c(x[1], diff(x))
}

parse_ts <- function(x) {
  # Your files look like "m/d/yy H:M"; tz not specified → use UTC (or set your TZ)
  suppressWarnings(mdy_hm(x, tz = "UTC"))
}

# Standardize model outputs to canonical column names
std_co2_model <- function(df){
  # Accept either NEE_gC_mean or NEE_gC_30_mean naming
  gC_col <- if ("NEE_gC_30_mean" %in% names(df)) "NEE_gC_30_mean" else "NEE_gC_mean"
  df %>%
    mutate(Station = recode(Station, "S1"="Degraded", "S2"="Conserved", .default=Station),
           datetime = as.POSIXct(datetime, tz="UTC"),
           season = if ("season" %in% names(df)) as.character(season) else as.character(season)) %>%
    transmute(
      Source   = "Model",
      Station  = factor(Station, levels=c("Conserved","Degraded")),
      datetime,
      season   = as.character(season),
      # Rate and gC per interval
      NEE_rate = NEE_rate_mean,                 # µmol CO2 m-2 s-1
      NEE_gC_30 = .data[[gC_col]],              # g C per 30 min
      CH4_rate = NA_real_,                      # not from CO2 model
      CH4_gC_30 = NA_real_
    )
}

std_ch4_model <- function(df){
  df %>%
    mutate(datetime = as.POSIXct(datetime, tz="UTC")) %>%
    transmute(
      Source   = "Model",
      Station  = factor(Station, levels=c("Conserved","Degraded")),
      datetime,
      season   = as.character(season),
      # CH4 (nmol m-2 s-1) + g C per 30 min
      NEE_rate = NA_real_,
      NEE_gC_30 = NA_real_,
      CH4_rate = CH4_rate_mean,                 # nmol CH4 m-2 s-1
      CH4_gC_30 = CH4_gC_30_mean                # g C per 30 min
    )
}

# Read EC file, compute per-interval NEE/CH4 and rates from cumulative columns
read_ec <- function(path, station_label){
  ec <- read_csv(path, show_col_types = FALSE)
  # Expected cols: TIMESTAMP, co2_accum, ch4_accum, Reco_DT_uStar (ER), GPP_DT_uStar (GPP), Season
  # Parse time
  if ("TIMESTAMP" %in% names(ec)) {
    dt <- parse_ts(ec$TIMESTAMP)
  } else if ("DateTime_fixed" %in% names(ec)) {
    dt <- parse_ts(ec$DateTime_fixed)
  } else stop("No timestamp column found in: ", path)

  # Season column
  season <- if ("Season" %in% names(ec)) as.character(ec$Season) else NA_character_

  # Per-interval g C from cumulative series
  co2_gC_30 <- if ("co2_accum" %in% names(ec)) diff_cum(ec$co2_accum) else NA_real_
  ch4_gC_30 <- if ("ch4_accum" %in% names(ec)) diff_cum(ec$ch4_accum) else NA_real_

  # If ER and GPP are available, compute a rate NEE = ER - GPP (µmol s-1);
  # If not present, derive NEE_rate from gC (µmol m-2 s-1).
  nee_rate <- NA_real_
  if (all(c("Reco_DT_uStar","GPP_DT_uStar") %in% names(ec))) {
    nee_rate <- ec$Reco_DT_uStar - ec$GPP_DT_uStar
  } else if (!all(is.na(co2_gC_30))) {
    nee_rate <- co2_gC_30 / (dt_sec * 12e-6)  # gC -> µmol s-1
  }

  # CH4 rate (nmol s-1) from gC if not explicitly provided
  ch4_rate <- if (!all(is.na(ch4_gC_30))) ch4_gC_30 / (dt_sec * 12e-9) else NA_real_

  tibble(
    Source     = "EC",
    Station    = factor(station_label, levels=c("Conserved","Degraded")),
    datetime   = dt,
    season     = season,
    NEE_rate   = as.numeric(nee_rate),
    NEE_gC_30  = as.numeric(co2_gC_30),
    CH4_rate   = as.numeric(ch4_rate),
    CH4_gC_30  = as.numeric(ch4_gC_30)
  )
}

## ---------------- 2) Load model outputs ----------------
co2_s1 <- readRDS(co2_s1_file)
co2_s2 <- readRDS(co2_s2_file)
co2_model <- bind_rows(std_co2_model(co2_s1), std_co2_model(co2_s2))

ch4_deg <- readRDS(ch4_deg_file)  # Degraded
ch4_con <- readRDS(ch4_con_file)  # Conserved
ch4_model <- bind_rows(std_ch4_model(ch4_con), std_ch4_model(ch4_deg))

## ---------------- 3) Load EC data (S1=Degraded, S2=Conserved) ---------------
ec_s1 <- read_ec(ec_s1_file, station_label = "Degraded")
ec_s2 <- read_ec(ec_s2_file, station_label = "Conserved")
ec_all <- bind_rows(ec_s1, ec_s2)

## ---------------- 4) Combine into one 30-min table --------------------------
all_30min <- bind_rows(
  co2_model,   # model CO2
  ch4_model,   # model CH4
  ec_all       # EC CO2 + CH4
) %>%
  mutate(
    Year = year(datetime),
    # if any 'season' missing in model outputs, keep as NA; EC already has Season
    season = factor(season, levels = c("Dry","Rain"))
  ) %>%
  arrange(Station, datetime, Source)

# Save the big table
saveRDS(all_30min, "ALL_fluxes_30min_CO2_CH4_model_EC.rds")
write.csv(all_30min, "ALL_fluxes_30min_CO2_CH4_model_EC.csv", row.names = FALSE)

## ---------------- 5) Mean/Variance by Season × Year (+ both years) ----------
summarize_season <- function(df, rate_col, gC_col, gas_label){
  base <- df %>%
    group_by(Source, Station, season, Year) %>%
    summarise(
      mean_rate = mean(.data[[rate_col]], na.rm = TRUE),
      var_rate  = var(.data[[rate_col]],  na.rm = TRUE),
      mean_gC30 = mean(.data[[gC_col]],   na.rm = TRUE),
      var_gC30  = var(.data[[gC_col]],    na.rm = TRUE),
      n_intervals = sum(is.finite(.data[[rate_col]])),
      .groups = "drop"
    ) %>% mutate(Gas = gas_label)

  both_years <- df %>%
    group_by(Source, Station, season) %>%
    summarise(
      mean_rate = mean(.data[[rate_col]], na.rm = TRUE),
      var_rate  = var(.data[[rate_col]],  na.rm = TRUE),
      mean_gC30 = mean(.data[[gC_col]],   na.rm = TRUE),
      var_gC30  = var(.data[[gC_col]],    na.rm = TRUE),
      n_intervals = sum(is.finite(.data[[rate_col]])),
      .groups = "drop"
    ) %>% mutate(Year = NA_integer_, Gas = gas_label)

  bind_rows(base, both_years)
}

# Restrict to the 2023–2024 window (your focus period)
win_start <- ymd_hms("2023-01-01 00:00:00", tz = "UTC")
win_end   <- ymd_hms("2024-12-31 23:59:59", tz = "UTC")
all_30min_win <- all_30min %>%
  filter(datetime >= win_start, datetime <= win_end)

co2_stats <- summarize_season(all_30min_win, "NEE_rate",  "NEE_gC_30",  "CO2 (NEE)")
ch4_stats <- summarize_season(all_30min_win, "CH4_rate",  "CH4_gC_30",  "CH4")

season_stats <- bind_rows(co2_stats, ch4_stats) %>%
  arrange(Gas, Source, Station, season, Year)

# Save summary
saveRDS(season_stats, "Season_MeanVariance_CO2_CH4_bySource.rds")
write.csv(season_stats, "Season_MeanVariance_CO2_CH4_bySource.csv", row.names = FALSE)

## ---------------- 6) Quick peek --------------------------------------------
print(head(all_30min))
print(season_stats %>% filter(is.na(Year)) %>% arrange(Gas, Source, Station, season))



################################################################################
##########Final synthetic table in co2 equivalents
# ================= FINAL PIPELINE (robust to EC format m/d/yy H:M) =================
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

## ---- A) Season calendar & helpers ----
season.start.date <- as.POSIXct(c("2021-01-01","2021-03-01","2021-09-01","2021-10-01","2021-11-01",
                                  "2022-03-01","2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02","2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21","2024-10-06","2024-12-15",
                                  "2025-03-01"), tz="UTC")
season.end.date   <- as.POSIXct(c("2021-03-01","2021-09-01","2021-10-01","2021-11-01",
                                  "2022-03-01","2022-09-01","2022-10-01","2022-12-01",
                                  "2023-03-10","2023-10-02","2023-10-27","2023-12-20",
                                  "2024-04-28","2024-08-21","2024-10-06","2024-12-15",
                                  "2025-03-01","2025-07-15"), tz="UTC")
season_vec <- c("Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain",
                "Dry","Rain","Dry","Rain","Dry","Rain","Dry","Rain")

win_start <- as.POSIXct("2023-01-01 00:00:00", tz="UTC")
win_end   <- as.POSIXct("2024-12-31 23:59:59", tz="UTC")

seasons_df <- data.frame(start=season.start.date, end=season.end.date,
                         season=factor(season_vec, levels=c("Dry","Rain"))) |>
  dplyr::filter(end >= win_start & start <= win_end) |>
  mutate(start = pmax(start, win_start), end = pmin(end, win_end))

assign_season <- function(dt, seasons_tbl = seasons_df){
  out <- rep(NA_character_, length(dt))
  for (i in seq_len(nrow(seasons_tbl))){
    idx <- dt >= seasons_tbl$start[i] & dt < seasons_tbl$end[i]
    out[idx] <- as.character(seasons_tbl$season[i])
  }
  factor(out, levels=c("Dry","Rain"))
}

## ---- B) EC reader (TIMESTAMP format = %m/%d/%y %H:%M e.g., 1/1/23 0:30) ----
ec_read_make_30 <- function(path, station_label){
  ec <- readr::read_csv(path, show_col_types = FALSE)
  # Drop blank index columns if present (like ...1)
  bad <- grep("^\\.\\.\\.", names(ec))
  if (length(bad)) ec <- ec[ , -bad, drop=FALSE]

  # Timestamp column (TIMESTAMP preferred; fallback DATE)
  if ("TIMESTAMP" %in% names(ec)) {
    dt <- suppressWarnings(mdy_hm(ec$TIMESTAMP, tz = "UTC"))
  } else if ("DATE" %in% names(ec)) {
    dt <- suppressWarnings(mdy_hm(ec$DATE, tz = "UTC"))
  } else stop("No TIMESTAMP/DATE column in: ", path)

  ec <- ec |>
    mutate(datetime = dt) |>
    arrange(datetime) |>
    # keep only the analysis window
    filter(datetime >= win_start, datetime <= win_end)

  stopifnot(all(c("co2_accum","ch4_accum") %in% names(ec)))

  ec <- ec |>
    mutate(
      # 30-min increments (signed; first diff = NA)
      NEE_gC_30 = co2_accum - dplyr::lag(co2_accum),
      CH4_gC_30 = ch4_accum - dplyr::lag(ch4_accum),
      Station   = factor(station_label, levels=c("Conserved","Degraded")),
      Year      = year(datetime),
      season    = assign_season(datetime)
    ) |>
    filter(is.finite(NEE_gC_30) | is.finite(CH4_gC_30))

  # Quick sanity
  message(station_label, " EC rows: ", nrow(ec))
  ec
}

# Your EC files
ec_deg <- ec_read_make_30("ST1NEEcum2.csv", "Degraded")
ec_con <- ec_read_make_30("ST2NEEcum2.csv", "Conserved")

## ---- C) Model outputs (per-30-min g C + simple SD) ----
sd_from_ci <- function(lo, hi) ifelse(is.finite(lo)&is.finite(hi), (hi-lo)/3.92, NA_real_)

std_co2_model <- function(df){
  gC_mean <- dplyr::coalesce(df$NEE_gC_30_mean, df$NEE_gC_mean)
  gC_lo   <- dplyr::coalesce(df$NEE_gC_30_lo,   df$NEE_gC_lo)
  gC_hi   <- dplyr::coalesce(df$NEE_gC_30_hi,   df$NEE_gC_hi)
  dt      <- as.POSIXct(df$datetime, tz="UTC")
  tibble(
    Source   = "Model",
    Station  = factor(dplyr::recode(df$Station, "S1"="Degraded", "S2"="Conserved", .default=as.character(df$Station)),
                      levels=c("Conserved","Degraded")),
    datetime = dt,
    Year     = year(dt),
    season   = assign_season(dt),
    Gas      = "CO2",
    gC_30_mean = gC_mean,
    gC_30_sd   = sd_from_ci(gC_lo, gC_hi)
  ) |>
    filter(datetime >= win_start, datetime <= win_end)
}

std_ch4_model <- function(df){
  dt <- as.POSIXct(df$datetime, tz="UTC")
  tibble(
    Source   = "Model",
    Station  = factor(df$Station, levels=c("Conserved","Degraded")),
    datetime = dt,
    Year     = year(dt),
    season   = assign_season(dt),
    Gas      = "CH4",
    gC_30_mean = df$CH4_gC_30_mean,
    gC_30_sd   = sd_from_ci(df$CH4_gC_30_lo, df$CH4_gC_30_hi)
  ) |>
    filter(datetime >= win_start, datetime <= win_end)
}

co2_mod <- bind_rows(
  std_co2_model(readRDS("S1_pred_augmented_30min.rds")),
  std_co2_model(readRDS("S2_pred_augmented_30min.rds"))
)
ch4_mod <- bind_rows(
  std_ch4_model(readRDS("Conserved_CH4_pred_30min_hump.rds")),
  std_ch4_model(readRDS("Degraded_CH4_pred_30min_hump.rds"))
)

## ---- D) EC tidy (SD=0) ----
ec_tidy <- bind_rows(
  ec_deg |> transmute(Source="EC", Station, datetime, Year, season, Gas="CO2", gC_30_mean=NEE_gC_30, gC_30_sd=0),
  ec_deg |> transmute(Source="EC", Station, datetime, Year, season, Gas="CH4", gC_30_mean=CH4_gC_30, gC_30_sd=0),
  ec_con |> transmute(Source="EC", Station, datetime, Year, season, Gas="CO2", gC_30_mean=NEE_gC_30, gC_30_sd=0),
  ec_con |> transmute(Source="EC", Station, datetime, Year, season, Gas="CH4", gC_30_mean=CH4_gC_30, gC_30_sd=0)
) |> filter(is.finite(gC_30_mean))

## ---- E) Unified 30-min table ----
all_30 <- bind_rows(co2_mod, ch4_mod, ec_tidy) |>
  mutate(Source = factor(Source, levels=c("Model","EC")),
         Station= factor(Station, levels=c("Conserved","Degraded"))) |>
  arrange(Station, Source, datetime, Gas)

# (Optional) save
# saveRDS(all_30, "ALL_30min_CO2_CH4_Model_EC_withSD.rds")
# write.csv(all_30, "ALL_30min_CO2_CH4_Model_EC_withSD.csv", row.names = FALSE)

## ---- F) Aggregate with simple uncertainty (sum of variances) ----
summarise_block <- function(df){
  out <- df |>
    summarise(
      mean_gC = sum(gC_30_mean, na.rm=TRUE),
      var_gC  = sum(replace_na(gC_30_sd, 0)^2, na.rm=TRUE),
      .groups="drop"
    )
  out$sd_gC <- sqrt(out$var_gC)
  out$lo_gC <- out$mean_gC - 1.96*out$sd_gC
  out$hi_gC <- out$mean_gC + 1.96*out$sd_gC
  out
}

# Season × Year
by_year_season <- all_30 |>
  group_by(Source, Station, Year, season, Gas) |>
  summarise_block() |> ungroup()

# Both years × Season
both_years_season <- all_30 |>
  group_by(Source, Station, season, Gas) |>
  summarise_block() |> ungroup() |>
  mutate(Year = NA_integer_)

# All seasons roll-up
by_year_all <- all_30 |>
  group_by(Source, Station, Year, Gas) |>
  summarise_block() |> ungroup() |>
  mutate(season = factor("All", levels=c("Dry","Rain","All")))
both_years_all <- all_30 |>
  group_by(Source, Station, Gas) |>
  summarise_block() |> ungroup() |>
  mutate(Year = NA_integer_, season = factor("All", levels=c("Dry","Rain","All")))

cum_gc <- bind_rows(by_year_season, both_years_season, by_year_all, both_years_all) |>
  mutate(Year_label = ifelse(is.na(Year), "2023–2024", as.character(Year))) |>
  arrange(Station, Source, Year_label, season, Gas)

## ---- G) Convert to CO2-eq (per gas) ----
MW_CO2 <- 44; MW_C <- 12; MW_CH4 <- 16
C_to_CO2 <- MW_CO2 / MW_C   # 44/12
C_to_CH4 <- MW_CH4 / MW_C   # 16/12
GWP_UNFCCC <- 28            # AR5 UNFCCC 100-yr
GWP_AR6    <- 27            # AR6 biogenic 100-yr
to_Mg_ha <- function(g_m2) g_m2 / 100

per_gas <- cum_gc |>
  mutate(
    mean_gas = ifelse(Gas=="CO2", mean_gC*C_to_CO2, mean_gC*C_to_CH4),  # g gas m-2
    sd_gas   = ifelse(Gas=="CO2", sd_gC*C_to_CO2,   sd_gC*C_to_CH4),

    mean_UNFCCC = ifelse(Gas=="CO2", mean_gas, mean_gas*GWP_UNFCCC),
    sd_UNFCCC   = ifelse(Gas=="CO2", sd_gas,   sd_gas*GWP_UNFCCC),

    mean_AR6 = ifelse(Gas=="CO2", mean_gas, mean_gas*GWP_AR6),
    sd_AR6   = ifelse(Gas=="CO2", sd_gas,   sd_gas*GWP_AR6),

    lo_UNFCCC = mean_UNFCCC - 1.96*sd_UNFCCC,
    hi_UNFCCC = mean_UNFCCC + 1.96*sd_UNFCCC,
    lo_AR6    = mean_AR6    - 1.96*sd_AR6,
    hi_AR6    = mean_AR6    + 1.96*sd_AR6,

    CO2eq_UNFCCC_Mg_ha_mean = to_Mg_ha(mean_UNFCCC),
    CO2eq_UNFCCC_Mg_ha_lo   = to_Mg_ha(lo_UNFCCC),
    CO2eq_UNFCCC_Mg_ha_hi   = to_Mg_ha(hi_UNFCCC),

    CO2eq_AR6_Mg_ha_mean = to_Mg_ha(mean_AR6),
    CO2eq_AR6_Mg_ha_lo   = to_Mg_ha(lo_AR6),
    CO2eq_AR6_Mg_ha_hi   = to_Mg_ha(hi_AR6)
  ) |>
  select(Source, Station, Year_label, season, Gas,
         mean_gC, lo_gC, hi_gC,
         CO2eq_UNFCCC_Mg_ha_mean, CO2eq_UNFCCC_Mg_ha_lo, CO2eq_UNFCCC_Mg_ha_hi,
         CO2eq_AR6_Mg_ha_mean,    CO2eq_AR6_Mg_ha_lo,    CO2eq_AR6_Mg_ha_hi) |>
  arrange(Station, Source, Year_label, season, Gas)

## ---- H) Totals across gases (CO2 + CH4) ----
total_eq <- cum_gc |>
  mutate(
    mean_gas = ifelse(Gas=="CO2", mean_gC*C_to_CO2, mean_gC*C_to_CH4),
    sd_gas   = ifelse(Gas=="CO2", sd_gC*C_to_CO2,   sd_gC*C_to_CH4),
    mean_UNFCCC = ifelse(Gas=="CO2", mean_gas, mean_gas*GWP_UNFCCC),
    sd_UNFCCC   = ifelse(Gas=="CO2", sd_gas,   sd_gas*GWP_UNFCCC),
    mean_AR6    = ifelse(Gas=="CO2", mean_gas, mean_gas*GWP_AR6),
    sd_AR6      = ifelse(Gas=="CO2", sd_gas,   sd_gas*GWP_AR6)
  ) |>
  group_by(Source, Station, Year_label, season) |>
  summarise(
    mean_UNFCCC = sum(mean_UNFCCC, na.rm=TRUE),
    sd_UNFCCC   = sqrt(sum(sd_UNFCCC^2, na.rm=TRUE)),
    mean_AR6    = sum(mean_AR6,    na.rm=TRUE),
    sd_AR6      = sqrt(sum(sd_AR6^2,    na.rm=TRUE)),
    .groups="drop"
  ) |>
  mutate(
    lo_UNFCCC = mean_UNFCCC - 1.96*sd_UNFCCC,
    hi_UNFCCC = mean_UNFCCC + 1.96*sd_UNFCCC,
    lo_AR6    = mean_AR6    - 1.96*sd_AR6,
    hi_AR6    = mean_AR6    + 1.96*sd_AR6,

    Total_CO2eq_UNFCCC_Mg_ha_mean = to_Mg_ha(mean_UNFCCC),
    Total_CO2eq_UNFCCC_Mg_ha_lo   = to_Mg_ha(lo_UNFCCC),
    Total_CO2eq_UNFCCC_Mg_ha_hi   = to_Mg_ha(hi_UNFCCC),

    Total_CO2eq_AR6_Mg_ha_mean = to_Mg_ha(mean_AR6),
    Total_CO2eq_AR6_Mg_ha_lo   = to_Mg_ha(lo_AR6),
    Total_CO2eq_AR6_Mg_ha_hi   = to_Mg_ha(hi_AR6)
  ) |>
  arrange(Station, Source, Year_label, season)

## ---- I) Quick checks (ensure EC present) ----
cat("\nCounts by Source (should include EC):\n")
print(all_30 |> count(Source))

cat("\nPer-gas head:\n"); print(head(per_gas, 6))
cat("\nTotals head:\n");  print(head(total_eq, 6))

# (Optional) write tables
write.csv(per_gas,  "PerGas_CO2_CH4_with_CO2eq_Mg_ha_mean95CI.csv", row.names = FALSE)
write.csv(total_eq, "Totals_CO2eq_across_gases_Mg_ha_mean95CI.csv", row.names = FALSE)

library(ggplot2)
library(dplyr)

# Load your data
df <- read.csv("PerGas_CO2_CH4_with_CO2eq_Mg_ha_mean95CI.csv")

# Recode: if method == "discharge" → call it "DOC"
df <- df %>%
  mutate(Gas = ifelse(method == "discharge", "DOC", Gas))

# --- 1. Year by year stacked bar plot ---
ggplot(df, aes(x = season, 
               y = CO2eq_AR6_Mg_ha_mean, 
               fill = Gas)) +
  geom_bar(stat = "identity") +
  facet_grid(Station ~ Year_label) +
  labs(y = "CO₂-eq (Mg ha⁻¹ yr⁻¹)", x = "Season",
       title = "Year by Year CO₂-eq contributions by gas (incl. DOC)") +
  theme_bw() +
  theme(panel.grid = element_blank())


# --- 2. Pooled across years ---
df_pooled <- df %>%
  group_by(Station, season, Gas) %>%
  summarise(CO2eq_AR6_Mg_ha_mean = mean(CO2eq_AR6_Mg_ha_mean),
            .groups = "drop")

ggplot(df_pooled, aes(x = season, 
                      y = CO2eq_AR6_Mg_ha_mean, 
                      fill = Gas)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Station) +
  labs(y = "CO₂-eq (Mg ha⁻¹ yr⁻¹)", x = "Season",
       title = "Pooled across years CO₂-eq contributions by gas (incl. DOC)") +
  theme_bw() +
  theme(panel.grid = element_blank())


