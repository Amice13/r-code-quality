library(dplyr)

# --- Packages
library(dplyr); library(rjags); library(coda)

# --- Read and prep
df <- read.csv("fluxes_final.csv", stringsAsFactors = FALSE)

df<-subset(df,Site2=="S1")

# 1) Keep daytime & informative light (tweak thresholds if needed)
gpp_fit <- df %>%
  filter(is.finite(GPP_flux), is.finite(PAR), is.finite(consecutive_rain_days),
         tolower(`Day.Night`) == "day",  # uses your Day.Night column
         PAR > 100)

# 2) Group for partial pooling (by Plot; you can try Date4 or Site2 as well)
gpp_fit <- gpp_fit %>% mutate(grp = as.integer(factor(Plot)))
G <- max(gpp_fit$grp)

# 3) Scale drivers
Pmin <- min(gpp_fit$consecutive_rain_days, na.rm=TRUE)
Pmax <- max(gpp_fit$consecutive_rain_days, na.rm=TRUE)
gpp_fit$Psc <- if ((Pmax - Pmin) == 0) 0.5 else
                 (gpp_fit$consecutive_rain_days - Pmin) / (Pmax - Pmin)

PAR95 <- as.numeric(quantile(gpp_fit$PAR, 0.95, na.rm=TRUE))
gpp_fit$PAR_sc <- gpp_fit$PAR / (PAR95 + 1e-6)

# 4) Data-anchored prior centers
hi <- gpp_fit$PAR >= quantile(gpp_fit$PAR, 0.95, na.rm=TRUE)
Amax_guess <- median(gpp_fit$GPP_flux[hi], na.rm=TRUE)
Amax_guess <- ifelse(is.finite(Amax_guess) && Amax_guess > 0, Amax_guess,
                     max(median(gpp_fit$GPP_flux, na.rm=TRUE), 0.1))
mu_logAmax  <- log(Amax_guess)
sd_logAmax  <- log(2)               # ~±2× on original scale
tau_logAmax <- 1 / sd_logAmax^2

# Alpha prior center on log-scale (so alpha_g = exp(etaα_g) around ~0.2 on PAR_sc)
mu_logAlpha <- log(0.2)
sd_logAlpha <- log(3)               # wide
tau_logAlpha <- 1 / sd_logAlpha^2

sigma_max <- 5 * sd(gpp_fit$GPP_flux, na.rm=TRUE)

data_gpp <- list(
  N = nrow(gpp_fit),
  GPP = gpp_fit$GPP_flux,
  PAR_sc = gpp_fit$PAR_sc,
  Psc = gpp_fit$Psc,
  grp = gpp_fit$grp,
  G = G,
  mu_logAmax = mu_logAmax,
  tau_logAmax = tau_logAmax,
  mu_logAlpha = mu_logAlpha,
  tau_logAlpha = tau_logAlpha,
  sigma_max = as.numeric(sigma_max)
)

model_gpp_hier_robust <- "
model {
  # Random effects on log-scale
  mu_etaA  ~ dnorm(mu_logAmax, tau_logAmax)
  tau_etaA ~ dgamma(0.5, 0.5)
  mu_etaAl ~ dnorm(mu_logAlpha, tau_logAlpha)
  tau_etaAl ~ dgamma(0.5, 0.5)

  for (g in 1:G) {
    etaA[g]  ~ dnorm(mu_etaA,  tau_etaA)
    etaAl[g] ~ dnorm(mu_etaAl, tau_etaAl)
    Amax_g[g]  <- exp(etaA[g])     # >0
    alpha_g[g] <- exp(etaAl[g])    # >0
  }

  # Curve & hydrologic gate
  theta ~ dunif(0.2, 1)            # NRH curvature
  betaP ~ dunif(0, 10)             # rain-days effect on capacity
  Rd    ~ dgamma(0.01, 0.01)       # small positive offset (optional)

  # Observation model: Student-t
  sigma ~ dunif(0, sigma_max)
  tau <- 1 / pow(sigma, 2)
  nu  ~ dexp(1/30) T(2,)           # df > 2

  for (i in 1:N) {
    # Capacity modulated by rain-days
    Pcap[i] <- Amax_g[grp[i]] * (1 - exp(-betaP * Psc[i]))

    # Non-rectangular hyperbola
    term[i] <- alpha_g[grp[i]] * PAR_sc[i] + Pcap[i]
    disc[i] <- pow(term[i], 2) - 4 * theta * alpha_g[grp[i]] * PAR_sc[i] * Pcap[i]
    Anet[i] <- (term[i] - sqrt(max(disc[i], 1.0E-9))) / (2 * theta)

    mu[i] <- Anet[i] + Rd
    GPP[i] ~ dt(mu[i], tau, nu)
  }
}
"
library(rjags); library(coda)

inits_fun <- function() list(
  mu_etaA  = log(Amax_guess),
  mu_etaAl = log(0.2),
  theta = runif(1, 0.6, 0.9),
  betaP = runif(1, 0.1, 2),
  Rd    = runif(1, 0, 0.3),
  sigma = runif(1, 0.1, max(1, 0.3 * sd(gpp_fit$GPP_flux, na.rm=TRUE))),
  nu = runif(1, 3, 15)
)

set.seed(1)
m_gpp <- jags.model(textConnection(model_gpp_hier_robust), data = data_gpp,
                    inits = inits_fun, n.chains = 4, n.adapt = 5000)
update(m_gpp, 25000)

pars <- c("Amax_g","alpha_g","theta","betaP","Rd","sigma","nu")
s_gpp <- coda.samples(m_gpp, pars, n.iter = 90000, thin = 10)
post_gpp <- as.data.frame(do.call(rbind, lapply(s_gpp, as.matrix)))
summary(s_gpp)[, c("Mean","2.5%","97.5%")]

# Posterior-mean predictions (same formula as model)
gpp_mu_fun <- function(PAR_sc, Psc, par, Amax_vec, alpha_vec, grp){
  Pcap <- Amax_vec[grp] * (1 - exp(-par["betaP"] * Psc))
  term <- alpha_vec[grp] * PAR_sc + Pcap
  disc <- term^2 - 4 * par["theta"] * alpha_vec[grp] * PAR_sc * Pcap
  Anet <- (term - sqrt(pmax(disc, 1e-9))) / (2 * par["theta"])
  Anet + par["Rd"]
}

set.seed(42)
K <- 2000
idx <- sample(seq_len(nrow(post_gpp)), K, replace = TRUE)
Ag_cols <- grep("^Amax_g\\[", colnames(post_gpp))
Al_cols <- grep("^alpha_g\\[", colnames(post_gpp))

MU <- matrix(NA_real_, nrow = K, ncol = nrow(gpp_fit))
for(k in 1:K){
  par <- unlist(post_gpp[idx[k], c("theta","betaP","Rd")])
  Amax_vec  <- as.numeric(post_gpp[idx[k], Ag_cols])
  alpha_vec <- as.numeric(post_gpp[idx[k], Al_cols])
  MU[k,] <- gpp_mu_fun(gpp_fit$PAR_sc, gpp_fit$Psc, par, Amax_vec, alpha_vec, gpp_fit$grp)
}
gpp_hat <- colMeans(MU)

pseudo_r2 <- function(y,yhat){ ok <- is.finite(y)&is.finite(yhat); y<-y[ok]; yhat<-yhat[ok]
  1 - sum((y - yhat)^2) / sum((y - mean(y))^2) }
R2_gpp <- pseudo_r2(gpp_fit$GPP_flux, gpp_hat)
R2_gpp


# install.packages("mgcv")
library(mgcv)

# Compute VPD (kPa) from AirTemp & RH
es <- 0.6108 * exp((17.27 * gpp_fit$AirTemp) / (gpp_fit$AirTemp + 237.3))
ea <- es * (gpp_fit$RH/100)
gpp_fit$VPD_kPa <- pmax(es - ea, 0)

gam_gpp <- gam(GPP_flux ~ s(PAR, k=6, bs="cs") +
                          s(SoilTemp, k=5, bs="cs") +
                          s(consecutive_rain_days, k=5, bs="cs")+
                          s(as.numeric(Plot), bs="re"),
               data = gpp_fit, method = "REML")


summary(gam_gpp)$dev.expl  # explained deviance ≈ sanity-check R²




