# Please note that these model take a considerable amount of time to run.

# load libraries
library(foreign); library(rjags); library(coda); library(xtable)


# load data
data <- read.dta("RECOUPING_DATA_FOR_PUBLICATION.dta")
#data <- read.dta("recouping_data_11-21-2014_old.dta")


# subset data for analysis
data <- subset(data, select=c(year, ccode, defense_allies, effective_number, min_regime, rival_threat_level_new, polity2, Oil,cinc, lmtnest, mideast, US_ally, Great_Britain_ally, Russia_ally, France_ally, China_ally, milper, pursuitnuk, acquirenuk, CBWpursuit, CBWpossess))
dim(data)


# generate variables consistent with the STATA code
data$ln_effective_number <- log(data$effective_number)
data$coup_proof_interact <-  data$min_regime*data$ln_effective_number


data$ln_effective_number <- log(data$effective_number)
data$coup_proof_interact <-  data$min_regime*data$ln_effective_number

data$ln_milper <- log(data$milper+1)


data <- subset(data, year>=1970 & year<=2002)
dim(data)

data <- na.omit(data)
dim(data)


# generate new DVs
summary(data$pursuitnuk)

data$pursuitnuk[data$acquirenuk==1] <- NA
summary(data$pursuitnuk)

summary(data$CBWpursuit)

data$CBWpursuit[data$CBWpossess==1] <- NA
summary(data$CBWpursuit)






# -------------------------#
# lagged DV version
# -------------------------#

x1 <- subset(data, select=c(rival_threat_level_new, polity2, Oil,cinc, lmtnest, mideast, US_ally, Great_Britain_ally, Russia_ally, France_ally, China_ally))

K1 <- ncol(x1)

# SELECT FIRST DV
#y1 <- data$defense_allies
#y1 <- log(data$defense_allies + 1)
#y1 <- data$pursuitnuk
#y1 <- data$CBWpursuit
#y1 <- data$acquirenuk
y1 <- data$CBWpossess


x2 <- subset(data, select=c(ln_milper, rival_threat_level_new, polity2, Oil, cinc, lmtnest, mideast, US_ally, Great_Britain_ally, Russia_ally, France_ally, China_ally))

# SELECT SECOND DV
y2 <- data$ln_effective_number
#y2 <- data$min_regime

K2 <- ncol(x2)


#
MakeInits <- function(){
    BETA1 <- runif(K1)
    ALPHA1 <- runif(1)

    BETA2 <- runif(K2)
    ALPHA2 <- runif(1)

    RHO <- runif(1,-1,1)

    out <- list(alpha1=ALPHA1, beta1=BETA1, alpha2=ALPHA2, beta2=BETA2, rho=RHO)
    return(out)
}
inits1 <- MakeInits()
inits2 <- MakeInits()
inits3 <- MakeInits()

inits.function <- function(chain){
    return(switch(chain,
    "1"=inits1,
    "2"=inits2,
    "3"=inits3
    ))
}


year <- data$year - 1970 + 1


#b0 <- c(0,0)
#B0 <- diag(1,2)


# jags.model arguments
ADAPT <- 1000
BURNIN <- 500
DRAWS <- 1000
THIN <- 10
CHAINS <- 3
NAME <- "recouping_SEM_Appendix_Rcode.bug"

dim(x1)
dim(x2)

#rjags code version
m <- jags.model(file=NAME, data=list("y1"=y1, "x1"=x1, "y2"=y2, "x2"=x2, "n"=length(y1), "K1"=K1, "K2"=K2), inits=inits.function, n.chains=CHAINS, n.adapt=ADAPT)

update(m, BURNIN)

draws <- coda.samples(m, variable.names=c("alpha1", "alpha2", "beta1", "beta2", "sigma1", "sigma2", "tau1", "tau2", "rho"), n.iter=DRAWS, progress.bar="text", thin=10)

mat1 <- as.matrix(draws[[1]])
mat2 <- as.matrix(draws[[2]])
mat3 <- as.matrix(draws[[3]])
posterior.estimates <- rbind(mat1, mat2, mat3)
vars <- t(as.matrix(posterior.estimates))
parameter.mean <- apply(vars, 1, mean)
parameter.median <- apply(vars, 1, median)
parameter.sd <- apply(vars, 1, sd)
parameter.lower.ci <- apply(vars, 1, quantile, c(0.025))
parameter.upper.ci <- apply(vars, 1, quantile, c(0.975))

# print the parameter estimates to the screen
cbind(parameter.mean, parameter.sd, parameter.lower.ci, parameter.upper.ci)

#xtable(cbind(parameter.mean, parameter.sd, parameter.lower.ci, parameter.upper.ci), digits=3)



