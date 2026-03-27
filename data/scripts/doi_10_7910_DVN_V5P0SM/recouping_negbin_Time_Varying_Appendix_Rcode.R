# Please note that these model take a considerable amount of time to run.


# SET SPECIFICATION TO 1,2,3, or 4 (SEE DETAILS BELOW)
SPECIFICATION <- 1
#SPECIFICATION <- 2
#SPECIFICATION <- 3
#SPECIFICATION <- 4


# load libraries
library(foreign); library(rjags); library(coda)


# load data
data <- read.dta("RECOUPING_DATA_FOR_PUBLICATION.dta")


# subset data for analysis
data <- subset(data, select=c(year, ccode, defense_allies, effective_number, min_regime, rival_threat_level_new, polity2, Oil,cinc, lmtnest, mideast, US_ally, Great_Britain_ally, Russia_ally, France_ally, China_ally))
dim(data)


# generate variables consistent with the STATA code
data$ln_effective_number <- log(data$effective_number)
data$coup_proof_interact <-  data$min_regime*data$ln_effective_number

# create lagged variables
lags <- subset(data, select=c(year, ccode, defense_allies))
lags$year <- lags$year + 1
names(lags) <- c("year", "ccode", "defense_allies_lag")

# merge data with lagged data
data <- merge(data, lags, by=c("ccode", "year"), all.x=TRUE, all.y=FALSE)
dim(data)

# subset time period
data <- subset(data, year>=1970 & year<=2002)
dim(data)

data <- na.omit(data)
dim(data)






# -------------------------#
# varying intercept version
# -------------------------#

data$id <- as.numeric(as.factor(data$ccode))
data$time <- as.numeric(as.factor(data$year))

id <- data$id
J <- max(unique(id))

time <- data$time
T <- max(unique(time))


specification01 <- subset(data, select=c(ln_effective_number, rival_threat_level_new, polity2, Oil,cinc, lmtnest, mideast, US_ally, Great_Britain_ally, Russia_ally, France_ally, China_ally))

specification02 <- subset(data, select=c(min_regime, rival_threat_level_new, polity2, Oil,cinc, lmtnest, mideast, US_ally, Great_Britain_ally, Russia_ally, France_ally, China_ally))

specification03 <- subset(data, select=c(ln_effective_number, min_regime, rival_threat_level_new, polity2, Oil,cinc, lmtnest, mideast, US_ally, Great_Britain_ally, Russia_ally, France_ally, China_ally))

specification04 <- subset(data, select=c(ln_effective_number, coup_proof_interact, min_regime, rival_threat_level_new, polity2, Oil,cinc, lmtnest, mideast, US_ally, Great_Britain_ally, Russia_ally, France_ally, China_ally))

x1 <- specification01
x2 <- specification02
x3 <- specification03
x4 <- specification04

if(SPECIFICATION==1)x <- x1
if(SPECIFICATION==2)x <- x2
if(SPECIFICATION==3)x <- x3
if(SPECIFICATION==4)x <- x4
K <- ncol(x)


# set y variable to pass to JAGS
y <- data$defense_allies


MakeInits <- function(){
    BETA <- runif(K)
    ALPHA <- runif(T)
    RATE <- runif(1)
    SIGMA <- runif(1)
    
    out <- list(alpha=ALPHA, beta=BETA, rate=RATE, sigma=SIGMA)
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


# jags.model arguments
ADAPT <- 1000
BURNIN <- 100 # make this number at least 10000 for inference (small numbers are for testing)
DRAWS <- 1000 # make this number at least 10000 for inference (small numbers are for testing)
THIN <- 10
CHAINS <- 3
if(SPECIFICATION==1)NAME <- "recouping_negbin_Time_Varying_Appendix_Rcode_SPEC01.bug"
if(SPECIFICATION==2)NAME <- "recouping_negbin_Time_Varying_Appendix_Rcode_SPEC02.bug"
if(SPECIFICATION==3)NAME <- "recouping_negbin_Time_Varying_Appendix_Rcode_SPEC03.bug"
if(SPECIFICATION==4)NAME <- "recouping_negbin_Time_Varying_Appendix_Rcode_SPEC04.bug"

#rjags code version
m <- jags.model(file=NAME, data=list("y"=y, "x"=x, "n"=length(y), "K"=K, "T"=T, "time"=time), inits=inits.function, n.chains=CHAINS, n.adapt=ADAPT)
m
update(m, BURNIN)
draws <- coda.samples(m, variable.names=c("alpha", "beta", "rate", "sigma"), n.iter=DRAWS, progress.bar="text", thin=10)


# combine estimates for inference
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



