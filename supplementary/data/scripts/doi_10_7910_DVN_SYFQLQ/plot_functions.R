## define a function to run fixed-effect model with standard errors clustered by group
MLMs_fit <- function(dvs, ivs, data){
       # load R pacakges      
       library(lme4)
       library(arm); library(purrr)
       library(ggplot2); library(ggthemes); library(viridis)
       library(ggridges); library(purrr); library(dplyr)
       # empty list to store model fit
       Fits <- list()
       # for loop to run all models
       
              # make a model formula
              f <- as.formula(paste(paste(dvs, paste(ivs, collapse=" + "), sep=" ~ "), paste("+(", Cov1[1], "|democracy_dd_bd_lag)+(1|year)+(1|ccode)", sep = "")))
              # set REML=FALSE option, it will use the optimization of the log-likelihood
              fit <- lmer(f, REML=FALSE, data = data)
              
       return(fit)
       
}

## define a linear function
MLs_fit <- function(dvs, ivs, data){
       # load R pacakges      
       Fits <- list()
       # for loop to run all models
       
              # make a model formula
              f <- as.formula(paste(paste(dvs, paste(ivs, collapse=" + "), sep=" ~ ")))
              # set REML=FALSE option, it will use the optimization of the log-likelihood
              fit <- lm(f,data = data)
              
       return(fit)
       
}

## MLM with two varying slopes without regime-specific slope
MLMs_Inter_fit <- function(dvs, ivs, data){
       # load R pacakges      
       library(lme4)
       library(arm); library(purrr)
       library(ggplot2); library(ggthemes); library(viridis)
       library(ggridges); library(purrr); library(dplyr)
       # empty list to store model fit
       Fits <- list()
       # for loop to run all models
       
              # make a model formula
              f <- as.formula(paste(paste(dvs, paste(ivs, collapse=" + "), sep=" ~ "), paste("+(1|year)+(1|ccode)", sep = "")))
              # set REML=FALSE option, it will use the optimization of the log-likelihood
              fit <- lmer(f, REML=FALSE, data = data)
              
       return(fit)
       
}

## MLM with three varying slopes including regime
MLMs_Inter_fit2 <- function(dvs, ivs, data){
       # load R pacakges      
       library(lme4)
       library(arm); library(purrr)
       library(ggplot2); library(ggthemes); library(viridis)
       library(ggridges); library(purrr); library(dplyr)
       # empty list to store model fit
       Fits <- list()
       # for loop to run all models
       
              # make a model formula
              f <- as.formula(paste(paste(dvs, paste(ivs, collapse=" + "), sep=" ~ "), paste("+(1|year)+(1|ccode)+(1|democracy_dd_bd)", sep = "")))
              # set REML=FALSE option, it will use the optimization of the log-likelihood
              fit <- lmer(f, REML=FALSE, data = data)
              
       return(fit)
       
}


## plot function
get.democracy <- function(x, i=2){
yname <- colnames(se.coef(x)$democracy)
b.hat.M1 <- coef(x)[[3]][,i] 
b.se.M1 <- sqrt(se.ranef(x)[[3]][,i]^2 + se.fixef(x)[i]^2)
lower <- b.hat.M1 - 1.96*b.se.M1
upper <- b.hat.M1 + 1.96*b.se.M1

  est_beta_mean <- b.hat.M1
  est_beta_ci <- cbind(lower, upper)
  data <- cbind.data.frame(est_beta_mean, est_beta_ci)
  names(data) <- c("mean", "ci_l", "ci_u")
  data$id <-  c("M2.aut.", "M2.dem.")
  return (data)
}

plot.democracy <- function(x, i=2){
yname <- colnames(se.coef(x)$democracy)
b.hat.M1 <- coef(x)$democracy[,i] 
b.se.M1 <- se.coef(x)$democracy[,i]
lower <- b.hat.M1 - 1.96*b.se.M1
upper <- b.hat.M1 + 1.96*b.se.M1

  est_beta_mean <- b.hat.M1
  est_beta_ci <- cbind(lower, upper)
  data <- cbind.data.frame(est_beta_mean, est_beta_ci)
  names(data) <- c("mean", "ci_l", "ci_u")
  data$id <- c("non-democracy", "democracy")

        p <- ggplot(data, aes(id, mean)) 
        p <- p + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50")
        p <- p +geom_point() + geom_linerange(aes(ymax = ci_u, ymin = ci_l))

        p <- p + xlab(" ") + ylab(expression(beta[j]))
        p
}

get.coef <- function(x, i){
    b.hat.M1 <- fixef(x)[i] 
    b.se.M1 <-sqrt(diag(vcov(x)))[i]
    lower <- b.hat.M1 - 1.96*b.se.M1
    upper <- b.hat.M1 + 1.96*b.se.M1

  est_beta_mean <- b.hat.M1
  est_beta_ci <- cbind(lower, upper)
  data <- cbind.data.frame(est_beta_mean, est_beta_ci)
  names(data) <- c("mean", "ci_l", "ci_u")
    return (data)
    }
    
## plot function
plot.democracy.int <- function(x){
yname <- colnames(se.coef(x)$democracy_dd_bd_lag)
b.hat.M1 <- coef(x)$democracy[,1] 
b.se.M1 <- se.coef(x)$democracy[,1]
lower <- b.hat.M1 - 1.96*b.se.M1
upper <- b.hat.M1 + 1.96*b.se.M1

  est_beta_mean <- b.hat.M1
  est_beta_ci <- cbind(lower, upper)
  data <- cbind.data.frame(est_beta_mean, est_beta_ci)
  names(data) <- c("mean", "ci_l", "ci_u")
  data$id <- c("M3.aut.", "M3.dem.")

        p <- ggplot(data, aes(id, mean)) 
        p <- p + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50")
        p <- p +geom_point() + geom_linerange(aes(ymax = ci_u, ymin = ci_l))

        p <- p + xlab(" ") + ylab(expression(alpha[j]))
        p
}


# to be added
plot.regime <- function(x, i=2){
yname <- colnames(se.coef(x)$regime1)
b.hat.M1 <- coef(x)$regime1[,i] 
b.se.M1 <- se.coef(x)$regime1[,i]
lower <- b.hat.M1 - 1.96*b.se.M1
upper <- b.hat.M1 + 1.96*b.se.M1

  est_beta_mean <- b.hat.M1
  est_beta_ci <- cbind(lower, upper)
  data <- cbind.data.frame(est_beta_mean, est_beta_ci)
  names(data) <- c("mean", "ci_l", "ci_u")
  data$id <- c("democracy", "civildic", "military", "monarchy")

        p <- ggplot(data, aes(id, mean)) 
        p <- p + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50")
        p <- p +geom_point() + geom_linerange(aes(ymax = ci_u, ymin = ci_l))

        p <- p + xlab(" ") + ylab(expression(beta[j]))
        p
}



## get interaction term marginal effects
get.democracy.interact <- function(M){
    k <- length(fixef(M))
    b.hat.M1 <-  c(fixef(M)[3], (fixef(M)[3]+fixef(M)[k]))
b.se.M1 <- c(sqrt(vcov(M)[3,3]), sqrt(vcov(M)[3,3] + vcov(M)[k,k] + 2*vcov(M)[3,k]))
lower <- b.hat.M1 - 1.96*b.se.M1
upper <- b.hat.M1 + 1.96*b.se.M1

  est_beta_mean <- b.hat.M1
  est_beta_ci <- cbind(lower, upper)
  data <- cbind.data.frame(est_beta_mean, est_beta_ci)
  names(data) <- c("mean", "ci_l", "ci_u")
  data$id <- c("M3.aut.", "M3.dem.")
  return (data)
}

## plot function
plot.inter <- function(M){ 
b.hat.M1 <-  c(fixef(M)[2], (fixef(M)[2]+fixef(M)[16]))
b.se.M1 <- c(sqrt(vcov(M)[2,2]), sqrt(vcov(M)[2,2] + vcov(M)[16,16] + 2*vcov(M)[2,16]))
lower <- b.hat.M1 - 1.96*b.se.M1
upper <- b.hat.M1 + 1.96*b.se.M1

  est_beta_mean <- b.hat.M1
  est_beta_ci <- cbind(lower, upper)
  data <- cbind.data.frame(est_beta_mean, est_beta_ci)
  names(data) <- c("mean", "ci_l", "ci_u")
  data$id <- c("non-democracy", "democracy")

        p <- ggplot(data, aes(id, mean)) 
        p <- p + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50")
        p <- p +geom_point() + geom_linerange(aes(ymax = ci_u, ymin = ci_l))

        p <- p + xlab(" ") + ylab(expression(alpha[j]))
        p
}


## plot function
plot.year <- function(x){
yname <- rownames(se.coef(x)$year)
b.hat.M1 <- ranef(x)$year[,1] 
b.se.M1 <- se.coef(x)$democracy[,1]
lower <- b.hat.M1 - 1.96*b.se.M1
upper <- b.hat.M1 + 1.96*b.se.M1

  est_beta_mean <- b.hat.M1
  est_beta_ci <- cbind(lower, upper)
  data <- cbind.data.frame(est_beta_mean, est_beta_ci)
  names(data) <- c("mean", "ci_l", "ci_u")
  data$id <- 1982:2010

        p <- ggplot(data, aes(id, mean)) 
        p <- p + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50")
        p <- p +geom_point() + geom_linerange(aes(ymax = ci_u, ymin = ci_l))

        p <- p + xlab(" ") + ylab(expression(alpha[t]))
        p
}

plot.country <- function(x){
yname <- rownames(se.coef(x)$wdicode)
b.hat.M1 <- ranef(x)$wdicode[,1] 
b.se.M1 <- se.coef(x)$wdicode[,1]
lower <- b.hat.M1 - 1.96*b.se.M1
upper <- b.hat.M1 + 1.96*b.se.M1

  est_beta_mean <- b.hat.M1
  est_beta_ci <- cbind(lower, upper)
  data <- cbind.data.frame(est_beta_mean, est_beta_ci)
  names(data) <- c("mean", "ci_l", "ci_u")
  data$id <- 1: length(yname)

        p <- ggplot(data, aes(id, mean)) 
        p <- p + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50")
        p <- p +geom_point() + geom_linerange(aes(ymax = ci_u, ymin = ci_l))

        p <- p + xlab(" ") + ylab(expression(alpha[i]))
        p
}

