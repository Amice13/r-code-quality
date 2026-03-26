# *********************************************************************
# Replication code for Systemic Discrimination Among Large U.S. Employers
# Patrick M. Kline, Evan K. Rose, Christopher R. Walters
# April, 2022

# This code produces Figure 12, which shows lower bounds on the prev-
# alence of job-level discrimination 

# It must be run in the top level of the replication archive directory.
# *********************************************************************

library(haven)
library(tidyverse)
library(qvalue)
library(ggtext)
library(ggplot2)
library(gtools)
library(deconvolveR)
library(locfit)
options(dplyr.summarise.inform = FALSE)

# Attribute to study
tostudy <- "white"

# Load up the data
df <- read_dta(file='data/data.dta')
df <- mutate(df, split = order >= 5)

# Subset appropriately and create covars
df <- df %>% group_by(job_id,pair) %>% 
  mutate(napps = n(),
         nblack = sum(black)) %>% ungroup()
df <- df %>% filter(napps != 2 | nblack == 1)

# Drop jobs without a split
df <- df %>% group_by(job_id) %>% mutate(napps = n()) %>% filter(napps >= 6)

# Function to calculate average cross products from two vectors
crossprod <- function(x,y,w=NULL) {
  # Take all products
  if (is.null(w)) {
    w <- rep(1, length(x))
  }
  prods <- as.matrix(x*w)%*%t(as.matrix(y*w))
  wprod <- as.matrix(w)%*%t(as.matrix(w))
  
  # Sum and average
  return((sum(prods) - sum(diag(prods)))/(sum(wprod) - sum(diag(wprod))))
}

# Function to estimate firm effects and unbiased SEs
ttester <- function(fid) {
  tmp <- means %>% filter(firm_id == fid)
  test <- t.test(tmp[["1"]],tmp[["0"]], paired=TRUE)
  p <- test$p.value
  se <- test$stderr
  est <- test$estimate
  p_greater <- t.test(tmp[["1"]],tmp[["0"]], paired=TRUE, alternative="greater")$p.value
  p_less <- t.test(tmp[["1"]],tmp[["0"]], paired=TRUE, alternative="less")$p.value
  return(cbind(firm_id=fid, est=est, se=se, ttest_p=p, ttest_p_greater=p_greater, ttest_p_less=p_less))
}

# Estimate firm effects
means <- df %>% group_by(firm_id, job_id, .data[[tostudy]]) %>% summarize(mean_cb=mean(cb))
means <- means %>% spread(.data[[tostudy]],mean_cb) %>% drop_na()
firm_effects <- as.data.frame(do.call(rbind,lapply(unique(df$firm_id),FUN=ttester)))
sdest <- sqrt(var(firm_effects$est) - mean(firm_effects$se^2))

# Get estimate of E[mu_{jf}^2] for each firm
muf2 <- function(fid,w=NULL) {
  if (is.null(fid)) {
    jobdata <- df
  } else {
  jobdata <- df %>% filter(firm_id == fid)
  }

  splits <- jobdata %>% group_by(job_id, split, .data[[tostudy]]) %>%
  summarize(mean_cb=mean(cb)) %>% spread(.data[[tostudy]],mean_cb) %>%
  mutate(gap = `1` - `0`) %>% drop_na() %>% select(job_id,split,gap) %>%
  spread(split,gap) %>% drop_na()
  
  if (is.null(w)) {
    w <- rep(1, nrow(splits))
  } else {
    w <- rexp(nrow(splits))
  }
  
  prods <- sum(w*splits$`FALSE`*splits$`TRUE`)
  nj <- sum(w)
  return(cbind(mujf2=prods/nj))
}

# Get estimate of firm squares
firm_squares <- as.data.frame(do.call(rbind,lapply(unique(df$firm_id),FUN=muf2)))
boot_sqares <- sapply(rep(1, 2), function(x) {
  as.data.frame(do.call(rbind,lapply(unique(df$firm_id),FUN=muf2, w=1)))    
})

bootwrapper <- function(x) {
  print(x)
  firm_squares <- as.data.frame(do.call(rbind,lapply(unique(df$firm_id),FUN=muf2, w=1)))
  return(mean(firm_squares$mujf2))
}

bootmu2 <- function(fid) {
  bootest <- sapply(rep(1, 10), bootwrapper)
  sqvar <- var(bsreps)
  q05 <- quantile(bsreps,0.05,na.rm=TRUE)
  q95 <- quantile(bsreps,0.95,na.rm=TRUE)
  return(cbind(sqvar=sqvar,q05=q05,q95=q95))
}

# Function to calculate pi0 bound for given set of jobs
pibound <- function(jobdata,w=NULL,constrain=FALSE) {
  # First we estimate the squared mean with the usual u-stat tricks
  jobs <- jobdata %>% group_by(job_id, .data[[tostudy]]) %>%
    summarize(mean_cb=mean(cb)) %>% spread(.data[[tostudy]],mean_cb) %>%
    mutate(gap = `1` - `0`) %>% drop_na()
  if (is.null(w)) {
    w <- rep(1, nrow(jobs))
  }
  mu2 <- crossprod(jobs$gap,jobs$gap,w=w)
  mu <- sum(w*jobs$gap)/sum(w)
  
  # Now we estimate sigma_f squared with split pairs
  splits <- jobdata %>% group_by(job_id, split, .data[[tostudy]]) %>%
    summarize(mean_cb=mean(cb)) %>% spread(.data[[tostudy]],mean_cb) %>%
    mutate(gap = `1` - `0`) %>% drop_na() %>% select(job_id,split,gap) %>%
    spread(split,gap) %>% drop_na()
  prods <- sum(w*splits$`FALSE`*splits$`TRUE`)
  nj <- sum(w)
  # var <- (nj-1)/nj * (prods/nj - mu2)
  # return(mu2/(var+mu2))
  pb <- mu2/(prods/nj)
  svb <- (prods/nj)/mu
  if (constrain) {
    pb <- max(min(pb,1),0)
    svb <- max(min(svb,1),-1)
  }
  return(list(pb,svb))
}

piwrapper <- function(fid,constrain=FALSE) {
  pb <- res[[1]]
  svb <- res[[2]]
  return(cbind(firm_id=fid,pibound=pb,svbound=svb))
}

piwrapper_cons <- function(fid,constrain=TRUE) {
  jobdata <- df %>% filter(firm_id == fid)
  res <- pibound(jobdata,constrain=constrain)
  pb <- res[[1]]
  svb <- res[[2]]
  return(cbind(firm_id=fid,picons=pb,svcons=svb))
}

bootpi <- function(fid) {
  print(paste("Working on firm",fid))
  jobdata <- df %>% filter(firm_id == fid)
  res <- pibound(jobdata)
  pb <- res[[1]]
  svb <- res[[2]]
  bsreps <- sapply(rep(1, 500), function(x) {pibound(jobdata,w=rexp(length(unique(jobdata$job_id))))[[1]]})
  pivar <- var(bsreps)
  q05 <- quantile(bsreps,0.05,na.rm=TRUE)
  q95 <- quantile(bsreps,0.95,na.rm=TRUE)
  return(cbind(firm_id=fid,pibound=pb,severbound=svb,pivar=pivar,q05=q05,q95=q95))
}

# Add qvalues
if (tostudy == "white") {
  qobj <- qvalue(firm_effects$ttest_p_greater, pi0.method="bootstrap")
} else {
  qobj <- qvalue(firm_effects$ttest_p, pi0.method="bootstrap")
}
firm_effects$qvalue = qobj$qvalues

# Make graphs for range of q value thresholds
qthresh <- function(q) {
  res <- df %>% filter(firm_id %in% firm_effects[firm_effects$qvalue <= q, 'firm_id']) %>%
    pibound()
  return(res[[1]])
}

# Qs for a sequence of values
qs <- seq(0.01, 0.5, by=0.01)
res <- sapply(qs,qthresh)

# Add pi0 bounds
cons_bounds <- as.data.frame(do.call(rbind,lapply(firm_effects$firm_id,FUN=piwrapper_cons)))
firm_effects <- firm_effects %>% left_join(cons_bounds, by="firm_id")

# Make the graph
ggplot() + geom_point(aes(x=firm_effects$qvalue, y=firm_effects$picons)) + 
  geom_line(aes(x=qs, y=res)) + 
  theme_minimal() + theme(text = element_text(size=25)) +
  labs(x = "Firm q-value", y = "Lower bound on share of jobs discriminating")
ggsave("figures/figure12.pdf",
      width = 16, height = 12)



