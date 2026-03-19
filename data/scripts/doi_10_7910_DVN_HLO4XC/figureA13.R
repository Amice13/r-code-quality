# *********************************************************************
# Replication code for Systemic Discrimination Among Large U.S. Employers
# Patrick M. Kline, Evan K. Rose, Christopher R. Walters
# April, 2022

# This code estimates the predictive power of posterior mean contact gaps
# from waves 1-3 of the experiment for contact gaps in waves 4-5 for 
# the sample of firms present in all five waves of the experiement.

# It must be run in the top level of the replication archive directory.
# *********************************************************************

# Load library
library(haven)
library(tidyverse)
library(qvalue)
library(ggtext)
library(ggplot2)
library(gtools)
library(deconvolveR)
library(ExtDist)
library(Matrix)
library(numDeriv)
library(PoissonBinomial)
library(scales)
library(miceadds)


# Difference to study
tostudy = "white"
pd <- 5
divider <- 1/40
xlab <- "Firm white-Black contact rate gap"
ylab <- "Posterior mean white-Black contact rate gap"
leg_pos <- c(0.8, 0.9)
xlb <- -0.08
xub <- 0.15

# Load up the data
df <- read_dta(file='data/data.dta')

# Drop apps sent out of order (~170 total)
df <- df %>% group_by(job_id,pair) %>% 
  mutate(napps = n(),
         nblack = sum(black)) %>% ungroup()
df <- df %>% filter(napps != 2 | nblack == 1)

# Restrict to balanced sample
df <- df %>% group_by(firm_id) %>% mutate(nwaves = n_distinct(wave)) %>% filter(nwaves == 5)

# Fit on waves 1-3 first
full_df <- df
df <- full_df %>% filter(wave <= 3)

# Simple difference in means se and p-values with correct DOF
means <- df %>% group_by(firm_id, job_id, .data[[tostudy]]) %>% summarise(mean_cb=mean(cb))
means <- means %>% spread(.data[[tostudy]],mean_cb) %>% drop_na()

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

firm_effects <- as.data.frame(do.call(rbind,lapply(unique(df$firm_id),FUN=ttester)))
first = mean(firm_effects$est)
second = (1/(nrow(firm_effects))*sum((firm_effects$est - first)**2) -
            (nrow(firm_effects)-1)/(nrow(firm_effects)**2)*sum(firm_effects$se**2))


# Set up objects to feed into Efron procedure
firm_effects$t_stat <- firm_effects$est / firm_effects$se
zf <- firm_effects$est / firm_effects$se
ses <- firm_effects$se

# Efron density estimation parameters
width <- .0001
ntaus <- 5000

leftlim <- 0
rightlim <- max(zf)+0.5
betax <- seq(-0.05,max(zf)*max(firm_effects$se),by=width)
blacklim <- c(-0.05,0.08)
tauwidth = (rightlim - leftlim)/ntaus
mus <- seq(leftlim, rightlim, by=tauwidth)

# Generate beta density
deltadens <- function(delta, mudens) {
  support <- delta/firm_effects$se
  fz <- sapply(support, function(x) {
    if (x < min(mus)) {
      return(0) # assume density is zero outside support for mus
    }
    else if (x > max(mus)) {
      return(0) # assume density is zero outside support for mus
    }
    return(mudens[which(abs(mus-x)==min(abs(mus-x)))])
  })
  inside <- fz/firm_effects$se
  dens <- sum(inside)/length(firm_effects$se)
  return(dens)
}

# Return G
get_g <- function(c0) {
  res <- deconv(tau=mus, X=firm_effects$est/firm_effects$se, family="Normal",
                c0=c0, pDegree=pd) 
  mudens <- res$stats[, "g"]
  betay <- sapply(betax, deltadens, mudens=mudens)
  return(list(res=res,betay=betay))
}

# Tune Efron regularization parameter
find_reg = function(c0) {
  betay <- get_g(c0)$betay
  implied_sd <- (sum(betax**2*betay)/sum(betay)-
                   (sum(betax*betay)/sum(betay))**2)
  gap = (implied_sd - second)**2*1000
  print(c0)
  print(gap)
  gap
}
o = optimize(find_reg, interval=c(0, .8))
reg = o$minimum[[1]]

# Efron density
res <- get_g(reg)
betay <- res$betay
betay_sum <- sum(betay)
betay <- betay / betay_sum / width # make beta density integrate to 1
implied_mean <- sum(betax*betay)/sum(betay)
implied_var <- (sum(betax**2*betay)/sum(betay)-
                  (sum(betax*betay)/sum(betay))**2)
implied_sd <- implied_var**0.5

# Discrete density of mus
result <- res$res
mudens <- result$stats[, "g"] / tauwidth
mus <- result$stats[, "theta"]

# Posterior for mu given each t-stat / z score
postmean <- function(z) {
  postdens <- dnorm(z-mus)*mudens
  return(sum(postdens*mus)/sum(postdens))
}

firm_effects$post_mean_u <- sapply(zf, postmean)
firm_effects$post_mean_beta <- firm_effects$post_mean_u*firm_effects$se

# Compare to observed gaps in waves 4-5
df <- full_df %>% filter(wave >= 4)

means <- df %>% group_by(firm_id, job_id, .data[[tostudy]]) %>% summarize(mean_cb=mean(cb))
means <- means %>% spread(.data[[tostudy]],mean_cb) %>% drop_na()

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

lastwaves <- as.data.frame(do.call(rbind,lapply(unique(df$firm_id),FUN=ttester)))
first = mean(lastwaves$est)
second = (1/(nrow(lastwaves))*sum((lastwaves$est - first)**2) -
            (nrow(lastwaves)-1)/(nrow(lastwaves)**2)*sum(lastwaves$se**2))

# Merge on posterior means
tmp <- firm_effects %>% select(firm_id, post_mean_beta) %>% inner_join(
        lastwaves, by='firm_id')

# Regresss and graph
lm_eqn <- function(df){
  m <- lm(est ~ post_mean_beta, df);
  eq <- substitute(beta == b~(c) *","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        c = format(unname(summary(m)$coefficients[2,2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

ggplot() + geom_point(aes(x=tmp$post_mean_beta, y=tmp$est)) +
    geom_line(aes(x=tmp$post_mean_beta,y=tmp$post_mean_beta), linetype="dashed") +
   geom_smooth(method='lm', formula='y~x', aes(y=tmp$est, x=tmp$post_mean_beta), se=FALSE) +
  theme_minimal() +
  labs(x="Posterior mean gap waves 1-3", y="Contact gap in waves 4-5") + 
  theme(text = element_text(size=25)) +
  geom_text(aes(x = .0075, y = 0.11), label = lm_eqn(tmp), parse = TRUE, size=5)
ggsave(paste("figures/figureA13.pdf",sep=""),
      width = 16, height = 12)

