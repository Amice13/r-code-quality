# *********************************************************************
# Replication code for Systemic Discrimination Among Large U.S. Employers
# Patrick M. Kline, Evan K. Rose, Christopher R. Walters
# April, 2022

# This code produce deconvolution estimates of population contact gaps
# for race without imposing the restriction that no firms discriminate
# against white applicants.

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

leftlim <- min(zf)-0.5
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

### Figure 12a: Deconvolution estimates of firm-level discrimination distribution without support restriction
plt <- ggplot() + geom_line(mapping = aes(x = betax, y = betay),
                            color = "blue", size=2) + 
  geom_histogram(mapping=aes(x=firm_effects$est,y=..density..),
                 color="red", fill="red", alpha=0.3, binwidth=0.01, center=0) +
  labs(x = xlab, y = "Density") +
  annotate("text", label=paste("\n   Implied firm mean\n     gap:",
                               format(sum(betax*betay)/sum(betay), digits=3),
                               "\n   Implied between\n     firm SD:",
                               format((sum(betax**2*betay)/sum(betay)-
                                         (sum(betax*betay)/sum(betay))**2)**0.5, digits=3)),
           size=8, x=-Inf, y=Inf,hjust=0,vjust=1) +
  theme_minimal() + xlim(xlb, xub) +
  theme(text = element_text(size=25))

plt <- plt + annotate("segment", x = -0.04, xend = -0.025, y = 8, yend = 6, size=1, alpha=0.8, arrow=arrow()) + 
  geom_text(aes( x=-0.04, y=9, label="Observed gaps"), size=5) + 
  annotate("segment", x = 0.045, xend = 0.028, y = 23, yend = 20, size=1, alpha=0.8, arrow=arrow()) + 
  geom_text(aes(  x=0.065, y=24, label="Deconvolved density"), size=5)     
ggsave(paste("figures/figureA12.pdf",sep=""), 
       width = 10, height = 8, dpi = 120)


