rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

## load required packages
library(data.table)
library(zoo)
library(ggplot2)
library(dplyr)
library(devtools)
devtools::install_local('./code/auxiliary packages/gsynth/gsynth', force=TRUE)
library(gsynth)

## **NOTE** Takes about 20hrs to run on a MacBook Pro Core i7 16GB ##

## load data
hate_crimes <- readRDS( './data/crime/hate_crime_did_analysis.RDS')

# define time, id variables
hate_crimes[ , time:=round(((as.yearmon(hate_crimes$month, format = '%m-%Y') - as.yearmon('2014-04'))*12)+1)]
hate_crimes[, id:=.GRP, by=.(PCON17CD)]


### Figure G8: First time ethnic minority victory effects on hate crime, varying margin of victory

# compute functions for estimation
conf.lvl = 0.95
get.pvalue <- function(vec) {
  if (NaN%in%vec|NA%in%vec) {
    nan.pos <- is.nan(vec)
    na.pos <- is.na(vec)
    pos <- c(which(nan.pos),which(na.pos))
    vec.a <- vec[-pos]
    a <- sum(vec.a >= 0)/(length(vec)-sum(nan.pos|na.pos)) * 2
    b <- sum(vec.a <= 0)/(length(vec)-sum(nan.pos|na.pos)) * 2  
  } else {
    a <- sum(vec >= 0)/length(vec) * 2
    b <- sum(vec <= 0)/length(vec) * 2  
  }
  return(min(as.numeric(min(a, b)),1))
}

# compute avg att and CI's at t = 1:10
align_sequences <- function(post, eff, num_periods) {
  first_treated_period <- apply(post,FUN=function(x) min(which(x)), 2)
  last_first_treated_period <- max(first_treated_period)
  first_first_treated_period <- min(first_treated_period)
  n_periods <- nrow(post)
  post_aligned <- matrix(nrow=n_periods+last_first_treated_period-first_first_treated_period, ncol=length(first_treated_period))
  eff_aligned <- matrix(nrow=n_periods+last_first_treated_period-first_first_treated_period, ncol=length(first_treated_period))
  
  for( i in seq(1, ncol(post))) {
    post_aligned[, i] <- c(
      rep(NA, last_first_treated_period- first_treated_period[i] ), 
      post[, i], 
      rep(NA, first_treated_period[i]-first_first_treated_period )
    ) 
    
    eff_aligned[, i] <- c(
      rep(NA, last_first_treated_period- first_treated_period[i] ), 
      eff[, i], 
      rep(NA, first_treated_period[i]-first_first_treated_period )
    )  
  }
  
  #min_treated_periods <- n_periods - last_first_treated_period
  
  min_treated_periods <- num_periods
  att <- sum(
    eff_aligned[last_first_treated_period:(last_first_treated_period+min_treated_periods),] * post_aligned[last_first_treated_period:(last_first_treated_period+min_treated_periods),]
  ) / sum( post_aligned[last_first_treated_period:(last_first_treated_period+min_treated_periods),])
  
  return(list(eff_aligned=eff_aligned, post_aligned=post_aligned, att=att))
}

estimates_list <- vector(mode='list', length = length(seq(22,46,by=4)))

for (r in seq(22,46,by=4)) {
  
  hate_crimes_sub <- hate_crimes[victory_margin<r]
  p <- which(r==seq(22,46,by=4))
  
  ## Estimation
  ## interactive fixed effects
  out <- gsynth(crime_rate ~ treat, data = hate_crimes_sub, index = c("id","time"), force = "two-way",
                CV = TRUE, se= TRUE, seed = 111, nboots = 1000, inference = "parametric", parallel = FALSE)
  
  att_avg <- vector(mode = 'list', length = length(seq(1,9)))
  for (i in seq(1,9)) {
    relevant_period_att <- align_sequences(out$post, out$eff, i-1)
    att_avg[[i]] <- relevant_period_att$att
  }
  
  
  #att.boot <- matrix(0, dim(out$all_boots[[1]]$eff[tp_ix,])[1], nboots)
  #att.avg.boot<-matrix(0,nboots,1)
  CI.avg <- vector(mode = 'list', length = length(seq(1,9))) 
  se.avg <- vector(mode = 'list', length = length(seq(1,9))) 
  pvalue.avg <- vector(mode = 'list', length = length(seq(1,9)))
  att.avg <- vector(mode = 'list', length = length(seq(1,9)))
  
  nboots <- length(out$all_boots)
  
  for (i in seq(1,9)) {
    att <- vector(mode='list', length=nboots)
    for (j in 1:nboots) {
      boot_rep <- out$all_boots[[j]]
      Y <- boot_rep$Y.boot
      I <- boot_rep$I.boot
      D <- boot_rep$D.boot
      TT <-dim(Y)[1]
      ## treatement indicator
      tr <- D[TT,] == 1  ## cross-sectional: treated unit
      I.tr <- as.matrix(I[, tr])
      if (!0 %in% I.tr) {
        boot_rep$post <- as.matrix(D[,which(tr == 1)] == 1)   
      } else {
        boot_rep$post <- as.matrix(D[,which(tr==1)] == 1 & I[,which(tr==1)] == 1)
      }
      #att.avg.boot[j,] <- sum(boot_rep$eff[tp_ix ,] * boot_rep$post[tp_ix ,])/sum(boot_rep$post[tp_ix ,])
      
      r <- align_sequences(boot_rep$post, boot_rep$eff, i-1)
      att[[j]] <- r$att
    }
    att <- unlist(att)
    conf.lvl.lb <- (1 - conf.lvl)/2
    conf.lvl.ub <- conf.lvl.lb + conf.lvl
    CI.avg[[i]] <- quantile(att, c(conf.lvl.lb, conf.lvl.ub), na.rm=TRUE)
    se.avg[[i]] <- sd(att, na.rm=TRUE)
    pvalue.avg[[i]] <- get.pvalue(att)
    att.avg[[i]] <- mean(att,na.rm = TRUE)
  }
  
  estimates <- cbind.data.frame(do.call(rbind, att_avg),do.call(rbind, se.avg),do.call(rbind,lapply(CI.avg, `[[`, 1)),
                                do.call(rbind,lapply(CI.avg, `[[`, 2)),do.call(rbind,pvalue.avg), seq(1,9))
  colnames(estimates) <- c('ATT.avg', 'S.E.', 'CI.lower', 'CI.upper', 'p.value', 'months')
  rownames(estimates) <- NULL
  
  estimates_list[[p]] <- estimates
  
}

estimates <- dplyr::bind_rows(estimates_list, .id = "column_label")
estimates <- data.table(estimates)
estimates[ , vote_bandwidth:=seq(22,46,by=4)[as.numeric(column_label)]]

estimates_full_bandwidth <- data.table(t(cbind(c(0.0153 , 0.0051 , 0.0045 , 0.0242 , 0.005 , 1),
                                               c(0.0088 , 0.0041 , -0.0003 , 0.0160 , 0.058 , 2), 
                                               c(0.0071 , 0.0038 , -0.0010 , 0.0139 , 0.078 , 3), 
                                               c(0.0046 , 0.0035 , -0.0029 , 0.0106 , 0.268 , 4), 
                                               c(0.0054 , 0.0035 , -0.0025 , 0.0110 , 0.199 , 5), 
                                               c(0.0048 , 0.0034 , -0.0032 , 0.0104 , 0.287 , 6), 
                                               c(0.0046 , 0.0034 , -0.0035 , 0.0100 , 0.350 , 7), 
                                               c(0.0060 , 0.0035 , -0.0026 , 0.0112 , 0.199 , 8), 
                                               c(0.0065 , 0.0036 , -0.0026 , 0.0114 , 0.207 , 9))))
colnames(estimates_full_bandwidth) <- c('ATT.avg', 'S.E.', 'CI.lower', 'CI.upper', 'p.value', 'months')
rownames(estimates_full_bandwidth) <- NULL
estimates_full_bandwidth[,vote_bandwidth:=100]

estimates <- rbind(estimates, estimates_full_bandwidth, fill=TRUE)


ggplot(estimates[vote_bandwidth==22 | vote_bandwidth==34 | vote_bandwidth==46 | vote_bandwidth==100],
       aes(x=months,y=ATT.avg,color=as.factor(vote_bandwidth))) +
  geom_point() +
  geom_errorbar(aes(ymin=CI.lower, ymax=CI.upper)) +
  scale_x_continuous(breaks = seq(1,9,1)) +
  geom_hline(yintercept = 0, linetype = "longdash", size=0.5, color='#666666') +
  ylab(paste('minority first victory effect on', 'hate crimes per 1000 residents', sep='\n')) +
  xlab('months after victory') +
  scale_color_discrete(name = paste('victory','margin', sep='\n'),
                       labels = c("+22", "+34", "+46", '+100')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.title.x = element_text(size =12), axis.title.y = element_text(size =12),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figureG8.pdf', width=6, height=4.85)
