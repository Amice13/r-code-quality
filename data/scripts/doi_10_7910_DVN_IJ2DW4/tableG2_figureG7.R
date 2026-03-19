rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

## load required packages
library(data.table)
library(zoo)
library(ggplot2)
library(xtable)
library(devtools)
devtools::install_local('./code/auxiliary packages/gsynth/gsynth', force=TRUE)
library(gsynth)

## load data
hate_crimes <- readRDS('./data/crime/hate_crime_did_analysis.RDS')
# define time, id variables
hate_crimes[ , time:=round(((as.yearmon(hate_crimes$month, format = '%m-%Y') - as.yearmon('2014-04'))*12)+1)]
hate_crimes[, id:=.GRP, by=.(PCON17CD)]


### Table G2: Ethnic minority victory effects on hate crime, first time a constituency experiences a minority victory
## Estimation
## interactive fixed effects
out <- gsynth(crime_rate ~ treat, data = hate_crimes, index = c("id","time"), force = "two-way",
              CV = TRUE, se= TRUE, seed = 111, nboots = 2000, inference = "parametric", parallel = FALSE)

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
  first_treated_period <- apply(post, FUN=function(x) min(which(x)), 2)
  last_first_treated_period <-max(first_treated_period)
  first_first_treated_period <-min(first_treated_period)
  n_periods <- nrow(post)
  post_aligned <- matrix(nrow=n_periods+last_first_treated_period-first_first_treated_period, ncol=46)
  eff_aligned <- matrix(nrow=n_periods+last_first_treated_period-first_first_treated_period, ncol=46)
  
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
  
  
  min_treated_periods <- num_periods
  att <- sum(
    eff_aligned[last_first_treated_period:(last_first_treated_period+min_treated_periods),] * post_aligned[last_first_treated_period:(last_first_treated_period+min_treated_periods),]
  ) / sum( post_aligned[last_first_treated_period:(last_first_treated_period+min_treated_periods),])
  
  return(list(eff_aligned=eff_aligned, post_aligned=post_aligned, att=att))
}

att_avg <- vector(mode = 'list', length = length(seq(1,9)))
for (i in seq(1,9)) {
  relevant_period_att <- align_sequences(out$post, out$eff, i-1)
  att_avg[[i]] <- relevant_period_att$att
}


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

## compute and print Table G.2
estimates <- cbind.data.frame(do.call(rbind, att_avg),do.call(rbind, se.avg),do.call(rbind,lapply(CI.avg, `[[`, 1)),
                              do.call(rbind,lapply(CI.avg, `[[`, 2)),do.call(rbind,pvalue.avg), seq(1,9))
colnames(estimates) <- c('ATT.avg', 'S.E.', 'CI.lower', 'CI.upper', 'p.value', 'months')
rownames(estimates) <- NULL

tableg2 <- estimates
# create footnotes for table
comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(tableg2))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize Notes: The dependent variable is monthly hate crimes (racially/religiously aggravated offenses) per 1,000 residents. Inference is conducted via bootstrapping. Standard errors are clustered by constituency.}\n", sep = ""))

# print table - Table G2
print(xtable(tableg2,
             align = "lcccccc",       
             caption = "First Time Ethnic Minority Victory Effects on Hate Crime (averaging across months after victory)",
             label = "table:table_G2",
             digits = c(0,4,4,4,4,3,0)),
      include.rownames=FALSE,
      caption.placement = "top",
      add.to.row = comment,
      timestamp = NULL,
      file='./output/tables/tableG2.tex')


### Figure G7: First time ethnic minority victory effects on hate crime
data_plot <- data.table(data.frame(out$est.att), keep.rownames = TRUE)
data_plot[,rn:=as.numeric(rn)]
data_plot <- data_plot[rn<=10]

ggplot(data_plot,aes(x=rn,y=ATT)) +
  geom_line(color="blue") +
  geom_ribbon(aes(ymin=CI.lower, ymax=CI.upper), alpha = 0.2,fill = "blue") +
  geom_vline(xintercept = 0, linetype = "longdash", linewidth=0.5, color='#666666') +
  geom_hline(yintercept = 0, linetype = "longdash", linewidth=0.5, color='#666666') +
  scale_x_continuous(name = 'Months relative to minority victory', breaks = c(-12:10)) +
  ylab('Effect on hate crimes per 1000 residents') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12),
        axis.title.x = element_text(size =12), axis.title.y = element_text(size =12),
        axis.text.y = element_text(size = 12))
ggsave('./output/figures/figureG7.pdf', width=6, height=4.85)
