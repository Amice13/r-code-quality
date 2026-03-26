############################################################################################################################################
## Replication Data for:                                                                                                                   #
## Wawro, Gregory, and Ira Katznelson. Time Counts: Quantitative Analysis for Historical Social Science. Princeton University Press, 2022. #
############################################################################################################################################
# Code and dataset are also available at:
# Wawro, Gregory, 2014, 
# "Replication data for: Designing Historical Social Scientific Inquiry: How Parameter Heterogeneity Can Bridge the Methodological Divide between Quantitative and Qualitative Approaches", 
# https://doi.org/10.7910/DVN/25013, Harvard Dataverse.
# LaborSenateRollCalls.txt is identical to rollcall.logit.inter.all.names.txt
# RollCallMapfile.txt is identical to rollcall.mapfile.txt
# votes_bycongress_spatial.prg is identical to the BayesX code run below 

rm(list=ls()) 
library(arm)
library(VGAM)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(grid)
library(BayesX)
library(R2BayesX)

## BayesX estimation
rc    <- read.table('../Data/LaborSenateRollCalls.txt', header = T)
rcmap <- read.bnd('../Data/RollCallMapfile.txt')
rc$district <- rc$t*10 + rc$region

b <- bayesx(formula = prolabor ~ sx(district, bs = 'spatial', map = rcmap) 
                               + sx(district, bs = 'spatial', map = rcmap, by = unionpop) 
                               + sx(district, bs = 'spatial', map = rcmap, by = aapct) 
                               + sx(district, bs = 'spatial', map = rcmap, by = urbanpct) 
                               + dem + laborcomm, 
             data     = rc, 
             control  = bayesx.control(
                           family     = "binomialprobit",
                           method     = "MCMC",
                           iterations = 250000,
                           burnin     = 2000,
                           step       = 10,
                           predict    = T,
                           verbose    = T, 
                           dir.rm     = F)
            )

file.copy(from = b$bayesx.prg$file.dir, to = '../Output/', overwrite = T, recursive = T, copy.mode = T)
unlink(b$bayesx.prg$file.dir, recursive = T)
fldr <- paste0('../Output/', basename(b$bayesx.prg$file.dir))

## Get estimates from Bayesx output
a.results    <- read.table(paste0(fldr, '/bayesx.estim_f_district_spatial.res'), header = T)
b.names      <- c("73rd","74th","75th","76th","77th","78th","79th","80th")
b.names.alt  <- t(matrix(b.names,3,8))
b.names.alt  <- c(matrix(b.names.alt,24,1),"Democrat","Labor Comm","Urban")
b.fe.results <- read.table(paste0(fldr, "/bayesx.estim_FixedEffects1.res"), header=T)

dta <- read.table("../Data/LaborSenateRollCalls.txt",header=T)
a   <- read.table(paste0(fldr, "/bayesx.estim_f_district_spatial_sample.raw"), header=T)[,-1]
b.union    <- read.table(paste0(fldr, "/bayesx.estim_unionpop_f_district_spatial_sample.raw"), header=T)[,-1]
b.aapct    <- read.table(paste0(fldr, "/bayesx.estim_aapct_f_district_spatial_sample.raw"), header=T)[,-1]
b.urbanpct <- read.table(paste0(fldr, "/bayesx.estim_urbanpct_f_district_spatial_sample.raw"), header=T)[,-1]
b.fe       <- read.table(paste0(fldr, "/bayesx.estim_FixedEffects1_sample.raw"), header=T)

congnum <- seq(73,80,1)
lb <- .05
ub <- .95

## Creating data frame for ggplot
alldf <- list(a = a, b.union = b.union, b.aapct = b.aapct, b.urbanpct = b.urbanpct)
grp   <- c('Region-Period Effect', 'Union Pct', 'African American Pct', 'Urban Pct')
reg   <- c('Deep South', 'Border South', 'Non-South')
regs  <- c(1, 9, 17)
df    <- NULL
for (i in 1:length(alldf)){
  for (r in 1:length(reg)){
    temp <- alldf[[i]][, regs[r]:(regs[r]+7)]
    for (c in 1:length(congnum)){
      df <- dplyr::bind_rows(df, data.frame(x   = congnum[c],
                                            est = median(temp[,c], na.rm=T),
                                            lb  = quantile(temp[,c], probs=0.05),
                                            ub  = quantile(temp[,c], probs=0.95),
                                            grp = grp[i],
                                            reg = reg[r]))
    }
    rm(temp)
  }
}
df$grp.f <- factor(df$grp, levels = c('Region-Period Effect', 'Union Pct', 'African American Pct', 'Urban Pct'))
df$reg.f <- factor(df$reg, levels = c('Deep South', 'Border South', 'Non-South'))

plt <- list()
for (r in unique(df$reg)){
  for (i in unique(df$grp)){
    t = paste0(substr(r, 1, 1), substr(i, 1, 3))
    plt[[t]] <- 
      ggplot(data = df[df$grp == i & df$reg == r,], aes(x = x, y = est)) +
      geom_hline(yintercept = 0, lty = 3, color = 'gray55') +
      geom_point() + 
      geom_errorbar(aes(ymin = lb, ymax = ub), width = 0) + 
      scale_x_continuous(breaks = seq(min(df$x), max(df$x), 1)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
      theme_bw() +
      labs(y = '', x = '') + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.text.x     = element_blank(), 
            strip.background = element_blank(),
            axis.text.y      = element_text(size = 10), 
            axis.text.x      = element_text(size = 10))
    if (i != 'Region-Period Effect'){
      plt[[t]] <- plt[[t]] +
        scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
        theme(axis.text.y = element_text(size = 10),
              axis.text.x = element_text(size = 10))
    }
  }
}

pdf('../Figures/Figure 4-9.pdf', width = 7, height = 9)
grid.arrange(arrangeGrob(plt[['DReg']], top=textGrob("Deep South",   hjust=0.2, gp=gpar(fontsize=11, fontface=1))),
             arrangeGrob(plt[['BReg']], top=textGrob("Border South", hjust=0.2, gp=gpar(fontsize=11, fontface=1))),
             arrangeGrob(plt[['NReg']], top=textGrob("Non-South",    hjust=0.2, gp=gpar(fontsize=11, fontface=1)),
                         right=textGrob(grp[1], hjust=0.55, rot=270, gp=gpar(fontsize=11, fontface=1))),
             plt[['DUni']],
             plt[['BUni']], 
             arrangeGrob(plt[['NUni']], right=textGrob(grp[2], hjust=.9, rot=270, gp=gpar(fontsize=11, fontface=1))),
             plt[['DAfr']],
             plt[['BAfr']],
             arrangeGrob(plt[['NAfr']], right=textGrob(grp[3], hjust=0.625, rot=270, gp=gpar(fontsize=11, fontface=1))),
             plt[['DUrb']], 
             plt[['BUrb']], 
             arrangeGrob(plt[['NUrb']], right=textGrob(grp[4], hjust=.85, rot=270, gp=gpar(fontsize=11, fontface=1))),
             ncol=3, nrow=4, heights = c(4.45,4,4,4), widths = c(4,4,4.3),
             left=textGrob("Estimates", rot=90, gp=gpar(fontsize=14, fontface=1)),
             bottom=textGrob("Congress", gp=gpar(fontsize=14, fontface=1)))
dev.off()


## Marginal effects
int         <- b.fe$b_1
b.dem       <- b.fe$b_2
b.laborcomm <- b.fe$b_3
n.sims      <- nrow(a)
n.period    <- ncol(a)/3

p.ds1        <- array(NA,c(n.sims,n.period))
p.ds2        <- array(NA,c(n.sims,n.period))
p.aa.ds2     <- array(NA,c(n.sims,n.period))
p.un.aa.ds2  <- array(NA,c(n.sims,n.period))
p.aa.urb.ds2 <- array(NA,c(n.sims,n.period))
p.bs1        <- array(NA,c(n.sims,n.period))
p.bs2        <- array(NA,c(n.sims,n.period))
p.aa.bs2     <- array(NA,c(n.sims,n.period))
p.un.aa.bs2  <- array(NA,c(n.sims,n.period))
p.aa.urb.bs2 <- array(NA,c(n.sims,n.period))
p.ns.dem1    <- array(NA,c(n.sims,n.period))
p.ns.dem2    <- array(NA,c(n.sims,n.period))
p.un.aa.ns.dem2  <- array(NA,c(n.sims,n.period))
p.aa.ns.dem2     <- array(NA,c(n.sims,n.period))
p.aa.urb.ns.dem2 <- array(NA,c(n.sims,n.period))
p.ns.rep1        <- array(NA,c(n.sims,n.period))
p.ns.rep2        <- array(NA,c(n.sims,n.period))

for (j in 1:n.period){
  aapct.med    <- median(dta$aapct[dta$period==j & dta$region==1]) 
  urbanpct.med <- median(dta$urbanpct[dta$period==j & dta$region==1])
  unionpop.med <- median(dta$unionpop[dta$period==j & dta$region==1])
  
  aapct.sd    <- sd(dta$aapct[dta$period==j & dta$region==1]) 
  urbanpct.sd <- sd(dta$urbanpct[dta$period==j & dta$region==1])
  unionpop.sd <- sd(dta$unionpop[dta$period==j & dta$region==1])
  
  p.ds1[,j] <- probit(int + a[,j] + b.dem +
                        b.urbanpct[,j]*urbanpct.med +
                        b.union[,j]*unionpop.med +
                        b.aapct[,j]*aapct.med, inverse=T)
  
  p.ds2[,j] <- probit(int + a[,j] + b.dem +
                        b.aapct[,j]*aapct.med +
                        b.urbanpct[,j]*urbanpct.med +
                        b.union[,j]*(unionpop.med + unionpop.sd), inverse=T)
  
  p.aa.ds2[,j] <- probit(int + a[,j] + b.dem +
                           b.aapct[,j]*(aapct.med + aapct.sd) +
                           b.urbanpct[,j]*urbanpct.med +
                           b.union[,j]*(unionpop.med), inverse=T)
  
  p.un.aa.ds2[,j] <- probit(int + a[,j] + b.dem +
                              b.aapct[,j]*(aapct.med + aapct.sd) +
                              b.urbanpct[,j]*urbanpct.med +
                              b.union[,j]*(unionpop.med + unionpop.sd), inverse=T)
  
  p.aa.urb.ds2[,j] <- probit(int + a[,j] + b.dem +
                               b.aapct[,j]*(aapct.med + aapct.sd) +
                               b.urbanpct[,j]*(urbanpct.med + urbanpct.sd) +
                               b.union[,j]*(unionpop.med + unionpop.sd), inverse=T)
}

for (j in 1:n.period){
  aapct.med    <- median(dta$aapct[dta$period==j & dta$region==2]) 
  urbanpct.med <- median(dta$urbanpct[dta$period==j & dta$region==2])
  unionpop.med <- median(dta$unionpop[dta$period==j & dta$region==2])
  
  aapct.sd <- sd(dta$aapct[dta$period==j & dta$region==2]) 
  urbanpct.sd <- sd(dta$urbanpct[dta$period==j & dta$region==2])
  unionpop.sd <- sd(dta$unionpop[dta$period==j & dta$region==2])
  
  k <- n.period + 8
  p.bs1[,j] <- probit(int + a[,k] + b.dem +
                        b.aapct[,k]*aapct.med +
                        b.urbanpct[,k]*urbanpct.med +
                        b.union[,k]*unionpop.med, inverse=T)
  
  p.bs2[,j] <- probit(int + a[,k] + b.dem +
                        b.urbanpct[,k]*urbanpct.med +
                        b.aapct[,k]*aapct.med +
                        b.union[,k]*(unionpop.med + unionpop.sd), inverse=T)
  
  p.aa.bs2[,j] <- probit(int + a[,k] + b.dem +
                           b.urbanpct[,k]*urbanpct.med +
                           b.aapct[,k]*(aapct.med + aapct.sd) +
                           b.union[,k]*unionpop.med, inverse=T)
  
  p.un.aa.bs2[,j] <- probit(int + a[,k] + b.dem +
                              b.urbanpct[,k]*urbanpct.med +
                              b.aapct[,k]*(aapct.med + aapct.sd) +
                              b.union[,k]*(unionpop.med + unionpop.sd), inverse=T)
  
  p.aa.urb.bs2[,j] <- probit(int + a[,k] + b.dem +
                               b.aapct[,k]*(aapct.med + aapct.sd) +
                               b.urbanpct[,k]*(urbanpct.med + urbanpct.sd) +
                               b.union[,k]*(unionpop.med + unionpop.sd), inverse=T)
}

for (j in 1:n.period){ 
  aapct.med    <- median(dta$aapct[dta$period==j & dta$region==3]) 
  urbanpct.med <- median(dta$urbanpct[dta$period==j & dta$region==3])
  unionpop.med <- median(dta$unionpop[dta$period==j & dta$region==3])
  
  aapct.sd    <- sd(dta$aapct[dta$period==j & dta$region==3]) 
  urbanpct.sd <- sd(dta$urbanpct[dta$period==j & dta$region==3])
  unionpop.sd <- sd(dta$unionpop[dta$period==j & dta$region==3])
  
  k <- n.period + 16
  p.ns.dem1[,j] <- probit(int + a[,k] + b.dem +
                            b.aapct[,k]*aapct.med +
                            b.urbanpct[,k]*urbanpct.med +
                            b.union[,k]*unionpop.med, inverse=T)
  
  p.ns.dem2[,j] <- probit(int + a[,k] + b.dem +
                            b.aapct[,k]*aapct.med +
                            b.urbanpct[,k]*urbanpct.med +
                            b.union[,k]*(unionpop.med + unionpop.sd), inverse=T)
  
  p.aa.ns.dem2[,j] <- probit(int + a[,k] + b.dem +
                               b.aapct[,k]*(aapct.med + aapct.sd) +
                               b.urbanpct[,k]*urbanpct.med +
                               b.union[,k]*unionpop.med, inverse=T)
  
  p.un.aa.ns.dem2[,j] <- probit(int + a[,k] + b.dem +
                                  b.aapct[,k]*(aapct.med + aapct.sd) +
                                  b.urbanpct[,k]*urbanpct.med +
                                  b.union[,k]*(unionpop.med + unionpop.sd), inverse=T)
  
  p.aa.urb.ns.dem2[,j] <- probit(int + a[,k] + b.dem +
                                   b.aapct[,k]*(aapct.med + aapct.sd) +
                                   b.urbanpct[,k]*(urbanpct.med + urbanpct.sd) +
                                   b.union[,k]*(unionpop.med + unionpop.sd), inverse=T)
  
  p.ns.rep1[,j] <- probit(int + a[,k] + 
                            b.aapct[,k]*aapct.med +
                            b.urbanpct[,k]*urbanpct.med +
                            b.union[,k]*unionpop.med, inverse=T)
  
  p.ns.rep2[,j] <- probit(int + a[,k] + 
                            b.aapct[,k]*aapct.med +
                            b.urbanpct[,k]*urbanpct.med +
                            b.union[,k]*(unionpop.med + unionpop.sd), inverse=T)
}

p.diff.ds     <- p.ds2 - p.ds1
p.diff.bs     <- p.bs2 - p.bs1
p.diff.dem.ns <- p.ns.dem2 - p.ns.dem1
p.diff.rep.ns <- p.ns.rep2 - p.ns.rep1

p.diff.aa.ds     <- p.aa.ds2 - p.ds1
p.diff.aa.bs     <- p.aa.bs2 - p.bs1
p.diff.aa.dem.ns <- p.aa.ns.dem2 - p.ns.dem1

p.diff.un.aa.ds     <- p.un.aa.ds2 - p.ds1
p.diff.un.aa.bs     <- p.un.aa.bs2 - p.bs1
p.diff.un.aa.dem.ns <- p.un.aa.ns.dem2 - p.ns.dem1

p.diff.aa.urb.ds     <- p.aa.urb.ds2 - p.ds1
p.diff.aa.urb.bs     <- p.aa.urb.bs2 - p.bs1
p.diff.aa.urb.dem.ns <- p.aa.urb.ns.dem2 - p.ns.dem1

p.dd.bs     <- p.diff.aa.urb.bs - p.diff.bs
p.dd.ds     <- p.diff.aa.urb.ds - p.diff.ds
p.dd.dem.ns <- p.diff.aa.urb.dem.ns - p.diff.dem.ns

alldf <- list( p.diff.aa.ds,    p.diff.aa.bs,    p.diff.aa.dem.ns, 
               p.diff.ds,       p.diff.bs,       p.diff.dem.ns, 
               p.diff.un.aa.ds, p.diff.un.aa.bs, p.diff.un.aa.dem.ns,
               p.diff.aa.urb.ds,p.diff.aa.urb.bs, p.diff.aa.urb.dem.ns)

grpl  <- paste0(c('Increase in AA Pct','Increase in Union Pct', 'Increase in Union Pct\n & AA Pct', 
                  'Increase in Union Pct, \nAA Pct & Urban Pct'))
grp   <- sort(rep(grpl,3))
reg   <- rep(c('Deep South', 'Border South', 'Non-South, Democrats'),4)
df    <- NULL

for (i in 1:length(alldf)){
  for (c in 1:length(congnum)){
    df <- dplyr::bind_rows(df, data.frame(x   = congnum[c],
                                          est = median(alldf[[i]][,c], na.rm=T),
                                          lb  = quantile(alldf[[i]][,c], probs=0.05),
                                          ub  = quantile(alldf[[i]][,c], probs=0.95),
                                          grp = grp[i],
                                          reg = reg[i]))
  }
}

df$grp.f <- factor(df$grp, levels = grpl)
df$reg.f <- factor(df$reg, levels = c('Deep South', 'Border South', 'Non-South, Democrats'))

plt <- list()
for (r in unique(df$reg)){
  for (i in unique(df$grp)){
    t = paste0(substr(r, 1, 1), nchar(i))
    plt[[t]] <- 
      ggplot(data = df[df$grp == i & df$reg == r,], aes(x = x, y = est)) +
      geom_hline(yintercept = 0, lty = 3, color = 'gray30') +
      geom_point() + 
      geom_errorbar(aes(ymin = lb, ymax = ub), width = 0) + 
      scale_x_continuous(breaks = seq(min(df$x), max(df$x), 1)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
      theme_bw() +
      labs(y = '', x = '') + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.text.x     = element_blank(), 
            strip.background = element_blank(),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10))
    
    plt[[t]] <- plt[[t]] +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
      theme(axis.text.y = element_text(size = 10))
  }
}


grp    <- unique(grp)
labs1  <- c("\n",grp[1])
r1a    <- textGrob(labs1[1], hjust=.625, rot=270, gp=gpar(fontsize=11, fontface=1))
r1b    <- textGrob(labs1[2], hjust=.55, rot=270, gp=gpar(fontsize=11, fontface=1))

labs2  <- c("\n",grp[2])
r2a    <- textGrob(labs2[1], hjust=.625, rot=270, gp=gpar(fontsize=11, fontface=1))
r2b    <- textGrob(labs2[2], hjust=.625, rot=270, gp=gpar(fontsize=11, fontface=1))

labs3 <- str_split(unique(grp)[3],'\\n')[[1]]
r3a   <- textGrob(labs3[1], hjust=.625, rot=270, gp=gpar(fontsize=11, fontface=1))
r3b   <- textGrob(labs3[2], hjust=.675, rot=270, gp=gpar(fontsize=11, fontface=1))

labs4 <- str_split(unique(grp)[4],'\\n')[[1]]
r4a   <- textGrob(labs4[1], hjust=.6, rot=270, gp=gpar(fontsize=11, fontface=1))
r4b   <- textGrob(labs4[2], hjust=.625, rot=270, gp=gpar(fontsize=11, fontface=1))

pdf('../Figures/Figure 4-10.pdf', width = 7, height = 9)
grid.arrange(arrangeGrob(plt[['D18']], top=textGrob("Deep South", hjust=0.2, gp=gpar(fontsize=11, fontface=1))),
             arrangeGrob(plt[['B18']], top=textGrob("Border South", hjust=0.27, gp=gpar(fontsize=11, fontface=1))),
             arrangeGrob(plt[['N18']], top=textGrob("Non-South", hjust=0.2, gp=gpar(fontsize=11, fontface=1))),
             r1b,
             r1a,
             plt[['D21']], 
             plt[['B21']], 
             plt[['N21']],
             r2b,
             r2a,
             plt[['D31']],
             plt[['B31']],
             plt[['N31']], 
             r3b,
             r3a,
             plt[['D42']], 
             plt[['B42']], 
             plt[['N42']], 
             r4b,
             r4a,
             ncol=5, 
             nrow=4, 
             heights = c(4.4,4,4,4), 
             widths = c(5,5,5,0.4,0.4),
             left=textGrob("Estimates", rot=90, gp=gpar(fontsize=14, fontface=1)),
             bottom=textGrob("Congress", gp=gpar(fontsize=14, fontface=1)))
dev.off()