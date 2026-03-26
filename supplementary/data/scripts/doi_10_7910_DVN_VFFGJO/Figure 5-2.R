############################################################################################################################################
## Replication Data for:                                                                                                                   #
## Wawro, Gregory, and Ira Katznelson. Time Counts: Quantitative Analysis for Historical Social Science. Princeton University Press, 2022. #
############################################################################################################################################
## assume exogenous z, no y lag in TVTP; AR(1) model with no other covariates
## no errors in TPs; deterministic

rm(list = ls())
library(ggplot2)
set.seed(10162010)
source('Macros/rescale.R')

t    <- 50
zt0  <- 0
z    <- rnorm(t)
zint <- cbind(1,z) 

u <- rnorm(t)
e <- rnorm(t)

y    <- rep(NA,t)
y[1] <- -5
s    <- rep(0,t)

sstar <- rep(0,t)
ps    <- rep(NA,t)  ## prob of staying in state t-1
pl    <- rep(NA,t)  ## prob of leaving in state t-1
p11   <- rep(NA,t)
p12   <- rep(NA,t)
p21   <- rep(NA,t)
p22   <- rep(NA,t)
ps1   <- rep(0,t)
ps2   <- rep(0,t)

rho1 <- .8
rho2 <- .8
 
g1 <- c(-.1, -.1)
g2 <- c(1, 1)

# intercept and coeff on trend var for outcome equation
b1 <- c(-.3,0)
b2 <- c(.3,0)

## draw initial state
s[1]     <- 0
sstar[1] <- 0

for(i in 2:t){
    if (s[(i-1)] == 0){
        sstar[i] <- g1[1] + y[(i-1)] * g1[2]
        s[i]     <- (sstar[i] > 0) 
        p11[i]   <- pnorm(-sstar[i])
        p21[i]   <- 1 - p11[i]
        ps[i]    <- p11[i] # prob stay
        pl[i]    <- p21[i] # prob leave
        ps1[i]   <- ifelse(sstar[i] < 0,ps[i],NA)
        ps2[i]   <- ifelse(sstar[i] > 0,ps[i],NA)
        y[i]     <- b1[1] + rho1*y[(i-1)] + b1[2]*z[i] 
    } 
    else {
        sstar[i] <- g2[1] + y[(i-1)] * g2[2]
        s[i]     <- (sstar[i] > 0)
        p12[i]   <- pnorm(-sstar[i])
        p22[i]   <- 1 - p12[i]
        ps[i]    <- p22[i]
        pl[i]    <- p12[i]
        ps1[i]   <- ifelse(sstar[i] < 0,ps[i],NA)
        ps2[i]   <- ifelse(sstar[i] > 0,ps[i],NA)
        y[i]     <- b2[1] + rho2*y[(i-1)] + b2[2]*z[i] 
    }
    
}

tlab    <- seq(1:(t-1))
ym1     <- y[-1]
sm1     <- s[-1]
ps1m1   <- ps1[-1]
ps2m1   <- ps2[-1]
sstarm1 <- sstar[-1]
psm1    <- ps[-1]
sm1     <- s[-length(s)]

y_series        <- data.frame(cbind(t = tlab,y = ym1, sstar =  sstarm1, ps = psm1,s = sm1))
y_series$ps_sc  <- rescale(y_series$ps, min(y_series$y, na.rm=T), max(y_series$y, na.rm=T))
y_series$ps_sc1 <- ifelse(y_series$s == 0, y_series$ps_sc, NA)
y_series$ps_sc2 <- ifelse(y_series$s == 0, NA, y_series$ps_sc)

pdf("../Figures/Figure 5-2.pdf")
par(mar = c(5,5,2,5))
ggplot(y_series, aes(x = t)) +
    geom_line(aes(y = y,       color = 'y', linetype = 'y'), lwd=0.9) + 
    geom_point(aes(y = ps_sc1, color = 'Pr[Stay in State 1]', shape = 'Pr[Stay in State 1]'), size = 1.5) +
    geom_point(aes(y = ps_sc2, color = 'Pr[Stay in State 2]', shape = 'Pr[Stay in State 2]'), size = 1.5) +
    scale_y_continuous("y", 
                       sec.axis = sec_axis(~ rescale(., 0, 0.5), 
                                           name = "Probability",
                                           labels = scales::number_format(accuracy = 0.1, decimal.mark = '.'),
                                           breaks = seq(0, 0.5, by = 0.1))) +
    scale_colour_manual(name="",
                        values=c('y'='black','Pr[Stay in State 1]'='black','Pr[Stay in State 2]'='black'), 
                        guide = guide_legend(override.aes=aes(fill=NA))) + 
    scale_shape_manual(name="",
                      values=c('Pr[Stay in State 1]'=19,'Pr[Stay in State 2]'=5),
                      guide = "none") +
    scale_linetype_manual(name="",
                          values=c('y'=1),
                          guide="none") +
    theme(text=element_text(size=14),
          axis.text=element_text(color="black"),
          legend.position = "bottom",
          legend.text = element_text(size = 13),
          legend.key=element_blank(),
          legend.background = element_rect(fill = "white", colour = "black", size = 0.2),
          panel.background = element_rect(fill = "white", colour = "black")) +
    guides(colour = guide_legend(keywidth = 2, keyheight = 1,
                                 override.aes = list(
                                     color = c('black','black','black'),
                                     shape = c(NA, 19, 5),
                                     linetype = c(1, NA, NA)
                                 )))
dev.off()

