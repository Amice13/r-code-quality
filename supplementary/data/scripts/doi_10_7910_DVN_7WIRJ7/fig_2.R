#   UNSC Debates over the Syrian Civil War plots
#       Fig 2. Daily steps
#   Juraj Medzihorsky
#   2017-02-28

#   libraries
library(rstan)
library(extrafont)
loadfonts()

#   #   The previous versions used the Cairo package, but something went wrong
#   #   and Gill Sans and Bitstram Charter no longer work on the author's 
#   #   primary machine. This version uses the extrafont package, with Gill Sans
#   #   installed locally in .ttf
#   library(Cairo)
#   CairoFonts(regular='Bitstream Charter:style=Regular')

#   misc
gr <- (1+sqrt(5))/2

p5 <- c('China', 'France', 'Russia', 'UK', 'USA')

iso3 <- c('AGO', 'ARG', 'AUS', 'AZE', 'BIH', 
          'BRA', 'TCD', 'CHL', 'CHN', 'COL', 
          'FRA', 'GER', 'GTM', 'IND', 'IRQ', 
          'ISR', 'JOR', 'LBN', 'LTU', 'LUX', 
          'MYS', 'MAR', 'NZL', 'NGA', 'PAK', 
          'POR', 'RUS', 'RWA', 'ZAF', 'KOR', 
          'ESP', 'SYR', 'TGO', 'TUR', 'GBR', 
          'USA', 'VEN')


#   load the data
load('sim_cvs.RData')
load('sim_avp.RData')


#   extract the posterior samples
avp <- extract(dim_avp)
cvs <- extract(dim_cvs)


#   --------------------
#       Fig 2 Sigmas
#   --------------------


nsim <- nrow(avp$country_mean)

scvs <- summary(dim_cvs, pars='sigma_theta')$summary
savp <- summary(dim_avp, pars='sigma_theta')$summary

max(c(scvs[,'97.5%'], savp[,'97.5%']))

rownames(scvs) <- rownames(savp) <- countries

scvs <- scvs[order(scvs[,'50%'], decreasing=F), ]
savp <- savp[order(savp[,'50%'], decreasing=F), ]

colcvs <- ifelse(rownames(scvs)%in%p5, 'darkblue', grey(0.45))
colavp <- ifelse(rownames(savp)%in%p5, 'darkblue', grey(0.45))

colcvs[rownames(scvs)=='Syria'] <- 'darkred'
colavp[rownames(savp)=='Syria'] <- 'darkred'



    pdf('fig_2_steps.pdf', width=10, height=6.2, family='Gill Sans Std')
xl <- c(0, 0.25)
yl <- c(1, 37)
par(mfrow=c(1,2), mar=c(5,5,1,1), xpd=T)
#   Crim vs Strug
plot(0,0,type='n',yaxt='n',xlim=xl,ylim=yl,ylab='',frame=F,
     xlab='Human Rights Violations')
for (i in 1:37) {
    tc <- colcvs[i]
    lines(xl, rep(i,2), col=grey(0.8), lty=3)
    lines(scvs[i, c('2.5%', '97.5%')], rep(i, 2), col=tc, lwd=gr^0)
    lines(scvs[i, c('25%', '75%')], rep(i, 2), col=tc, lwd=gr^1)
}
points(scvs[,'50%'], 1:37, pch=16, col=colcvs) 
text(rep(xl[1], nrow(scvs)), 1:37, rownames(scvs), pos=2, col=colcvs, cex=gr^-1) 
#    Act vs Proc
plot(0,0,type='n',yaxt='n',xlim=xl,ylim=yl,ylab='',frame=F,
     xlab='Intervention')
for (i in 1:37) {
    tc <- colavp[i]
    lines(xl, rep(i,2), col=grey(0.8), lty=3)
    lines(savp[i, c('2.5%', '97.5%')], rep(i, 2), col=tc, lwd=gr^0)
    lines(savp[i, c('25%', '75%')], rep(i, 2), col=tc, lwd=gr^1)
}
points(savp[,'50%'], 1:37, pch=16, col=colavp) 
text(rep(xl[1], nrow(savp)), 1:37, rownames(savp), pos=2, col=colavp, cex=gr^-1) 
    dev.off()

embedFonts('fig_2_steps.pdf')

#   SCRIPT END
