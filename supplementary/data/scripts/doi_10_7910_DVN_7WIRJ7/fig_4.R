#   UNSC Debates over the Syrian Civil War plots
#       Fig 4. gammas
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


#    misc
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

#   NOTE: Later in the script the dimensions are flipped (multiplied with -1).


#   load the timeline data
u <- read.csv('Syria_timeline.csv')
u[,1] <- as.Date(u[,1], format='%m/%d/%Y')
u <- u[u$descr!='', ]
u <- u[u$descr!='Arab League', ]
u$descr[1] <- 'Conflict starts'
u$un <- rep(0, nrow(u))
u$un[grep('esolution', u$descr)] <- 1
u$linecol <- ifelse(u$un==1, grey(0.25), grey(0.55))


#   load the original data
load('d20160511.RData')


#   unique dates
dates <- substr(unique(d$date), 1, 1e1)
dates <- dates[order(dates)]


#   gamma posterior summaries
gcvs <- as.data.frame(summary(dim_cvs, pars='gamma')$summary)
gavp <- as.data.frame(summary(dim_avp, pars='gamma')$summary)

#   flip the dimensions
gcvs[, 1:8] <- -1 * gcvs[, 1:8]
gavp[, 1:8] <- -1 * gavp[, 1:8]


    pdf('fig_4_gammas.pdf', width=10, height=10, family='Gill Sans Std')
mat <- matrix(c(1,2,3), 3, 1)
layout(mat, widths=c(1), c(gr^-1, gr, gr))
par(xpd=T, mar=c(1.5,8.5,1,6.5))
x0 <- as.Date(c('2011-05-01', paste(2012:2016, '-01-01', sep='')))
xl0 <- paste(c('1 May', rep('1 January', 5)), substr(x0, 1, 4))
yl <- c(-1.0,3.25)
plot(0, 0, frame=F, xlim=range(x0), ylim=c(3,nrow(u)+0), 
     xaxt='n', yaxt='n', xlab='', ylab='', main='')
for(i in 1:nrow(u)) {
    points(as.Date(u[i,1]), 1+nrow(u)-i, pch=16, cex=gr^-2, col=u$linecol[i])
    text(as.Date(u[i,1]), 1+nrow(u)-i, u$descr[i], cex=gr^-1, pos=4, col=u$linecol[i])
    lines(rep(as.Date(u[i,1]), 2), c(-5, 1+nrow(u)-i), col=u$linecol[i], lwd=0.5, lty=3)
}
#
par(mar=c(2.5,8.5,0,6.5))
plot(0, 0, frame=F, xlim=range(x0), ylim=yl-2,
     xaxt='n', yaxt='n', xlab='', ylab='', main='')
axis(2, axTicks(2), axTicks(2), las=2)
axis(2, mean(axTicks(2)), '', las=2, line=2, lwd=0)
text(as.Date('2010-07-01'), -1,'Human\nRights\nViolations', pos=4)
for (i in 1:nrow(gcvs)) {
    lines(rep(as.Date(dates)[i], 2), gcvs[i, c('2.5%', '97.5%')], col=rgb(0,0.5,0), lwd=gr^-1)
    lines(rep(as.Date(dates)[i], 2), gcvs[i, c('25%', '75%')], col=rgb(0,0.5,0), lwd=gr^1)
}
points(as.Date(dates), gcvs[,'50%'], pch=16, col=rgb(0,0.5,0), cex=1)
#
par(mar=c(2.5,8.5,0,6.5))
plot(0, 0, frame=F, xlim=range(x0), ylim=yl-1,
     xaxt='n', yaxt='n', xlab='', ylab='', main='')
axis(1, x0, xl0)
axis(2, axTicks(2), axTicks(2), las=2)
axis(2, mean(axTicks(2)), '', las=2, line=2, lwd=0)
text(as.Date('2010-07-01'), 0,'Intervention', pos=4)
for (i in 1:nrow(gcvs)) {
    lines(rep(as.Date(dates)[i], 2), gavp[i, c('2.5%', '97.5%')], col=rgb(0,0.5,0), lwd=gr^-1)
    lines(rep(as.Date(dates)[i], 2), gavp[i, c('25%', '75%')], col=rgb(0,0.5,0), lwd=gr^1)
}
points(as.Date(dates), gavp[,'50%'], pch=16, col=rgb(0,0.5,0), cex=1)
legend('bottom', horiz=T, pch=rep(16,3), pt.cex=c(1,0,0), 
       lwd=c(0,gr^1,gr^-1), legend=c('Median', '50% Interval', '95% Interval'),
       col=rep(rgb(0,0.5,0), 3), lty=c(0,1,1))
    dev.off()

embedFonts('fig_4_gammas.pdf')

#   SCRIPT END
