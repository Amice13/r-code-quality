#   UNSC Debates over the Syrian Civil War plots
#       Fig C2. Country means
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


#   shorten selected state names
d$country[grep('^Russ', d$country)] <- 'Russia'
d$country[grep('^United K', d$country)] <- 'UK'
d$country[grep('^United S', d$country)] <- 'USA'


#   posterior summaries
s1 <- summary(dim_avp, pars='country_mean')$summary
s2 <- summary(dim_cvs, pars='country_mean')$summary
rownames(s1) <- rownames(s2) <- countries

s1 <- s1[order(s1[,'50%'], decreasing=F), ]
s2 <- s2[order(s2[,'50%'], decreasing=F), ]

#   flip the dimensions
s1[,1:8] <- -1 * s1[, 1:8]
s2[,1:8] <- -1 * s2[, 1:8]

col1 <- ifelse(rownames(s1)%in%p5, 'darkblue', grey(0.45))
col2 <- ifelse(rownames(s2)%in%p5, 'darkblue', grey(0.45))

col1[rownames(s1)=='Syria'] <- 'darkred'
col2[rownames(s2)=='Syria'] <- 'darkred'



    pdf('fig_C2_means.pdf', width=10, height=6.2, family='Gill Sans Std')
xl <- c(-3,+2)
yl <- c(1, 37)
par(mfrow=c(1,2), mar=c(5,5,1,1), xpd=T)
#   D1 (old D2) Crim vs Strug
plot(0,0,type='n',yaxt='n',xlim=xl,ylim=yl,ylab='',frame=F,
     xlab='Human Rights Violations')
for (i in 1:37) {
    tc <- col2[i]
    lines(xl, rep(i,2), col=grey(0.8), lty=3)
    tl2 <- tl <- s2[i, c('2.5%', '97.5%')][2:1]
    tl2[1] <- max(tl[1], xl[1])
    tl2[2] <- min(tl[2], xl[2])
    lines(tl2, rep(i, 2), col=tc, lwd=gr^0)
    if (tl[1] < tl2[1]) { points(tl2[1], i, pch='|', col=tc, cex=gr^-2)  }
    if (tl[2] > tl2[2]) { points(tl2[2], i, pch='|', col=tc, cex=gr^-2)  }
    lines(s2[i, c('25%', '75%')], rep(i, 2), col=tc, lwd=gr^1)
}
points(s2[,'50%'], 1:37, pch=16, col=col2) 
text(rep(xl[1], nrow(s2)), 1:37, rownames(s2), pos=2, col=col2, cex=gr^-1) 
#   D2 (old D1) Act vs Proc
plot(0,0,type='n',yaxt='n',xlim=xl,ylim=yl,ylab='',frame=F,
     xlab='Intervention')
for (i in 1:37) {
    tc <- col1[i]
    lines(xl, rep(i,2), col=grey(0.8), lty=3)
    tl2 <- tl <- s1[i, c('2.5%', '97.5%')][2:1]
    tl2[1] <- max(tl[1], xl[1])
    tl2[2] <- min(tl[2], xl[2])
    lines(tl2, rep(i, 2), col=tc, lwd=gr^0)
    if (tl[1] < tl2[1]) { points(tl2[1], i, pch='|', col=tc, cex=gr^-2)  }
    if (tl[2] > tl2[2]) { points(tl2[2], i, pch='|', col=tc, cex=gr^-2)  }
    lines(s1[i, c('25%', '75%')], rep(i, 2), col=tc, lwd=gr^1)
}
points(s1[,'50%'], 1:37, pch=16, col=col1) 
text(rep(xl[1], nrow(s1)), 1:37, rownames(s1), pos=2, col=col1, cex=gr^-1) 
    dev.off()

embedFonts('fig_C2_means.pdf')

#   SCRIPT END
