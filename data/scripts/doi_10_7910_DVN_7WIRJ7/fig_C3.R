#   UNSC Debates over the Syrian Civil War plots
#       Fig C3. correlations
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


load('sim_cvs.RData')
load('sim_avp.RData')


#   load the original data
load('d20160511.RData')

#   unique meeting dates
dates <- substr(unique(d$date), 1, 1e1)
dates <- dates[order(dates)]


#   extract the posterior samples
avp <- extract(dim_avp)
cvs <- extract(dim_cvs)


#   1 speech in CvS is not in AvP and vice versa
meet_avp <- data_avp$meet_fact
meet_cvs <- data_cvs$meet_fact

id_avp <- paste(data_avp$country, data_avp$meet_fact, sep='_')
id_cvs <- paste(data_cvs$country, data_cvs$meet_fact, sep='_')

id_both <- id_avp[id_avp %in% id_cvs]


theta_avp <- avp$theta
theta_cvs <- cvs$theta

colnames(theta_avp) <- id_avp
colnames(theta_cvs) <- id_cvs

pos_avp <- theta_avp[, id_both]
pos_cvs <- theta_cvs[, id_both]

mean(colnames(pos_avp)==colnames(pos_cvs))


#   a function to return the posterior for rho for a meeting 
corfun <-
    function(meeting, pos1=pos_cvs, pos2=pos_avp)
    {
        m1 <- sapply(colnames(pos1), function(x) strsplit(x, '_')[[1]][2]) 
        m2 <- sapply(colnames(pos2), function(x) strsplit(x, '_')[[1]][2]) 
        x1 <- pos1[,m1==meeting]
        x2 <- pos2[,m2==meeting]
        r <- sapply(1:nrow(x1), function(j) cor(x1[j,], x2[j,]))
        return(r)
    }


corpos <- sapply(unique(data_avp$meet_fact), corfun, pos1=pos_cvs, pos2=pos_avp)


#   a function to return the number of speeches in a meeting
Nfun <-
    function(meeting, pos1=pos_cvs)
    {
        m1 <- sapply(colnames(pos1), function(x) strsplit(x, '_')[[1]][2]) 
        x1 <- pos1[,m1==meeting]
        N <- ncol(x1)
        return(N)
    }


NN <- sapply(1:length(dates), Nfun, pos1=pos_avp) ; NN



    pdf('fig_C3_correlations.pdf', width=10, height=10*(gr^-1),
        family='Gill Sans Std')
mred <- 'darkred'
par(xpd=T, mar=c(3,4.5,0,1))
x0 <- as.Date(c('2011-05-01', paste(2012:2016, '-01-01', sep='')))
xl0 <- paste(c('1 May', rep('1 Jan.', 5)), substr(x0, 1, 4))
yl <- c(-1.0,1.0)
plot(0, 0, frame=F, xlim=range(x0), ylim=yl,
     xaxt='n', yaxt='n', xlab='', ylab='', main='')
lines(range(x0), c(0,0), col=grey(0.45), lty=3)
for (j in 1:ncol(corpos)) {
    td <- as.Date(dates[j])
    tq <- quantile(corpos[,j], probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
    lines(x=rep(td,2), y=tq[c(1,5)], col=mred, lwd=gr^-2)
    lines(x=rep(td,2), y=tq[c(2,4)], col=mred, lwd=gr^1)
    points(td, tq[3], pch=16, col=mred, cex=sqrt(NN[j]/5))
}
axis(1, x0, xl0)
axis(2, axTicks(2), axTicks(2), las=2)
axis(2, mean(axTicks(2)), expression(rho), las=2, line=2, lwd=0)
lv <- c(30, 20, 10, 5, 1)
legend('bottomright', horiz=F, bty='n', pch=rep(16, length(lv)), 
       title='States',
       col=rep(grey(0.45), length(lv)), legend=lv, pt.cex=sqrt(lv/5))
    dev.off()

embedFonts('fig_C3_correlations.pdf')

#   SCRIPT END
