#   UNSC Debates over the Syrian Civil War plots
#       Fig 1 Country means
#   Juraj Medzihorsky
#   2017-02-28

#   libraries
library(ellipse)
library(rstan)
library(extrafont)
loadfonts()

fonts()                                 

#   #   The previous versions used the Cairo package, but something went wrong
#   #   and Gill Sans and Bitstram Charter no longer work on the author's 
#   #   primary machine. This version uses the extrafont package, with Gill Sans
#   #   installed locally in .ttf
#   library(Cairo)
#   CairoFonts(regular='Bitstream Charter:style=Regular')


# misc
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

#   NOTE: Later in the script the dimensions are flipped (multiplied with -1).


#   -------------------------------
#       Figure 1. Country means
#   -------------------------------


#   get posterior medians for country means
cm1 <- apply(avp$country_mean, 2, median)
cm2 <- apply(cvs$country_mean, 2, median)

#   get posterior densities for correlations (Pearson and Spearman)
nsim <- nrow(avp$country_mean)
cor_pos_p <- sapply(1:nsim, function(i) cor(avp$country_mean[i,], cvs$country_mean[i,]))
cor_pos_s <- sapply(1:nsim, function(i) cor(avp$country_mean[i,], cvs$country_mean[i,], method='spearman'))

summary(cor_pos_p)
summary(cor_pos_s)

round(quantile(cor_pos_p, probs=c(0.025, 0.5, 0.975)), 2)
round(quantile(cor_pos_s, probs=c(0.025, 0.5, 0.975)), 2)  


#   get ranges, for plot
xl <- range(cm2)
yl <- range(cm1)


#   a function to plot elliptical credible regions
getE <-
    function(x, y, l1=0.8, l2=0.5, col1=rgb(0,0,1,0.05), col2=rgb(0,0,1,0.15))
    {
        E1 <- ellipse::ellipse(cor(cbind(x,y)), scale=c(sd(x), sd(y)), 
                               centre=c(mean(x), mean(y)), level=l1) 
        E2 <- ellipse::ellipse(cor(cbind(x,y)), scale=c(sd(x), sd(y)), 
                               centre=c(mean(x), mean(y)), level=l2) 
        polygon(E1, border=rgb(1,1,1,0), col=col1)
        polygon(E2, border=rgb(1,1,1,0), col=col2)
    }


    pdf('fig_1_ellipses.pdf', width=10, height=6.2, family='Gill Sans Std')
par(mar=c(5,9,1,2), xpd=T)
xl <- c(-3.25, 1.00)
yl <- c(-1.25, 1.25)
plot(0,0,type='n',xlim=xl,ylim=yl,asp=1, xaxt='n', yaxt='n', frame=F,
     xlab='', ylab='')
axis(2, 0, 'Intervention', las=2, line=2, lwd=0)
axis(1, -1, 'Human Rights Violations', line=2, lwd=0)
axis(1, -3:1, -3:1)
axis(2, -1:1, -1:1, las=2)
#points(avp$country_mean[,1], cvs$country_mean[,1], pch=16, cex=gr^-3, col=rgb(0,0,1,0.1))
p5vec <- (1:37)[countries%in%p5]
invisible(sapply(p5vec, function(i) getE(-cvs$country_mean[,i], -avp$country_mean[,i], l1=0.95)))
text(-cm2, -cm1, iso3, col=ifelse(countries %in% p5, 'blue', grey(0.3, 0.8)), cex=gr^-1)
legend('bottomright', bty='n', pch=c(4,15,15), pt.cex=c(1,gr,gr), 
       col=c('blue', rgb(0,0,1,0.20), rgb(0,0,1,0.05)),
       legend=c('Median', '50% Region', '95% Region'))
    dev.off()

embedFonts('fig_1_ellipses.pdf')

#   SCRIPT END
