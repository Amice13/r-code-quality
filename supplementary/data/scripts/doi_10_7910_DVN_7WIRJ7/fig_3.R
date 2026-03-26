#   UNSC Debates over the Syrian Civil War plots
#       Fig 3. P5
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

#   P5 colors
cnt <- c('China', 'France', 'Russia', 'UK', 'USA') 
ccl <- c('red3', 'blue', 'darkorange2', 'darkorchid3', 'navyblue')
names(ccl) <- cnt
cca <- apply(col2rgb(ccl), 2, function(i) rgb(i[1],i[2],i[3],75,maxColorValue=255)) 


#   unique dates
dates <- substr(unique(d$date), 1, 1e1)
dates <- dates[order(dates)]


#   Dim 1 (CvS) posterior summaries
tcvs <- as.data.frame(summary(dim_cvs, pars='theta')$summary)
#   flip the dim
tcvs[, 1:8] <- -1 * tcvs[, 1:8]
tcvs$country <- data_cvs$country
tcvs$date_fact <- data_cvs$meet_fact
tcvs$date <- dates[tcvs$date]
tcvs <- tcvs[data_cvs$country %in% match(p5, countries), ]
tcvs_list <- split(tcvs, f=tcvs$country)
names(tcvs_list) <- p5

#   Dim 2 (AvP) posterior summaries
tavp <- as.data.frame(summary(dim_avp, pars='theta')$summary)
#   flip the dim
tavp[, 1:8] <- -1 * tavp[, 1:8]
tavp$country <- data_avp$country
tavp$date_fact <- data_avp$meet_fact
tavp$date <- dates[tavp$date]
tavp <- tavp[data_avp$country %in% match(p5, countries), ]
tavp_list <- split(tavp, f=tavp$country)
names(tavp_list) <- p5


#   -----------------
#       Fig 3. P5
#   -----------------


    pdf('fig_3_P5.pdf', width=10, height=10, family='Gill Sans Std')
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
par(xpd=T, mar=c(2.5,8.5,0.0,6.5))
plot(0, 0, frame=F, xlim=range(x0), ylim=yl-2,
     xaxt='n', yaxt='n', xlab='', ylab='', main='')
axis(2, axTicks(2), axTicks(2), las=2)
axis(2, mean(axTicks(2)), '', las=2, line=2, lwd=0)
text(as.Date('2010-07-01'), -1,'Human\nRights\nViolations', pos=4)
for (i in 1:length(tcvs_list)) {
    td <- as.Date(tcvs_list[[i]]$date)
    ld <- nrow(tcvs_list[[i]])
    xp <- c(td, td[ld:1])
    yp <- c(tcvs_list[[i]]$'25%', tcvs_list[[i]]$'75%'[ld:1])
    points(td, tcvs_list[[i]]$'50%', pch=14+i, col=ccl[i], cex=gr^-1)
    polygon(xp, yp, col=cca[i], border=grey(0,0))
    text(as.Date('2015-12-22'), tcvs_list[[i]]$mean[ld], names(tavp_list)[i],
         col=ccl[i], pos=4, cex=gr^0)
}
par(mar=c(2.5,8.5,0,6.5))
plot(0, 0, frame=F, xlim=range(x0), ylim=yl-1,
     xaxt='n', yaxt='n', xlab='', ylab='', main='')
axis(1, x0, xl0)
axis(2, axTicks(2), axTicks(2), las=2)
axis(2, mean(axTicks(2)), '', las=2, line=2, lwd=0)
text(as.Date('2010-07-01'), 0,'Intervention', pos=4)
for (i in 1:length(tavp_list)) {
    td <- as.Date(tavp_list[[i]]$date)
    ld <- nrow(tavp_list[[i]])
    xp <- c(td, td[ld:1])
    yp <- c(tavp_list[[i]]$'25%', tavp_list[[i]]$'75%'[ld:1])
    points(td, tavp_list[[i]]$'50%', pch=14+i, col=ccl[i], cex=gr^-1)
    polygon(xp, yp, col=cca[i], border=grey(0,0))
    text(as.Date('2015-12-22'), tavp_list[[i]]$mean[ld], names(tavp_list)[i],
         col=ccl[i], pos=4, cex=gr^0)
}
    dev.off()

    embedFonts('fig_3_P5.pdf')

#   SCRIPT END
