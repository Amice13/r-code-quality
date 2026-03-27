#   UNSC Debates over the Syrian Civil War plots
#       Fig B1. basic logistic scaling 
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


#   a function for logistic scaling
logfun <- function(x, y, a=0.5) log((x+a)/(y+a))

#   scale the texts (flip, i.e., multiply by -1)
log_cvs <- -1 * logfun(x=data_cvs$y, y=data_cvs$nw-data_cvs$y)
log_avp <- -1 * logfun(x=data_avp$y, y=data_avp$nw-data_avp$y)


#   due to this author's lazines this reuses code for Fig 2
tcvs <- as.data.frame(summary(dim_cvs, pars='theta')$summary)
tcvs$country <- data_cvs$country
tcvs$date_fact <- data_cvs$meet_fact
tcvs$date <- dates[tcvs$date]
tcvs$log <- log_cvs
tcvs <- tcvs[data_cvs$country %in% match(p5, countries), ]
tcvs_list <- split(tcvs, f=tcvs$country)
names(tcvs_list) <- p5

tavp <- as.data.frame(summary(dim_avp, pars='theta')$summary)
tavp$country <- data_avp$country
tavp$date_fact <- data_avp$meet_fact
tavp$date <- dates[tavp$date]
tavp$log <- log_avp
tavp <- tavp[data_avp$country %in% match(p5, countries), ]
tavp_list <- split(tavp, f=tavp$country)
names(tavp_list) <- p5



    pdf('fig_B1_logistic.pdf', width=10, height=10, family='Gill Sans Std')
x0 <- as.Date(c('2011-05-01', paste(2012:2016, '-01-01', sep='')))
par(xpd=T, mar=c(2,8.5,1,2.5), mfrow=c(2,1))
xl0 <- paste(c('1 May', rep('1 Jan.', 5)), substr(x0, 1, 4))
plot(0, 0, frame=F, xlim=range(x0), ylim=c(-4.5,1.5),
     xaxt='n', yaxt='n', xlab='', ylab='', main='')
axis(2, axTicks(2), axTicks(2), las=2)
text(as.Date('2010-03-01'), -1.5, 'Human\nRights\nViolations', pos=4)
for (i in 1:length(tcvs_list)) {
    tl <- loess.smooth(as.Date(tcvs_list[[i]]$date), tcvs_list[[i]]$log)
    lines(tl, col=ccl[i], lty=1)
    text(max(as.Date(dates)), tl$y[length(tl$y)], names(tcvs_list)[i], col=ccl[i], 
         pos=4, cex=gr^0) 
    points(as.Date(tcvs_list[[i]]$date), tcvs_list[[i]]$log, col=cca[i], pch=14+i,
           cex=gr^0)
}
#
par(mar=c(3,8.5,0,2.5))
plot(0, 0, frame=F, xlim=range(x0), ylim=c(-2.5, 3.5),
     xaxt='n', yaxt='n', xlab='', ylab='', main='')
axis(1, x0, xl0)
axis(2, axTicks(2), axTicks(2), las=2)
text(as.Date('2010-03-01'), +0.5, 'Intervention', pos=4)
for (i in 1:length(tavp_list)) {
    tl <- loess.smooth(as.Date(tavp_list[[i]]$date), tavp_list[[i]]$log)
    lines(tl, col=ccl[i], lty=1)
    text(max(as.Date(dates)), tl$y[length(tl$y)], names(tavp_list)[i], col=ccl[i], 
         pos=4, cex=gr^0)
    points(as.Date(tavp_list[[i]]$date), tavp_list[[i]]$log, col=cca[i], pch=14+i,
           cex=gr^0)
}
legend('bottom', horiz=T, pch=14+(1:5), p5, col=ccl, pt.cex=gr^0)
    dev.off()


embedFonts('fig_B1_logistic.pdf')

#   SCRIPT END
