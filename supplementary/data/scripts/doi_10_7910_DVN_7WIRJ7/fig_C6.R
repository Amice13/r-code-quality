#   UNSC Debates over the Syrian Civil War plots
#       Fig C6. Panel CvS
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


#   load the data
load('sim_cvs.RData')

#   NOTE: Later in the script the dimension is flipped (multiplied with -1).


#   load the original data
load('d20160511.RData')


#   shorten selected state names
d$country[grep('^Russ', d$country)] <- 'Russia'
d$country[grep('^United K', d$country)] <- 'UK'
d$country[grep('^United S', d$country)] <- 'USA'


meetings <- unique(d$date)[order(unique(d$date))]


tscvs <- split(data.frame(summary(dim_cvs, pars='theta')$summary,
                          meet_fact=data_cvs$meet_fact),
               f=data_cvs$country)


linefun <-
    function(x, mg=grey(0.5,0.25))
    {
        lines(as.Date(meetings[x$meet_fact]), -x$'X50.', col=mg)
    }


    pdf('fig_C6_panel_cvs.pdf', width=20, height=12.4,
        family='Gill Sans Std')
par(xpd=T, mar=c(0,2,5,0), mfrow=c(5,8), oma=c(1,0,0,1))
    plot(0,0,type='n',frame=F,xaxt='n',yaxt='n',xlab='',ylab='')
    text(0,0, 'Human\nRights\nViolations', cex=gr^2)
for (i in 1:length(tscvs)) {
    plot(0,0,xlim=range(as.Date(meetings)),ylim=c(-3.5,1),frame=F,xlab='',ylab='',
         xaxt='n', yaxt='n', main='')
    if (i==1) {axis(2, axTicks(2), axTicks(2), las=2)}
    mtext(countries[as.numeric(names(tscvs)[i])], side=3, col=grey(0, 0.7))
    invisible(lapply(tscvs, linefun, mg=grey(0,0.1)))
    lines(as.Date(meetings[tscvs[[i]]$meet_fact]), -tscvs[[i]]$'X50.', col='darkblue')
    points(as.Date(meetings[tscvs[[i]]$meet_fact]), -tscvs[[i]]$'X50.', col='darkblue',
           pch=16, cex=gr^0)
}
    dev.off()

    embedFonts('fig_C6_panel_cvs.pdf')

#   SCRIPT END
