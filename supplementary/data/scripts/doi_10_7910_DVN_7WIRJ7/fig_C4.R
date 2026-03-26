#   UNSC Debates over the Syrian Civil War plots
#       Fig C4. fancy CvS
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


#   ----------------

t1 <- summary(dim_cvs, pars='theta')$summary

p5_vec <- match(p5, countries)
p5_col <- ccl
p5_pch <- 15:19 
names(p5_col) <- names(p5_pch) <- p5


idnos <- unique(d$meeting)[order(unique(d$meeting))]
meetings <- unique(d$date)[order(unique(d$date))]
x0 <- as.Date(c('2011-05-01', paste(2012:2016, '-01-01', sep='')))
l0 <- c('1 May 2011', paste('1 January', 2012:2016))
xl <- range(x0)
yl <- c(-gr^-3,1+gr^-4)

inc <- dist(xl)/(length(meetings))
halfinc <- inc/2

p5_pos <- c(-2, -1, 0, 1, 2)*halfinc*gr^-3
names(p5_pos) <- p5

drawbox <-  
    function(m, h, color, lwdt=gr^-2)
    {
        h <- as.numeric(h)
        m <- as.Date(m)
        h <- h-h*gr^-5
        tx <- c(m-h, m+h, m+h, m-h)
        ty <- c(0,0,1,1)
        polygon(tx, ty, border=color, col=color, lwd=lwdt)
    }


m0 <- xl[1] + halfinc + (0:(length(meetings)-1))*inc
mg <- grey(0.925)
mga <- grey(0.65)



#   move <- -3
move <- + 1
scalefun <- function(x, d=10.2, a=0.5) { x/d + a }
unscale <- function(x, d=10.2, a=0.5) { d*(x-a) }
#   y0 <- scalefun(c(-2,0,2,4,6,8)+move)
y0 <- scalefun(c(-6,-4,-2,0,2,4)+move)


    pdf('fig_C4_fancy_cvs.pdf', width=10, height=10*(gr^-2),
        family='Gill Sans Std')
par(xpd=T, mar=c(0.38,0.1,1,0.1))
plot(0,0,type='n',frame=F,xaxt='n',yaxt='n',xlab='',ylab='',xlim=xl, ylim=yl)
lines(xl, rep(yl[2],2))
invisible(sapply(x0, function(tx) lines(rep(tx,2), c(yl[2], yl[2]+gr^-8))))
text(x0, rep(yl[2]+gr^-8,length(x0)), l0, pos=3, cex=gr^-1)
invisible(sapply(1:length(m0), function(i) lines(c(m0[i], as.Date(meetings[i])), c(1, yl[2]), col=mg)))
invisible(sapply(m0, function(tm) drawbox(tm, halfinc, color=mg)))
text(m0, rep(1.0, length(m0)), idnos, cex=gr^-1, col=mga, pos=1)
points(as.Date(meetings), rep(yl[2],length(meetings)), pch=16, cex=gr^-1, col=grey(0.0))
text(rep(xl[1]-as.numeric(2*halfinc*gr^-3, 5)), y0, round(unscale(y0)-move), cex=gr^-1, pos=2)
invisible(sapply(y0, function(ty) { lines(xl[1]-c(1,2)*halfinc*gr^-3, rep(ty,2)) } ))
legend('bottom', horiz=F, ncol=3, bg=mg, box.col='white',
       pch=c(p5_pch, 6, 16, rep(1,2)),
       col=c(p5_col, 'white', rep(mga,3)), 
       lwd=c(rep(gr^1,5),0,0,gr^1, gr^-2), 
       lty=c(rep(1,5), 0, 0, rep(1,2)),
       legend=c(p5, '', 'Median', '50% Interval', '95% Interval'),
       cex=gr^-1,
       pt.cex=c(rep(gr^-1,5), 0, 1, 0,0))
    for (i in 1:nrow(t1)) {
        if (data_avp$country[i] %in% p5_vec) {
            tc <- match(data_avp$country[i], p5_vec)
            tm <- m0[data_avp$meet_fact[i]] + p5_pos[tc]
            ty1 <- scalefun(-t1[i,c('2.5%', '97.5%')]+move)
            ty2 <- scalefun(-t1[i,c('25%', '75%')]+move)
            tym <- scalefun(-t1[i, '50%']+move)
            lines(rep(tm,2), ty1, col=p5_col[tc], lwd=gr^-2)
            lines(rep(tm,2), ty2, col=p5_col[tc], lwd=gr^1)
            points(tm, tym, pch=p5_pch[tc], col=p5_col[tc], cex=gr^-1)
        }
    }
dev.off()  


embedFonts('fig_C4_fancy_cvs.pdf')

# SCRIPT END
