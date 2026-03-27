##########################################################
## Estimating Media Slant from South Korean News Agencies
## using Sewol Ferry Disaster reports
## Written by Jong Hee Park (jongheepark@snu.ac.kr)
## Prepared "Fri Feb 26 10:51:15 2016"
##########################################################


##########################################################
## Citation Info: 
## Jong Hee Park, "Estimation of Media Slants in South Korean News Agencies 
## Using News Reports on the Sewol Ferry Disaster,"
## Korean Political Science Review, 2016, Vol.50, No.1.
##########################################################

##########################################################
## Disclaimer:
## The replciation code is checked under Mac.
## If you use it in a different OS,
## it might need some corrections. 
##########################################################


#############################
## load data and packages
#############################
load("news_partisan_weight.RData")
load("tv_partisan_weight.RData")
x <- c("Ruchardet", "KoNLP", "tm", "SnowballC", "RColorBrewer", "squash", 
       "ggplot2", "arules", "extrafont",  "xtable",  "rjags", "MCMCpack")
lapply(x, require, character.only=T)

#############################
## compare densities
#############################
par(mfrow=c(1,2))
## plot 1
plot(density(dem.partisan$count), xlab="", ylab="분포", main="정당별 분포 (빈도)", ylim=c(0, 0.07), lwd=1, lty=2, col="grey20")
lines(density(rep.partisan$count), lwd=1, col="grey60", lty=1)
legend("topright", legend=c("새누리", "새정치연합"), lwd=1, lty=c(1, 2), col=c("grey60", "grey20"))
## plot 2
plot(density( c(unlist(y.count.list), unlist(y.count.tv.list))),
     xlab="", ylab="분포", main="미디어 전체 분포", xlim=c(-10, 1), ylim=c(0, 5), 
    lwd=1, lty=2, col="grey20")
lines(density( c(unlist(y.weight.list), unlist(y.weight.tv.list))), lwd=2, col="grey60", lty=1)
legend("topleft", legend=c("상대빈도", "가중 상대빈도"), lwd=1, lty=c(1, 2), col=c("grey60", "grey20"))

#############################
# Model 1 raw count
#############################
Sewol <- data.frame(y = c(unlist(y.count.list), unlist(y.count.tv.list)))
Sewol$Date <- c(unlist(lapply(Dem.day, rownames)), unlist(lapply(Dem.tv.day, rownames)))
Sewol$Dates <- as.numeric(factor(julian(as.Date(Sewol$Date, format="%Y-%m-%d"), origin=as.Date("2014-04-16"))))
Sewol$nPollHouse <- c(rep(1:N.news, unlist(lapply(Dem.day, nrow))), rep((N.news + 1):(N.news + N.tv), unlist(lapply(Dem.tv.day, nrow))))
n.house <- length(unique(Sewol$nPollHouse))
n.period <- length(unique(Sewol$Dates))
mean.y <- sapply(1:n.period, function(i){mean(Sewol$y[Sewol$Dates == i])})
std.y <- (mean.y - mean(mean.y))/sd(mean.y)
trend <- 1:n.period

## come back to original analysis
n.house <- length(unique(Sewol$nPollHouse))
n.period <- length(unique(Sewol$Dates))
Sewol.var <- var(Sewol$y, na.rm=TRUE)
a01 <- mean(mean.y) - 1.96* mean(sqrt(Sewol.var))
a02 <- mean(mean.y) + 1.96* mean(sqrt(Sewol.var))

foo <- list(y=Sewol$y, 
            date=Sewol$Dates,
            nPollHouse=as.integer(Sewol$nPollHouse),
            n.poll=length(na.exclude(Sewol$y)),
            n.period=n.period, n.house=n.house,
            a01 = a01, a02 = a02, alpha= rep(NA, n.period))

initfunc <- function(){
  house <- rnorm(n=n.house, sd=.05)
  n.period <- n.period
  alpha <- c(runif(1, a01, a02), runif(n=n.period - 2), NA)
  sigma <- runif(n=1, 0, 0.1)
  sigma.y <- runif(n=1, 0, 1)
  list(alpha=alpha, house=house, sigma=sigma, sigma.y = sigma.y)
}
model <- jags.model("Sewol.bug", data = foo, inits = initfunc, n.chains = 1, n.adapt=1000)
output1 <- coda.samples(model=model,variable.names=c("alpha","house","sigma","sigma.y"), n.iter=5000, thin=5)

alphaFirst <- output1[[1]][,1:n.period]
houseFirst <- output1[[1]][,(n.period + 1):(n.period + n.house)]
sigmaFirst <- output1[[1]][,(n.period + n.house + 1):(n.period + n.house + 2)]
  
alpha.mean <- apply(alphaFirst,2,mean)
alpha.ci <- apply(alphaFirst,2,quantile,c(.025,.975))

house.mean <- apply(houseFirst,2,mean)
house.ci <- apply(houseFirst,2,quantile,c(.025,.975))
names(house.mean) <- c(as.character(news.names), as.character(tv.names))

out1 <- data.frame(mean = house.mean, lower = house.ci[1,], upper = house.ci[2,])

#############################
# Model 2 weighted count
#############################
Sewol <- data.frame(y = c(unlist(y.weight.list), unlist(y.weight.tv.list))*10)
Sewol$Date <- c(unlist(lapply(Dem.day, rownames)), unlist(lapply(Dem.tv.day, rownames)))
Sewol$Dates <- as.numeric(factor(julian(as.Date(Sewol$Date, format="%Y-%m-%d"), origin=as.Date("2014-04-16"))))
Sewol$nPollHouse <- c(rep(1:N.news, unlist(lapply(Dem.day, nrow))), rep((N.news + 1):(N.news + N.tv), unlist(lapply(Dem.tv.day, nrow))))
n.house <- length(unique(Sewol$nPollHouse))
n.period <- length(unique(Sewol$Dates))
mean.y <- sapply(1:n.period, function(i){mean(Sewol$y[Sewol$Dates == i])})
std.y <- (mean.y - mean(mean.y))/sd(mean.y)
trend <- 1:n.period

## come back to original analysis
n.house <- length(unique(Sewol$nPollHouse))
n.period <- length(unique(Sewol$Dates))
Sewol.var <- var(Sewol$y, na.rm=TRUE)
a01 <- mean(mean.y) - 1.96* mean(sqrt(Sewol.var))
a02 <- mean(mean.y) + 1.96* mean(sqrt(Sewol.var))

foo <- list(y=Sewol$y, 
            date=Sewol$Dates,
            nPollHouse=as.integer(Sewol$nPollHouse),
            n.poll=length(na.exclude(Sewol$y)),
            n.period=n.period, n.house=n.house,
            a01 = a01, a02 = a02, alpha= rep(NA, n.period))

initfunc <- function(){
  house <- rnorm(n=n.house, sd=.05)
  n.period <- n.period
  alpha <- c(runif(1, a01, a02), runif(n=n.period - 2), NA)
  sigma <- runif(n=1, 0, 0.1)
  sigma.y <- runif(n=1, 0, 1)
  list(alpha=alpha, house=house, sigma=sigma, sigma.y = sigma.y)
}
model <- jags.model("Sewol.bug", data = foo, inits = initfunc, n.chains = 1, n.adapt=1000)
output2 <- coda.samples(model=model,variable.names=c("alpha","house","sigma","sigma.y"), n.iter=5000, thin=5)

alphaSecond <- output2[[1]][,1:n.period]
houseSecond <- output2[[1]][,(n.period + 1):(n.period + n.house)]
sigmaSecond <- output2[[1]][,(n.period + n.house + 1):(n.period + n.house + 2)]
  
alpha.mean2 <- apply(alphaSecond,2,mean)
alpha.ci2 <- apply(alphaSecond,2,quantile,c(.025,.975))

house.mean2 <- apply(houseSecond,2,mean)
house.ci2 <- apply(houseSecond,2,quantile,c(.025,.975))
names(house.mean2) <- c(as.character(news.names), as.character(tv.names))

out2 <- data.frame(mean = house.mean2, lower = house.ci2[1,], upper = house.ci2[2,])
###################################
## plot the trend
###################################
color.list0 <- colorRampPalette(brewer.pal(9,"RdYlBu"))(n.house) 
color.list <- color.list0[order(house.mean)]
mean.y <- sapply(1:n.period, function(i){mean(Sewol$y[Sewol$Dates == i])})

d <- alpha.mean
plot(1: n.period,   d, type="n", axes=F,     xlab="", ylab= "당파적 문구 상대빈도", ylim=c(-12, 2),
     yaxs="i",      xaxs="i", main = "여론의 시간적 변화")
lines(1:n.period,   alpha.mean, lty=1, lwd=3, col=addTrans("grey20", 100))
lines(1:n.period, mean.y, lwd=0.5, lty=1, col="forestgreen")
lines(1:n.period, alpha.mean2, lwd=1, lty=1, col=addTrans("navy", 200))
abline(h=mean(d), lty=3, col="red")

text(which.max(d), max(d), labels=substr(Sewol$Date[which.max(d)], 6, 10), cex=0.4, adj=c(-0.1, -1))
text(which.min(d), min(d), labels=substr(Sewol$Date[which.min(d)], 6, 10), cex=0.4, adj=c(-0.1, -1))
ymin <- min(d); tmin <- which.min(d); ymax <- max(d); tmax<-which.max(d);
points(x=c(tmin,tmax), y=c(ymin,ymax), pch=19, col=c(addTrans("red", 200),addTrans("blue", 200)), cex=1.5)
local.min <- c(13, 99, 133, 258, 352); y.local.min <- alpha.mean[local.min]
points(x=local.min, y= y.local.min, pch=19, col=addTrans("red", 60), cex=1.5)
text(local.min, y.local.min, labels=substr(Sewol$Date[local.min], 6, 10), cex=0.4, adj=c(-0.1, -1))
local.max <- c(67, 147, 165, 217,  288); y.local.max <- alpha.mean[local.max]
points(x=local.max, y= y.local.max, pch=19, col=addTrans("blue", 60), cex=1.5)
text(local.max, y.local.max, labels=substr(Sewol$Date[local.max], 6, 10), cex=0.4, adj=c(-0.1, -1))

opar=par(ps=8); axis(2); 
axis(1, seq(1, n.period, by=3), 
     labels=substr(sort(unique(Sewol$Date))[seq(1, n.period, by=3)], 6, 10), cex.axis= 0.6, las=2)
par(xpd=TRUE)
legend(x=300, y=-4, legend=c("추세 (빈도)", "추세 (가중빈도)",  "일일평균 (빈도)", "평균"),
       col=c(addTrans("grey20", 100), addTrans("navy", 200), "forestgreen", "red"),
       cex=1, lty=c(1, 1, 1, 3), lwd=c(3, 1, 0.5, 1), bty="n")

## Partisan Trend with event 
png(width = 3000, height = 1500, filename = "web.png", res = 300)
d <- alpha.mean2
plot(1: n.period,   d, type="n", axes=F,     xlab="", ylab= "경도", ylim=c(-12, 2),
     yaxs="i",      xaxs="i", main = "세월호 참사 언론보도의 시간적 변화")
mtext("(정부비판 성향 강할수록 음수)", cex=0.8)
lines(1:n.period, alpha.mean2, lwd=1, lty=1, col=addTrans("navy", 200))
xpts <- which(is.element(as.character(unique(Sewol$Date)), sewol.chro$Date))
ypts <- as.numeric(d[xpts])
points(xpts, y=ypts, pch=19, cex=1.5, col=addTrans("indianred", 100))
words=sewol.chro$Event[1:42]; x=xpts; y=ypts; cex=0.3
lay <- wordlayout(x, y, words, cex=cex)
lay[,1] <- abs(lay[,1])
lay[11:42, 2] <- lay[11:42, 2]*3
for (i in 1:length(x)) {
    xl <- lay[i, 1]
    yl <- lay[i, 2]
    w <- lay[i, 3]
    h <- lay[i, 4]
    points(x[i], y[i], pch = 16, col = "red", cex = 0.5)
    if (x[i] < xl || x[i] > xl + w || y[i] < yl || y[i] > 
        yl + h) {
        nx <- xl + 0.5 * w
        ny <- yl + 0.5 * h
        lines(c(x[i], nx), c(y[i], ny), col = "grey20", lwd=0.2)
    }
}
text(lay[, 1] + 0.5 * lay[, 3], lay[, 2] + 0.5 * lay[, 4], words, cex = cex)
opar=par(ps=8); axis(2); 
axis(1, seq(1, n.period, by=3), 
     labels=substr(sort(unique(Sewol$Date))[seq(1, n.period, by=3)], 6, 10), cex.axis= 0.6, las=2)
dev.off()



###################################
## Plot the slant
###################################
## Data for the first plot
out1 <- data.frame(mean = house.mean, lower = house.ci[1,], upper = house.ci[2,])
data1 <-data.frame(Media= rownames(out1), Mean = out1$mean, Upper = out1$upper, Lower = out1$lower)
## Data for the second plot
out2 <- data.frame(mean = house.mean2, lower = house.ci2[1,], upper = house.ci2[2,])
data2 <-data.frame(Media= rownames(out2), Mean = out2$mean, Upper = out2$upper, Lower = out2$lower)
    
rownames(data1) <- data1$Media <- c(as.character(news.names), as.character(tv.names))
rownames(data1)[22] <- data1$Media[22] <- "M B C"
p1 <- ggplot(data = data1, aes(x = reorder(Media, Mean), y = Mean, ymin = Lower, ymax = Upper)) +
    geom_point(alpha = 1/2, position = position_dodge(width = 0.4),
               aes(size = 3, colour = Mean)) + scale_colour_gradient(low = "blue", high= "red") + 
        geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, aes(colour = Mean)) +
            coord_flip() +
                xlab("매체") + ylab("경도") +
                    ggtitle("매체의 당파적 경도 (빈도)") + 
                        theme_bw() +
                            theme(legend.position = "none",
                                  panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
                                  panel.grid.major.x = element_blank(),
                                  panel.grid.minor.x = element_blank()) +
                                      geom_hline(yintercept=c(median(data1$Mean)), linetype="dotted")

p1

rownames(data2) <- data2$Media <- c(as.character(news.names), as.character(tv.names))
rownames(data2)[22] <- data2$Media[22] <- "M B C"
p2 <- ggplot(data = data2, aes(x = reorder(Media, Mean), y = Mean, ymin = Lower, ymax = Upper)) +
    geom_point(alpha = 1/2, position = position_dodge(width = 0.4),
               aes(size = 3, colour = Mean)) + scale_colour_gradient(low = "blue", high= "red") + 
        geom_errorbar(position = position_dodge(width = 0.2), width = 0.1, aes(colour = Mean)) +
            coord_flip() +
                xlab("매체") + ylab("경도") +
                    ggtitle("매체의 당파적 경도 (가중빈도)") + 
                        theme_bw() +
                            theme(legend.position = "none",
                                  panel.grid.major.y = element_line(colour = "grey", linetype = "dashed"),
                                  panel.grid.major.x = element_blank(),
                                  panel.grid.minor.x = element_blank()) +
                                      geom_hline(yintercept=c(median(data2$Mean)), linetype="dotted")

p2


#######################################################################################
## Media outlet-specific analysis
#######################################################################################
## newspaper only 
SewolNews <- data.frame(y = c(unlist(y.weight.list))*10)
SewolNews$Date <- c(unlist(lapply(Dem.day, rownames)))
SewolNews$Dates <- as.numeric(factor(julian(as.Date(SewolNews$Date, format="%Y-%m-%d"),
                                                 origin=as.Date("2014-04-16"))))
SewolNews$nPollHouse <- c(rep(1:N.news, unlist(lapply(Dem.day, nrow))))
n.house <- length(unique(SewolNews$nPollHouse))
n.period <- length(unique(SewolNews$Dates))
mean.y.news <- sapply(1:n.period, function(i){mean(SewolNews$y[SewolNews$Dates == i])})
std.y.news <- (mean.y.news - mean(mean.y.news))/sd(mean.y.news)
trend <- 1:n.period

n.house <- length(unique(SewolNews$nPollHouse))
n.period <- length(unique(SewolNews$Dates))
SewolNews.var <- var(SewolNews$y, na.rm=TRUE)

foo <- list(y=SewolNews$y, 
            date=SewolNews$Dates,
            nPollHouse=as.integer(SewolNews$nPollHouse),
            n.poll=length(na.exclude(SewolNews$y)),
            n.period=n.period, n.house=n.house,
            a01 = a01, a02 = a02, alpha= rep(NA, n.period))

initfunc <- function(){
  house <- rnorm(n=n.house, sd=.05)
  n.period <- n.period
  alpha <- c(runif(1, a01, a02), runif(n=n.period - 2), NA)
  sigma <- runif(n=1, 0, 0.1)
  sigma.y <- runif(n=1, 0, 1)
  list(alpha=alpha, house=house, sigma=sigma, sigma.y = sigma.y)
}
model <- jags.model("~/Dropbox/Sewol/Rcode/Sewol.bug", data = foo, inits = initfunc, n.chains = 1, n.adapt=1000)
output3 <- coda.samples(model=model,variable.names=c("alpha","house","sigma","sigma.y"), n.iter=5000, thin=5)

alphaThird <- output3[[1]][,1:n.period]
houseThird <- output3[[1]][,(n.period + 1):(n.period + n.house)]
sigmaThird <- output3[[1]][,(n.period + n.house + 1):(n.period + n.house + 2)]
  
alpha.mean.news <- apply(alphaThird,2,mean)
alpha.ci.news <- apply(alphaThird,2,quantile,c(.025,.975))

house.mean3 <- apply(houseThird,2,mean)
house.ci3 <- apply(houseThird,2,quantile,c(.025,.975))
names(house.mean3) <- c(as.character(news.names))

out3 <- data.frame(mean = house.mean3, lower = house.ci3[1,], upper = house.ci3[2,])

## tv only 
TV.indicator <- 1:3
N.TV <- 3
SewolTV <- data.frame(y = c(unlist(y.weight.tv.list[TV.indicator]))*10)
SewolTV$Date <- c(unlist(lapply(Dem.tv.day[TV.indicator], rownames)))
SewolTV$Dates <- as.numeric(factor(julian(as.Date(SewolTV$Date, format="%Y-%m-%d"),
                                                 origin=as.Date("2014-04-16"))))
SewolTV$nPollHouse <- c(rep(1:N.TV, unlist(lapply(Dem.tv.day[TV.indicator], nrow))))
n.house <- length(unique(SewolTV$nPollHouse))
n.period <- length(unique(SewolTV$Dates))
mean.y.TV <- sapply(1:n.period, function(i){mean(SewolTV$y[SewolTV$Dates == i])})
std.y.TV <- (mean.y.TV - mean(mean.y.TV))/sd(mean.y.TV)
trend <- 1:n.period

## come back to original analysis
n.house <- length(unique(SewolTV$nPollHouse))
n.period <- length(unique(SewolTV$Dates))
SewolTV.var <- var(SewolTV$y, na.rm=TRUE)

foo <- list(y=SewolTV$y, 
            date=SewolTV$Dates,
            nPollHouse=as.integer(SewolTV$nPollHouse),
            n.poll=length(na.exclude(SewolTV$y)),
            n.period=n.period, n.house=n.house,
            a01 = a01, a02 = a02, alpha= rep(NA, n.period))

initfunc <- function(){
  house <- rnorm(n=n.house, sd=.05)
  n.period <- n.period
  alpha <- c(runif(1, a01, a02), runif(n=n.period - 2), NA)
  sigma <- runif(n=1, 0, 0.1)
  sigma.y <- runif(n=1, 0, 1)
  list(alpha=alpha, house=house, sigma=sigma, sigma.y = sigma.y)
}
model <- jags.model("~/Dropbox/Sewol/Rcode/Sewol.bug", data = foo, inits = initfunc, n.chains = 1, n.adapt=1000)
output4 <- coda.samples(model=model,variable.names=c("alpha","house","sigma","sigma.y"), n.iter=5000, thin=5)


alphaFourth <- output4[[1]][,1:n.period]
houseFourth <- output4[[1]][,(n.period + 1):(n.period + n.house)]
sigmaFourth <- output4[[1]][,(n.period + n.house + 1):(n.period + n.house + 2)]
  
alpha.mean.tv <- apply(alphaFourth,2,mean)
alpha.ci.tv <- apply(alphaFourth,2,quantile,c(.025,.975))

house.mean4 <- apply(houseFourth,2,mean)
house.ci4 <- apply(houseFourth,2,quantile,c(.025,.975))
names(house.mean4) <- c(as.character(tv.names)[1:3])

out4 <- data.frame(mean = house.mean4, lower = house.ci4[1,], upper = house.ci4[2,])

## cable only 
Cable.indicator <- 4:8
N.cable <- 5
SewolCable <- data.frame(y = c(unlist(y.weight.tv.list[Cable.indicator]))*10)
SewolCable$Date <- c(unlist(lapply(Dem.tv.day[Cable.indicator], rownames)))
SewolCable$Dates <- as.numeric(factor(julian(as.Date(SewolCable$Date, format="%Y-%m-%d"),
                                                 origin=as.Date("2014-04-16"))))
SewolCable$nPollHouse <- c(rep(1:N.cable, unlist(lapply(Dem.tv.day[Cable.indicator], nrow))))
mean.y <- sapply(1:n.period, function(i){mean(SewolCable$y[SewolCable$Dates == i])})
std.y <- (mean.y - mean(mean.y))/sd(mean.y)
trend <- 1:n.period
n.house <- length(unique(SewolCable$nPollHouse))
n.period <- length(unique(SewolCable$Dates))
mean.y <- sapply(1:n.period, function(i){mean(SewolCable$y[SewolCable$Dates == i])})
std.y <- (mean.y - mean(mean.y))/sd(mean.y)
trend <- 1:n.period

n.house <- length(unique(SewolCable$nPollHouse))
n.period <- length(unique(SewolCable$Dates))
SewolCable.var <- var(SewolCable$y, na.rm=TRUE)

foo <- list(y=SewolCable$y, 
            date=SewolCable$Dates,
            nPollHouse=as.integer(SewolCable$nPollHouse),
            n.poll=length(na.exclude(SewolCable$y)),
            n.period=n.period, n.house=n.house,
            a01 = a01, a02 = a02, alpha= rep(NA, n.period))

initfunc <- function(){
  house <- rnorm(n=n.house, sd=.05)
  n.period <- n.period
  alpha <- c(runif(1, a01, a02), runif(n=n.period - 2), NA)
  sigma <- runif(n=1, 0, 0.1)
  sigma.y <- runif(n=1, 0, 1)
  list(alpha=alpha, house=house, sigma=sigma, sigma.y = sigma.y)
}
model <- jags.model("~/Dropbox/Sewol/Rcode/Sewol.bug", data = foo, inits = initfunc, n.chains = 1, n.adapt=1000)
output5 <- coda.samples(model=model,variable.names=c("alpha","house","sigma","sigma.y"), n.iter=5000, thin=5)


alphaFifth <- output5[[1]][,1:n.period]
houseFifth <- output5[[1]][,(n.period + 1):(n.period + n.house)]
sigmaFifth <- output5[[1]][,(n.period + n.house + 1):(n.period + n.house + 2)]
  
alpha.mean.cable <- apply(alphaFifth,2,mean)
alpha.ci.cable <- apply(alphaFifth,2,quantile,c(.025,.975))

house.mean5 <- apply(houseFifth,2,mean)
house.ci5 <- apply(houseFifth,2,quantile,c(.025,.975))
names(house.mean5) <- c(as.character(tv.names)[Cable.indicator])

out5 <- data.frame(mean = house.mean5, lower = house.ci5[1,], upper = house.ci5[2,])


#######################################################################################
## Draw a plot with all the results
#######################################################################################
n.period.tv <- which(is.element(sort(unique(Sewol$Date)), sort(unique(SewolTV$Date))))
n.period.news <- which(is.element(sort(unique(Sewol$Date)), sort(unique(SewolNews$Date))))
n.period.cable <- which(is.element(sort(unique(Sewol$Date)), sort(unique(SewolCable$Date))))

n.house <- length(unique(Sewol$nPollHouse))
n.period <- length(unique(Sewol$Dates))
xlocs <- jitter(Sewol$Dates)
plot(1:n.period,   alpha.mean2, type="n",  axes=F,     xlab="", ylab= "당파적 경도",
     ylim=c(-11, 1), yaxs="i",      xaxs="i", main = "매체별 당파적 트렌드")
lines(1:n.period,   alpha.mean2, lty=1, lwd=5, col=addTrans("grey20", 100))
lines(n.period.news,   alpha.mean.news, lty=1, lwd=3, col=addTrans("navy", 150))
lines(n.period.tv,   alpha.mean.tv, lty=1, lwd=2, col=addTrans("purple", 200))
lines(n.period.cable,   alpha.mean.cable, lty=1, lwd=1, col=addTrans("forestgreen", 200))

opar=par(ps=8)
axis(1, seq(1, n.period, by=3), 
     labels=substr(unique(Sewol$Date)[seq(1, n.period, by=3)], 6, 10), cex = 0.8, las=2)
axis(2); grid()

par(xpd=TRUE)
legend("bottomright", legend=c("전체 (28개사)", "신문 (19 개사)", "공중파 (3개사)", "종편 (5개사)"),
       col=c(addTrans("grey20", 100),addTrans("navy", 150),
           addTrans("purple", 150),
           addTrans("forestgreen", 150)), 
       cex=1, lty=1, lwd=c(5, 3, 2, 1), bty="n")

