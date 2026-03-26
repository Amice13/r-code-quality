###Kenya Seasonality###
#setwd("")

kis <- read.csv('agg_kis.csv')
kili <- read.csv('agg_kili.csv')
nai <- read.csv('agg_nai.csv')


kili <- kili[order(kili$yr, kili$wk), ]
kis <- kis[order(kis$yr, kis$wk), ]
nai <- nai[order(nai$yr, nai$wk), ]

# #create vector of blanks
# dates <- seq(as.Date("2006-01-01"), as.Date("2018-12-31"), by="week")
# dates <- as.data.frame(dates)
# dates$dates <- as.Date(dates$dates)
# dates$wk <- strftime(dates$dates, "%W")
# dates$yr <- strftime(dates$dates, "%Y")
# 
# kis <- merge(kis, dates, by = c('wk', 'yr'), all = TRUE)
# nai <- merge(nai, dates, by = c('wk', 'yr'), all = TRUE)
# dates$wk <- as.integer(dates$wk)
# dates$yr <- as.integer(dates$yr)
# kili <- merge(kili, dates, by = c('wk', 'yr'), all = TRUE)
# 
# kili <- kili[order(kili$yr, kili$wk), ]
# kis <- kis[order(kis$yr, kis$wk), ]
# nai <- nai[order(nai$yr, nai$wk), ]

kili$prop.pos <- ifelse(kili$prop.pos == 'NaN' | is.na(kili$prop.pos), 0, kili$prop.pos)
kis$prop.pos <- ifelse(kis$prop.pos == 'NaN' | is.na(kis$prop.pos), 0, kis$prop.pos)
nai$prop.pos <- ifelse(nai$prop.pos == 'NaN' | is.na(nai$prop.pos), 0, nai$prop.pos)


kili$prop.pos.avg <- kili$prop.pos
for(i in 3:(nrow(kili) - 2)){
  kili$prop.pos.avg[i] <- mean(kili$prop.pos[c(i - 2, i - 1, i, i + 1, i + 2)])
}

kis$prop.pos.avg <- kis$prop.pos
for(i in 3:(nrow(kis) - 2)){
  kis$prop.pos.avg[i] <- mean(kis$prop.pos[c(i - 2, i - 1, i, i + 1, i + 2)])
}

nai$prop.pos.avg <- nai$prop.pos
for(i in 3:(nrow(nai) - 2)){
  nai$prop.pos.avg[i] <- mean(nai$prop.pos[c(i - 2, i - 1, i, i + 1, i + 2)])
}



kili.threshold <- NULL
kili$ind <- 0
for(i in 2006:2018) {
  kili.threshold[i - 2005] <- mean(kili$prop.pos[which(kili$yr == i & !is.na(kili$RSV_test))], na.rm = TRUE)
  for(j in 1:nrow(kili)){
    kili$ind[j] <- ifelse(kili$prop.pos.avg[j] > kili.threshold[i - 2005] & kili$yr[j] == i, 1, kili$ind[j])
  }
}


kis.threshold <- NULL
kis$ind <- 0
for(i in 2006:2018) {
  kis.threshold[i - 2005] <- mean(kis$prop.pos[which(kis$yr == i  & !is.na(kis$RSV_test))], na.rm = TRUE)
  for(j in 1:nrow(kis)){
    kis$ind[j] <- ifelse(kis$prop.pos.avg[j] > kis.threshold[i - 2005] & kis$yr[j] == i, 1, kis$ind[j])
  }
}

nai.threshold <- NULL
nai$ind <- 0
for(i in 2006:2018) {
  nai.threshold[i - 2005] <- mean(nai$prop.pos[which(nai$yr == i  & !is.na(nai$RSV_test))], na.rm = TRUE)
  for(j in 1:nrow(nai)){
    nai$ind[j] <- ifelse(nai$prop.pos.avg[j] > nai.threshold[i - 2005] & nai$yr[j] == i, 1, nai$ind[j])
  }
}


write.csv(kili, 'agg_kili_ind_5avg.csv')
write.csv(kis, 'agg_kis_ind_5avg.csv')
write.csv(nai, 'agg_nai_ind_5avg.csv')

kili$col <- ifelse(kili$ind == 1, '#5ab4ac', '#fed9a6')
kis$col <- ifelse(kis$ind == 1, '#8dd3c7', '#ffffb3')
nai$col <- ifelse(nai$ind == 1, '#8dd3c7', '#ffffb3')


tiff("Kilifi_Plot_11062019.tiff", height=500, width=2000)
par(mar=c(5.1,4.1,4.1,4.1))
barplot(kili$RSV_test, ylab="Number of Tests", border=NA, ylim=c(0,60), col = kili$col)
par(new=T, mar=c(5.1,4.1,4.1,4.1))
plot(as.Date(kili$dates, format = c('%m/%d/%Y')), kili$prop.pos.avg, type="l", lwd=2, xlab="", yaxt="n", ylab="", xaxt="n")
axis(side=4, at=c(0.0,0.1,0.2,0.3,0.4, 0.5, 0.6), labels=c(0.0,0.1,0.2,0.3,0.4, 0.5, 0.6)) 
axis(side=1, 
     at=c(as.Date(kili$dates, format = c('%m/%d/%Y'))[seq(1,676, by=26)]), 
     labels=format(c(as.Date(kili$dates, format = c('%m/%d/%Y'))[seq(1,676, by=26)]),("%W-%Y")), las = 2)
mtext("% Positive", side=4, line=3)
dev.off()



tiff("Kisumu_Plot_11062019.tiff", height=500, width=2000)
par(mar=c(5.1,4.1,4.1,4.1))
barplot(kis$RSV_test, ylab="Number of Tests", border=NA, ylim=c(0,180), col = kis$col)
par(new=T, mar=c(5.1,4.1,4.1,4.1))
plot(as.Date(kis$dates, format = c('%m/%d/%Y')), kis$prop.pos.avg, type="l", lwd=2, xlab="", yaxt="n", ylab="", xaxt="n")
axis(side=4, at=c(0.0,0.1,0.2,0.3,0.4, 0.5, 0.6), labels=c(0.0,0.1,0.2,0.3,0.4, 0.5, 0.6)) 
axis(side=1, 
     at=c(as.Date(kili$dates, format = c('%m/%d/%Y'))[seq(1,678, by=26)]), 
     labels=format(c(as.Date(kili$dates, format = c('%m/%d/%Y'))[seq(1,678, by=26)]),("%W-%Y")), las = 2)
mtext("% Positive", side=4, line=3)
dev.off()




tiff("Nairobi_Plot_11062019.tiff", height=500, width=2000)
par(mar=c(5.1,4.1,4.1,4.1))
barplot(nai$RSV_test, ylab="Number of Tests", border=NA, ylim=c(0,100), col = nai$col)
par(new=T, mar=c(5.1,4.1,4.1,4.1))
plot(as.Date(nai$dates, format = c('%m/%d/%Y')), nai$prop.pos.avg, type="l", lwd=2, xlab="", yaxt="n", ylab="", xaxt="n")
axis(side=4, at=c(0.0,0.1,0.2,0.3,0.4, 0.5, 0.6), labels=c(0.0,0.1,0.2,0.3,0.4, 0.5, 0.6)) 
axis(side=1, 
     at=c(as.Date(kili$dates, format = c('%m/%d/%Y'))[seq(1,671, by=26)]), 
     labels=format(c(as.Date(kili$dates, format = c('%m/%d/%Y'))[seq(1,671, by=26)]),("%W-%Y")), las = 2)
mtext("% Positive", side=4, line=3)
dev.off()


########################
library(lubridate)
kili <- read.csv('agg_kili_ind_5avg.csv')
kili$col <- ifelse(is.na(kili$season), '#fdb462',  '#8dd3c7')

kili.npos <- rep(NA, 13)
kili.npos.in <- rep(NA, 13)
for(i in 2006:2018){
  kili.npos.in[i-2005] <- sum(kili$RSV_detect[which(kili$yr == i & kili$season == 1)])
  kili.npos[i-2005] <- sum(kili$RSV_detect[which(kili$yr == i)])
}

tiff("Kilifi_Plot_12272019.tiff", height=5, width=12, units = 'in', res = 300)
layout(rbind(1,2), heights=c(7,1))
par(mar=c(5.1,4.1,4.1,4.1), xpd = TRUE)
barplot(kili$RSV_test, ylab="Number of Tests", border=NA, ylim=c(0,60), col = kili$col)
# barplot(kili$RSV_test*kili$season, col = '#7fc97f', add = TRUE)
par(new=T, mar=c(5.1,4.1,4.1,4.1), xpd = TRUE)
plot(as.Date(kili$dates, format = c('%m/%d/%Y')), kili$prop.pos.avg, type="l", lwd=1.5, xlab="", yaxt="n", ylab="", xaxt="n")
points(as.Date(kili$dates, format = c('%m/%d/%Y')), kili$prop.pos.avg*kili$peak, col = '#e31a1c', pch = 8, cex = 1.5, lwd = 2)
axis(side=4, at=c(0.0,0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7, 0.8), labels=c(0.0,0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7, 0.8)) 
axis(side=1, 
     at = as.Date(kili$dates, format = c('%m/%d/%Y'))[which(kili$wk == 1 | kili$wk == 27)], 
     labels=paste0(kili$wk, '-', kili$yr, sep = '')[which(kili$wk == 1 | kili$wk == 27)], las = 2)
mtext("% Positive", side=4, line=3)
mtext('Week-Year', side = 1, line = 5)
# par(mar = c(0,0,0,0))
# plot.new()
# legend('center', legend = c('Peak', 'Season', 'Non-season'), fill = c(NA, '#8dd3c7', '#fdb462'), pch = c(8, NA, NA), 
#        col = c('#e31a1c', NA, NA), lwd = c(2,2,2), lty = c(NA, NA, NA), border = NA, bty = 'n', ncol = 3)
dev.off()

kis <- read.csv('agg_kis_ind_5avg.csv')
kis$col <- ifelse(is.na(kis$season), '#fdb462',  '#8dd3c7')
kis$epiweek <- epiweek(as.Date(kis$dates, format = c('%m/%d/%Y')))
write.csv(kis, 'Kisumu_Epiweek.csv')

kis.sens <- table(kis$season, by = kis$yr)
mean(kis.sens[2:13])

kis.npos <- rep(NA, 13)
kis.npos.in <- rep(NA, 13)
for(i in 2006:2018){
  kis.npos.in[i-2005] <- sum(kis$RSV_detect[which(kis$yr == i & kis$season == 1)], na.rm = TRUE)
  kis.npos[i-2005] <- sum(kis$RSV_detect[which(kis$yr == i)], na.rm = TRUE)
}

tiff("Kisumu_Plot_12272019.tiff", height=5, width=12, units = 'in', res = 300)
layout(rbind(1,2), heights=c(7,1))
par(mar=c(5.1,4.1,4.1,4.1))
barplot(kis$RSV_test, ylab="Number of Tests", border=NA, ylim=c(0,180), col = kis$col)
par(new=T, mar=c(5.1,4.1,4.1,4.1), xpd = TRUE)
plot(as.Date(kis$dates, format = c('%m/%d/%Y')), kis$prop.pos.avg, type="l", lwd=1.5, xlab="", yaxt="n", ylab="", xaxt="n")
points(as.Date(kis$dates, format = c('%m/%d/%Y')), kis$prop.pos.avg*kis$peak, col = '#e31a1c', pch = 8, cex = 1.5, lwd = 2)
axis(side=4, at=c(0.0,0.1,0.2,0.3), labels=c(0.0,0.1,0.2,0.3)) 
axis(side=1, 
     at = as.Date(kis$dates, format = c('%m/%d/%Y'))[which(kis$epiweek == 1 | kis$epiweek == 27)], 
     labels=paste0(kis$epiweek, '-', kis$yr, sep = '')[which(kis$epiweek == 1 | kis$epiweek == 27)], las = 2)
mtext("% Positive", side=4, line=3)
mtext('Week-Year', side = 1, line = 5)
# par(mar = c(0,0,0,0))
# plot.new()
# legend('center', legend = c('Peak', 'Season', 'Non-season'), fill = c(NA, '#8dd3c7', '#fdb462'), pch = c(8, NA, NA), 
#        col = c('#e31a1c', NA, NA), lwd = c(2,2,2), lty = c(NA, NA, NA), border = NA, bty = 'n', ncol = 3)
dev.off()





nai <- read.csv('agg_nai_ind_5avg.csv')
nai$epiweek <- epiweek(as.Date(nai$dates, format = c('%m/%d/%Y')))
write.csv(nai, 'naiumu_Epiweek.csv')

nai.npos <- rep(NA, 13)
nai.npos.in <- rep(NA, 13)
for(i in 2006:2018){
  nai.npos.in[i-2005] <- sum(nai$RSV_detect[which(nai$yr == i & nai$season == 1)], na.rm = TRUE)
  nai.npos[i-2005] <- sum(nai$RSV_detect[which(nai$yr == i)], na.rm = TRUE)
}

tiff("Nairobi_Plot_12272019.tiff", height=5, width=12, units = 'in', res = 300)
layout(rbind(1,2), heights=c(7,1))
par(mar=c(5.1,4.1,0,4.1))
barplot(nai$RSV_test, ylab="Number of Tests", border=NA, ylim=c(0,180), col = '#fdb462')
par(new=T, mar=c(5.1,4.1,0,4.1), xpd = TRUE)
plot(as.Date(nai$dates, format = c('%m/%d/%Y')), nai$prop.pos.avg, type="l", lwd=1.5, xlab="", yaxt="n", ylab="", xaxt="n")
axis(side=4, at=c(0.0,0.1,0.2,0.3, 0.4, 0.5), labels=c(0.0,0.1,0.2,0.3, 0.4, 0.5)) 
axis(side=1, 
     at = as.Date(kis$dates, format = c('%m/%d/%Y'))[which(kis$epiweek == 1 | kis$epiweek == 27)], 
     labels=paste0(kis$epiweek, '-', kis$yr, sep = '')[which(kis$epiweek == 1 | kis$epiweek == 27)], las = 2)
mtext("% Positive", side=4, line=3)
mtext('Week-Year', side = 1, line = 5)
par(mar = c(0,0,0,0))
plot.new()
legend('center', legend = c('Peak', 'Season', 'Non-season'), fill = c(NA, '#8dd3c7', '#fdb462'), pch = c(8, NA, NA), 
       col = c('#e31a1c', NA, NA), lwd = c(2,2,2), lty = c(NA, NA, NA), border = NA, bty = 'n', ncol = 3)
dev.off()


tiff("Combined_Plot_12272019.tiff", height=10, width=20, units = 'in', res = 300)
layout(rbind(1,2,3,4), heights=c(10,10,10,1))
par(mar=c(1,4,1,4), xpd = TRUE)
barplot(kili$RSV_test, ylab="Number of Tests", border=NA, ylim=c(0,60), col = kili$col)
# barplot(kili$RSV_test*kili$season, col = '#7fc97f', add = TRUE)
par(new=T, mar=c(1,4,1,4), xpd = TRUE)
plot(as.Date(kili$dates, format = c('%m/%d/%Y')), kili$prop.pos.avg, type="l", lwd=1.5, xlab="", yaxt="n", ylab="", xaxt="n")
points(as.Date(kili$dates, format = c('%m/%d/%Y')), kili$prop.pos.avg*kili$peak, col = '#e31a1c', pch = 8, cex = 1.5, lwd = 2)
axis(side=4, at=c(0.0,0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7, 0.8), labels=c(0.0,0.1,0.2,0.3,0.4, 0.5, 0.6, 0.7, 0.8)) 
axis(side=1, 
     at = as.Date(kili$dates, format = c('%m/%d/%Y'))[which(kili$wk == 1 | kili$wk == 27)], 
     labels=paste0(kili$wk, '-', kili$yr, sep = '')[which(kili$wk == 1 | kili$wk == 27)], las = 2)
mtext("% Positive", side=4, line=3)
mtext('Week-Year', side = 1, line = 5)
plot.new()
par(mar=c(1,4,1,4), xpd = FALSE)
barplot(kis$RSV_test, ylab="Number of Tests", border=NA, ylim=c(0,180), col = kis$col)
par(new=T, mar=c(1,4,1,4), xpd = TRUE)
plot(as.Date(kis$dates, format = c('%m/%d/%Y')), kis$prop.pos.avg, type="l", lwd=1.5, xlab="", yaxt="n", ylab="", xaxt="n")
points(as.Date(kis$dates, format = c('%m/%d/%Y')), kis$prop.pos.avg*kis$peak, col = '#e31a1c', pch = 8, cex = 1.5, lwd = 2)
axis(side=4, at=c(0.0,0.1,0.2,0.3), labels=c(0.0,0.1,0.2,0.3)) 
axis(side=1, 
     at = as.Date(kis$dates, format = c('%m/%d/%Y'))[which(kis$epiweek == 1 | kis$epiweek == 27)], 
     labels=paste0(kis$epiweek, '-', kis$yr, sep = '')[which(kis$epiweek == 1 | kis$epiweek == 27)], las = 2)
mtext("% Positive", side=4, line=3)
mtext('Week-Year', side = 1, line = 5)
plot.new()
par(mar=c(1,4,1,4))
barplot(nai$RSV_test, ylab="Number of Tests", border=NA, ylim=c(0,180), col = '#fdb462')
par(new=T, mar=c(1,4,1,4), xpd = TRUE)
plot(as.Date(nai$dates, format = c('%m/%d/%Y')), nai$prop.pos.avg, type="l", lwd=1.5, xlab="", yaxt="n", ylab="", xaxt="n")
axis(side=4, at=c(0.0,0.1,0.2,0.3, 0.4, 0.5), labels=c(0.0,0.1,0.2,0.3, 0.4, 0.5)) 
axis(side=1, 
     at = as.Date(kis$dates, format = c('%m/%d/%Y'))[which(kis$epiweek == 1 | kis$epiweek == 27)], 
     labels=paste0(kis$epiweek, '-', kis$yr, sep = '')[which(kis$epiweek == 1 | kis$epiweek == 27)], las = 2)
mtext("% Positive", side=4, line=3)
mtext('Week-Year', side = 1, line = 5)
par(mar = c(0,0,0,0))
plot.new()
legend('center', legend = c('Peak', 'Season', 'Non-season'), fill = c(NA, '#8dd3c7', '#fdb462'), pch = c(8, NA, NA), 
       col = c('#e31a1c', NA, NA), lwd = c(2,2,2), lty = c(NA, NA, NA), border = NA, bty = 'n', ncol = 3)
dev.off()




